open Windtrap

exception Injected_probe_failure

type failing_tty = {
  fd : Eio_unix.Fd.t;
  mutable writes : int;
  on_write : Cstruct.t list -> int;
}

module Failing_tty = struct
  type t = failing_tty

  let read_methods = []
  let single_read _ _ = raise End_of_file

  let single_write t buffers =
    t.writes <- t.writes + 1;
    t.on_write buffers

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
end

let failing_tty ?(on_write = fun _ -> raise Injected_probe_failure) ~sw fd =
  let state =
    { fd = Eio_unix.Fd.of_unix ~sw ~close_unix:false fd; writes = 0; on_write }
  in
  let handler =
    Eio.Resource.handler
      [
        Eio.Resource.H (Eio.Flow.Pi.Source, (module Failing_tty));
        Eio.Resource.H (Eio.Flow.Pi.Sink, (module Failing_tty));
        Eio.Resource.H (Eio.Resource.Close, fun _ -> ());
        Eio.Resource.H (Eio_unix.Resource.T, fun t -> t.fd);
      ]
  in
  (Eio.Resource.T (state, handler), state)

type counting_source = {
  source : Eio_unix.source_ty Eio.Resource.t;
  fd : Eio_unix.Fd.t;
  mutable reads : int;
}

module Counting_source = struct
  type t = counting_source

  let read_methods = []

  let single_read t buffer =
    t.reads <- t.reads + 1;
    Eio.Flow.single_read t.source buffer
end

let counting_source source =
  let state = { source; fd = Eio_unix.Resource.fd source; reads = 0 } in
  let handler =
    Eio.Resource.handler
      [
        Eio.Resource.H (Eio.Flow.Pi.Source, (module Counting_source));
        Eio.Resource.H (Eio.Resource.Close, fun _ -> ());
        Eio.Resource.H (Eio_unix.Resource.T, fun t -> t.fd);
      ]
  in
  (Eio.Resource.T (state, handler), state)

let peek_signal number =
  let behavior = Sys.signal number Sys.Signal_ignore in
  ignore (Sys.signal number behavior : Sys.signal_behavior);
  behavior

let with_distinct_signal_handler number fn =
  let handler _ = () in
  let previous = Sys.signal number (Sys.Signal_handle handler) in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.signal number previous : Sys.signal_behavior))
    (fun () -> fn handler)

let assert_signal_handler ~msg number expected =
  match peek_signal number with
  | Sys.Signal_handle actual -> is_true ~msg (actual == expected)
  | Sys.Signal_default | Sys.Signal_ignore -> fail msg

let nonblocking_read_returns_without_input ~master fd =
  let outcome = Atomic.make `Pending in
  let byte = Bytes.create 1 in
  let reader =
    Thread.create
      (fun () ->
        let result =
          match Unix.read fd byte 0 1 with
          | _ -> `Read
          | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _)
            ->
              `Nonblocking
        in
        Atomic.set outcome result)
      ()
  in
  let deadline = Unix.gettimeofday () +. 0.1 in
  while Atomic.get outcome = `Pending && Unix.gettimeofday () < deadline do
    Thread.delay 0.001
  done;
  if Atomic.get outcome = `Pending then
    ignore (Pty.write_string master "x" 0 1 : int);
  Thread.join reader;
  Atomic.get outcome = `Nonblocking

(* The concurrent driver pattern a deterministic Eio test harness builds on
   matrix.test: the Matrix loop runs in one fiber, blocked inside [on_idle]
   on an [Eio.Condition]; the test script drives it from another fiber; an
   asynchronous fiber (a completed perform, in TEA terms) wakes the quiescent
   loop with [Matrix.post]. Runs under [Eio_mock.Backend]: single domain,
   deterministic scheduling.

   [signaled] is set before the broadcast and re-checked in a loop, so a wake
   that lands while the loop is between draining and awaiting is never lost.
   On a single domain the loop fiber only yields inside
   [Eio.Condition.await_no_mutex], so a fresh [idles] increment seen from the
   script means the loop is parked and the presented frame is stable. *)

(* The round-trip oracle, shared with mosaic/test/loop: the bytes the driver
   emitted must, replayed through matrix's own VTE, reconstruct the grid the
   backend believes it painted. *)
let rstrip_rows s =
  String.split_on_char '\n' s
  |> List.map (fun row ->
      let n = ref (String.length row) in
      while !n > 0 && row.[!n - 1] = ' ' do
        decr n
      done;
      String.sub row 0 !n)
  |> String.concat "\n" |> String.trim

let screen_replays t ~width ~height =
  let vte = Vte.create ~rows:height ~cols:width () in
  ignore (Vte.feed_string vte (Matrix_test.output t) : _ list);
  equal ~msg:"replaying the emitted ANSI reconstructs the grid" text
    (rstrip_rows (Matrix_test.screen t))
    (rstrip_rows (Vte.to_string vte))

type driver = {
  mutable terminal : Matrix_test.t option;
  cond : Eio.Condition.t;
  mutable signaled : bool;
  mutable idles : int;
}

let make_driver ~width ~height =
  let driver =
    {
      terminal = None;
      cond = Eio.Condition.create ();
      signaled = false;
      idles = 0;
    }
  in
  let on_idle t ~until =
    if not (Matrix_test.settle t ~until) then (
      driver.idles <- driver.idles + 1;
      while not driver.signaled do
        Eio.Condition.await_no_mutex driver.cond
      done;
      driver.signaled <- false)
  in
  let wake () =
    driver.signaled <- true;
    Eio.Condition.broadcast driver.cond
  in
  driver.terminal <-
    Some (Matrix_test.create ~on_idle ~on_wake:wake ~width ~height ());
  driver

let terminal driver = Option.get driver.terminal

(* Block the script fiber until the loop parks at a quiescent point newer
   than [beyond]. *)
let await_idle driver ~beyond =
  while driver.idles <= beyond do
    Eio.Fiber.yield ()
  done

let poke driver =
  driver.signaled <- true;
  Eio.Condition.broadcast driver.cond

let feed driver bytes =
  Matrix_test.feed (terminal driver) bytes;
  poke driver

(* Ends the input, which ends the run. *)
let stop driver =
  Matrix_test.stop (terminal driver);
  poke driver

(* What another fiber sends the loop. *)
type message = Redraw

let run_typed_app driver ~script =
  let t = terminal driver in
  let typed = Buffer.create 8 in
  let frames = ref 0 in
  let post = ref ignore in
  let loop () =
    Matrix.run ~backend:(Matrix_test.backend t) ~config:(Matrix_test.config t)
      (fun app ->
        (post := fun () -> Matrix.post app Redraw);
        let rec loop () =
          match Matrix.next app with
          | Stopped -> ()
          | Event (Key { key = Char u; _ }) when Uchar.is_char u ->
              Buffer.add_char typed (Uchar.to_char u);
              Matrix.request_redraw app;
              loop ()
          | Posted Redraw ->
              Matrix.request_redraw app;
              loop ()
          | Frame ->
              incr frames;
              Matrix.draw app (fun grid ->
                  Matrix.Grid.draw_text grid ~x:0 ~y:0
                    ~text:("typed:" ^ Buffer.contents typed));
              loop ()
          | Event _ | Timer _ -> loop ()
        in
        loop ())
  in
  Eio.Fiber.both loop (fun () -> script ~frames ~post:(fun () -> !post ()))

let test_input_crosses_fibers () =
  Eio_mock.Backend.run @@ fun () ->
  let driver = make_driver ~width:24 ~height:2 in
  run_typed_app driver ~script:(fun ~frames:_ ~post:_ ->
      await_idle driver ~beyond:0;
      equal ~msg:"initial frame presented" string "typed:"
        (String.trim (Matrix_test.screen (terminal driver)));
      screen_replays (terminal driver) ~width:24 ~height:2;
      let before = driver.idles in
      feed driver "hi";
      await_idle driver ~beyond:before;
      equal ~msg:"input crossed fibers and re-rendered" string "typed:hi"
        (String.trim (Matrix_test.screen (terminal driver)));
      screen_replays (terminal driver) ~width:24 ~height:2;
      stop driver)

let test_async_wake_reaches_quiescent_loop () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let driver = make_driver ~width:24 ~height:2 in
  run_typed_app driver ~script:(fun ~frames ~post ->
      await_idle driver ~beyond:0;
      let settled_frames = !frames in
      let before = driver.idles in
      (* A completed perform posts from its own fiber; the wake unparks the
         loop. *)
      Eio.Fiber.fork ~sw post;
      await_idle driver ~beyond:before;
      equal ~msg:"the post produced exactly one frame" int (settled_frames + 1)
        !frames;
      stop driver)

let quiet = Matrix.config ~raw_mode:false ~mouse:`Off ()

(* Draws the frames that come until [until] is true of a step. *)
let rec loop_until app until =
  match Matrix.next app with
  | Frame ->
      Matrix.draw app ignore;
      loop_until app until
  | step -> if not (until step) then loop_until app until

let test_native_thread_wakes_eio_loop () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdin, stdin_writer = Eio_unix.pipe sw in
  let stdout_reader, stdout = Eio_unix.pipe sw in
  let backend =
    Matrix_eio.backend ~clock:(Eio.Stdenv.mono_clock env) ~stdin ~stdout ()
  in
  let result =
    Eio.Time.with_timeout (Eio.Stdenv.clock env) 60. (fun () ->
        Matrix.run ~backend ~config:quiet (fun app ->
            let worker =
              Thread.create
                (fun () ->
                  Thread.delay 0.01;
                  Matrix.post app ignore)
                ()
            in
            loop_until app (function Posted _ -> true | _ -> false);
            Thread.join worker);
        Ok ())
  in
  Eio.Flow.close stdin_writer;
  Eio.Flow.close stdout_reader;
  match result with
  | Ok () -> ()
  | Error `Timeout -> fail "a post from a native thread did not wake Eio"

let test_stdin_eof_stops_the_runtime () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdin_pipe, stdin_writer = Eio_unix.pipe sw in
  let stdin, state = counting_source stdin_pipe in
  let stdout_reader, stdout = Eio_unix.pipe sw in
  Eio.Flow.close stdin_writer;
  let backend =
    Matrix_eio.backend ~clock:(Eio.Stdenv.mono_clock env) ~stdin ~stdout ()
  in
  let result =
    Eio.Time.with_timeout (Eio.Stdenv.clock env) 60. (fun () ->
        Matrix.run ~backend ~config:quiet (fun app ->
            loop_until app (function Stopped -> true | _ -> false));
        Ok ())
  in
  Eio.Flow.close stdout_reader;
  (match result with
  | Ok () -> ()
  | Error `Timeout -> fail "the run did not stop after stdin EOF");
  equal ~msg:"EOF is read exactly once" int 1 state.reads

let measure_successful_read_allocations iterations =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdin, stdin_writer = Eio_unix.pipe sw in
  let stdout_reader, stdout = Eio_unix.pipe sw in
  let seen = ref 0 in
  let consumed = Eio.Condition.create () in
  let backend =
    Matrix_eio.backend ~clock:(Eio.Stdenv.mono_clock env) ~stdin ~stdout ()
  in
  Gc.full_major ();
  let allocated_before = Gc.allocated_bytes () in
  Eio.Fiber.both
    (fun () ->
      Eio.Fiber.both
        (fun () ->
          Matrix.run ~backend ~config:quiet (fun app ->
              loop_until app (function
                | Stopped -> true
                | Event _ ->
                    incr seen;
                    Eio.Condition.broadcast consumed;
                    false
                | _ -> false));
          Eio.Flow.close stdout)
        (fun () ->
          for expected = 1 to iterations do
            Eio.Flow.copy_string "a" stdin_writer;
            while !seen < expected do
              Eio.Condition.await_no_mutex consumed
            done
          done;
          Eio.Flow.close stdin_writer))
    (fun () ->
      let reader = Eio.Buf_read.of_flow stdout_reader ~max_size:max_int in
      try
        while true do
          ignore (Eio.Buf_read.any_char reader : char)
        done
      with End_of_file -> ());
  let allocated_after = Gc.allocated_bytes () in
  equal ~msg:"successful reads observed" int iterations !seen;
  (allocated_after -. allocated_before) /. float_of_int (Sys.word_size / 8)

let test_successful_reads_do_not_allocate_a_result () =
  ignore (measure_successful_read_allocations 1 : float);
  let short = measure_successful_read_allocations 100 in
  let long = measure_successful_read_allocations 200 in
  let words_per_read = (long -. short) /. 100. in
  is_true
    ~msg:
      (Printf.sprintf
         "successful Matrix-Eio reads allocate %.3f words each; expected at \
          most 1575"
         words_per_read)
    (words_per_read <= 1575.)

(* A pty-backed resource whose writes go to the terminal, or fail. *)
let pty_tty ~sw fd =
  failing_tty ~sw fd ~on_write:(fun buffers ->
      List.fold_left
        (fun written buffer ->
          let text = Cstruct.to_string buffer in
          ignore (Unix.write_substring fd text 0 (String.length text) : int);
          written + String.length text)
        0 buffers)

let test_probe_failure_restores_pty_termios () =
  with_distinct_signal_handler Sys.sighup @@ fun sighup_handler ->
  with_distinct_signal_handler Sys.sigwinch @@ fun winch_handler ->
  Pty.with_pty ~winsize:Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 }
  @@ fun master slave ->
  let slave_fd = Pty.file_descr slave in
  Unix.set_nonblock slave_fd;
  let before = Unix.tcgetattr slave_fd in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let tty, state = failing_tty ~sw slave_fd in
  let backend =
    Matrix_eio.backend
      ~clock:(Eio.Stdenv.mono_clock env)
      ~stdin:tty ~stdout:tty ()
  in
  (match Matrix.run ~backend ignore with
  | () -> fail "probe failure did not escape Matrix.run"
  | exception Injected_probe_failure -> ()
  | exception exn ->
      failf "unexpected probe exception: %s" (Printexc.to_string exn));
  equal ~msg:"no mode was set, so none is reset" int 1 state.writes;
  let after = Unix.tcgetattr slave_fd in
  is_true ~msg:"echo restored" (before.c_echo = after.c_echo);
  is_true ~msg:"canonical input restored" (before.c_icanon = after.c_icanon);
  is_true ~msg:"signal keys restored" (before.c_isig = after.c_isig);
  equal ~msg:"minimum input bytes restored" int before.c_vmin after.c_vmin;
  equal ~msg:"input timeout restored" int before.c_vtime after.c_vtime;
  is_true ~msg:"the backend's nonblocking flag is kept"
    (nonblocking_read_returns_without_input ~master slave_fd);
  assert_signal_handler ~msg:"SIGHUP disposition kept" Sys.sighup sighup_handler;
  assert_signal_handler ~msg:"SIGWINCH disposition restored" Sys.sigwinch
    winch_handler

let test_cancelled_setup_restores_terminal () =
  Pty.with_pty ~winsize:Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 }
  @@ fun master slave ->
  let fd = Pty.file_descr slave in
  let before = Unix.tcgetattr fd in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let output = ref [] in
  let cancelled = ref false in
  let failure =
    try
      Eio.Cancel.sub (fun context ->
          let on_write buffers =
            Eio.Fiber.check ();
            List.fold_left
              (fun written buffer ->
                let text = Cstruct.to_string buffer in
                if !output = [] then
                  ignore (Pty.write_string master "x" 0 1 : int);
                (* Cancelled at the first write after the setup sets a
                   mode. *)
                if (not !cancelled) && List.mem "\027[?2004h" !output then (
                  cancelled := true;
                  is_false ~msg:"raw mode acquired before cancellation"
                    (Unix.tcgetattr fd).c_echo;
                  Eio.Cancel.cancel context Injected_probe_failure;
                  Eio.Fiber.check ());
                output := text :: !output;
                written + String.length text)
              0 buffers
          in
          let tty = fst (failing_tty ~on_write ~sw fd) in
          let backend =
            Matrix_eio.backend
              ~clock:(Eio.Stdenv.mono_clock env)
              ~stdin:tty ~stdout:tty ()
          in
          Matrix.run ~backend ignore);
      None
    with exn -> Some exn
  in
  (match failure with
  | Some (Eio.Cancel.Cancelled _) -> ()
  | Some exn -> failf "unexpected setup exception: %s" (Printexc.to_string exn)
  | None -> fail "setup did not propagate cancellation");
  let after = Unix.tcgetattr fd in
  is_true ~msg:"raw mode restored before cancellation escapes"
    (before.c_echo = after.c_echo
    && before.c_icanon = after.c_icanon
    && before.c_isig = after.c_isig);
  List.iter
    (fun sequence ->
      is_true
        ~msg:(Printf.sprintf "terminal setup/teardown emitted %S" sequence)
        (List.mem sequence !output))
    [ "\027[?1049h"; "\027[?1049l"; "\027[?2004h"; "\027[?2004l" ]

(* SIGWINCH is the controlling terminal's: a run on a terminal the process
   serves, such as this pseudo-terminal, leaves it to the application, whose
   handler keeps it and is called. *)
let test_sigwinch_is_the_controlling_terminals () =
  let calls = ref 0 in
  let prior _ = incr calls in
  let previous = Sys.signal Sys.sigwinch (Sys.Signal_handle prior) in
  Fun.protect ~finally:(fun () ->
      ignore (Sys.signal Sys.sigwinch previous : Sys.signal_behavior))
  @@ fun () ->
  Pty.with_pty ~winsize:Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 }
  @@ fun master slave ->
  let fd = Pty.file_descr slave in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let tty = fst (pty_tty ~sw fd) in
  let backend =
    Matrix_eio.backend
      ~clock:(Eio.Stdenv.mono_clock env)
      ~stdin:tty ~stdout:tty ()
  in
  is_false ~msg:"not the controlling terminal" backend.controlling;
  Matrix.run ~backend
    ~config:(Matrix.config ~kitty_keyboard:`Off ~mouse:`Off ()) (fun app ->
      (match Matrix.next app with
      | Frame -> Matrix.draw app ignore
      | _ -> fail "expected the first frame");
      Pty.set_winsize master
        Pty.{ rows = 30; cols = 100; xpixel = 0; ypixel = 0 };
      Unix.kill (Unix.getpid ()) Sys.sigwinch;
      let later = Matrix.at app (Matrix.now app + 300_000_000) in
      let resized = ref false in
      loop_until app (function
        | Event (Resize _) ->
            resized := true;
            false
        | Timer t -> t = later
        | _ -> false);
      is_false ~msg:"the run did not act on it" !resized);
  is_true ~msg:"the application's handler was called" (!calls > 0)

(* A post from another fiber that lands while the loop is out of its wait,
   with a subprocess storm perturbing the scheduler, still reaches the loop
   at once. *)
let test_wake_survives_subprocess_storm () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let stdin, stdin_writer = Eio_unix.pipe sw in
  let stdout_reader, stdout = Eio_unix.pipe sw in
  let renders = Atomic.make 0 in
  let bumped = Eio.Condition.create () in
  let backend =
    Matrix_eio.backend ~clock:(Eio.Stdenv.mono_clock env) ~stdin ~stdout ()
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let reader = Eio.Buf_read.of_flow stdout_reader ~max_size:max_int in
      (try
         while true do
           ignore (Eio.Buf_read.any_char reader : char)
         done
       with End_of_file | Eio.Cancel.Cancelled _ -> ());
      `Stop_daemon);
  Eio.Fiber.fork_daemon ~sw (fun () ->
      (try
         while true do
           Eio.Process.run proc_mgr [ "true" ]
         done
       with Eio.Cancel.Cancelled _ | End_of_file -> ());
      `Stop_daemon);
  let iterations = 400 in
  let stalled = ref 0 in
  let elapsed = ref 0. in
  Matrix.run ~backend ~config:quiet (fun app ->
      let finished = ref false in
      Eio.Fiber.both
        (fun () ->
          let rec loop () =
            if not !finished then
              match Matrix.next app with
              | Frame ->
                  Matrix.draw app ignore;
                  Atomic.incr renders;
                  Eio.Condition.broadcast bumped;
                  (* Out of the wait: a post from another fiber lands now. *)
                  Eio.Fiber.yield ();
                  loop ()
              | Posted f ->
                  f ();
                  loop ()
              | _ -> loop ()
          in
          loop ())
        (fun () ->
          let await_render ~before =
            Eio.Time.with_timeout clock 60. (fun () ->
                while Atomic.get renders <= before do
                  Eio.Condition.await_no_mutex bumped
                done;
                Ok ())
          in
          (match await_render ~before:0 with Ok () | Error `Timeout -> ());
          let start = Eio.Time.now clock in
          let index = ref 0 in
          while !index < iterations && !stalled = 0 do
            incr index;
            let before = Atomic.get renders in
            Matrix.post app (fun () -> Matrix.request_redraw app);
            match await_render ~before with
            | Ok () -> ()
            | Error `Timeout -> stalled := !index
          done;
          elapsed := Eio.Time.now clock -. start;
          finished := true;
          Matrix.post app ignore));
  Eio.Flow.close stdout;
  Eio.Flow.close stdin_writer;
  if !stalled > 0 then
    failf "render loop stranded after wake %d/%d (no frame within a minute)"
      !stalled iterations;
  is_true
    ~msg:
      (Printf.sprintf "%d gap wakes painted %d frames in %.2fs, none stranded"
         iterations (Atomic.get renders) !elapsed)
    (Atomic.get renders >= iterations)

let () =
  run "matrix-eio.driver"
    [
      group "Concurrent driver"
        [
          test "input crosses fibers" test_input_crosses_fibers;
          test "async wake reaches a quiescent loop"
            test_async_wake_reaches_quiescent_loop;
          test "native thread wakes Eio loop" test_native_thread_wakes_eio_loop;
          test "wake survives a subprocess storm"
            test_wake_survives_subprocess_storm;
          test "stdin EOF stops the runtime" test_stdin_eof_stops_the_runtime;
          test "successful reads avoid adapter result allocations"
            test_successful_reads_do_not_allocate_a_result;
          test "probe failure restores PTY termios"
            test_probe_failure_restores_pty_termios;
          test "cancelled setup restores terminal modes"
            test_cancelled_setup_restores_terminal;
          test "SIGWINCH is the controlling terminal's"
            test_sigwinch_is_the_controlling_terminals;
        ];
    ]
