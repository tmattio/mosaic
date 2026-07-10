open Windtrap

exception Injected_probe_failure

type failing_tty = { fd : Eio_unix.Fd.t; mutable writes : int }

module Failing_tty = struct
  type t = failing_tty

  let read_methods = []
  let single_read _ _ = raise End_of_file

  let single_write t _ =
    t.writes <- t.writes + 1;
    raise Injected_probe_failure

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
end

let failing_tty ~sw fd =
  let state =
    { fd = Eio_unix.Fd.of_unix ~sw ~close_unix:false fd; writes = 0 }
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

(* Proves the concurrent driver pattern a deterministic Eio test harness
   builds on matrix.test: the Matrix loop runs in one fiber, blocked inside
   [on_idle] on an [Eio.Condition]; the test script drives it from another
   fiber; an asynchronous fiber (a completed perform, in TEA terms) wakes the
   quiescent loop through [Matrix.request_redraw]'s wake channel. Runs under
   [Eio_mock.Backend]: single domain, deterministic scheduling.

   The condition-variable discipline matters: [signaled] is set before the
   broadcast and re-checked in a loop, so a wake that lands while the loop is
   between draining and awaiting is never lost. On a single domain the loop
   fiber only yields inside [Eio.Condition.await_no_mutex], so observing a
   fresh [idles] increment from the script implies the loop is parked and the
   presented frame is stable. *)

type driver = {
  mutable backend : Matrix_test.t option;
  cond : Eio.Condition.t;
  mutable signaled : bool;
  mutable idles : int;
  mutable stopping : bool;
}

let make_driver ~width ~height =
  let driver =
    {
      backend = None;
      cond = Eio.Condition.create ();
      signaled = false;
      idles = 0;
      stopping = false;
    }
  in
  let on_idle _t ~timeout:_ =
    driver.idles <- driver.idles + 1;
    while (not driver.signaled) && not driver.stopping do
      Eio.Condition.await_no_mutex driver.cond
    done;
    driver.signaled <- false
  in
  let wake () =
    driver.signaled <- true;
    Eio.Condition.broadcast driver.cond
  in
  let backend = Matrix_test.create ~on_idle ~on_wake:wake ~width ~height () in
  driver.backend <- Some backend;
  driver

let backend driver = Option.get driver.backend

(* Block the script fiber until the loop parks at a quiescent point newer
   than [beyond]. *)
let await_idle driver ~beyond =
  while driver.idles <= beyond do
    Eio.Fiber.yield ()
  done

let feed driver bytes =
  Matrix_test.feed (backend driver) bytes;
  driver.signaled <- true;
  Eio.Condition.broadcast driver.cond

let stop driver =
  driver.stopping <- true;
  (* [Matrix.stop] raises the wake channel, which unparks [on_idle]. *)
  Matrix_test.stop (backend driver)

let run_typed_app driver ~script =
  let t = backend driver in
  let typed = Buffer.create 8 in
  let frames = ref 0 in
  let loop () =
    Matrix.run (Matrix_test.app t)
      ~on_input:(fun _app event ->
        match event with
        | Matrix.Input.Key { key = Matrix.Input.Key.Char u; _ }
          when Uchar.is_char u ->
            Buffer.add_char typed (Uchar.to_char u)
        | _ -> ())
      ~on_render:(fun app ->
        incr frames;
        Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0
          ~text:("typed:" ^ Buffer.contents typed))
  in
  Eio.Fiber.both loop (fun () -> script ~frames)

let test_input_crosses_fibers () =
  Eio_mock.Backend.run @@ fun () ->
  let driver = make_driver ~width:24 ~height:2 in
  run_typed_app driver ~script:(fun ~frames:_ ->
      await_idle driver ~beyond:0;
      equal ~msg:"initial frame presented" string "typed:"
        (String.trim (Matrix_test.screen (backend driver)));
      let before = driver.idles in
      feed driver "hi";
      await_idle driver ~beyond:before;
      equal ~msg:"input crossed fibers and re-rendered" string "typed:hi"
        (String.trim (Matrix_test.screen (backend driver)));
      stop driver)

let test_async_wake_reaches_quiescent_loop () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let driver = make_driver ~width:24 ~height:2 in
  run_typed_app driver ~script:(fun ~frames ->
      await_idle driver ~beyond:0;
      let settled_frames = !frames in
      let before = driver.idles in
      (* A completed perform calls [Matrix.request_redraw] from its own
         fiber; the wake channel must unpark the loop. *)
      Eio.Fiber.fork ~sw (fun () ->
          Matrix.request_redraw (Matrix_test.app (backend driver)));
      await_idle driver ~beyond:before;
      equal ~msg:"async wake produced exactly one frame" int
        (settled_frames + 1) !frames;
      stop driver)

let test_stdin_eof_stops_the_runtime () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdin_pipe, stdin_writer = Eio_unix.pipe sw in
  let stdin, state = counting_source stdin_pipe in
  let stdout_reader, stdout = Eio_unix.pipe sw in
  Eio.Flow.close stdin_writer;
  let app =
    Matrix_eio.create ~sw ~clock:(Eio.Stdenv.clock env) ~stdin ~stdout
      ~raw_mode:false ~target_fps:None ~mouse_enabled:false
      ~signal_handlers:false ~start_idle:true ()
  in
  let result =
    Eio.Time.with_timeout (Eio.Stdenv.clock env) 0.5 (fun () ->
        Matrix.run app ~on_render:(fun _app -> ());
        Ok ())
  in
  Eio.Flow.close stdout_reader;
  (match result with
  | Ok () -> ()
  | Error `Timeout -> fail "Matrix.run did not return after stdin EOF");
  equal ~msg:"EOF is read exactly once" int 1 state.reads;
  is_false ~msg:"EOF closes the application" (Matrix.running app);
  Matrix.close app;
  equal ~msg:"repeated close does not poll EOF again" int 1 state.reads

let measure_successful_read_allocations iterations =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdin, stdin_writer = Eio_unix.pipe sw in
  let stdout_reader, stdout = Eio_unix.pipe sw in
  let seen = ref 0 in
  let consumed = Eio.Condition.create () in
  let app =
    Matrix_eio.create ~sw ~clock:(Eio.Stdenv.clock env) ~stdin ~stdout
      ~raw_mode:false ~target_fps:(Some 1.) ~mouse_enabled:false
      ~signal_handlers:false ~start_idle:true ()
  in
  let payload = [ Cstruct.of_string "a" ] in
  Gc.full_major ();
  let allocated_before = Gc.allocated_bytes () in
  Eio.Fiber.both
    (fun () ->
      Eio.Fiber.both
        (fun () ->
          Matrix.run app
            ~on_input:(fun _app _event ->
              incr seen;
              Eio.Condition.broadcast consumed)
            ~on_render:(fun _app -> ());
          Eio.Flow.close stdout)
        (fun () ->
          for expected = 1 to iterations do
            Eio.Flow.write stdin_writer payload;
            while !seen < expected do
              Eio.Condition.await_no_mutex consumed
            done
          done;
          Eio.Flow.close stdin_writer))
    (fun () ->
      let buffer = Cstruct.create 4096 in
      try
        while true do
          ignore (Eio.Flow.single_read stdout_reader buffer : int)
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

let test_probe_failure_restores_pty_termios () =
  Pty.with_pty ~winsize:Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 }
  @@ fun master slave ->
  let slave_fd = Pty.file_descr slave in
  Unix.set_nonblock slave_fd;
  let before = Unix.tcgetattr slave_fd in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let tty, state = failing_tty ~sw slave_fd in
  let failure =
    try
      ignore
        (Matrix_eio.create ~sw ~clock:(Eio.Stdenv.clock env) ~stdin:tty
           ~stdout:tty ~signal_handlers:false ());
      None
    with exn -> Some exn
  in
  (match failure with
  | Some Injected_probe_failure -> ()
  | Some exn -> failf "unexpected probe exception: %s" (Printexc.to_string exn)
  | None -> fail "probe failure did not escape Matrix_eio.create");
  equal ~msg:"probe output failed on its first write" int 1 state.writes;
  let after = Unix.tcgetattr slave_fd in
  is_true ~msg:"echo restored after failed construction"
    (before.c_echo = after.c_echo);
  is_true ~msg:"canonical input restored after failed construction"
    (before.c_icanon = after.c_icanon);
  is_true ~msg:"terminal signal processing restored after failed construction"
    (before.c_isig = after.c_isig);
  equal ~msg:"minimum input bytes restored after failed construction" int
    before.c_vmin after.c_vmin;
  equal ~msg:"input timeout restored after failed construction" int
    before.c_vtime after.c_vtime;
  is_true ~msg:"failed construction preserves the backend's nonblocking flag"
    (nonblocking_read_returns_without_input ~master slave_fd)

let () =
  run "matrix-eio.driver"
    [
      group "Concurrent driver"
        [
          test "input crosses fibers" test_input_crosses_fibers;
          test "async wake reaches a quiescent loop"
            test_async_wake_reaches_quiescent_loop;
          test "stdin EOF stops the runtime" test_stdin_eof_stops_the_runtime;
          test "successful reads avoid adapter result allocations"
            test_successful_reads_do_not_allocate_a_result;
          test "probe failure restores PTY termios"
            test_probe_failure_restores_pty_termios;
        ];
    ]
