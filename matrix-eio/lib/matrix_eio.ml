(* ───── SIGWINCH Handling ───── *)

let with_rollback ~rollback fn =
  try fn ()
  with exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    (try rollback () with _ -> ());
    Printexc.raise_with_backtrace exn backtrace

(* Signal handlers cannot use Eio primitives, so we poll an atomic flag at most
   every 50ms — imperceptible for resize events. *)
let sigwinch_poll_interval = 0.05
let winch_flag = Atomic.make false

let install_winch_handler () =
  try
    Atomic.set winch_flag false;
    let previous =
      Sys.signal Sys.sigwinch
        (Sys.Signal_handle (fun _ -> Atomic.set winch_flag true))
    in
    let active = ref true in
    fun () ->
      if !active then (
        active := false;
        (try ignore (Sys.signal Sys.sigwinch previous : Sys.signal_behavior)
         with _ -> ());
        Atomic.set winch_flag false)
  with Invalid_argument _ -> ignore

(* ───── Termination Signals ───── *)

(* Termination signals must not run [Matrix.close] from the signal handler:
   for this backend close performs Eio effects (terminal output writes go
   through [Eio.Flow], flush_input through [Eio_unix.Fd.use_exn]), and
   performing an effect inside an OCaml signal handler is unsound. When the
   domain is parked in the Eio scheduler — the normal state for an idle TUI —
   the interrupted stack has no effect handler, so the perform raises
   [Effect.Unhandled]; the terminal is then left raw exactly in the
   clean-shutdown case the handler exists for. When a fiber is running, the
   effect would instead suspend that fiber mid-handler, corrupting scheduler
   state. The handler therefore only records the signal; the event loop polls
   the flag at the same <=50ms cadence as SIGWINCH and shuts down from the
   loop fiber, where Eio operations are legal. *)
let termination_signal = Atomic.make 0

let install_termination_handlers () =
  Atomic.set termination_signal 0;
  let installed = ref [] in
  let restore () =
    List.iter
      (fun (signal, previous) ->
        try ignore (Sys.signal signal previous : Sys.signal_behavior)
        with _ -> ())
      !installed;
    Atomic.set termination_signal 0
  in
  let handler =
    Sys.Signal_handle (fun signum -> Atomic.set termination_signal signum)
  in
  List.iter
    (fun signal ->
      match Sys.signal signal handler with
      | previous -> installed := (signal, previous) :: !installed
      | exception Invalid_argument _ -> ())
    [ Sys.sigterm; Sys.sigint; Sys.sigquit; Sys.sigabrt; Sys.sighup ];
  let active = ref true in
  fun () ->
    if !active then (
      active := false;
      restore ())

(* ───── Application Setup ───── *)

let create ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(respect_alpha = false) ?(mouse_enabled = true) ?(mouse = None)
    ?(bracketed_paste = true) ?(focus_reporting = true)
    ?(kitty_keyboard = `Auto) ?(exit_on_ctrl_c = true) ?(debug_overlay = false)
    ?(debug_overlay_corner = `Bottom_right) ?(debug_overlay_capacity = 120)
    ?(frame_dump_every = 0) ?frame_dump_dir ?frame_dump_pattern
    ?(frame_dump_hits = false) ?(cursor_visible = mode = `Alt)
    ?(explicit_width = false) ?(input_timeout = None)
    ?(resize_debounce = Some 0.1) ?(min_tui_height = 1) ?(start_idle = false)
    ?(signal_handlers = true) ?initial_caps ~sw ~clock ~stdin ~stdout () =
  let input_eio_fd = Eio_unix.Resource.fd stdin in
  let output_eio_fd = Eio_unix.Resource.fd stdout in
  let output_is_tty =
    Eio_unix.Fd.use_exn "is_tty" output_eio_fd Matrix.Terminal.is_tty
  in
  let input_is_tty =
    Eio_unix.Fd.use_exn "is_tty" input_eio_fd Matrix.Terminal.is_tty
  in
  let wakeup = Eio.Condition.create () in
  (* Level-triggered wake: [wake] sets [wake_pending] before broadcasting, and
     the wait's fast path takes a pending wake without awaiting, so a wake that
     lands while the loop is out of its wait (mid-render, or descheduled by
     another fiber) is never lost to an edge-triggered broadcast. *)
  let wake_pending = ref false in
  let terminal =
    Matrix.Terminal.make
      ~output:(fun s -> Eio.Flow.copy_string s stdout)
      ~tty:output_is_tty ?initial_caps ()
  in
  let parser = Matrix.Input.Parser.create () in
  let original_termios = ref None in
  let restore_winch = ref ignore in
  let restore_termination = ref ignore in
  let set_raw_mode enabled =
    match (enabled, !original_termios) with
    | true, Some _ | false, None -> ()
    | true, None ->
        if input_is_tty then
          original_termios :=
            Some
              (Eio_unix.Fd.use_exn "set_raw" input_eio_fd
                 Matrix.Terminal.set_raw)
    | false, Some saved ->
        Eio_unix.Fd.use_exn "restore" input_eio_fd (fun fd ->
            Matrix.Terminal.restore fd saved);
        original_termios := None
  in
  let app_ref = ref None in
  (* Shut down from the loop fiber when a termination signal was recorded:
     close the app (legal here, unlike in the signal handler) and exit with
     the conventional status, matching the Unix backend's semantics. *)
  let check_termination () =
    match Atomic.get termination_signal with
    | 0 -> ()
    | signum ->
        (match !app_ref with
        | Some app -> ( try Matrix.close app with _ -> ())
        | None -> ());
        exit (128 + signum)
  in
  (* Register before acquiring raw mode. Once attachment succeeds this same
     callback transfers from construction rollback to [Matrix.close]. *)
  let close_owned () =
    match !app_ref with
    | Some app -> Matrix.close app
    | None -> set_raw_mode false
  in
  let close_owned_protected () = Eio.Cancel.protect close_owned in
  Eio.Switch.on_release sw close_owned_protected;
  with_rollback ~rollback:close_owned_protected (fun () ->
      let input_buffer = Bytes.create 4096 in
      let input_cs = Cstruct.create 4096 in
      let await_readable ~timeout =
        match
          Eio.Time.with_timeout clock timeout (fun () ->
              Eio_unix.Fd.use_exn "await" input_eio_fd (fun fd ->
                  Eio_unix.await_readable fd);
              Ok ())
        with
        | Ok () -> true
        | Error `Timeout -> false
      in
      let read_stdin () =
        (* [single_read] returns a positive length or raises [End_of_file]. Keep
           EOF as an immediate sentinel so successful reads do not allocate. *)
        match Eio.Flow.single_read stdin input_cs with
        | n ->
            Cstruct.blit_to_bytes input_cs 0 input_buffer 0 n;
            n
        | exception End_of_file -> 0
      in
      let read_input buf off len =
        let cs =
          if len <= Cstruct.length input_cs then Cstruct.sub input_cs 0 len
          else Cstruct.create len
        in
        match Eio.Flow.single_read stdin cs with
        | n ->
            Cstruct.blit_to_bytes cs 0 buf off n;
            n
        | exception End_of_file -> 0
      in
      let terminal_size () =
        Eio_unix.Fd.use_exn "size" output_eio_fd Matrix.Terminal.size
      in
      (* ───── IO Callbacks ───── *)
      let now () = Eio.Time.now clock in
      let wake () =
        wake_pending := true;
        Eio.Condition.broadcast wakeup
      in
      let flush_input () =
        if input_is_tty then
          Eio_unix.Fd.use_exn "flush_input" input_eio_fd
            Matrix.Terminal.flush_input
      in
      let write_output buf off len =
        Eio.Flow.write stdout [ Cstruct.of_bytes ~off ~len buf ]
      in
      (* The three peer branches of the wait, allocated once so a successful
         read stays on the no-allocation path; the deadline is read from a cell
         each call rather than re-closed over. *)
      let wait_deadline = ref sigwinch_poll_interval in
      let await_input () =
        Eio_unix.Fd.use_exn "await" input_eio_fd (fun fd ->
            Eio_unix.await_readable fd);
        `Input
      in
      let await_wake () =
        Eio.Condition.await_no_mutex wakeup;
        wake_pending := false;
        `Wakeup
      in
      let await_deadline () =
        Eio.Time.sleep clock !wait_deadline;
        `Timeout
      in
      let wait_branches = [ await_input; await_wake; await_deadline ] in
      let read_events ~timeout ~on_event ~on_response =
        let effective_timeout =
          match timeout with
          | None -> sigwinch_poll_interval
          | Some t -> Float.min t sigwinch_poll_interval
        in
        (* Input, wake, and deadline race as three peers. The deadline is a
           plain [sleep] re-armed every call — not a [with_timeout] wrapping
           the wait, whose nested [Fiber.first] can leave the loop parked in
           the wake branch after the timer fired — so it always resolves and
           no cancellation race can strand the loop. [wake] is
           level-triggered: it sets [wake_pending] before its broadcast, so a
           wake that lands while the loop is out of this wait is taken by the
           fast path without awaiting, never lost to the edge-triggered
           broadcast nor to a cancelled wake branch. *)
        wait_deadline := effective_timeout;
        let got =
          if !wake_pending then (
            wake_pending := false;
            `Wakeup)
          else Eio.Fiber.any wait_branches
        in
        check_termination ();
        if Atomic.get winch_flag then (
          Atomic.set winch_flag false;
          let cols, rows = terminal_size () in
          on_event (Matrix.Input.Resize (cols, rows)));
        let result =
          match got with
          | `Input -> (
              match read_stdin () with
              | 0 -> `End
              | n ->
                  let now = Eio.Time.now clock in
                  Matrix.Input.Parser.feed parser input_buffer 0 n ~now
                    ~on_event ~on_response;
                  `Continue)
          | `Wakeup | `Timeout -> `Continue
        in
        (match result with
        | `Continue ->
            let now = Eio.Time.now clock in
            Matrix.Input.Parser.drain parser ~now ~on_event ~on_response
        | `End -> ());
        result
      in
      let backend : Matrix.Backend.t =
        {
          now;
          wake;
          write_output;
          read_input;
          wait_readable = await_readable;
          read_events;
          terminal_size;
          set_raw_mode;
          flush_input;
          cleanup =
            (fun () ->
              !restore_termination ();
              !restore_winch ());
        }
      in
      let session =
        Matrix.Backend.bootstrap ~mode ~raw_mode ~input_is_tty ~terminal ~parser
          backend
      in
      let app =
        (* [signal_handlers:false]: Matrix.attach would install handlers that
           call Matrix.close from signal context, which performs Eio effects —
           see [install_termination_handlers]. Termination signals are handled
           Eio-natively below instead. *)
        Matrix.attach ~mode ~raw_mode ~target_fps ~respect_alpha ~mouse_enabled
          ~mouse ~bracketed_paste ~focus_reporting ~kitty_keyboard
          ~exit_on_ctrl_c ~debug_overlay ~debug_overlay_corner
          ~debug_overlay_capacity ~frame_dump_every ?frame_dump_dir
          ?frame_dump_pattern ~frame_dump_hits ~cursor_visible ~explicit_width
          ~input_timeout ~resize_debounce ~min_tui_height ~start_idle
          ~signal_handlers:false ~startup_events:session.startup_events ~backend
          ~parser ~terminal ~width:session.width ~height:session.height
          ~render_offset:session.render_offset
          ~static_needs_newline:session.static_needs_newline ()
      in
      app_ref := Some app;
      Matrix.Terminal.query_pixel_resolution terminal;
      restore_winch := install_winch_handler ();
      if signal_handlers then
        restore_termination := install_termination_handlers ();
      app)
