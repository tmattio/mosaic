open Eio.Std

let src = Logs.Src.create "program" ~doc:"Program and render loop events"

module Log = (val Logs.src_log src : Logs.LOG)

(* Setup logging with an out_channel *)
let setup_logging oc enabled =
  if not enabled then ()
  else
    let formatter = Format.formatter_of_out_channel oc in
    let report : type a b.
        Logs.src ->
        Logs.level ->
        over:(unit -> unit) ->
        (unit -> b) ->
        (a, b) Logs.msgf ->
        b =
     fun src level ~over k msgf ->
      let dt = Unix.gettimeofday () in
      let k' _ =
        Format.pp_print_flush formatter ();
        over ();
        k ()
      in
      msgf @@ fun ?header:_ ?tags:_ fmt ->
      Format.fprintf formatter "[%.3f] [%s] [%s] " dt
        (Logs.level_to_string (Some level))
        (Logs.Src.name src);
      Format.kfprintf
        (fun formatter ->
          Format.pp_print_newline formatter ();
          k' formatter)
        formatter fmt
    in
    Logs.set_reporter { Logs.report };
    Logs.set_level (Some Logs.Debug)

(*  Public configuration type *)

type config = {
  terminal : Tty_eio.t option;
  alt_screen : bool;
  mouse : bool;
  fps : int;
  debug_log : out_channel option;
}

let config ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 30)
    ?debug_log () =
  { terminal; alt_screen; mouse; fps; debug_log }

(*  Render‑mode polymorphic state *)

type render_mode =
  | Alt_screen of { mutable previous : Screen.t option }
  | Standard of { mutable previous_dynamic : Screen.t option }

let is_alt = function Alt_screen _ -> true | Standard _ -> false

(*  Opaque program handle *)

type t = {
  (* mutable flag *)
  mutable running : bool;
  (* terminal & I *)
  term : Tty_eio.t;
  mutable event_source : Event_source.t option;
  (* user confi *)
  mouse : bool;
  fps : int;
  (* rendering stat *)
  mutable render_mode : render_mode;
  mutable static_elements : Ui.element list;
  (* environmen *)
  clock : float Eio.Time.clock_ty r;
  sw : Switch.t;
  env : Eio_unix.Stdenv.base;
  (* synchronisatio *)
  terminal_mutex : Eio.Mutex.t;
  state_mutex : Eio.Mutex.t;
  resize_cond : Eio.Condition.t;
  render_cond : Eio.Condition.t;
  mutable needs_render : bool;
  (* render coalescing *)
  mutable last_render_time : float;
  mutable render_pending : bool;
  (* saved signal handler *)
  mutable sigint_prev : Sys.signal_behavior option;
  mutable sigterm_prev : Sys.signal_behavior option;
  mutable sighup_prev : Sys.signal_behavior option;
  (* layout snapshot for mouse support *)
  snapshot : Ui.Layout_snapshot.t option;
  (* callback for snapshot updates *)
  mutable on_snapshot : (Ui.Layout_snapshot.t -> unit) option;
}

(*  Constructors / helpers *)

let make_terminal ?terminal ~sw ~env () =
  match terminal with
  | Some tty -> tty
  | None -> Tty_eio.create ~sw ~env ~tty:true ()

let create (cfg : config) ~(sw : Switch.t) ~(env : Eio_unix.Stdenv.base) : t =
  (* Setup logging if debug_log is provided *)
  (match cfg.debug_log with Some oc -> setup_logging oc true | None -> ());
  let term = make_terminal ?terminal:cfg.terminal ~sw ~env () in
  let render_mode =
    if cfg.alt_screen then Alt_screen { previous = None }
    else Standard { previous_dynamic = None }
  in
  {
    running = true;
    term;
    event_source = None;
    mouse = cfg.mouse;
    fps = cfg.fps;
    render_mode;
    static_elements = [];
    clock = Eio.Stdenv.clock env;
    sw;
    env;
    terminal_mutex = Eio.Mutex.create ();
    state_mutex = Eio.Mutex.create ();
    resize_cond = Eio.Condition.create ();
    render_cond = Eio.Condition.create ();
    needs_render = false;
    last_render_time = 0.0;
    render_pending = false;
    sigint_prev = None;
    sigterm_prev = None;
    sighup_prev = None;
    snapshot = (if cfg.mouse then Some (Ui.Layout_snapshot.create ()) else None);
    on_snapshot = None;
    (* Will be set in start function *)
  }

let with_term_mutex t ~f = Eio.Mutex.use_rw ~protect:true t.terminal_mutex f
let with_state_mutex t ~f = Eio.Mutex.use_rw ~protect:true t.state_mutex f

(*  Terminal output logging helper *)

let log_terminal_output output description =
  let escape_char c =
    match c with
    | '\027' -> "\\ESC"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | '\t' -> "\\t"
    | c when Char.code c < 32 -> Printf.sprintf "\\x%02x" (Char.code c)
    | c -> String.make 1 c
  in
  let escaped =
    String.to_seq output |> Seq.map escape_char |> List.of_seq
    |> String.concat ""
  in
  let bytes_count = String.length output in
  Log.info (fun m ->
      m "Terminal output (%s): %d bytes | %s" description bytes_count escaped)

(*  Terminal setup / teardown *)

let build_cleanup_sequence had_alt_screen =
  let esc = Buffer.create 32 in
  Buffer.add_string esc Ansi.cursor_show;
  Buffer.add_string esc Ansi.mouse_off;
  Buffer.add_string esc Ansi.bracketed_paste_off;
  Buffer.add_string esc Ansi.kitty_keyboard_off;
  Buffer.add_string esc Ansi.focus_event_off;
  if had_alt_screen then Buffer.add_string esc Ansi.alternate_screen_off;
  Buffer.contents esc

let full_cleanup term had_alt_screen =
  (* Build comprehensive cleanup sequence *)
  let seq = build_cleanup_sequence had_alt_screen in
  (* Write cleanup sequence *)
  Tty_eio.write term (Bytes.of_string seq) 0 (String.length seq);
  (* Ensure it's flushed immediately *)
  Tty_eio.flush term;
  (* Reset terminal to cooked mode *)
  Tty_eio.set_mode term `Cooked;
  (* Final flush to ensure all changes are applied *)
  Tty_eio.flush term

let setup_terminal t =
  with_term_mutex t ~f:(fun () ->
      try
        (* Save the current terminal state *)
        Tty_eio.save_state t.term;
        Log.debug (fun m -> m "Saved terminal state");

        (* First ensure the terminal is in a clean state *)
        let reset_seq =
          Ansi.mouse_off ^ Ansi.bracketed_paste_off ^ Ansi.kitty_keyboard_off
          ^ Ansi.focus_event_off (* Disable focus events first *)
        in
        log_terminal_output reset_seq "terminal reset sequence";
        Tty_eio.write t.term
          (Bytes.of_string reset_seq)
          0 (String.length reset_seq);
        Tty_eio.flush t.term;

        (* Now set up the terminal for our use *)
        Tty_eio.set_mode t.term `Raw;
        Tty_eio.hide_cursor t.term;
        (match t.render_mode with
        | Alt_screen _ ->
            Tty_eio.enable_alternate_screen t.term;
            Tty_eio.clear_screen t.term
        | _ -> ());
        if t.mouse then Tty_eio.set_mouse_mode t.term `Any;
        Tty_eio.enable_focus_reporting t.term;
        Tty_eio.enable_kitty_keyboard t.term;
        Tty_eio.enable_bracketed_paste t.term;
        Tty_eio.flush t.term;

        Log.debug (fun m -> m "Terminal setup complete")
      with exn ->
        Log.err (fun m ->
            m "Setup terminal failed: %s" (Printexc.to_string exn));
        raise exn)

let resize_handler t _ = Eio.Condition.broadcast t.resize_cond

let setup_signal_handlers t =
  let emergency _ =
    (* In signal handler context, just release the terminal *)
    (* The Tty module's release function now handles signal contexts properly *)
    (try Tty_eio.release t.term with _ -> ());
    exit 130
  in
  t.sigint_prev <- Some (Sys.signal Sys.sigint (Sys.Signal_handle emergency));
  t.sigterm_prev <- Some (Sys.signal Sys.sigterm (Sys.Signal_handle emergency));
  t.sighup_prev <- Some (Sys.signal Sys.sighup (Sys.Signal_handle emergency));
  (* Only set resize handler if we're in a TTY *)
  try
    if Unix.isatty (Tty_eio.input_fd t.term) then
      Tty_eio.set_resize_handler t.term (resize_handler t)
  with _ -> ()

let restore sig_no v =
  Sys.set_signal sig_no (Option.value v ~default:Sys.Signal_default)

let cleanup t =
  Tty_eio.remove_resize_handlers t.term;
  (try
     with_term_mutex t ~f:(fun () ->
         (* Send cleanup escape sequences *)
         full_cleanup t.term (is_alt t.render_mode);
         (* Restore the saved terminal state *)
         Tty_eio.restore_state t.term;
         Log.debug (fun m -> m "Restored terminal state during cleanup"))
   with Eio.Mutex.Poisoned _ -> (
     (* If mutex is poisoned, still try to restore *)
     try
       Tty_eio.restore_state t.term;
       Tty_eio.flush t.term
     with _ -> ( try Tty_eio.release t.term with _ -> ())));
  restore Sys.sigint t.sigint_prev;
  restore Sys.sigterm t.sigterm_prev;
  restore Sys.sighup t.sighup_prev

let stop t =
  if t.running then (
    t.running <- false;
    Eio.Condition.broadcast t.resize_cond;
    Eio.Condition.broadcast t.render_cond;
    cleanup t)

(*  Rendering *)

let do_render t dyn_el =
  let render_start = Unix.gettimeofday () in
  let width, height = with_term_mutex t ~f:(fun () -> Tty_eio.size t.term) in
  Log.debug (fun m -> m "Starting render: width=%d height=%d" width height);

  (* Prepare final element for both modes *)
  let final_el =
    if t.static_elements = [] then dyn_el
    else Ui.vbox (t.static_elements @ [ Ui.vbox ~flex_grow:1.0 [ dyn_el ] ])
  in

  (* Clear and prepare snapshot if mouse is enabled *)
  let () =
    match t.snapshot with Some s -> Ui.Layout_snapshot.clear s | None -> ()
  in

  let render_to scr =
    Screen.begin_frame scr;
    Ui.render ?snapshot:t.snapshot scr final_el;
    (* Call the on_snapshot callback if we have a snapshot *)
    match (t.snapshot, t.on_snapshot) with
    | Some snapshot, Some callback -> callback snapshot
    | _ -> ()
  in

  match t.render_mode with
  | Alt_screen rm ->
      (* Ensure we reuse a persistent [Screen.t] so that [present] can diff
         against the previous front buffer. Resize it if the terminal size
         changed; create it on first use *)
      let is_first_frame = Option.is_none rm.previous in
      let scr =
        match rm.previous with
        | Some s when Screen.rows s = height && Screen.cols s = width -> s
        | Some s ->
            Screen.resize s ~rows:height ~cols:width;
            s
        | None -> Screen.create ~rows:height ~cols:width ()
      in
      render_to scr;

      (if is_first_frame then (
         (* First frame: must do a full render since front buffer is empty *)
         let output = Ansi.cursor_position 1 1 ^ Screen.render_to_string scr in
         log_terminal_output output "alt-screen first frame (full render)";
         with_term_mutex t ~f:(fun () ->
             Tty_eio.write_string t.term output;
             Tty_eio.flush t.term))
       else
         (* Subsequent frames: use patches with synchronized updates *)
         let patches = Screen.render scr in
         if patches <> [] then (
           let output = Screen.patches_to_sgr_synchronized patches in
           Log.info (fun m ->
               m "Alt-screen incremental update: %d patches"
                 (List.length patches));
           log_terminal_output output
             (Printf.sprintf "alt-screen patches (%d patches)"
                (List.length patches));
           with_term_mutex t ~f:(fun () ->
               Tty_eio.write_string t.term output;
               Tty_eio.flush t.term))
         else Log.info (fun m -> m "Alt-screen frame skipped: no patches"));

      (* Swap buffers for next frame *)
      let dirty_regions = Screen.present scr in
      let render_elapsed_ms = (Unix.gettimeofday () -. render_start) *. 1000. in
      Log.debug (fun m ->
          m
            "Rendered alt-screen frame with %d dirty regions in %.2fms \
             (buffered)"
            (List.length dirty_regions)
            render_elapsed_ms);
      rm.previous <- Some scr
  | Standard rm ->
      let needs_full =
        match rm.previous_dynamic with
        | None -> true
        | Some s -> Screen.rows s <> height || Screen.cols s <> width
      in
      if needs_full then (
        let scr = Screen.create ~rows:height ~cols:width () in
        render_to scr;
        let output =
          Ansi.cursor_position 1 1 ^ Ansi.clear_screen
          ^ Screen.render_to_string scr
          ^ "\n"
        in
        log_terminal_output output "standard mode full render";
        with_term_mutex t ~f:(fun () ->
            Tty_eio.write_string t.term output;
            Tty_eio.flush t.term);
        let dirty_regions = Screen.present scr in
        let render_elapsed_ms =
          (Unix.gettimeofday () -. render_start) *. 1000.
        in
        Log.debug (fun m ->
            m "Rendered standard frame (full) with %d dirty regions in %.2fms"
              (List.length dirty_regions)
              render_elapsed_ms);
        rm.previous_dynamic <- Some scr)
      else
        let scr = Option.get rm.previous_dynamic in
        render_to scr;
        let patches = Screen.render scr in
        if patches <> [] then (
          let output = Screen.patches_to_sgr_synchronized patches in
          Log.info (fun m ->
              m "Standard mode incremental update: %d patches"
                (List.length patches));
          log_terminal_output output
            (Printf.sprintf "standard mode patches (%d patches)"
               (List.length patches));
          with_term_mutex t ~f:(fun () ->
              Tty_eio.write_string t.term output;
              Tty_eio.flush t.term))
        else Log.info (fun m -> m "Standard mode frame skipped: no patches");
        let dirty_regions = Screen.present scr in
        let render_elapsed_ms =
          (Unix.gettimeofday () -. render_start) *. 1000.
        in
        Log.debug (fun m ->
            m
              "Rendered standard frame (partial) with %d dirty regions in \
               %.2fms"
              (List.length dirty_regions)
              render_elapsed_ms);
        ()

(*  Command execution *)

let process_meta_command t meta =
  match meta with
  | Cmd.Quit -> stop t
  | Cmd.Print el ->
      with_state_mutex t ~f:(fun () ->
          t.static_elements <- t.static_elements @ [ el ];
          match t.render_mode with
          | Standard rm -> rm.previous_dynamic <- None
          | Alt_screen _ -> ())
  | Cmd.Clear_screen ->
      with_term_mutex t ~f:(fun () ->
          let seq = Ansi.clear_screen ^ Ansi.esc ^ "H" in
          Tty_eio.write t.term (Bytes.of_string seq) 0 (String.length seq);
          Tty_eio.flush t.term);
      with_state_mutex t ~f:(fun () ->
          t.static_elements <- [];
          match t.render_mode with Standard _ -> () | Alt_screen _ -> ())
  | Cmd.Clear_terminal ->
      with_term_mutex t ~f:(fun () ->
          let seq = Ansi.clear_screen ^ "\027[3J" ^ Ansi.esc ^ "H" in
          Tty_eio.write t.term (Bytes.of_string seq) 0 (String.length seq);
          Tty_eio.flush t.term);
      with_state_mutex t ~f:(fun () ->
          t.static_elements <- [];
          match t.render_mode with
          | Standard rm -> rm.previous_dynamic <- None
          | Alt_screen _ -> ())
  | Cmd.Set_window_title title ->
      with_term_mutex t ~f:(fun () ->
          let seq = Printf.sprintf "\027]0;%s\007" title in
          Tty_eio.write t.term (Bytes.of_string seq) 0 (String.length seq);
          Tty_eio.flush t.term)
  | Cmd.Enter_alt_screen -> (
      match t.render_mode with
      | Standard _ ->
          with_term_mutex t ~f:(fun () ->
              let seq = "\0277" ^ "\027[?1049h" in
              Tty_eio.write t.term (Bytes.of_string seq) 0 (String.length seq);
              Tty_eio.flush t.term);
          with_state_mutex t ~f:(fun () ->
              t.render_mode <- Alt_screen { previous = None })
      | Alt_screen _ -> ())
  | Cmd.Exit_alt_screen -> (
      match t.render_mode with
      | Alt_screen _ ->
          with_term_mutex t ~f:(fun () ->
              let seq = "\027[?1049l" ^ "\0278" in
              Tty_eio.write t.term (Bytes.of_string seq) 0 (String.length seq);
              Tty_eio.flush t.term);
          with_state_mutex t ~f:(fun () ->
              t.render_mode <- Standard { previous_dynamic = None })
      | Standard _ -> ())
  | Cmd.Repaint -> (
      (* Force re-render by invalidating previous buffers *)
      match t.render_mode with
      | Alt_screen st -> st.previous <- None
      | Standard st -> st.previous_dynamic <- None)
  | Cmd.Log s -> Log.info (fun m -> m "%s" s)
  | Cmd.Tick (duration, f) ->
      Fiber.fork ~sw:t.sw (fun () ->
          let start = Eio.Time.now t.clock in
          Eio.Time.sleep t.clock duration;
          let elapsed = Eio.Time.now t.clock -. start in
          if t.running then f elapsed)
  | Cmd.Perform f -> Fiber.fork ~sw:t.sw (fun () -> f ())
  | Cmd.Perform_eio f -> Fiber.fork ~sw:t.sw (fun () -> f ~sw:t.sw ~env:t.env)

let process_cmd_internal t dispatch cmd =
  (* Run the command and get meta commands *)
  let metas = Cmd.run ~dispatch cmd in
  (* Process the meta commands *)
  List.iter (process_meta_command t) metas

(*  Runtime loops *)

let run_input_loop t on_input =
  match t.event_source with
  | None -> Log.warn (fun m -> m "No event source, input loop not running")
  | Some event_source ->
      while t.running do
        let timeout = Some (1.0 /. float_of_int t.fps) in
        match Event_source.read event_source ~clock:t.clock ~timeout with
        | `Event ev -> on_input ev
        | `Timeout -> ()
      done

let request_render t =
  with_state_mutex t ~f:(fun () ->
      if not t.render_pending then (
        t.render_pending <- true;
        let now = Eio.Time.now t.clock in
        let frame_duration = 1.0 /. float_of_int t.fps in
        let elapsed = now -. t.last_render_time in
        if elapsed >= frame_duration then (
          (* Enough time has passed, render immediately *)
          Log.debug (fun m ->
              m "Render requested - immediate (elapsed: %.3fms)"
                (elapsed *. 1000.));
          t.needs_render <- true;
          t.render_pending <- false;
          t.last_render_time <- now;
          Eio.Condition.broadcast t.render_cond)
        else
          (* Schedule for next frame *)
          let delay = frame_duration -. elapsed in
          Log.debug (fun m ->
              m "Render requested - scheduled (delay: %.3fms)" (delay *. 1000.));
          Fiber.fork ~sw:t.sw (fun () ->
              Eio.Time.sleep t.clock delay;
              with_state_mutex t ~f:(fun () ->
                  t.render_pending <- false;
                  t.needs_render <- true;
                  t.last_render_time <- Eio.Time.now t.clock;
                  Eio.Condition.broadcast t.render_cond)))
      else Log.debug (fun m -> m "Render requested - already pending"))

let force_full_redraw t =
  Log.info (fun m -> m "Full redraw forced");
  with_state_mutex t ~f:(fun () ->
      match t.render_mode with
      | Alt_screen st -> st.previous <- None
      | Standard st -> st.previous_dynamic <- None);
  request_render t

let run_render_loop t get_view =
  Log.debug (fun m -> m "Starting render loop");
  (* initial render *)
  request_render t;
  while t.running do
    (* wait until we're asked to render *)
    with_state_mutex t ~f:(fun () ->
        while t.running && not t.needs_render do
          Eio.Condition.await t.render_cond t.state_mutex
        done;
        t.needs_render <- false);
    if t.running then (
      Log.debug (fun m -> m "Rendering frame");
      do_render t (get_view ()))
  done;
  Log.debug (fun m -> m "Render loop ended")

let run_resize_loop t on_resize =
  while t.running do
    Eio.Condition.await_no_mutex t.resize_cond;
    if t.running then
      let w, h = Tty_eio.size t.term in
      on_resize ~w ~h
  done

let run_tick_loop t tick_cb =
  let frame_duration = 1.0 /. float_of_int t.fps in
  let prev_time = ref (Eio.Time.now t.clock) in
  while t.running do
    let current_time = Eio.Time.now t.clock in
    let elapsed = current_time -. !prev_time in
    prev_time := current_time;

    (* Execute tick callback with actual elapsed time *)
    if t.running then tick_cb ~elapsed;

    (* Calculate time taken for tick_cb and sleep for the remainder *)
    let tick_end = Eio.Time.now t.clock in
    let tick_duration = tick_end -. current_time in
    let sleep_time = frame_duration -. tick_duration in
    if sleep_time > 0.0 then Eio.Time.sleep t.clock sleep_time
  done

(*  Public entry points *)

let start ~(sw : Switch.t) ~(env : Eio_unix.Stdenv.base) cfg ~render ~on_input
    ~(on_resize : w:int -> h:int -> unit) ?on_snapshot ?tick () =
  let prog = create cfg ~sw ~env in
  prog.on_snapshot <- on_snapshot;

  (* Ensure cleanup happens if setup fails *)
  try
    Log.info (fun m -> m "Starting program");
    setup_terminal prog;
    setup_signal_handlers prog;

    (* Create event source AFTER terminal is set up *)
    Log.debug (fun m -> m "Creating event source");
    prog.event_source <-
      Some (Event_source.create ~sw ~env ~mouse:prog.mouse prog.term);

    Log.debug (fun m -> m "Forking fibers");
    Fiber.fork ~sw (fun () ->
        Log.debug (fun m -> m "Input loop fiber started");
        run_input_loop prog on_input);
    Fiber.fork ~sw (fun () ->
        Log.debug (fun m -> m "Render loop fiber started");
        run_render_loop prog (fun () -> render ()));
    Fiber.fork ~sw (fun () ->
        Log.debug (fun m -> m "Resize loop fiber started");
        run_resize_loop prog on_resize);
    (match tick with
    | None -> ()
    | Some cb -> Fiber.fork ~sw (fun () -> run_tick_loop prog cb));

    let process_cmd disp cmd = process_cmd_internal prog disp cmd in
    Log.info (fun m -> m "Program started successfully");
    (prog, process_cmd)
  with exn ->
    (* Emergency cleanup if setup fails *)
    Log.err (fun m -> m "Program start failed: %s" (Printexc.to_string exn));
    (try cleanup prog with _ -> ());
    raise exn

let is_running t = t.running
let get_snapshot t = t.snapshot
