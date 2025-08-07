open Eio.Std

(*  Public configuration type *)

type config = {
  terminal : Tty.t option;
  alt_screen : bool;
  mouse : bool;
  fps : int;
  debug_log : out_channel option;
}

let config ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    ?debug_log () =
  { terminal; alt_screen; mouse; fps; debug_log }

(*  Render‑mode polymorphic state *)

type render_mode =
  | Alt_screen of { mutable previous : Screen.t option }
  | Standard of {
      mutable previous_dynamic : Screen.t option;
      mutable last_dynamic_height : int;
      mutable last_static_height : int;
    }

let is_alt = function Alt_screen _ -> true | Standard _ -> false

(*  Opaque program handle *)

type t = {
  (* mutable flag *)
  mutable running : bool;
  (* terminal & I *)
  term : Tty.t;
  mutable event_source : Event_source.t option;
  (* user confi *)
  mouse : bool;
  fps : int;
  (* rendering stat *)
  render_mode : render_mode;
  mutable static_elements : Ui.element list;
  mutable static_cache : (int * string) option; (* (width, rendered_string) *)
  (* environmen *)
  clock : float Eio.Time.clock_ty r;
  sw : Switch.t;
  env : Eio_unix.Stdenv.base;
  (* mis *)
  debug_log : out_channel option;
  (* synchronisatio *)
  terminal_mutex : Eio.Mutex.t;
  state_mutex : Eio.Mutex.t;
  resize_cond : Eio.Condition.t;
  (* saved signal handler *)
  mutable sigint_prev : Sys.signal_behavior option;
  mutable sigterm_prev : Sys.signal_behavior option;
  mutable sighup_prev : Sys.signal_behavior option;
}

(*  Debug helper *)

let log_debug t msg =
  Option.iter
    (fun ch -> Printf.fprintf ch "[%f] %s\n%!" (Unix.gettimeofday ()) msg)
    t.debug_log

(*  Constructors / helpers *)

let make_terminal ?terminal () =
  match terminal with
  | Some tty -> tty
  | None -> Tty.create ~tty:true Unix.stdin Unix.stdout

let create (cfg : config) ~(sw : Switch.t) ~(env : Eio_unix.Stdenv.base) : t =
  let term = make_terminal ?terminal:cfg.terminal () in
  let render_mode =
    if cfg.alt_screen then Alt_screen { previous = None }
    else
      Standard
        {
          previous_dynamic = None;
          last_dynamic_height = 0;
          last_static_height = 0;
        }
  in
  {
    running = true;
    term;
    event_source = None;
    mouse = cfg.mouse;
    fps = cfg.fps;
    render_mode;
    static_elements = [];
    static_cache = None;
    clock = Eio.Stdenv.clock env;
    sw;
    env;
    debug_log = cfg.debug_log;
    terminal_mutex = Eio.Mutex.create ();
    state_mutex = Eio.Mutex.create ();
    resize_cond = Eio.Condition.create ();
    sigint_prev = None;
    sigterm_prev = None;
    sighup_prev = None;
  }

let with_term_mutex t ~f = Eio.Mutex.use_rw ~protect:true t.terminal_mutex f
let with_state_mutex t ~f = Eio.Mutex.use_rw ~protect:true t.state_mutex f

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
  let seq = build_cleanup_sequence had_alt_screen in
  Tty.write term (Bytes.of_string seq) 0 (String.length seq);
  Tty.set_mode term `Cooked;
  Tty.flush term

let setup_terminal t =
  log_debug t "Setting up terminal";
  with_term_mutex t ~f:(fun () ->
      try
        (* First ensure the terminal is in a clean state *)
        let reset_seq =
          Ansi.mouse_off ^ Ansi.bracketed_paste_off ^ Ansi.kitty_keyboard_off
          ^ Ansi.focus_event_off (* Disable focus events first *)
        in
        Tty.write t.term (Bytes.of_string reset_seq) 0 (String.length reset_seq);
        Tty.flush t.term;

        (* Now set up the terminal for our use *)
        log_debug t "Setting terminal to raw mode";
        Tty.set_mode t.term `Raw;
        log_debug t "Raw mode set";
        Tty.hide_cursor t.term;
        (match t.render_mode with
        | Alt_screen _ -> Tty.enable_alternate_screen t.term
        | _ -> ());
        if t.mouse then Tty.set_mouse_mode t.term `Normal;
        Tty.enable_focus_reporting t.term;
        Tty.enable_kitty_keyboard t.term;
        Tty.enable_bracketed_paste t.term;
        Tty.flush t.term;

        (* Clear any pending input that might have been buffered before raw mode *)
        let input_fd = Tty.input_fd t.term in
        let buf = Bytes.create 256 in
        (try
           Unix.set_nonblock input_fd;
           while Unix.read input_fd buf 0 256 > 0 do
             ()
           done
         with
         | Unix.Unix_error (Unix.EAGAIN, _, _)
         | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
         ->
           ());

        log_debug t "Terminal setup complete"
      with exn ->
        log_debug t
          (Printf.sprintf "Setup terminal failed: %s" (Printexc.to_string exn));
        raise exn)

let resize_handler t _ = Eio.Condition.broadcast t.resize_cond

let setup_signal_handlers t =
  let had_alt = is_alt t.render_mode in
  let emergency _ =
    full_cleanup t.term had_alt;
    exit 130
  in
  t.sigint_prev <- Some (Sys.signal Sys.sigint (Sys.Signal_handle emergency));
  t.sigterm_prev <- Some (Sys.signal Sys.sigterm (Sys.Signal_handle emergency));
  t.sighup_prev <- Some (Sys.signal Sys.sighup (Sys.Signal_handle emergency));
  Tty.set_resize_handler t.term (resize_handler t)

let restore sig_no v =
  Sys.set_signal sig_no (Option.value v ~default:Sys.Signal_default)

let cleanup t =
  Tty.remove_resize_handlers t.term;
  (try
     with_term_mutex t ~f:(fun () -> full_cleanup t.term (is_alt t.render_mode))
   with Eio.Mutex.Poisoned _ -> full_cleanup t.term (is_alt t.render_mode));
  restore Sys.sigint t.sigint_prev;
  restore Sys.sigterm t.sigterm_prev;
  restore Sys.sighup t.sighup_prev

(*  Rendering *)

let screen_to_string = Screen.render_to_string

let render_static t ~width =
  if t.static_elements = [] then ""
  else
    match t.static_cache with
    | Some (cached_width, cached_string) when cached_width = width ->
        cached_string
    | _ ->
        let total_rows =
          List.fold_left
            (fun acc el ->
              let _, h = Ui.measure ~width el in
              acc + h)
            0 t.static_elements
        in
        let scr = Screen.create ~rows:total_rows ~cols:width () in
        Screen.begin_frame scr;
        Ui.render scr (Ui.vbox t.static_elements);
        let rendered = screen_to_string scr ^ "\n" in
        t.static_cache <- Some (width, rendered);
        rendered

let do_render t dyn_el =
  let width, height = with_term_mutex t ~f:(fun () -> Tty.size t.term) in
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
      (* Draw the frame into the back buffer *)
      Screen.begin_frame scr;
      let final_el =
        if t.static_elements = [] then dyn_el
        else Ui.vbox (t.static_elements @ [ Ui.vbox ~flex_grow:1.0 [ dyn_el ] ])
      in
      Ui.render scr final_el;

      (if is_first_frame then
         (* First frame: must do a full render since front buffer is empty *)
         let output = Ansi.cursor_position 1 1 ^ Screen.render_to_string scr in
         with_term_mutex t ~f:(fun () ->
             Tty.write t.term (Bytes.of_string output) 0 (String.length output);
             Tty.flush t.term)
       else
         (* Subsequent frames: use patches *)
         let patches = Screen.render scr in
         if patches <> [] then
           let output = Screen.patches_to_sgr patches in
           with_term_mutex t ~f:(fun () ->
               Tty.write t.term (Bytes.of_string output) 0
                 (String.length output);
               Tty.flush t.term));

      (* Swap buffers for next frame *)
      let _dirty_regions = Screen.present scr in
      rm.previous <- Some scr
  | Standard rm ->
      (* Use incremental diffing for standard screen mode *)
      let static_out = render_static t ~width in

      let needs_full_redraw =
        match rm.previous_dynamic with
        | None -> true
        | Some prev -> Screen.rows prev <> height || Screen.cols prev <> width
      in

      if needs_full_redraw then (
        (* Full redraw on size change or first render *)
        let dyn_scr = Screen.create ~rows:height ~cols:width () in
        Screen.begin_frame dyn_scr;
        Ui.render dyn_scr dyn_el;
        let dyn_out = Screen.render_to_string dyn_scr in
        let output =
          Ansi.cursor_position 1 1 ^ Ansi.clear_screen ^ static_out ^ dyn_out
          ^ "\n"
        in
        with_term_mutex t ~f:(fun () ->
            Tty.write t.term (Bytes.of_string output) 0 (String.length output);
            Tty.flush t.term);
        (* Ensure Screen state is consistent for future incremental updates *)
        let _ = Screen.present dyn_scr in
        rm.previous_dynamic <- Some dyn_scr;
        rm.last_dynamic_height <- Screen.rows dyn_scr;
        rm.last_static_height <- Screen.rows dyn_scr)
      else
        (* Incremental update *)
        let scr = Option.get rm.previous_dynamic in
        Screen.begin_frame scr;
        Ui.render scr dyn_el;
        let patches = Screen.render scr in
        if patches <> [] then (
          let output = Screen.patches_to_sgr patches in
          with_term_mutex t ~f:(fun () ->
              Tty.write t.term (Bytes.of_string output) 0 (String.length output);
              Tty.flush t.term);
          let _dirty_regions = Screen.present scr in
          ())

(*  Command execution *)

let process_meta_command t meta =
  match meta with
  | Cmd.Quit -> t.running <- false
  | Cmd.Print el ->
      with_state_mutex t ~f:(fun () ->
          t.static_elements <- t.static_elements @ [ el ];
          t.static_cache <- None)
  | Cmd.Clear_screen ->
      with_term_mutex t ~f:(fun () ->
          let seq = Ansi.clear_screen ^ Ansi.esc ^ "H" in
          Tty.write t.term (Bytes.of_string seq) 0 (String.length seq);
          Tty.flush t.term);
      with_state_mutex t ~f:(fun () ->
          t.static_elements <- [];
          t.static_cache <- None;
          match t.render_mode with
          | Standard rm -> rm.last_static_height <- 0
          | Alt_screen _ -> ())
  | Cmd.Clear_terminal ->
      with_term_mutex t ~f:(fun () ->
          let seq = Ansi.clear_screen ^ "\027[3J" ^ Ansi.esc ^ "H" in
          Tty.write t.term (Bytes.of_string seq) 0 (String.length seq);
          Tty.flush t.term);
      with_state_mutex t ~f:(fun () ->
          t.static_elements <- [];
          t.static_cache <- None)
  | Cmd.Set_window_title title ->
      with_term_mutex t ~f:(fun () ->
          let seq = Printf.sprintf "\027]0;%s\007" title in
          Tty.write t.term (Bytes.of_string seq) 0 (String.length seq);
          Tty.flush t.term)
  | Cmd.Enter_alt_screen ->
      if not (is_alt t.render_mode) then
        with_term_mutex t ~f:(fun () ->
            let seq = "\0277" ^ "\027[?1049h" in
            Tty.write t.term (Bytes.of_string seq) 0 (String.length seq);
            Tty.flush t.term)
  | Cmd.Exit_alt_screen ->
      if is_alt t.render_mode then
        with_term_mutex t ~f:(fun () ->
            let seq = "\027[?1049l" ^ "\0278" in
            Tty.write t.term (Bytes.of_string seq) 0 (String.length seq);
            Tty.flush t.term)
  | Cmd.Repaint -> (
      (* Force re-render by invalidating previous buffers *)
      match t.render_mode with
      | Alt_screen st -> st.previous <- None
      | Standard st -> st.previous_dynamic <- None)
  | Cmd.Log s -> log_debug t s
  | Cmd.Tick (duration, f) ->
      Fiber.fork ~sw:t.sw (fun () ->
          let start = Eio.Time.now t.clock in
          Eio.Time.sleep t.clock duration;
          let elapsed = Eio.Time.now t.clock -. start in
          if t.running then f elapsed)
  | Cmd.Perform_eio f -> Fiber.fork ~sw:t.sw (fun () -> f ~sw:t.sw ~env:t.env)

let process_cmd_internal t dispatch cmd =
  (* Run the command and get meta commands *)
  let metas = Cmd.run ~dispatch cmd in
  (* Process the meta commands *)
  List.iter (process_meta_command t) metas

(*  Runtime loops *)

let run_input_loop t on_input =
  match t.event_source with
  | None -> log_debug t "No event source, input loop not running"
  | Some event_source ->
      while t.running do
        let timeout = Some (1.0 /. float_of_int t.fps) in
        match Event_source.read event_source ~clock:t.clock ~timeout with
        | `Event ev -> on_input ev
        | `Timeout -> ()
      done

let run_render_loop t get_view =
  log_debug t "Starting render loop";
  while t.running do
    log_debug t "Rendering frame";
    do_render t (get_view ());
    Eio.Time.sleep t.clock (1.0 /. float_of_int t.fps)
  done;
  log_debug t "Render loop ended"

let run_resize_loop t on_resize =
  while t.running do
    Eio.Condition.await_no_mutex t.resize_cond;
    if t.running then
      let w, h = Tty.size t.term in
      on_resize ~w ~h
  done

let run_tick_loop t tick_cb =
  while t.running do
    let start = Eio.Time.now t.clock in
    Eio.Time.sleep t.clock (1.0 /. float_of_int t.fps);
    let elapsed = Eio.Time.now t.clock -. start in
    if t.running then tick_cb ~elapsed
  done

(*  Public entry points *)

let start ~(sw : Switch.t) ~(env : Eio_unix.Stdenv.base) cfg ~render ~on_input
    ~(on_resize : w:int -> h:int -> unit) ?tick () =
  let prog = create cfg ~sw ~env in

  (* Ensure cleanup happens if setup fails *)
  try
    log_debug prog "Starting program";
    setup_terminal prog;
    setup_signal_handlers prog;

    (* Create event source AFTER terminal is set up *)
    log_debug prog "Creating event source";
    prog.event_source <-
      Some (Event_source.create ~sw ~env ~mouse:prog.mouse prog.term);

    log_debug prog "Forking fibers";
    Fiber.fork ~sw (fun () ->
        log_debug prog "Input loop fiber started";
        run_input_loop prog on_input);
    Fiber.fork ~sw (fun () ->
        log_debug prog "Render loop fiber started";
        run_render_loop prog (fun () -> render ()));
    Fiber.fork ~sw (fun () ->
        log_debug prog "Resize loop fiber started";
        run_resize_loop prog on_resize);
    (match tick with
    | None -> ()
    | Some cb -> Fiber.fork ~sw (fun () -> run_tick_loop prog cb));

    let process_cmd disp cmd = process_cmd_internal prog disp cmd in
    log_debug prog "Program started successfully";
    (prog, process_cmd)
  with exn ->
    (* Emergency cleanup if setup fails *)
    log_debug prog
      (Printf.sprintf "Program start failed: %s" (Printexc.to_string exn));
    (try cleanup prog with _ -> ());
    raise exn

let stop t =
  if t.running then (
    t.running <- false;
    Eio.Condition.broadcast t.resize_cond;
    cleanup t)

let is_running t = t.running

let request_render t =
  match t.render_mode with
  | Alt_screen st -> st.previous <- None
  | Standard st -> st.previous_dynamic <- None
