module Ansi = Ansi
module Glyph = Glyph
module Grid = Grid
module Input = Input
module Screen = Screen
module Terminal = Terminal
module Image = Image

(* App State & Types *)

type kitty_keyboard = [ `Auto | `Disabled | `Enabled of int ]
type mode = [ `Alt | `Primary ]
type debug_overlay_corner = Debug_overlay.corner

type control_state =
  [ `Idle
  | `Auto_started
  | `Explicit_started
  | `Explicit_paused
  | `Explicit_suspended
  | `Explicit_stopped ]

type app = {
  terminal : Terminal.t;
  screen : Screen.t;
  mutable caps : Terminal.capabilities;
  mutable running : bool;
  mutable redraw_requested : bool;
  mutable alt_enabled : bool;
  mutable bracketed_paste_enabled : bool;
  mutable focus_enabled : bool;
  mutable kitty_enabled : bool;
  mutable modify_other_keys_enabled : bool;
  mutable unicode_enabled : bool;
  mutable width : int;
  mutable height : int;
  output : [ `Stdout | `Fd of Unix.file_descr ];
  (* Primary mode layout - region model *)
  mutable render_offset : int;
  mutable tui_height : int;
  mutable prev_render_offset : int;
  mutable prev_tui_height : int;
  mutable static_needs_newline : bool;
  mutable static_queue : string list;
  mutable in_submit : bool;
  mutable needs_region_clear : bool;
  (* Resize and frame timing *)
  mutable last_resize_apply_time : float;
  mutable pending_resize : (int * int) option;
  mutable force_full_next_frame : bool;
  mutable next_frame_deadline : float option;
  (* Config *)
  mode : mode;
  raw_mode : bool;
  mouse_mode : Terminal.mouse_mode option;
  bracketed_paste : bool;
  focus_reporting : bool;
  kitty_keyboard : kitty_keyboard;
  exit_on_ctrl_c : bool;
  target_fps : float option;
  explicit_width : bool;
  input_timeout : float option;
  resize_debounce : float option;
  (* Diagnostics *)
  mutable debug_overlay_enabled : bool;
  debug_overlay_capacity : int;
  mutable debug_overlay_cb : Screen.t -> unit;
  mutable frame_dump_every : int;
  mutable frame_dump_dir : string option;
  mutable frame_dump_pattern : string option;
  mutable frame_dump_hits : bool;
  mutable frame_dump_counter : int;
  mutable last_frame_callback_ms : float;
  mutable signal_handler : (unit -> unit) option;
  mutable closed : bool;
  mutable loop_active : bool;
  mutable control_state : control_state;
  mutable previous_control_state : control_state;
  mutable live_requests : int;
}

(* Shutdown handler registry for graceful cleanup. Apps register cleanup
   functions that run on exit or signal. *)
let shutdown_handlers : (unit -> unit) list ref = ref []
let shutdown_mutex = Mutex.create ()
let shutdown_triggered = ref false
let signal_handlers_installed = ref false

let run_shutdown_handlers () =
  let handlers =
    Mutex.lock shutdown_mutex;
    let already_triggered = !shutdown_triggered in
    if already_triggered then (
      Mutex.unlock shutdown_mutex;
      None)
    else (
      shutdown_triggered := true;
      let handlers = !shutdown_handlers in
      Mutex.unlock shutdown_mutex;
      Some handlers)
  in
  match handlers with
  | None -> ()
  | Some handlers -> List.iter (fun f -> try f () with _ -> ()) handlers

let register_shutdown_handler fn =
  Mutex.lock shutdown_mutex;
  shutdown_handlers := fn :: !shutdown_handlers;
  Mutex.unlock shutdown_mutex

let deregister_shutdown_handler fn =
  Mutex.lock shutdown_mutex;
  shutdown_handlers := List.filter (fun f -> f != fn) !shutdown_handlers;
  Mutex.unlock shutdown_mutex

let shutdown_signal_handler signum =
  run_shutdown_handlers ();
  exit (128 + signum)

(* Helper to safely set signal handlers - on Windows most signals are
   unavailable *)
let try_set_signal sig_num handler =
  try Sys.set_signal sig_num handler with Invalid_argument _ -> ()

let install_signal_handlers () =
  if not !signal_handlers_installed then (
    signal_handlers_installed := true;
    (* On Windows, only sigint is reliably available *)
    try_set_signal Sys.sigterm (Sys.Signal_handle shutdown_signal_handler);
    try_set_signal Sys.sigint (Sys.Signal_handle shutdown_signal_handler);
    try_set_signal Sys.sigquit (Sys.Signal_handle shutdown_signal_handler);
    try_set_signal Sys.sigabrt (Sys.Signal_handle shutdown_signal_handler);
    Printexc.set_uncaught_exception_handler (fun exn ->
        prerr_endline (Printexc.to_string exn);
        run_shutdown_handlers ();
        exit 1))

(* Only register at_exit which is benign and ensures cleanup on normal exit.
   Signal handlers must be explicitly installed via install_signal_handlers. *)
let () = at_exit run_shutdown_handlers

(* Helpers & Accessors *)

let clamp lo hi v = if v < lo then lo else if v > hi then hi else v

let output_is_tty t =
  try Unix.isatty (Terminal.output_fd t.terminal) with _ -> false

let min_static_rows t ~height ~output_is_tty =
  if t.mode = `Primary && output_is_tty && height > 1 then 1 else 0

let apply_primary_region t ~render_offset ~resize =
  let height = max 1 t.height in
  let is_tty = output_is_tty t in
  let min_static = min_static_rows t ~height ~output_is_tty:is_tty in
  let render_offset = clamp min_static (height - 1) render_offset in
  let tui_height = max 1 (height - render_offset) in
  t.render_offset <- render_offset;
  t.tui_height <- tui_height;
  if render_offset > 0 then
    Terminal.set_scroll_region t.terminal ~top:1 ~bottom:render_offset
  else Terminal.clear_scroll_region t.terminal;
  Screen.set_row_offset t.screen render_offset;
  if resize then Screen.resize t.screen ~width:t.width ~height:tui_height

let size t =
  match t.mode with
  | `Alt -> (t.width, t.height)
  | `Primary -> (t.width, t.tui_height)

(* Mouse offset: number of static rows above the TUI. *)
let mouse_offset t = if t.mode = `Primary then t.render_offset else 0
let pixel_resolution t = Terminal.pixel_resolution t.terminal
let terminal t = t.terminal
let capabilities t = t.caps
let running t = t.running
let wake_loop t = Terminal.wake t.terminal

let request_redraw t =
  if t.control_state <> `Explicit_suspended then (
    t.redraw_requested <- true;
    wake_loop t)

let compute_loop_interval t =
  match t.target_fps with
  | Some fps when fps > 0. -> Some (1. /. fps)
  | _ -> None

let set_loop_active t active =
  if t.loop_active <> active then (
    t.loop_active <- active;
    if not active then t.next_frame_deadline <- None
    else
      let now = Unix.gettimeofday () in
      t.next_frame_deadline <- Some now)

let update_feature ~cond ~desired ~enabled ~enable ~disable =
  if not cond then (
    if enabled then disable ();
    false)
  else if desired then (
    if not enabled then enable ();
    true)
  else (
    if enabled then disable ();
    false)

(* Invalidate inline state - force a full repaint and clear the UI region on the
   next frame. Used on resize and layout changes. *)
let invalidate_inline_state t =
  t.force_full_next_frame <- true;
  t.needs_region_clear <- true

(* Forget inline height - use when we truly lost control of the display
   (suspend, external output, etc.) and need a full repaint. *)
let forget_inline_height t =
  t.force_full_next_frame <- true;
  t.needs_region_clear <- true

let refresh_capabilities t =
  let caps = Terminal.capabilities t.terminal in
  t.caps <- caps;
  Screen.apply_capabilities t.screen ~explicit_width:caps.explicit_width
    ~hyperlinks:caps.hyperlinks

let refresh_render_region t =
  match t.mode with
  | `Alt ->
      Terminal.clear_scroll_region t.terminal;
      t.render_offset <- 0;
      t.tui_height <- t.height;
      Screen.set_row_offset t.screen 0;
      Screen.resize t.screen ~width:t.width ~height:t.height
  | `Primary ->
      apply_primary_region t ~render_offset:t.render_offset ~resize:true

(* In raw mode, \n doesn't imply CR, so use explicit CRLF for line breaks *)
let crlf = "\r\n"
let erase_entire_line = Ansi.(to_string (erase_line ~mode:`All))

let ensure_trailing_newline s =
  let len = String.length s in
  if len > 0 && Char.equal (String.unsafe_get s (len - 1)) '\n' then s
  else s ^ crlf

let starts_with_newline s =
  let len = String.length s in
  if len = 0 then false
  else if Char.equal (String.unsafe_get s 0) '\n' then true
  else
    len > 1
    && Char.equal (String.unsafe_get s 0) '\r'
    && Char.equal (String.unsafe_get s 1) '\n'

let ends_with_newline s =
  let len = String.length s in
  len > 0 && Char.equal (String.unsafe_get s (len - 1)) '\n'

let normalize_newlines s =
  let len = String.length s in
  let rec count i acc =
    if i >= len then acc
    else
      let c = String.unsafe_get s i in
      if Char.equal c '\n' then
        if i > 0 && Char.equal (String.unsafe_get s (i - 1)) '\r' then
          count (i + 1) acc
        else count (i + 1) (acc + 1)
      else count (i + 1) acc
  in
  let extra = count 0 0 in
  if extra = 0 then s
  else
    let bytes = Bytes.create (len + extra) in
    let rec fill i j =
      if i >= len then ()
      else
        let c = String.unsafe_get s i in
        if
          Char.equal c '\n'
          && not (i > 0 && Char.equal (String.unsafe_get s (i - 1)) '\r')
        then (
          Bytes.unsafe_set bytes j '\r';
          Bytes.unsafe_set bytes (j + 1) '\n';
          fill (i + 1) (j + 2))
        else (
          Bytes.unsafe_set bytes j c;
          fill (i + 1) (j + 1))
    in
    fill 0 0;
    Bytes.unsafe_to_string bytes

let static_write_raw_immediate t text =
  if t.mode = `Alt || String.length text = 0 then ()
  else begin
    (* Primary mode: write into the static scroll region when available. *)
    let prev_render_offset = t.render_offset in
    let prev_tui_height = t.tui_height in
    apply_primary_region t ~render_offset:t.render_offset ~resize:false;
    if t.render_offset <> prev_render_offset || t.tui_height <> prev_tui_height
    then (
      t.needs_region_clear <- true;
      t.force_full_next_frame <- true);
    let term = t.terminal in
    let text = if t.raw_mode then normalize_newlines text else text in
    let needs_leading_newline =
      t.static_needs_newline && not (starts_with_newline text)
    in
    if t.render_offset > 0 then (
      Terminal.move_cursor term ~row:t.render_offset ~col:1
        ~visible:(Terminal.cursor_visible term);
      if needs_leading_newline then Terminal.write term crlf;
      Terminal.write term text;
      Terminal.flush term)
    else (
      (* No static region available: let output scroll the whole screen and
         force a full repaint of the UI region. *)
      Terminal.move_cursor term ~row:t.height ~col:1
        ~visible:(Terminal.cursor_visible term);
      if needs_leading_newline then Terminal.write term crlf;
      Terminal.write term text;
      Terminal.flush term;
      Screen.invalidate_presented t.screen;
      forget_inline_height t);
    t.static_needs_newline <- not (ends_with_newline text);
    request_redraw t
  end

let enqueue_static t text = t.static_queue <- text :: t.static_queue

let flush_static_queue t =
  match t.static_queue with
  | [] -> ()
  | rev ->
      t.static_queue <- [];
      List.iter (fun text -> static_write_raw_immediate t text) (List.rev rev)

let static_write_raw t text =
  if t.mode = `Alt || String.length text = 0 then ()
  else if t.in_submit then (
    enqueue_static t text;
    request_redraw t)
  else static_write_raw_immediate t text

let static_write t text = static_write_raw t text
let static_print t text = static_write_raw t (ensure_trailing_newline text)

let static_clear t =
  if t.mode = `Alt then ()
  else (
    Terminal.write t.terminal Ansi.(to_string clear_and_home);
    let cols, rows = Terminal.size t.terminal in
    t.width <- max 1 cols;
    t.height <- max 1 rows;
    let min_static =
      min_static_rows t ~height:t.height ~output_is_tty:(output_is_tty t)
    in
    t.render_offset <- min_static;
    t.tui_height <- max 1 (t.height - t.render_offset);
    t.static_queue <- [];
    t.static_needs_newline <- false;
    forget_inline_height t;
    refresh_render_region t;
    request_redraw t)

(* Lifecycle: Prepare / Grid / Submit *)

let prepare t =
  refresh_capabilities t;
  t.redraw_requested <- false;
  refresh_render_region t;
  let g = Screen.grid t.screen in
  Grid.clear g;
  Screen.Hit_grid.clear (Screen.hit_grid t.screen)

let grid t = Screen.grid t.screen
let hits t = Screen.hit_grid t.screen

let apply_debug_overlay t frame =
  if t.debug_overlay_enabled then t.debug_overlay_cb frame

let maybe_dump_frame t frame =
  if t.frame_dump_every > 0 then (
    t.frame_dump_counter <- t.frame_dump_counter + 1;
    if t.frame_dump_counter mod t.frame_dump_every = 0 then
      Frame_dump.snapshot ?dir:t.frame_dump_dir ?pattern:t.frame_dump_pattern
        ~hits:t.frame_dump_hits frame)

let submit t =
  if not t.running then ()
  else
    let overall_start = Unix.gettimeofday () in
    let is_tty = output_is_tty t in
    t.in_submit <- true;
    Fun.protect
      ~finally:(fun () -> t.in_submit <- false)
      (fun () ->
        (* 1. Apply overlay first so we render the final frame *)
        apply_debug_overlay t t.screen;
        let cursor = Screen.cursor_info t.screen in

        (* 2. Output Synchronization - start early to include clears *)
        let use_sync = is_tty && t.caps.sync in

        (* All output goes through Terminal to ensure proper serialization and
           prevent interleaving between frame writes and control sequences. *)
        let send s = Terminal.write t.terminal s in

        (* Begin sync bracket early to include clears (prevents flicker) *)
        if use_sync then send Ansi.(to_string (enable Sync_output));

        (* Hide cursor during render to avoid flicker. *)
        if Terminal.cursor_visible t.terminal then
          Terminal.set_cursor_visible t.terminal false;

        flush_static_queue t;

        let height = max 1 t.height in
        let required_rows = max 1 (Grid.height (Screen.grid t.screen)) in
        let render_height_limit = ref None in
        let row_offset_changed = ref false in
        let clipped = ref false in

        (match t.mode with
        | `Primary ->
            let min_static = min_static_rows t ~height ~output_is_tty:is_tty in
            let max_ui_rows = max 1 (height - min_static) in
            let target_rows = min required_rows max_ui_rows in
            if required_rows > max_ui_rows then (
              clipped := true;
              render_height_limit := Some max_ui_rows);
            let render_offset = clamp min_static (height - 1) t.render_offset in
            if render_offset <> t.render_offset then (
              t.render_offset <- render_offset;
              t.tui_height <- max 1 (height - render_offset);
              if render_offset > 0 then
                Terminal.set_scroll_region t.terminal ~top:1
                  ~bottom:render_offset
              else Terminal.clear_scroll_region t.terminal;
              Screen.set_row_offset t.screen render_offset;
              row_offset_changed := true);
            if target_rows > t.tui_height then (
              let new_render_offset = height - target_rows in
              let delta = t.render_offset - new_render_offset in
              if delta > 0 && is_tty && t.render_offset > 0 then (
                Terminal.set_scroll_region t.terminal ~top:1
                  ~bottom:t.render_offset;
                send Ansi.(to_string (scroll_up ~n:delta)));
              t.render_offset <- new_render_offset;
              t.tui_height <- target_rows;
              if new_render_offset > 0 then
                Terminal.set_scroll_region t.terminal ~top:1
                  ~bottom:new_render_offset
              else Terminal.clear_scroll_region t.terminal;
              Screen.set_row_offset t.screen new_render_offset;
              row_offset_changed := true)
        | `Alt -> ());

        if !row_offset_changed then t.needs_region_clear <- true;

        (* 3. Primary mode: clear stale region if needed *)
        (match t.mode with
        | `Primary when t.needs_region_clear ->
            if is_tty then (
              let clear_lines ~start ~count =
                for row = start to start + count - 1 do
                  Terminal.move_cursor t.terminal ~row ~col:1
                    ~visible:(Terminal.cursor_visible t.terminal);
                  send erase_entire_line
                done
              in
              if t.prev_tui_height > 0 && t.render_offset > t.prev_render_offset
              then
                clear_lines ~start:(t.prev_render_offset + 1)
                  ~count:(t.render_offset - t.prev_render_offset);
              if t.tui_height > 0 then
                clear_lines ~start:(t.render_offset + 1) ~count:t.tui_height);
            Screen.invalidate_presented t.screen;
            t.needs_region_clear <- false
        | _ -> ());

        (* 4. Render to Bytes - use diff rendering for efficiency. *)
        let forced_full =
          let ff = t.force_full_next_frame in
          if ff then t.force_full_next_frame <- false;
          ff || !row_offset_changed || !clipped
        in
        let render_buf = Terminal.render_buffer t.terminal in
        let len =
          Screen.render_to_bytes ~full:forced_full
            ?height_limit:!render_height_limit t.screen render_buf
        in

        (* 5. Present *)
        let stdout_start = Unix.gettimeofday () in
        if len > 0 then Terminal.present t.terminal len;

        (* 6. Restore cursor state *)
        let cursor_max_row =
          match !render_height_limit with
          | Some limit -> max 1 limit
          | None -> t.tui_height
        in
        if cursor.has_position then
          let row = min cursor_max_row (max 1 cursor.row) in
          Terminal.move_cursor t.terminal ~row:(t.render_offset + row)
            ~col:(max 1 cursor.col) ~visible:cursor.visible
        else if cursor.visible && t.mode = `Primary then
          Terminal.move_cursor t.terminal
            ~row:(t.render_offset + cursor_max_row)
            ~col:1 ~visible:true
        else Terminal.set_cursor_visible t.terminal cursor.visible;
        Terminal.set_cursor_style t.terminal cursor.style
          ~blinking:cursor.blinking;
        (match cursor.color with
        | Some (r, g, b) ->
            let to_float v = Float.of_int v /. 255. in
            Terminal.set_cursor_color t.terminal ~r:(to_float r) ~g:(to_float g)
              ~b:(to_float b) ~a:1.
        | None -> Terminal.reset_cursor_color t.terminal);

        (* End sync bracket after all frame-mutating writes *)
        if use_sync then (
          send Ansi.(to_string (disable Sync_output));
          Terminal.drain t.terminal);

        (match t.output with
        | `Stdout -> Terminal.flush t.terminal
        | `Fd _ -> ());

        let stdout_end = Unix.gettimeofday () in
        let stdout_ms = Float.max 0. ((stdout_end -. stdout_start) *. 1000.) in
        let overall_frame_ms =
          Float.max 0. ((stdout_end -. overall_start) *. 1000.)
        in

        (* 7. Update inline tracking for next frame *)
        (match t.mode with
        | `Primary ->
            t.prev_render_offset <- t.render_offset;
            t.prev_tui_height <- t.tui_height
        | `Alt ->
            t.prev_render_offset <- 0;
            t.prev_tui_height <- t.height);

        (* 8. Metrics & Dumps *)
        Screen.record_runtime_metrics t.screen
          ~frame_callback_ms:t.last_frame_callback_ms ~overall_frame_ms
          ~stdout_ms;
        maybe_dump_frame t t.screen)

let apply_config t =
  refresh_capabilities t;
  let term = t.terminal in
  let caps = t.caps in
  let desired_mode = if t.raw_mode then `Raw else `Cooked in
  if Terminal.mode term <> desired_mode then
    Terminal.switch_mode term desired_mode;
  t.alt_enabled <-
    update_feature ~cond:true ~desired:(t.mode = `Alt) ~enabled:t.alt_enabled
      ~enable:(fun () -> Terminal.enter_alternate_screen term)
      ~disable:(fun () -> Terminal.leave_alternate_screen term);
  if t.alt_enabled then Screen.set_cursor_visible t.screen false;
  t.force_full_next_frame <- true;
  (match t.mouse_mode with
  | Some mode -> Terminal.set_mouse_mode term mode
  | None -> Terminal.set_mouse_mode term `Off);
  t.bracketed_paste_enabled <-
    update_feature ~cond:true ~desired:t.bracketed_paste
      ~enabled:t.bracketed_paste_enabled
      ~enable:(fun () -> Terminal.enable_bracketed_paste term true)
      ~disable:(fun () -> Terminal.enable_bracketed_paste term false);
  t.focus_enabled <-
    update_feature ~cond:caps.focus_tracking ~desired:t.focus_reporting
      ~enabled:t.focus_enabled
      ~enable:(fun () -> Terminal.enable_focus_reporting term true)
      ~disable:(fun () -> Terminal.enable_focus_reporting term false);
  t.kitty_enabled <-
    (let cond, desired, enable =
       match t.kitty_keyboard with
       | `Disabled -> (true, false, fun () -> ())
       | `Auto ->
           ( caps.kitty_keyboard,
             caps.kitty_keyboard,
             fun () -> Terminal.enable_kitty_keyboard term true )
       | `Enabled flags ->
           ( true,
             true,
             fun () -> Terminal.enable_kitty_keyboard ~flags term true )
     in
     update_feature ~cond ~desired ~enabled:t.kitty_enabled ~enable
       ~disable:(fun () -> Terminal.enable_kitty_keyboard term false));
  let desired_mok = not t.kitty_enabled in
  t.modify_other_keys_enabled <-
    update_feature ~cond:true ~desired:desired_mok
      ~enabled:t.modify_other_keys_enabled
      ~enable:(fun () -> Terminal.enable_modify_other_keys term true)
      ~disable:(fun () -> Terminal.enable_modify_other_keys term false);
  let unicode_supported =
    match caps.unicode_width with `Unicode -> true | `Wcwidth -> false
  in
  let using_explicit = t.explicit_width in
  t.unicode_enabled <-
    update_feature
      ~cond:(unicode_supported && not using_explicit)
      ~desired:true ~enabled:t.unicode_enabled
      ~enable:(fun () -> Terminal.set_unicode_width term `Unicode)
      ~disable:(fun () -> Terminal.set_unicode_width term `Wcwidth);
  refresh_render_region t

let stop t =
  if not t.closed then (
    t.running <- false;
    t.control_state <- `Explicit_stopped;
    set_loop_active t false;
    t.next_frame_deadline <- None;
    t.redraw_requested <- false;
    wake_loop t)

let request_live t =
  if t.closed then ()
  else (
    t.live_requests <- t.live_requests + 1;
    if t.control_state = `Idle && t.live_requests > 0 then (
      t.control_state <- `Auto_started;
      set_loop_active t true;
      t.redraw_requested <- true;
      wake_loop t))

let drop_live t =
  t.live_requests <- max 0 (t.live_requests - 1);
  if t.control_state = `Auto_started && t.live_requests = 0 then (
    t.control_state <- `Idle;
    set_loop_active t false)

let start t =
  if not t.closed then (
    t.control_state <- `Explicit_started;
    set_loop_active t true;
    if not t.running then t.running <- true;
    t.redraw_requested <- true;
    wake_loop t)

let auto t = t.control_state <- (if t.loop_active then `Auto_started else `Idle)

let pause t =
  t.control_state <- `Explicit_paused;
  set_loop_active t false

let suspend t =
  t.previous_control_state <- t.control_state;
  t.control_state <- `Explicit_suspended;
  set_loop_active t false;
  t.redraw_requested <- false;
  forget_inline_height t;
  (try Terminal.set_mouse_mode t.terminal `Off with _ -> ());
  (try Terminal.enable_bracketed_paste t.terminal false with _ -> ());
  (try Terminal.enable_focus_reporting t.terminal false with _ -> ());
  (try Terminal.enable_kitty_keyboard t.terminal false with _ -> ());
  (try Terminal.enable_modify_other_keys t.terminal false with _ -> ());
  (try Terminal.switch_mode t.terminal `Cooked with _ -> ());
  try Terminal.flush_input t.terminal with _ -> ()

let resume t =
  if t.control_state <> `Explicit_suspended then ()
  else (
    (if t.raw_mode then try Terminal.switch_mode t.terminal `Raw with _ -> ());
    (try Terminal.flush_input t.terminal with _ -> ());
    (if t.mode = `Primary then
       let height = max 1 t.height in
       match Terminal.query_cursor_position ~timeout:0.1 t.terminal with
       | Some (row, col) ->
           let row = clamp 1 height row in
           let col = max 1 col in
           let render_offset, static_needs_newline =
             if row >= height then (
               Terminal.write t.terminal crlf;
               Terminal.flush t.terminal;
               (height - 1, false))
             else (row, col <> 1)
           in
           let tui_height = max 1 (height - render_offset) in
           t.render_offset <- render_offset;
           t.tui_height <- tui_height;
           t.prev_render_offset <- render_offset;
           t.prev_tui_height <- tui_height;
           t.static_needs_newline <- static_needs_newline
       | None -> ());
    forget_inline_height t;
    apply_config t;
    Grid.clear (Screen.grid t.screen);
    Screen.Hit_grid.clear (Screen.hit_grid t.screen);
    let next_state = t.previous_control_state in
    t.control_state <- next_state;
    let should_run =
      match next_state with
      | `Explicit_started -> true
      | `Auto_started -> t.live_requests > 0
      | _ -> false
    in
    set_loop_active t should_run;
    if should_run then (
      t.redraw_requested <- true;
      wake_loop t)
    else request_redraw t)

let set_cursor ?visible ?style t =
  let term = t.terminal in
  Option.iter
    (fun v ->
      Terminal.set_cursor_visible term v;
      Screen.set_cursor_visible t.screen v)
    visible;
  Option.iter
    (fun style ->
      let _, blinking = Terminal.cursor_style_state term in
      Terminal.set_cursor_style term style ~blinking;
      Screen.set_cursor_style t.screen ~style ~blinking)
    style

let set_cursor_style t ~style ~blinking =
  Terminal.set_cursor_style t.terminal style ~blinking;
  Screen.set_cursor_style t.screen ~style ~blinking

let set_cursor_position t ~row ~col =
  (* Use mouse_offset for absolute terminal position *)
  let max_row =
    if t.mode = `Primary then max 1 t.tui_height else max 1 t.height
  in
  let row = min max_row (max 1 row) in
  let target_row = mouse_offset t + row in
  let target_col = max 1 col in
  Screen.set_cursor_position t.screen ~row ~col:target_col;
  Terminal.move_cursor t.terminal ~row:target_row ~col:target_col
    ~visible:(Terminal.cursor_visible t.terminal)

let set_cursor_color t ~r ~g ~b ~a =
  let clamp_01 f = Float.max 0. (Float.min 1. f) in
  let r_f = clamp_01 r and g_f = clamp_01 g and b_f = clamp_01 b in
  Terminal.set_cursor_color t.terminal ~r:r_f ~g:g_f ~b:b_f ~a:(clamp_01 a);
  let to_byte f = int_of_float (Float.round (f *. 255.)) |> clamp 0 255 in
  Screen.set_cursor_color t.screen ~r:(to_byte r_f) ~g:(to_byte g_f)
    ~b:(to_byte b_f)

let apply_resize t cols rows now =
  if cols <= 0 || rows <= 0 then ()
  else if t.width = cols && t.height = rows then ()
  else (
    t.width <- cols;
    t.height <- rows;
    Terminal.query_pixel_resolution t.terminal;
    invalidate_inline_state t;
    refresh_render_region t;
    t.last_resize_apply_time <- now;
    t.pending_resize <- None;
    request_redraw t)

let handle_resize t cols rows =
  let now = Unix.gettimeofday () in
  match t.resize_debounce with
  | None -> apply_resize t cols rows now
  | Some window_s ->
      (* Resize events arrive in bursts while the user drags; apply the first
         immediately to keep input responsive, then debounce subsequent ones. *)
      if
        t.last_resize_apply_time = 0.
        || now -. t.last_resize_apply_time >= window_s
      then apply_resize t cols rows now
      else t.pending_resize <- Some (cols, rows)

let maybe_apply_pending_resize t =
  match (t.pending_resize, t.resize_debounce) with
  | Some (cols, rows), Some window_s ->
      let now = Unix.gettimeofday () in
      if now -. t.last_resize_apply_time >= window_s then
        apply_resize t cols rows now
  | _ -> ()

let adjust_event_for_offset t =
  if t.mode = `Alt then Fun.id
  else
    let offset = mouse_offset t in
    (* Map terminal y coordinate to logical coordinate. - Static region (y <=
       offset): return -1 as sentinel - Dynamic region (y > offset): return
       1-based logical row *)
    let map_y y = if y <= offset then -1 else y - offset in
    fun event ->
      match event with
      | Input.Mouse (Input.Mouse.Button_press (x, y, button, modifiers)) ->
          Input.Mouse (Input.Mouse.Button_press (x, map_y y, button, modifiers))
      | Input.Mouse (Input.Mouse.Button_release (x, y, button, modifiers)) ->
          Input.Mouse
            (Input.Mouse.Button_release (x, map_y y, button, modifiers))
      | Input.Mouse (Input.Mouse.Motion (x, y, state, modifiers)) ->
          Input.Mouse (Input.Mouse.Motion (x, map_y y, state, modifiers))
      | Input.Scroll (x, y, dir, delta, mods) ->
          Input.Scroll (x, map_y y, dir, delta, mods)
      | event -> event

let close t =
  if t.closed then ()
  else (
    t.closed <- true;
    t.running <- false;
    t.control_state <- `Explicit_stopped;
    set_loop_active t false;
    (match t.signal_handler with
    | Some handler ->
        deregister_shutdown_handler handler;
        t.signal_handler <- None
    | None -> ());
    let term = t.terminal in
    let output_is_tty =
      try Unix.isatty (Terminal.output_fd term) with _ -> false
    in
    if t.mode = `Primary && output_is_tty then (
      let height = max 1 t.height in
      let render_offset = clamp 0 (height - 1) t.render_offset in
      let start_row = render_offset + 1 in
      Terminal.clear_scroll_region term;
      for row = start_row to height do
        Terminal.move_cursor term ~row ~col:1
          ~visible:(Terminal.cursor_visible term);
        Terminal.write term erase_entire_line
      done;
      Terminal.move_cursor term ~row:start_row ~col:1 ~visible:true;
      Terminal.flush term);
    (try Terminal.flush_input term with _ -> ());
    (try Terminal.set_mouse_mode term `Off with _ -> ());
    (try Terminal.enable_bracketed_paste term false with _ -> ());
    (try Terminal.enable_focus_reporting term false with _ -> ());
    (try Terminal.enable_kitty_keyboard term false with _ -> ());
    (try Terminal.enable_modify_other_keys term false with _ -> ());
    (try Terminal.switch_mode term `Cooked with _ -> ());
    if t.alt_enabled then (
      Terminal.leave_alternate_screen term;
      t.alt_enabled <- false);
    Terminal.set_cursor_visible term true;
    Terminal.close term;
    ())

let classify_event t evt =
  match evt with
  | Input.Resize (cols, rows) ->
      handle_resize t cols rows;
      Some (`Resize (cols, rows))
  | evt ->
      let adjusted = adjust_event_for_offset t evt in
      if t.exit_on_ctrl_c then
        match adjusted with
        | Input.Key { key = Input.Key.Char u; modifier; _ } ->
            let cp = Uchar.to_int u in
            if
              (modifier.ctrl && (cp = Char.code 'c' || cp = Char.code 'C'))
              || cp = 0x03
            then (
              close t;
              None)
            else Some (`Input adjusted)
        | _ -> Some (`Input adjusted)
      else Some (`Input adjusted)

let create ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(respect_alpha = false) ?(mouse_enabled = true) ?(mouse = None)
    ?(bracketed_paste = true) ?(focus_reporting = true)
    ?(kitty_keyboard = `Auto) ?(exit_on_ctrl_c = true) ?(debug_overlay = false)
    ?(debug_overlay_corner = `Bottom_right) ?(debug_overlay_capacity = 120)
    ?(frame_dump_every = 0) ?frame_dump_dir ?frame_dump_pattern
    ?(frame_dump_hits = false) ?(cursor_visible = mode = `Alt) ?explicit_width
    ?render_thread ?(input_timeout = None) ?(resize_debounce = Some 0.1)
    ?initial_caps ?(output = `Stdout) ?(signal_handlers = true) () : app =
  let term_output = match output with `Stdout -> Unix.stdout | `Fd fd -> fd in
  let render_buffer_size = 1024 * 1024 * 2 in
  let terminal =
    Terminal.open_terminal ~probe:true ~probe_timeout:1.0 ~output:term_output
      ?initial_caps ?render_thread ~render_buffer_size ()
  in
  let caps = Terminal.capabilities terminal in
  let initial_cols, initial_rows = Terminal.size terminal in
  let width = max 1 initial_cols in
  let height = max 1 initial_rows in
  let width_method =
    match caps.unicode_width with `Unicode -> `Unicode | `Wcwidth -> `Wcwidth
  in
  let screen =
    Screen.create ~width_method ~respect_alpha ~mouse_enabled ~cursor_visible
      ~explicit_width:
        (match explicit_width with Some v -> v | None -> caps.explicit_width)
      ()
  in
  Screen.apply_capabilities screen ~explicit_width:caps.explicit_width
    ~hyperlinks:caps.hyperlinks;
  Screen.resize screen ~width ~height;
  let focus_reporting = focus_reporting && caps.focus_tracking in
  let effective_mouse_mode =
    if mouse_enabled then
      match mouse with Some m -> Some m | None -> Some `Sgr_any
    else None
  in
  let render_offset, static_needs_newline =
    if mode = `Primary then
      match Terminal.query_cursor_position ~timeout:0.1 terminal with
      | Some (row, col) ->
          let row = clamp 1 height row in
          let col = max 1 col in
          if row >= height then (
            Terminal.write terminal crlf;
            Terminal.flush terminal;
            (height - 1, false))
          else (row, col <> 1)
      | None -> (0, true)
    else (0, false)
  in
  let tui_height = max 1 (height - render_offset) in

  let rec runtime =
    {
      terminal;
      screen;
      caps;
      running = true;
      redraw_requested = false;
      alt_enabled = false;
      bracketed_paste_enabled = false;
      focus_enabled = false;
      kitty_enabled = false;
      modify_other_keys_enabled = false;
      unicode_enabled = false;
      width;
      height;
      output;
      render_offset;
      tui_height;
      prev_render_offset = render_offset;
      prev_tui_height = tui_height;
      static_needs_newline;
      static_queue = [];
      in_submit = false;
      needs_region_clear = false;
      last_resize_apply_time = 0.;
      pending_resize = None;
      force_full_next_frame = true;
      next_frame_deadline = None;
      mode;
      raw_mode;
      mouse_mode = effective_mouse_mode;
      bracketed_paste;
      focus_reporting;
      kitty_keyboard;
      exit_on_ctrl_c;
      target_fps;
      explicit_width =
        (match explicit_width with Some v -> v | None -> caps.explicit_width);
      input_timeout;
      resize_debounce;
      debug_overlay_enabled = debug_overlay;
      debug_overlay_capacity;
      debug_overlay_cb =
        Debug_overlay.on_frame ~corner:debug_overlay_corner
          ~capacity:debug_overlay_capacity ();
      frame_dump_every = max 0 frame_dump_every;
      frame_dump_dir;
      frame_dump_pattern;
      frame_dump_hits;
      frame_dump_counter = 0;
      last_frame_callback_ms = 0.;
      signal_handler = Some shutdown_fn;
      closed = false;
      loop_active = false;
      control_state = `Idle;
      previous_control_state = `Idle;
      live_requests = 0;
    }
  and shutdown_fn () = close runtime in
  if signal_handlers then install_signal_handlers ();
  register_shutdown_handler shutdown_fn;
  apply_config runtime;
  Terminal.query_pixel_resolution runtime.terminal;
  runtime.next_frame_deadline <- None;
  runtime

(* Event Loop *)

let run ?on_frame ?on_input ?on_resize ~on_render t =
  if not t.running then t.running <- true;
  (* Ensure the main loop is active when run is invoked *)
  (match t.control_state with
  | `Idle | `Auto_started -> start t
  | `Explicit_paused | `Explicit_suspended ->
      t.redraw_requested <- true;
      set_loop_active t true;
      wake_loop t
  | `Explicit_started | `Explicit_stopped -> ());

  let render_cycle ~now ~last_time =
    Option.iter (fun f -> f t ~dt:(now -. last_time)) on_frame;
    prepare t;
    let user_start = Unix.gettimeofday () in
    on_render t;
    let user_end = Unix.gettimeofday () in
    let user_ms = Float.max 0. ((user_end -. user_start) *. 1000.) in
    t.last_frame_callback_ms <- user_ms;
    submit t;
    user_end
  in

  (* Process a single input event - defined once, not in the hot loop *)
  let handle_event evt =
    if not t.running then ()
    else
      match classify_event t evt with
      | None -> ()
      | Some (`Resize (cols, rows)) ->
          Option.iter (fun f -> f t ~cols ~rows) on_resize;
          Option.iter (fun f -> f t (Input.Resize (cols, rows))) on_input;
          request_redraw t
      | Some (`Input event) ->
          Option.iter (fun f -> f t event) on_input;
          request_redraw t
  in

  (* Calculate timeout until next frame or other deadline *)
  let compute_timeout ~now =
    let min_opt a b =
      match (a, b) with
      | None, x | x, None -> x
      | Some x, Some y -> Some (Float.min x y)
    in
    let pending_timeout =
      match (t.pending_resize, t.resize_debounce) with
      | Some _, Some window_s ->
          let remaining = t.last_resize_apply_time +. window_s -. now in
          if remaining > 0. then Some remaining else Some 0.
      | _ -> None
    in
    let deadline_timeout =
      match t.next_frame_deadline with
      | Some dl ->
          let dt = dl -. now in
          if dt <= 0. then Some 0. else Some dt
      | None -> None
    in
    (* When redraw is requested with no cadence, don't wait *)
    let immediate_redraw_timeout =
      if t.redraw_requested && compute_loop_interval t = None then Some 0.
      else None
    in
    min_opt immediate_redraw_timeout
      (min_opt (min_opt deadline_timeout t.input_timeout) pending_timeout)
  in

  (* Check if we need to render immediately *)
  let should_render_now ~now =
    let deadline_due =
      t.loop_active
      && match t.next_frame_deadline with Some dl -> now >= dl | None -> false
    in
    let immediate_redraw =
      t.redraw_requested && compute_loop_interval t = None
    in
    immediate_redraw || deadline_due
  in

  let rec loop last_time =
    if not (running t) then ()
    else (
      maybe_apply_pending_resize t;
      (* Get single timestamp for this iteration *)
      let now = Unix.gettimeofday () in
      (* Update frame timing *)
      let frame_interval =
        if t.loop_active then compute_loop_interval t else None
      in
      t.next_frame_deadline <-
        (match (frame_interval, t.loop_active) with
        | None, _ | _, false -> None
        | Some _, true -> (
            match t.next_frame_deadline with
            | Some dl when dl > now -> Some dl
            | _ -> Some now));

      (* Render if needed *)
      let last_time =
        if should_render_now ~now then (
          t.next_frame_deadline <-
            Option.map (fun iv -> Unix.gettimeofday () +. iv) frame_interval;
          render_cycle ~now ~last_time)
        else last_time
      in

      (* Wait for input with appropriate timeout *)
      let timeout = compute_timeout ~now in
      let _ = Terminal.read ?timeout t.terminal handle_event in

      loop last_time)
  in
  let start_time = Unix.gettimeofday () in
  loop start_time;
  if not t.closed then close t

(* Static Output & Utilities *)

let set_debug_overlay ?corner t ~enabled =
  let previous = t.debug_overlay_enabled in
  t.debug_overlay_enabled <- enabled;
  Option.iter
    (fun c ->
      t.debug_overlay_cb <-
        Debug_overlay.on_frame ~corner:c ~capacity:t.debug_overlay_capacity ())
    corner;
  if previous <> enabled || Option.is_some corner then request_redraw t

let toggle_debug_overlay ?corner t =
  set_debug_overlay ?corner t ~enabled:(not t.debug_overlay_enabled)

let configure_frame_dump ?every ?dir ?pattern ?hits t =
  Option.iter
    (fun ev ->
      t.frame_dump_every <- max 0 ev;
      t.frame_dump_counter <- 0)
    every;
  Option.iter (fun d -> t.frame_dump_dir <- Some d) dir;
  Option.iter (fun p -> t.frame_dump_pattern <- Some p) pattern;
  Option.iter (fun h -> t.frame_dump_hits <- h) hits

let dump_frame ?hits ?dir ?pattern t =
  let effective_hits = Option.value ~default:t.frame_dump_hits hits in
  let dir = match dir with Some d -> Some d | None -> t.frame_dump_dir in
  let pattern =
    match pattern with Some p -> Some p | None -> t.frame_dump_pattern
  in
  Frame_dump.snapshot ?dir ?pattern ~hits:effective_hits t.screen
