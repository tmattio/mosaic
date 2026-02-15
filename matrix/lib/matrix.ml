module Ansi = Ansi
module Glyph = Glyph
module Grid = Grid
module Input = Input
module Screen = Screen
module Terminal = Terminal
module Image = Image

(* Types *)

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
  output_fd : Unix.file_descr;
  input_fd : Unix.file_descr;
  wakeup_r : Unix.file_descr;
  wakeup_w : Unix.file_descr;
  mutable original_termios : Unix.terminal_io option;
  input_buffer : bytes;
  render_buffer : bytes;
  mutable running : bool;
  mutable redraw_requested : bool;
  mutable width : int;
  mutable height : int;
  (* Primary mode layout *)
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

(* Shutdown handler registry *)

let shutdown_handlers : (unit -> unit) list ref = ref []
let shutdown_triggered = ref false

let run_shutdown_handlers () =
  if !shutdown_triggered then ()
  else (
    shutdown_triggered := true;
    List.iter (fun f -> (try f () with _ -> ())) !shutdown_handlers)

let register_shutdown_handler fn =
  shutdown_handlers := fn :: !shutdown_handlers

let deregister_shutdown_handler fn =
  shutdown_handlers := List.filter (fun f -> f != fn) !shutdown_handlers

let shutdown_signal_handler signum =
  run_shutdown_handlers ();
  exit (128 + signum)

let signal_handlers_installed = ref false

let try_set_signal sig_num handler =
  try Sys.set_signal sig_num handler with Invalid_argument _ -> ()

let install_signal_handlers () =
  if not !signal_handlers_installed then (
    signal_handlers_installed := true;
    try_set_signal Sys.sigterm (Sys.Signal_handle shutdown_signal_handler);
    try_set_signal Sys.sigint (Sys.Signal_handle shutdown_signal_handler);
    try_set_signal Sys.sigquit (Sys.Signal_handle shutdown_signal_handler);
    try_set_signal Sys.sigabrt (Sys.Signal_handle shutdown_signal_handler);
    Printexc.set_uncaught_exception_handler (fun exn ->
        prerr_endline (Printexc.to_string exn);
        run_shutdown_handlers ();
        exit 1))

let () = at_exit run_shutdown_handlers

(* I/O helpers *)

let write_all fd buf off len =
  let rec go off remaining =
    if remaining > 0 then
      let n =
        try Unix.write fd buf off remaining
        with Unix.Unix_error (Unix.EINTR, _, _) ->
          Unix.write fd buf off remaining
      in
      go (off + n) (remaining - n)
  in
  go off len

let write_string fd s =
  write_all fd (Bytes.unsafe_of_string s) 0 (String.length s)

let wake_byte = Bytes.of_string "w"

let wake_fd fd =
  let _ : int =
    try Unix.write fd wake_byte 0 1 with Unix.Unix_error _ -> 0
  in
  ()

let wake t = wake_fd t.wakeup_w

let drain_wakeup_fd fd =
  let buf = Bytes.create 64 in
  let rec go () =
    match Unix.read fd buf 0 64 with
    | n when n > 0 -> go ()
    | _ -> ()
    | exception Unix.Unix_error _ -> ()
  in
  go ()

(* SIGWINCH: global flag + wakeup pipe fd (set once at create time) *)

let winch_received = ref false
let install_winch_handler wakeup_w =
  let handler _ =
    winch_received := true;
    wake_fd wakeup_w
  in
  try_set_signal Sys.sigwinch (Sys.Signal_handle handler)

(* Small helpers *)

let clamp lo hi v = max lo (min hi v)

let min_static_rows ~is_tty ~mode ~height =
  if mode = `Primary && is_tty && height > 1 then 1 else 0

(* Interpret a CPR response as (render_offset, static_needs_newline).
   Emits a CRLF to scroll if the cursor is at the bottom row. *)
let render_offset_of_cursor ~terminal ~height row col =
  let row = clamp 1 height row in
  let col = max 1 col in
  if row >= height then (
    Terminal.send terminal "\r\n";
    (height - 1, false))
  else (row, col <> 1)

(* Low-level I/O: parameterized to work before app record exists *)

let wait_readable_fds ~input_fd ~wakeup_r ~timeout =
  let fds = [ input_fd; wakeup_r ] in
  let timeout_f = Option.value ~default:(-1.) timeout in
  let readable, _, _ =
    try Unix.select fds [] [] timeout_f
    with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
  in
  readable <> []

let read_input t ~timeout ~on_event =
  let parser = Terminal.parser t.terminal in
  let on_caps event = Terminal.apply_capability_event t.terminal event in
  let has_input =
    wait_readable_fds ~input_fd:t.input_fd ~wakeup_r:t.wakeup_r ~timeout
  in
  if has_input then (
    drain_wakeup_fd t.wakeup_r;
    if !winch_received then (
      winch_received := false;
      let cols, rows = Terminal.size t.output_fd in
      on_event (Input.Resize (cols, rows)));
    (match
       Unix.read t.input_fd t.input_buffer 0 (Bytes.length t.input_buffer)
     with
    | n when n > 0 ->
        let now = Unix.gettimeofday () in
        Input.Parser.feed parser t.input_buffer 0 n ~now ~on_event ~on_caps
    | _ -> ()
    | exception Unix.Unix_error _ -> ()));
  let now = Unix.gettimeofday () in
  Input.Parser.drain parser ~now ~on_event ~on_caps

(* Blocking cursor position query. Takes explicit I/O parameters so it can
   be called from [create] before the app record is fully built. *)

let query_cursor_position_raw ~terminal ~input_fd ~wakeup_r ~input_buffer
    ~timeout =
  Terminal.send terminal "\027[6n";
  let result = ref None in
  let on_caps = function
    | Input.Caps.Cursor_position (row, col) -> result := Some (row, col)
    | event -> Terminal.apply_capability_event terminal event
  in
  let parser = Terminal.parser terminal in
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if Option.is_some !result then ()
    else
      let remaining = deadline -. Unix.gettimeofday () in
      if remaining <= 0. then ()
      else if
        wait_readable_fds ~input_fd ~wakeup_r ~timeout:(Some remaining)
      then (
        drain_wakeup_fd wakeup_r;
        (match Unix.read input_fd input_buffer 0 (Bytes.length input_buffer) with
        | n when n > 0 ->
            let now = Unix.gettimeofday () in
            Input.Parser.feed parser input_buffer 0 n ~now
              ~on_event:(fun _ -> ()) ~on_caps
        | _ -> ()
        | exception Unix.Unix_error _ -> ());
        loop ())
  in
  loop ();
  !result

let query_cursor_position t ~timeout =
  query_cursor_position_raw ~terminal:t.terminal ~input_fd:t.input_fd
    ~wakeup_r:t.wakeup_r ~input_buffer:t.input_buffer ~timeout

(* Layout *)

let apply_primary_region t ~render_offset ~resize =
  let height = max 1 t.height in
  let is_tty = Terminal.tty t.terminal in
  let min_static = min_static_rows ~is_tty ~mode:t.mode ~height in
  let render_offset = clamp min_static (height - 1) render_offset in
  let tui_height = max 1 (height - render_offset) in
  t.render_offset <- render_offset;
  t.tui_height <- tui_height;
  if render_offset > 0 then
    Terminal.set_scroll_region t.terminal ~top:1 ~bottom:render_offset
  else Terminal.clear_scroll_region t.terminal;
  Screen.set_row_offset t.screen render_offset;
  if resize then Screen.resize t.screen ~width:t.width ~height:tui_height

let invalidate_inline_state t =
  t.force_full_next_frame <- true;
  t.needs_region_clear <- true

(* Accessors *)

let size t =
  match t.mode with
  | `Alt -> (t.width, t.height)
  | `Primary -> (t.width, t.tui_height)

let mouse_offset t = if t.mode = `Primary then t.render_offset else 0
let pixel_resolution t = Terminal.pixel_resolution t.terminal
let terminal t = t.terminal
let capabilities t = Terminal.capabilities t.terminal
let running t = t.running

let request_redraw t =
  if t.control_state <> `Explicit_suspended then (
    t.redraw_requested <- true;
    wake t)

let refresh_capabilities t =
  let caps = Terminal.capabilities t.terminal in
  Screen.apply_capabilities t.screen ~explicit_width:caps.explicit_width
    ~explicit_cursor_positioning:caps.explicit_cursor_positioning
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

(* Frame timing *)

let compute_loop_interval t =
  match t.target_fps with
  | Some fps when fps > 0. -> Some (1. /. fps)
  | _ -> None

let set_loop_active t active =
  if t.loop_active <> active then (
    t.loop_active <- active;
    if not active then t.next_frame_deadline <- None
    else t.next_frame_deadline <- Some (Unix.gettimeofday ()))

(* Static output (primary mode only) *)

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
      if needs_leading_newline then Terminal.send term crlf;
      Terminal.send term text)
    else (
      Terminal.move_cursor term ~row:t.height ~col:1
        ~visible:(Terminal.cursor_visible term);
      if needs_leading_newline then Terminal.send term crlf;
      Terminal.send term text;
      Screen.invalidate_presented t.screen;
      invalidate_inline_state t);
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
    Terminal.send t.terminal Ansi.(to_string clear_and_home);
    let cols, rows = Terminal.size t.output_fd in
    t.width <- max 1 cols;
    t.height <- max 1 rows;
    let is_tty = Terminal.tty t.terminal in
    let min_static = min_static_rows ~is_tty ~mode:t.mode ~height:t.height in
    t.render_offset <- min_static;
    t.tui_height <- max 1 (t.height - t.render_offset);
    t.static_queue <- [];
    t.static_needs_newline <- false;
    invalidate_inline_state t;
    refresh_render_region t;
    request_redraw t)

(* Protocol configuration *)

let apply_config t =
  refresh_capabilities t;
  let term = t.terminal in
  let caps = Terminal.capabilities term in
  (* Raw mode *)
  (match t.raw_mode, t.original_termios with
  | true, None ->
      let saved = Terminal.set_raw t.input_fd in
      t.original_termios <- Some saved
  | false, Some saved ->
      Terminal.restore t.input_fd saved;
      t.original_termios <- None
  | _ -> ());
  (* Alt screen *)
  if t.mode = `Alt then (
    Terminal.enter_alternate_screen term;
    Screen.set_cursor_visible t.screen false)
  else Terminal.leave_alternate_screen term;
  t.force_full_next_frame <- true;
  (* Mouse *)
  (match t.mouse_mode with
  | Some mode -> Terminal.set_mouse_mode term mode
  | None -> Terminal.set_mouse_mode term `Off);
  (* Bracketed paste *)
  Terminal.enable_bracketed_paste term t.bracketed_paste;
  (* Focus reporting *)
  Terminal.enable_focus_reporting term
    (caps.focus_tracking && t.focus_reporting);
  (* Kitty keyboard *)
  (match t.kitty_keyboard with
  | `Disabled -> Terminal.enable_kitty_keyboard term false
  | `Auto -> Terminal.enable_kitty_keyboard term caps.kitty_keyboard
  | `Enabled flags -> Terminal.enable_kitty_keyboard ~flags term true);
  (* ModifyOtherKeys: enable when kitty keyboard is not active *)
  Terminal.enable_modify_other_keys term
    (not (Terminal.kitty_keyboard_enabled term));
  (* Unicode width *)
  Terminal.set_unicode_width term
    (if caps.unicode_width = `Unicode && not t.explicit_width then `Unicode
     else `Wcwidth);
  refresh_render_region t

(* Lifecycle: Prepare / Grid / Submit *)

let prepare t =
  refresh_capabilities t;
  t.redraw_requested <- false;
  refresh_render_region t;
  Grid.clear (Screen.grid t.screen);
  Screen.Hit_grid.clear (Screen.hit_grid t.screen)

let grid t = Screen.grid t.screen
let hits t = Screen.hit_grid t.screen

let submit t =
  if not t.running then ()
  else
    let overall_start = Unix.gettimeofday () in
    let is_tty = Terminal.tty t.terminal in
    t.in_submit <- true;
    Fun.protect
      ~finally:(fun () -> t.in_submit <- false)
      (fun () ->
        (* 1. Debug overlay *)
        if t.debug_overlay_enabled then t.debug_overlay_cb t.screen;
        let cursor = Screen.cursor_info t.screen in

        (* 2. Sync bracket *)
        let caps = Terminal.capabilities t.terminal in
        let use_sync = is_tty && caps.sync in
        let send s = Terminal.send t.terminal s in
        if use_sync then send Ansi.(to_string (enable Sync_output));
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
            let min_static = min_static_rows ~is_tty ~mode:t.mode ~height in
            let max_ui_rows = max 1 (height - min_static) in
            let target_rows = min required_rows max_ui_rows in
            if required_rows > max_ui_rows then (
              clipped := true;
              render_height_limit := Some max_ui_rows);
            let render_offset =
              clamp min_static (height - 1) t.render_offset
            in
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

        (* 3. Clear stale region (primary mode) *)
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

        (* 4. Render to bytes *)
        let forced_full =
          let ff = t.force_full_next_frame in
          if ff then t.force_full_next_frame <- false;
          ff || !row_offset_changed || !clipped
        in
        let len =
          Screen.render_to_bytes ~full:forced_full
            ?height_limit:!render_height_limit t.screen t.render_buffer
        in

        (* 5. Present *)
        let stdout_start = Unix.gettimeofday () in
        if len > 0 then write_all t.output_fd t.render_buffer 0 len;

        (* 6. Restore cursor *)
        let cursor_max_row =
          match !render_height_limit with
          | Some limit -> max 1 limit
          | None -> t.tui_height
        in
        if cursor.has_position then
          let row = clamp 1 cursor_max_row cursor.row in
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
            Terminal.set_cursor_color t.terminal ~r:(to_float r)
              ~g:(to_float g) ~b:(to_float b) ~a:1.
        | None -> Terminal.reset_cursor_color t.terminal);

        (* End sync bracket *)
        if use_sync then send Ansi.(to_string (disable Sync_output));

        let stdout_end = Unix.gettimeofday () in
        let stdout_ms = Float.max 0. ((stdout_end -. stdout_start) *. 1000.) in
        let overall_frame_ms =
          Float.max 0. ((stdout_end -. overall_start) *. 1000.)
        in

        (* 7. Update inline tracking *)
        (match t.mode with
        | `Primary ->
            t.prev_render_offset <- t.render_offset;
            t.prev_tui_height <- t.tui_height
        | `Alt ->
            t.prev_render_offset <- 0;
            t.prev_tui_height <- t.height);

        (* 8. Metrics & dumps *)
        Screen.record_runtime_metrics t.screen
          ~frame_callback_ms:t.last_frame_callback_ms ~overall_frame_ms
          ~stdout_ms;
        if t.frame_dump_every > 0 then (
          t.frame_dump_counter <- t.frame_dump_counter + 1;
          if t.frame_dump_counter mod t.frame_dump_every = 0 then
            Frame_dump.snapshot ?dir:t.frame_dump_dir
              ?pattern:t.frame_dump_pattern ~hits:t.frame_dump_hits t.screen))

(* Control state *)

let stop t =
  if not t.closed then (
    t.running <- false;
    t.control_state <- `Explicit_stopped;
    set_loop_active t false;
    t.next_frame_deadline <- None;
    t.redraw_requested <- false;
    wake t)

let request_live t =
  if t.closed then ()
  else (
    t.live_requests <- t.live_requests + 1;
    if t.control_state = `Idle && t.live_requests > 0 then (
      t.control_state <- `Auto_started;
      set_loop_active t true;
      t.redraw_requested <- true;
      wake t))

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
    wake t)

let auto t =
  t.control_state <- (if t.loop_active then `Auto_started else `Idle)

let pause t =
  t.control_state <- `Explicit_paused;
  set_loop_active t false

let suspend t =
  t.previous_control_state <- t.control_state;
  t.control_state <- `Explicit_suspended;
  set_loop_active t false;
  t.redraw_requested <- false;
  invalidate_inline_state t;
  (try Terminal.set_mouse_mode t.terminal `Off with _ -> ());
  (try Terminal.enable_bracketed_paste t.terminal false with _ -> ());
  (try Terminal.enable_focus_reporting t.terminal false with _ -> ());
  (try Terminal.enable_kitty_keyboard t.terminal false with _ -> ());
  (try Terminal.enable_modify_other_keys t.terminal false with _ -> ());
  (match t.original_termios with
  | Some saved ->
      (try Terminal.restore t.input_fd saved with _ -> ());
      t.original_termios <- None
  | None -> ());
  (try Terminal.flush_input t.input_fd with _ -> ())

let resume t =
  if t.control_state <> `Explicit_suspended then ()
  else (
    if t.raw_mode then (
      try
        let saved = Terminal.set_raw t.input_fd in
        t.original_termios <- Some saved
      with _ -> ());
    (try Terminal.flush_input t.input_fd with _ -> ());
    (if t.mode = `Primary then
       let height = max 1 t.height in
       match query_cursor_position t ~timeout:0.1 with
       | Some (row, col) ->
           let render_offset, static_needs_newline =
             render_offset_of_cursor ~terminal:t.terminal ~height row col
           in
           let tui_height = max 1 (height - render_offset) in
           t.render_offset <- render_offset;
           t.tui_height <- tui_height;
           t.prev_render_offset <- render_offset;
           t.prev_tui_height <- tui_height;
           t.static_needs_newline <- static_needs_newline
       | None -> ());
    invalidate_inline_state t;
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
      wake t)
    else request_redraw t)

(* Cursor *)

let set_cursor ?visible ?style t =
  Option.iter
    (fun v ->
      Terminal.set_cursor_visible t.terminal v;
      Screen.set_cursor_visible t.screen v)
    visible;
  Option.iter
    (fun style ->
      let _, blinking = Terminal.cursor_style_state t.terminal in
      Terminal.set_cursor_style t.terminal style ~blinking;
      Screen.set_cursor_style t.screen ~style ~blinking)
    style

let set_cursor_style t ~style ~blinking =
  Terminal.set_cursor_style t.terminal style ~blinking;
  Screen.set_cursor_style t.screen ~style ~blinking

let set_cursor_position t ~row ~col =
  let max_row =
    if t.mode = `Primary then max 1 t.tui_height else max 1 t.height
  in
  let row = clamp 1 max_row row in
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

(* Resize *)

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

(* Event dispatch *)

let adjust_event_for_offset t =
  if t.mode = `Alt then Fun.id
  else
    let offset = mouse_offset t in
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
            then None
            else Some (`Input adjusted)
        | _ -> Some (`Input adjusted)
      else Some (`Input adjusted)

(* Close *)

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
    let is_tty = Terminal.tty term in
    if t.mode = `Primary && is_tty then (
      let height = max 1 t.height in
      let render_offset = clamp 0 (height - 1) t.render_offset in
      let start_row = render_offset + 1 in
      Terminal.clear_scroll_region term;
      for row = start_row to height do
        Terminal.move_cursor term ~row ~col:1
          ~visible:(Terminal.cursor_visible term);
        Terminal.send term erase_entire_line
      done;
      Terminal.move_cursor term ~row:start_row ~col:1 ~visible:true);
    (try Terminal.flush_input t.input_fd with _ -> ());
    Terminal.close term;
    (match t.original_termios with
    | Some saved ->
        (try Terminal.restore t.input_fd saved with _ -> ());
        t.original_termios <- None
    | None -> ());
    (try Unix.close t.wakeup_r with _ -> ());
    (try Unix.close t.wakeup_w with _ -> ()))

(* Constructor *)

let create ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(respect_alpha = false) ?(mouse_enabled = true) ?(mouse = None)
    ?(bracketed_paste = true) ?(focus_reporting = true)
    ?(kitty_keyboard = `Auto) ?(exit_on_ctrl_c = true) ?(debug_overlay = false)
    ?(debug_overlay_corner = `Bottom_right) ?(debug_overlay_capacity = 120)
    ?(frame_dump_every = 0) ?frame_dump_dir ?frame_dump_pattern
    ?(frame_dump_hits = false) ?(cursor_visible = mode = `Alt) ?explicit_width
    ?(input_timeout = None) ?(resize_debounce = Some 0.1) ?initial_caps
    ?(output = `Stdout) ?(signal_handlers = true) () : app =
  let output_fd = match output with `Stdout -> Unix.stdout | `Fd fd -> fd in
  let input_fd = Unix.stdin in
  let output_is_tty = Terminal.is_tty output_fd in
  let input_is_tty = Terminal.is_tty input_fd in
  (* Wakeup pipe for cross-thread/signal wakeup *)
  let wakeup_r, wakeup_w = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock wakeup_r;
  Unix.set_nonblock wakeup_w;
  (* Output callback for Terminal *)
  let output_fn = write_string output_fd in
  let terminal =
    Terminal.make ~output:output_fn ~tty:output_is_tty ?initial_caps ()
  in
  (* Raw mode *)
  let original_termios =
    if raw_mode && input_is_tty then Some (Terminal.set_raw input_fd) else None
  in
  (* Probe capabilities *)
  let input_buffer = Bytes.create 4096 in
  if input_is_tty then
    Terminal.probe ~timeout:1.0
      ~on_event:(fun _ -> ())
      ~read_into:(fun buf off len ->
        try Unix.read input_fd buf off len
        with Unix.Unix_error _ -> 0)
      ~wait_readable:(fun ~timeout ->
        let readable, _, _ =
          try Unix.select [ input_fd ] [] [] timeout
          with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
        in
        readable <> [])
      terminal;
  let caps = Terminal.capabilities terminal in
  let initial_cols, initial_rows = Terminal.size output_fd in
  let width = max 1 initial_cols in
  let height = max 1 initial_rows in
  let width_method : Glyph.width_method =
    match caps.unicode_width with `Unicode -> `Unicode | `Wcwidth -> `Wcwidth
  in
  let screen =
    Screen.create ~width_method ~respect_alpha ~mouse_enabled ~cursor_visible
      ~explicit_width:(Option.value ~default:caps.explicit_width explicit_width)
      ()
  in
  Screen.apply_capabilities screen ~explicit_width:caps.explicit_width
    ~explicit_cursor_positioning:caps.explicit_cursor_positioning
    ~hyperlinks:caps.hyperlinks;
  Screen.resize screen ~width ~height;
  let focus_reporting = focus_reporting && caps.focus_tracking in
  let effective_mouse_mode =
    if mouse_enabled then Some (Option.value ~default:`Sgr_any mouse) else None
  in
  (* Render buffer *)
  let render_buffer = Bytes.create (1024 * 1024 * 2) in
  (* Primary mode: detect initial cursor position *)
  let render_offset, static_needs_newline =
    if mode = `Primary then
      match
        query_cursor_position_raw ~terminal ~input_fd ~wakeup_r ~input_buffer
          ~timeout:0.1
      with
      | Some (row, col) ->
          render_offset_of_cursor ~terminal ~height row col
      | None -> (0, true)
    else (0, false)
  in
  let tui_height = max 1 (height - render_offset) in
  let rec runtime =
    { terminal; screen; output_fd; input_fd; wakeup_r; wakeup_w;
      original_termios; input_buffer; render_buffer;
      running = true; redraw_requested = false; width; height;
      render_offset; tui_height;
      prev_render_offset = render_offset; prev_tui_height = tui_height;
      static_needs_newline; static_queue = []; in_submit = false;
      needs_region_clear = false;
      last_resize_apply_time = 0.; pending_resize = None;
      force_full_next_frame = true; next_frame_deadline = None;
      mode; raw_mode; mouse_mode = effective_mouse_mode;
      bracketed_paste; focus_reporting; kitty_keyboard;
      exit_on_ctrl_c; target_fps;
      explicit_width =
        Option.value ~default:caps.explicit_width explicit_width;
      input_timeout; resize_debounce;
      debug_overlay_enabled = debug_overlay; debug_overlay_capacity;
      debug_overlay_cb =
        Debug_overlay.on_frame ~corner:debug_overlay_corner
          ~capacity:debug_overlay_capacity ();
      frame_dump_every = max 0 frame_dump_every; frame_dump_dir;
      frame_dump_pattern; frame_dump_hits; frame_dump_counter = 0;
      last_frame_callback_ms = 0.; signal_handler = Some shutdown_fn;
      closed = false; loop_active = false;
      control_state = `Idle; previous_control_state = `Idle; live_requests = 0 }
  and shutdown_fn () = close runtime in
  if signal_handlers then install_signal_handlers ();
  register_shutdown_handler shutdown_fn;
  install_winch_handler wakeup_w;
  apply_config runtime;
  Terminal.query_pixel_resolution runtime.terminal;
  runtime.next_frame_deadline <- None;
  runtime

(* Event loop *)

let run ?on_frame ?on_input ?on_resize ~on_render t =
  if not t.running then t.running <- true;
  (match t.control_state with
  | `Idle | `Auto_started -> start t
  | `Explicit_paused | `Explicit_suspended ->
      t.redraw_requested <- true;
      set_loop_active t true;
      wake t
  | `Explicit_started | `Explicit_stopped -> ());

  let render_cycle ~now ~last_time =
    Option.iter (fun f -> f t ~dt:(now -. last_time)) on_frame;
    prepare t;
    let user_start = Unix.gettimeofday () in
    on_render t;
    let user_end = Unix.gettimeofday () in
    t.last_frame_callback_ms <-
      Float.max 0. ((user_end -. user_start) *. 1000.);
    submit t;
    user_end
  in

  let handle_event evt =
    if not t.running then ()
    else
      match classify_event t evt with
      | None -> close t
      | Some (`Resize (cols, rows)) ->
          Option.iter (fun f -> f t ~cols ~rows) on_resize;
          Option.iter (fun f -> f t (Input.Resize (cols, rows))) on_input;
          request_redraw t
      | Some (`Input event) ->
          Option.iter (fun f -> f t event) on_input;
          request_redraw t
  in

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
    let parser_timeout =
      match Input.Parser.deadline (Terminal.parser t.terminal) with
      | Some dl ->
          let dt = dl -. now in
          if dt <= 0. then Some 0. else Some dt
      | None -> None
    in
    let immediate_redraw_timeout =
      if t.redraw_requested && compute_loop_interval t = None then Some 0.
      else None
    in
    min_opt immediate_redraw_timeout
      (min_opt (min_opt deadline_timeout t.input_timeout)
         (min_opt pending_timeout parser_timeout))
  in

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
      let now = Unix.gettimeofday () in
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
      let last_time =
        if should_render_now ~now then (
          t.next_frame_deadline <-
            Option.map (fun iv -> Unix.gettimeofday () +. iv) frame_interval;
          render_cycle ~now ~last_time)
        else last_time
      in
      let timeout = compute_timeout ~now in
      read_input t ~timeout ~on_event:handle_event;
      loop last_time)
  in
  let start_time = Unix.gettimeofday () in
  loop start_time;
  if not t.closed then close t

(* Diagnostics *)

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
  let or_default a b = match a with Some _ -> a | None -> b in
  Frame_dump.snapshot
    ?dir:(or_default dir t.frame_dump_dir)
    ?pattern:(or_default pattern t.frame_dump_pattern)
    ~hits:(Option.value ~default:t.frame_dump_hits hits)
    t.screen
