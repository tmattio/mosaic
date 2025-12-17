module Ansi = Ansi
module Glyph = Glyph
module Grid = Grid
module Input = Input
module Screen = Screen
module Terminal = Terminal
module Image = Image

(* App State & Types *)

type kitty_keyboard = [ `Auto | `Disabled | `Enabled of int ]
type mode = [ `Alt | `Primary_split | `Primary_inline ]
type debug_overlay_corner = Debug_overlay.corner

type control_state =
  [ `Idle
  | `Auto_started
  | `Explicit_started
  | `Explicit_paused
  | `Explicit_suspended
  | `Explicit_stopped ]

type stdout_capture = { read_fd : Unix.file_descr; write_fd : Unix.file_descr }

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
  mutable screen_height : int;
  output : [ `Stdout | `Fd of Unix.file_descr ];
  mutable render_offset : int;
  mutable static_height : int;
  mutable inline_base_row : int option;
  mutable primary_region_reserved : bool;
  mutable primary_region_height : int;
  mutable primary_region_offset : int;
  mutable last_resize_apply_time : float;
  mutable pending_resize : (int * int) option;
  mutable force_full_next_frame : bool;
  mutable frame_interval : float option;
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
  mutable stdout_capture : stdout_capture option;
  mutable stdout_restore_fd : Unix.file_descr option;
  stdout_buffer : Buffer.t;
  stdout_tui_fd : Unix.file_descr option;
}

(* Shutdown handler registry for graceful cleanup.
   Apps register cleanup functions that run on exit or signal. *)
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

(* Helper to safely set signal handlers - on Windows most signals are unavailable *)
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

(* Time source for scheduling and performance measurements.
   Uses gettimeofday which is universally available. While it can be affected
   by system time changes (NTP, DST), this is rarely an issue for TUI apps. *)
let monotonic_now = Unix.gettimeofday
let size t = (t.width, t.height)
let pixel_resolution t = Terminal.pixel_resolution t.terminal
let terminal t = t.terminal
let capabilities t = t.caps
let running t = t.running
let wake_loop t = Terminal.wake t.terminal

let request_redraw t =
  if t.control_state <> `Explicit_suspended then (
    t.redraw_requested <- true;
    wake_loop t)

let output_is_tty t =
  let fd = Terminal.output_fd t.terminal in
  Unix.isatty fd

let compute_loop_interval t =
  match t.target_fps with
  | Some fps when fps > 0. -> Some (1. /. fps)
  | _ -> None

let set_loop_active t active =
  if t.loop_active <> active then (
    t.loop_active <- active;
    if not active then t.next_frame_deadline <- None
    else
      let now = monotonic_now () in
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

let inline_base_row t = t.inline_base_row
let reset_inline_base_row t = t.inline_base_row <- None

let bump_inline_base_row t ~delta =
  t.inline_base_row <-
    Option.map (fun row -> max 1 (row + delta)) t.inline_base_row

let invalidate_inline_baseline t =
  reset_inline_base_row t;
  t.render_offset <- 0;
  t.force_full_next_frame <- true

module Inline_layout = struct
  type t = { base_row : int; offset : int; grow : int }

  let compute ~current_base_row ~term_rows ~h_needed ~query_cursor =
    let base_row0 =
      match current_base_row with
      | Some row when row >= 1 && row <= term_rows -> row
      | _ -> ( match query_cursor () with Some row -> row | None -> term_rows)
    in
    let free_lines0 = max 0 (term_rows - base_row0 + 1) in
    let grow = if h_needed <= free_lines0 then 0 else h_needed - free_lines0 in
    let base_row =
      if h_needed <= 0 then base_row0
      else
        let max_top = max 1 (term_rows - h_needed + 1) in
        let proposed = base_row0 + grow in
        min max_top proposed
    in
    let offset = max 0 (base_row - 1) in
    { base_row; offset; grow }

  let validate_base_row ~term_rows = function
    | Some row when row >= 1 && row <= term_rows -> Some row
    | _ -> None
end

let refresh_capabilities t =
  let caps = Terminal.capabilities t.terminal in
  t.caps <- caps;
  Screen.apply_capabilities t.screen ~explicit_width:caps.explicit_width
    ~hyperlinks:caps.hyperlinks

(* Split / Static Region Logic *)

let clear_primary_region_state t =
  t.primary_region_reserved <- false;
  t.primary_region_height <- 0;
  t.primary_region_offset <- 0

let release_primary_region t =
  if not t.primary_region_reserved then ()
  else (
    if output_is_tty t then (
      let _, rows = Terminal.size t.terminal in
      let target_row =
        max 1 (min rows (t.primary_region_offset + t.primary_region_height))
      in
      Terminal.move_cursor t.terminal ~row:target_row ~col:1
        ~visible:(Terminal.cursor_visible t.terminal);
      Terminal.set_cursor_visible t.terminal true);
    clear_primary_region_state t)

let prepare_primary_region t ~height ~clear =
  if not (output_is_tty t) then (
    clear_primary_region_state t;
    0)
  else
    let height = max 0 height in
    if height = 0 then (
      release_primary_region t;
      0)
    else
      let _, rows = Terminal.size t.terminal in
      let offset = max 0 (rows - height) in
      let needs_refresh =
        (not t.primary_region_reserved)
        || t.primary_region_height <> height
        || t.primary_region_offset <> offset
      in
      if needs_refresh then (
        let old_height = t.primary_region_height in
        t.primary_region_reserved <- true;
        t.primary_region_height <- height;
        t.primary_region_offset <- offset;
        Terminal.write t.terminal Ansi.Escape.(to_string cursor_save);
        let grow =
          if old_height = 0 then max 0 (height - 1)
          else max 0 (height - old_height)
        in
        if grow > 0 then Terminal.write t.terminal (String.make grow '\n');
        let shrink =
          if old_height > 0 then max 0 (old_height - height) else 0
        in
        if shrink > 0 then
          Terminal.write t.terminal
            (Ansi.Escape.to_string (Ansi.Escape.scroll_down ~n:shrink));
        Terminal.write t.terminal Ansi.Escape.(to_string cursor_restore));
      let target_row = max 1 (offset + 1) in
      Terminal.move_cursor t.terminal ~row:target_row ~col:1
        ~visible:(Terminal.cursor_visible t.terminal);
      if needs_refresh && clear then
        Terminal.write t.terminal Ansi.Escape.(to_string erase_below_cursor);
      Terminal.set_cursor_visible t.terminal false;
      offset

let refresh_render_region t =
  let screen_rows = max 1 t.screen_height in
  let static_height =
    if t.mode = `Primary_split then
      let max_static = max 0 (screen_rows - 1) in
      let clamped = if t.static_height < 0 then 0 else t.static_height in
      min clamped max_static
    else (
      if t.primary_region_reserved then release_primary_region t;
      0)
  in
  t.static_height <- static_height;
  let render_height = max 1 (screen_rows - static_height) in
  t.height <- render_height;
  Screen.resize t.screen ~width:t.width ~height:render_height;
  if t.mode = `Alt then (
    if t.primary_region_reserved then release_primary_region t;
    t.render_offset <- 0;
    Screen.set_row_offset t.screen 0)
  else if t.mode = `Primary_split then (
    t.render_offset <-
      prepare_primary_region t ~height:render_height ~clear:true;
    Screen.set_row_offset t.screen t.render_offset;
    t.static_height <- t.render_offset)
  else (
    if t.primary_region_reserved then release_primary_region t;
    t.render_offset <- 0;
    Screen.set_row_offset t.screen 0)

(* Count explicit newline characters; a trailing non-newline line does not
   contribute. Callers that need line-aware layout should include newlines. *)
let rows_of_string s =
  let len = String.length s in
  if len = 0 then 0
  else
    let rec loop idx acc =
      if idx >= len then acc
      else
        let acc = if Char.equal (String.get s idx) '\n' then acc + 1 else acc in
        loop (idx + 1) acc
    in
    loop 0 0

let ensure_trailing_newline s =
  let len = String.length s in
  if len > 0 && Char.equal (String.get s (len - 1)) '\n' then s else s ^ "\n"

let static_write_raw t text =
  if t.mode = `Alt then ()
  else (
    (* All output goes through Terminal.write for proper serialization.
       Terminal.write automatically drains pending writes before writing. *)
    let send s = Terminal.write t.terminal s in
    (match t.mode with
    | `Primary_split ->
        (* Temporarily release the reserved region, print static text, then
           re-establish the split at the bottom. *)
        release_primary_region t;
        let rows =
          if String.length text = 0 then 0
          else (
            send text;
            rows_of_string text)
        in
        if rows <> 0 then t.static_height <- t.static_height + rows;
        refresh_render_region t
    | `Primary_inline ->
        let rows = rows_of_string text in
        if String.length text > 0 then (
          let _, term_rows = Terminal.size t.terminal in
          (match
             Inline_layout.validate_base_row ~term_rows (inline_base_row t)
           with
          | Some base_row ->
              let target_row = max 1 (base_row - 1) in
              Terminal.write t.terminal
                (Ansi.Escape.to_string
                   (Ansi.Escape.cursor_position ~row:target_row ~col:1));
              if rows > 0 then
                Terminal.write t.terminal
                  (Ansi.Escape.to_string (Ansi.Escape.insert_lines ~n:rows));
              Terminal.write t.terminal text
          | None -> Terminal.write t.terminal text);
          Terminal.flush t.terminal;
          if rows > 0 then bump_inline_base_row t ~delta:rows)
    | `Alt -> ());
    request_redraw t)

let static_write t text = static_write_raw t text
let static_print t text = static_write_raw t (ensure_trailing_newline text)

let static_clear t =
  if t.mode = `Alt then ()
  else (
    release_primary_region t;
    Terminal.write t.terminal Ansi.clear_and_home;
    let cols, rows = Terminal.size t.terminal in
    t.width <- max 1 cols;
    t.screen_height <- max 1 rows;
    t.static_height <- 0;
    invalidate_inline_baseline t;
    refresh_render_region t;
    request_redraw t)

let setup_stdout_capture output =
  match output with
  | `Stdout -> (
      flush stdout;
      let stdout_fd = Unix.descr_of_out_channel stdout in
      try
        let restore_fd = Unix.dup stdout_fd in
        let tui_fd = Unix.dup stdout_fd in
        let read_fd, write_fd = Unix.pipe () in
        Unix.set_close_on_exec read_fd;
        Unix.set_close_on_exec write_fd;
        (try Unix.set_nonblock read_fd with _ -> ());
        Unix.dup2 write_fd stdout_fd;
        (Some { read_fd; write_fd }, Some restore_fd, Some tui_fd)
      with _ -> (None, None, None))
  | `Fd _ -> (None, None, None)

let drain_stdout_capture t =
  match t.stdout_capture with
  | None -> false
  | Some cap ->
      let chunk = Bytes.create 4096 in
      let rec loop had_data =
        match Unix.read cap.read_fd chunk 0 (Bytes.length chunk) with
        | 0 -> had_data
        | n ->
            Buffer.add_subbytes t.stdout_buffer chunk 0 n;
            loop true
        | exception
            Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
          ->
            had_data
      in
      loop false

let flush_stdout_capture t =
  let had_data = drain_stdout_capture t in
  if Buffer.length t.stdout_buffer = 0 then false
  else
    match t.mode with
    | `Alt ->
        (* In Alt mode we cannot display captured output inline, so discard it
           to prevent unbounded memory growth. Any output captured between the
           last flush and shutdown will still be replayed via
           teardown_stdout_capture. *)
        Buffer.clear t.stdout_buffer;
        had_data
    | `Primary_split | `Primary_inline ->
        let text = Buffer.contents t.stdout_buffer in
        Buffer.clear t.stdout_buffer;
        if String.length text > 0 then static_write_raw t text;
        true

let write_buffer_to_fd fd buffer =
  let data = Buffer.contents buffer in
  let len = String.length data in
  let rec loop off =
    if off >= len then ()
    else
      let written = Unix.write_substring fd data off (len - off) in
      if written > 0 then loop (off + written) else ()
  in
  loop 0

let teardown_stdout_capture t ~flush_remaining =
  let _ = drain_stdout_capture t in
  (match (flush_remaining, t.stdout_restore_fd) with
  | true, Some fd when Buffer.length t.stdout_buffer > 0 ->
      (try write_buffer_to_fd fd t.stdout_buffer with _ -> ());
      Buffer.clear t.stdout_buffer
  | _ -> ());
  (match t.stdout_restore_fd with
  | Some fd ->
      (try Unix.dup2 fd Unix.stdout with _ -> ());
      (try Unix.close fd with _ -> ());
      t.stdout_restore_fd <- None
  | None -> ());
  (match t.stdout_capture with
  | Some cap -> (
      (try Unix.close cap.read_fd with _ -> ());
      try Unix.close cap.write_fd with _ -> ())
  | None -> ());
  t.stdout_capture <- None

(* Lifecycle: Prepare / Grid / Submit *)

let prepare t =
  refresh_capabilities t;
  let _ = flush_stdout_capture t in
  t.redraw_requested <- false;

  (* Ensure screen geometry matches current window size *)
  let screen_rows = max 1 t.screen_height in
  let static_height =
    if t.mode = `Primary_split then
      let max_static = max 0 (screen_rows - 1) in
      let clamped = if t.static_height < 0 then 0 else t.static_height in
      min clamped max_static
    else (
      if t.primary_region_reserved then release_primary_region t;
      0)
  in
  t.static_height <- static_height;
  let render_height = max 1 (screen_rows - static_height) in
  t.height <- render_height;
  Screen.resize t.screen ~width:t.width ~height:render_height;

  (* Explicitly clear the buffers for the new frame *)
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
    let overall_start = monotonic_now () in

    (* 1. Finalize Offsets based on Content or Mode *)
    Screen.set_row_offset t.screen 0;

    if t.mode = `Primary_inline then (
      let h_needed = Screen.active_height t.screen in
      let _, term_rows = Terminal.size t.terminal in
      let query_cursor () =
        match Terminal.query_cursor_position ~timeout:0.02 t.terminal with
        | Some (row, _) ->
            t.inline_base_row <- Some row;
            Some row
        | None -> None
      in
      let layout =
        Inline_layout.compute ~current_base_row:t.inline_base_row ~term_rows
          ~h_needed ~query_cursor
      in
      if layout.grow > 0 then
        Terminal.write t.terminal (String.make layout.grow '\n');
      t.inline_base_row <- Some layout.base_row;
      (* Track offset changes to force full diff *)
      let old_offset = t.render_offset in
      t.render_offset <- layout.offset;
      Screen.set_row_offset t.screen layout.offset;
      if layout.offset <> old_offset then t.force_full_next_frame <- true)
    else
      (* Split/Alt mode: render_offset is pre-calculated/static *)
      Screen.set_row_offset t.screen t.render_offset;

    (* 2. Overlays *)
    apply_debug_overlay t t.screen;

    (* 3. Output Synchronization *)
    let use_sync =
      match t.output with
      | `Stdout -> true
      | `Fd fd -> ( try Unix.isatty fd with _ -> false)
    in

    (* All output goes through Terminal to ensure proper serialization
       and prevent interleaving between frame writes and control sequences. *)
    let send s = Terminal.write t.terminal s in
    let sync_on =
      if use_sync then Some (Ansi.Escape.to_string Ansi.Escape.sync_output_on)
      else None
    in
    let sync_off =
      if use_sync then Some (Ansi.Escape.to_string Ansi.Escape.sync_output_off)
      else None
    in

    Option.iter send sync_on;

    (* 4. Render to Bytes *)
    let forced_full = t.force_full_next_frame in
    if forced_full then t.force_full_next_frame <- false;
    let render_buf = Terminal.render_buffer t.terminal in
    let len =
      Screen.render_to_bytes ~full:forced_full t.screen render_buf
    in

    (* 5. Present *)
    let stdout_start = monotonic_now () in
    if len > 0 then Terminal.present t.terminal len;

    (* When sync sequences are enabled, ensure the frame bytes have flushed
       before emitting the trailing sync_off to avoid interleaving on threaded
       writers. *)
    if use_sync then Terminal.drain t.terminal;
    Option.iter send sync_off;
    (match t.output with `Stdout -> Terminal.flush t.terminal | `Fd _ -> ());

    let stdout_end = monotonic_now () in
    let stdout_ms = Float.max 0. ((stdout_end -. stdout_start) *. 1000.) in
    let overall_frame_ms =
      Float.max 0. ((stdout_end -. overall_start) *. 1000.)
    in

    (* 6. Metrics & Dumps *)
    Screen.record_runtime_metrics t.screen
      ~frame_callback_ms:t.last_frame_callback_ms ~overall_frame_ms ~stdout_ms;
    maybe_dump_frame t t.screen

let apply_config t =
  refresh_capabilities t;
  let term = t.terminal in
  let caps = t.caps in
  let desired_mode = if t.raw_mode then `Raw else `Cooked in
  if Terminal.mode term <> desired_mode then
    Terminal.switch_mode term desired_mode;
  t.alt_enabled <-
    update_feature ~cond:true ~desired:(t.mode = `Alt) ~enabled:t.alt_enabled
      ~enable:(fun () ->
        release_primary_region t;
        Terminal.enter_alternate_screen term)
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
    t.frame_interval <- None;
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
  invalidate_inline_baseline t;
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
    invalidate_inline_baseline t;
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
  let target_row = t.render_offset + max 1 row in
  let target_col = max 1 col in
  Screen.set_cursor_position t.screen ~row ~col;
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
  else if t.width = cols && t.screen_height = rows then ()
  else (
    t.width <- cols;
    t.screen_height <- rows;
    Terminal.query_pixel_resolution t.terminal;
    invalidate_inline_baseline t;
    refresh_render_region t;
    t.last_resize_apply_time <- now;
    t.pending_resize <- None;
    request_redraw t)

let handle_resize t cols rows =
  let now = monotonic_now () in
  let in_split_mode = t.mode = `Primary_split in
  if in_split_mode then apply_resize t cols rows now
  else
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
      let now = monotonic_now () in
      if now -. t.last_resize_apply_time >= window_s then
        apply_resize t cols rows now
  | _ -> ()

let adjust_event_for_offset t =
  if t.mode = `Alt then Fun.id
  else
    let offset = t.render_offset in
    (* Map terminal y coordinate to logical coordinate.
       - Static region (y <= offset): return -1 as sentinel
       - Dynamic region (y > offset): return 1-based logical row *)
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
    ignore (flush_stdout_capture t);
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
    release_primary_region t;
    Terminal.set_cursor_visible term true;
    Terminal.close term;
    teardown_stdout_capture t ~flush_remaining:true;
    match t.stdout_tui_fd with
    | Some fd -> ( try Unix.close fd with _ -> ())
    | None -> ())

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
    ?(frame_dump_hits = false) ?(cursor_visible = true) ?explicit_width
    ?render_thread ?(input_timeout = None)
    ?(resize_debounce = Some 0.1) ?initial_caps ?(output = `Stdout)
    ?(signal_handlers = true) () : app =
  let capture, stdout_restore_fd, stdout_tui_fd = setup_stdout_capture output in
  let term_output =
    match (stdout_tui_fd, output) with
    | Some fd, _ -> fd
    | None, `Stdout -> Unix.stdout
    | None, `Fd fd -> fd
  in
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
  let bracketed_paste = bracketed_paste in
  let focus_reporting = focus_reporting && caps.focus_tracking in
  let target_fps = target_fps in
  let effective_mouse_mode =
    if mouse_enabled then
      match mouse with Some m -> Some m | None -> Some `Sgr_any
    else None
  in
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
      screen_height = height;
      output;
      render_offset = 0;
      static_height = 0;
      inline_base_row = None;
      primary_region_reserved = false;
      primary_region_height = 0;
      primary_region_offset = 0;
      last_resize_apply_time = 0.;
      pending_resize = None;
      force_full_next_frame = true;
      frame_interval = None;
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
      stdout_capture = capture;
      stdout_restore_fd;
      stdout_buffer = Buffer.create (16 * 1024);
      stdout_tui_fd;
    }
  and shutdown_fn () = close runtime in
  if signal_handlers then install_signal_handlers ();
  register_shutdown_handler shutdown_fn;
  apply_config runtime;
  Terminal.query_pixel_resolution runtime.terminal;
  runtime.frame_interval <- None;
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
    let user_start = monotonic_now () in
    on_render t;
    let user_end = monotonic_now () in
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
      if t.redraw_requested && t.frame_interval = None then Some 0. else None
    in
    min_opt immediate_redraw_timeout
      (min_opt (min_opt deadline_timeout t.input_timeout) pending_timeout)
  in

  (* Pre-allocate select list for stdout capture check *)
  let capture_select_list =
    match t.stdout_capture with Some cap -> [ cap.read_fd ] | None -> []
  in

  (* Check if we need to render immediately *)
  let should_render_now ~now =
    let deadline_due =
      t.loop_active
      && match t.next_frame_deadline with Some dl -> now >= dl | None -> false
    in
    let has_captured_output =
      match t.stdout_capture with
      | None -> false
      | Some _ -> (
          try
            let ready, _, _ = Unix.select capture_select_list [] [] 0. in
            not (List.is_empty ready)
          with _ -> false)
    in
    let immediate_redraw = t.redraw_requested && t.frame_interval = None in
    has_captured_output || immediate_redraw || deadline_due
  in

  let rec loop last_time =
    if not (running t) then ()
    else (
      maybe_apply_pending_resize t;
      (* Get single timestamp for this iteration *)
      let now = monotonic_now () in
      (* Update frame timing *)
      let frame_interval =
        if t.loop_active then compute_loop_interval t else None
      in
      t.frame_interval <- frame_interval;
      t.next_frame_deadline <-
        (match (frame_interval, t.loop_active) with
        | None, _ | _, false -> None
        | Some _, true -> (
            match t.next_frame_deadline with
            | Some dl when dl > now -> Some dl
            | _ -> Some now));

      (* Check for captured stdout *)
      (match t.stdout_capture with
      | Some _ -> ignore (flush_stdout_capture t)
      | None -> ());

      (* Render if needed *)
      let last_time =
        if should_render_now ~now then (
          t.next_frame_deadline <-
            Option.map (fun iv -> monotonic_now () +. iv) t.frame_interval;
          render_cycle ~now ~last_time)
        else last_time
      in

      (* Wait for input with appropriate timeout *)
      let timeout = compute_timeout ~now in
      let _ = Terminal.read ?timeout t.terminal handle_event in

      loop last_time)
  in
  let start_time = monotonic_now () in
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
