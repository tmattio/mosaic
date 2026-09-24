module Ansi = Ansi
module Grid = Matrix_grid
module Input = Matrix_input
module Screen = Matrix_screen
module Terminal = Matrix_terminal
module Text = Matrix_text

(* Types *)

type kitty_keyboard = [ `Auto | `Disabled | `Enabled of int ]
type mode = [ `Alt | `Primary ]
type debug_overlay_corner = Debug_overlay.corner

module Backend = struct
  type t = {
    now : unit -> float;
    wake : unit -> unit;
    write_output : bytes -> int -> int -> unit;
    read_input : bytes -> int -> int -> int;
    wait : timeout:float option -> on_event:(Input.t -> unit) -> bool;
    input_is_tty : bool;
    output_is_tty : bool;
    terminal_size : unit -> int * int;
    set_raw_mode : bool -> unit;
    flush_input : unit -> unit;
    protect : (unit -> unit) -> unit;
    cleanup : unit -> unit;
  }

  (* The runtime's protocol policy for terminal replies, handed to
     the shared parser so every backend routes responses identically. *)
  let dispatch_response ~terminal ~on_event = function
    | Input.Response.Capability event -> (
        Terminal.apply_capability_event terminal event;
        match event with
        | Input.Response.Color_scheme ((`Dark | `Light) as scheme) ->
            on_event (Input.Color_scheme scheme)
        | _ -> ())
    | Input.Response.Clipboard _ | Input.Response.Osc _
    | Input.Response.Unknown _ ->
        ()

  let query_cursor_position ~terminal ~parser ~timeout ~on_event t =
    Terminal.send terminal Ansi.(to_string (query Cursor_position));
    let result = ref None in
    let on_response = function
      | Input.Response.Capability (Input.Response.Cursor_position (row, col)) ->
          result := Some (row, col)
      | Input.Response.Capability event ->
          Terminal.apply_capability_event terminal event
      | Input.Response.Clipboard _ | Input.Response.Osc _
      | Input.Response.Unknown _ ->
          ()
    in
    let buf = Bytes.create 512 in
    let deadline = t.now () +. timeout in
    let rec loop () =
      if Option.is_some !result then ()
      else
        let remaining = deadline -. t.now () in
        if remaining > 0. then
          if t.wait ~timeout:(Some remaining) ~on_event then (
            match t.read_input buf 0 (Bytes.length buf) with
            | 0 -> ()
            | n when n < 0 -> loop ()
            | n ->
                Input.Parser.feed parser buf 0 n ~now:(t.now ()) ~on_event
                  ~on_response;
                loop ())
          else loop ()
    in
    loop ();
    !result
end

type control_state =
  [ `Idle
  | `Auto_started
  | `Explicit_started
  | `Explicit_paused
  | `Explicit_suspended
  | `Explicit_stopped ]

type config = {
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
  debug_overlay_capacity : int;
  min_tui_height : int;
  pace_redraws : bool;
}

type app = {
  terminal : Terminal.t;
  parser : Input.Parser.t;
  input_buffer : bytes;
  input_ended : bool;
  config : config;
  backend : Backend.t;
  screen : Screen.t;
  (* One retained writer owns the complete terminal frame. *)
  submit_buf : Ansi.Writer.t;
  (* Input events captured during startup (probe, primary anchor), replayed
     before the first backend read. *)
  mutable pending_startup_events : Input.t list;
  mutable running : bool;
  redraw_state : [ `Disabled | `Clean | `Dirty ] Atomic.t;
  mutable width : int;
  mutable height : int;
  (* Primary mode layout *)
  mutable primary : Primary.t;
  (* Scroll optimisation *)
  mutable scroll_hint : Screen.scroll_hint option;
  (* Cursor state tracking — visual caches are valid only while the cursor is
     visible. [None] means the terminal state is unknown (startup, hidden, or
     after a suspend handed the terminal to another program), so the next
     visible frame must emit. *)
  mutable last_cursor_position : (int * int) option;
  mutable last_cursor_visible : bool option;
  mutable last_cursor_shape : Ansi.cursor_shape option;
  mutable last_cursor_color : (int * int * int) option option;
  (* Resize and frame timing *)
  mutable last_resize_apply_time : float;
  mutable pending_resize : (int * int) option;
  mutable force_full_next_frame : bool;
  mutable next_frame_deadline : float option;
  mutable wakeup_deadline : float option;
  mutable last_render_time : float;
  (* Diagnostics *)
  mutable debug_overlay_enabled : bool;
  mutable debug_overlay_cb : Screen.t -> unit;
  mutable frame_dump_every : int;
  mutable frame_dump_dir : string option;
  mutable frame_dump_pattern : string option;
  mutable frame_dump_hits : bool;
  mutable frame_dump_counter : int;
  mutable closed : bool;
  mutable frame_modes_armed : bool;
  mutable loop_active : bool;
  mutable control_state : control_state;
  mutable previous_control_state : control_state;
  mutable live_requests : int;
  mutable restore_modes_on_next_focus : bool;
  mutable capability_window_until : float option;
  (* Whether the current suspend left the alternate-screen buffer, so [resume]
     re-enters it. [false] outside a [~leave_alt:true] suspend. *)
  mutable suspend_left_alt : bool;
}

let with_rollback ~rollback fn =
  try fn ()
  with exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    (try rollback () with _ -> ());
    Printexc.raise_with_backtrace exn backtrace

(* Unix I/O helpers *)

let write_all fd buf off len =
  let rec wait_writable () =
    try ignore (Unix.select [] [ fd ] [] (-1.))
    with Unix.Unix_error (Unix.EINTR, _, _) -> wait_writable ()
  in
  let rec go off remaining =
    if remaining > 0 then
      let n =
        try Unix.write fd buf off remaining with
        | Unix.Unix_error (Unix.EINTR, _, _) -> 0
        | Unix.Unix_error (Unix.EAGAIN, _, _) ->
            wait_writable ();
            0
      in
      go (off + n) (remaining - n)
  in
  go off len

let wake_byte = Bytes.of_string "w"

let wake_fd fd =
  let (_ : int) =
    try Unix.write fd wake_byte 0 1 with Unix.Unix_error _ -> 0
  in
  ()

let drain_wakeup_fd fd =
  let buf = Bytes.create 64 in
  let rec go () =
    match Unix.read fd buf 0 64 with
    | n when n > 0 -> go ()
    | _ -> ()
    | exception Unix.Unix_error _ -> ()
  in
  go ()

(* Process lifecycle *)

let shutdown_handlers : (unit -> unit) list ref = ref []

let run_shutdown_handlers () =
  List.iter (fun f -> try f () with _ -> ()) !shutdown_handlers

let register_shutdown_handler fn = shutdown_handlers := fn :: !shutdown_handlers

let deregister_shutdown_handler fn =
  shutdown_handlers := List.filter (fun f -> f != fn) !shutdown_handlers

let shutdown_signal_handler signum =
  run_shutdown_handlers ();
  exit (128 + signum)

let acquire_signal_dispositions handlers =
  let acquired = ref [] in
  let restore () =
    List.iter
      (fun (signal, previous) ->
        try ignore (Sys.signal signal previous : Sys.signal_behavior)
        with _ -> ())
      !acquired
  in
  try
    List.iter
      (fun (signal, behavior) ->
        match Sys.signal signal behavior with
        | previous -> acquired := (signal, previous) :: !acquired
        | exception Invalid_argument _ -> ())
      handlers;
    let active = ref true in
    fun () ->
      if !active then (
        active := false;
        restore ())
  with exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    restore ();
    Printexc.raise_with_backtrace exn backtrace

let own_process_lifecycle ~signal_handlers ~additional_signals shutdown_fn =
  register_shutdown_handler shutdown_fn;
  try
    let termination_signals =
      if signal_handlers then
        List.map
          (fun signal -> (signal, Sys.Signal_handle shutdown_signal_handler))
          [ Sys.sigterm; Sys.sigint; Sys.sigquit; Sys.sigabrt; Sys.sighup ]
      else []
    in
    let restore_signals =
      acquire_signal_dispositions (termination_signals @ additional_signals)
    in
    let active = ref true in
    fun () ->
      if !active then (
        active := false;
        restore_signals ();
        deregister_shutdown_handler shutdown_fn)
  with exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    deregister_shutdown_handler shutdown_fn;
    Printexc.raise_with_backtrace exn backtrace

let () = at_exit run_shutdown_handlers

(* SIGWINCH: the active Unix app's flag + wakeup pipe fd *)

let winch_received = ref false

(* Unix low-level I/O *)

let wait_input_unix ~input_fd ~wakeup_r ~output_fd =
  let fds = [ input_fd; wakeup_r ] in
  fun ~timeout ~on_event ->
    let readable, _, _ =
      try Unix.select fds [] [] (Option.value ~default:(-1.) timeout)
      with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
    in
    if List.mem wakeup_r readable then drain_wakeup_fd wakeup_r;
    if !winch_received then (
      winch_received := false;
      let cols, rows = Terminal.size output_fd in
      on_event (Input.Resize (cols, rows)));
    List.mem input_fd readable

let rec read_input_unix ~input_fd buf off len =
  match Unix.read input_fd buf off len with
  | n -> n
  | exception Unix.Unix_error (Unix.EINTR, _, _) ->
      read_input_unix ~input_fd buf off len
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> -1

(* Small helpers *)

let clamp lo hi v = max lo (min hi v)

(* Layout *)

let sync_primary_layout t ~resize =
  let region = Primary.live_region t.primary in
  Screen.set_row_offset t.screen region.row_offset;
  if resize then Screen.resize t.screen ~width:t.width ~height:region.height

let force_full_redraw t = t.force_full_next_frame <- true

(* Accessors *)

let size t =
  match t.config.mode with
  | `Alt -> (t.width, t.height)
  | `Primary -> Primary.size t.primary ~width:t.width

let full_size t = (t.width, t.height)
let mode t = t.config.mode
let pixel_resolution t = Terminal.pixel_resolution t.terminal
let terminal t = t.terminal
let capabilities t = Terminal.capabilities t.terminal
let running t = t.running
let now t = t.backend.now ()

let rec request_redraw t =
  match Atomic.get t.redraw_state with
  | `Disabled -> ()
  | (`Clean | `Dirty) as state ->
      if Atomic.compare_and_set t.redraw_state state `Dirty then
        t.backend.wake ()
      else request_redraw t

let schedule_wakeup t deadline =
  if not t.closed then (
    t.wakeup_deadline <- deadline;
    t.backend.wake ())

let redraw_requested t = Atomic.get t.redraw_state = `Dirty

let request_immediate_redraw t =
  if t.closed then ()
  else if t.control_state <> `Explicit_suspended then (
    Atomic.set t.redraw_state `Dirty;
    if t.loop_active then t.next_frame_deadline <- Some (t.backend.now ())
    else t.last_render_time <- Float.neg_infinity;
    t.backend.wake ())

let refresh_capabilities t =
  let caps = Terminal.capabilities t.terminal in
  let color_depth = if caps.rgb then `Truecolor else `Ansi256 in
  Screen.apply_capabilities t.screen ~explicit_width:caps.explicit_width
    ~explicit_cursor_positioning:caps.explicit_cursor_positioning
    ~hyperlinks:caps.hyperlinks ~color_depth

let startup_capability_window_s = 5.

let capability_parser_context : Input.Parser.protocol_context =
  {
    kitty_keyboard = true;
    explicit_width_cpr = true;
    startup_cursor_cpr = true;
    pixel_resolution = true;
    private_capability_replies = true;
  }

let update_capability_parser_context t ~now =
  match t.capability_window_until with
  | Some deadline when now <= deadline ->
      Input.Parser.set_protocol_context t.parser capability_parser_context
  | Some _ ->
      t.capability_window_until <- None;
      Input.Parser.set_protocol_context t.parser
        Input.Parser.default_protocol_context
  | None -> ()

let refresh_after_capability_change t before =
  let after = Terminal.capabilities t.terminal in
  if after <> before then (
    refresh_capabilities t;
    force_full_redraw t;
    request_redraw t)

let refresh_render_region ?(effective = false) t =
  match t.config.mode with
  | `Alt ->
      Screen.set_row_offset t.screen 0;
      Screen.resize t.screen ~width:t.width ~height:t.height
  | `Primary ->
      (* Resize plans carry no terminal ops (see Primary.resize); the layout
         sync below covers [region_changed], so only the redraw flags need
         routing. *)
      let primary, plan =
        Primary.resize t.primary ~terminal_height:(max 1 t.height)
      in
      t.primary <- primary;
      if plan.invalidate_presented then Screen.invalidate_presented t.screen;
      if plan.force_full_redraw then force_full_redraw t;
      if effective then (
        let region = Primary.effective_region t.primary in
        Screen.set_row_offset t.screen region.row_offset;
        Screen.resize t.screen ~width:t.width ~height:region.height)
      else sync_primary_layout t ~resize:true

(* Frame timing *)

let compute_loop_interval t =
  match t.config.target_fps with
  | Some fps when fps > 0. -> Some (1. /. fps)
  | _ -> None

let update_loop_active t =
  let active =
    match t.control_state with
    | `Explicit_started | `Auto_started -> true
    | `Idle | `Explicit_paused | `Explicit_suspended | `Explicit_stopped ->
        false
  in
  if t.loop_active <> active then (
    t.loop_active <- active;
    if active then t.next_frame_deadline <- Some (t.backend.now ())
    else t.next_frame_deadline <- None)

(* Static output (primary mode only) *)

let erase_entire_line = Ansi.(to_string (erase_line ~mode:`All))
let sgr_reset = Ansi.(to_string reset)
let sync_output_on = Ansi.(to_string (enable Sync_output))
let sync_output_off = Ansi.(to_string (disable Sync_output))
let cursor_hide_seq = Ansi.(to_string (disable Cursor_visible))
let cursor_show_seq = Ansi.(to_string (enable Cursor_visible))
let erase_below_seq = Ansi.(to_string erase_below_cursor)
let clear_and_home_seq = Ansi.(to_string clear_and_home)
let clear_scrollback_seq = Ansi.(to_string (erase_display ~mode:`Scrollback))
let reset_scroll_region_seq = Ansi.(to_string reset_scrolling_region)
let reset_cursor_color_seq = Ansi.(to_string reset_cursor_color)

(* Helpers for buffered frame output. All frame content is accumulated into
   an ANSI writer and flushed as a single write in submit. *)

let buf_cursor_position buf ~row ~col =
  Ansi.cursor_position ~row:(max 1 row) ~col:(max 1 col) buf

let buf_erase_line buf = Ansi.Writer.write_string buf erase_entire_line

let apply_primary_op buf = function
  | Primary.Move_cursor { row; col } -> buf_cursor_position buf ~row ~col
  | Primary.Reset_sgr -> Ansi.Writer.write_string buf sgr_reset
  | Primary.Write s -> Ansi.Writer.write_string buf s
  | Primary.Erase_line -> buf_erase_line buf
  | Primary.Erase_below -> Ansi.Writer.write_string buf erase_below_seq
  | Primary.Clear_and_home -> Ansi.Writer.write_string buf clear_and_home_seq
  | Primary.Clear_scrollback ->
      Ansi.Writer.write_string buf clear_scrollback_seq
  | Primary.Scroll_up n -> Ansi.scroll_up ~n buf
  | Primary.Set_scroll_region { top; bottom } ->
      Ansi.set_scrolling_region ~top ~bottom buf
  | Primary.Reset_scroll_region ->
      Ansi.Writer.write_string buf reset_scroll_region_seq

let apply_primary_plan ?(resize_screen = true) t ~buf (plan : Primary.plan) =
  List.iter (apply_primary_op buf) plan.terminal_ops;
  if plan.invalidate_presented then Screen.invalidate_presented t.screen;
  if plan.region_changed then
    begin if resize_screen then sync_primary_layout t ~resize:true
    else
      let region = Primary.live_region t.primary in
      Screen.set_row_offset t.screen region.row_offset
    end;
  if plan.force_full_redraw then t.force_full_next_frame <- true

(* Apply queued primary static writes before the live viewport render. The
   Primary planner owns the viewport movement, DECSTBM lifetime, and baseline
   invalidation decisions. *)
let flush_static_queue ?(resize_screen = true) t ~buf =
  let primary, plan = Primary.flush_static t.primary in
  t.primary <- primary;
  if plan.terminal_ops <> [] then (
    Ansi.Writer.write_string buf "\027[?7l";
    apply_primary_plan ~resize_screen t ~buf plan;
    Ansi.Writer.write_string buf "\027[?7h")

let effective_size t =
  match t.config.mode with
  | `Alt -> (t.width, t.height)
  | `Primary -> Primary.effective_size t.primary ~width:t.width

let static_snapshot t ?rows grid =
  if Grid.width grid > t.width then
    invalid_arg "Matrix.static_write: grid is wider than the terminal";
  let rows = Option.value rows ~default:(Grid.height grid) in
  let text = Grid.to_ansi ~reset:false ~complete_rows:true ~rows grid in
  (rows, text)

let static_write ?(preserve_live_region = false) ?rows t grid =
  if t.config.mode <> `Alt then
    let rows, text = static_snapshot t ?rows grid in
    if rows > 0 then (
      t.primary <-
        Primary.enqueue_static t.primary { text; rows; preserve_live_region };
      request_redraw t)

let static_replace ?(preserve_live_region = false) ?rows t grid =
  if t.config.mode <> `Alt then (
    let rows, text = static_snapshot t ?rows grid in
    t.primary <-
      Primary.replace_static t.primary { text; rows; preserve_live_region };
    request_redraw t)

(* Immediate action, not part of the frame pipeline: the plan's terminal ops
   are encoded and sent directly, not queued into the frame buffer, so the
   primary screen clears before the next frame renders. The plan's flags are
   still routed like any other plan; [region_changed] is covered by
   [refresh_render_region], which also folds in the terminal resize. *)
let static_clear t =
  if t.config.mode = `Alt then ()
  else
    let primary, plan = Primary.clear_static t.primary in
    t.primary <- primary;
    let buf = Ansi.Writer.create 16 in
    List.iter (apply_primary_op buf) plan.terminal_ops;
    Terminal.send t.terminal (Ansi.Writer.contents buf);
    if plan.invalidate_presented then Screen.invalidate_presented t.screen;
    if plan.force_full_redraw then force_full_redraw t;
    let cols, rows = t.backend.terminal_size () in
    t.width <- max 1 cols;
    t.height <- max 1 rows;
    refresh_render_region t;
    request_redraw t

let set_scroll_hint t hint = t.scroll_hint <- Some hint

(* Protocol configuration *)

let apply_config t =
  refresh_capabilities t;
  let terminal = t.terminal in
  let caps = Terminal.capabilities terminal in
  t.backend.set_raw_mode t.config.raw_mode;
  if t.config.mode = `Alt then (
    Terminal.enter_alternate_screen terminal;
    let cursor = Screen.cursor t.screen in
    Screen.set_cursor t.screen { cursor with visible = false })
  else Terminal.leave_alternate_screen terminal;
  t.force_full_next_frame <- true;
  (match t.config.mouse_mode with
  | Some mode -> Terminal.set_mouse_mode terminal mode
  | None -> Terminal.set_mouse_mode terminal `Off);
  Terminal.enable_bracketed_paste terminal t.config.bracketed_paste;
  Terminal.enable_focus_reporting terminal
    (caps.focus_tracking && t.config.focus_reporting);
  (match t.config.kitty_keyboard with
  | `Disabled -> Terminal.enable_kitty_keyboard terminal false
  | `Auto -> Terminal.enable_kitty_keyboard terminal caps.kitty_keyboard
  | `Enabled flags -> Terminal.enable_kitty_keyboard ~flags terminal true);
  Terminal.enable_modify_other_keys terminal
    (not (Terminal.kitty_keyboard_enabled terminal));
  Terminal.set_unicode_width terminal
    (if caps.unicode_width <> `Wcwidth && not t.config.explicit_width then
       caps.unicode_width
     else `Wcwidth);
  refresh_render_region t

(* Lifecycle: Prepare / Grid / Submit *)

(* No eager buffer clear here: Screen defers the post-present clear of the
   next grid and hit grid to their first observation ([Screen.next_grid],
   [Screen.build], ...), so both direct-draw apps reading {!grid} and
   renderer hosts building through [Screen.build] pay for exactly one clear
   per frame instead of clearing here and again downstream. *)
let prepare t =
  refresh_capabilities t;
  ignore (Atomic.compare_and_set t.redraw_state `Dirty `Clean);
  refresh_render_region ~effective:true t

let grid t = Screen.next_grid t.screen
let current_grid t = Screen.current_grid t.screen
let hits t = Screen.next_hit_grid t.screen
let screen t = t.screen

(* Ensure primary layout stays consistent. Grows the dynamic region when
   content or hints require more rows than the live viewport. Returns a render
   height when the grid still exceeds the available space. *)
let adjust_primary_layout t ~buf ~required_rows_hint =
  let active_rows = max 1 (Screen.active_height t.screen) in
  let primary, plan, render_height =
    Primary.apply_required_rows t.primary ~active_rows
      ~required_rows:required_rows_hint
  in
  t.primary <- primary;
  apply_primary_plan ~resize_screen:false t ~buf plan;
  match render_height with
  | Some _ as limit -> limit
  | None ->
      let live_height = (Primary.live_region t.primary).height in
      let grid_h = Grid.height (Screen.next_grid t.screen) in
      if grid_h > live_height then Some live_height else None

(* Apply cursor position, style, and color into the frame buffer.
   Style and color are only emitted when they differ from the last frame. *)
let cursor_shape (cursor : Screen.cursor) : Ansi.cursor_shape =
  Ansi.cursor_shape_of_style ~style:cursor.style ~blinking:cursor.blinking

let cursor_output_position t ~(cursor : Screen.cursor) ~cursor_max_row =
  match cursor.position with
  | Some (x, y) ->
      let row =
        match t.config.mode with
        | `Primary ->
            Primary.terminal_cursor_row t.primary ~live_row:y
              ~live_height:cursor_max_row
        | `Alt -> clamp 0 (cursor_max_row - 1) y + 1
      in
      Some (row, max 1 (x + 1))
  | None when cursor.visible && t.config.mode = `Primary ->
      Some
        ( Primary.default_terminal_cursor_row t.primary
            ~live_height:cursor_max_row,
          1 )
  | None -> None

(* Allocation-free "changed since last frame" checks: comparing through the
   stored options directly avoids wrapping the current value in [Some]. *)
let[@inline] visible_dirty t visible =
  match t.last_cursor_visible with Some v -> v <> visible | None -> true

let[@inline] shape_dirty t shape =
  match t.last_cursor_shape with Some s -> s <> shape | None -> true

let[@inline] color_dirty t color =
  match t.last_cursor_color with Some c -> c <> color | None -> true

let cursor_dirty t ~(cursor : Screen.cursor) ~position =
  position <> t.last_cursor_position
  || visible_dirty t cursor.visible
  || cursor.visible
     && (shape_dirty t (cursor_shape cursor) || color_dirty t cursor.color)

let apply_cursor_state t ~buf ~(cursor : Screen.cursor) ~position =
  (match position with
  | Some (row, col) ->
      buf_cursor_position buf ~row ~col;
      if cursor.visible then Ansi.Writer.write_string buf cursor_show_seq
  | None -> ());
  if cursor.visible then begin
    let shape = cursor_shape cursor in
    if shape_dirty t shape then begin
      Ansi.cursor_style ~shape buf;
      Terminal.note_appearance_emitted t.terminal `Cursor_style;
      t.last_cursor_shape <- Some shape
    end;
    if color_dirty t cursor.color then begin
      (match cursor.color with
      | Some (r, g, b) ->
          Ansi.cursor_color ~r ~g ~b buf;
          Terminal.note_appearance_emitted t.terminal `Cursor_color
      | None ->
          Ansi.Writer.write_string buf reset_cursor_color_seq;
          Terminal.note_appearance_reset t.terminal `Cursor_color);
      t.last_cursor_color <- Some cursor.color
    end
  end
  else begin
    t.last_cursor_shape <- None;
    t.last_cursor_color <- None
  end;
  t.last_cursor_position <- position;
  t.last_cursor_visible <- Some cursor.visible

(* All frame output is accumulated into a single buffer and flushed as one
   write. Skips the write entirely when nothing changed. *)
let submit ?primary_required_rows t =
  if not t.running then ()
  else begin
    if t.debug_overlay_enabled then t.debug_overlay_cb t.screen;
    let cursor = Screen.cursor t.screen in
    let caps = Terminal.capabilities t.terminal in
    let use_sync = Terminal.tty t.terminal && caps.sync in
    let buf = t.submit_buf in
    Ansi.Writer.reset_pos buf;

    (* Preamble: BSU + cursor hide. *)
    if use_sync then Ansi.Writer.write_string buf sync_output_on;
    Ansi.Writer.write_string buf cursor_hide_seq;
    let preamble_len = Ansi.Writer.len buf in

    (* No cursor homing here: the diff renderer emits an absolute
       cursor_position for every run it touches, so anything written between
       the preamble capture and the skip check would defeat the
       unchanged-frame write skip. *)
    let render_height =
      match t.config.mode with
      | `Primary ->
          adjust_primary_layout t ~buf ~required_rows_hint:primary_required_rows
      | `Alt -> None
    in
    if t.config.mode = `Primary then
      flush_static_queue ~resize_screen:false t ~buf;
    let render_height =
      match t.config.mode with
      | `Alt -> render_height
      | `Primary ->
          let live_height = (Primary.live_region t.primary).height in
          Some
            (match render_height with
            | Some limit -> min limit live_height
            | None -> live_height)
    in

    let forced_full =
      let ff = t.force_full_next_frame in
      if ff then t.force_full_next_frame <- false;
      ff
    in
    let scroll_hint =
      let h = t.scroll_hint in
      t.scroll_hint <- None;
      match t.config.mode with `Alt -> h | `Primary -> None
    in
    (* In full mode on primary screen, cap to active_height to skip blank
       rows and erase below content to clear stale rows. *)
    let active_h =
      if forced_full && t.config.mode = `Primary then
        Some (max 1 (Screen.active_height t.screen))
      else None
    in
    let render_height =
      match active_h with
      | Some active -> (
          match render_height with
          | Some limit -> Some (min limit active)
          | None -> Some active)
      | None -> render_height
    in
    let viewport =
      match render_height with
      | None -> None
      | Some height ->
          Some { Screen.y = Primary.render_offset t.primary; height }
    in
    Screen.render_into ~full:forced_full ?scroll_hint ?viewport t.screen buf;
    (match active_h with
    | Some active ->
        let offset = Primary.render_offset t.primary in
        if offset + active < t.height then (
          buf_cursor_position buf ~row:(offset + active + 1) ~col:1;
          Ansi.Writer.write_string buf erase_below_seq)
    | None -> ());

    (* Skip the write when nothing beyond the preamble was produced and cursor
       state is unchanged. *)
    let cursor_max_row =
      match t.config.mode with
      | `Primary -> (
          match render_height with
          | Some limit -> max 1 limit
          | None -> (Primary.live_region t.primary).height)
      | `Alt -> t.height
    in
    let position = cursor_output_position t ~cursor ~cursor_max_row in
    if Ansi.Writer.len buf > preamble_len || cursor_dirty t ~cursor ~position
    then begin
      if t.config.mode = `Alt then Ansi.cursor_position ~row:t.height ~col:1 buf;
      apply_cursor_state t ~buf ~cursor ~position;
      if use_sync then Ansi.Writer.write_string buf sync_output_off;

      t.frame_modes_armed <- true;
      t.backend.write_output (Ansi.Writer.buffer buf) 0 (Ansi.Writer.len buf)
    end;

    if t.frame_dump_every > 0 then (
      t.frame_dump_counter <- t.frame_dump_counter + 1;
      if t.frame_dump_counter mod t.frame_dump_every = 0 then
        Frame_dump.snapshot ?dir:t.frame_dump_dir ?pattern:t.frame_dump_pattern
          ~hits:t.frame_dump_hits t.screen)
  end

(* Control state *)

let stop t =
  if not t.closed then (
    t.running <- false;
    t.control_state <- `Explicit_stopped;
    update_loop_active t;
    Atomic.set t.redraw_state `Clean;
    t.backend.wake ())

let request_live t =
  if t.closed then ()
  else (
    t.live_requests <- t.live_requests + 1;
    if t.control_state = `Idle && t.live_requests > 0 then (
      t.control_state <- `Auto_started;
      update_loop_active t;
      Atomic.set t.redraw_state `Dirty;
      t.backend.wake ()))

let drop_live t =
  t.live_requests <- max 0 (t.live_requests - 1);
  if t.control_state = `Auto_started && t.live_requests = 0 then (
    t.control_state <- `Idle;
    update_loop_active t)

let start t =
  if not t.closed then (
    t.control_state <- `Explicit_started;
    update_loop_active t;
    if not t.running then t.running <- true;
    Atomic.set t.redraw_state `Dirty;
    t.backend.wake ())

let pause t =
  t.control_state <- `Explicit_paused;
  update_loop_active t;
  if not t.closed then
    ignore (Atomic.compare_and_set t.redraw_state `Disabled `Clean)

(* Frame writes bypass the terminal handle and may stop between mode changes.
   Attempt every reset independently, before handing the terminal back. *)
let restore_frame_modes t =
  if t.frame_modes_armed then (
    (try Terminal.send t.terminal sync_output_off with _ -> ());
    (try Terminal.send t.terminal reset_scroll_region_seq with _ -> ());
    try Terminal.send t.terminal "\027[?7h" with _ -> ())

let suspend ?(leave_alt = false) t =
  (* A second suspend must be a no-op: it would overwrite
     [previous_control_state] with [`Explicit_suspended], after which every
     resume restores the suspended state and the app can never render again.
     It would also drop a pending [~leave_alt:true] re-enter obligation. *)
  if t.closed || t.control_state = `Explicit_suspended then ()
  else (
    Atomic.set t.redraw_state `Disabled;
    t.previous_control_state <- t.control_state;
    t.control_state <- `Explicit_suspended;
    update_loop_active t;
    Input.Parser.reset t.parser;
    force_full_redraw t;
    (* The suspended program owns the terminal; whatever cursor state it leaves
       behind is unknown, so the first frame after resume must re-emit. *)
    t.last_cursor_position <- None;
    t.last_cursor_visible <- None;
    t.last_cursor_shape <- None;
    t.last_cursor_color <- None;
    t.backend.protect (fun () -> restore_frame_modes t);
    (try Terminal.set_mouse_mode t.terminal `Off with _ -> ());
    (try Terminal.enable_bracketed_paste t.terminal false with _ -> ());
    (try Terminal.enable_focus_reporting t.terminal false with _ -> ());
    (try Terminal.enable_kitty_keyboard t.terminal false with _ -> ());
    (try Terminal.enable_modify_other_keys t.terminal false with _ -> ());
    (try t.backend.set_raw_mode false with _ -> ());
    t.suspend_left_alt <- leave_alt;
    (if leave_alt then
       try Terminal.leave_alternate_screen t.terminal with _ -> ());
    try t.backend.flush_input () with _ -> ())

let resume t =
  if t.control_state <> `Explicit_suspended then ()
  else (
    (if t.config.raw_mode then try t.backend.set_raw_mode true with _ -> ());
    (try t.backend.flush_input () with _ -> ());
    Input.Parser.reset t.parser;
    if t.suspend_left_alt then (
      t.suspend_left_alt <- false;
      try Terminal.enter_alternate_screen t.terminal with _ -> ());
    (if t.config.mode = `Primary then
       let height = max 1 t.height in
       match
         Backend.query_cursor_position ~terminal:t.terminal ~parser:t.parser
           ~timeout:0.1
           ~on_event:(function
             | Input.Resize _ as event ->
                 t.pending_startup_events <-
                   t.pending_startup_events @ [ event ]
             | _ -> ())
           t.backend
       with
       | Some (row, col) ->
           let anchor =
             Primary.anchor_of_cursor ~terminal_height:height ~row ~col
           in
           if anchor.scroll_bottom then Terminal.send t.terminal "\r\n";
           t.primary <-
             Primary.reanchor t.primary ~render_offset:anchor.render_offset
               ~static_needs_newline:anchor.static_needs_newline
       | None -> ());
    force_full_redraw t;
    apply_config t;
    Grid.clear (Screen.next_grid t.screen);
    Screen.Hit_grid.clear (Screen.next_hit_grid t.screen);
    t.control_state <- t.previous_control_state;
    if t.control_state = `Auto_started && t.live_requests = 0 then
      t.control_state <- `Idle;
    update_loop_active t;
    Atomic.set t.redraw_state `Dirty;
    t.backend.wake ())

(* Cursor *)

(* Cursor setters update Screen state only. The actual terminal output
   is deferred to submit's apply_cursor_state, which writes everything
   into the frame buffer as a single write. *)

let set_cursor ?visible ?style t =
  let cursor = Screen.cursor t.screen in
  let visible = Option.value visible ~default:cursor.visible in
  let style = Option.value style ~default:cursor.style in
  Screen.set_cursor t.screen { cursor with visible; style }

let set_cursor_style t ~style ~blinking =
  let cursor = Screen.cursor t.screen in
  Screen.set_cursor t.screen { cursor with style; blinking }

let set_cursor_position t ~row ~col =
  let max_row =
    if t.config.mode = `Primary then (Primary.live_region t.primary).height
    else max 1 t.height
  in
  let row = clamp 1 max_row row in
  let col = max 1 col in
  let cursor = Screen.cursor t.screen in
  Screen.set_cursor t.screen { cursor with position = Some (col - 1, row - 1) }

let set_cursor_color t ~r ~g ~b =
  let clamp_01 f = Float.max 0. (Float.min 1. f) in
  let to_byte f = int_of_float (Float.round (f *. 255.)) |> clamp 0 255 in
  let cursor = Screen.cursor t.screen in
  Screen.set_cursor t.screen
    {
      cursor with
      color =
        Some (to_byte (clamp_01 r), to_byte (clamp_01 g), to_byte (clamp_01 b));
    }

(* Resize *)

let apply_resize t cols rows now =
  (* Reaching a resize decision consumes any pending debounced resize in every
     branch, including the no-ops for an unchanged or degenerate size. A
     pending entry left behind keeps its debounce window elapsed, which pins
     [compute_timeout] at zero and spins the loop — starving input — until a
     differently-sized resize clears it. *)
  t.pending_resize <- None;
  if cols <= 0 || rows <= 0 then ()
  else if t.width = cols && t.height = rows then ()
  else (
    t.width <- cols;
    t.height <- rows;
    Terminal.query_pixel_resolution t.terminal;
    force_full_redraw t;
    refresh_render_region t;
    t.last_resize_apply_time <- now;
    request_redraw t)

let handle_resize t cols rows =
  let now = t.backend.now () in
  match t.config.resize_debounce with
  | None -> apply_resize t cols rows now
  | Some window_s ->
      if
        t.last_resize_apply_time = 0.
        || now -. t.last_resize_apply_time >= window_s
      then apply_resize t cols rows now
      else t.pending_resize <- Some (cols, rows)

let maybe_apply_pending_resize t =
  match (t.pending_resize, t.config.resize_debounce) with
  | Some (cols, rows), Some window_s ->
      let now = t.backend.now () in
      if now -. t.last_resize_apply_time >= window_s then
        apply_resize t cols rows now
  | _ -> ()

(* Event dispatch *)

let adjust_event_for_offset t =
  if t.config.mode = `Alt then Fun.id
  else
    let map_y = Primary.map_mouse_y t.primary in
    fun event ->
      match event with
      | Input.Mouse mouse ->
          Input.Mouse { mouse with Input.Mouse.y = map_y mouse.y }
      | event -> event

let classify_event t evt =
  match evt with
  | Input.Resize (cols, rows) ->
      handle_resize t cols rows;
      Some (`Resize (cols, rows))
  | Input.Focus ->
      if t.restore_modes_on_next_focus then (
        t.restore_modes_on_next_focus <- false;
        Terminal.restore_modes t.terminal);
      let adjusted = adjust_event_for_offset t evt in
      Some (`Input adjusted)
  | Input.Blur ->
      t.restore_modes_on_next_focus <- true;
      let adjusted = adjust_event_for_offset t evt in
      Some (`Input adjusted)
  | evt ->
      let adjusted = adjust_event_for_offset t evt in
      if t.config.exit_on_ctrl_c then
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
  else
    t.backend.protect (fun () ->
        Atomic.set t.redraw_state `Disabled;
        t.closed <- true;
        t.running <- false;
        t.control_state <- `Explicit_stopped;
        update_loop_active t;
        restore_frame_modes t;
        (try
           let is_tty = Terminal.tty t.terminal in
           if t.config.mode = `Primary && is_tty then (
             let height = max 1 t.height in
             let render_offset =
               clamp 0 (height - 1) (Primary.render_offset t.primary)
             in
             let start_row =
               if Primary.static_needs_newline t.primary then render_offset + 1
               else max 1 render_offset
             in
             for row = start_row to height do
               Terminal.move_cursor t.terminal ~row ~col:1
                 ~visible:(Terminal.cursor_visible t.terminal);
               Terminal.send t.terminal erase_entire_line
             done;
             Terminal.move_cursor t.terminal ~row:start_row ~col:1 ~visible:true);
           (* Flush pending mouse/input bytes both before and after mode teardown.
          This avoids leaking trailing SGR mouse payloads back to the shell. *)
           (try t.backend.flush_input () with _ -> ());
           Terminal.close t.terminal
         with _ -> ());
        (try t.backend.flush_input () with _ -> ());
        (try t.backend.set_raw_mode false with _ -> ());
        (try t.backend.flush_input () with _ -> ());
        try t.backend.cleanup () with _ -> ())

(* Construct the runtime over backend I/O. *)

let attach ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(mouse_enabled = true) ?(mouse = None) ?(bracketed_paste = true)
    ?(focus_reporting = true) ?(kitty_keyboard = `Auto) ?(exit_on_ctrl_c = true)
    ?(debug_overlay = false) ?(debug_overlay_corner = `Bottom_right)
    ?(debug_overlay_capacity = 120) ?(frame_dump_every = 0) ?frame_dump_dir
    ?frame_dump_pattern ?(frame_dump_hits = false)
    ?(cursor_visible = mode = `Alt) ?(explicit_width = false)
    ?(input_timeout = None) ?(resize_debounce = Some 0.1) ?(min_tui_height = 1)
    ?(start_idle = false) ?(pace_redraws = true) ?initial_caps
    ~(backend : Backend.t) () =
  let config =
    let effective_mouse_mode =
      if mouse_enabled then Some (Option.value ~default:`Sgr_any mouse)
      else None
    in
    {
      mode;
      raw_mode;
      mouse_mode = effective_mouse_mode;
      bracketed_paste;
      focus_reporting;
      kitty_keyboard;
      exit_on_ctrl_c;
      target_fps;
      explicit_width;
      input_timeout;
      resize_debounce;
      debug_overlay_capacity;
      min_tui_height;
      pace_redraws;
    }
  in
  let terminal =
    with_rollback
      ~rollback:(fun () -> backend.protect backend.cleanup)
      (fun () ->
        Terminal.make ~tty:backend.output_is_tty ?initial_caps
          ~output:(fun s ->
            backend.write_output (Bytes.unsafe_of_string s) 0 (String.length s))
          ())
  in
  let app_ref = ref None in
  let rollback () =
    match !app_ref with
    | Some app -> close app
    | None ->
        backend.protect (fun () ->
            (try Terminal.close terminal with _ -> ());
            (try backend.set_raw_mode false with _ -> ());
            backend.cleanup ())
  in
  with_rollback ~rollback (fun () ->
      let parser = Input.Parser.create () in
      let input_ended = ref false in
      let read_input buf off len =
        let n = if !input_ended then 0 else backend.read_input buf off len in
        if n = 0 then input_ended := true;
        n
      in
      if backend.input_is_tty && raw_mode then backend.set_raw_mode true;
      let startup_events = ref [] in
      let queue_startup_event event =
        startup_events := event :: !startup_events
      in
      if backend.input_is_tty && raw_mode then
        Terminal.probe ~timeout:0.5 ~now:backend.now
          ~on_event:queue_startup_event ~read_into:read_input
          ~wait_readable:(fun ~timeout ->
            backend.wait ~timeout:(Some timeout) ~on_event:queue_startup_event)
          ~parser terminal;
      let cols, rows = backend.terminal_size () in
      let width = max 1 cols in
      let height = max 1 rows in
      let render_offset, static_needs_newline =
        if
          mode = `Primary && backend.input_is_tty && raw_mode
          && not !input_ended
        then
          match
            Backend.query_cursor_position ~terminal ~parser ~timeout:0.1
              ~on_event:queue_startup_event
              { backend with read_input }
          with
          | Some (row, col) ->
              let anchor =
                Primary.anchor_of_cursor ~terminal_height:height ~row ~col
              in
              if anchor.scroll_bottom then Terminal.send terminal "\r\n";
              (anchor.render_offset, anchor.static_needs_newline)
          | None -> (0, false)
        else (0, false)
      in
      let app =
        let primary =
          Primary.create ~terminal_height:height
            ~min_live_height:config.min_tui_height ~render_offset
            ~static_needs_newline
        in
        let caps = Terminal.capabilities terminal in
        let screen =
          Screen.create ~width_method:caps.unicode_width ~cursor_visible
            ~explicit_width:config.explicit_width ~clock:backend.now ()
        in
        let capability_window_until =
          if config.raw_mode && Terminal.tty terminal then
            Some (backend.now () +. startup_capability_window_s)
          else None
        in
        {
          terminal;
          parser;
          input_buffer = Bytes.create 4096;
          input_ended = !input_ended;
          config;
          backend;
          screen;
          submit_buf = Ansi.Writer.create 4096;
          pending_startup_events = List.rev !startup_events;
          running = true;
          redraw_state = Atomic.make `Clean;
          width;
          height;
          primary;
          scroll_hint = None;
          last_cursor_position = None;
          last_cursor_visible = None;
          last_cursor_shape = None;
          last_cursor_color = None;
          last_resize_apply_time = 0.;
          pending_resize = None;
          force_full_next_frame = true;
          next_frame_deadline = None;
          wakeup_deadline = None;
          last_render_time = Float.neg_infinity;
          debug_overlay_enabled = debug_overlay;
          debug_overlay_cb =
            Debug_overlay.on_frame ~corner:debug_overlay_corner
              ~capacity:config.debug_overlay_capacity ();
          frame_dump_every = max 0 frame_dump_every;
          frame_dump_dir;
          frame_dump_pattern;
          frame_dump_hits;
          frame_dump_counter = 0;
          closed = false;
          frame_modes_armed = false;
          loop_active = false;
          control_state =
            (if start_idle then `Idle
             else
               match config.target_fps with
               | Some fps when fps > 0. -> `Explicit_started
               | _ -> `Idle);
          previous_control_state = `Idle;
          live_requests = 0;
          restore_modes_on_next_focus = false;
          capability_window_until;
          suspend_left_alt = false;
        }
      in
      app_ref := Some app;
      apply_config app;
      Terminal.query_pixel_resolution terminal;
      app)

(* Constructor: create a live app with Unix I/O *)

let create ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(mouse_enabled = true) ?(mouse = None) ?(bracketed_paste = true)
    ?(focus_reporting = true) ?(kitty_keyboard = `Auto) ?(exit_on_ctrl_c = true)
    ?(debug_overlay = false) ?(debug_overlay_corner = `Bottom_right)
    ?(debug_overlay_capacity = 120) ?(frame_dump_every = 0) ?frame_dump_dir
    ?frame_dump_pattern ?(frame_dump_hits = false)
    ?(cursor_visible = mode = `Alt) ?(explicit_width = false)
    ?(input_timeout = None) ?(resize_debounce = Some 0.1) ?(output = `Stdout)
    ?(signal_handlers = true) ?initial_caps ?(min_tui_height = 1)
    ?(start_idle = false) ?(pace_redraws = true) () =
  let output_fd = match output with `Stdout -> Unix.stdout | `Fd fd -> fd in
  let input_fd = Unix.stdin in
  let output_is_tty = Terminal.is_tty output_fd in
  let input_is_tty = Terminal.is_tty input_fd in
  let wakeup_r, wakeup_w = Unix.pipe ~cloexec:true () in
  let release_lifecycle = ref ignore in
  let backend_cleaned = ref false in
  let wakeup_lock = Mutex.create () in
  (* A request published before close may reach wake afterwards. Serialize
     wake with pipe disposal so a reused descriptor can never receive it. *)
  let wake_locked () = if not !backend_cleaned then wake_fd wakeup_w in
  let wake () = Mutex.protect wakeup_lock wake_locked in
  let cleanup_backend () =
    (* Signal handlers cannot take [wakeup_lock]. Revoke their access before
       disposing of the pipe; worker wakeups use the lock below. *)
    !release_lifecycle ();
    let cleaned =
      Mutex.protect wakeup_lock (fun () ->
          if !backend_cleaned then false
          else (
            backend_cleaned := true;
            (try Unix.close wakeup_r with _ -> ());
            (try Unix.close wakeup_w with _ -> ());
            true))
    in
    if cleaned then winch_received := false
  in
  let original_termios = ref None in
  let set_raw_mode enabled =
    if enabled then (
      match !original_termios with
      | Some _ -> ()
      | None ->
          if input_is_tty then
            original_termios := Some (Terminal.set_raw input_fd))
    else
      match !original_termios with
      | Some saved ->
          Terminal.restore input_fd saved;
          original_termios := None
      | None -> ()
  in
  let app_ref = ref None in
  (* Before [attach] returns, the constructor owns raw mode and the wakeup
     pipe directly. Afterwards the app's idempotent close path owns both. *)
  let rollback () =
    match !app_ref with
    | Some app -> close app
    | None ->
        (try set_raw_mode false with _ -> ());
        cleanup_backend ()
  in
  with_rollback ~rollback (fun () ->
      Unix.set_nonblock wakeup_r;
      Unix.set_nonblock wakeup_w;
      let backend : Backend.t =
        {
          now = (fun () -> Int64.to_float (Mtime_clock.elapsed_ns ()) *. 1e-9);
          wake;
          write_output = write_all output_fd;
          read_input = read_input_unix ~input_fd;
          wait = wait_input_unix ~input_fd ~wakeup_r ~output_fd;
          input_is_tty;
          output_is_tty;
          terminal_size = (fun () -> Terminal.size output_fd);
          set_raw_mode;
          flush_input =
            (fun () -> if input_is_tty then Terminal.flush_input input_fd);
          protect = (fun f -> f ());
          cleanup = cleanup_backend;
        }
      in
      let app =
        attach ~mode ~raw_mode ~target_fps ~mouse_enabled ~mouse
          ~bracketed_paste ~focus_reporting ~kitty_keyboard ~exit_on_ctrl_c
          ~debug_overlay ~debug_overlay_corner ~debug_overlay_capacity
          ~frame_dump_every ?frame_dump_dir ?frame_dump_pattern ~frame_dump_hits
          ~cursor_visible ~explicit_width ~input_timeout ~resize_debounce
          ?initial_caps ~min_tui_height ~start_idle ~pace_redraws ~backend ()
      in
      app_ref := Some app;
      let shutdown_fn () = close app in
      let winch_handler _ =
        winch_received := true;
        wake_fd wakeup_w
      in
      winch_received := false;
      release_lifecycle :=
        own_process_lifecycle ~signal_handlers
          ~additional_signals:
            [ (Sys.sigwinch, Sys.Signal_handle winch_handler) ]
          shutdown_fn;
      app)

(* Event loop helpers *)

let min_timeout a b =
  match (a, b) with
  | None, x | x, None -> x
  | Some x, Some y -> Some (Float.min x y)

(* A one-shot redraw renders as soon as the frame pacing allows. Outside live
   mode there is no frame deadline, so pacing hangs off the last render time:
   an event storm (streaming deltas, rapid dispatches) coalesces to the target
   fps instead of rendering back-to-back. With [pace_redraws] off — a virtual
   clock where coalescing buys nothing and "waiting for the frame" would move
   time that timers read — a one-shot renders immediately at the current
   instant. Live animation still paces off [target_fps] via the frame
   deadline; only one-shots skip the wait. *)
let one_shot_render_due t ~now =
  redraw_requested t && (not t.loop_active)
  && ((not t.config.pace_redraws)
     ||
     match compute_loop_interval t with
     | None -> true
     | Some interval -> now -. t.last_render_time >= interval)

let compute_timeout t ~now =
  let pending_timeout =
    match (t.pending_resize, t.config.resize_debounce) with
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
  let wakeup_timeout =
    match t.wakeup_deadline with
    | Some dl ->
        let dt = dl -. now in
        if dt <= 0. then Some 0. else Some dt
    | None -> None
  in
  let parser_timeout =
    match Input.Parser.deadline t.parser with
    | Some dl ->
        let dt = dl -. now in
        if dt <= 0. then Some 0. else Some dt
    | None -> None
  in
  let redraw_timeout =
    if redraw_requested t && not t.loop_active then
      if not t.config.pace_redraws then Some 0.
      else
        match compute_loop_interval t with
        | None -> Some 0.
        | Some interval ->
            Some (Float.max 0. (t.last_render_time +. interval -. now))
    else None
  in
  min_timeout redraw_timeout
    (min_timeout wakeup_timeout
       (min_timeout
          (min_timeout deadline_timeout t.config.input_timeout)
          (min_timeout pending_timeout parser_timeout)))

let should_render_now t ~now =
  one_shot_render_due t ~now
  || t.loop_active
     && match t.next_frame_deadline with Some dl -> now >= dl | None -> false

(* Event loop *)

let run ?on_frame ?on_input ?on_resize ?primary_required_rows ~on_render t =
  if not t.running then t.running <- true;
  (match t.control_state with
  | `Auto_started when t.live_requests = 0 ->
      t.control_state <- `Idle;
      update_loop_active t
  | _ -> update_loop_active t);
  if
    t.control_state <> `Explicit_suspended
    && t.control_state <> `Explicit_stopped
  then (
    Atomic.set t.redraw_state `Dirty;
    t.backend.wake ());
  (* [on_resize] always reports the usable surface — {!size}, the live
     viewport in [`Primary] mode — never the raw terminal dimensions of the
     resize event, so the first callback and later ones agree. It fires once
     before the first render and then whenever the applied size changes,
     including debounced resizes applied after their event. *)
  let last_reported_size = ref None in
  let notify_resize ~initial =
    Option.iter
      (fun f ->
        let ((cols, rows) as reported) = size t in
        if initial || !last_reported_size <> Some reported then (
          last_reported_size := Some reported;
          f t ~cols ~rows))
      on_resize
  in
  notify_resize ~initial:true;

  let render_cycle ~now ~last_time =
    Option.iter (fun f -> f t ~dt:(now -. last_time)) on_frame;
    prepare t;
    on_render t;
    let user_end = t.backend.now () in
    let required_rows_hint =
      match primary_required_rows with
      | Some f -> (
          match f t with Some rows when rows > 0 -> Some rows | _ -> None)
      | None -> None
    in
    submit ?primary_required_rows:required_rows_hint t;
    user_end
  in

  let handle_event evt =
    if not t.running then ()
    else
      match classify_event t evt with
      | None -> close t
      | Some (`Resize (cols, rows)) ->
          notify_resize ~initial:false;
          Option.iter (fun f -> f t (Input.Resize (cols, rows))) on_input;
          request_redraw t
      | Some (`Input event) ->
          Option.iter (fun f -> f t event) on_input;
          request_redraw t
  in
  (* Built once: [handle_event] is stable for the whole run, so allocating the
     response dispatcher per read would be pure per-iteration churn. *)
  let on_response =
    Backend.dispatch_response ~terminal:t.terminal ~on_event:handle_event
  in
  let read_events ~now ~timeout =
    update_capability_parser_context t ~now;
    let caps_before = Terminal.capabilities t.terminal in
    let result =
      match t.pending_startup_events with
      | [] ->
          if t.input_ended then `End
          else
            let ready = t.backend.wait ~timeout ~on_event:handle_event in
            if t.closed then `Continue
            else
              let n =
                if ready then
                  t.backend.read_input t.input_buffer 0
                    (Bytes.length t.input_buffer)
                else -1
              in
              if n = 0 then `End
              else (
                if n > 0 then
                  Input.Parser.feed t.parser t.input_buffer 0 n
                    ~now:(t.backend.now ()) ~on_event:handle_event ~on_response;
                Input.Parser.drain t.parser ~now:(t.backend.now ())
                  ~on_event:handle_event ~on_response;
                `Continue)
      | events ->
          t.pending_startup_events <- [];
          List.iter handle_event events;
          `Continue
    in
    refresh_after_capability_change t caps_before;
    match result with
    | `Continue -> ()
    | `End ->
        Input.Parser.finish t.parser ~on_event:handle_event;
        close t
  in

  let rec loop last_time =
    if not (running t) then ()
    else (
      if t.pending_resize <> None then (
        maybe_apply_pending_resize t;
        if t.pending_resize = None then notify_resize ~initial:false);
      let now = t.backend.now () in
      (* A due one-shot wakeup runs the frame callback without a render: it
         exists so time-based work (timers) can advance off the clock while
         the loop is otherwise idle. Whatever the callback dispatches requests
         its own redraw, which the pacing below then honors. *)
      let last_time =
        match t.wakeup_deadline with
        | Some dl when now >= dl ->
            t.wakeup_deadline <- None;
            Option.iter (fun f -> f t ~dt:(now -. last_time)) on_frame;
            now
        | Some _ | None -> last_time
      in
      if should_render_now t ~now then (
        let frame_interval = compute_loop_interval t in
        let last_time = render_cycle ~now ~last_time in
        (* Schedule next frame deadline after rendering: delay =
           target_frame_time - frame_elapsed. The deadline drives only the
           live cadence — left armed while idle it would read as perpetually
           due and spin the loop; one-shot pacing hangs off
           [last_render_time] instead. *)
        let render_end = t.backend.now () in
        t.last_render_time <- render_end;
        t.next_frame_deadline <-
          (if t.loop_active then
             match frame_interval with
             | Some iv ->
                 let elapsed = render_end -. now in
                 let delay = Float.max 0. (iv -. elapsed) in
                 Some (render_end +. delay)
             | None -> None
           else None);
        let timeout = compute_timeout t ~now:render_end in
        read_events ~now:render_end ~timeout;
        loop last_time)
      else
        let timeout = compute_timeout t ~now in
        read_events ~now ~timeout;
        loop last_time)
  in
  let start_time = t.backend.now () in
  Fun.protect
    (fun () -> loop start_time)
    ~finally:(fun () -> if not t.closed then close t)

(* Diagnostics *)

let set_debug_overlay ?corner t ~enabled =
  let previous = t.debug_overlay_enabled in
  t.debug_overlay_enabled <- enabled;
  Option.iter
    (fun c ->
      t.debug_overlay_cb <-
        Debug_overlay.on_frame ~corner:c
          ~capacity:t.config.debug_overlay_capacity ())
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
