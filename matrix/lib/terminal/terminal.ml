open Unix
module Esc = Ansi.Escape

exception Error of string

external get_size : file_descr -> int * int = "terminal_get_size"
external enable_vt : file_descr -> unit = "terminal_enable_vt"

type unicode_width = [ `Wcwidth | `Unicode ]

type capabilities = Caps.t = {
  term : string;
  rgb : bool;
  kitty_keyboard : bool;
  kitty_graphics : bool;
  bracketed_paste : bool;
  focus_tracking : bool;
  unicode_width : unicode_width;
  sgr_pixels : bool;
  color_scheme_updates : bool;
  explicit_width : bool;
  scaled_text : bool;
  sixel : bool;
  sync : bool;
  hyperlinks : bool;
}

type terminal_info = Caps.terminal_info = {
  name : string;
  version : string;
  from_xtversion : bool;
}

let ignore_unix_errors f x = try f x with Unix_error _ -> ()
let ignore_exn f x = try f x with _ -> ()
let osc_prefix = "\027]"
let osc_suffix = "\007"
let[@inline] make_osc payload = osc_prefix ^ payload ^ osc_suffix

type mode = [ `Raw | `Cooked | `Custom of terminal_io -> terminal_io ]

type mouse_mode =
  [ `Off
  | `X10
  | `Normal
  | `Button
  | `Any
  | `Sgr_normal
  | `Sgr_button
  | `Sgr_any ]

type cursor_style = [ `Block | `Line | `Underline ]
type cursor_position = { x : int; y : int; visible : bool }

type cursor_state = {
  mutable x : int;
  mutable y : int;
  mutable visible : bool;
  mutable style : cursor_style;
  mutable blinking : bool;
  mutable color : float * float * float * float;
}

type t = {
  input : file_descr;
  output : file_descr;
  wakeup_r : file_descr;
  wakeup_w : file_descr;
  original_termios : terminal_io option;
  mutable current_mode : mode;
  input_is_tty : bool;
  output_is_tty : bool;
  mutable caps : Caps.t;
  mutable terminal_info : Caps.terminal_info;
  parser : Input.Parser.t;
  pending_events : Input.t Queue.t;
  input_buffer : bytes;
  mutable mouse_mode : mouse_mode;
  cursor : cursor_state;
  mutable alt_screen : bool;
  mutable bracketed_paste_enabled : bool;
  mutable focus_enabled : bool;
  mutable kitty_keyboard_enabled : bool;
  mutable kitty_keyboard_flags : int;
  mutable modify_other_keys_enabled : bool;
  mutable unicode_mode_enabled : bool;
  mutable winch_handler : (unit -> unit) option;
  mutable pixel_resolution : (int * int) option;
  mutable scroll_region : (int * int) option;
  env_overrides : bool;
  mutable writer : Frame_writer.t option;
}

let is_tty fd = try Unix.isatty fd with Unix_error _ -> false

let clamp_color_component x =
  let x = if Float.is_nan x then 0. else x in
  let v = Float.max 0. (Float.min 1. x) in
  int_of_float (Float.round (v *. 255.))

let alternate_on = Esc.(to_string enter_alternate_screen)
let alternate_off = Esc.(to_string exit_alternate_screen)
let focus_on = Esc.(to_string focus_tracking_on)
let focus_off = Esc.(to_string focus_tracking_off)
let paste_on = Esc.(to_string bracketed_paste_on)
let paste_off = Esc.(to_string bracketed_paste_off)
let kitty_kb_push flags = Printf.sprintf "\027[>%du" flags
let kitty_kb_pop = "\027[<u"
let modify_other_keys_on = Esc.(to_string modify_other_keys_on)
let modify_other_keys_off = Esc.(to_string modify_other_keys_off)
let cursor_show = Esc.(to_string show_cursor)
let cursor_hide = Esc.(to_string hide_cursor)
let sgr_enable = Esc.(to_string mouse_sgr_mode_on)
let unicode_on = Esc.(to_string unicode_mode_on)
let unicode_off = Esc.(to_string unicode_mode_off)
let cursor_position_request = Esc.(to_string request_cursor_position)
let reset_sgr = Esc.(to_string reset)
let erase_below = Esc.(to_string erase_below_cursor)
let kitty_cursor_block = Esc.(to_string cursor_block)
let kitty_cursor_block_blink = Esc.(to_string cursor_block_blink)
let kitty_cursor_line = Esc.(to_string cursor_line)
let kitty_cursor_line_blink = Esc.(to_string cursor_line_blink)
let kitty_cursor_underline = Esc.(to_string cursor_underline)
let kitty_cursor_underline_blink = Esc.(to_string cursor_underline_blink)

let submit_string t s =
  match t.writer with
  | Some w -> Frame_writer.submit_string w s
  | None -> raise (Error "Terminal.write: writer not initialized")

let send t seq = if t.output_is_tty then submit_string t seq

let disable_all_mouse t =
  send t Esc.(to_string mouse_tracking_off);
  send t Esc.(to_string mouse_button_tracking_off);
  send t Esc.(to_string mouse_motion_off);
  send t Esc.(to_string urxvt_mouse_off);
  send t Esc.(to_string mouse_sgr_mode_off)

let toggle_feature t ~current ~set ~enable ~on_seq ~off_seq =
  if not t.output_is_tty then set enable
  else if current () = enable then ()
  else (
    send t (if enable then on_seq else off_seq);
    set enable)

let set_kitty_keyboard t ~enable ~flags =
  if not t.output_is_tty then (
    t.kitty_keyboard_enabled <- enable;
    t.kitty_keyboard_flags <- flags)
  else if enable then (
    if (not t.kitty_keyboard_enabled) || t.kitty_keyboard_flags <> flags then (
      send t (kitty_kb_push flags);
      t.kitty_keyboard_enabled <- true;
      t.kitty_keyboard_flags <- flags))
  else if t.kitty_keyboard_enabled then (
    send t kitty_kb_pop;
    t.kitty_keyboard_enabled <- false)

let set_mouse_mode t mode =
  if not t.output_is_tty then t.mouse_mode <- mode
  else if t.mouse_mode = mode then ()
  else (
    disable_all_mouse t;
    (match mode with
    | `Off -> ()
    | `X10 -> send t Esc.(to_string mouse_x10_on)
    | `Normal -> send t Esc.(to_string mouse_tracking_on)
    | `Button -> send t Esc.(to_string mouse_button_tracking_on)
    | `Any -> send t Esc.(to_string mouse_motion_on)
    | `Sgr_normal ->
        (* SGR + press/release only (1000) *)
        send t sgr_enable;
        send t Esc.(to_string mouse_tracking_on)
    | `Sgr_button ->
        (* SGR + press/release/drag (1000 + 1002) *)
        send t sgr_enable;
        send t Esc.(to_string mouse_tracking_on);
        send t Esc.(to_string mouse_button_tracking_on)
    | `Sgr_any ->
        (* SGR + all motion (1000 + 1002 + 1003) *)
        send t sgr_enable;
        send t Esc.(to_string mouse_tracking_on);
        send t Esc.(to_string mouse_button_tracking_on);
        send t Esc.(to_string mouse_motion_on));
    t.mouse_mode <- mode)

let mouse_mode t = t.mouse_mode

let set_unicode_enabled t enable =
  toggle_feature t
    ~current:(fun () -> t.unicode_mode_enabled)
    ~set:(fun v -> t.unicode_mode_enabled <- v)
    ~enable ~on_seq:unicode_on ~off_seq:unicode_off

let set_unicode_width t width =
  let enable = match width with `Unicode -> true | `Wcwidth -> false in
  set_unicode_enabled t enable;
  t.caps <- { t.caps with unicode_width = width }

let enable_bracketed_paste t enable =
  toggle_feature t
    ~current:(fun () -> t.bracketed_paste_enabled)
    ~set:(fun v -> t.bracketed_paste_enabled <- v)
    ~enable ~on_seq:paste_on ~off_seq:paste_off

let enable_focus_reporting t enable =
  toggle_feature t
    ~current:(fun () -> t.focus_enabled)
    ~set:(fun v -> t.focus_enabled <- v)
    ~enable ~on_seq:focus_on ~off_seq:focus_off

let enable_kitty_keyboard ?(flags = 0b00001) t enable =
  set_kitty_keyboard t ~enable ~flags

let enable_modify_other_keys t enable =
  toggle_feature t
    ~current:(fun () -> t.modify_other_keys_enabled)
    ~set:(fun v -> t.modify_other_keys_enabled <- v)
    ~enable ~on_seq:modify_other_keys_on ~off_seq:modify_other_keys_off

let modify_other_keys_enabled t = t.modify_other_keys_enabled

let set_cursor_visible t visible =
  toggle_feature t
    ~current:(fun () -> t.cursor.visible)
    ~set:(fun v -> t.cursor.visible <- v)
    ~enable:visible ~on_seq:cursor_show ~off_seq:cursor_hide

let cursor_visible t = t.cursor.visible

let cursor_position t =
  { x = t.cursor.x; y = t.cursor.y; visible = t.cursor.visible }

let cursor_style_state t = (t.cursor.style, t.cursor.blinking)
let cursor_color t = t.cursor.color

let set_cursor_state t ~x ~y ~visible =
  t.cursor.x <- max 1 x;
  t.cursor.y <- max 1 y;
  t.cursor.visible <- visible

let move_cursor ?(visible = true) t ~row ~col =
  let row = max 1 row in
  let col = max 1 col in
  if visible <> t.cursor.visible then set_cursor_visible t visible;
  set_cursor_state t ~x:col ~y:row ~visible:t.cursor.visible;
  if t.output_is_tty then send t (Printf.sprintf "\027[%d;%dH" row col)

let clamp_color_string r g b =
  let r = clamp_color_component r in
  let g = clamp_color_component g in
  let b = clamp_color_component b in
  Printf.sprintf "\027]12;#%02X%02X%02X\007" r g b

let set_cursor_visuals t =
  if not t.output_is_tty then ()
  else if t.cursor.visible then (
    let r, g, b, _ = t.cursor.color in
    send t (clamp_color_string r g b);
    let seq =
      match (t.cursor.style, t.cursor.blinking) with
      | `Block, true -> kitty_cursor_block_blink
      | `Block, false -> kitty_cursor_block
      | `Line, true -> kitty_cursor_line_blink
      | `Line, false -> kitty_cursor_line
      | `Underline, true -> kitty_cursor_underline_blink
      | `Underline, false -> kitty_cursor_underline
    in
    send t seq)
  else send t cursor_hide

let set_cursor_style t style ~blinking =
  t.cursor.style <- style;
  t.cursor.blinking <- blinking;
  if t.output_is_tty then set_cursor_visuals t

let set_cursor_color t ~r ~g ~b ~a =
  t.cursor.color <- (r, g, b, a);
  if t.output_is_tty then set_cursor_visuals t

let reset_cursor_color t =
  t.cursor.color <- (1., 1., 1., 1.);
  if t.output_is_tty then (
    send t Ansi.reset_cursor_color_fallback;
    send t Ansi.reset_cursor_color)

let set_title t title = if t.output_is_tty then send t (make_osc ("0;" ^ title))
let flush t = match t.writer with None -> () | Some w -> Frame_writer.drain w

(* Shared buffer for draining the wakeup pipe, avoids allocation on every wake *)
let wakeup_drain_buffer = Bytes.create 64

let wait_readable t ~timeout =
  let timeout = if timeout < 0. then -1. else timeout in
  let rec loop () =
    try
      let read, _, _ = Unix.select [ t.input; t.wakeup_r ] [] [] timeout in
      let woke = List.mem t.wakeup_r read in
      let input_ready = List.mem t.input read in
      if woke then (
        ignore_exn
          (fun fd -> ignore (Unix.read fd wakeup_drain_buffer 0 64))
          t.wakeup_r;
        input_ready)
      else input_ready
    with
    | Unix_error (EINTR, _, _) -> loop ()
    | Unix_error _ -> false
  in
  loop ()

(* Non-blocking read: returns 0 on EAGAIN/EWOULDBLOCK instead of blocking.
   Use wait_readable first if you need to block until data is available. *)
let read_bytes_nonblocking t bytes off len =
  let rec loop () =
    match Unix.read t.input bytes off len with
    | n -> n
    | exception Unix_error (EINTR, _, _) -> loop ()
    | exception Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> 0
    | exception Unix_error (err, _, _) -> raise (Error (Unix.error_message err))
  in
  loop ()

let wake_signal = Bytes.of_string "x"

let wake t =
  try ignore (Unix.write t.wakeup_w wake_signal 0 1) with
  | Unix_error (EAGAIN, _, _) -> ()
  | Unix_error (EPIPE, _, _) -> ()
  | _ -> ()

let size t = try get_size t.output with _ -> (80, 24)

let reset_state t =
  if t.output_is_tty then (
    send t cursor_show;
    send t reset_sgr;
    send t Ansi.reset_cursor_color_fallback;
    send t Ansi.reset_cursor_color;
    send t Ansi.default_cursor_style;
    if t.kitty_keyboard_enabled then enable_kitty_keyboard t false;
    if t.modify_other_keys_enabled then (
      send t modify_other_keys_off;
      t.modify_other_keys_enabled <- false);
    if t.mouse_mode <> `Off then set_mouse_mode t `Off;
    if t.bracketed_paste_enabled then enable_bracketed_paste t false;
    if t.focus_enabled then enable_focus_reporting t false;
    if t.scroll_region <> None then (
      send t Ansi.reset_scrolling_region;
      t.scroll_region <- None);
    if t.alt_screen then (
      send t alternate_off;
      t.alt_screen <- false)
    else if Sys.win32 then (
      send t "\r";
      let rows = max 0 (t.cursor.y - 1) in
      for _ = 1 to rows do
        send t (Ansi.cursor_up ~n:1)
      done;
      send t erase_below);
    set_title t "";
    (* Ghostty/Alacritty occasionally drop the final cursor restore; send it
       twice with a small delay and drain the fd to increase reliability. *)
    send t cursor_show;
    ignore_exn Unix.tcdrain t.output;
    (try Unix.sleepf 0.01 with _ -> ());
    send t cursor_show;
    (try Unix.sleepf 0.01 with _ -> ());
    t.cursor.visible <- true)

let detect_term () = Sys.getenv_opt "TERM" |> Option.value ~default:"unknown"
let capabilities t = t.caps
let terminal_info t = t.terminal_info
let terminal_name t = t.terminal_info.name
let terminal_version t = t.terminal_info.version
let mode t = t.current_mode
let take_from_pending t = Queue.take_opt t.pending_events

(* Single-event version for callback-based parsing *)
let apply_capability_event t (event : Input.Caps.event) =
  let caps, info =
    Caps.apply_event ~apply_env_overrides:t.env_overrides ~caps:t.caps
      ~info:t.terminal_info event
  in
  t.caps <- caps;
  t.terminal_info <- info;
  match event with
  | Input.Caps.Pixel_resolution (w, h) -> t.pixel_resolution <- Some (w, h)
  | _ -> ()

let read ?(timeout = -1.) t on_event =
  let got_event = ref false in
  let emit e =
    got_event := true;
    on_event e
  in

  (* 1. Drain queued events first *)
  let rec drain_pending () =
    match take_from_pending t with
    | Some evt ->
        emit evt;
        drain_pending ()
    | None -> ()
  in
  drain_pending ();

  (* 2. Drain parser timeouts *)
  let drain_parser () =
    let now = Unix.gettimeofday () in
    Input.Parser.drain t.parser ~now ~on_event:emit
      ~on_caps:(apply_capability_event t)
  in
  drain_parser ();

  (* 3. Read available bytes (non-blocking drain) *)
  let read_available () =
    let rec loop () =
      if wait_readable t ~timeout:0. then
        let n =
          read_bytes_nonblocking t t.input_buffer 0
            (Bytes.length t.input_buffer)
        in
        if n > 0 then (
          let now = Unix.gettimeofday () in
          Input.Parser.feed t.parser t.input_buffer 0 n ~now ~on_event:emit
            ~on_caps:(apply_capability_event t);
          loop ())
    in
    loop ()
  in
  read_available ();
  drain_parser ();

  (* 4. If non-blocking (timeout=0) or we got events, we're done *)
  if timeout = 0. || !got_event then !got_event
  else
    (* 5. Blocking: wait for input with timeout *)
    let deadline =
      if timeout < 0. then None else Some (Unix.gettimeofday () +. timeout)
    in
    let rec wait_loop () =
      let now = Unix.gettimeofday () in
      (* Check deadline *)
      match deadline with
      | Some d when now >= d -> !got_event
      | _ ->
          (* Calculate wait time considering parser drain deadline *)
          let flush_deadline = Input.Parser.deadline t.parser in
          let user_wait =
            match deadline with Some d -> max 0. (d -. now) | None -> -1.
          in
          let flush_wait =
            match flush_deadline with
            | Some d -> max 0. (d -. now)
            | None -> -1.
          in
          let wait_time =
            match (user_wait < 0., flush_wait < 0.) with
            | true, true -> -1.
            | false, true -> user_wait
            | true, false -> flush_wait
            | false, false -> min user_wait flush_wait
          in
          let ready = wait_readable t ~timeout:wait_time in
          if ready then (
            read_available ();
            drain_parser ());
          if !got_event then true else wait_loop ()
    in
    wait_loop ()

let poll t on_event = read ~timeout:0. t on_event

let switch_mode t mode =
  (if not t.input_is_tty then ()
   else
     match mode with
     | `Raw ->
         let termios = Unix.tcgetattr t.input in
         let raw =
           {
             termios with
             c_echo = false;
             c_icanon = false;
             c_isig = false;
             c_vmin = 1;
             c_vtime = 0;
             c_ixon = false;
             c_icrnl = false;
           }
         in
         Unix.tcsetattr t.input TCSANOW raw;
         ignore_unix_errors Unix.set_nonblock t.input
     | `Cooked -> (
         match t.original_termios with
         | Some saved ->
             Unix.tcsetattr t.input TCSANOW saved;
             ignore_unix_errors Unix.clear_nonblock t.input
         | None -> ())
     | `Custom f ->
         let termios = Unix.tcgetattr t.input in
         let custom = f termios in
         Unix.tcsetattr t.input TCSANOW custom);
  t.current_mode <- mode

let with_mode t mode f =
  let previous = t.current_mode in
  switch_mode t mode;
  Fun.protect f ~finally:(fun () -> switch_mode t previous)

let query_capabilities ?(timeout = 0.2) t =
  if not (t.input_is_tty && t.output_is_tty) then ()
  else
    with_mode t `Raw (fun () ->
        let was_visible = t.cursor.visible in
        if was_visible then send t cursor_hide;
        let caps, info =
          Caps.probe ~timeout ~apply_env_overrides:t.env_overrides
            ~on_event:(fun e -> Queue.add e t.pending_events)
            ~read_into:(fun buf off len -> read_bytes_nonblocking t buf off len)
            ~wait_readable:(fun ~timeout -> wait_readable t ~timeout)
            ~send:(fun payload -> send t payload)
            ~caps:t.caps ~info:t.terminal_info ()
        in
        t.caps <- caps;
        t.terminal_info <- info;
        if was_visible then send t cursor_show)

let enter_alternate_screen t =
  if t.output_is_tty && not t.alt_screen then (
    send t alternate_on;
    send t cursor_hide;
    t.cursor.visible <- false;
    t.alt_screen <- true)

let leave_alternate_screen t =
  if t.output_is_tty && t.alt_screen then (
    send t alternate_off;
    t.alt_screen <- false)

let set_scroll_region t ~top ~bottom =
  if not t.output_is_tty then t.scroll_region <- Some (top, bottom)
  else if t.scroll_region = Some (top, bottom) then ()
  else (
    send t (Ansi.set_scrolling_region ~top ~bottom);
    t.scroll_region <- Some (top, bottom))

let clear_scroll_region t =
  if not t.output_is_tty then t.scroll_region <- None
  else if t.scroll_region = None then ()
  else (
    send t Ansi.reset_scrolling_region;
    t.scroll_region <- None)

let scroll_region t = t.scroll_region

let register_winch_handler, deregister_winch_handler =
  let handlers : (unit -> unit) list ref = ref [] in
  (* Signal handlers must never let exceptions escape, as this can crash
     the program or leave it in an inconsistent state. *)
  let safe_call f = try f () with _ -> () in
  let callback _ = List.iter safe_call !handlers in
  (* SIGWINCH is not available on Windows - silently ignore if unavailable *)
  let () =
    try Sys.set_signal Sys.sigwinch (Sys.Signal_handle callback)
    with Invalid_argument _ -> ()
  in
  ( (fun fn -> handlers := fn :: !handlers),
    fun fn -> handlers := List.filter (fun f -> f != fn) !handlers )

(* Platform detection for render_thread default.
   Threaded rendering is disabled on Linux due to instability. *)
let is_linux () =
  Sys.os_type = "Unix"
  &&
    try
      let ic = Unix.open_process_in "uname -s" in
      let result =
        Fun.protect
          ~finally:(fun () ->
            try ignore (Unix.close_process_in ic) with _ -> ())
          (fun () -> try Some (input_line ic) with End_of_file -> None)
      in
      match result with Some "Linux" -> true | _ -> false
    with _ -> false

let default_render_buffer_size = 1024 * 1024 * 2 (* 2MB *)

let open_terminal ?(probe = true) ?(probe_timeout = 0.2) ?(input = Unix.stdin)
    ?(output = Unix.stdout) ?initial_caps ?render_thread ?render_buffer_size ()
    =
  let use_thread =
    match render_thread with Some v -> v | None -> not (is_linux ())
  in
  let buffer_size =
    match render_buffer_size with
    | Some v -> v
    | None -> default_render_buffer_size
  in
  let input_is_tty = is_tty input in
  let output_is_tty = is_tty output in
  let parser = Input.Parser.create () in
  let pending_events = Queue.create () in
  let input_buffer = Bytes.create 4096 in
  let wake_r, wake_w = Unix.pipe () in
  ignore_exn Unix.set_nonblock wake_r;
  ignore_exn Unix.set_nonblock wake_w;
  ignore_exn Unix.set_close_on_exec wake_r;
  ignore_exn Unix.set_close_on_exec wake_w;
  let original_termios =
    if input_is_tty then
      try Some (Unix.tcgetattr input) with Unix_error _ -> None
    else None
  in
  if output_is_tty then ignore_exn enable_vt output;
  let term = detect_term () in
  let caps, terminal_info = Caps.initial ?provided:initial_caps ~term () in
  let env_overrides = Option.is_none initial_caps in
  let result =
    {
      input;
      output;
      wakeup_r = wake_r;
      wakeup_w = wake_w;
      original_termios;
      current_mode = `Cooked;
      input_is_tty;
      output_is_tty;
      caps;
      terminal_info;
      parser;
      pending_events;
      input_buffer;
      mouse_mode = `Off;
      cursor =
        {
          x = 1;
          y = 1;
          visible = true;
          style = `Block;
          blinking = false;
          color = (1., 1., 1., 1.);
        };
      alt_screen = false;
      bracketed_paste_enabled = false;
      focus_enabled = false;
      kitty_keyboard_enabled = false;
      kitty_keyboard_flags = 0b00001;
      modify_other_keys_enabled = false;
      unicode_mode_enabled = false;
      winch_handler = None;
      pixel_resolution = None;
      scroll_region = None;
      env_overrides;
      writer =
        Some (Frame_writer.create ~fd:output ~size:buffer_size ~use_thread);
    }
  in
  let cb =
   fun () ->
    match size result with
    | w, h when w > 0 && h > 0 ->
        if Queue.length result.pending_events < 1024 then
          Queue.add (Input.Resize (w, h)) result.pending_events;
        ignore (Unix.write result.wakeup_w wake_signal 0 1)
    | _ -> ()
  in
  register_winch_handler cb;
  result.winch_handler <- Some cb;
  if probe then query_capabilities ~timeout:probe_timeout result;
  result

let close t =
  (* Reset state while the writer is still active for proper ordering. *)
  ignore_exn reset_state t;
  Option.iter (fun w -> ignore_exn Frame_writer.close w) t.writer;
  t.writer <- None;
  Option.iter (fun cb -> ignore_exn deregister_winch_handler cb) t.winch_handler;
  t.winch_handler <- None;
  ignore_exn Unix.close t.wakeup_r;
  ignore_exn Unix.close t.wakeup_w;
  match t.original_termios with
  | Some termios ->
      ignore_unix_errors (Unix.tcsetattr t.input TCSANOW) termios;
      ignore_unix_errors Unix.clear_nonblock t.input
  | None -> ()

let flush_input t =
  if t.input_is_tty then ignore_unix_errors (Unix.tcflush t.input) Unix.TCIFLUSH

let query_cursor_position ?(timeout = 0.05) t =
  if not t.output_is_tty then None
  else
    (* Must use raw mode to properly read the CPR response.
       In cooked mode, echo is enabled and the response may be printed. *)
    with_mode t `Raw (fun () ->
        send t cursor_position_request;
        let deadline = Unix.gettimeofday () +. max 0. timeout in
        let rec loop () =
          let now = Unix.gettimeofday () in
          if now >= deadline then None
          else
            let remaining = deadline -. now in
            let ready = wait_readable t ~timeout:remaining in
            if not ready then None
            else
              let read =
                read_bytes_nonblocking t t.input_buffer 0
                  (Bytes.length t.input_buffer)
              in
              if read <= 0 then None
              else
                let cursor_pos = ref None in
                Input.Parser.feed t.parser t.input_buffer 0 read ~now
                  ~on_event:(fun e -> Queue.add e t.pending_events)
                  ~on_caps:(fun c ->
                    apply_capability_event t c;
                    match c with
                    | Input.Caps.Cursor_position (row, col) ->
                        cursor_pos := Some (row, col)
                    | _ -> ());
                match !cursor_pos with Some _ as r -> r | None -> loop ()
        in
        loop ())

let output_fd t = t.output

let query_pixel_resolution t =
  if t.output_is_tty then send t Esc.(to_string request_pixel_size)

let pixel_resolution t = t.pixel_resolution

(* Frame writing API *)

let render_buffer t =
  match t.writer with
  | None -> failwith "Terminal.render_buffer: writer not initialized"
  | Some w -> Frame_writer.render_buffer w

let drain t = match t.writer with None -> () | Some w -> Frame_writer.drain w

let present t len =
  match t.writer with None -> () | Some w -> Frame_writer.present w len

let write t str = submit_string t str

let write_bytes t bytes =
  let len = Bytes.length bytes in
  if len > 0 then write t (Bytes.unsafe_to_string bytes)
