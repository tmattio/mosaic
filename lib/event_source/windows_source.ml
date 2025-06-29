type paste_detector = {
  mutable buffer : Input.event list;
  mutable last_event_time : float;
}

type t = {
  handle : int; (* Console handle *)
  enable_mouse : bool; (* Whether to enable mouse input *)
  mutable initialized : bool;
  paste_detector : paste_detector;
}

external read_console_input : int -> int -> Input.event option
  = "event_read_console_input"

external enable_console_mode : int -> bool -> unit = "event_enable_console_mode"
(* external get_console_cursor_info : int -> int * bool = "event_get_console_cursor_info" *)
(* external set_console_cursor_info : int -> int -> bool -> unit = "event_set_console_cursor_info" *)

let create ~mouse terminal =
  let input_fd = Terminal.input_fd terminal in
  (* On Windows, OCaml's Unix.file_descr type is actually just an integer handle
     that can be passed directly to Win32 API functions. We use Obj.magic here
     to convert from the abstract file_descr type to int. This is safe because
     the OCaml runtime represents Windows file descriptors as plain integers. *)
  let handle = Obj.magic input_fd in
  let paste_detector = { buffer = []; last_event_time = 0.0 } in
  { handle; enable_mouse = mouse; initialized = false; paste_detector }

let initialize t =
  if not t.initialized then (
    (* Enable console input mode *)
    enable_console_mode t.handle t.enable_mouse;
    t.initialized <- true)

(* Heuristic for paste detection: if multiple character key events arrive
   within a very short time window, consider them pasted content.
   
   Unlike Unix terminals which send special escape sequences for bracketed paste,
   Windows console applications must infer paste operations from the timing of
   key events. When text is pasted, all characters arrive nearly simultaneously,
   much faster than human typing speed. *)
let paste_threshold = 0.01 (* 10ms between key events *)
let paste_min_chars = 3 (* Minimum characters to consider as paste *)

(* [flush_paste_buffer detector] processes the accumulated buffer of events.
   If the buffer contains enough plain text characters (no modifiers),
   it returns a single [Paste] event. Otherwise, it returns the original
   sequence of key events. This ensures we don't incorrectly convert
   legitimate fast typing or keyboard shortcuts into paste events. *)
let flush_paste_buffer detector =
  let events = List.rev detector.buffer in
  detector.buffer <- [];

  (* Extract only plain text characters from the buffered events.
     We ignore events with Ctrl/Alt modifiers as these are likely
     keyboard shortcuts rather than pasted content. *)
  let text =
    events
    |> List.filter_map (function
         | Input.Key { key = Input.Char c; modifier }
           when (not modifier.ctrl) && not modifier.alt ->
             (* Convert Uchar to string *)
             let buf = Buffer.create 4 in
             Buffer.add_utf_8_uchar buf c;
             Some (Buffer.contents buf)
         | _ -> None)
    |> String.concat ""
  in

  (* Only treat as paste if we have enough characters. This prevents
     single fast keypresses or very short sequences from being
     misidentified as pastes. *)
  if String.length text >= paste_min_chars then [ Input.Paste text ] else events

(* [handle_paste_detection detector event] is the core of the paste heuristic.
   It buffers character key events that arrive in quick succession. If a
   non-character event arrives, or if the time since the last event exceeds
   the threshold, the buffer is flushed, either as a single [Paste] event
   or as the original sequence of key events.
   
   Returns a list of events to be dispatched immediately. *)
let handle_paste_detection detector event =
  let now = Unix.gettimeofday () in

  match event with
  | Input.Key { key = Input.Char _; _ } as key_event ->
      let time_diff = now -. detector.last_event_time in
      detector.last_event_time <- now;

      if detector.buffer = [] || time_diff <= paste_threshold then (
        (* This character arrived quickly after the previous one (or is the
           first character). Buffer it as part of a potential paste operation.
           We don't emit anything yet - we need to see if more characters
           follow quickly. *)
        detector.buffer <- key_event :: detector.buffer;
        [] (* Don't emit yet *))
      else
        (* Too much time has passed since the last character. This is likely
           a new sequence of typing, not a continuation of a paste. Flush
           the old buffer and start tracking a new potential paste. *)
        let flushed = flush_paste_buffer detector in
        detector.buffer <- [ key_event ];
        flushed
  | _ ->
      (* Non-character event (like arrow key, mouse, etc). Any pending paste
         must be complete, so flush the buffer and emit this event. *)
      detector.last_event_time <- now;
      let flushed = flush_paste_buffer detector in
      flushed @ [ event ]

let rec read t ~timeout =
  initialize t;
  let timeout_ms =
    match timeout with
    | None -> -1 (* Block indefinitely *)
    | Some t when t <= 0.0 -> 0 (* Poll without blocking *)
    | Some t -> int_of_float (t *. 1000.0)
  in

  (* Check if we have characters buffered from a potential paste operation.
     If so, we need special handling to ensure we don't block for too long
     while waiting for more paste characters. *)
  if t.paste_detector.buffer <> [] then
    let now = Unix.gettimeofday () in
    let time_since_last = now -. t.paste_detector.last_event_time in
    if time_since_last > paste_threshold then
      (* Inactivity timeout exceeded. The paste operation (if any) is complete.
         Flush the buffer and return the first event immediately. *)
      match flush_paste_buffer t.paste_detector with
      | event :: _ -> `Event event
      | [] -> (
          (* Buffer was empty after flush (shouldn't happen).
             Continue with a normal read. *)
          match read_console_input t.handle timeout_ms with
          | None -> `Timeout
          | Some event -> (
              match handle_paste_detection t.paste_detector event with
              | event :: _ -> `Event event
              | [] -> read t ~timeout:(Some 0.0))) (* Try again immediately *)
    else
      (* Still within the paste time window. We wait for a short period
         (paste_threshold) for more characters to arrive, rather than blocking
         for the full user-specified timeout. This ensures paste detection
         remains responsive. *)
      let adjusted_timeout = int_of_float (paste_threshold *. 1000.0) in
      match read_console_input t.handle adjusted_timeout with
      | None -> (
          (* Short timeout expired. Assume the paste is complete and flush
             the buffer to emit the paste event. *)
          match flush_paste_buffer t.paste_detector with
          | event :: _ -> `Event event
          | [] -> `Timeout)
      | Some event -> (
          (* Another event arrived within the paste window. Process it through
             the paste detector, which will either add it to the buffer or
             flush if it's not a character. *)
          match handle_paste_detection t.paste_detector event with
          | event :: _ -> `Event event
          | [] -> read t ~timeout:(Some 0.0)) (* Try again immediately *)
  else
    (* No pending paste buffer. Perform a normal read with the user's
       requested timeout. Any character events will be routed through
       paste detection. *)
    match read_console_input t.handle timeout_ms with
    | None -> `Timeout
    | Some event -> (
        match handle_paste_detection t.paste_detector event with
        | event :: _ -> `Event event
        | [] ->
            (* Event was buffered for paste detection. Recursively read
               with zero timeout to check for more events immediately. *)
            read t ~timeout:(Some 0.0))
