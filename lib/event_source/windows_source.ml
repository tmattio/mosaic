type raw_event =
  | Key_event of bool * int * int * int * int
    (* bKeyDown, wRepeatCount, wVirtualKeyCode, uChar.UnicodeChar, dwControlKeyState *)
  | Mouse_event of int * int * int * int * int
    (* dwMousePosition.X, .Y, dwButtonState, dwControlKeyState, dwEventFlags *)
  | Window_buffer_size_event of int * int (* dwSize.X, .Y *)
  | Focus_event of bool (* bSetFocus *)
[@@warning "-37"]
(* Suppress "constructor is never used to build values" - they're built by C stubs *)

type mouse_state = { mutable buttons : int }

type paste_detector = {
  mutable buffer : (Input.key_event * float) list;
  mutable last_time : float;
  threshold : float;
  min_chars : int;
}

let left_ctrl_pressed = 0x0008
let right_ctrl_pressed = 0x0004
let left_alt_pressed = 0x0002
let right_alt_pressed = 0x0001
let shift_pressed = 0x0010
let from_left_1st_button_pressed = 0x0001 (* Left *)
let rightmost_button_pressed = 0x0002 (* Right *)
let from_left_2nd_button_pressed = 0x0004 (* Middle *)
let from_left_3rd_button_pressed = 0x0008 (* X1 *)
let from_left_4th_button_pressed = 0x0010 (* X2 *)
let mouse_moved = 0x0001

let _double_click =
  0x0002 (* Currently unused but part of Windows API constants *)

let mouse_wheeled = 0x0004
let mouse_hwheeled = 0x0008

external raw_events : int -> int -> raw_event list option
  = "event_read_console_input"

external get_console_mode : int -> int = "event_get_console_mode"
external set_console_mode : int -> int -> unit = "event_set_console_mode"
external enable_console_mode : int -> bool -> unit = "event_enable_console_mode"

external get_number_of_console_input_events : int -> int
  = "event_get_number_of_console_input_events"

external get_console_size : int -> int * int = "event_get_console_size"

let convert_key (down, _repeat, vk, uchar, ctrl_state) =
  if not down then None
  else
    let ctrl =
      ctrl_state land (left_ctrl_pressed lor right_ctrl_pressed) <> 0
    in
    let alt = ctrl_state land (left_alt_pressed lor right_alt_pressed) <> 0 in
    let shift = ctrl_state land shift_pressed <> 0 in
    let modifier =
      {
        Input.ctrl;
        alt;
        shift;
        super = false;
        hyper = false;
        meta = false;
        caps_lock = false;
        num_lock = false;
      }
    in
    let key_opt =
      match vk with
      | 13 -> Some Input.Enter
      | 9 -> Some Input.Tab
      | 8 -> Some Input.Backspace
      | 46 -> Some Input.Delete
      | 27 -> Some Input.Escape
      | 38 -> Some Input.Up
      | 40 -> Some Input.Down
      | 37 -> Some Input.Left
      | 39 -> Some Input.Right
      | 36 -> Some Input.Home
      | 35 -> Some Input.End
      | 33 -> Some Input.Page_up
      | 34 -> Some Input.Page_down
      | 45 -> Some Input.Insert
      | i when i >= 112 && i <= 123 -> Some (Input.F (i - 111))
      | _ -> if uchar <> 0 then Some (Input.Char (Uchar.of_int uchar)) else None
    in
    Option.map
      (fun key ->
        {
          Input.key;
          modifier;
          event_type = Input.Press;
          associated_text = "";
          shifted_key = None;
          base_key = None;
        })
      key_opt

let flush_paste detector =
  if detector.buffer = [] then []
  else
    let buf = List.rev detector.buffer in
    (* oldest first *)
    detector.buffer <- [];
    let groups = ref [] in
    let current = ref [] in
    let prev_t = ref (snd (List.hd buf)) in
    List.iter
      (fun (ke, t) ->
        let delta = t -. !prev_t in
        let is_paste_char =
          match ke.Input.key with Input.Char _ -> true | _ -> false
        in
        let not_modified =
          (not ke.Input.modifier.ctrl) && not ke.Input.modifier.alt
        in
        if delta <= detector.threshold && is_paste_char && not_modified then (
          current := ke :: !current;
          prev_t := t)
        else (
          (if List.length !current >= detector.min_chars then (
             let b = Buffer.create 16 in
             !current |> List.rev
             |> List.iter (fun ke ->
                    match ke.Input.key with
                    | Input.Char u -> Buffer.add_utf_8_uchar b u
                    | _ -> ());
             groups := Input.Paste (Buffer.contents b) :: !groups)
           else
             let keys =
               !current |> List.rev |> List.map (fun ke -> Input.Key ke)
             in
             groups := keys @ !groups);
          current := [];
          prev_t := t;
          if is_paste_char && not_modified then current := [ ke ]
          else groups := Input.Key ke :: !groups))
      buf;
    (if List.length !current >= detector.min_chars then (
       let b = Buffer.create 16 in
       !current |> List.rev
       |> List.iter (fun ke ->
              match ke.Input.key with
              | Input.Char u -> Buffer.add_utf_8_uchar b u
              | _ -> ());
       groups := Input.Paste (Buffer.contents b) :: !groups)
     else
       let keys = !current |> List.rev |> List.map (fun ke -> Input.Key ke) in
       groups := keys @ !groups);
    List.rev !groups

let process_mouse state (x, y, new_buttons, ctrl_state, event_flags) =
  let ctrl = ctrl_state land (left_ctrl_pressed lor right_ctrl_pressed) <> 0 in
  let alt = ctrl_state land (left_alt_pressed lor right_alt_pressed) <> 0 in
  let shift = ctrl_state land shift_pressed <> 0 in
  let mods =
    {
      Input.ctrl;
      alt;
      shift;
      super = false;
      hyper = false;
      meta = false;
      caps_lock = false;
      num_lock = false;
    }
  in
  let events = ref [] in

  (* Handle mouse motion *)
  (if event_flags land mouse_moved <> 0 then
     let button_state =
       {
         Input.left = state.buttons land from_left_1st_button_pressed <> 0;
         middle = state.buttons land from_left_2nd_button_pressed <> 0;
         right = state.buttons land rightmost_button_pressed <> 0;
       }
     in
     events := Input.Mouse (Input.Motion (x, y, button_state, mods)) :: !events);

  (* Handle mouse wheel *)
  if event_flags land (mouse_wheeled lor mouse_hwheeled) <> 0 then (
    let mut_delta = ref (new_buttons lsr 16) in
    if !mut_delta land 0x8000 <> 0 then mut_delta := !mut_delta - 0x10000;
    let delta = !mut_delta / 120 in
    let horizontal = event_flags land mouse_hwheeled <> 0 in
    let wheel_button =
      if horizontal then
        if delta > 0 then Input.Wheel_right else Input.Wheel_left
      else if delta > 0 then Input.Wheel_up
      else Input.Wheel_down
    in
    for _ = 1 to abs delta do
      events :=
        Input.Mouse (Input.Button_press (x, y, wheel_button, mods)) :: !events
    done);

  (* Handle double click *)
  (* Ignore double click for simplicity *)

  (* Handle button press/release when no other events *)
  if event_flags = 0 then (
    let changed = new_buttons lxor state.buttons in
    let button_masks =
      [
        (0, from_left_1st_button_pressed, Input.Left);
        (1, rightmost_button_pressed, Input.Right);
        (2, from_left_2nd_button_pressed, Input.Middle);
        (3, from_left_3rd_button_pressed, Input.Button 8);
        (4, from_left_4th_button_pressed, Input.Button 9);
      ]
    in
    List.iter
      (fun (_, mask, btn) ->
        if changed land mask <> 0 then
          if new_buttons land mask <> 0 then
            events :=
              Input.Mouse (Input.Button_press (x, y, btn, mods)) :: !events
          else
            events :=
              Input.Mouse (Input.Button_release (x, y, btn, mods)) :: !events)
      button_masks;
    state.buttons <- new_buttons);
  List.rev !events

let create ~sw ~env ~mouse ~paste_threshold ~paste_min_chars terminal =
  let clock = Eio.Stdenv.clock env in
  let handle = Obj.magic (Terminal.input_fd terminal) in
  let original_mode = get_console_mode handle in
  enable_console_mode handle mouse;
  let events = Eio.Stream.create 1024 in
  let detector =
    {
      buffer = [];
      last_time = 0.0;
      threshold = paste_threshold;
      min_chars = paste_min_chars;
    }
  in
  let mouse_state = { buttons = 0 } in
  (* Initial resize *)
  (try
     let w, h = get_console_size handle in
     Eio.Stream.add events (Input.Resize (w, h))
   with _ -> ());

  (* Polling fiber with cooperative sleep *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
      try
        while true do
          let num = get_number_of_console_input_events handle in
          if num > 0 then
            match raw_events handle (min num 128) with
            | Some raw_list ->
                List.iter
                  (fun raw ->
                    let evs =
                      match raw with
                      | Key_event (down, repeat, vk, uchar, ctrl_state) -> (
                          match
                            convert_key (down, repeat, vk, uchar, ctrl_state)
                          with
                          | None -> []
                          | Some ke -> (
                              let now = Unix.gettimeofday () in
                              match ke.Input.key with
                              | Char _
                                when (not ke.Input.modifier.ctrl)
                                     && not ke.Input.modifier.alt ->
                                  detector.buffer <-
                                    (ke, now) :: detector.buffer;
                                  detector.last_time <- now;
                                  [] (* Don't emit yet *)
                              | _ ->
                                  let flushed = flush_paste detector in
                                  flushed @ [ Input.Key ke ]))
                      | Mouse_event (x, y, button_state, ctrl_state, event_flags)
                        ->
                          process_mouse mouse_state
                            (x, y, button_state, ctrl_state, event_flags)
                      | Window_buffer_size_event (w, h) ->
                          [ Input.Resize (w, h) ]
                      | Focus_event b ->
                          [ (if b then Input.Focus else Input.Blur) ]
                    in
                    List.iter (Eio.Stream.add events) evs)
                  raw_list
            | None -> ()
          else
            let now = Unix.gettimeofday () in
            (if
               detector.buffer <> []
               && now -. detector.last_time > detector.threshold
             then
               let flushed = flush_paste detector in
               List.iter (Eio.Stream.add events) flushed);
            Eio.Time.sleep clock 0.001 (* Short cooperative sleep *)
        done
      with Eio.Cancel.Cancelled _ -> `Stop_daemon (* Exit on cancel *));

  Eio.Switch.on_release sw (fun () -> set_console_mode handle original_mode);
  events
