type key =
  | Char of Uchar.t
  | Enter
  | Tab
  | Backspace
  | Delete
  | Escape
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Page_up
  | Page_down
  | Insert
  | F of int
  | Print_screen
  | Pause
  | Menu
  | Scroll_lock
  | Media_play
  | Media_pause
  | Media_play_pause
  | Media_stop
  | Media_reverse
  | Media_fast_forward
  | Media_rewind
  | Media_next
  | Media_prev
  | Media_record
  | Volume_up
  | Volume_down
  | Volume_mute
  | Shift_left
  | Shift_right
  | Ctrl_left
  | Ctrl_right
  | Alt_left
  | Alt_right
  | Super_left
  | Super_right
  | Hyper_left
  | Hyper_right
  | Meta_left
  | Meta_right
  | Iso_level3_shift
  | Iso_level5_shift
  | Caps_lock
  | Num_lock
  | KP_0
  | KP_1
  | KP_2
  | KP_3
  | KP_4
  | KP_5
  | KP_6
  | KP_7
  | KP_8
  | KP_9
  | KP_decimal
  | KP_divide
  | KP_multiply
  | KP_subtract
  | KP_add
  | KP_enter
  | KP_equal
  | KP_separator
  | KP_begin
  | KP_left
  | KP_right
  | KP_up
  | KP_down
  | KP_page_up
  | KP_page_down
  | KP_home
  | KP_end
  | KP_insert
  | KP_delete
  | Unknown of int

type event_type = Press | Repeat | Release

type modifier = {
  ctrl : bool;
  alt : bool;
  shift : bool;
  super : bool;
  hyper : bool;
  meta : bool;
  caps_lock : bool;
  num_lock : bool;
}

let no_modifier =
  {
    ctrl = false;
    alt = false;
    shift = false;
    super = false;
    hyper = false;
    meta = false;
    caps_lock = false;
    num_lock = false;
  }

type key_event = {
  key : key;
  modifier : modifier;
  event_type : event_type;
  associated_text : string;
  shifted_key : Uchar.t option;
  base_key : Uchar.t option;
}

let key ?(modifier = no_modifier) ?(event_type = Press) ?(associated_text = "")
    ?(shifted_key = None) ?(base_key = None) k =
  { key = k; modifier; event_type; associated_text; shifted_key; base_key }

let char ?(modifier = no_modifier) ?(event_type = Press) ?(associated_text = "")
    ?(shifted_key = None) ?(base_key = None) c =
  {
    key = Char (Uchar.of_char c);
    modifier;
    event_type;
    associated_text;
    shifted_key;
    base_key;
  }

type mouse_button =
  | Left
  | Middle
  | Right
  | Wheel_up
  | Wheel_down
  | Wheel_left
  | Wheel_right
  | Button of int

type mouse_button_state = { left : bool; middle : bool; right : bool }

type mouse_event =
  | Button_press of int * int * mouse_button * modifier
  | Button_release of int * int * mouse_button * modifier
  | Motion of int * int * mouse_button_state * modifier

type event =
  | Key of key_event
  | Mouse of mouse_event
  | Resize of int * int
  | Focus
  | Blur
  | Paste_start
  | Paste_end
  | Paste of string
  | Clipboard of string * string
  | Osc of int * string
  | Cursor_position of int * int
  | Device_attributes of int list

type parser = {
  buffer : bytes;
  mutable length : int;
  mutable in_paste : bool;
  paste_buffer : Buffer.t;
  utf8_decoder : Uutf.decoder;
}

let create () =
  {
    buffer = Bytes.create 4096;
    length = 0;
    in_paste = false;
    paste_buffer = Buffer.create 256;
    utf8_decoder = Uutf.decoder ~encoding:`UTF_8 `Manual;
  }

let is_ascii_digit c = c >= '0' && c <= '9'
let is_csi_param c = c >= '\x30' && c <= '\x3f'
let is_csi_intermediate c = c >= '\x20' && c <= '\x2f'
let is_csi_final c = c >= '\x40' && c <= '\x7e'

let parse_int s start end_ =
  let rec loop acc i =
    if i >= end_ then Some acc
    else if is_ascii_digit s.[i] then
      loop ((acc * 10) + (Char.code s.[i] - 48)) (i + 1)
    else None
  in
  loop 0 start

let parse_csi_params_string s start end_ =
  let rec loop acc start =
    if start >= end_ then List.rev acc
    else
      let rec find_end i =
        if i >= end_ || s.[i] = ';' then i else find_end (i + 1)
      in
      let param_end = find_end start in
      let param =
        if param_end > start then String.sub s start (param_end - start) else ""
      in
      let acc = param :: acc in
      if param_end < end_ && s.[param_end] = ';' then loop acc (param_end + 1)
      else List.rev acc
  in
  loop [] start

let parse_csi_params s start end_ =
  let params_str = parse_csi_params_string s start end_ in
  List.map
    (fun p -> if p = "" then None else parse_int p 0 (String.length p))
    params_str

let parse_sub_params param_str =
  let fields = String.split_on_char ':' param_str in
  List.map (fun f -> parse_int f 0 (String.length f)) fields

let parse_x10_normal_mouse bytes start =
  (* Helper to decode a UTF-8 character and return its code and byte length *)
  let decode_utf8_char pos =
    if pos >= Bytes.length bytes then None
    else
      let b0 = Char.code (Bytes.get bytes pos) in
      if b0 < 0x80 then
        (* Single byte UTF-8 *)
        Some (b0, 1)
      else if b0 < 0xC0 then
        (* Invalid start byte *)
        None
      else if b0 < 0xE0 && pos + 1 < Bytes.length bytes then
        (* 2-byte UTF-8 *)
        let b1 = Char.code (Bytes.get bytes (pos + 1)) in
        if b1 land 0xC0 = 0x80 then
          let code = ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F) in
          Some (code, 2)
        else None
      else if b0 < 0xF0 && pos + 2 < Bytes.length bytes then
        (* 3-byte UTF-8 *)
        let b1 = Char.code (Bytes.get bytes (pos + 1)) in
        let b2 = Char.code (Bytes.get bytes (pos + 2)) in
        if b1 land 0xC0 = 0x80 && b2 land 0xC0 = 0x80 then
          let code =
            ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
          in
          Some (code, 3)
        else None
      else if b0 < 0xF8 && pos + 3 < Bytes.length bytes then
        (* 4-byte UTF-8 *)
        let b1 = Char.code (Bytes.get bytes (pos + 1)) in
        let b2 = Char.code (Bytes.get bytes (pos + 2)) in
        let b3 = Char.code (Bytes.get bytes (pos + 3)) in
        if b1 land 0xC0 = 0x80 && b2 land 0xC0 = 0x80 && b3 land 0xC0 = 0x80
        then
          let code =
            ((b0 land 0x07) lsl 18)
            lor ((b1 land 0x3F) lsl 12)
            lor ((b2 land 0x3F) lsl 6)
            lor (b3 land 0x3F)
          in
          Some (code, 4)
        else None
      else None
  in
  if start + 3 <= Bytes.length bytes then
    match decode_utf8_char start with
    | None -> None
    | Some (btn, len1) -> (
        match decode_utf8_char (start + len1) with
        | None -> None
        | Some (ux, len2) -> (
            match decode_utf8_char (start + len1 + len2) with
            | None -> None
            | Some (uy, len3) ->
                let cb = btn - 32 in
                let x = ux - 33 in
                let y = uy - 33 in
                let consumed = len1 + len2 + len3 in
                if x < 0 || y < 0 then None
                else
                  let button_code = cb land 0b11111 in
                  let button =
                    match button_code with
                    | 0 -> Left
                    | 1 -> Middle
                    | 2 -> Right
                    | 3 -> Left (* release *)
                    | 64 -> Wheel_up
                    | 65 -> Wheel_down
                    | 66 -> Wheel_left
                    | 67 -> Wheel_right
                    | 128 -> Button 8
                    | 129 -> Button 9
                    | 130 -> Button 10
                    | 131 -> Button 11
                    | _ -> Button button_code
                  in
                  let modifier =
                    {
                      shift = cb land 4 <> 0;
                      alt = cb land 8 <> 0;
                      ctrl = cb land 16 <> 0;
                      super = false;
                      hyper = false;
                      meta = false;
                      caps_lock = false;
                      num_lock = false;
                    }
                  in
                  let is_motion = cb land 32 <> 0 in
                  let event =
                    if is_motion then
                      let state =
                        {
                          left = button_code = 0;
                          middle = button_code = 1;
                          right = button_code = 2;
                        }
                      in
                      Mouse (Motion (x, y, state, modifier))
                    else if button_code = 3 then
                      Mouse (Button_release (x, y, button, modifier))
                    else Mouse (Button_press (x, y, button, modifier))
                  in
                  Some (event, consumed)))
  else None

let parse_sgr_mouse s start end_ =
  let params_start = if s.[start] = '<' then start + 1 else start in
  let params = parse_csi_params s params_start (end_ - 1) in
  let final = s.[end_ - 1] in
  match params with
  | [ Some btn; Some x; Some y ] when final = 'M' || final = 'm' ->
      let button_code = btn land 0b11111 in
      let button =
        match button_code with
        | 0 -> Left
        | 1 -> Middle
        | 2 -> Right
        | 64 -> Wheel_up
        | 65 -> Wheel_down
        | 66 -> Wheel_left
        | 67 -> Wheel_right
        | 128 -> Button 8
        | 129 -> Button 9
        | 130 -> Button 10
        | 131 -> Button 11
        | _ -> Button button_code
      in
      let modifier =
        {
          shift = btn land 4 <> 0;
          alt = btn land 8 <> 0;
          ctrl = btn land 16 <> 0;
          super = false;
          hyper = false;
          meta = false;
          caps_lock = false;
          num_lock = false;
        }
      in
      let is_motion = btn land 32 <> 0 in
      let event =
        if is_motion then
          let state =
            {
              left = button_code = 0;
              middle = button_code = 1;
              right = button_code = 2;
            }
          in
          Motion (x - 1, y - 1, state, modifier)
        else if final = 'M' then Button_press (x - 1, y - 1, button, modifier)
        else Button_release (x - 1, y - 1, button, modifier)
      in
      Some (Mouse event)
  | _ -> None

let parse_urxvt_mouse s start end_ =
  match parse_csi_params s start (end_ - 1) with
  | [ Some btn; Some x; Some y ] ->
      let base_btn = btn - 32 in
      let button_code = base_btn land 0b11111 in
      let button =
        match button_code with
        | 0 -> Left
        | 1 -> Middle
        | 2 -> Right
        | 64 -> Wheel_up
        | 65 -> Wheel_down
        | 66 -> Wheel_left
        | 67 -> Wheel_right
        | 128 -> Button 8
        | 129 -> Button 9
        | 130 -> Button 10
        | 131 -> Button 11
        | _ -> Button button_code
      in
      let modifier =
        {
          shift = base_btn land 4 <> 0;
          alt = base_btn land 8 <> 0;
          ctrl = base_btn land 16 <> 0;
          super = false;
          hyper = false;
          meta = false;
          caps_lock = false;
          num_lock = false;
        }
      in
      let is_motion = base_btn land 32 <> 0 in
      let is_release = button_code = 3 in
      let event =
        if is_motion then
          let state =
            {
              left = button_code = 0;
              middle = button_code = 1;
              right = button_code = 2;
            }
          in
          Motion (x - 1, y - 1, state, modifier)
        else if is_release then Button_release (x - 1, y - 1, Left, modifier)
          (* fallback *)
        else Button_press (x - 1, y - 1, button, modifier)
      in
      Some (Mouse event)
  | _ -> None

let pua_to_key c =
  match c with
  (* Kitty's PUA codes start at 57358 (0xE00E) *)
  | 57358 -> Caps_lock (* 0xE00E *)
  | 57359 -> Scroll_lock (* 0xE00F *)
  | 57360 -> Num_lock (* 0xE010 *)
  | 57361 -> Print_screen (* 0xE011 *)
  | 57362 -> Pause (* 0xE012 *)
  | 57363 -> Menu (* 0xE013 *)
  (* Extended Function keys F13-F35 *)
  | 57376 -> F 13 (* 0xE020 *)
  | 57377 -> F 14 (* 0xE021 *)
  | 57378 -> F 15 (* 0xE022 *)
  | 57379 -> F 16 (* 0xE023 *)
  | 57380 -> F 17 (* 0xE024 *)
  | 57381 -> F 18 (* 0xE025 *)
  | 57382 -> F 19 (* 0xE026 *)
  | 57383 -> F 20 (* 0xE027 *)
  | 57384 -> F 21 (* 0xE028 *)
  | 57385 -> F 22 (* 0xE029 *)
  | 57386 -> F 23 (* 0xE02A *)
  | 57387 -> F 24 (* 0xE02B *)
  | 57388 -> F 25 (* 0xE02C *)
  | 57389 -> F 26 (* 0xE02D *)
  | 57390 -> F 27 (* 0xE02E *)
  | 57391 -> F 28 (* 0xE02F *)
  | 57392 -> F 29 (* 0xE030 *)
  | 57393 -> F 30 (* 0xE031 *)
  | 57394 -> F 31 (* 0xE032 *)
  | 57395 -> F 32 (* 0xE033 *)
  | 57396 -> F 33 (* 0xE034 *)
  | 57397 -> F 34 (* 0xE035 *)
  | 57398 -> F 35 (* 0xE036 *)
  (* Keypad keys *)
  | 57399 -> KP_0 (* 0xE037 *)
  | 57400 -> KP_1 (* 0xE038 *)
  | 57401 -> KP_2 (* 0xE039 *)
  | 57402 -> KP_3 (* 0xE03A *)
  | 57403 -> KP_4 (* 0xE03B *)
  | 57404 -> KP_5 (* 0xE03C *)
  | 57405 -> KP_6 (* 0xE03D *)
  | 57406 -> KP_7 (* 0xE03E *)
  | 57407 -> KP_8 (* 0xE03F *)
  | 57408 -> KP_9 (* 0xE040 *)
  | 57409 -> KP_decimal (* 0xE041 *)
  | 57410 -> KP_divide (* 0xE042 *)
  | 57411 -> KP_multiply (* 0xE043 *)
  | 57412 -> KP_subtract (* 0xE044 *)
  | 57413 -> KP_add (* 0xE045 *)
  | 57414 -> KP_enter (* 0xE046 *)
  | 57415 -> KP_equal (* 0xE047 *)
  | 57416 -> KP_separator (* 0xE048 *)
  | 57417 -> KP_left (* 0xE049 *)
  | 57418 -> KP_right (* 0xE04A *)
  | 57419 -> KP_up (* 0xE04B *)
  | 57420 -> KP_down (* 0xE04C *)
  | 57421 -> KP_page_up (* 0xE04D *)
  | 57422 -> KP_page_down (* 0xE04E *)
  | 57423 -> KP_home (* 0xE04F *)
  | 57424 -> KP_end (* 0xE050 *)
  | 57425 -> KP_insert (* 0xE051 *)
  | 57426 -> KP_delete (* 0xE052 *)
  | 57427 -> KP_begin (* 0xE053 *)
  (* Media keys *)
  | 57428 -> Media_play (* 0xE054 *)
  | 57429 -> Media_pause (* 0xE055 *)
  | 57430 -> Media_play_pause (* 0xE056 *)
  | 57431 -> Media_reverse (* 0xE057 *)
  | 57432 -> Media_stop (* 0xE058 *)
  | 57433 -> Media_fast_forward (* 0xE059 *)
  | 57434 -> Media_rewind (* 0xE05A *)
  | 57435 -> Media_next (* 0xE05B - MEDIA_TRACK_NEXT *)
  | 57436 -> Media_prev (* 0xE05C - MEDIA_TRACK_PREVIOUS *)
  | 57437 -> Media_record (* 0xE05D *)
  | 57438 -> Volume_down (* 0xE05E - LOWER_VOLUME *)
  | 57439 -> Volume_up (* 0xE05F - RAISE_VOLUME *)
  | 57440 -> Volume_mute (* 0xE060 - MUTE_VOLUME *)
  (* Modifier keys *)
  | 57441 -> Shift_left (* 0xE061 - LEFT_SHIFT *)
  | 57442 -> Ctrl_left (* 0xE062 - LEFT_CONTROL *)
  | 57443 -> Alt_left (* 0xE063 - LEFT_ALT *)
  | 57444 -> Super_left (* 0xE064 - LEFT_SUPER *)
  | 57445 -> Hyper_left (* 0xE065 - LEFT_HYPER *)
  | 57446 -> Meta_left (* 0xE066 - LEFT_META *)
  | 57447 -> Shift_right (* 0xE067 - RIGHT_SHIFT *)
  | 57448 -> Ctrl_right (* 0xE068 - RIGHT_CONTROL *)
  | 57449 -> Alt_right (* 0xE069 - RIGHT_ALT *)
  | 57450 -> Super_right (* 0xE06A - RIGHT_SUPER *)
  | 57451 -> Hyper_right (* 0xE06B - RIGHT_HYPER *)
  | 57452 -> Meta_right (* 0xE06C - RIGHT_META *)
  | 57453 -> Iso_level3_shift (* 0xE06D *)
  | 57454 -> Iso_level5_shift (* 0xE06E *)
  | _ -> Unknown c

let parse_kitty_keyboard s start end_ =
  let param_str = String.sub s start (end_ - start - 1) in
  let fields = String.split_on_char ';' param_str in
  if List.length fields = 0 then None
  else
    let code_field = String.split_on_char ':' (List.nth fields 0) in
    let code = try int_of_string (List.nth code_field 0) with _ -> -1 in
    let shifted =
      try Some (Uchar.of_int (int_of_string (List.nth code_field 1)))
      with _ -> None
    in
    let base =
      try Some (Uchar.of_int (int_of_string (List.nth code_field 2)))
      with _ -> None
    in
    let mods_field =
      try String.split_on_char ':' (List.nth fields 1) with _ -> [ "1" ]
    in
    let mods = try int_of_string (List.nth mods_field 0) with _ -> 1 in
    let event_val = try int_of_string (List.nth mods_field 1) with _ -> 1 in
    let text_field =
      try String.split_on_char ':' (List.nth fields 2) with _ -> []
    in
    let associated_text =
      let buf = Buffer.create (List.length text_field * 4) in
      List.iter
        (fun str ->
          try
            let cp = int_of_string str in
            Uutf.Buffer.add_utf_8 buf (Uchar.of_int cp)
          with _ -> ())
        text_field;
      Buffer.contents buf
    in
    let modifier_val = mods - 1 in
    let modifier =
      {
        shift = modifier_val land 1 <> 0;
        alt = modifier_val land 2 <> 0;
        ctrl = modifier_val land 4 <> 0;
        super = modifier_val land 8 <> 0;
        hyper = modifier_val land 16 <> 0;
        meta = modifier_val land 32 <> 0;
        caps_lock = modifier_val land 64 <> 0;
        num_lock = modifier_val land 128 <> 0;
      }
    in
    let event_type =
      match event_val with 2 -> Repeat | 3 -> Release | _ -> Press
    in
    let key =
      match code with
      | -1 -> None
      | 9 -> Some Tab
      | 13 -> Some Enter
      | 27 -> Some Escape
      | 127 -> Some Backspace
      | c when c >= 57344 && c <= 63743 -> Some (pua_to_key c)
      | c when c > 0 -> Some (Char (Uchar.of_int c))
      | _ -> None
    in
    match key with
    | None -> None
    | Some k ->
        Some
          (Key
             {
               key = k;
               modifier;
               event_type;
               associated_text;
               shifted_key = shifted;
               base_key = base;
             })

let key_of_char = function
  | '\r' | '\n' -> Enter
  | '\t' -> Tab
  | '\x7f' -> Backspace
  | ' ' -> Char (Uchar.of_int 32)
  | c -> Char (Uchar.of_int (Char.code c))

let parse_csi s start end_ =
  let params_end = ref start in
  while !params_end < end_ && is_csi_param s.[!params_end] do
    incr params_end
  done;

  let intermediate_end = ref !params_end in
  while !intermediate_end < end_ && is_csi_intermediate s.[!intermediate_end] do
    incr intermediate_end
  done;

  if !intermediate_end < end_ && is_csi_final s.[!intermediate_end] then
    let final_char = s.[!intermediate_end] in
    let params = parse_csi_params s start !params_end in

    (* Parse modifiers and event types from params for cursor keys *)
    let params_str = parse_csi_params_string s start !params_end in
    let parse_modifiers_and_event params_str =
      let parse_modifier_val m =
        let m = m - 1 in
        {
          shift = m land 1 <> 0;
          alt = m land 2 <> 0;
          ctrl = m land 4 <> 0;
          super = m land 8 <> 0;
          hyper = m land 16 <> 0;
          meta = m land 32 <> 0;
          caps_lock = m land 64 <> 0;
          num_lock = m land 128 <> 0;
        }
      in
      let parse_event_val = function
        | 2 -> Repeat
        | 3 -> Release
        | _ -> Press
      in
      match params_str with
      | [] -> (no_modifier, Press)
      | [ p ] -> (
          (* Single param, might have colon sub-params like "2:3" *)
          let sub = parse_sub_params p in
          match sub with
          | [ Some m ] -> (parse_modifier_val m, Press)
          | [ Some m; Some e ] -> (parse_modifier_val m, parse_event_val e)
          | _ -> (no_modifier, Press))
      | [ _; p ] -> (
          (* Two params like "1;2" or "1;2:3" *)
          let sub = parse_sub_params p in
          match sub with
          | [ Some m ] -> (parse_modifier_val m, Press)
          | [ Some m; Some e ] -> (parse_modifier_val m, parse_event_val e)
          | _ -> (no_modifier, Press))
      | _ -> (no_modifier, Press)
    in

    match final_char with
    (* Cursor movement - also handle keypad in numeric mode *)
    | 'A' ->
        let modifier, event_type = parse_modifiers_and_event params_str in
        Some
          (Key
             {
               key = Up;
               modifier;
               event_type;
               associated_text = "";
               shifted_key = None;
               base_key = None;
             })
    | 'B' ->
        let modifier, event_type = parse_modifiers_and_event params_str in
        Some
          (Key
             {
               key = Down;
               modifier;
               event_type;
               associated_text = "";
               shifted_key = None;
               base_key = None;
             })
    | 'C' ->
        let modifier, event_type = parse_modifiers_and_event params_str in
        Some
          (Key
             {
               key = Right;
               modifier;
               event_type;
               associated_text = "";
               shifted_key = None;
               base_key = None;
             })
    | 'D' ->
        let modifier, event_type = parse_modifiers_and_event params_str in
        Some
          (Key
             {
               key = Left;
               modifier;
               event_type;
               associated_text = "";
               shifted_key = None;
               base_key = None;
             })
    | 'E' ->
        (* Begin/Center key (keypad 5) *)
        Some
          (Key
             {
               key = KP_5;
               modifier = fst (parse_modifiers_and_event params_str);
               event_type = Press;
               associated_text = "";
               shifted_key = None;
               base_key = None;
             })
    (* Home/End *)
    | 'H' ->
        Some
          (Key
             {
               key = Home;
               modifier = fst (parse_modifiers_and_event params_str);
               event_type = snd (parse_modifiers_and_event params_str);
               associated_text = "";
               shifted_key = None;
               base_key = None;
             })
    | 'F' ->
        Some
          (Key
             {
               key = End;
               modifier = fst (parse_modifiers_and_event params_str);
               event_type = snd (parse_modifiers_and_event params_str);
               associated_text = "";
               shifted_key = None;
               base_key = None;
             })
    (* Tab *)
    | 'Z' ->
        Some
          (Key
             {
               key = Tab;
               modifier = { no_modifier with shift = true };
               event_type = Press;
               associated_text = "";
               shifted_key = None;
               base_key = None;
             })
    (* Focus events *)
    | 'I' -> Some Focus
    | 'O' -> Some Blur
    (* Tilde sequences *)
    | '~' -> (
        (* Parse tilde sequences with colon-separated event types *)
        let parse_tilde_params params_str =
          match params_str with
          | [] -> (None, no_modifier, Press)
          | [ code ] ->
              (parse_int code 0 (String.length code), no_modifier, Press)
          | [ code; mods ] -> (
              let code_val = parse_int code 0 (String.length code) in
              let sub = parse_sub_params mods in
              match sub with
              | [ Some m ] ->
                  let m = m - 1 in
                  let modifier =
                    {
                      shift = m land 1 <> 0;
                      alt = m land 2 <> 0;
                      ctrl = m land 4 <> 0;
                      super = m land 8 <> 0;
                      hyper = m land 16 <> 0;
                      meta = m land 32 <> 0;
                      caps_lock = m land 64 <> 0;
                      num_lock = m land 128 <> 0;
                    }
                  in
                  (code_val, modifier, Press)
              | [ Some m; Some e ] ->
                  let m = m - 1 in
                  let modifier =
                    {
                      shift = m land 1 <> 0;
                      alt = m land 2 <> 0;
                      ctrl = m land 4 <> 0;
                      super = m land 8 <> 0;
                      hyper = m land 16 <> 0;
                      meta = m land 32 <> 0;
                      caps_lock = m land 64 <> 0;
                      num_lock = m land 128 <> 0;
                    }
                  in
                  let event_type =
                    match e with 2 -> Repeat | 3 -> Release | _ -> Press
                  in
                  (code_val, modifier, event_type)
              | _ -> (code_val, no_modifier, Press))
          | code :: mods :: _ -> (
              (* Handle legacy format with 3 params like "5;1;3" for page_up with mods and release *)
              let code_val = parse_int code 0 (String.length code) in
              let mods_val = parse_int mods 0 (String.length mods) in
              match mods_val with
              | Some m ->
                  let m = m - 1 in
                  let modifier =
                    {
                      shift = m land 1 <> 0;
                      alt = m land 2 <> 0;
                      ctrl = m land 4 <> 0;
                      super = m land 8 <> 0;
                      hyper = m land 16 <> 0;
                      meta = m land 32 <> 0;
                      caps_lock = m land 64 <> 0;
                      num_lock = m land 128 <> 0;
                    }
                  in
                  (* Check if third param is event type *)
                  let event_type =
                    match List.nth_opt params_str 2 with
                    | Some e_str -> (
                        match parse_int e_str 0 (String.length e_str) with
                        | Some 2 -> Repeat
                        | Some 3 -> Release
                        | _ -> Press)
                    | None -> Press
                  in
                  (code_val, modifier, event_type)
              | None -> (code_val, no_modifier, Press))
        in
        let code, modifier, event_type = parse_tilde_params params_str in
        (* Map tilde codes to events *)
        match code with
        | Some 200 -> Some Paste_start
        | Some 201 -> Some Paste_end
        | Some n -> (
            (* Map tilde codes to keys *)
            let key_opt =
              match n with
              | 1 -> Some Home
              | 2 -> Some Insert
              | 3 -> Some Delete
              | 4 -> Some End
              | 5 -> Some Page_up
              | 6 -> Some Page_down
              | 7 -> Some Home (* Alternative *)
              | 8 -> Some End (* Alternative *)
              | 11 -> Some (F 1)
              | 12 -> Some (F 2)
              | 13 -> Some (F 3)
              | 14 -> Some (F 4)
              | 15 -> Some (F 5)
              | 17 -> Some (F 6)
              | 18 -> Some (F 7)
              | 19 -> Some (F 8)
              | 20 -> Some (F 9)
              | 21 -> Some (F 10)
              | 23 -> Some (F 11)
              | 24 -> Some (F 12)
              | 25 -> Some (F 13)
              | 26 -> Some (F 14)
              | 28 -> Some (F 15)
              | 29 -> Some (F 16)
              | 31 -> Some (F 17)
              | 32 -> Some (F 18)
              | 33 -> Some (F 19)
              | 34 -> Some (F 20)
              | _ -> None
            in
            match key_opt with
            | Some key ->
                Some
                  (Key
                     {
                       key;
                       modifier;
                       event_type;
                       associated_text = "";
                       shifted_key = None;
                       base_key = None;
                     })
            | None -> None)
        | None -> None)
    (* CSI-u format (modern keyboard protocol) *)
    | 'u' -> parse_kitty_keyboard s start (!intermediate_end + 1)
    (* Window manipulation *)
    | 't' -> (
        match params with
        | [ Some 8; Some h; Some w ] ->
            (* CSI 8 ; height ; width t reports terminal size *)
            Some (Resize (w, h))
        | _ -> None)
    (* Cursor position report *)
    | 'R' -> (
        match params with
        | [ Some row; Some col ] ->
            (* CSI row ; col R reports cursor position *)
            Some (Cursor_position (row, col))
        | _ -> None)
    (* Device attributes *)
    | 'c' ->
        if
          (* Check if this is a device attributes response (CSI ? ... c) *)
          (* The '?' is at the beginning of the params section *)
          start > 0 && s.[start] = '?'
        then
          (* Device attributes response - parse params after the '?' *)
          let attrs = parse_csi_params s (start + 1) !params_end in
          let attrs = List.filter_map (fun x -> x) attrs in
          Some (Device_attributes attrs)
        else
          (* Primary DA request - we don't handle this as an event *)
          None
    (* Mouse events *)
    | '<' ->
        parse_sgr_mouse
          (String.sub s start (!intermediate_end - start + 1))
          0
          (!intermediate_end - start + 1)
    | 'M' | 'm' ->
        if start < String.length s && s.[start] = '<' then
          (* SGR mouse format: ESC [ < params M/m *)
          parse_sgr_mouse s start (!intermediate_end + 1)
        else if start = 0 && !params_end > start then
          (* URXVT format *)
          parse_urxvt_mouse s start end_
        else if start = 0 && !params_end = start then
          (* Normal/X10 format - parse bytes that follow *)
          None (* Will be handled in feed function *)
        else None
    | _ -> None
  else None

let parse_escape_sequence s start length =
  if length < 2 then (None, 0)
  else
    let esc2 = s.[start + 1] in
    if esc2 = '[' then
      match parse_csi s (start + 2) (start + length) with
      | Some event -> (Some event, length)
      | None -> (None, 0)
    else if esc2 = 'O' && length >= 3 then
      (* SS3 sequences *)
      let event =
        match s.[start + 2] with
        | 'A' ->
            Some
              (Key
                 {
                   key = Up;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'B' ->
            Some
              (Key
                 {
                   key = Down;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'C' ->
            Some
              (Key
                 {
                   key = Right;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'D' ->
            Some
              (Key
                 {
                   key = Left;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'H' ->
            Some
              (Key
                 {
                   key = Home;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'F' ->
            Some
              (Key
                 {
                   key = End;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'P' ->
            Some
              (Key
                 {
                   key = F 1;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'Q' ->
            Some
              (Key
                 {
                   key = F 2;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'R' ->
            Some
              (Key
                 {
                   key = F 3;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'S' ->
            Some
              (Key
                 {
                   key = F 4;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        (* Keypad sequences in application mode *)
        | 'M' ->
            Some
              (Key
                 {
                   key = KP_enter;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'j' ->
            Some
              (Key
                 {
                   key = KP_multiply;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'k' ->
            Some
              (Key
                 {
                   key = KP_add;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'l' ->
            Some
              (Key
                 {
                   key = KP_separator;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
            (* comma *)
        | 'm' ->
            Some
              (Key
                 {
                   key = KP_subtract;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'n' ->
            Some
              (Key
                 {
                   key = KP_decimal;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'o' ->
            Some
              (Key
                 {
                   key = KP_divide;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'p' ->
            Some
              (Key
                 {
                   key = KP_0;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'q' ->
            Some
              (Key
                 {
                   key = KP_1;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'r' ->
            Some
              (Key
                 {
                   key = KP_2;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 's' ->
            Some
              (Key
                 {
                   key = KP_3;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 't' ->
            Some
              (Key
                 {
                   key = KP_4;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'u' ->
            Some
              (Key
                 {
                   key = KP_5;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'v' ->
            Some
              (Key
                 {
                   key = KP_6;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'w' ->
            Some
              (Key
                 {
                   key = KP_7;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'x' ->
            Some
              (Key
                 {
                   key = KP_8;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'y' ->
            Some
              (Key
                 {
                   key = KP_9;
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
        | 'X' ->
            Some
              (Key
                 {
                   key = Char (Uchar.of_char '=');
                   modifier = no_modifier;
                   event_type = Press;
                   associated_text = "";
                   shifted_key = None;
                   base_key = None;
                 })
            (* equal *)
        | _ -> None
      in
      (event, 3)
    else if esc2 = ']' then
      (* OSC sequences *)
      let rec find_terminator i =
        if i >= start + length then None
        else
          let c = s.[i] in
          if c = '\x07' then Some i
          else if c = '\x1b' && i + 1 < start + length && s.[i + 1] = '\\' then
            Some (i + 1)
          else find_terminator (i + 1)
      in
      match find_terminator (start + 2) with
      | None -> (None, 0)
      | Some pos ->
          let seq_len = pos - (start + 2) in
          let seq = String.sub s (start + 2) seq_len in
          let semi = try String.index seq ';' with _ -> String.length seq in
          let code_str = String.sub seq 0 semi in
          let data =
            if semi < String.length seq then
              String.sub seq (semi + 1) (String.length seq - semi - 1)
            else ""
          in
          let code = try int_of_string code_str with _ -> 0 in
          let consumed = pos - start + 1 in
          let event =
            if code = 52 then
              (* OSC 52 - Clipboard access *)
              (* Format: selection;base64-data *)
              let selection_end =
                try String.index data ';' with _ -> String.length data
              in
              let selection = String.sub data 0 selection_end in
              let clipboard_data =
                if selection_end < String.length data then
                  String.sub data (selection_end + 1)
                    (String.length data - selection_end - 1)
                else ""
              in
              Clipboard (selection, clipboard_data)
            else Osc (code, data)
          in
          (Some event, consumed)
    else if length >= 2 then
      (* Alt+char sequences *)
      let c = s.[start + 1] in
      if c >= ' ' && c <= '~' then
        ( Some
            (Key
               {
                 key = Char (Uchar.of_int (Char.code c));
                 modifier = { no_modifier with alt = true };
                 event_type = Press;
                 associated_text = "";
                 shifted_key = None;
                 base_key = None;
               }),
          2 )
      else (None, 0)
    else (None, 0)

let feed parser bytes offset length =
  (* Helper to process and clear buffer *)
  let process_and_clear_buffer () =
    let rec process_buffer acc pos =
      if pos >= parser.length then (
        (* Move any unprocessed bytes to the beginning of the buffer *)
        if pos < parser.length then (
          Bytes.blit parser.buffer pos parser.buffer 0 (parser.length - pos);
          parser.length <- parser.length - pos)
        else parser.length <- 0;
        List.rev acc)
      else
        let c = Bytes.get parser.buffer pos in
        if parser.in_paste then (
          (* Always add the current byte to paste_buffer first *)
          Buffer.add_char parser.paste_buffer c;
          let buf_len = Buffer.length parser.paste_buffer in
          (* Check if the buffer now ends with the end sequence *)
          if buf_len >= 6 then
            let last_6 = Buffer.sub parser.paste_buffer (buf_len - 6) 6 in
            if last_6 = "\x1b[201~" then (
              (* Extract content excluding the end sequence *)
              let content_len = buf_len - 6 in
              let paste_content =
                if content_len > 0 then
                  Buffer.sub parser.paste_buffer 0 content_len
                else ""
              in
              Buffer.clear parser.paste_buffer;
              parser.in_paste <- false;
              (* Build events: Paste if content, always Paste_end *)
              let events =
                if paste_content = "" then Paste_end :: acc
                else Paste_end :: Paste paste_content :: acc
              in
              process_buffer events (pos + 1))
            else
              (* No match, continue *)
              process_buffer acc (pos + 1)
          else
            (* Too short for end sequence, continue *)
            process_buffer acc (pos + 1))
        else if c = '\x1b' then
          if
            (* Check for X10/normal mouse protocol first *)
            pos + 1 < parser.length
            && Bytes.get parser.buffer (pos + 1) = '['
            && pos + 2 < parser.length
            && Bytes.get parser.buffer (pos + 2) = 'M'
            && pos + 5 < parser.length
          then
            (* X10/normal mouse: ESC [ M btn x y *)
            match parse_x10_normal_mouse parser.buffer (pos + 3) with
            | Some (event, consumed) ->
                process_buffer (event :: acc) (pos + 3 + consumed)
            | None ->
                (* Fall back to normal escape sequence parsing *)
                let rec find_end i =
                  if i >= parser.length then i
                  else
                    let c = Bytes.get parser.buffer i in
                    if i = pos + 1 && c <> '[' && c <> 'O' then i + 1
                    else if
                      i > pos + 1
                      && (is_csi_final c
                         || (i = pos + 2 && c >= 'A' && c <= 'Z'))
                    then i + 1
                    else find_end (i + 1)
                in
                let seq_end = find_end (pos + 1) in
                if seq_end > pos + 1 then
                  let seq_str =
                    Bytes.sub_string parser.buffer pos (seq_end - pos)
                  in
                  match parse_escape_sequence seq_str 0 (seq_end - pos) with
                  | Some Paste_start, consumed ->
                      parser.in_paste <- true;
                      process_buffer (Paste_start :: acc) (pos + consumed)
                  | Some event, consumed ->
                      process_buffer (event :: acc) (pos + consumed)
                  | None, _ ->
                      if seq_end = pos + 2 && pos + 1 < parser.length then
                        let next_char = Bytes.get parser.buffer (pos + 1) in
                        if next_char = '[' || next_char = 'O' then
                          (* Incomplete escape sequence, keep buffering *)
                          List.rev acc
                        else
                          (* ESC followed by non-sequence character *)
                          process_buffer
                            (Key
                               {
                                 key = Escape;
                                 modifier = no_modifier;
                                 event_type = Press;
                                 associated_text = "";
                                 shifted_key = None;
                                 base_key = None;
                               }
                            :: acc)
                            (pos + 1)
                      else process_buffer acc (pos + 1)
                else List.rev acc
          else
            let rec find_end i =
              if i >= parser.length then i
              else
                let c = Bytes.get parser.buffer i in
                if i = pos + 1 && c <> '[' && c <> 'O' && c <> ']' then i + 1
                else if
                  i > pos + 1
                  && (is_csi_final c || (i = pos + 2 && c >= 'A' && c <= 'Z'))
                then i + 1
                else find_end (i + 1)
            in
            let seq_end = find_end (pos + 1) in
            if seq_end > pos + 1 then
              let seq_str =
                Bytes.sub_string parser.buffer pos (seq_end - pos)
              in
              match parse_escape_sequence seq_str 0 (seq_end - pos) with
              | Some Paste_start, consumed ->
                  parser.in_paste <- true;
                  process_buffer (Paste_start :: acc) (pos + consumed)
              | Some event, consumed ->
                  process_buffer (event :: acc) (pos + consumed)
              | None, _ ->
                  if seq_end = pos + 2 && pos + 1 < parser.length then
                    let next_char = Bytes.get parser.buffer (pos + 1) in
                    if next_char = '[' || next_char = 'O' || next_char = ']'
                    then List.rev acc
                    else
                      process_buffer
                        (Key
                           {
                             key = Escape;
                             modifier = no_modifier;
                             event_type = Press;
                             associated_text = "";
                             shifted_key = None;
                             base_key = None;
                           }
                        :: acc)
                        (pos + 1)
                  else process_buffer acc (pos + 1)
            else List.rev acc
        else if c >= '\x00' && c <= '\x1f' then
          (* Control character *)
          let key_event =
            match c with
            | '\x00' ->
                Key
                  {
                    key = Char (Uchar.of_int 32);
                    modifier = { no_modifier with ctrl = true };
                    event_type = Press;
                    associated_text = "";
                    shifted_key = None;
                    base_key = None;
                  }
            | ('\x01' .. '\x08' | '\x0b' .. '\x0c' | '\x0e' .. '\x1a') as c ->
                (* Skip tab (0x09), LF (0x0a), CR (0x0d) *)
                Key
                  {
                    key = Char (Uchar.of_int (Char.code c + 64));
                    modifier = { no_modifier with ctrl = true };
                    event_type = Press;
                    associated_text = "";
                    shifted_key = None;
                    base_key = None;
                  }
            | '\t' ->
                Key
                  {
                    key = Tab;
                    modifier = no_modifier;
                    event_type = Press;
                    associated_text = "";
                    shifted_key = None;
                    base_key = None;
                  }
            | '\n' | '\r' ->
                Key
                  {
                    key = Enter;
                    modifier = no_modifier;
                    event_type = Press;
                    associated_text = "";
                    shifted_key = None;
                    base_key = None;
                  }
            | _ ->
                Key
                  {
                    key = key_of_char c;
                    modifier = no_modifier;
                    event_type = Press;
                    associated_text = "";
                    shifted_key = None;
                    base_key = None;
                  }
          in
          process_buffer (key_event :: acc) (pos + 1)
        else if c = '\x7f' then
          process_buffer
            (Key
               {
                 key = Backspace;
                 modifier = no_modifier;
                 event_type = Press;
                 associated_text = "";
                 shifted_key = None;
                 base_key = None;
               }
            :: acc)
            (pos + 1)
        else if c < '\x80' then
          (* ASCII character *)
          let key_event =
            Key
              {
                key = key_of_char c;
                modifier = no_modifier;
                event_type = Press;
                associated_text = "";
                shifted_key = None;
                base_key = None;
              }
          in
          process_buffer (key_event :: acc) (pos + 1)
        else
          (* UTF-8 multi-byte character *)
          (* Try to decode UTF-8 from current position *)
          (* First, check if this looks like the start of a UTF-8 sequence *)
          let utf8_len =
            if c < '\x80' then 1
            else if c < '\xc0' then 1 (* Invalid continuation byte *)
            else if c < '\xe0' then 2 (* 2-byte sequence *)
            else if c < '\xf0' then 3 (* 3-byte sequence *)
            else if c < '\xf8' then 4 (* 4-byte sequence *)
            else 1 (* Invalid *)
          in

          (* Check if we have enough bytes for the complete sequence *)
          if pos + utf8_len > parser.length then
            (* Incomplete UTF-8 sequence at end of buffer - stop processing here *)
            (* The bytes will be preserved for the next feed *)
            List.rev acc
          else (
            (* We have enough bytes, feed them to the decoder *)
            Uutf.Manual.src parser.utf8_decoder parser.buffer pos utf8_len;
            match Uutf.decode parser.utf8_decoder with
            | `Uchar u ->
                let event =
                  Key
                    {
                      key = Char u;
                      modifier = no_modifier;
                      event_type = Press;
                      associated_text = "";
                      shifted_key = None;
                      base_key = None;
                    }
                in
                process_buffer (event :: acc) (pos + utf8_len)
            | `Malformed _ ->
                (* Skip this byte and continue *)
                process_buffer acc (pos + 1)
            | `Await ->
                (* This shouldn't happen if we calculated utf8_len correctly *)
                (* Skip one byte to avoid infinite loop *)
                process_buffer acc (pos + 1)
            | `End ->
                (* Shouldn't happen in manual mode *)
                process_buffer acc (pos + 1))
    in
    process_buffer [] 0
  in

  (* For large inputs, process in chunks *)
  if parser.length + length > Bytes.length parser.buffer then
    (* Process current buffer content first if any *)
    let initial_events =
      if parser.length > 0 then process_and_clear_buffer () else []
    in

    (* Process input in buffer-sized chunks *)
    let rec process_chunks events offset remaining =
      if remaining = 0 then events
      else
        let chunk_size = min remaining (Bytes.length parser.buffer) in
        Bytes.blit bytes offset parser.buffer 0 chunk_size;
        parser.length <- chunk_size;
        let chunk_events = process_and_clear_buffer () in
        process_chunks (events @ chunk_events) (offset + chunk_size)
          (remaining - chunk_size)
    in

    initial_events @ process_chunks [] offset length
  else (
    (* Normal case: input fits in buffer *)
    Bytes.blit bytes offset parser.buffer parser.length length;
    parser.length <- parser.length + length;
    process_and_clear_buffer ())

let pending parser = Bytes.sub parser.buffer 0 parser.length

let reset parser =
  parser.length <- 0;
  parser.in_paste <- false;
  Buffer.clear parser.paste_buffer

(* Pretty-printing *)
let pp_key fmt = function
  | Char u ->
      let b = Buffer.create 4 in
      Uutf.Buffer.add_utf_8 b u;
      Format.fprintf fmt "Char(%s)" (Buffer.contents b)
  | Enter -> Format.fprintf fmt "Enter"
  | Tab -> Format.fprintf fmt "Tab"
  | Backspace -> Format.fprintf fmt "Backspace"
  | Delete -> Format.fprintf fmt "Delete"
  | Escape -> Format.fprintf fmt "Escape"
  | Up -> Format.fprintf fmt "Up"
  | Down -> Format.fprintf fmt "Down"
  | Left -> Format.fprintf fmt "Left"
  | Right -> Format.fprintf fmt "Right"
  | Home -> Format.fprintf fmt "Home"
  | End -> Format.fprintf fmt "End"
  | Page_up -> Format.fprintf fmt "Page_up"
  | Page_down -> Format.fprintf fmt "Page_down"
  | Insert -> Format.fprintf fmt "Insert"
  | F n -> Format.fprintf fmt "F%d" n
  | Print_screen -> Format.fprintf fmt "Print_screen"
  | Pause -> Format.fprintf fmt "Pause"
  | Menu -> Format.fprintf fmt "Menu"
  | Scroll_lock -> Format.fprintf fmt "Scroll_lock"
  | Media_play -> Format.fprintf fmt "Media_play"
  | Media_pause -> Format.fprintf fmt "Media_pause"
  | Media_play_pause -> Format.fprintf fmt "Media_play_pause"
  | Media_stop -> Format.fprintf fmt "Media_stop"
  | Media_reverse -> Format.fprintf fmt "Media_reverse"
  | Media_fast_forward -> Format.fprintf fmt "Media_fast_forward"
  | Media_rewind -> Format.fprintf fmt "Media_rewind"
  | Media_next -> Format.fprintf fmt "Media_next"
  | Media_prev -> Format.fprintf fmt "Media_prev"
  | Media_record -> Format.fprintf fmt "Media_record"
  | Volume_up -> Format.fprintf fmt "Volume_up"
  | Volume_down -> Format.fprintf fmt "Volume_down"
  | Volume_mute -> Format.fprintf fmt "Volume_mute"
  | Shift_left -> Format.fprintf fmt "Shift_left"
  | Shift_right -> Format.fprintf fmt "Shift_right"
  | Ctrl_left -> Format.fprintf fmt "Ctrl_left"
  | Ctrl_right -> Format.fprintf fmt "Ctrl_right"
  | Alt_left -> Format.fprintf fmt "Alt_left"
  | Alt_right -> Format.fprintf fmt "Alt_right"
  | Super_left -> Format.fprintf fmt "Super_left"
  | Super_right -> Format.fprintf fmt "Super_right"
  | Hyper_left -> Format.fprintf fmt "Hyper_left"
  | Hyper_right -> Format.fprintf fmt "Hyper_right"
  | Meta_left -> Format.fprintf fmt "Meta_left"
  | Meta_right -> Format.fprintf fmt "Meta_right"
  | Iso_level3_shift -> Format.fprintf fmt "Iso_level3_shift"
  | Iso_level5_shift -> Format.fprintf fmt "Iso_level5_shift"
  | Caps_lock -> Format.fprintf fmt "Caps_lock"
  | Num_lock -> Format.fprintf fmt "Num_lock"
  | KP_0 -> Format.fprintf fmt "KP_0"
  | KP_1 -> Format.fprintf fmt "KP_1"
  | KP_2 -> Format.fprintf fmt "KP_2"
  | KP_3 -> Format.fprintf fmt "KP_3"
  | KP_4 -> Format.fprintf fmt "KP_4"
  | KP_5 -> Format.fprintf fmt "KP_5"
  | KP_6 -> Format.fprintf fmt "KP_6"
  | KP_7 -> Format.fprintf fmt "KP_7"
  | KP_8 -> Format.fprintf fmt "KP_8"
  | KP_9 -> Format.fprintf fmt "KP_9"
  | KP_decimal -> Format.fprintf fmt "KP_decimal"
  | KP_divide -> Format.fprintf fmt "KP_divide"
  | KP_multiply -> Format.fprintf fmt "KP_multiply"
  | KP_subtract -> Format.fprintf fmt "KP_subtract"
  | KP_add -> Format.fprintf fmt "KP_add"
  | KP_enter -> Format.fprintf fmt "KP_enter"
  | KP_equal -> Format.fprintf fmt "KP_equal"
  | KP_separator -> Format.fprintf fmt "KP_separator"
  | KP_begin -> Format.fprintf fmt "KP_begin"
  | KP_left -> Format.fprintf fmt "KP_left"
  | KP_right -> Format.fprintf fmt "KP_right"
  | KP_up -> Format.fprintf fmt "KP_up"
  | KP_down -> Format.fprintf fmt "KP_down"
  | KP_page_up -> Format.fprintf fmt "KP_page_up"
  | KP_page_down -> Format.fprintf fmt "KP_page_down"
  | KP_home -> Format.fprintf fmt "KP_home"
  | KP_end -> Format.fprintf fmt "KP_end"
  | KP_insert -> Format.fprintf fmt "KP_insert"
  | KP_delete -> Format.fprintf fmt "KP_delete"
  | Unknown n -> Format.fprintf fmt "Unknown(%d)" n

let pp_event_type fmt (event_type : event_type) =
  match event_type with
  | Press -> Format.fprintf fmt "Press"
  | Repeat -> Format.fprintf fmt "Repeat"
  | Release -> Format.fprintf fmt "Release"

let pp_modifier fmt m =
  let mods = [] in
  let mods = if m.shift then "shift" :: mods else mods in
  let mods = if m.alt then "alt" :: mods else mods in
  let mods = if m.ctrl then "ctrl" :: mods else mods in
  let mods = if m.super then "super" :: mods else mods in
  let mods = if m.hyper then "hyper" :: mods else mods in
  let mods = if m.meta then "meta" :: mods else mods in
  let mods = if m.caps_lock then "caps_lock" :: mods else mods in
  let mods = if m.num_lock then "num_lock" :: mods else mods in
  match mods with
  | [] -> Format.fprintf fmt "no_modifier"
  | _ -> Format.fprintf fmt "{%s}" (String.concat "+" mods)

let pp_uchar fmt u =
  let b = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 b u;
  Format.fprintf fmt "%S" (Buffer.contents b)

let pp_key_event fmt e =
  Format.fprintf fmt "{key=%a; modifier=%a; event_type=%a" pp_key e.key
    pp_modifier e.modifier pp_event_type e.event_type;
  if e.associated_text <> "" then
    Format.fprintf fmt "; associated_text=%S" e.associated_text;
  (match e.shifted_key with
  | None -> ()
  | Some u -> Format.fprintf fmt "; shifted_key=%a" pp_uchar u);
  (match e.base_key with
  | None -> ()
  | Some u -> Format.fprintf fmt "; base_key=%a" pp_uchar u);
  Format.fprintf fmt "}"

let pp_mouse_button fmt = function
  | Left -> Format.fprintf fmt "Left"
  | Middle -> Format.fprintf fmt "Middle"
  | Right -> Format.fprintf fmt "Right"
  | Wheel_up -> Format.fprintf fmt "Wheel_up"
  | Wheel_down -> Format.fprintf fmt "Wheel_down"
  | Wheel_left -> Format.fprintf fmt "Wheel_left"
  | Wheel_right -> Format.fprintf fmt "Wheel_right"
  | Button n -> Format.fprintf fmt "Button(%d)" n

let pp_mouse_button_state fmt s =
  let buttons = [] in
  let buttons = if s.left then "left" :: buttons else buttons in
  let buttons = if s.middle then "middle" :: buttons else buttons in
  let buttons = if s.right then "right" :: buttons else buttons in
  match buttons with
  | [] -> Format.fprintf fmt "{}"
  | _ -> Format.fprintf fmt "{%s}" (String.concat "," buttons)

let pp_mouse_event fmt = function
  | Button_press (x, y, btn, mods) ->
      Format.fprintf fmt "Button_press(%d,%d,%a,%a)" x y pp_mouse_button btn
        pp_modifier mods
  | Button_release (x, y, btn, mods) ->
      Format.fprintf fmt "Button_release(%d,%d,%a,%a)" x y pp_mouse_button btn
        pp_modifier mods
  | Motion (x, y, state, mods) ->
      Format.fprintf fmt "Motion(%d,%d,%a,%a)" x y pp_mouse_button_state state
        pp_modifier mods

let pp_event fmt = function
  | Key k -> Format.fprintf fmt "Key(%a)" pp_key_event k
  | Mouse m -> Format.fprintf fmt "Mouse(%a)" pp_mouse_event m
  | Resize (w, h) -> Format.fprintf fmt "Resize(%d,%d)" w h
  | Focus -> Format.fprintf fmt "Focus"
  | Blur -> Format.fprintf fmt "Blur"
  | Paste_start -> Format.fprintf fmt "Paste_start"
  | Paste_end -> Format.fprintf fmt "Paste_end"
  | Paste s -> Format.fprintf fmt "Paste(%S)" s
  | Clipboard (selection, data) ->
      Format.fprintf fmt "Clipboard(%S,%S)" selection data
  | Osc (code, data) -> Format.fprintf fmt "Osc(%d,%S)" code data
  | Cursor_position (row, col) ->
      Format.fprintf fmt "Cursor_position(%d,%d)" row col
  | Device_attributes attrs ->
      Format.fprintf fmt "Device_attributes([%s])"
        (String.concat ";" (List.map string_of_int attrs))

(* Convenience functions *)

let parse_single s =
  let parser = create () in
  feed parser (Bytes.of_string s) 0 (String.length s)

(* Event creation helpers *)

let key_event ?(modifier = no_modifier) ?(event_type = Press) k =
  Key
    {
      key = k;
      modifier;
      event_type;
      associated_text = "";
      shifted_key = None;
      base_key = None;
    }

let char_event ?(modifier = no_modifier) ?(event_type = Press) c =
  key_event ~modifier ~event_type (Char (Uchar.of_char c))

let mouse_press ?(modifier = no_modifier) x y button =
  Mouse (Button_press (x, y, button, modifier))

let mouse_release ?(modifier = no_modifier) x y button =
  Mouse (Button_release (x, y, button, modifier))

let mouse_motion ?(modifier = no_modifier) x y state =
  Mouse (Motion (x, y, state, modifier))
