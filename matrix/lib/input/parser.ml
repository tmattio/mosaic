(* parser.ml *)

open Event

type key = Key.t
type event_type = Key.event_type
type modifier = Key.modifier
type mouse_button = Mouse.button
type mouse_button_state = Mouse.button_state
type scroll_direction = Mouse.scroll_direction
type event = Event.t
type parsed = [ `User of event | `Caps of Caps.event ]

type t = {
  raw : Tokenizer.parser;
  scratch_buffer : Buffer.t; (* Reusable buffer for various operations *)
  utf8_dec : Uutf.decoder; (* Persistent decoder to avoid allocations *)
}

(* Modifiers *)

let no_modifier = Key.no_modifier

(* Precomputed modifier table to avoid allocations *)
let modifier_table : modifier array =
  Array.init 256 (fun mask ->
      {
        Key.ctrl = mask land 4 <> 0;
        alt = mask land 2 <> 0;
        shift = mask land 1 <> 0;
        super = mask land 8 <> 0;
        hyper = mask land 16 <> 0;
        meta = mask land 32 <> 0;
        caps_lock = mask land 64 <> 0;
        num_lock = mask land 128 <> 0;
      })

(* Small “modifier DSL” *)
let mk_modifier ?(ctrl = false) ?(alt = false) ?(shift = false) ?(super = false)
    ?(hyper = false) ?(meta = false) ?(caps_lock = false) ?(num_lock = false) ()
    : modifier =
  { Key.ctrl; alt; shift; super; hyper; meta; caps_lock; num_lock }

let modifier_of_bits bits =
  if bits <= 0 then no_modifier else modifier_table.((bits - 1) land 0xff)

let event_type_of_int : int -> event_type = function
  | 2 -> Repeat
  | 3 -> Release
  | _ -> Press

(* Mouse helpers *)

let mouse_button_of_code : int -> mouse_button = function
  | 0 -> Left
  | 1 -> Middle
  | 2 -> Right
  | 3 -> Left
  | 64 -> Wheel_up
  | 65 -> Wheel_down
  | 66 -> Wheel_left
  | 67 -> Wheel_right
  | 128 -> Button 8
  | 129 -> Button 9
  | 130 -> Button 10
  | 131 -> Button 11
  | n -> Button n

let is_scroll_button = function
  | Mouse.Wheel_up | Wheel_down | Wheel_left | Wheel_right -> true
  | _ -> false

let scroll_direction_of_button : mouse_button -> scroll_direction = function
  | Wheel_up -> Scroll_up
  | Wheel_down -> Scroll_down
  | Wheel_left -> Scroll_left
  | Wheel_right -> Scroll_right
  | _ -> Scroll_down

let mouse_button_state_of_code code : mouse_button_state =
  { left = code = 0; middle = code = 1; right = code = 2 }

let modifier_of_mouse_value value : modifier =
  let mask = (value land 0x1c) lsr 2 in
  modifier_table.(mask land 0xff)

(* Parser state *)

let create () =
  {
    raw = Tokenizer.create ();
    scratch_buffer = Buffer.create 32;
    utf8_dec = Uutf.decoder ~encoding:`UTF_8 `Manual;
  }

(* CSI helpers *)

let is_ascii_digit c = c >= '0' && c <= '9'
let is_csi_param c = c >= '\x30' && c <= '\x3f'
let is_csi_intermediate c = c >= '\x20' && c <= '\x2f'

let is_csi_final c =
  let code = Char.code c in
  (code >= 0x40 && code <= 0x7e) || code = 0x24 || code = 0x5e

(* Strip ANSI/terminal escape sequences from text. Used to sanitize
   bracketed paste payloads so pasted content cannot inject control codes. *)
let strip_ansi parser s =
  if not (String.contains s '\x1b') then s
  else
    let len = String.length s in
    let buf = parser.scratch_buffer in
    Buffer.clear buf;
    let rec find_st i =
      if i >= len then len
      else if s.[i] = '\x1b' && i + 1 < len && s.[i + 1] = '\\' then i + 2
      else find_st (i + 1)
    in
    let rec loop i =
      if i >= len then Buffer.contents buf
      else
        let c = s.[i] in
        if c <> '\x1b' then (
          Buffer.add_char buf c;
          loop (i + 1))
        else if i + 1 >= len then Buffer.contents buf
        else
          let c2 = s.[i + 1] in
          if c2 = '[' then (
            (* CSI: ESC [ ... final (0x40-0x7e) *)
            let j = ref (i + 2) in
            while
              !j < len
              &&
              let ch = s.[!j] in
              let code = Char.code ch in
              code < 0x40 || code > 0x7e
            do
              incr j
            done;
            if !j < len then loop (!j + 1) else Buffer.contents buf)
          else if c2 = ']' || c2 = 'P' || c2 = '_' then
            if
              (* OSC (]), DCS (P), APC (_): end with ST (ESC \). OSC can also end with BEL. *)
              c2 = ']'
            then
              let rec find_end k =
                if k >= len then len
                else if s.[k] = '\x07' then k + 1
                else if k + 1 < len && s.[k] = '\x1b' && s.[k + 1] = '\\' then
                  k + 2
                else find_end (k + 1)
              in
              let k = find_end (i + 2) in
              if k <= len then loop k else Buffer.contents buf
            else
              let k = find_st (i + 2) in
              if k <= len then loop k else Buffer.contents buf
          else
            (* Other ESC-prefixed sequence: skip ESC + following char *)
            loop (i + 2)
    in
    loop 0

(* Manual integer parsing without exceptions *)
let parse_int_range s start end_ =
  if start >= end_ then 0
  else
    let rec loop acc i =
      if i >= end_ then acc
      else if is_ascii_digit s.[i] then
        loop ((acc * 10) + (Char.code s.[i] - 48)) (i + 1)
      else acc
    in
    loop 0 start

let parse_int_range_opt s start end_ =
  if start >= end_ then None
  else
    let rec loop acc i =
      if i >= end_ then Some acc
      else
        let c = s.[i] in
        if is_ascii_digit c then loop ((acc * 10) + (Char.code c - 48)) (i + 1)
        else None
    in
    loop 0 start

type csi_param = { start : int; stop : int; value : int option }

let parse_csi_params s start end_ =
  let rec loop acc i =
    if i >= end_ then List.rev acc
    else
      let rec find_param_end j =
        if j >= end_ || s.[j] = ';' || not (is_csi_param s.[j]) then j
        else find_param_end (j + 1)
      in
      let param_end = find_param_end i in
      let value = parse_int_range_opt s i param_end in
      let next =
        if param_end < end_ && s.[param_end] = ';' then param_end + 1
        else param_end
      in
      loop ({ start = i; stop = param_end; value } :: acc) next
  in
  loop [] start

let parse_sub_params_slice s start stop =
  let rec loop acc i =
    if i >= stop then List.rev acc
    else
      let rec find_field_end j =
        if j >= stop || s.[j] = ':' then j else find_field_end (j + 1)
      in
      let field_end = find_field_end i in
      let field_opt = parse_int_range_opt s i field_end in
      let next =
        if field_end < stop && s.[field_end] = ':' then field_end + 1
        else field_end
      in
      loop (field_opt :: acc) next
  in
  loop [] start

let has_prefix s prefix =
  let len = String.length prefix in
  String.length s >= len
  &&
  let rec loop i =
    if i = len then true else if s.[i] <> prefix.[i] then false else loop (i + 1)
  in
  loop 0

let has_suffix s suffix =
  let len = String.length suffix in
  let slen = String.length s in
  slen >= len
  &&
  let rec loop i =
    if i = len then true
    else if s.[slen - len + i] <> suffix.[i] then false
    else loop (i + 1)
  in
  loop 0

let contains_substring s sub =
  let len_s = String.length s and len_sub = String.length sub in
  if len_sub = 0 then true
  else
    let rec loop idx =
      if idx + len_sub > len_s then false
      else if String.sub s idx len_sub = sub then true
      else loop (idx + 1)
    in
    loop 0

type capability_match = Event of Caps.event | Drop | No_match

let capability_event_of_sequence seq =
  let len = String.length seq in
  if len >= 6 && has_prefix seq "\x1bP>|" && has_suffix seq "\x1b\\" then
    let payload_len = len - 6 in
    let payload =
      if payload_len > 0 then String.sub seq 4 payload_len else ""
    in
    Event (Caps.Xtversion payload)
  else if
    len >= 4 && has_prefix seq "\x1bP" && has_suffix seq "\x1b\\"
    && contains_substring (String.lowercase_ascii seq) "kitty"
  then
    (* Some terminals respond with a plain DCS payload containing "kitty"
       without the XTVersion prefix; treat as Kitty identification. *)
    Event (Caps.Xtversion "kitty")
  else if len >= 5 && has_prefix seq "\x1b_G" && has_suffix seq "\x1b\\" then
    let payload_len = len - 5 in
    let payload =
      if payload_len > 0 then String.sub seq 3 payload_len else ""
    in
    Event (Caps.Kitty_graphics_reply payload)
  else if len >= 4 && has_prefix seq "\x1b[?" && has_suffix seq "u" then
    let body_len = len - 4 in
    if body_len <= 0 then Drop
    else
      let body = String.sub seq 3 body_len in
      let parts = String.split_on_char ';' body in
      let to_int_opt s = try Some (int_of_string s) with _ -> None in
      match parts with
      | level_str :: rest -> (
          match to_int_opt level_str with
          | None -> Drop
          | Some level ->
              let flags =
                match rest with [] -> None | h :: _ -> to_int_opt h
              in
              Event (Caps.Kitty_keyboard { level; flags }))
      | _ -> Drop
  else No_match

type csi_mod = { code : int option; mods : int option; event : int option }

let get_value values idx =
  match List.nth_opt values idx with Some v -> v | None -> None

let split_param s (field : csi_param) =
  let values = parse_sub_params_slice s field.start field.stop in
  (get_value values 0, get_value values 1, get_value values 2)

let csi_mod_of_params s params =
  match params with
  | [] -> { code = None; mods = None; event = None }
  | [ p ] ->
      let code, mods, event = split_param s p in
      { code; mods; event }
  | [ code; mods ] ->
      let code, _, _ = split_param s code in
      let mods, event, _ = split_param s mods in
      { code; mods; event }
  | code :: mods :: event :: _ ->
      let code, _, _ = split_param s code in
      let mods, _, _ = split_param s mods in
      let event, _, _ = split_param s event in
      { code; mods; event }

let modifier_of_csi_mod m =
  match m.mods with Some bits -> modifier_of_bits bits | None -> no_modifier

let event_type_of_csi_mod m =
  match m.event with Some e -> event_type_of_int e | None -> Press

let ensure_shift m = if m.Key.shift then m else { m with shift = true }
let ensure_ctrl m = if m.Key.ctrl then m else { m with Key.ctrl = true }

(* UTF-8 helpers *)

let decode_utf8_char_string s pos =
  let len = String.length s in
  if pos >= len then None
  else
    let b0 = Char.code s.[pos] in
    if b0 < 0x80 then Some (b0, 1)
    else if b0 < 0xC0 then None
    else if b0 < 0xE0 && pos + 1 < len then
      let b1 = Char.code s.[pos + 1] in
      if b1 land 0xC0 = 0x80 then
        let code = ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F) in
        Some (code, 2)
      else None
    else if b0 < 0xF0 && pos + 2 < len then
      let b1 = Char.code s.[pos + 1] in
      let b2 = Char.code s.[pos + 2] in
      if b1 land 0xC0 = 0x80 && b2 land 0xC0 = 0x80 then
        let code =
          ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
        in
        Some (code, 3)
      else None
    else if b0 < 0xF8 && pos + 3 < len then
      let b1 = Char.code s.[pos + 1] in
      let b2 = Char.code s.[pos + 2] in
      let b3 = Char.code s.[pos + 3] in
      if b1 land 0xC0 = 0x80 && b2 land 0xC0 = 0x80 && b3 land 0xC0 = 0x80 then
        let code =
          ((b0 land 0x07) lsl 18)
          lor ((b1 land 0x3F) lsl 12)
          lor ((b2 land 0x3F) lsl 6)
          lor (b3 land 0x3F)
        in
        Some (code, 4)
      else None
    else None

let parse_x10_normal_mouse_string s start =
  let len = String.length s in
  if start + 3 <= len then
    match decode_utf8_char_string s start with
    | None -> None
    | Some (btn, len1) -> (
        match decode_utf8_char_string s (start + len1) with
        | None -> None
        | Some (ux, len2) -> (
            match decode_utf8_char_string s (start + len1 + len2) with
            | None -> None
            | Some (uy, len3) ->
                let cb = btn - 32 in
                let x = ux - 33 in
                let y = uy - 33 in
                let consumed = len1 + len2 + len3 in
                if x < 0 || y < 0 then None
                else
                  let base_button = cb land 3 in
                  let is_scroll = cb land 64 <> 0 in
                  let modifier = modifier_of_mouse_value cb in
                  let ev : event =
                    if is_scroll then
                      let button = mouse_button_of_code (cb land 0xC3) in
                      Scroll
                        (x, y, scroll_direction_of_button button, 1, modifier)
                    else if base_button = 3 then
                      Mouse (Mouse.Button_release (x, y, Button 0, modifier))
                    else
                      let button =
                        match base_button with
                        | 0 -> Mouse.Left
                        | 1 -> Middle
                        | 2 -> Right
                        | n -> Button n
                      in
                      Mouse (Mouse.Button_press (x, y, button, modifier))
                  in
                  Some (ev, consumed)))
  else None

(* Generic helper for mouse events used by SGR/URXVT-style encodings *)
let mouse_event_of_codes ?release_button ~button_code ~x ~y ~is_release
    ~is_motion ~modifier () : event =
  let button = mouse_button_of_code button_code in
  let is_scroll = is_scroll_button button in
  if is_scroll then Scroll (x, y, scroll_direction_of_button button, 1, modifier)
  else if is_motion then
    Mouse
      (Mouse.Motion (x, y, mouse_button_state_of_code button_code, modifier))
  else if is_release then
    let b = match release_button with Some b -> b | None -> button in
    Mouse (Mouse.Button_release (x, y, b, modifier))
  else Mouse (Mouse.Button_press (x, y, button, modifier))

let parse_sgr_mouse s start end_ =
  (* Format: "<btn;x;y" followed by final M/m at [end_ - 1]. *)
  let final = s.[end_ - 1] in
  if final <> 'M' && final <> 'm' then None
  else if s.[start] <> '<' || end_ - start < 5 then None
  else
    let limit = end_ - 1 in
    let parse_field i =
      if i >= limit then (None, i)
      else
        let c0 = s.[i] in
        if c0 < '0' || c0 > '9' then (None, i)
        else
          let rec loop acc j =
            if j >= limit then (Some acc, j)
            else
              let c = s.[j] in
              if c >= '0' && c <= '9' then
                loop ((acc * 10) + (Char.code c - 48)) (j + 1)
              else (Some acc, j)
          in
          loop 0 i
    in
    let btn_opt, idx1 = parse_field (start + 1) in
    match btn_opt with
    | None -> None
    | Some btn -> (
        if idx1 >= limit || s.[idx1] <> ';' then None
        else
          let x_opt, idx2 = parse_field (idx1 + 1) in
          match x_opt with
          | None -> None
          | Some x -> (
              if idx2 >= limit || s.[idx2] <> ';' then None
              else
                let y_opt, idx3 = parse_field (idx2 + 1) in
                match y_opt with
                | None -> None
                | Some y ->
                    if idx3 <> limit then None
                    else
                      let button_code = btn land 3 lor ((btn lsr 6) lsl 6) in
                      let modifier = modifier_of_mouse_value btn in
                      let button = mouse_button_of_code button_code in
                      let is_scroll = is_scroll_button button in
                      let is_motion = btn land 32 <> 0 && not is_scroll in
                      let is_release =
                        final = 'm' && (not is_scroll) && not is_motion
                      in
                      let event =
                        mouse_event_of_codes ~button_code ~x:(x - 1) ~y:(y - 1)
                          ~is_release ~is_motion ~modifier ()
                      in
                      Some event))

let parse_urxvt_mouse s start end_ =
  (* Format: "btn;x;y" followed by final M/m at [end_ - 1]. *)
  let final = s.[end_ - 1] in
  if final <> 'M' && final <> 'm' then None
  else if end_ - start < 5 then None
  else
    let limit = end_ - 1 in
    let parse_field i =
      if i >= limit then (None, i)
      else
        let c0 = s.[i] in
        if c0 < '0' || c0 > '9' then (None, i)
        else
          let rec loop acc j =
            if j >= limit then (Some acc, j)
            else
              let c = s.[j] in
              if c >= '0' && c <= '9' then
                loop ((acc * 10) + (Char.code c - 48)) (j + 1)
              else (Some acc, j)
          in
          loop 0 i
    in
    let btn_opt, idx1 = parse_field start in
    match btn_opt with
    | None -> None
    | Some btn -> (
        if idx1 >= limit || s.[idx1] <> ';' then None
        else
          let x_opt, idx2 = parse_field (idx1 + 1) in
          match x_opt with
          | None -> None
          | Some x -> (
              if idx2 >= limit || s.[idx2] <> ';' then None
              else
                let y_opt, idx3 = parse_field (idx2 + 1) in
                match y_opt with
                | None -> None
                | Some y ->
                    if idx3 <> limit then None
                    else
                      let base_btn = btn - 32 in
                      let button_code =
                        base_btn land 3 lor ((base_btn lsr 6) lsl 6)
                      in
                      let button = mouse_button_of_code button_code in
                      let is_scroll = is_scroll_button button in
                      let modifier = modifier_of_mouse_value base_btn in
                      let is_motion = base_btn land 32 <> 0 && not is_scroll in
                      let is_release =
                        button_code = 3 && (not is_scroll) && not is_motion
                      in
                      let release_button =
                        if is_release then Some (Mouse.Button 0) else None
                      in
                      let event =
                        if is_scroll then
                          Scroll
                            ( x - 1,
                              y - 1,
                              scroll_direction_of_button button,
                              1,
                              modifier )
                        else
                          mouse_event_of_codes ?release_button ~button_code
                            ~x:(x - 1) ~y:(y - 1) ~is_release ~is_motion
                            ~modifier ()
                      in
                      Some event))

(* Kitty keyboard PUA mapping *)

let pua_to_key c : key =
  match c with
  (* Kitty PUA codes follow the upstream mapping documented in the protocol spec. *)
  (* Standard keys in PUA block *)
  | 57344 -> Escape
  | 57345 -> Enter
  | 57346 -> Tab
  | 57347 -> Backspace
  | 57348 -> Insert
  | 57349 -> Delete
  | 57350 -> Left
  | 57351 -> Right
  | 57352 -> Up
  | 57353 -> Down
  | 57354 -> Page_up
  | 57355 -> Page_down
  | 57356 -> Home
  | 57357 -> End
  (* Locks and system keys *)
  | 57358 -> Caps_lock
  | 57359 -> Scroll_lock
  | 57360 -> Num_lock
  | 57361 -> Print_screen
  | 57362 -> Pause
  | 57363 -> Menu
  (* Function keys F1-F35: 57364..57398 *)
  | c when c >= 57364 && c <= 57398 -> F (c - 57363)
  (* Keypad keys follow the same numbering scheme as Kitty's reference implementation. *)
  | 57400 -> KP_0
  | 57401 -> KP_1
  | 57402 -> KP_2
  | 57403 -> KP_3
  | 57404 -> KP_4
  | 57405 -> KP_5
  | 57406 -> KP_6
  | 57407 -> KP_7
  | 57408 -> KP_8
  | 57409 -> KP_9
  | 57410 -> KP_decimal
  | 57411 -> KP_divide
  | 57412 -> KP_multiply
  | 57413 -> KP_subtract
  | 57414 -> KP_add
  | 57415 -> KP_enter
  | 57416 -> KP_equal
  | 57417 -> KP_left
  | 57418 -> KP_right
  | 57419 -> KP_up
  | 57420 -> KP_down
  | 57421 -> KP_page_up
  | 57422 -> KP_page_down
  | 57423 -> KP_home
  | 57424 -> KP_end
  | 57425 -> KP_insert
  | 57426 -> KP_delete
  | 57427 -> KP_begin
  (* Media keys *)
  | 57428 -> Media_play
  | 57429 -> Media_pause
  | 57430 -> Media_play_pause
  | 57431 -> Media_reverse
  | 57432 -> Media_stop
  | 57433 -> Media_fast_forward
  | 57434 -> Media_rewind
  | 57435 -> Media_next
  | 57436 -> Media_prev
  | 57437 -> Media_record
  | 57438 -> Volume_down
  | 57439 -> Volume_up
  | 57440 -> Volume_mute
  (* Modifier keys *)
  | 57441 -> Shift_left
  | 57442 -> Ctrl_left
  | 57443 -> Alt_left
  | 57444 -> Super_left
  | 57445 -> Hyper_left
  | 57446 -> Meta_left
  | 57447 -> Shift_right
  | 57448 -> Ctrl_right
  | 57449 -> Alt_right
  | 57450 -> Super_right
  | 57451 -> Hyper_right
  | 57452 -> Meta_right
  | 57453 -> Iso_level3_shift
  | 57454 -> Iso_level5_shift
  | _ -> Unknown c

let key_of_tilde_code = function
  | 1 | 7 -> Some Key.Home
  | 2 -> Some Insert
  | 3 -> Some Delete
  | 4 | 8 -> Some End
  | 5 -> Some Page_up
  | 6 -> Some Page_down
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

(* Key helpers *)

let make_key_event ?(modifier = no_modifier) ?(event_type = Key.Press)
    ?(associated_text = "") ?shifted_key ?base_key (k : key) : event =
  Event.key ~modifier ~event_type ~associated_text ?shifted_key ?base_key k

let make_char_event ?modifier ?event_type ?associated_text ?shifted_key
    ?base_key c : event =
  Event.char ?modifier ?event_type ?associated_text ?shifted_key ?base_key c

let alt_escape_event : event =
  make_key_event ~modifier:(mk_modifier ~alt:true ()) Escape

(* Kitty keyboard CSI u *)

let parse_kitty_keyboard parser s start end_ =
  (* Parse CSI ... u format directly from string to avoid allocations *)
  let i = ref start in
  let len = end_ - 1 in
  (* exclude the 'u' final char *)

  (* Helper to find next separator *)
  let find_sep sep =
    let start_pos = !i in
    while !i < len && s.[!i] <> sep do
      incr i
    done;
    let end_pos = !i in
    if !i < len then incr i;
    (* skip separator *)
    (start_pos, end_pos)
  in

  (* Parse first field: code[:shifted[:base]] *)
  let code_start, code_end = find_sep ';' in
  if code_start >= code_end then None
  else
    (* Parse code with optional :shifted:base *)
    let code_i = ref code_start in
    let code = parse_int_range s !code_i (min (!code_i + 10) code_end) in
    while !code_i < code_end && s.[!code_i] <> ':' do
      incr code_i
    done;
    let shifted =
      if !code_i < code_end then (
        incr code_i;
        (* skip ':' *)
        let shifted_start = !code_i in
        while !code_i < code_end && s.[!code_i] <> ':' do
          incr code_i
        done;
        let shifted_val = parse_int_range s shifted_start !code_i in
        if shifted_val > 0 then Some (Uchar.of_int shifted_val) else None)
      else None
    in
    let base =
      if !code_i < code_end then (
        incr code_i;
        (* skip ':' *)
        let base_val = parse_int_range s !code_i code_end in
        if base_val > 0 then Some (Uchar.of_int base_val) else None)
      else None
    in

    (* Parse second field: mods[:event] *)
    let mods_start, mods_end = find_sep ';' in
    let mods_i = ref mods_start in
    let mods = parse_int_range s !mods_i (min (!mods_i + 5) mods_end) in
    while !mods_i < mods_end && s.[!mods_i] <> ':' do
      incr mods_i
    done;
    let event_val =
      if !mods_i < mods_end then (
        incr mods_i;
        (* skip ':' *)
        parse_int_range s !mods_i mods_end)
      else 1
    in

    (* Parse optional third field: text as colon-separated codepoints *)
    let associated_text =
      if !i >= len then ""
      else
        let buf = parser.scratch_buffer in
        Buffer.clear buf;
        let pos = ref !i in
        while !pos < len do
          let field_start = !pos in
          while !pos < len && s.[!pos] <> ':' do
            incr pos
          done;
          let cp = parse_int_range s field_start !pos in
          if cp > 0 then Uutf.Buffer.add_utf_8 buf (Uchar.of_int cp);
          if !pos < len then incr pos (* skip ':' *)
        done;
        Buffer.contents buf
    in

    let modifier = modifier_of_bits mods in
    let event_type = event_type_of_int event_val in
    let key_opt : key option =
      match code with
      | 0 -> None
      | 9 -> Some Tab
      | 13 -> Some Enter
      | 27 -> Some Escape
      | 127 -> Some Backspace
      | c when c >= 57344 && c <= 63743 -> Some (pua_to_key c)
      | c when c > 0 -> Some (Char (Uchar.of_int c))
      | _ -> None
    in
    match key_opt with
    | None -> None
    | Some k ->
        let associated_text =
          if associated_text <> "" then associated_text
          else
            match k with
            | Char u ->
                let buf = parser.scratch_buffer in
                Buffer.clear buf;
                let chosen =
                  match (modifier.Key.shift, shifted) with
                  | true, Some su -> su
                  | _ -> u
                in
                Uutf.Buffer.add_utf_8 buf chosen;
                Buffer.contents buf
            | _ -> ""
        in
        Some
          (make_key_event ~modifier ~event_type ~associated_text
             ?shifted_key:shifted ?base_key:base k)

(* CSI parsing *)

let parse_csi parser s start end_ : parsed option =
  let params_end = ref start in
  while !params_end < end_ && is_csi_param s.[!params_end] do
    incr params_end
  done;

  let intermediate_end = ref !params_end in
  let rec consume_intermediate () =
    if !intermediate_end < end_ then
      let ch = s.[!intermediate_end] in
      let is_dollar = ch = '$' || ch = '^' in
      if is_csi_intermediate ch then
        if is_dollar && !intermediate_end + 1 >= end_ then ()
        else (
          incr intermediate_end;
          consume_intermediate ())
      else ()
    else ()
  in
  consume_intermediate ();

  if !intermediate_end < end_ && is_csi_final s.[!intermediate_end] then
    let final_char = s.[!intermediate_end] in
    let params = parse_csi_params s start !params_end in

    (* Parse modifiers and event types from params for cursor keys *)
    let csi_mod = csi_mod_of_params s params in
    let modifier = modifier_of_csi_mod csi_mod in
    let event_type = event_type_of_csi_mod csi_mod in

    match final_char with
    (* Cursor movement - also handle keypad in numeric mode *)
    | 'A' -> Some (`User (make_key_event ~modifier ~event_type Up))
    | 'B' -> Some (`User (make_key_event ~modifier ~event_type Down))
    | 'C' -> Some (`User (make_key_event ~modifier ~event_type Right))
    | 'D' -> Some (`User (make_key_event ~modifier ~event_type Left))
    | 'E' -> Some (`User (make_key_event ~modifier ~event_type KP_5))
    | 'a' ->
        let modifier = ensure_shift modifier in
        Some (`User (make_key_event ~modifier ~event_type Up))
    | 'b' ->
        let modifier = ensure_shift modifier in
        Some (`User (make_key_event ~modifier ~event_type Down))
    | 'd' ->
        let modifier = ensure_shift modifier in
        Some (`User (make_key_event ~modifier ~event_type Left))
    | 'e' ->
        let modifier = ensure_shift modifier in
        Some (`User (make_key_event ~modifier ~event_type KP_5))
    (* Home/End *)
    | 'H' -> Some (`User (make_key_event ~modifier ~event_type Home))
    | 'F' -> Some (`User (make_key_event ~modifier ~event_type End))
    (* Tab *)
    | 'Z' ->
        Some (`User (make_key_event ~modifier:(mk_modifier ~shift:true ()) Tab))
    (* Focus events *)
    | 'I' -> Some (`User Focus)
    | 'O' -> Some (`User Blur)
    (* Tilde sequences *)
    | ('$' | '^') as suffix -> (
        match params with
        | [ { value = Some n; _ } ] -> (
            match key_of_tilde_code n with
            | Some key ->
                let modifier =
                  if suffix = '$' then ensure_shift modifier
                  else ensure_ctrl modifier
                in
                Some (`User (make_key_event ~modifier ~event_type key))
            | None -> None)
        | _ -> None)
    | '~' -> (
        match params with
        | { value = Some 27; _ }
          :: { value = Some mods; _ }
          :: { value = Some code; _ }
          :: _ ->
            let modifier = modifier_of_bits mods in
            let key_from_charcode = function
              | 13 -> Key.Enter
              | 27 -> Escape
              | 9 -> Tab
              | 32 -> Char (Uchar.of_int 32)
              | 127 | 8 -> Backspace
              | c when c > 0 -> Char (Uchar.of_int c)
              | _ -> Char (Uchar.of_int 0)
            in
            Some (`User (make_key_event ~modifier (key_from_charcode code)))
        | _ -> (
            match csi_mod.code with
            | Some 200 | Some 201 -> None
            | Some n -> (
                match key_of_tilde_code n with
                | Some key ->
                    Some (`User (make_key_event ~modifier ~event_type key))
                | None -> None)
            | None -> None))
    (* CSI-u format (modern keyboard protocol) *)
    | 'u' -> (
        match parse_kitty_keyboard parser s start (!intermediate_end + 1) with
        | None -> None
        | Some e -> Some (`User e))
    | 'y' ->
        let is_private =
          start < !params_end && s.[start] = '?' && start + 1 <= !params_end
        in
        let params =
          if is_private then parse_csi_params s (start + 1) !params_end
          else params
        in
        let rec parse_pairs acc = function
          | { value = Some mode; _ } :: { value = Some value; _ } :: rest ->
              parse_pairs ((mode, value) :: acc) rest
          | _ -> List.rev acc
        in
        let modes = parse_pairs [] params in
        Some (`Caps (Caps.Mode_report { Caps.is_private; modes }))
    (* Window manipulation *)
    | 't' -> (
        match params with
        | [
         { value = Some 8; _ }; { value = Some h; _ }; { value = Some w; _ };
        ] ->
            (* CSI 8 ; height ; width t reports terminal size *)
            Some (`User (Resize (w, h)))
        | [
         { value = Some 4; _ }; { value = Some h; _ }; { value = Some w; _ };
        ] ->
            (* CSI 4 ; height ; width t reports pixel resolution *)
            Some (`Caps (Caps.Pixel_resolution (w, h)))
        | _ -> None)
    (* Cursor position report *)
    | 'R' -> (
        match params with
        | [ { value = Some row; _ }; { value = Some col; _ } ] ->
            (* CSI row ; col R reports cursor position *)
            Some (`Caps (Caps.Cursor_position (row, col)))
        | _ -> None)
    (* Device attributes *)
    | 'c' ->
        if start > 0 && s.[start] = '?' then
          let attrs =
            parse_csi_params s (start + 1) !params_end
            |> List.filter_map (fun x -> x.value)
          in
          Some (`Caps (Caps.Device_attributes attrs))
        else if params = [] then
          (* rxvt sends shifted arrows as CSI a/b/c/d/e. Keep support while
             still treating CSI ?...c as device attributes. *)
          let modifier = ensure_shift modifier in
          Some (`User (make_key_event ~modifier ~event_type Right))
        else None
    (* Mouse events *)
    | 'M' | 'm' ->
        if start < String.length s && s.[start] = '<' then
          (* SGR mouse format: ESC [ < params M/m *)
          Option.map
            (fun e -> `User e)
            (parse_sgr_mouse s start (!intermediate_end + 1))
        else if !params_end > start then
          (* URXVT format: ESC [ params M/m (no '<') *)
          Option.map
            (fun e -> `User e)
            (parse_urxvt_mouse s start (!intermediate_end + 1))
        else None
    | _ -> None
  else None

(* ESC parsing *)

let parse_escape_sequence parser s start length : parsed option * int =
  if length <= 0 then (None, 0)
  else if length = 1 then (Some (`User (make_key_event Escape)), 1)
  else
    let esc2 = s.[start + 1] in
    if esc2 = '[' then
      match parse_csi parser s (start + 2) (start + length) with
      | Some event -> (Some event, length)
      | None -> (None, 0)
    else if esc2 = 'O' && length >= 3 then
      (* SS3 sequences - used by terminals for arrows, home/end, function keys *)
      let event_opt =
        match s.[start + 2] with
        | 'A' -> Some (make_key_event Up)
        | 'B' -> Some (make_key_event Down)
        | 'C' -> Some (make_key_event Right)
        | 'D' -> Some (make_key_event Left)
        | 'F' -> Some (make_key_event End)
        | 'H' -> Some (make_key_event Home)
        | 'P' -> Some (make_key_event (F 1))
        | 'Q' -> Some (make_key_event (F 2))
        | 'R' -> Some (make_key_event (F 3))
        | 'S' -> Some (make_key_event (F 4))
        | 'a' -> Some (make_key_event ~modifier:(mk_modifier ~ctrl:true ()) Up)
        | 'b' ->
            Some (make_key_event ~modifier:(mk_modifier ~ctrl:true ()) Down)
        | 'c' ->
            Some (make_key_event ~modifier:(mk_modifier ~ctrl:true ()) Right)
        | 'd' ->
            Some (make_key_event ~modifier:(mk_modifier ~ctrl:true ()) Left)
        | 'e' ->
            Some (make_key_event ~modifier:(mk_modifier ~ctrl:true ()) KP_5)
        | 'X' -> Some (make_char_event '=') (* keypad = *)
        | _ -> None
      in
      (Option.map (fun e -> `User e) event_opt, 3)
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
              let base64_data =
                if selection_end < String.length data then
                  String.sub data (selection_end + 1)
                    (String.length data - selection_end - 1)
                else ""
              in
              (* Decode base64 *)
              let decode_base64 s =
                let decode_char = function
                  | 'A' .. 'Z' as c -> Char.code c - Char.code 'A'
                  | 'a' .. 'z' as c -> Char.code c - Char.code 'a' + 26
                  | '0' .. '9' as c -> Char.code c - Char.code '0' + 52
                  | '+' -> 62
                  | '/' -> 63
                  | '=' -> 0 (* Padding *)
                  | _ -> 0
                in
                let len = String.length s in
                let out = parser.scratch_buffer in
                Buffer.clear out;
                let rec decode i =
                  if i + 3 < len then (
                    let a = decode_char s.[i] in
                    let b = decode_char s.[i + 1] in
                    let c = decode_char s.[i + 2] in
                    let d = decode_char s.[i + 3] in
                    Buffer.add_char out (Char.chr ((a lsl 2) lor (b lsr 4)));
                    if s.[i + 2] <> '=' then (
                      Buffer.add_char out
                        (Char.chr (((b land 15) lsl 4) lor (c lsr 2)));
                      if s.[i + 3] <> '=' then
                        Buffer.add_char out
                          (Char.chr (((c land 3) lsl 6) lor d)));
                    decode (i + 4))
                in
                decode 0;
                Buffer.contents out
              in
              let clipboard_data =
                try decode_base64 base64_data
                with _ ->
                  base64_data (* Fallback to raw data if decode fails *)
              in
              Clipboard (selection, clipboard_data)
            else Osc (code, data)
          in
          (Some (`User event), consumed)
    else if length >= 2 then
      (* Alt (ESC-prefix) combinations and ESC+control handling *)
      let c = s.[start + 1] in
      (* ESC + control-char -> Alt+Ctrl letter (or space for NUL) *)
      if c >= '\x00' && c <= '\x1a' then
        let key, modifier =
          if c = '\x00' then
            (Key.Char (Uchar.of_int 32), mk_modifier ~alt:true ~ctrl:true ())
          else if c = '\t' then (Tab, mk_modifier ~alt:true ())
          else if c = '\n' then (Line_feed, mk_modifier ~alt:true ())
          else if c = '\r' then (Enter, mk_modifier ~alt:true ())
          else
            (* Map 0x01..0x1a to 'A'..'Z' with ctrl+alt *)
            ( Char (Uchar.of_int (Char.code c + 64)),
              mk_modifier ~alt:true ~ctrl:true () )
        in
        (Some (`User (make_key_event ~modifier key)), 2)
      else if c = '\x7f' || c = '\b' then
        ( Some
            (`User
               (make_key_event ~modifier:(mk_modifier ~alt:true ()) Backspace)),
          2 )
      else if c = ' ' then
        ( Some
            (`User
               (make_key_event ~modifier:(mk_modifier ~alt:true ())
                  (Char (Uchar.of_int 32)))),
          2 )
      else if c >= '!' && c <= '~' then
        let is_upper = c >= 'A' && c <= 'Z' in
        let modifier =
          if is_upper then mk_modifier ~alt:true ~shift:true ()
          else mk_modifier ~alt:true ()
        in
        ( Some
            (`User
               (make_key_event ~modifier (Char (Uchar.of_int (Char.code c))))),
          2 )
      else (None, 0)
    else (None, 0)

(* Char decoding *)

let special_key_event_of_code code : event option =
  match code with
  | 0x00 -> Some (make_char_event ~modifier:(mk_modifier ~ctrl:true ()) ' ')
  | 0x08 | 0x7f -> Some (make_key_event Backspace)
  | 0x09 -> Some (make_key_event Tab)
  | 0x0a -> Some (make_key_event Line_feed)
  | 0x0d -> Some (make_key_event Enter)
  | c when c >= 0x01 && c <= 0x1A ->
      let letter = 0x40 + c in
      Some
        (make_key_event
           ~modifier:(mk_modifier ~ctrl:true ())
           (Char (Uchar.of_int letter)))
  | _ -> None

(* Precomputed ASCII key events to avoid per-character allocation in hot paths. *)
let ascii_events : event option array =
  Array.init 128 (fun code ->
      match special_key_event_of_code code with
      | Some ev -> Some ev
      | None ->
          let modifier =
            if code >= Char.code 'A' && code <= Char.code 'Z' then
              mk_modifier ~shift:true ()
            else no_modifier
          in
          Some (make_char_event ~modifier (Char.chr code)))

(* Unified logic for “what to do with a code point” *)
let key_event_of_code parser code : event option =
  if code >= 0 && code < Array.length ascii_events then ascii_events.(code)
  else
    let u = Uchar.of_int code in
    let modifier =
      if code >= Char.code 'A' && code <= Char.code 'Z' then
        mk_modifier ~shift:true ()
      else no_modifier
    in
    let buf = parser.scratch_buffer in
    Buffer.clear buf;
    Uutf.Buffer.add_utf_8 buf u;
    let associated_text = Buffer.contents buf in
    Some (make_key_event ~modifier ~associated_text (Char u))

let text_events_of_bytes parser bytes off len : event list =
  let dec = parser.utf8_dec in
  Uutf.Manual.src dec bytes off len;
  let rec loop acc =
    match Uutf.decode dec with
    | `Uchar u ->
        let code = Uchar.to_int u in
        let acc =
          match key_event_of_code parser code with
          | Some e -> e :: acc
          | None -> acc
        in
        loop acc
    | `Malformed _ -> loop acc
    | `Await | `End -> List.rev acc
  in
  loop []

(* Event classification: user vs caps *)

let add_parsed_to_acc (p : parsed)
    ((user_acc, caps_acc) : event list * Caps.event list) :
    event list * Caps.event list =
  match p with
  | `User e -> (e :: user_acc, caps_acc)
  | `Caps c -> (user_acc, c :: caps_acc)

(* Token processing *)

let process_tokens parser tokens : event list * Caps.event list =
  let rec loop (user_acc, caps_acc) = function
    | [] -> (List.rev user_acc, List.rev caps_acc)
    | Tokenizer.Paste payload :: rest ->
        let sanitized = strip_ansi parser payload in
        let user_acc =
          if sanitized = "" then user_acc else Paste sanitized :: user_acc
        in
        loop (user_acc, caps_acc) rest
    | Tokenizer.Sequence seq :: rest -> (
        if seq = "\x1b[200~" || seq = "\x1b[201~" then
          loop (user_acc, caps_acc) rest
        else
          (* Prefer capability responses over generic escape parsing so DCS/OSC
             replies (e.g., Kitty detection) are handled correctly. *)
          match capability_event_of_sequence seq with
          | Event cap ->
              loop (add_parsed_to_acc (`Caps cap) (user_acc, caps_acc)) rest
          | Drop -> loop (user_acc, caps_acc) rest
          | No_match -> (
              let ev_opt =
                let seq_len = String.length seq in
                if
                  seq_len >= 3
                  && seq.[0] = '\x1b'
                  && seq.[1] = '['
                  && seq.[2] = 'M'
                then
                  match parse_x10_normal_mouse_string seq 3 with
                  | Some (e, _) -> Some (`User e)
                  | None -> None
                else if seq = "\x1b\x1b" then Some (`User alt_escape_event)
                else
                  let ev, _ = parse_escape_sequence parser seq 0 seq_len in
                  ev
              in
              match ev_opt with
              | Some p -> loop (add_parsed_to_acc p (user_acc, caps_acc)) rest
              | None ->
                  let len = String.length seq in
                  if len > 1 then
                    let bytes = Bytes.unsafe_of_string seq in
                    let evs = text_events_of_bytes parser bytes 1 (len - 1) in
                    let user_acc, caps_acc =
                      List.fold_left
                        (fun acc e -> add_parsed_to_acc (`User e) acc)
                        (user_acc, caps_acc) evs
                    in
                    loop (user_acc, caps_acc) rest
                  else loop (user_acc, caps_acc) rest))
    | Tokenizer.Text s :: rest ->
        let bytes = Bytes.unsafe_of_string s in
        let evs = text_events_of_bytes parser bytes 0 (String.length s) in
        let user_acc, caps_acc =
          List.fold_left
            (fun acc e -> add_parsed_to_acc (`User e) acc)
            (user_acc, caps_acc) evs
        in
        loop (user_acc, caps_acc) rest
  in
  loop ([], []) tokens

(* Public API *)

let feed parser bytes offset length : event list * Caps.event list =
  let tokens = Tokenizer.feed parser.raw bytes offset length in
  process_tokens parser tokens

let flush ?now parser : event list * Caps.event list =
  let timestamp = match now with Some t -> t | None -> Unix.gettimeofday () in
  let tokens = Tokenizer.flush_expired parser.raw timestamp in
  if tokens = [] then ([], []) else process_tokens parser tokens

let next_flush_deadline parser = Tokenizer.deadline parser.raw
let pending parser = Tokenizer.pending parser.raw

let reset parser =
  Tokenizer.reset parser.raw;
  Buffer.clear parser.scratch_buffer;
  Uutf.Manual.src parser.utf8_dec Bytes.empty 0 0

let parse_single s : event list * Caps.event list =
  let p = create () in
  feed p (Bytes.of_string s) 0 (String.length s)
