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

type modifier = { ctrl : bool; alt : bool; shift : bool }

let no_modifier = { ctrl = false; alt = false; shift = false }

type key_event = { key : key; modifier : modifier }

let key ?(ctrl = false) ?(alt = false) ?(shift = false) k =
  { key = k; modifier = { ctrl; alt; shift } }

let char ?(ctrl = false) ?(alt = false) ?(shift = false) c =
  { key = Char (Uchar.of_char c); modifier = { ctrl; alt; shift } }

type mouse_button =
  | Left
  | Middle
  | Right
  | Wheel_up
  | Wheel_down
  | Button of int

type mouse_button_state = { left : bool; middle : bool; right : bool }

type mouse_event =
  | Press of int * int * mouse_button * modifier
  | Release of int * int * mouse_button * modifier
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

type parser = {
  buffer : bytes;
  mutable length : int;
  mutable in_paste : bool;
  paste_buffer : Buffer.t;
}

let create () =
  {
    buffer = Bytes.create 4096;
    length = 0;
    in_paste = false;
    paste_buffer = Buffer.create 256;
  }

let is_ascii_digit c = c >= '0' && c <= '9'
let is_csi_param c = c >= '\x30' && c <= '\x3f'
let is_csi_intermediate c = c >= '\x20' && c <= '\x2f'
let is_csi_final c = c >= '\x40' && c <= '\x7e'

let parse_int s start end_ =
  let rec loop acc i =
    if i >= end_ then Some acc
    else
      let c = s.[i] in
      if is_ascii_digit c then loop ((acc * 10) + (Char.code c - 48)) (i + 1)
      else None
  in
  loop 0 start

let parse_csi_params s start end_ =
  let rec loop acc start =
    if start >= end_ then List.rev acc
    else
      let rec find_end i =
        if i >= end_ || s.[i] = ';' then i else find_end (i + 1)
      in
      let param_end = find_end start in
      let param =
        if param_end > start then parse_int s start param_end else None
      in
      let acc = param :: acc in
      if param_end < end_ && s.[param_end] = ';' then loop acc (param_end + 1)
      else List.rev acc
  in
  loop [] start

let parse_x10_mouse bytes start =
  if start + 3 <= Bytes.length bytes then
    let btn = Char.code (Bytes.get bytes start) - 32 in
    let x = Char.code (Bytes.get bytes (start + 1)) - 33 in
    let y = Char.code (Bytes.get bytes (start + 2)) - 33 in
    let button_code = btn land 0b11 in
    let button =
      match button_code with
      | 0 -> Left
      | 1 -> Middle
      | 2 -> Right
      | 3 -> Button 3 (* Release in X10 *)
      | _ -> Button button_code
    in
    let modifier =
      {
        shift = btn land 4 <> 0;
        alt = btn land 8 <> 0;
        ctrl = btn land 16 <> 0;
      }
    in
    let event =
      if button_code = 3 then Release (x, y, Left, modifier)
        (* X10 doesn't tell us which button was released *)
      else if btn land 32 <> 0 then
        let button_state =
          {
            left = button_code = 0;
            middle = button_code = 1;
            right = button_code = 2;
          }
        in
        Motion (x, y, button_state, modifier)
      else Press (x, y, button, modifier)
    in
    Some (Mouse event, 3)
  else None

let parse_sgr_mouse s start end_ =
  (* Skip the '<' character if present *)
  let params_start =
    if start < end_ && s.[start] = '<' then start + 1 else start
  in
  match parse_csi_params s params_start (end_ - 1) with
  | [ Some btn; Some x; Some y ]
    when end_ > start && (s.[end_ - 1] = 'M' || s.[end_ - 1] = 'm') ->
      let button_code = btn land 0b11111 in
      let button =
        match button_code with
        | 0 -> Left
        | 1 -> Middle
        | 2 -> Right
        | 64 -> Wheel_up
        | 65 -> Wheel_down
        | n -> Button n
      in
      let modifier =
        {
          shift = btn land 4 <> 0;
          alt = btn land 8 <> 0;
          ctrl = btn land 16 <> 0;
        }
      in
      let event =
        if btn land 32 <> 0 then
          let button_state =
            {
              left = button_code = 0;
              middle = button_code = 1;
              right = button_code = 2;
            }
          in
          Motion (x - 1, y - 1, button_state, modifier)
        else if s.[end_ - 1] = 'M' then Press (x - 1, y - 1, button, modifier)
        else Release (x - 1, y - 1, button, modifier)
      in
      Some (Mouse event)
  | _ -> None

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

    (* Parse modifiers from params for cursor keys *)
    let parse_modifiers = function
      | [ Some m ] ->
          (* For sequences like CSI 2 A, where param is modifier *)
          let m = m - 1 in
          { shift = m land 1 <> 0; alt = m land 2 <> 0; ctrl = m land 4 <> 0 }
      | [ Some _key_code; Some m ] ->
          (* For sequences like CSI 1;2 A or CSI 13;2~ *)
          let m = m - 1 in
          { shift = m land 1 <> 0; alt = m land 2 <> 0; ctrl = m land 4 <> 0 }
      | _ -> no_modifier (* Includes [], [None], etc. *)
    in

    match final_char with
    (* Cursor movement *)
    | 'A' -> Some (Key { key = Up; modifier = parse_modifiers params })
    | 'B' -> Some (Key { key = Down; modifier = parse_modifiers params })
    | 'C' -> Some (Key { key = Right; modifier = parse_modifiers params })
    | 'D' -> Some (Key { key = Left; modifier = parse_modifiers params })
    (* Home/End *)
    | 'H' -> Some (Key { key = Home; modifier = parse_modifiers params })
    | 'F' -> Some (Key { key = End; modifier = parse_modifiers params })
    (* Tab *)
    | 'Z' ->
        Some
          (Key
             {
               key = Tab;
               modifier = { shift = true; ctrl = false; alt = false };
             })
    (* Focus events *)
    | 'I' -> Some Focus
    | 'O' -> Some Blur
    (* Tilde sequences *)
    | '~' -> (
        match params with
        | [ Some 1 ] | [ Some 1; Some 1 ] ->
            Some (Key { key = Home; modifier = no_modifier })
        | [ Some 1; Some _ ] ->
            let mods = parse_modifiers params in
            Some (Key { key = Home; modifier = mods })
        | [ Some 2 ] -> Some (Key { key = Insert; modifier = no_modifier })
        | [ Some 2; Some _ ] ->
            Some (Key { key = Insert; modifier = parse_modifiers params })
        | [ Some 3 ] -> Some (Key { key = Delete; modifier = no_modifier })
        | [ Some 3; Some _ ] ->
            let mods = parse_modifiers params in
            Some (Key { key = Delete; modifier = mods })
        | [ Some 4 ] | [ Some 4; Some 1 ] ->
            Some (Key { key = End; modifier = no_modifier })
        | [ Some 4; Some _ ] ->
            let mods = parse_modifiers params in
            Some (Key { key = End; modifier = mods })
        | [ Some 5 ] -> Some (Key { key = Page_up; modifier = no_modifier })
        | [ Some 5; Some _ ] ->
            let mods = parse_modifiers params in
            Some (Key { key = Page_up; modifier = mods })
        | [ Some 6 ] -> Some (Key { key = Page_down; modifier = no_modifier })
        | [ Some 6; Some _ ] ->
            let mods = parse_modifiers params in
            Some (Key { key = Page_down; modifier = mods })
        (* Function keys *)
        | ([ Some n ] | [ Some n; Some 1 ]) when n >= 11 && n <= 24 ->
            let f =
              if n <= 15 then n - 10 else if n <= 21 then n - 11 else n - 12
            in
            Some (Key { key = F f; modifier = no_modifier })
        | [ Some n; Some _ ] when n >= 11 && n <= 24 ->
            let f =
              if n <= 15 then n - 10 else if n <= 21 then n - 11 else n - 12
            in
            let mods = parse_modifiers params in
            Some (Key { key = F f; modifier = mods })
        (* Paste mode *)
        | [ Some 200 ] -> Some Paste_start
        | [ Some 201 ] -> Some Paste_end
        | _ -> None)
    (* CSI-u format (modern keyboard protocol) *)
    | 'u' -> (
        match params with
        | [ Some 13 ] -> Some (Key { key = Enter; modifier = no_modifier })
        | [ Some 13; Some _ ] ->
            let mods = parse_modifiers params in
            Some (Key { key = Enter; modifier = mods })
        | [ Some 9 ] -> Some (Key { key = Tab; modifier = no_modifier })
        | [ Some 9; Some _ ] ->
            let mods = parse_modifiers params in
            Some (Key { key = Tab; modifier = mods })
        | [ Some 127 ] -> Some (Key { key = Backspace; modifier = no_modifier })
        | [ Some 127; Some _ ] ->
            let mods = parse_modifiers params in
            Some (Key { key = Backspace; modifier = mods })
        | [ Some 27 ] -> Some (Key { key = Escape; modifier = no_modifier })
        | [ Some 27; Some _ ] ->
            let mods = parse_modifiers params in
            Some (Key { key = Escape; modifier = mods })
        | [ Some 32 ] ->
            Some (Key { key = Char (Uchar.of_int 32); modifier = no_modifier })
        | [ Some 32; Some _ ] ->
            let mods = parse_modifiers params in
            Some (Key { key = Char (Uchar.of_int 32); modifier = mods })
        (* Handle lowercase letters with ctrl modifier - Kitty sends these as letter code with modifier 5+ *)
        | [ Some c; Some m ] when c >= 97 && c <= 122 && m >= 5 && m <= 8 ->
            (* a-z with ctrl modifier: preserve original case as per Kitty protocol *)
            let mods = parse_modifiers params in
            Some
              (Key
                 {
                   key = Char (Uchar.of_int c);
                   modifier =
                     { ctrl = true; alt = mods.alt; shift = mods.shift };
                 })
        | [ Some c ] when c >= 33 && c <= 126 ->
            Some (Key { key = Char (Uchar.of_int c); modifier = no_modifier })
        | [ Some c; Some _ ] when c >= 33 && c <= 126 ->
            let mods = parse_modifiers params in
            Some (Key { key = Char (Uchar.of_int c); modifier = mods })
        | _ -> None)
    (* Window manipulation *)
    | 't' -> (
        match params with
        | [ Some 8; Some h; Some w ] ->
            (* CSI 8 ; height ; width t reports terminal size *)
            Some (Resize (w, h))
        | _ -> None)
    (* Mouse events *)
    | '<' ->
        parse_sgr_mouse
          (String.sub s start (!intermediate_end - start + 1))
          0
          (!intermediate_end - start + 1)
    | ('M' | 'm') when start < String.length s && s.[start] = '<' ->
        (* SGR mouse format: ESC [ < params M/m *)
        parse_sgr_mouse s start (!intermediate_end + 1)
    | _ -> None
  else None

let parse_escape_sequence s start length =
  if length < 2 then None
  else if s.[start + 1] = '[' then parse_csi s (start + 2) (start + length)
  else if s.[start + 1] = 'O' && length >= 3 then
    (* SS3 sequences *)
    match s.[start + 2] with
    | 'A' -> Some (Key { key = Up; modifier = no_modifier })
    | 'B' -> Some (Key { key = Down; modifier = no_modifier })
    | 'C' -> Some (Key { key = Right; modifier = no_modifier })
    | 'D' -> Some (Key { key = Left; modifier = no_modifier })
    | 'H' -> Some (Key { key = Home; modifier = no_modifier })
    | 'F' -> Some (Key { key = End; modifier = no_modifier })
    | 'P' -> Some (Key { key = F 1; modifier = no_modifier })
    | 'Q' -> Some (Key { key = F 2; modifier = no_modifier })
    | 'R' -> Some (Key { key = F 3; modifier = no_modifier })
    | 'S' -> Some (Key { key = F 4; modifier = no_modifier })
    | _ -> None
  else if length >= 2 then
    (* Alt+char sequences *)
    let c = s.[start + 1] in
    if c >= ' ' && c <= '~' then
      Some
        (Key
           {
             key = Char (Uchar.of_int (Char.code c));
             modifier = { alt = true; ctrl = false; shift = false };
           })
    else None
  else None

let decode_utf8 bytes offset length =
  let decoder =
    Uutf.decoder ~encoding:`UTF_8
      (`String (Bytes.sub_string bytes offset length))
  in
  let rec loop acc =
    match Uutf.decode decoder with
    | `Uchar u -> loop (u :: acc)
    | `End -> List.rev acc
    | `Malformed _ -> List.rev acc
    | `Await -> List.rev acc
  in
  loop []

let feed parser bytes offset length =
  if parser.length + length > Bytes.length parser.buffer then
    failwith "Input buffer overflow";

  Bytes.blit bytes offset parser.buffer parser.length length;
  parser.length <- parser.length + length;

  let rec process_buffer acc pos =
    if pos >= parser.length then (
      if pos > 0 then (
        Bytes.blit parser.buffer pos parser.buffer 0 (parser.length - pos);
        parser.length <- parser.length - pos);
      List.rev acc)
    else
      let c = Bytes.get parser.buffer pos in
      if parser.in_paste then
        if
          pos + 6 <= parser.length
          && Bytes.sub_string parser.buffer pos 6 = "\x1b[201~"
        then (
          parser.in_paste <- false;
          Buffer.clear parser.paste_buffer;
          process_buffer (Paste_end :: acc) (pos + 6))
        else
          (* Emit individual Key events for each character in paste mode *)
          let key_event = Key { key = key_of_char c; modifier = no_modifier } in
          process_buffer (key_event :: acc) (pos + 1)
      else if c = '\x1b' then
        if
          (* Check for X10 mouse protocol first *)
          pos + 1 < parser.length
          && Bytes.get parser.buffer (pos + 1) = '['
          && pos + 2 < parser.length
          && Bytes.get parser.buffer (pos + 2) = 'M'
          && pos + 5 < parser.length
        then
          (* X10 mouse: ESC [ M btn x y *)
          match parse_x10_mouse parser.buffer (pos + 3) with
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
                    && (is_csi_final c || (i = pos + 2 && c >= 'A' && c <= 'Z'))
                  then i + 1
                  else find_end (i + 1)
              in
              let seq_end = find_end (pos + 1) in
              if seq_end > pos + 1 then
                match
                  parse_escape_sequence
                    (Bytes.to_string parser.buffer)
                    pos (seq_end - pos)
                with
                | Some Paste_start ->
                    parser.in_paste <- true;
                    process_buffer (Paste_start :: acc) seq_end
                | Some event -> process_buffer (event :: acc) seq_end
                | None ->
                    if seq_end = pos + 2 && pos + 1 < parser.length then
                      let next_char = Bytes.get parser.buffer (pos + 1) in
                      if next_char = '[' || next_char = 'O' then
                        (* Incomplete escape sequence, keep buffering *)
                        List.rev acc
                      else
                        (* ESC followed by non-sequence character *)
                        process_buffer
                          (Key { key = Escape; modifier = no_modifier } :: acc)
                          (pos + 1)
                    else process_buffer acc (pos + 1)
              else List.rev acc
        else
          let rec find_end i =
            if i >= parser.length then i
            else
              let c = Bytes.get parser.buffer i in
              if i = pos + 1 && c <> '[' && c <> 'O' then i + 1
              else if
                i > pos + 1
                && (is_csi_final c || (i = pos + 2 && c >= 'A' && c <= 'Z'))
              then i + 1
              else find_end (i + 1)
          in
          let seq_end = find_end (pos + 1) in
          if seq_end > pos + 1 then
            match
              parse_escape_sequence
                (Bytes.to_string parser.buffer)
                pos (seq_end - pos)
            with
            | Some Paste_start ->
                parser.in_paste <- true;
                process_buffer (Paste_start :: acc) seq_end
            | Some event -> process_buffer (event :: acc) seq_end
            | None ->
                if seq_end = pos + 2 && pos + 1 < parser.length then
                  let next_char = Bytes.get parser.buffer (pos + 1) in
                  if next_char = '[' || next_char = 'O' then
                    (* Incomplete escape sequence, keep buffering *)
                    List.rev acc
                  else
                    (* ESC followed by non-sequence character *)
                    process_buffer
                      (Key { key = Escape; modifier = no_modifier } :: acc)
                      (pos + 1)
                else process_buffer acc (pos + 1)
          else List.rev acc
      else if c = '\x00' then
        process_buffer
          (Key
             {
               key = Char (Uchar.of_int 32);
               modifier = { ctrl = true; alt = false; shift = false };
             }
          :: acc)
          (pos + 1)
      else if c = '\r' || c = '\n' || c = '\t' || c = '\x7f' then
        (* Handle special keys before control character processing *)
        process_buffer
          (Key { key = key_of_char c; modifier = no_modifier } :: acc)
          (pos + 1)
      else if c >= '\x01' && c <= '\x1a' then
        let ch = Char.chr (Char.code c + 64) in
        process_buffer
          (Key
             {
               key = Char (Uchar.of_char ch);
               modifier = { ctrl = true; alt = false; shift = false };
             }
          :: acc)
          (pos + 1)
      else if c = '\x1c' then
        process_buffer
          (Key
             {
               key = Char (Uchar.of_char '\\');
               modifier = { ctrl = true; alt = false; shift = false };
             }
          :: acc)
          (pos + 1)
      else if c = '\x1d' then
        process_buffer
          (Key
             {
               key = Char (Uchar.of_char ']');
               modifier = { ctrl = true; alt = false; shift = false };
             }
          :: acc)
          (pos + 1)
      else if c = '\x1e' then
        process_buffer
          (Key
             {
               key = Char (Uchar.of_char '^');
               modifier = { ctrl = true; alt = false; shift = false };
             }
          :: acc)
          (pos + 1)
      else if c = '\x1f' then
        process_buffer
          (Key
             {
               key = Char (Uchar.of_char '_');
               modifier = { ctrl = true; alt = false; shift = false };
             }
          :: acc)
          (pos + 1)
      else if c < '\x80' then
        process_buffer
          (Key { key = key_of_char c; modifier = no_modifier } :: acc)
          (pos + 1)
      else
        let rec find_utf8_end i =
          if i >= parser.length then i
          else
            let c = Bytes.get parser.buffer i in
            if c < '\x80' || c >= '\xc0' then i else find_utf8_end (i + 1)
        in
        let utf8_end = find_utf8_end (pos + 1) in
        match decode_utf8 parser.buffer pos (utf8_end - pos) with
        | [] -> process_buffer acc (pos + 1)
        | chars ->
            let events =
              List.map
                (fun u -> Key { key = Char u; modifier = no_modifier })
                chars
            in
            process_buffer (List.rev_append events acc) utf8_end
  in
  process_buffer [] 0

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

let pp_modifier fmt m =
  let mods = [] in
  let mods = if m.ctrl then "ctrl" :: mods else mods in
  let mods = if m.alt then "alt" :: mods else mods in
  let mods = if m.shift then "shift" :: mods else mods in
  match mods with
  | [] -> Format.fprintf fmt "no_modifier"
  | _ -> Format.fprintf fmt "{%s}" (String.concat "+" mods)

let pp_key_event fmt e =
  Format.fprintf fmt "{key=%a; modifier=%a}" pp_key e.key pp_modifier e.modifier

let pp_mouse_button fmt = function
  | Left -> Format.fprintf fmt "Left"
  | Middle -> Format.fprintf fmt "Middle"
  | Right -> Format.fprintf fmt "Right"
  | Wheel_up -> Format.fprintf fmt "Wheel_up"
  | Wheel_down -> Format.fprintf fmt "Wheel_down"
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
  | Press (x, y, btn, mods) ->
      Format.fprintf fmt "Press(%d,%d,%a,%a)" x y pp_mouse_button btn
        pp_modifier mods
  | Release (x, y, btn, mods) ->
      Format.fprintf fmt "Release(%d,%d,%a,%a)" x y pp_mouse_button btn
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
