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
  match parse_csi_params s start (end_ - 1) with
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
      | [] | [ None ] -> no_modifier
      | [ Some 1; Some m ] | [ _; Some m ] ->
          let m = m - 1 in
          { shift = m land 1 <> 0; alt = m land 2 <> 0; ctrl = m land 4 <> 0 }
      | _ -> no_modifier
    in

    match final_char with
    (* Cursor movement *)
    | 'A' ->
        let mods = parse_modifiers params in
        if mods.ctrl && mods.shift then
          Some
            (Key
               {
                 key = Up;
                 modifier = { ctrl = true; shift = true; alt = false };
               })
        else if mods.ctrl then
          Some
            (Key
               {
                 key = Up;
                 modifier = { ctrl = true; shift = false; alt = false };
               })
        else if mods.shift then
          Some
            (Key
               {
                 key = Up;
                 modifier = { shift = true; ctrl = false; alt = false };
               })
        else if mods.alt then
          Some
            (Key
               {
                 key = Up;
                 modifier = { alt = true; ctrl = false; shift = false };
               })
        else Some (Key { key = Up; modifier = no_modifier })
    | 'B' ->
        let mods = parse_modifiers params in
        if mods.ctrl && mods.shift then
          Some
            (Key
               {
                 key = Down;
                 modifier = { ctrl = true; shift = true; alt = false };
               })
        else if mods.ctrl then
          Some
            (Key
               {
                 key = Down;
                 modifier = { ctrl = true; shift = false; alt = false };
               })
        else if mods.shift then
          Some
            (Key
               {
                 key = Down;
                 modifier = { shift = true; ctrl = false; alt = false };
               })
        else if mods.alt then
          Some
            (Key
               {
                 key = Down;
                 modifier = { alt = true; ctrl = false; shift = false };
               })
        else Some (Key { key = Down; modifier = no_modifier })
    | 'C' ->
        let mods = parse_modifiers params in
        if mods.ctrl && mods.shift then
          Some
            (Key
               {
                 key = Right;
                 modifier = { ctrl = true; shift = true; alt = false };
               })
        else if mods.ctrl then
          Some
            (Key
               {
                 key = Right;
                 modifier = { ctrl = true; shift = false; alt = false };
               })
        else if mods.shift then
          Some
            (Key
               {
                 key = Right;
                 modifier = { shift = true; ctrl = false; alt = false };
               })
        else if mods.alt then
          Some
            (Key
               {
                 key = Right;
                 modifier = { alt = true; ctrl = false; shift = false };
               })
        else Some (Key { key = Right; modifier = no_modifier })
    | 'D' ->
        let mods = parse_modifiers params in
        if mods.ctrl && mods.shift then
          Some
            (Key
               {
                 key = Left;
                 modifier = { ctrl = true; shift = true; alt = false };
               })
        else if mods.ctrl then
          Some
            (Key
               {
                 key = Left;
                 modifier = { ctrl = true; shift = false; alt = false };
               })
        else if mods.shift then
          Some
            (Key
               {
                 key = Left;
                 modifier = { shift = true; ctrl = false; alt = false };
               })
        else if mods.alt then
          Some
            (Key
               {
                 key = Left;
                 modifier = { alt = true; ctrl = false; shift = false };
               })
        else Some (Key { key = Left; modifier = no_modifier })
    (* Home/End *)
    | 'H' ->
        let mods = parse_modifiers params in
        if mods.ctrl then
          Some
            (Key
               {
                 key = Home;
                 modifier = { ctrl = true; shift = false; alt = false };
               })
        else if mods.shift then
          Some
            (Key
               {
                 key = Home;
                 modifier = { shift = true; ctrl = false; alt = false };
               })
        else Some (Key { key = Home; modifier = no_modifier })
    | 'F' ->
        let mods = parse_modifiers params in
        if mods.ctrl then
          Some
            (Key
               {
                 key = End;
                 modifier = { ctrl = true; shift = false; alt = false };
               })
        else if mods.shift then
          Some
            (Key
               {
                 key = End;
                 modifier = { shift = true; ctrl = false; alt = false };
               })
        else Some (Key { key = End; modifier = no_modifier })
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
        | [ Some 1; Some m ] ->
            let mods = parse_modifiers [ Some 1; Some m ] in
            if mods.ctrl then
              Some
                (Key
                   {
                     key = Home;
                     modifier = { ctrl = true; shift = false; alt = false };
                   })
            else if mods.shift then
              Some
                (Key
                   {
                     key = Home;
                     modifier = { shift = true; ctrl = false; alt = false };
                   })
            else Some (Key { key = Home; modifier = no_modifier })
        | [ Some 2 ] -> Some (Key { key = Insert; modifier = no_modifier })
        | [ Some 2; Some m ] ->
            let mods = parse_modifiers [ Some 2; Some m ] in
            Some (Key { key = Insert; modifier = mods })
        | [ Some 3 ] -> Some (Key { key = Delete; modifier = no_modifier })
        | [ Some 3; Some m ] ->
            let mods = parse_modifiers [ Some 3; Some m ] in
            Some (Key { key = Delete; modifier = mods })
        | [ Some 4 ] | [ Some 4; Some 1 ] ->
            Some (Key { key = End; modifier = no_modifier })
        | [ Some 4; Some m ] ->
            let mods = parse_modifiers [ Some 4; Some m ] in
            if mods.ctrl then
              Some
                (Key
                   {
                     key = End;
                     modifier = { ctrl = true; shift = false; alt = false };
                   })
            else if mods.shift then
              Some
                (Key
                   {
                     key = End;
                     modifier = { shift = true; ctrl = false; alt = false };
                   })
            else Some (Key { key = End; modifier = no_modifier })
        | [ Some 5 ] -> Some (Key { key = Page_up; modifier = no_modifier })
        | [ Some 5; Some m ] ->
            let mods = parse_modifiers [ Some 5; Some m ] in
            if mods.ctrl then
              Some
                (Key
                   {
                     key = Page_up;
                     modifier = { ctrl = true; shift = false; alt = false };
                   })
            else Some (Key { key = Page_up; modifier = no_modifier })
        | [ Some 6 ] -> Some (Key { key = Page_down; modifier = no_modifier })
        | [ Some 6; Some m ] ->
            let mods = parse_modifiers [ Some 6; Some m ] in
            if mods.ctrl then
              Some
                (Key
                   {
                     key = Page_down;
                     modifier = { ctrl = true; shift = false; alt = false };
                   })
            else Some (Key { key = Page_down; modifier = no_modifier })
        (* Function keys *)
        | [ Some n ] when n >= 11 && n <= 15 ->
            Some (Key { key = F (n - 10); modifier = no_modifier })
        | [ Some n ] when n >= 17 && n <= 21 ->
            Some (Key { key = F (n - 11); modifier = no_modifier })
        | [ Some n ] when n >= 23 && n <= 24 ->
            Some (Key { key = F (n - 12); modifier = no_modifier })
        | [ Some n; Some 2 ] when n >= 11 && n <= 24 ->
            let f =
              if n <= 15 then n - 10 else if n <= 21 then n - 11 else n - 12
            in
            Some
              (Key
                 {
                   key = F f;
                   modifier = { shift = true; ctrl = false; alt = false };
                 })
        | [ Some n; Some 5 ] when n >= 11 && n <= 24 ->
            let f =
              if n <= 15 then n - 10 else if n <= 21 then n - 11 else n - 12
            in
            Some
              (Key
                 {
                   key = F f;
                   modifier = { ctrl = true; shift = false; alt = false };
                 })
        | [ Some n; Some 3 ] when n >= 11 && n <= 24 ->
            let f =
              if n <= 15 then n - 10 else if n <= 21 then n - 11 else n - 12
            in
            Some
              (Key
                 {
                   key = F f;
                   modifier = { alt = true; shift = false; ctrl = false };
                 })
        (* Paste mode *)
        | [ Some 200 ] -> Some Paste_start
        | [ Some 201 ] -> Some Paste_end
        | _ -> None)
    (* Mouse events *)
    | '<' ->
        parse_sgr_mouse
          (String.sub s start (!intermediate_end - start + 1))
          0
          (!intermediate_end - start + 1)
    | ('M' | 'm') when !params_end = start ->
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
          let paste_content = Buffer.contents parser.paste_buffer in
          Buffer.clear parser.paste_buffer;
          process_buffer (Paste paste_content :: Paste_end :: acc) (pos + 6))
        else (
          Buffer.add_char parser.paste_buffer c;
          process_buffer acc (pos + 1))
      else if c = '\x1b' then
        (* Check for X10 mouse protocol first *)
        if
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
                  if is_csi_final c || (i = pos + 2 && c >= 'A' && c <= 'Z')
                  then i + 1
                  else if i = pos + 1 && c <> '[' && c <> 'O' then i + 1
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
                    if seq_end = pos + 2 then
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
              if is_csi_final c || (i = pos + 2 && c >= 'A' && c <= 'Z') then
                i + 1
              else if i = pos + 1 && c <> '[' && c <> 'O' then i + 1
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
                if seq_end = pos + 2 then
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
