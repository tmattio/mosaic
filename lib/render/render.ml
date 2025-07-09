type style = {
  fg : Ansi.color option;
  bg : Ansi.color option;
  bold : bool;
  dim : bool;
  italic : bool;
  underline : bool;
  double_underline : bool;
  blink : bool;
  reverse : bool;
  strikethrough : bool;
  overline : bool;
  uri : string option;
}

let default_style =
  {
    fg = None;
    bg = None;
    bold = false;
    dim = false;
    italic = false;
    underline = false;
    double_underline = false;
    blink = false;
    reverse = false;
    strikethrough = false;
    overline = false;
    uri = None;
  }

type cell = { chars : Uchar.t list; style : style; width : int }

let empty_cell =
  { chars = [ Uchar.of_int 0x20 ]; style = default_style; width = 1 }

type buffer = { width : int; height : int; cells : cell array }

let create width height =
  { width; height; cells = Array.make (width * height) empty_cell }

let clear buffer =
  Array.fill buffer.cells 0 (Array.length buffer.cells) empty_cell

let dimensions buffer = (buffer.width, buffer.height)
let index buffer x y = (y * buffer.width) + x

let get buffer x y =
  if x >= 0 && x < buffer.width && y >= 0 && y < buffer.height then
    buffer.cells.(index buffer x y)
  else empty_cell

let set buffer x y cell =
  if x >= 0 && x < buffer.width && y >= 0 && y < buffer.height then
    buffer.cells.(index buffer x y) <- cell

let uchar_width u =
  match Uucp.Break.tty_width_hint u with
  | -1 -> 1 (* Uucp returns -1 for control characters, we treat as width 1 *)
  | n -> n

let set_char buffer x y char style =
  let width = uchar_width char in
  if width = 0 && x > 0 then
    (* Combining character - append to previous cell *)
    let prev_cell = get buffer (x - 1) y in
    set buffer (x - 1) y { prev_cell with chars = prev_cell.chars @ [ char ] }
  else (
    (* Regular character *)
    set buffer x y { chars = [ char ]; style; width };
    if width = 2 && x + 1 < buffer.width then
      set buffer (x + 1) y { chars = []; style; width = 0 })

let set_string buffer x y str style =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let rec loop x =
    if x >= buffer.width then ()
    else
      match Uutf.decode decoder with
      | `Uchar u ->
          let width = uchar_width u in
          if x + width <= buffer.width then (
            set_char buffer x y u style;
            loop (x + width))
      | `End -> ()
      | `Malformed _ ->
          (* Replace malformed sequence with replacement character *)
          let replacement = Uchar.of_int 0xFFFD in
          if x < buffer.width then (
            set_char buffer x y replacement style;
            loop (x + 1))
      | `Await -> () (* Should not happen with a string source *)
  in
  loop x

type patch = { row : int; col : int; old_cell : cell; new_cell : cell }
type cursor_pos = [ `Hide | `Move of int * int ]

let diff old_buffer new_buffer =
  if
    old_buffer.width <> new_buffer.width
    || old_buffer.height <> new_buffer.height
  then
    invalid_arg
      (Printf.sprintf "Buffer dimensions must match: old=(%d,%d), new=(%d,%d)"
         old_buffer.width old_buffer.height new_buffer.width new_buffer.height);

  let patches = ref [] in
  for y = 0 to new_buffer.height - 1 do
    for x = 0 to new_buffer.width - 1 do
      let idx = index new_buffer x y in
      let old_cell = old_buffer.cells.(idx) in
      let new_cell = new_buffer.cells.(idx) in
      if old_cell <> new_cell then
        patches := { row = y; col = x; old_cell; new_cell } :: !patches
    done
  done;
  List.rev !patches

let style_to_attrs style =
  let attrs = ref [] in
  if style.overline then attrs := `Overline :: !attrs;
  if style.strikethrough then attrs := `Strikethrough :: !attrs;
  if style.reverse then attrs := `Reverse :: !attrs;
  if style.blink then attrs := `Blink :: !attrs;
  if style.double_underline then attrs := `Double_underline :: !attrs
  else if style.underline then attrs := `Underline :: !attrs;
  if style.italic then attrs := `Italic :: !attrs;
  if style.dim then attrs := `Dim :: !attrs;
  if style.bold then attrs := `Bold :: !attrs;
  (match style.bg with Some c -> attrs := `Bg c :: !attrs | None -> ());
  (match style.fg with Some c -> attrs := `Fg c :: !attrs | None -> ());
  !attrs

let render_cell cell =
  let attrs = style_to_attrs cell.style in
  let sgr = Ansi.sgr attrs in
  let char_str =
    match cell.chars with
    | [] -> " "
    | chars ->
        let buf = Buffer.create 8 in
        List.iter (Uutf.Buffer.add_utf_8 buf) chars;
        Buffer.contents buf
  in
  match cell.style.uri with
  | Some uri -> sgr ^ Ansi.hyperlink ~uri char_str
  | None -> sgr ^ char_str

let render_patch patch =
  let cursor_pos = Ansi.cursor_position (patch.row + 1) (patch.col + 1) in
  cursor_pos ^ render_cell patch.new_cell

let render_patches ?cursor_pos patches =
  let by_row =
    List.stable_sort
      (fun p1 p2 ->
        match compare p1.row p2.row with 0 -> compare p1.col p2.col | n -> n)
      patches
  in

  let buf = Buffer.create 1024 in
  let last_style = ref default_style in
  let last_row = ref (-1) in
  let last_col = ref (-1) in

  List.iter
    (fun patch ->
      if patch.row <> !last_row || patch.col <> !last_col + 1 then
        Buffer.add_string buf
          (Ansi.cursor_position (patch.row + 1) (patch.col + 1));

      if patch.new_cell.style <> !last_style then (
        let attrs = style_to_attrs patch.new_cell.style in
        if attrs = [] then Buffer.add_string buf Ansi.reset
        else Buffer.add_string buf (Ansi.sgr attrs);
        last_style := patch.new_cell.style);

      let char_str =
        match patch.new_cell.chars with
        | [] -> " "
        | chars ->
            let buf = Buffer.create 8 in
            List.iter (Uutf.Buffer.add_utf_8 buf) chars;
            Buffer.contents buf
      in

      (match patch.new_cell.style.uri with
      | Some uri -> Buffer.add_string buf (Ansi.hyperlink ~uri char_str)
      | None -> Buffer.add_string buf char_str);

      last_row := patch.row;
      last_col := patch.col + patch.new_cell.width - 1)
    by_row;

  (* Only add reset if we actually rendered something *)
  if patches <> [] then Buffer.add_string buf Ansi.reset;

  (* Position cursor if requested *)
  (match cursor_pos with
  | None -> ()
  | Some `Hide -> Buffer.add_string buf Ansi.cursor_hide
  | Some (`Move (x, y)) ->
      Buffer.add_string buf (Ansi.cursor_position (y + 1) (x + 1));
      Buffer.add_string buf Ansi.cursor_show);

  Buffer.contents buf

let render_full ?cursor_pos buffer =
  let buf = Buffer.create (buffer.width * buffer.height * 10) in
  Buffer.add_string buf (Ansi.cursor_position 1 1);
  Buffer.add_string buf Ansi.clear_screen;
  Buffer.add_string buf Ansi.reset;

  (* Ensure we start with a clean slate *)
  let last_style = ref default_style in

  for y = 0 to buffer.height - 1 do
    if y > 0 then Buffer.add_char buf '\n';
    for x = 0 to buffer.width - 1 do
      let cell = get buffer x y in
      if cell.width > 0 then (
        if cell.style <> !last_style then (
          let attrs = style_to_attrs cell.style in
          if attrs = [] then Buffer.add_string buf Ansi.reset
          else Buffer.add_string buf (Ansi.sgr attrs);
          last_style := cell.style);
        let char_str =
          match cell.chars with
          | [ c ] when Uchar.to_int c = 0x20 -> " "
          | [] -> " "
          | chars ->
              let buf = Buffer.create 8 in
              List.iter (Uutf.Buffer.add_utf_8 buf) chars;
              Buffer.contents buf
        in
        match cell.style.uri with
        | Some uri -> Buffer.add_string buf (Ansi.hyperlink ~uri char_str)
        | None -> Buffer.add_string buf char_str)
    done
  done;

  Buffer.add_string buf Ansi.reset;

  (* Position cursor if requested *)
  (match cursor_pos with
  | None -> ()
  | Some `Hide -> Buffer.add_string buf Ansi.cursor_hide
  | Some (`Move (x, y)) ->
      Buffer.add_string buf (Ansi.cursor_position (y + 1) (x + 1));
      Buffer.add_string buf Ansi.cursor_show);

  Buffer.contents buf

let measure_string str =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let rec loop width =
    match Uutf.decode decoder with
    | `Uchar u -> loop (width + uchar_width u)
    | `End -> width
    | `Malformed _ -> loop width
    | `Await -> width
  in
  loop 0

let truncate_string str max_width =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let buf = Buffer.create (String.length str) in
  let rec loop width =
    match Uutf.decode decoder with
    | `Uchar u ->
        let w = uchar_width u in
        if width + w <= max_width then (
          Uutf.Buffer.add_utf_8 buf u;
          loop (width + w))
        else width
    | `End -> width
    | `Malformed _ -> loop width
    | `Await -> width
  in
  let _ = loop 0 in
  Buffer.contents buf

(* Pad a string with spaces to reach the specified width *)
let pad_string str width =
  let str_width = measure_string str in
  if str_width >= width then str else str ^ String.make (width - str_width) ' '

(* Truncate a string with ellipsis if it exceeds the specified width *)
let truncate_string_with_ellipsis str max_width ellipsis =
  let str_width = measure_string str in
  if str_width <= max_width then str
  else
    let ellipsis_width = measure_string ellipsis in
    if ellipsis_width >= max_width then truncate_string str max_width
    else
      let truncated = truncate_string str (max_width - ellipsis_width) in
      truncated ^ ellipsis
