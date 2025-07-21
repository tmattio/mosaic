type attr = {
  fg : Ansi.color option;
  bg : Ansi.color option;
  bold : bool;
  dim : bool;
  italic : bool;
  underline : bool;
  blink : bool;
  reverse : bool;
  strikethrough : bool;
  uri : string option; (* Hyperlink URI *)
}

let default_attr =
  {
    fg = None;
    bg = None;
    bold = false;
    dim = false;
    italic = false;
    underline = false;
    blink = false;
    reverse = false;
    strikethrough = false;
    uri = None;
  }

type cell = { chars : Uchar.t list; attr : attr; width : int }

let empty_cell =
  { chars = [ Uchar.of_int 0x20 ]; attr = default_attr; width = 1 }

type buffer = { width : int; height : int; cells : cell array }

module Clip = struct
  type t = { x : int; y : int; width : int; height : int }

  let make x y width height = { x; y; width; height }
  let x { x; _ } = x
  let y { y; _ } = y
  let width { width; _ } = width
  let height { height; _ } = height

  let intersect c1 c2 =
    let x1 = max c1.x c2.x in
    let y1 = max c1.y c2.y in
    let x2 = min (c1.x + c1.width) (c2.x + c2.width) in
    let y2 = min (c1.y + c1.height) (c2.y + c2.height) in
    if x2 > x1 && y2 > y1 then make x1 y1 (x2 - x1) (y2 - y1) else make 0 0 0 0

  let intersect_opt c1_opt c2_opt =
    match (c1_opt, c2_opt) with
    | None, c | c, None -> c
    | Some c1, Some c2 -> Some (intersect c1 c2)

  let contains { x; y; width; height } x' y' =
    x' >= x && x' < x + width && y' >= y && y' < y + height
end

let create width height =
  if width <= 0 || height <= 0 then
    invalid_arg "create: dimensions must be positive";
  { width; height; cells = Array.make (width * height) empty_cell }

let clear buffer =
  Array.fill buffer.cells 0 (Array.length buffer.cells) empty_cell

let dimensions (buffer : buffer) = (buffer.width, buffer.height)

(* Clipping functions *)

let full_clip (buffer : buffer) =
  Clip.{ x = 0; y = 0; width = buffer.width; height = buffer.height }

let index (buffer : buffer) x y = (y * buffer.width) + x

let get (buffer : buffer) x y =
  if x >= 0 && x < buffer.width && y >= 0 && y < buffer.height then
    buffer.cells.(index buffer x y)
  else empty_cell

let set ?(clip : Clip.t option) (buffer : buffer) x y cell =
  let in_bounds = x >= 0 && x < buffer.width && y >= 0 && y < buffer.height in
  let in_clip_bounds =
    match clip with None -> true | Some c -> Clip.contains c x y
  in
  if in_bounds && in_clip_bounds then buffer.cells.(index buffer x y) <- cell

let uchar_width u =
  match Uucp.Break.tty_width_hint u with
  | -1 -> 1 (* Uucp returns -1 for control characters, we treat as width 1 *)
  | n -> n

let set_char ?(clip : Clip.t option) (buffer : buffer) x y char attr =
  let width = uchar_width char in
  if width = 0 && x > 0 then
    (* Combining character - append to previous cell *)
    let prev_cell = get buffer (x - 1) y in
    set ?clip buffer (x - 1) y
      { prev_cell with chars = prev_cell.chars @ [ char ] }
  else (
    (* Regular character *)
    set ?clip buffer x y { chars = [ char ]; attr; width };
    if width = 2 && x + 1 < buffer.width then
      set ?clip buffer (x + 1) y { chars = []; attr; width = 0 })

let set_string ?(clip : Clip.t option) (buffer : buffer) x y str attr =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let rec loop x =
    let max_x =
      match clip with
      | None -> buffer.width
      | Some c -> min buffer.width (c.x + c.width)
    in
    if x >= max_x then ()
    else
      match Uutf.decode decoder with
      | `Uchar u ->
          let width = uchar_width u in
          if x + width <= max_x then (
            set_char ?clip buffer x y u attr;
            loop (x + width))
      | `End -> ()
      | `Malformed _ ->
          (* Replace malformed sequence with replacement character *)
          let replacement = Uchar.of_int 0xFFFD in
          if x < max_x then (
            set_char ?clip buffer x y replacement attr;
            loop (x + 1))
      | `Await -> () (* Should not happen with a string source *)
  in
  loop x

let set_multiline_string ?(clip : Clip.t option) (buffer : buffer) x y str attr
    =
  let lines = String.split_on_char '\n' str in
  List.iteri (fun i line -> set_string ?clip buffer x (y + i) line attr) lines;
  List.length lines

type patch =
  | Change of { row : int; col : int; new_cell : cell }
  | Clear of { row : int; col : int; width : int; height : int }

let diff (old_buffer : buffer) (new_buffer : buffer) =
  let patches = ref [] in
  let old_w, old_h = (old_buffer.width, old_buffer.height) in
  let new_w, new_h = (new_buffer.width, new_buffer.height) in
  let min_w = min old_w new_w in
  let min_h = min old_h new_h in

  (* Compare common area *)
  for y = 0 to min_h - 1 do
    for x = 0 to min_w - 1 do
      let old_cell = get old_buffer x y in
      let new_cell = get new_buffer x y in
      if old_cell <> new_cell then
        patches := Change { row = y; col = x; new_cell } :: !patches
    done
  done;

  (* Extra width in new *)
  if new_w > old_w then
    for y = 0 to min_h - 1 do
      for x = old_w to new_w - 1 do
        let new_cell = get new_buffer x y in
        patches := Change { row = y; col = x; new_cell } :: !patches
      done
    done;

  (* Extra height in new *)
  if new_h > old_h then
    for y = old_h to new_h - 1 do
      for x = 0 to new_w - 1 do
        let new_cell = get new_buffer x y in
        patches := Change { row = y; col = x; new_cell } :: !patches
      done
    done;

  (* Clear extra width in old *)
  if old_w > new_w then
    for y = 0 to min_h - 1 do
      patches :=
        Clear { row = y; col = new_w; width = old_w - new_w; height = 1 }
        :: !patches
    done;

  (* Clear extra height in old *)
  if old_h > new_h then
    patches :=
      Clear { row = new_h; col = 0; width = old_w; height = old_h - new_h }
      :: !patches;

  List.rev !patches

type cursor_pos = [ `Hide | `Move of int * int ]
type render_mode = Absolute | Relative

(* Convert attr to ANSI attributes *)
let attr_to_ansi_attrs attr =
  let attrs = ref [] in

  (* Colors *)
  (match attr.fg with Some color -> attrs := `Fg color :: !attrs | None -> ());

  (match attr.bg with Some color -> attrs := `Bg color :: !attrs | None -> ());

  (* Attributes *)
  if attr.bold then attrs := `Bold :: !attrs;
  if attr.dim then attrs := `Dim :: !attrs;
  if attr.italic then attrs := `Italic :: !attrs;
  if attr.underline then attrs := `Underline :: !attrs;
  if attr.blink then attrs := `Blink :: !attrs;
  if attr.reverse then attrs := `Reverse :: !attrs;
  if attr.strikethrough then attrs := `Strikethrough :: !attrs;

  List.rev !attrs

let render_patches ?cursor_pos ?(mode = Absolute) patches =
  let sorted =
    List.stable_sort
      (fun p1 p2 ->
        let row1, col1 =
          match p1 with Change c -> (c.row, c.col) | Clear c -> (c.row, c.col)
        in
        let row2, col2 =
          match p2 with Change c -> (c.row, c.col) | Clear c -> (c.row, c.col)
        in
        match compare row1 row2 with 0 -> compare col1 col2 | n -> n)
      patches
  in
  let buf = Buffer.create 1024 in
  let current_row = ref (-1) in
  let current_col = ref (-1) in
  let last_attr = ref default_attr in

  let move_to r c =
    match mode with
    | Absolute -> Buffer.add_string buf (Ansi.cursor_position (r + 1) (c + 1))
    | Relative ->
        let row_diff = r - !current_row in
        if row_diff > 0 then Buffer.add_string buf (Ansi.cursor_down row_diff)
        else if row_diff < 0 then
          Buffer.add_string buf (Ansi.cursor_up (-row_diff));
        if row_diff <> 0 then Buffer.add_string buf "\r";
        let col_diff = c - if row_diff <> 0 then 0 else !current_col in
        if col_diff > 0 then
          Buffer.add_string buf (Ansi.cursor_forward col_diff)
        else if col_diff < 0 then
          Buffer.add_string buf (Ansi.cursor_back (-col_diff));
        current_row := r;
        current_col := c
  in

  let apply_attr attr =
    if attr <> !last_attr then (
      Buffer.add_string buf Ansi.reset;
      let attrs = attr_to_ansi_attrs attr in
      if attrs <> [] then Buffer.add_string buf (Ansi.sgr attrs);
      last_attr := attr)
  in

  List.iter
    (fun patch ->
      match patch with
      | Change { row; col; new_cell } ->
          move_to row col;
          apply_attr new_cell.attr;
          let char_str =
            match new_cell.chars with
            | [] -> " "
            | _ ->
                let b = Buffer.create 8 in
                List.iter (Uutf.Buffer.add_utf_8 b) new_cell.chars;
                Buffer.contents b
          in
          (match new_cell.attr.uri with
          | Some uri -> Buffer.add_string buf (Ansi.hyperlink ~uri char_str)
          | None -> Buffer.add_string buf char_str);
          current_col := !current_col + new_cell.width
      | Clear { row; col; width = _; height } ->
          for dy = 0 to height - 1 do
            move_to (row + dy) col;
            Buffer.add_string buf (Ansi.erase_in_line 0)
            (* Erase to end of line *)
          done)
    sorted;

  if !last_attr <> default_attr then Buffer.add_string buf Ansi.reset;

  (match cursor_pos with
  | None -> ()
  | Some `Hide -> Buffer.add_string buf Ansi.cursor_hide
  | Some (`Move (x, y)) ->
      move_to y x;
      Buffer.add_string buf Ansi.cursor_show);

  Buffer.contents buf

let render_full ?cursor_pos ?(mode = Absolute) (buffer : buffer) =
  let buf = Buffer.create (buffer.width * buffer.height * 10) in

  (match mode with
  | Absolute ->
      Buffer.add_string buf Ansi.clear_screen;
      Buffer.add_string buf (Ansi.cursor_position 1 1)
  | Relative -> ());

  Buffer.add_string buf Ansi.reset;

  let last_attr = ref default_attr in

  let apply_attr attr =
    if attr <> !last_attr then (
      Buffer.add_string buf Ansi.reset;
      let attrs = attr_to_ansi_attrs attr in
      if attrs <> [] then Buffer.add_string buf (Ansi.sgr attrs);
      last_attr := attr)
  in

  for y = 0 to buffer.height - 1 do
    if y > 0 then Buffer.add_string buf "\n";
    Buffer.add_string buf (Ansi.cursor_position (y + 1) 1);
    for x = 0 to buffer.width - 1 do
      let cell = get buffer x y in
      if cell.width > 0 then (
        apply_attr cell.attr;
        let char_str =
          match cell.chars with
          | [] -> " "
          | chars ->
              let b = Buffer.create 8 in
              List.iter (Uutf.Buffer.add_utf_8 b) chars;
              Buffer.contents b
        in
        match cell.attr.uri with
        | Some uri -> Buffer.add_string buf (Ansi.hyperlink ~uri char_str)
        | None -> Buffer.add_string buf char_str)
    done
  done;

  Buffer.add_string buf Ansi.reset;

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
    | `Malformed _ -> loop (width + 1) (* Count replacement as 1 *)
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
    | `Malformed _ ->
        let replacement = Uchar.of_int 0xFFFD in
        if width + 1 <= max_width then (
          Uutf.Buffer.add_utf_8 buf replacement;
          loop (width + 1))
        else width
    | `Await -> width
  in
  let _ = loop 0 in
  Buffer.contents buf

(* Pad a string with spaces to reach the specified width *)
let pad_string str width =
  let str_width = measure_string str in
  if str_width >= width then str else str ^ String.make (width - str_width) ' '

let expand_tabs s tab_width =
  let rec expand_line line =
    match String.index_opt line '\t' with
    | None -> line
    | Some idx ->
        let before = String.sub line 0 idx in
        let after = String.sub line (idx + 1) (String.length line - idx - 1) in
        let col = measure_string before in
        let spaces_needed = tab_width - (col mod tab_width) in
        let spaces = String.make spaces_needed ' ' in
        expand_line (before ^ spaces ^ after)
  in
  String.split_on_char '\n' s |> List.map expand_line |> String.concat "\n"

let unicode_substring str max_cells =
  if max_cells <= 0 then ""
  else
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
    let buffer = Buffer.create (String.length str) in
    let rec loop cells =
      if cells >= max_cells then Buffer.contents buffer
      else
        match Uutf.decode decoder with
        | `Uchar u ->
            let width = max 0 (uchar_width u) in
            if cells + width > max_cells then Buffer.contents buffer
            else (
              Buffer.add_utf_8_uchar buffer u;
              loop (cells + width))
        | `End | `Await -> Buffer.contents buffer
        | `Malformed _ -> loop cells
    in
    loop 0

let truncate_string_with_ellipsis str max_width ellipsis =
  let ellipsis_width = measure_string ellipsis in
  if measure_string str <= max_width then str
  else if ellipsis_width >= max_width then truncate_string str max_width
  else truncate_string str (max_width - ellipsis_width) ^ ellipsis
