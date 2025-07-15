module Style = struct
  (* Adaptive color support *)
  type adaptive_color = { light : Ansi.color; dark : Ansi.color }

  (* Gradient type for linear color interpolation *)
  type gradient = {
    colors : Ansi.color list;
    direction : [ `Horizontal | `Vertical ];
  }

  (* Color specification: solid, adaptive, or gradient *)
  type color_spec =
    | Solid of Ansi.color
    | Adaptive of adaptive_color
    | Gradient of gradient

  type t = {
    fg : color_spec option;
    bg : color_spec option;
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

  let default =
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

  (* Attribute type for building styles from lists *)
  type attr =
    | Fg of Ansi.color
    | Bg of Ansi.color
    | Fg_gradient of gradient
    | Bg_gradient of gradient
    | Bold
    | Dim
    | Italic
    | Underline
    | Blink
    | Reverse
    | Strikethrough
    | Link of string

  let empty = default
  let fg color = { empty with fg = Some (Solid color) }
  let bg color = { empty with bg = Some (Solid color) }

  (* Gradient constructors *)
  let gradient ~colors ~direction = { colors; direction }

  let gradient_fg ~colors ~direction =
    { empty with fg = Some (Gradient { colors; direction }) }

  let gradient_bg ~colors ~direction =
    { empty with bg = Some (Gradient { colors; direction }) }

  let bold = { empty with bold = true }
  let dim = { empty with dim = true }
  let italic = { empty with italic = true }
  let underline = { empty with underline = true }
  let blink = { empty with blink = true }
  let reverse = { empty with reverse = true }
  let strikethrough = { empty with strikethrough = true }
  let link uri = { empty with uri = Some uri }

  (* Create a style from a list of attributes *)
  let of_list attrs =
    List.fold_left
      (fun style attr ->
        match attr with
        | Fg color -> { style with fg = Some (Solid color) }
        | Bg color -> { style with bg = Some (Solid color) }
        | Fg_gradient g -> { style with fg = Some (Gradient g) }
        | Bg_gradient g -> { style with bg = Some (Gradient g) }
        | Bold -> { style with bold = true }
        | Dim -> { style with dim = true }
        | Italic -> { style with italic = true }
        | Underline -> { style with underline = true }
        | Blink -> { style with blink = true }
        | Reverse -> { style with reverse = true }
        | Strikethrough -> { style with strikethrough = true }
        | Link uri -> { style with uri = Some uri })
      empty attrs

  let ( ++ ) (a : t) (b : t) : t =
    {
      fg = (match b.fg with Some _ -> b.fg | None -> a.fg);
      bg = (match b.bg with Some _ -> b.bg | None -> a.bg);
      bold = a.bold || b.bold;
      dim = a.dim || b.dim;
      italic = a.italic || b.italic;
      underline = a.underline || b.underline;
      double_underline = a.double_underline || b.double_underline;
      blink = a.blink || b.blink;
      reverse = a.reverse || b.reverse;
      strikethrough = a.strikethrough || b.strikethrough;
      overline = a.overline || b.overline;
      uri = (match b.uri with Some _ -> b.uri | None -> a.uri);
    }

  let ansi256 n = Ansi.Index n
  let rgb r g b = Ansi.RGB (r, g, b)

  (* Color type export *)
  type color = Ansi.color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Default
    | Bright_black
    | Bright_red
    | Bright_green
    | Bright_yellow
    | Bright_blue
    | Bright_magenta
    | Bright_cyan
    | Bright_white
    | Index of int (* 256-color palette (0-255) *)
    | RGB of int * int * int (* 24-bit color (0-255 each) *)

  (* Color helpers *)
  let gray n = Index (232 + min 23 (max 0 n))

  let rgb_hex hex =
    let r = (hex lsr 16) land 0xFF in
    let g = (hex lsr 8) land 0xFF in
    let b = hex land 0xFF in
    RGB (r, g, b)

  let adaptive ~light ~dark = { light; dark }

  (* Global reference for terminal background state *)
  let is_dark_background = ref true

  (* Function to update background state - called by terminal detection *)
  let set_dark_background dark = is_dark_background := dark
  let adaptive_fg color = { empty with fg = Some (Adaptive color) }
  let adaptive_bg color = { empty with bg = Some (Adaptive color) }

  (* Common adaptive colors *)
  let adaptive_primary = { light = Black; dark = White }
  let adaptive_secondary = { light = gray 8; dark = gray 15 }
  let adaptive_accent = { light = Blue; dark = Bright_blue }
  let adaptive_error = { light = Red; dark = Bright_red }
  let adaptive_warning = { light = Yellow; dark = Bright_yellow }
  let adaptive_success = { light = Green; dark = Bright_green }

  (* Color interpolation for gradients *)

  (* Convert any color to RGB for interpolation *)
  let to_rgb = function
    | RGB (r, g, b) -> (r, g, b)
    (* Basic colors to RGB - approximate values *)
    | Black -> (0, 0, 0)
    | Red -> (170, 0, 0)
    | Green -> (0, 170, 0)
    | Yellow -> (170, 170, 0)
    | Blue -> (0, 0, 170)
    | Magenta -> (170, 0, 170)
    | Cyan -> (0, 170, 170)
    | White -> (170, 170, 170)
    | Default -> (170, 170, 170) (* Assume default is like white *)
    | Bright_black -> (85, 85, 85)
    | Bright_red -> (255, 85, 85)
    | Bright_green -> (85, 255, 85)
    | Bright_yellow -> (255, 255, 85)
    | Bright_blue -> (85, 85, 255)
    | Bright_magenta -> (255, 85, 255)
    | Bright_cyan -> (85, 255, 255)
    | Bright_white -> (255, 255, 255)
    | Index i ->
        (* Convert 256-color palette to RGB - simplified *)
        if i < 16 then
          (* Standard colors - use same as above *)
          match i with
          | 0 -> (0, 0, 0)
          | 1 -> (170, 0, 0)
          | 2 -> (0, 170, 0)
          | 3 -> (170, 170, 0)
          | 4 -> (0, 0, 170)
          | 5 -> (170, 0, 170)
          | 6 -> (0, 170, 170)
          | 7 -> (170, 170, 170)
          | 8 -> (85, 85, 85)
          | 9 -> (255, 85, 85)
          | 10 -> (85, 255, 85)
          | 11 -> (255, 255, 85)
          | 12 -> (85, 85, 255)
          | 13 -> (255, 85, 255)
          | 14 -> (85, 255, 255)
          | 15 -> (255, 255, 255)
          | _ -> (0, 0, 0)
        else if i < 232 then
          (* 6x6x6 RGB cube *)
          let i = i - 16 in
          let r = i / 36 * 51 in
          let g = i / 6 mod 6 * 51 in
          let b = i mod 6 * 51 in
          (r, g, b)
        else
          (* Grayscale *)
          let v = 8 + ((i - 232) * 10) in
          (v, v, v)

  (* Linear interpolation between two values *)
  let lerp start_val end_val t =
    int_of_float
      (float_of_int start_val +. (t *. float_of_int (end_val - start_val)))

  (* Interpolate between two RGB colors *)
  let interpolate_rgb (r1, g1, b1) (r2, g2, b2) t =
    let t = max 0.0 (min 1.0 t) in
    (* Clamp t to [0, 1] *)
    RGB (lerp r1 r2 t, lerp g1 g2 t, lerp b1 b2 t)

  (* Calculate the color at position t (0.0 to 1.0) along a gradient *)
  let calculate_gradient_color colors t =
    match colors with
    | [] -> Default (* No colors, use default *)
    | [ c ] -> c (* Single color, solid fill *)
    | _ ->
        let num_segments = float_of_int (List.length colors - 1) in
        let segment_index = int_of_float (t *. num_segments) in
        let segment_index = min segment_index (List.length colors - 2) in
        let segment_start_t = float_of_int segment_index /. num_segments in

        let c1 = List.nth colors segment_index in
        let c2 = List.nth colors (segment_index + 1) in

        let segment_t =
          if num_segments > 0. then (t -. segment_start_t) *. num_segments
          else 0.0
        in

        interpolate_rgb (to_rgb c1) (to_rgb c2) segment_t
end

type cell = { chars : Uchar.t list; style : Style.t; width : int }

let empty_cell =
  { chars = [ Uchar.of_int 0x20 ]; style = Style.default; width = 1 }

(* Terminal background detection *)
let set_terminal_background ~dark = Style.set_dark_background dark

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

(* Gradient-aware rendering functions *)

(* Resolve a color_spec to a concrete color based on position *)
let resolve_color_spec spec ~x ~y ~width ~height =
  match spec with
  | None -> None
  | Some (Style.Solid color) -> Some color
  | Some (Style.Adaptive adaptive) ->
      let selected =
        if !Style.is_dark_background then adaptive.dark else adaptive.light
      in
      Some selected
  | Some (Style.Gradient gradient) ->
      let t =
        match gradient.direction with
        | `Horizontal ->
            if width <= 1 then 0.5
            else float_of_int x /. float_of_int (width - 1)
        | `Vertical ->
            if height <= 1 then 0.5
            else float_of_int y /. float_of_int (height - 1)
      in
      Some (Style.calculate_gradient_color gradient.colors t)

(* Apply gradient-aware style to a position *)
let apply_gradient_style base_style ~x ~y ~width ~height =
  let fg =
    match base_style.Style.fg with
    | None -> None
    | Some (Style.Solid _) -> base_style.Style.fg
    | Some ((Style.Adaptive _ | Style.Gradient _) as spec) ->
        resolve_color_spec (Some spec) ~x ~y ~width ~height
        |> Option.map (fun c -> Style.Solid c)
  in
  let bg =
    match base_style.Style.bg with
    | None -> None
    | Some (Style.Solid _) -> base_style.Style.bg
    | Some ((Style.Adaptive _ | Style.Gradient _) as spec) ->
        resolve_color_spec (Some spec) ~x ~y ~width ~height
        |> Option.map (fun c -> Style.Solid c)
  in
  { base_style with Style.fg; Style.bg }

(* Set a string with gradient support *)
let set_string_gradient buffer x y str style ~width ~height ~line_offset =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let start_x = x in
  let rec loop x =
    if x >= buffer.width then ()
    else
      match Uutf.decode decoder with
      | `Uchar u ->
          let width_char = uchar_width u in
          if x + width_char <= buffer.width then (
            (* Calculate style for this specific position *)
            let pos_style =
              apply_gradient_style style ~x:(x - start_x) ~y:line_offset ~width ~height
            in
            set_char buffer x y u pos_style;
            loop (x + width_char))
      | `End -> ()
      | `Malformed _ ->
          let replacement = Uchar.of_int 0xFFFD in
          if x < buffer.width then (
            let pos_style =
              apply_gradient_style style ~x:(x - start_x) ~y:line_offset ~width ~height
            in
            set_char buffer x y replacement pos_style;
            loop (x + 1))
      | `Await -> ()
  in
  loop x

(* Fill a rectangular area with gradient background *)
let fill_rect_gradient buffer x y width height style =
  for dy = 0 to height - 1 do
    for dx = 0 to width - 1 do
      let px = x + dx in
      let py = y + dy in
      if px >= 0 && px < buffer.width && py >= 0 && py < buffer.height then
        let pos_style = apply_gradient_style style ~x:dx ~y:dy ~width ~height in
        (* Set a space character with the gradient background *)
        set_char buffer px py (Uchar.of_int 0x20) pos_style
    done
  done

(* Fill a rectangular area with solid background - optimized version *)
let fill_rect buffer x y width height style =
  (* For solid colors, we can optimize by setting each cell directly *)
  let space_char = Uchar.of_int 0x20 in
  for dy = 0 to height - 1 do
    for dx = 0 to width - 1 do
      let px = x + dx in
      let py = y + dy in
      if px >= 0 && px < buffer.width && py >= 0 && py < buffer.height then
        set_char buffer px py space_char style
    done
  done

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
  let open Style in
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
  (* Extract concrete colors from color_spec *)
  (match style.bg with
  | Some (Solid c) -> attrs := `Bg c :: !attrs
  | Some (Adaptive adaptive) ->
      let c = if !is_dark_background then adaptive.dark else adaptive.light in
      attrs := `Bg c :: !attrs
  | Some (Gradient _) -> () (* Gradients should be resolved before this *)
  | None -> ());
  (match style.fg with
  | Some (Solid c) -> attrs := `Fg c :: !attrs
  | Some (Adaptive adaptive) ->
      let c = if !is_dark_background then adaptive.dark else adaptive.light in
      attrs := `Fg c :: !attrs
  | Some (Gradient _) -> () (* Gradients should be resolved before this *)
  | None -> ());
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

type render_mode = Absolute | Relative

let render_patch patch =
  let cursor_pos = Ansi.cursor_position (patch.row + 1) (patch.col + 1) in
  cursor_pos ^ render_cell patch.new_cell

let render_patches ?cursor_pos ?(mode = Absolute) patches =
  let by_row =
    List.stable_sort
      (fun p1 p2 ->
        match compare p1.row p2.row with 0 -> compare p1.col p2.col | n -> n)
      patches
  in

  let buf = Buffer.create 1024 in
  let last_style = ref Style.default in
  let current_row = ref (if mode = Relative then 0 else -1) in
  let current_col = ref (if mode = Relative then 0 else -1) in

  List.iter
    (fun patch ->
      (* Position cursor *)
      (match mode with
      | Absolute ->
          if patch.row <> !current_row || patch.col <> !current_col + 1 then
            Buffer.add_string buf
              (Ansi.cursor_position (patch.row + 1) (patch.col + 1))
      | Relative ->
          (* Move vertically if needed *)
          let row_diff = patch.row - !current_row in
          if row_diff > 0 then (
            Buffer.add_string buf (Ansi.cursor_down row_diff);
            (* When moving to a new line, always reset horizontal position *)
            Buffer.add_string buf "\r";
            current_col := 0)
          else if row_diff < 0 then (
            (* This case is less common but included for completeness *)
            Buffer.add_string buf (Ansi.cursor_up (-row_diff));
            Buffer.add_string buf "\r";
            current_col := 0);
          current_row := patch.row;

          (* Move horizontally if needed *)
          let col_diff = patch.col - !current_col in
          if col_diff > 0 then
            Buffer.add_string buf (Ansi.cursor_forward col_diff);
          current_col := patch.col);

      (* Apply style - more aggressive reset in Relative mode *)
      (match mode with
      | Absolute ->
          if patch.new_cell.style <> !last_style then (
            let attrs = style_to_attrs patch.new_cell.style in
            if attrs = [] then Buffer.add_string buf Ansi.reset
            else Buffer.add_string buf (Ansi.sgr attrs);
            last_style := patch.new_cell.style)
      | Relative ->
          (* Apply style with reset to prevent bleeding *)
          if patch.new_cell.style <> !last_style then (
            (* Always reset before applying new style to prevent bleeding attributes *)
            if !last_style <> Style.default then
              Buffer.add_string buf Ansi.reset;
            let attrs = style_to_attrs patch.new_cell.style in
            if attrs <> [] then Buffer.add_string buf (Ansi.sgr attrs);
            last_style := patch.new_cell.style));

      (* Render content *)
      let char_str =
        match patch.new_cell.chars with
        | [] -> " "
        | chars ->
            let b = Buffer.create 8 in
            List.iter (Uutf.Buffer.add_utf_8 b) chars;
            Buffer.contents b
      in

      (match patch.new_cell.style.uri with
      | Some uri -> Buffer.add_string buf (Ansi.hyperlink ~uri char_str)
      | None -> Buffer.add_string buf char_str);

      (* Update position tracking based on mode *)
      match mode with
      | Absolute ->
          current_row := patch.row;
          current_col := patch.col + patch.new_cell.width - 1
      | Relative ->
          (* In Relative mode, we already updated row and col during positioning *)
          current_col := !current_col + patch.new_cell.width)
    by_row;

  (* Reset style at the end *)
  if patches <> [] && (!last_style <> Style.default || mode = Relative) then
    Buffer.add_string buf Ansi.reset;

  (* Position cursor if requested *)
  (match cursor_pos with
  | None -> ()
  | Some `Hide -> Buffer.add_string buf Ansi.cursor_hide
  | Some (`Move (x, y)) ->
      (match mode with
      | Absolute -> Buffer.add_string buf (Ansi.cursor_position (y + 1) (x + 1))
      | Relative ->
          let row_diff = y - !current_row in
          let col_diff = x - !current_col in
          if row_diff > 0 then Buffer.add_string buf (Ansi.cursor_down row_diff)
          else if row_diff < 0 then
            Buffer.add_string buf (Ansi.cursor_up (-row_diff));
          if col_diff > 0 then
            Buffer.add_string buf (Ansi.cursor_forward col_diff)
          else if col_diff < 0 then
            Buffer.add_string buf (Ansi.cursor_back (-col_diff)));
      Buffer.add_string buf Ansi.cursor_show);

  Buffer.contents buf

let render_full ?cursor_pos ?(mode = Absolute) buffer =
  let buf = Buffer.create (buffer.width * buffer.height * 10) in

  (* Initial setup based on mode *)
  (match mode with
  | Absolute ->
      Buffer.add_string buf (Ansi.cursor_position 1 1);
      Buffer.add_string buf Ansi.clear_screen;
      Buffer.add_string buf Ansi.reset
  | Relative ->
      (* In relative mode, assume we're already at the correct position *)
      Buffer.add_string buf Ansi.reset);

  (* Ensure we start with a clean slate *)
  let last_style = ref Style.default in

  for y = 0 to buffer.height - 1 do
    if y > 0 then Buffer.add_char buf '\n';
    for x = 0 to buffer.width - 1 do
      let cell = get buffer x y in
      if cell.width > 0 then (
        (* Style handling - more aggressive in Relative mode *)
        (match mode with
        | Absolute ->
            if cell.style <> !last_style then (
              let attrs = style_to_attrs cell.style in
              if attrs = [] then Buffer.add_string buf Ansi.reset
              else Buffer.add_string buf (Ansi.sgr attrs);
              last_style := cell.style)
        | Relative ->
            (* Always reset before styled cells to prevent bleeding *)
            if cell.style <> Style.default then (
              Buffer.add_string buf Ansi.reset;
              let attrs = style_to_attrs cell.style in
              Buffer.add_string buf (Ansi.sgr attrs);
              last_style := cell.style)
            else if !last_style <> Style.default then (
              Buffer.add_string buf Ansi.reset;
              last_style := Style.default));

        let char_str =
          match cell.chars with
          | [ c ] when Uchar.to_int c = 0x20 -> " "
          | [] -> " "
          | chars ->
              let b = Buffer.create 8 in
              List.iter (Uutf.Buffer.add_utf_8 b) chars;
              Buffer.contents b
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
      (match mode with
      | Absolute -> Buffer.add_string buf (Ansi.cursor_position (y + 1) (x + 1))
      | Relative ->
          (* In relative mode, we're at the bottom right, so move to target *)
          let row_diff = y - (buffer.height - 1) in
          let col_diff = x - (buffer.width - 1) in
          if row_diff > 0 then Buffer.add_string buf (Ansi.cursor_down row_diff)
          else if row_diff < 0 then
            Buffer.add_string buf (Ansi.cursor_up (-row_diff));
          if col_diff > 0 then
            Buffer.add_string buf (Ansi.cursor_forward col_diff)
          else if col_diff < 0 then
            Buffer.add_string buf (Ansi.cursor_back (-col_diff)));
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
