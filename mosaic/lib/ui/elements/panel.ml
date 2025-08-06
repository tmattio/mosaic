(* Use qualified names instead of open to avoid name conflicts *)
module E = Element

(* Import the cells constructor from Element *)
let cells = E.cells

type align = [ `Left | `Center | `Right ]

let get_border_chars = function
  | Border.Solid -> ("┌", "─", "┐", "│", "│", "└", "─", "┘")
  | Border.Rounded -> ("╭", "─", "╮", "│", "│", "╰", "─", "╯")
  | Border.Double -> ("╔", "═", "╗", "║", "║", "╚", "═", "╝")
  | Border.Thick -> ("┏", "━", "┓", "┃", "┃", "┗", "━", "┛")
  | Border.ASCII -> ("+", "-", "+", "|", "|", "+", "-", "+")

let repeat_str s n =
  if n <= 0 then "" else String.concat "" (List.init n (fun _ -> s))

let truncate_to_width s max_w =
  let rec loop len =
    if len < 0 then ""
    else
      let sub = String.sub s 0 len in
      if String.length sub <= max_w then sub else loop (len - 1)
  in
  loop (String.length s)

let make_border_line ~width ~text_opt ~align ~left_char ~line_char ~right_char
    ~border_style ~text_style =
  let corner_left_w = String.length (left_char ^ line_char) in
  let corner_right_w = String.length (line_char ^ right_char) in
  let available_w = width - corner_left_w - corner_right_w in
  if available_w <= 0 || text_opt = None then
    let line_w = width - String.length left_char - String.length right_char in
    E.text ~style:border_style
      (left_char ^ repeat_str line_char line_w ^ right_char)
  else
    let text_content = Option.get text_opt in
    let padded_text = " " ^ text_content ^ " " in
    let truncated_text =
      if String.length padded_text > available_w then
        truncate_to_width padded_text available_w
      else padded_text
    in
    let text_w = String.length truncated_text in
    let excess_space = available_w - text_w in
    let left_fill, right_fill =
      match align with
      | `Left -> (0, excess_space)
      | `Center ->
          let half = excess_space / 2 in
          (half, excess_space - half)
      | `Right -> (excess_space, 0)
    in
    E.rich_text
      [
        (left_char ^ line_char ^ repeat_str line_char left_fill, border_style);
        (truncated_text, text_style);
        (repeat_str line_char right_fill ^ line_char ^ right_char, border_style);
      ]

let vertical_border char_str style h =
  if h <= 0 then E.spacer ~flex_grow:0.0 ()
  else
    let line = E.text ~style ~wrap:`Clip char_str in
    E.vbox ~gap:(cells 0) (List.init h (fun _ -> line))

let panel ?(box_style = Border.Rounded) ?title ?(title_align = `Center)
    ?subtitle ?(subtitle_align = `Center) ?(expand = true)
    ?(style = Style.empty) ?(border_style = Style.empty) ?width ?height ?padding
    ?(highlight = false) child =
  (* Apply default if no fg set *)
  let border_style =
    if border_style.fg = None then
      Style.(border_style ++ fg Ansi.Default ++ dim)
    else border_style
  in
  let top_left, top, top_right, left, right, bottom_left, bottom, bottom_right =
    get_border_chars box_style
  in
  let styled_child =
    if Style.equal style Style.empty then child else E.styled style child
  in
  let actual_padding = Option.value padding ~default:(E.xy 1 0) in
  let title_opt = match title with Some "" -> None | _ -> title in
  let subtitle_opt = match subtitle with Some "" -> None | _ -> subtitle in
  let title_style =
    if highlight then Style.merge border_style Style.bold else border_style
  in
  let title_min_w =
    match title_opt with
    | None -> 0
    | Some t -> String.length (" " ^ t ^ " ") + 4
  in
  let subtitle_min_w =
    match subtitle_opt with
    | None -> 0
    | Some s -> String.length (" " ^ s ^ " ") + 4
  in
  let header_min_w = max title_min_w subtitle_min_w in
  let natural_inner_w, natural_inner_h = E.measure styled_child in
  let padding_left =
    match actual_padding.left with
    | `Cells n -> n
    | `Pct _ -> 0 (* For percentages, default to 0 for width calculation *)
  in
  let padding_right =
    match actual_padding.right with `Cells n -> n | `Pct _ -> 0
  in
  let padding_top =
    match actual_padding.top with `Cells n -> n | `Pct _ -> 0
  in
  let padding_bottom =
    match actual_padding.bottom with `Cells n -> n | `Pct _ -> 0
  in
  let padded_inner_w = natural_inner_w + padding_left + padding_right in
  let _padded_inner_h = natural_inner_h + padding_top + padding_bottom in
  let required_inner_w = max padded_inner_w header_min_w in
  let panel_width = Option.value width ~default:(required_inner_w + 2) in
  let inner_width = panel_width - 2 in
  let base_content =
    E.vbox ~padding:actual_padding ~width:(`Cells inner_width) [ styled_child ]
  in
  let _, content_height = E.measure ~width:inner_width base_content in
  let inner_height =
    Option.map (fun h -> h - 2) height |> Option.value ~default:content_height
  in
  let extra = max 0 (inner_height - content_height) in
  let content_column =
    let core = base_content in
    if inner_height < content_height then
      E.scroll_view ~width:(`Cells inner_width) ~height:(`Cells inner_height)
        ~h_offset:0 ~v_offset:0 core
    else if extra > 0 then
      E.vbox ~gap:(cells 0) ~width:(`Cells inner_width)
        [
          core; E.text (if extra = 1 then "" else String.make (extra - 1) '\n');
        ]
    else core
  in
  let left_border = vertical_border left border_style inner_height in
  let right_border = vertical_border right border_style inner_height in
  let middle =
    E.hbox ~gap:(cells 0) [ left_border; content_column; right_border ]
  in
  let top_border =
    make_border_line ~width:panel_width ~text_opt:title_opt ~align:title_align
      ~left_char:top_left ~line_char:top ~right_char:top_right ~border_style
      ~text_style:title_style
  in
  let bottom_border =
    make_border_line ~width:panel_width ~text_opt:subtitle_opt
      ~align:subtitle_align ~left_char:bottom_left ~line_char:bottom
      ~right_char:bottom_right ~border_style ~text_style:title_style
  in
  let panel_elements = [ top_border; middle; bottom_border ] in
  let panel_box = E.vbox ~gap:(cells 0) panel_elements in
  if expand then E.hbox ~flex_grow:1.0 [ panel_box ] else panel_box
