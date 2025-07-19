module Padding = struct
  type t = { top : int; right : int; bottom : int; left : int }

  let make ?(top = 0) ?(right = 0) ?(bottom = 0) ?(left = 0) () =
    {
      top = max 0 top;
      right = max 0 right;
      bottom = max 0 bottom;
      left = max 0 left;
    }

  let no_padding = make ()
  let all n = { top = n; right = n; bottom = n; left = n }
  let xy x y = { top = y; right = x; bottom = y; left = x }
  let top p = p.top
  let right p = p.right
  let bottom p = p.bottom
  let left p = p.left

  let pad ?all ?x ?y ?top ?right ?bottom ?left () =
    let base_val = Option.value all ~default:0 in
    let h_val = Option.value x ~default:base_val in
    let v_val = Option.value y ~default:base_val in

    {
      top = max 0 (Option.value top ~default:v_val);
      right = max 0 (Option.value right ~default:h_val);
      bottom = max 0 (Option.value bottom ~default:v_val);
      left = max 0 (Option.value left ~default:h_val);
    }
end

type align = [ `Start | `Center | `End | `Stretch ]
type size_def = [ `Fixed of int | `Flex of int ]

(* These are the internal record types. They are not exposed in the mli. *)

type text_data = {
  content : string;
  style : Render.Style.t;
  align : align;
  tab_width : int;
  wrap : bool;
}

type rich_text_data = { segments : (string * Render.Style.t) list }
type spacer_data = { size : int; flex : int }

type box_layout_options = {
  direction : [ `Horizontal | `Vertical ];
  gap : int;
  width : int option;
  height : int option;
  min_width : int option;
  min_height : int option;
  max_width : int option;
  max_height : int option;
  margin : Padding.t;
  padding : Padding.t;
  border : Border.t option;
  background : Render.Style.t option;
  align : align;
  justify : align;
  flex_grow : int;
  flex_shrink : int;
  fill : bool;
}

type z_align =
  | Top_left
  | Top
  | Top_right
  | Left
  | Center
  | Right
  | Bottom_left
  | Bottom
  | Bottom_right

type box_data = { children : t list; options : box_layout_options }
and z_stack_data = { children : t list; align : z_align }
and flow_data = { children : t list; h_gap : int; v_gap : int }

and grid_data = {
  children : t list;
  columns : size_def list;
  rows : size_def list;
  col_spacing : int;
  row_spacing : int;
}

and scroll_data = {
  child : t;
  width : int option;
  height : int option;
  h_offset : int;
  v_offset : int;
}

and t =
  | Text of text_data
  | Rich_text of rich_text_data
  | Spacer of spacer_data
  | Box of box_data
  | Z_stack of z_stack_data
  | Flow of flow_data
  | Grid of grid_data
  | Scroll of scroll_data

module Text = struct
  type nonrec t = text_data

  let make ?(style = Render.Style.empty) ?(align = `Start) ?(tab_width = 4)
      ?(wrap = false) content =
    { content; style; align; tab_width; wrap }

  let content t = t.content
  let style t = t.style
  let alignment (t : t) = t.align
  let tab_width t = t.tab_width
  let is_wrapping t = t.wrap
end

module Rich_text = struct
  type nonrec t = rich_text_data

  let make segments = { segments }
  let segments t = t.segments
end

module Spacer = struct
  type nonrec t = spacer_data

  let make ?(flex = 0) size = { size; flex }
  let size t = t.size
  let flex t = t.flex
end

module Box = struct
  type nonrec t = box_data
  type direction = [ `Horizontal | `Vertical ]

  type layout_options = box_layout_options = {
    direction : direction;
    gap : int;
    width : int option;
    height : int option;
    min_width : int option;
    min_height : int option;
    max_width : int option;
    max_height : int option;
    margin : Padding.t;
    padding : Padding.t;
    border : Border.t option;
    background : Render.Style.t option;
    align : align;
    justify : align;
    flex_grow : int;
    flex_shrink : int;
    fill : bool;
  }

  let make ~options children = { options; children }
  let children (t : t) = t.children
  let options t = t.options
end

module Z_stack = struct
  type nonrec t = z_stack_data

  type nonrec z_align = z_align =
    | Top_left
    | Top
    | Top_right
    | Left
    | Center
    | Right
    | Bottom_left
    | Bottom
    | Bottom_right

  let make ?(align = Top_left) children = { children; align }
  let children (t : t) = t.children
  let alignment t = t.align
end

module Flow = struct
  type nonrec t = flow_data

  let make ?(h_gap = 1) ?(v_gap = 0) children = { children; h_gap; v_gap }
  let children (t : t) = t.children
  let h_gap t = t.h_gap
  let v_gap t = t.v_gap
end

module Grid = struct
  type nonrec t = grid_data

  let make ?(col_spacing = 0) ?(row_spacing = 0) ~columns ~rows children =
    { children; columns; rows; col_spacing; row_spacing }

  let children (t : t) = t.children
  let columns t = t.columns
  let rows t = t.rows
  let col_spacing t = t.col_spacing
  let row_spacing t = t.row_spacing
end

module Scroll = struct
  type nonrec t = scroll_data

  let make ?width ?height ?(h_offset = 0) ?(v_offset = 0) child =
    { child; width; height; h_offset; v_offset }

  let child t = t.child
  let width t = t.width
  let height t = t.height
  let h_offset t = t.h_offset
  let v_offset t = t.v_offset
end

module T = struct
  type nonrec t = t =
    | Text of Text.t
    | Rich_text of Rich_text.t
    | Spacer of Spacer.t
    | Box of Box.t
    | Z_stack of Z_stack.t
    | Flow of Flow.t
    | Grid of Grid.t
    | Scroll of Scroll.t
end

let border_opt_h border =
  Option.map Border.space_h border |> Option.value ~default:0

let border_opt_v border =
  Option.map Border.space_v border |> Option.value ~default:0

let rec measure ?(width = max_int) element =
  let max_reasonable = 10_000 in
  let width = min width max_reasonable in
  let clamp_result (w, h) = (min w max_reasonable, min h max_reasonable) in
  let result =
    match element with
    | Text { content; tab_width; wrap; _ } ->
        let expanded = Graphics.expand_tabs content tab_width in
        let lines =
          if wrap && width < max_int then Graphics.wrap_text expanded width
          else String.split_on_char '\n' expanded
        in
        let max_width =
          List.fold_left
            (fun acc line -> max acc (Render.measure_string line))
            0 lines
        in
        (max_width, max 1 (List.length lines))
    | Rich_text { segments } ->
        ( List.fold_left
            (fun acc (s, _) -> acc + Render.measure_string s)
            0 segments,
          1 )
    | Spacer { size; _ } -> (max 0 size, 0)
    | Box { children; options } ->
        let border_h = border_opt_h options.border in
        let border_v = border_opt_v options.border in
        let padding_h = options.padding.left + options.padding.right in
        let padding_v = options.padding.top + options.padding.bottom in
        let content_width = max 0 (width - border_h - padding_h) in

        let children_sizes =
          List.map
            (fun child ->
              let child_width =
                match options.direction with
                | `Vertical -> content_width
                | `Horizontal -> (
                    (* For wrapped boxes containing flow, constrain width *)
                    match (child, options.width) with
                    | Flow _, Some _ -> content_width
                    | _ -> max_int)
              in
              measure ~width:child_width child)
            children
        in

        let natural_w, natural_h =
          match options.direction with
          | `Horizontal ->
              let total_width =
                List.fold_left (fun acc (w, _) -> acc + w) 0 children_sizes
              in
              let max_height =
                List.fold_left (fun acc (_, h) -> max acc h) 0 children_sizes
              in
              let gap_space = options.gap * max 0 (List.length children - 1) in
              ( total_width + gap_space + padding_h + border_h,
                max_height + padding_v + border_v )
          | `Vertical ->
              let max_width =
                List.fold_left (fun acc (w, _) -> max acc w) 0 children_sizes
              in
              let total_height =
                List.fold_left (fun acc (_, h) -> acc + h) 0 children_sizes
              in
              let gap_space = options.gap * max 0 (List.length children - 1) in
              ( max_width + padding_h + border_h,
                total_height + gap_space + padding_v + border_v )
        in
        let resolve_dim value min_v max_v natural =
          let max_reasonable = 10_000 in
          let resolved =
            Option.value value ~default:natural |> min max_reasonable
          in
          let with_min =
            match min_v with Some m -> max m resolved | None -> resolved
          in
          match max_v with Some m -> min m with_min | None -> with_min
        in
        ( resolve_dim options.width options.min_width options.max_width natural_w,
          resolve_dim options.height options.min_height options.max_height
            natural_h )
    | Z_stack { children; _ } ->
        List.fold_left
          (fun (max_w, max_h) child ->
            let w, h = measure ~width child in
            (max max_w w, max max_h h))
          (0, 0) children
    | Flow { children; h_gap; v_gap; _ } ->
        if width = max_int then (* Unconstrained width, just sum horizontally *)
          let total_w =
            List.fold_left (fun acc c -> acc + fst (measure c)) 0 children
          in
          let max_h =
            List.fold_left (fun acc c -> max acc (snd (measure c))) 0 children
          in
          (total_w + (h_gap * max 0 (List.length children - 1)), max_h)
        else (* Constrained width, simulate wrapping *)
          let measured =
            List.map (fun c -> measure ~width:max_int c) children
          in
          let rec simulate_wrap current_x current_h total_h rem_measured =
            match rem_measured with
            | [] -> if current_x > 0 then total_h + current_h else total_h
            | (cw, ch) :: rest ->
                let gap = if current_x > 0 then h_gap else 0 in
                if current_x + gap + cw > width && current_x > 0 then
                  let new_total_h =
                    total_h + current_h + if total_h > 0 then v_gap else 0
                  in
                  simulate_wrap cw ch new_total_h rest
                else
                  simulate_wrap
                    (current_x + gap + cw)
                    (max current_h ch) total_h rest
          in
          (width, simulate_wrap 0 0 0 measured)
    | Grid { columns; rows; col_spacing; row_spacing; _ } ->
        let fixed_width =
          List.fold_left
            (fun acc col -> match col with `Fixed w -> acc + w | _ -> acc)
            0 columns
        in
        let fixed_height =
          List.fold_left
            (fun acc row -> match row with `Fixed h -> acc + h | _ -> acc)
            0 rows
        in
        let col_gaps = col_spacing * max 0 (List.length columns - 1) in
        let row_gaps = row_spacing * max 0 (List.length rows - 1) in
        (fixed_width + col_gaps, fixed_height + row_gaps)
    | Scroll { child; width = scroll_w; height = scroll_h; _ } ->
        let child_w, child_h = measure child in
        ( Option.value scroll_w ~default:child_w,
          Option.value scroll_h ~default:child_h )
  in
  clamp_result result

and min_width element =
  match element with
  | Text { content; wrap; tab_width; _ } ->
      let expanded = Graphics.expand_tabs content tab_width in
      if not wrap then Render.measure_string expanded
      else
        let lines = String.split_on_char '\n' expanded in
        List.fold_left
          (fun acc line ->
            let words =
              String.split_on_char ' ' line
              |> List.map String.trim
              |> List.filter (( <> ) "")
            in
            let longest_word =
              List.fold_left
                (fun acc w -> max acc (Render.measure_string w))
                0 words
            in
            max acc longest_word)
          0 lines
        |> max 1
  | Box { options; children; _ } ->
      let pad_h = options.padding.left + options.padding.right in
      let bor_h = border_opt_h options.border in
      let child_min_w =
        match options.direction with
        | `Horizontal ->
            List.fold_left
              (fun acc c -> acc + min_width c)
              (options.gap * max 0 (List.length children - 1))
              children
        | `Vertical ->
            List.fold_left (fun acc c -> max acc (min_width c)) 0 children
      in
      child_min_w + pad_h + bor_h
  | _ -> fst (measure element)

and min_height element =
  match element with
  | Text { content; _ } ->
      String.split_on_char '\n' content |> List.length |> max 1
  | Spacer _ -> 0
  | Box { options; children; _ } ->
      let pad_v = options.padding.top + options.padding.bottom in
      let bor_v = border_opt_v options.border in
      let child_min_h =
        match options.direction with
        | `Vertical ->
            List.fold_left
              (fun acc c -> acc + min_height c)
              (options.gap * max 0 (List.length children - 1))
              children
        | `Horizontal ->
            List.fold_left (fun acc c -> max acc (min_height c)) 0 children
      in
      child_min_h + pad_v + bor_v
  | _ -> snd (measure element)

let grow_fact = function
  | Spacer { flex; _ } -> flex
  | Box { options; _ } -> options.flex_grow
  | _ -> 0

let shrink_fact = function
  | Box { options; _ } -> options.flex_shrink
  | Text { wrap = true; _ } -> 1
  | Spacer { flex; _ } -> flex
  | _ -> 0

let text ?(style = Render.Style.empty) ?(align = `Start) ?(tab_width = 4)
    ?(wrap = false) content =
  Text (Text.make ~style ~align ~tab_width ~wrap content)

let rich_text segments = Rich_text (Rich_text.make segments)
let spacer ?(flex = 0) size = Spacer (Spacer.make ~flex size)

let flow ?(h_gap = 1) ?(v_gap = 0) children =
  Flow (Flow.make ~h_gap ~v_gap children)

let grid ?(col_spacing = 0) ?(row_spacing = 0) ~columns ~rows children =
  Grid (Grid.make ~col_spacing ~row_spacing ~columns ~rows children)

let scroll ?width ?height ?(h_offset = 0) ?(v_offset = 0) child =
  Scroll (Scroll.make ?width ?height ~h_offset ~v_offset child)

let zstack ?(align = Top_left) children = Z_stack (Z_stack.make ~align children)

let hbox ?(gap = 0) ?width ?height ?min_width ?min_height ?max_width ?max_height
    ?(margin = Padding.no_padding) ?(padding = Padding.no_padding) ?border
    ?background ?(align_items = `Stretch) ?(justify_content = `Start)
    ?(flex_grow = 0) ?(flex_shrink = 0) ?(fill = false) ?(wrap = false) children
    =
  let options =
    {
      Box.direction = `Horizontal;
      gap;
      width;
      height;
      min_width;
      min_height;
      max_width;
      max_height;
      margin;
      padding;
      border;
      background;
      align = align_items;
      justify = justify_content;
      flex_grow;
      flex_shrink;
      fill;
    }
  in
  if wrap then
    let flow_content = flow ~h_gap:gap ~v_gap:0 children in
    let wrapper_options = { options with gap = 0 } in
    Box (Box.make ~options:wrapper_options [ flow_content ])
  else Box (Box.make ~options children)

let vbox ?(gap = 0) ?width ?height ?min_width ?min_height ?max_width ?max_height
    ?(margin = Padding.no_padding) ?(padding = Padding.no_padding) ?border
    ?background ?(align_items = `Stretch) ?(justify_content = `Start)
    ?(flex_grow = 0) ?(flex_shrink = 0) ?(fill = true) children =
  let options =
    {
      Box.direction = `Vertical;
      gap;
      width;
      height;
      min_width;
      min_height;
      max_width;
      max_height;
      margin;
      padding;
      border;
      background;
      align = align_items;
      justify = justify_content;
      flex_grow;
      flex_shrink;
      fill;
    }
  in
  Box (Box.make ~options children)

let flex_spacer () = spacer ~flex:1 0

let divider ?(style = Render.Style.(fg (gray 8))) ?(char = "─") () =
  let content =
    if String.length char = 0 then String.make 1000 '-'
    else
      let rec repeat s n =
        if n <= 0 then "" else if n = 1 then s else s ^ repeat s (n - 1)
      in
      repeat char 1000
  in
  hbox ~flex_grow:1 [ text ~style content ]

let center child =
  vbox ~align_items:`Center ~justify_content:`Center ~flex_grow:1
    [
      hbox ~align_items:`Center ~justify_content:`Center ~flex_grow:1 [ child ];
    ]

let styled ?fg ?bg child =
  let style =
    match (fg, bg) with
    | Some fg_color, Some bg_color -> Render.Style.(fg fg_color ++ bg bg_color)
    | Some fg_color, None -> Render.Style.fg fg_color
    | None, Some bg_color -> Render.Style.bg bg_color
    | None, None -> Render.Style.empty
  in
  hbox ~background:style [ child ]

let rec pp fmt = function
  | Text _ -> Format.fprintf fmt "Text"
  | Rich_text _ -> Format.fprintf fmt "Rich_text"
  | Spacer { flex; _ } -> Format.fprintf fmt "Spacer(flex=%d)" flex
  | Box { options; children; _ } ->
      let dir = if options.direction = `Horizontal then "H" else "V" in
      Format.fprintf fmt "%sBox[@[<hv>%a@]]" dir
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           pp)
        children
  | Z_stack { children; _ } ->
      Format.fprintf fmt "Z_stack[@[<hv>%a@]]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           pp)
        children
  | Flow { children; _ } ->
      Format.fprintf fmt "Flow[@[<hv>%a@]]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           pp)
        children
  | Grid { children; _ } ->
      Format.fprintf fmt "Grid[@[<hv>%a@]]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           pp)
        children
  | Scroll { child; _ } -> Format.fprintf fmt "Scroll(%a)" pp child
