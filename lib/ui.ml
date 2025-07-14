(* High-level rendering API *)

(* Layout types *)
type padding = { top : int; right : int; bottom : int; left : int }

let padding ?(top = 0) ?(right = 0) ?(bottom = 0) ?(left = 0) () =
  { top; right; bottom; left }

let pad ?all ?x ?y ?top ?right ?bottom ?left () =
  let base_val = Option.value all ~default:0 in
  let h_val = Option.value x ~default:base_val in
  let v_val = Option.value y ~default:base_val in
  {
    top = Option.value top ~default:v_val;
    right = Option.value right ~default:h_val;
    bottom = Option.value bottom ~default:v_val;
    left = Option.value left ~default:h_val;
  }

let padding_all n = { top = n; right = n; bottom = n; left = n }
let padding_xy x y = { top = y; right = x; bottom = y; left = x }

type border_style = Solid | Rounded | Double | Thick | ASCII

type border_spec = {
  top : bool;
  bottom : bool;
  left : bool;
  right : bool;
  style : border_style;
  color : Ansi.color option;
}

type border = border_spec

let border ?(style = Solid) ?color () =
  { top = true; bottom = true; left = true; right = true; style; color }

let border_spec ?(top = true) ?(bottom = true) ?(left = true) ?(right = true)
    ?(style = Solid) ?color () =
  { top; bottom; left; right; style; color }

let normal_border = border ~style:Solid ()
let rounded_border = border ~style:Rounded ()
let double_border = border ~style:Double ()
let thick_border = border ~style:Thick ()
let ascii_border = border ~style:ASCII ()

type align = Start | Center | End | Stretch

(* Cache for layout results *)
type layout_cache = {
  (* The context in which this was rendered *)
  ctx_width : int;
  ctx_height : int;
  (* The computed result *)
  computed_width : int;
  computed_height : int;
  (* The full computed layout of children for redraw *)
  children_layouts : computed_element list;
}

and computed_element = {
  element : element;
  x : int;
  y : int;
  width : int;
  height : int;
}

and element =
  | Text of text_data
  | Box of box_data
  | Spacer of int
  | Expand of element (* Wrapper to mark expandable elements *)
  | Rich_text of (string * Render.Style.t) list
  | Z_stack of z_stack_data
  | Flow of flow_data
  | Grid of grid_data

and text_data = {
  content : string;
  style : Render.Style.t;
  align : align;
  tab_width : int;
}

and box_data = {
  children : element list;
  options : layout_options;
  (* Add a mutable cache field to every box *)
  mutable cache : layout_cache option;
}

and layout_options = {
  direction : [ `Horizontal | `Vertical ];
  gap : int;
  width : int option;
  height : int option;
  min_width : int option;
  min_height : int option;
  max_width : int option;
  max_height : int option;
  margin : padding;
  padding : padding;
  border : border option;
  background : Render.Style.t option;
  align : align;
  justify : align;
}

and z_align =
  | Top_left
  | Top
  | Top_right
  | Left
  | Center
  | Right
  | Bottom_left
  | Bottom
  | Bottom_right

and z_stack_data = {
  children : element list;
  align : z_align;
  mutable cache : layout_cache option;
}

and flow_data = {
  children : element list;
  h_gap : int;
  v_gap : int;
  mutable cache : layout_cache option;
}

and size_def = Fixed of int | Flex of int
and col_def = size_def
and row_def = size_def

and grid_data = {
  children : element list;
  columns : col_def list;
  rows : row_def list;
  col_spacing : int;
  row_spacing : int;
  mutable cache : layout_cache option;
}

let text ?(style = Render.Style.empty) ?(align = Start) ?(tab_width = 4) content
    =
  Text { content; style; align; tab_width }

let no_padding = padding ()

let hbox ?(gap = 0) ?width ?height ?min_width ?min_height ?max_width ?max_height
    ?(margin = no_padding) ?(padding = no_padding) ?border ?background
    ?(align_items = Stretch) ?(justify_content = Start) children =
  let options =
    {
      direction = `Horizontal;
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
    }
  in
  Box { children; options; cache = None }

let vbox ?(gap = 0) ?width ?height ?min_width ?min_height ?max_width ?max_height
    ?(margin = no_padding) ?(padding = no_padding) ?border ?background
    ?(align_items = Stretch) ?(justify_content = Start) children =
  let options =
    {
      direction = `Vertical;
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
    }
  in
  Box { children; options; cache = None }

let spacer n = Spacer n
let space = spacer (* Alias for API compatibility *)
let expand element = Expand element

(* Rich text element *)
let rich_text segments = Rich_text segments

(* Z-stack layout *)
let zstack ?(align = Top_left) children =
  Z_stack { children; align; cache = None }

(* Flow layout *)
let flow ?(h_gap = 0) ?(v_gap = 0) children =
  Flow { children; h_gap; v_gap; cache = None }

(* Grid layout *)
let grid ?(col_spacing = 0) ?(row_spacing = 0) ~columns ~rows children =
  Grid { children; columns; rows; col_spacing; row_spacing; cache = None }

(* Helper functions *)
let flex_spacer () = Expand (Spacer 0)

let divider ?(style = Render.Style.(fg (gray 8))) ?(char = "─") () =
  let content =
    if String.length char = 0 then String.make 1000 '-'
    else
      (* Repeat the string (which may be multi-byte) instead of just the first byte *)
      let rec repeat s n =
        if n <= 0 then "" else if n = 1 then s else s ^ repeat s (n - 1)
      in
      repeat char 1000
  in
  expand (text ~style content)

(* Border drawing characters *)
let border_chars style =
  match style with
  | Solid -> ("┌", "─", "┐", "│", "└", "─", "┘", "│")
  | Rounded -> ("╭", "─", "╮", "│", "╰", "─", "╯", "│")
  | Double -> ("╔", "═", "╗", "║", "╚", "═", "╝", "║")
  | Thick -> ("┏", "━", "┓", "┃", "┗", "━", "┛", "┃")
  | ASCII -> ("+", "-", "+", "|", "+", "-", "+", "|")

(* Layout engine that renders elements to a Render buffer *)
type layout_context = {
  x : int; (* Current x position *)
  y : int; (* Current y position *)
  width : int; (* Available width *)
  height : int; (* Available height *)
}

(* Helper to draw a border with per-side control *)
let draw_border buffer x y width height (border_spec : border_spec) =
  let tl, t, tr, r, bl, b, br, l = border_chars border_spec.style in
  let border_style =
    match border_spec.color with
    | Some color -> Render.Style.fg color
    | None -> Render.Style.empty
  in

  (* Determine corner characters based on which sides are enabled *)
  let top_left =
    match (border_spec.top, border_spec.left) with
    | true, true -> tl
    | true, false -> t
    | false, true -> l
    | false, false -> " "
  in
  let top_right =
    match (border_spec.top, border_spec.right) with
    | true, true -> tr
    | true, false -> t
    | false, true -> r
    | false, false -> " "
  in
  let bottom_left =
    match (border_spec.bottom, border_spec.left) with
    | true, true -> bl
    | true, false -> b
    | false, true -> l
    | false, false -> " "
  in
  let bottom_right =
    match (border_spec.bottom, border_spec.right) with
    | true, true -> br
    | true, false -> b
    | false, true -> r
    | false, false -> " "
  in

  (* Draw corners *)
  if border_spec.top || border_spec.left then
    Render.set_string buffer x y top_left border_style;
  if border_spec.top || border_spec.right then
    Render.set_string buffer (x + width - 1) y top_right border_style;
  if border_spec.bottom || border_spec.left then
    Render.set_string buffer x (y + height - 1) bottom_left border_style;
  if border_spec.bottom || border_spec.right then
    Render.set_string buffer
      (x + width - 1)
      (y + height - 1)
      bottom_right border_style;

  (* Top border *)
  if border_spec.top && width > 2 then
    for i = 1 to width - 2 do
      Render.set_string buffer (x + i) y t border_style
    done;

  (* Bottom border *)
  if border_spec.bottom && width > 2 then
    for i = 1 to width - 2 do
      Render.set_string buffer (x + i) (y + height - 1) b border_style
    done;

  (* Side borders *)
  if height > 2 then
    for i = 1 to height - 2 do
      if border_spec.left then Render.set_string buffer x (y + i) l border_style;
      if border_spec.right then
        Render.set_string buffer (x + width - 1) (y + i) r border_style
    done

(* Helper to expand tabs to spaces *)
let expand_tabs s tab_width =
  let rec expand_line line =
    match String.index_opt line '\t' with
    | None -> line
    | Some idx ->
        let before = String.sub line 0 idx in
        let after = String.sub line (idx + 1) (String.length line - idx - 1) in
        let col = Render.measure_string before in
        let spaces_needed = tab_width - (col mod tab_width) in
        let spaces = String.make spaces_needed ' ' in
        expand_line (before ^ spaces ^ after)
  in
  String.split_on_char '\n' s |> List.map expand_line |> String.concat "\n"

(* Calculate border space based on which sides are enabled *)
let border_space_h border_opt =
  match border_opt with
  | None -> 0
  | Some b -> (if b.left then 1 else 0) + if b.right then 1 else 0

let border_space_v border_opt =
  match border_opt with
  | None -> 0
  | Some b -> (if b.top then 1 else 0) + if b.bottom then 1 else 0

(* Get natural size of element without rendering *)
let rec measure_element element =
  match element with
  | Text { content; tab_width; _ } ->
      let expanded = expand_tabs content tab_width in
      let lines = String.split_on_char '\n' expanded in
      let max_width =
        List.fold_left
          (fun acc line -> max acc (Render.measure_string line))
          0 lines
      in
      let height = List.length lines in
      (max_width, max height 1)
  | Spacer n -> (n, 1)
  | Expand e -> measure_element e
  | Rich_text segments ->
      let width =
        List.fold_left
          (fun acc (s, _) -> acc + Render.measure_string s)
          0 segments
      in
      (width, 1)
  | Z_stack { children; _ } ->
      (* Z-stack size is the maximum of all children *)
      List.fold_left
        (fun (max_w, max_h) child ->
          let w, h = measure_element child in
          (max max_w w, max max_h h))
        (0, 0) children
  | Flow { children; h_gap; v_gap = _; _ } ->
      (* Flow layout needs context width to calculate wrapping, so return sum for now *)
      let total_width =
        List.fold_left
          (fun acc child ->
            let w, _ = measure_element child in
            acc + w)
          0 children
      in
      let max_height =
        List.fold_left
          (fun acc child ->
            let _, h = measure_element child in
            max acc h)
          0 children
      in
      let gap_space = h_gap * max 0 (List.length children - 1) in
      (total_width + gap_space, max_height)
  | Grid { children = _; columns; rows; col_spacing; row_spacing; _ } ->
      (* Grid natural size based on column/row definitions *)
      let fixed_width =
        List.fold_left
          (fun acc col -> match col with Fixed w -> acc + w | Flex _ -> acc)
          0 columns
      in
      let fixed_height =
        List.fold_left
          (fun acc row -> match row with Fixed h -> acc + h | Flex _ -> acc)
          0 rows
      in
      let col_gaps = col_spacing * max 0 (List.length columns - 1) in
      let row_gaps = row_spacing * max 0 (List.length rows - 1) in
      (fixed_width + col_gaps, fixed_height + row_gaps)
  | Box { children; options = opts; _ } -> (
      let children_sizes =
        List.map
          (fun child ->
            match child with
            | Spacer n -> if opts.direction = `Horizontal then (n, 1) else (1, n)
            | _ -> measure_element child)
          children
      in
      let border_h = border_space_h opts.border in
      let border_v = border_space_v opts.border in
      let padding_h = opts.padding.left + opts.padding.right in
      let padding_v = opts.padding.top + opts.padding.bottom in
      let margin_h = opts.margin.left + opts.margin.right in
      let margin_v = opts.margin.top + opts.margin.bottom in

      match opts.direction with
      | `Horizontal ->
          let total_width =
            List.fold_left (fun acc (w, _) -> acc + w) 0 children_sizes
          in
          let max_height =
            List.fold_left (fun acc (_, h) -> max acc h) 0 children_sizes
          in
          let gap_space = opts.gap * max 0 (List.length children - 1) in
          let width =
            Option.value opts.width
              ~default:
                (total_width + gap_space + padding_h + border_h + margin_h)
          in
          let height =
            Option.value opts.height
              ~default:(max_height + padding_v + border_v + margin_v)
          in
          (width, height)
      | `Vertical ->
          let max_width =
            List.fold_left (fun acc (w, _) -> max acc w) 0 children_sizes
          in
          let total_height =
            List.fold_left (fun acc (_, h) -> acc + h) 0 children_sizes
          in
          let gap_space = opts.gap * max 0 (List.length children - 1) in
          let width =
            Option.value opts.width
              ~default:(max_width + padding_h + border_h + margin_h)
          in
          let height =
            Option.value opts.height
              ~default:
                (total_height + gap_space + padding_v + border_v + margin_v)
          in
          (width, height))

(* Unwrap all layers of Expand *)
let rec unwrap_expand element =
  match element with Expand e -> unwrap_expand e | _ -> element

(* Check if element is expandable *)
let is_expandable = function Expand _ -> true | _ -> false

(* Clear layout caches from previous frame *)
let rec clear_cache element =
  match element with
  | Text _ | Spacer _ | Rich_text _ -> ()
  | Expand e -> clear_cache e
  | Box data ->
      data.cache <- None;
      List.iter clear_cache data.children
  | Z_stack data ->
      data.cache <- None;
      List.iter clear_cache data.children
  | Flow data ->
      data.cache <- None;
      List.iter clear_cache data.children
  | Grid data ->
      data.cache <- None;
      List.iter clear_cache data.children

(* Apply alignment offset *)
let align_offset available used align =
  match align with
  | Start -> 0
  | Center -> max 0 ((available - used) / 2)
  | End -> max 0 (available - used)
  | Stretch -> 0

(* Calculate box layout without rendering - pure function *)
let rec calculate_box_layout ctx children (opts : layout_options) =
  (* First, account for margins by shrinking the available context *)
  let margin_ctx =
    {
      x = ctx.x + opts.margin.left;
      y = ctx.y + opts.margin.top;
      width = ctx.width - opts.margin.left - opts.margin.right;
      height = ctx.height - opts.margin.top - opts.margin.bottom;
    }
  in

  (* Calculate box dimensions with min/max constraints *)
  let natural_width, natural_height =
    measure_element (Box { children; options = opts; cache = None })
  in

  (* Check if this box has expandable children *)
  let has_expandable_children = List.exists is_expandable children in

  let resolve_dimension value min_val max_val available natural use_available =
    let resolved =
      match value with
      | Some v -> v
      | None -> if use_available then available else min available natural
    in
    let with_min =
      match min_val with Some m -> max m resolved | None -> resolved
    in
    match max_val with Some m -> min m with_min | None -> with_min
  in

  (* For horizontal boxes with expandable children, use available width by default *)
  (* For vertical boxes, also use available width if they have expandable children,
     since expandable children likely want to expand horizontally *)
  let use_available_width = has_expandable_children && opts.width = None in

  let box_width =
    resolve_dimension opts.width opts.min_width opts.max_width margin_ctx.width
      natural_width use_available_width
  in

  (* For vertical boxes with expandable children, use available height by default *)
  let use_available_height =
    opts.direction = `Vertical && has_expandable_children && opts.height = None
  in

  let box_height =
    resolve_dimension opts.height opts.min_height opts.max_height
      margin_ctx.height natural_height use_available_height
  in

  (* Calculate content area after border and padding *)
  let border_left =
    match opts.border with None -> 0 | Some b -> if b.left then 1 else 0
  in
  let border_right =
    match opts.border with None -> 0 | Some b -> if b.right then 1 else 0
  in
  let border_top =
    match opts.border with None -> 0 | Some b -> if b.top then 1 else 0
  in
  let border_bottom =
    match opts.border with None -> 0 | Some b -> if b.bottom then 1 else 0
  in

  let content_x = margin_ctx.x + border_left + opts.padding.left in
  let content_y = margin_ctx.y + border_top + opts.padding.top in
  let content_width =
    box_width - border_left - border_right - opts.padding.left
    - opts.padding.right
  in
  let content_height =
    box_height - border_top - border_bottom - opts.padding.top
    - opts.padding.bottom
  in

  (* Count expandable children *)
  let expandable_count = List.filter is_expandable children |> List.length in

  (* Measure non-expandable children *)
  let measured_children =
    List.map
      (fun child ->
        if is_expandable child then (child, 0, 0)
        else
          match child with
          | Spacer n ->
              let w = if opts.direction = `Horizontal then n else 1 in
              let h = if opts.direction = `Vertical then n else 1 in
              (child, w, h)
          | _ ->
              let w, h = measure_element child in
              (child, w, h))
      children
  in

  match opts.direction with
  | `Horizontal ->
      (* Calculate space for expandable items *)
      let fixed_width =
        List.fold_left (fun acc (_, w, _) -> acc + w) 0 measured_children
      in
      let gap_space = opts.gap * max 0 (List.length children - 1) in
      let available_expand = max 0 (content_width - fixed_width - gap_space) in
      let expand_each =
        if expandable_count > 0 then available_expand / expandable_count else 0
      in

      (* Calculate children layouts *)
      let total_children_width =
        fixed_width + (expand_each * expandable_count) + gap_space
      in
      let x_offset =
        align_offset content_width total_children_width opts.justify
      in

      let rec calc_h x children_with_sizes acc =
        match children_with_sizes with
        | [] -> List.rev acc
        | (child, w, _) :: rest ->
            let child_width = if is_expandable child then expand_each else w in
            let child_height = content_height in

            (* Apply vertical alignment *)
            let measured_h = snd (measure_element (unwrap_expand child)) in
            let y_offset = align_offset child_height measured_h opts.align in

            let computed =
              {
                element = child;
                x;
                y = content_y + y_offset;
                width = child_width;
                height =
                  (if opts.align = Stretch then child_height
                   else min child_height measured_h);
              }
            in

            let next_x = x + child_width + if rest = [] then 0 else opts.gap in
            calc_h next_x rest (computed :: acc)
      in

      let children_layouts =
        calc_h (content_x + x_offset) measured_children []
      in
      (box_width, box_height, children_layouts)
  | `Vertical ->
      (* Calculate space for expandable items *)
      let fixed_height =
        List.fold_left (fun acc (_, _, h) -> acc + h) 0 measured_children
      in
      let gap_space = opts.gap * max 0 (List.length children - 1) in
      let available_expand =
        max 0 (content_height - fixed_height - gap_space)
      in
      let expand_each =
        if expandable_count > 0 then available_expand / expandable_count else 0
      in

      (* Calculate children layouts *)
      let total_children_height =
        fixed_height + (expand_each * expandable_count) + gap_space
      in
      let y_offset =
        align_offset content_height total_children_height opts.justify
      in

      let rec calc_v y children_with_sizes acc =
        (* Clip: stop laying out children that would overflow *)
        if y >= content_y + content_height then
          List.rev acc
        else
          match children_with_sizes with
          | [] -> List.rev acc
          | (child, _, h) :: rest ->
              let child_width = content_width in
              let child_height = if is_expandable child then expand_each else h in
              
              (* Check if this child would overflow vertically *)
              if y + child_height > content_y + content_height then
                List.rev acc  (* Clip partial child *)
              else
                (* Apply horizontal alignment *)
                let measured_w = fst (measure_element (unwrap_expand child)) in
                let x_offset = align_offset child_width measured_w opts.align in

                let computed =
                  {
                    element = child;
                    x = content_x + x_offset;
                    y;
                    width =
                      (if opts.align = Stretch then child_width
                       else min child_width measured_w);
                    height = child_height;
                  }
                in

                let next_y = y + child_height + if rest = [] then 0 else opts.gap in
                calc_v next_y rest (computed :: acc)
      in

      let children_layouts =
        calc_v (content_y + y_offset) measured_children []
      in
      (box_width, box_height, children_layouts)

(* Redraw from cached layout information *)
and redraw_from_cache ctx buffer opts cache =
  (* Apply margin offset for drawing *)
  let draw_x = ctx.x + opts.margin.left in
  let draw_y = ctx.y + opts.margin.top in

  (* Draw background if specified *)
  (match opts.background with
  | Some bg_style ->
      (* Check if background has gradient or adaptive color *)
      let has_gradient =
        match bg_style.Render.Style.bg with
        | Some (Render.Style.Gradient _) -> true
        | Some (Render.Style.Adaptive _) -> true
        | _ -> false
      in
      if has_gradient then
        (* Use gradient-aware background fill *)
        Render.fill_rect_gradient buffer draw_x draw_y cache.computed_width
          cache.computed_height bg_style
      else
        (* Use optimized solid background fill *)
        Render.fill_rect buffer draw_x draw_y cache.computed_width
          cache.computed_height bg_style
  | None -> ());

  (* Draw the border for the parent box if needed *)
  (match opts.border with
  | Some border_spec when cache.computed_width > 2 && cache.computed_height > 2
    ->
      draw_border buffer draw_x draw_y cache.computed_width
        cache.computed_height border_spec
  | _ -> ());

  (* Recursively render children using computed geometry *)
  List.iter
    (fun (cl : computed_element) ->
      let child_ctx =
        { x = cl.x; y = cl.y; width = cl.width; height = cl.height }
      in
      ignore (render_at child_ctx buffer cl.element))
    cache.children_layouts

(* Layout calculation for z-stack *)
and calculate_z_stack_layout ctx children align =
  let width = ctx.width in
  let height = ctx.height in

  (* Calculate position based on alignment *)
  let align_child child =
    let child_w, child_h = measure_element child in
    let x_offset =
      match align with
      | Top_left | Left | Bottom_left -> 0
      | Top | Center | Bottom -> (width - child_w) / 2
      | Top_right | Right | Bottom_right -> width - child_w
    in
    let y_offset =
      match align with
      | Top_left | Top | Top_right -> 0
      | Left | Center | Right -> (height - child_h) / 2
      | Bottom_left | Bottom | Bottom_right -> height - child_h
    in
    {
      element = child;
      x = ctx.x + x_offset;
      y = ctx.y + y_offset;
      width = min child_w width;
      height = min child_h height;
    }
  in

  let children_layouts = List.map align_child children in
  (width, height, children_layouts)

(* Layout calculation for flow *)
and calculate_flow_layout ctx children h_gap v_gap =
  let width = ctx.width in

  (* Measure all children *)
  let measured =
    List.map
      (fun child ->
        let w, h = measure_element child in
        (child, w, h))
      children
  in

  (* Calculate wrapped lines *)
  let rec wrap_lines x y current_line remaining acc_lines max_h =
    match remaining with
    | [] ->
        let lines =
          if current_line = [] then acc_lines else current_line :: acc_lines
        in
        (List.rev lines, y + max_h)
    | (child, w, h) :: rest ->
        if x + w <= width || current_line = [] then
          (* Add to current line *)
          let item =
            {
              element = child;
              x = ctx.x + x;
              y = ctx.y + y;
              width = w;
              height = h;
            }
          in
          wrap_lines
            (x + w + h_gap)
            y (item :: current_line) rest acc_lines (max max_h h)
        else
          (* Start new line *)
          let lines =
            if current_line = [] then acc_lines else current_line :: acc_lines
          in
          let item =
            {
              element = child;
              x = ctx.x;
              y = ctx.y + y + max_h + v_gap;
              width = w;
              height = h;
            }
          in
          wrap_lines (w + h_gap) (y + max_h + v_gap) [ item ] rest lines h
  in

  let lines, total_height = wrap_lines 0 0 [] measured [] 0 in
  let all_items = List.concat (List.map List.rev lines) in
  (width, total_height, all_items)

(* Layout calculation for grid *)
and calculate_grid_layout ctx children columns rows col_spacing row_spacing =
  let width = ctx.width in
  let height = ctx.height in

  (* Calculate column widths *)
  let flex_cols =
    List.filter (function Flex _ -> true | _ -> false) columns
  in
  let fixed_col_width =
    List.fold_left
      (fun acc col -> match col with Fixed w -> acc + w | _ -> acc)
      0 columns
  in
  let col_gaps = col_spacing * max 0 (List.length columns - 1) in
  let available_col_flex = max 0 (width - fixed_col_width - col_gaps) in
  let flex_col_unit =
    if flex_cols = [] then 0
    else
      available_col_flex
      / List.fold_left
          (fun acc col -> match col with Flex n -> acc + n | _ -> acc)
          0 columns
  in

  let col_widths =
    List.map (function Fixed w -> w | Flex n -> n * flex_col_unit) columns
  in

  (* Calculate row heights similarly *)
  let flex_rows = List.filter (function Flex _ -> true | _ -> false) rows in
  let fixed_row_height =
    List.fold_left
      (fun acc row -> match row with Fixed h -> acc + h | _ -> acc)
      0 rows
  in
  let row_gaps = row_spacing * max 0 (List.length rows - 1) in
  let available_row_flex = max 0 (height - fixed_row_height - row_gaps) in
  let flex_row_unit =
    if flex_rows = [] then 0
    else
      available_row_flex
      / List.fold_left
          (fun acc row -> match row with Flex n -> acc + n | _ -> acc)
          0 rows
  in

  let row_heights =
    List.map (function Fixed h -> h | Flex n -> n * flex_row_unit) rows
  in

  (* Calculate cell positions *)
  let rec calc_positions col row x y col_widths row_heights acc children =
    match children with
    | [] -> List.rev acc
    | child :: rest ->
        if col >= List.length columns then
          calc_positions 0 (row + 1) ctx.x
            (y + List.nth row_heights row + row_spacing)
            col_widths row_heights acc children
        else if row >= List.length rows then (
          (* Warn about extra children being dropped *)
          let extra_count = List.length children in
          if extra_count > 0 then
            Printf.eprintf
              "Warning: Grid has %d extra children that don't fit in %dx%d grid\n%!"
              extra_count (List.length columns) (List.length rows);
          List.rev acc (* Ignore extra children *)
        )
        else
          let w = List.nth col_widths col in
          let h = List.nth row_heights row in
          let item = { element = child; x; y; width = w; height = h } in
          let next_x = x + w + col_spacing in
          calc_positions (col + 1) row next_x y col_widths row_heights
            (item :: acc) rest
  in

  let children_layouts =
    calc_positions 0 0 ctx.x ctx.y col_widths row_heights [] children
  in
  (width, height, children_layouts)

(* Main render function with caching *)
and render_at ctx buffer element =
  match element with
  | Text { content; style; align; tab_width } ->
      let expanded = expand_tabs content tab_width in
      let lines = String.split_on_char '\n' expanded in
      let max_width =
        List.fold_left
          (fun acc line -> max acc (Render.measure_string line))
          0 lines
      in

      (* Check if style has gradients or adaptive colors *)
      let has_gradient =
        match (style.Render.Style.fg, style.Render.Style.bg) with
        | Some (Render.Style.Gradient _), _ -> true
        | _, Some (Render.Style.Gradient _) -> true
        | Some (Render.Style.Adaptive _), _ -> true
        | _, Some (Render.Style.Adaptive _) -> true
        | _ -> false
      in

      (* Render each line with alignment and clipping *)
      List.iteri
        (fun i line ->
          (* Skip lines that are outside the vertical bounds *)
          if i < ctx.height then
            let line_width = Render.measure_string line in
            let x_offset = align_offset ctx.width line_width align in
            (* Clip line to context width *)
            let clipped_line = 
              if line_width > ctx.width - x_offset then
                let max_chars = ctx.width - x_offset in
                if max_chars > 0 then
                  (* Simple character-based clipping - may not handle Unicode perfectly *)
                  String.sub line 0 (min (String.length line) max_chars)
                else ""
              else line
            in
            let clipped_width = Render.measure_string clipped_line in
            if clipped_width > 0 then
              if has_gradient then
                (* Use gradient-aware rendering *)
                Render.set_string_gradient buffer (ctx.x + x_offset) (ctx.y + i)
                  clipped_line style ~width:clipped_width ~height:1
              else
                (* Use regular rendering *)
                Render.set_string buffer (ctx.x + x_offset) (ctx.y + i) clipped_line style)
        lines;

      (min max_width ctx.width, min (List.length lines) ctx.height)
  | Rich_text segments ->
      let rec render_segments x segments total_width =
        match segments with
        | [] -> total_width
        | (s, style) :: rest ->
            (* Check if we've exceeded the horizontal bounds *)
            if x - ctx.x >= ctx.width then
              total_width
            else
              let w = Render.measure_string s in
              (* Clip segment if it extends beyond context width *)
              let available = ctx.x + ctx.width - x in
              let (clipped_s, clipped_w) =
                if w > available && available > 0 then
                  (* Simple clipping - may need refinement for Unicode *)
                  let max_chars = min (String.length s) available in
                  let clipped = String.sub s 0 max_chars in
                  (clipped, Render.measure_string clipped)
                else if available <= 0 then
                  ("", 0)
                else
                  (s, w)
              in
              if clipped_w > 0 then
                (* Check if this segment has gradients or adaptive colors *)
                let has_gradient =
                  match (style.Render.Style.fg, style.Render.Style.bg) with
                  | Some (Render.Style.Gradient _), _ -> true
                  | _, Some (Render.Style.Gradient _) -> true
                  | Some (Render.Style.Adaptive _), _ -> true
                  | _, Some (Render.Style.Adaptive _) -> true
                  | _ -> false
                in
                if has_gradient then
                  Render.set_string_gradient buffer x ctx.y clipped_s style ~width:clipped_w
                    ~height:1
                else Render.set_string buffer x ctx.y clipped_s style;
                render_segments (x + clipped_w) rest (total_width + clipped_w)
              else
                total_width
      in
      let width = render_segments ctx.x segments 0 in
      (min width ctx.width, 1)
  | Spacer _ ->
      (* Spacers should not render anything - they're just layout placeholders *)
      (* The parent already filled the background, so we don't need to clear *)
      (ctx.width, ctx.height)
  | Expand e ->
      (* Expanded elements fill available space *)
      (* The context width/height is the allocated space for this element *)
      let _w, _h = render_at ctx buffer e in
      (ctx.width, ctx.height)
  | Z_stack data -> (
      match data.cache with
      | Some cache
        when cache.ctx_width = ctx.width && cache.ctx_height = ctx.height ->
          List.iter
            (fun (cl : computed_element) ->
              let child_ctx =
                { x = cl.x; y = cl.y; width = cl.width; height = cl.height }
              in
              ignore (render_at child_ctx buffer cl.element))
            cache.children_layouts;
          (cache.computed_width, cache.computed_height)
      | _ ->
          let computed_width, computed_height, children_layouts =
            calculate_z_stack_layout ctx data.children data.align
          in
          data.cache <-
            Some
              {
                ctx_width = ctx.width;
                ctx_height = ctx.height;
                computed_width;
                computed_height;
                children_layouts;
              };
          List.iter
            (fun (cl : computed_element) ->
              let child_ctx =
                { x = cl.x; y = cl.y; width = cl.width; height = cl.height }
              in
              ignore (render_at child_ctx buffer cl.element))
            children_layouts;
          (computed_width, computed_height))
  | Flow data -> (
      match data.cache with
      | Some cache
        when cache.ctx_width = ctx.width && cache.ctx_height = ctx.height ->
          List.iter
            (fun (cl : computed_element) ->
              let child_ctx =
                { x = cl.x; y = cl.y; width = cl.width; height = cl.height }
              in
              ignore (render_at child_ctx buffer cl.element))
            cache.children_layouts;
          (cache.computed_width, cache.computed_height)
      | _ ->
          let computed_width, computed_height, children_layouts =
            calculate_flow_layout ctx data.children data.h_gap data.v_gap
          in
          data.cache <-
            Some
              {
                ctx_width = ctx.width;
                ctx_height = ctx.height;
                computed_width;
                computed_height;
                children_layouts;
              };
          List.iter
            (fun (cl : computed_element) ->
              let child_ctx =
                { x = cl.x; y = cl.y; width = cl.width; height = cl.height }
              in
              ignore (render_at child_ctx buffer cl.element))
            children_layouts;
          (computed_width, computed_height))
  | Grid data -> (
      match data.cache with
      | Some cache
        when cache.ctx_width = ctx.width && cache.ctx_height = ctx.height ->
          List.iter
            (fun (cl : computed_element) ->
              let child_ctx =
                { x = cl.x; y = cl.y; width = cl.width; height = cl.height }
              in
              ignore (render_at child_ctx buffer cl.element))
            cache.children_layouts;
          (cache.computed_width, cache.computed_height)
      | _ ->
          let computed_width, computed_height, children_layouts =
            calculate_grid_layout ctx data.children data.columns data.rows
              data.col_spacing data.row_spacing
          in
          data.cache <-
            Some
              {
                ctx_width = ctx.width;
                ctx_height = ctx.height;
                computed_width;
                computed_height;
                children_layouts;
              };
          List.iter
            (fun (cl : computed_element) ->
              let child_ctx =
                { x = cl.x; y = cl.y; width = cl.width; height = cl.height }
              in
              ignore (render_at child_ctx buffer cl.element))
            children_layouts;
          (computed_width, computed_height))
  | Box data -> (
      (* 1. Check the cache *)
      match data.cache with
      | Some cache
        when cache.ctx_width = ctx.width && cache.ctx_height = ctx.height ->
          (* Cache hit! The box has been laid out in this context before.
             We can skip all calculations and just redraw. *)
          redraw_from_cache ctx buffer data.options cache;
          (cache.computed_width, cache.computed_height)
      | _ ->
          (* Cache miss. We must perform the full layout calculation. *)
          let computed_width, computed_height, children_layouts =
            calculate_box_layout ctx data.children data.options
          in

          (* 2. Store the result in the cache *)
          data.cache <-
            Some
              {
                ctx_width = ctx.width;
                ctx_height = ctx.height;
                computed_width;
                computed_height;
                children_layouts;
              };

          (* 3. Render the box and its children for the first time *)
          redraw_from_cache ctx buffer data.options (Option.get data.cache);

          (computed_width, computed_height))

let render buffer element =
  let width, height = Render.dimensions buffer in
  let ctx = { x = 0; y = 0; width; height } in
  ignore (render_at ctx buffer element)

(* Pretty-printing *)
let rec pp_element fmt = function
  | Text { content; _ } -> Format.fprintf fmt "Text(%S)" content
  | Rich_text segments ->
      Format.fprintf fmt "Rich_text[@[<hv>%a@]]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (s, _) -> Format.fprintf fmt "%S" s))
        segments
  | Box { children; options; _ } ->
      let dir =
        match options.direction with `Horizontal -> "H" | `Vertical -> "V"
      in
      Format.fprintf fmt "%sBox[@[<hv>%a@]]" dir
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           pp_element)
        children
  | Z_stack { children; align; _ } ->
      Format.fprintf fmt "Z_stack(%s)[@[<hv>%a@]]"
        (match align with
        | Top_left -> "TL"
        | Top -> "T"
        | Top_right -> "TR"
        | Left -> "L"
        | Center -> "C"
        | Right -> "R"
        | Bottom_left -> "BL"
        | Bottom -> "B"
        | Bottom_right -> "BR")
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           pp_element)
        children
  | Flow { children; h_gap; v_gap; _ } ->
      Format.fprintf fmt "Flow(h:%d,v:%d)[@[<hv>%a@]]" h_gap v_gap
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           pp_element)
        children
  | Grid { children; columns; rows; _ } ->
      Format.fprintf fmt "Grid(%dx%d)[@[<hv>%a@]]" (List.length columns)
        (List.length rows)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           pp_element)
        children
  | Spacer n -> Format.fprintf fmt "Spacer(%d)" n
  | Expand e -> Format.fprintf fmt "Expand(%a)" pp_element e

(* Public API for measuring elements *)
let measure element = measure_element element
