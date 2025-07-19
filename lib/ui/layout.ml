open Element

module Bounds = struct
  type t = { x : int; y : int; width : int; height : int }

  let make ~x ~y ~width ~height = { x; y; width; height }
  let x b = b.x
  let y b = b.y
  let width b = b.width
  let height b = b.height
end

type t = {
  element : Element.t;
  x : int;
  y : int;
  width : int;
  height : int;
  children : t list;
}

let element l = l.element
let x l = l.x
let y l = l.y
let width l = l.width
let height l = l.height
let geometry l = (l.x, l.y, l.width, l.height)
let children l = l.children

(* Helper to distribute integer remainder fairly using largest remainder method *)
let distribute_fairly total_to_distribute weights =
  let total_weight = List.fold_left ( + ) 0 weights in
  if total_weight = 0 then List.map (fun _ -> 0) weights
  else
    let with_fractions =
      List.mapi
        (fun i w ->
          let ideal =
            float_of_int total_to_distribute
            *. float_of_int w /. float_of_int total_weight
          in
          (i, floor ideal, ideal -. floor ideal))
        weights
    in
    let base_amounts =
      List.map (fun (_, base, _) -> int_of_float base) with_fractions
    in
    let distributed = List.fold_left ( + ) 0 base_amounts in
    let remainder = total_to_distribute - distributed in
    let sorted_by_fraction =
      List.sort (fun (_, _, f1) (_, _, f2) -> compare f2 f1) with_fractions
    in
    let rec distribute_remainder n acc = function
      | [] -> acc
      | (idx, base, _) :: rest ->
          if n > 0 then
            distribute_remainder (n - 1) ((idx, base + 1) :: acc) rest
          else distribute_remainder 0 ((idx, base) :: acc) rest
    in
    let final_amounts_with_remainder =
      distribute_remainder remainder []
        (List.map (fun (i, b, f) -> (i, int_of_float b, f)) sorted_by_fraction)
    in
    List.sort
      (fun (i1, _) (i2, _) -> compare i1 i2)
      final_amounts_with_remainder
    |> List.map snd

(* Helper to assign sizes along the main axis using flexbox-like logic *)
let assign_main_sizes ~available_main ~main_is_width ~content_cross_size
    children_elements gap =
  let item_infos =
    List.map
      (fun child ->
        let measure_width =
          if main_is_width then max_int else content_cross_size
        in
        let w, h = Element.measure ~width:measure_width child in
        let preferred_main = if main_is_width then w else h in
        let min_main =
          if main_is_width then Element.min_width child
          else Element.min_height child
        in
        ( child,
          preferred_main,
          min_main,
          Element.grow_fact child,
          Element.shrink_fact child ))
      children_elements
  in

  let total_preferred =
    List.fold_left (fun acc (_, pref, _, _, _) -> acc + pref) 0 item_infos
  in
  (* Count only non-zero size elements for gap calculation *)
  let non_zero_count =
    List.length (List.filter (fun (_, pref, _, _, _) -> pref > 0) item_infos)
  in
  let total_gap = gap * max 0 (non_zero_count - 1) in
  let available_for_children = max 0 (available_main - total_gap) in

  if total_preferred <= available_for_children then (* Grow phase *)
    let extra_space = available_for_children - total_preferred in
    let total_grow =
      List.fold_left (fun acc (_, _, _, grow, _) -> acc + grow) 0 item_infos
    in
    if total_grow = 0 then List.map (fun (_, pref, _, _, _) -> pref) item_infos
    else
      let grow_amounts =
        distribute_fairly extra_space
          (List.map (fun (_, _, _, g, _) -> g) item_infos)
      in
      List.map2
        (fun (_, pref, _, _, _) extra -> pref + extra)
        item_infos grow_amounts
  else (* Shrink phase *)
    let shortage = total_preferred - available_for_children in
    let weighted_shrinks =
      List.map (fun (_, pref, _, _, shrink) -> shrink * pref) item_infos
    in
    let total_shrink_weight = List.fold_left ( + ) 0 weighted_shrinks in
    if total_shrink_weight = 0 then
      List.map (fun (_, pref, _, _, _) -> pref) item_infos
    else
      let shrink_amounts = distribute_fairly shortage weighted_shrinks in
      List.map2
        (fun (_, pref, min_main, _, _) shrink_amount ->
          max min_main (pref - shrink_amount))
        item_infos shrink_amounts

let align_offset available used align =
  match align with
  | `Start -> 0
  | `Center -> max 0 ((available - used) / 2)
  | `End -> max 0 (available - used)
  | `Stretch -> 0

let rec calculate bounds element =
  match element with
  | Box b -> calculate_box bounds b
  | Z_stack z -> calculate_z_stack bounds z
  | Flow f -> calculate_flow bounds f
  | Grid g -> calculate_grid bounds g
  | Scroll s -> calculate_scroll bounds s
  | Text _ | Rich_text _ | Spacer _ ->
      (* Leaf elements: expand width to full bounds to enable proper alignment *)
      let _natural_w, natural_h =
        Element.measure ~width:(Bounds.width bounds) element
      in
      {
        element;
        x = Bounds.x bounds;
        y = Bounds.y bounds;
        width = Bounds.width bounds;
        (* Use full available width *)
        height = min natural_h (Bounds.height bounds);
        children = [];
      }

and calculate_box bounds box_data =
  let element = Element.Box box_data in
  let options = Element.Box.options box_data in
  let children_elements = Element.Box.children box_data in
  let is_expandable el = Element.grow_fact el > 0 in

  let children_elements =
    if
      options.fill
      && (not (List.exists is_expandable children_elements))
      && children_elements <> []
    then
      let spacer = Element.spacer ~flex:1 0 in
      match options.justify with
      | `Start -> children_elements @ [ spacer ]
      | `End -> spacer :: children_elements
      | `Center -> (spacer :: children_elements) @ [ spacer ]
      | `Stretch -> children_elements (* No spacers needed for stretch *)
    else children_elements
  in

  let margin_bounds =
    Bounds.make
      ~x:(Bounds.x bounds + Element.Padding.left options.margin)
      ~y:(Bounds.y bounds + Element.Padding.top options.margin)
      ~width:
        (max 0
           (Bounds.width bounds
           - Element.Padding.left options.margin
           - Element.Padding.right options.margin))
      ~height:
        (max 0
           (Bounds.height bounds
           - Element.Padding.top options.margin
           - Element.Padding.bottom options.margin))
  in

  let natural_w, natural_h =
    Element.measure ~width:(Bounds.width margin_bounds) element
  in
  let resolve_dim value min_v max_v _natural available =
    let max_reasonable = 10_000 in
    let r = Option.value value ~default:available |> min max_reasonable in
    let r = match min_v with Some m -> max m r | _ -> r in
    match max_v with Some m -> min m r | _ -> r
  in
  let box_w =
    resolve_dim options.width options.min_width options.max_width natural_w
      (Bounds.width margin_bounds)
  in
  let box_h =
    resolve_dim options.height options.min_height options.max_height natural_h
      (Bounds.height margin_bounds)
  in

  let border_h =
    Option.map Border.space_h options.border |> Option.value ~default:0
  in
  let border_v =
    Option.map Border.space_v options.border |> Option.value ~default:0
  in
  let content_x =
    Bounds.x margin_bounds
    + (match options.border with
      | None -> 0
      | Some b -> if Border.left b then 1 else 0)
    + Element.Padding.left options.padding
  in
  let content_y =
    Bounds.y margin_bounds
    + (match options.border with
      | None -> 0
      | Some b -> if Border.top b then 1 else 0)
    + Element.Padding.top options.padding
  in
  let content_w =
    max 0
      (box_w - border_h
      - Element.Padding.left options.padding
      - Element.Padding.right options.padding)
  in
  let content_h =
    max 0
      (box_h - border_v
      - Element.Padding.top options.padding
      - Element.Padding.bottom options.padding)
  in

  let children_layouts =
    match options.direction with
    | `Horizontal ->
        let assigned_widths =
          assign_main_sizes ~available_main:content_w ~main_is_width:true
            ~content_cross_size:content_h children_elements options.gap
        in
        (* Count only non-zero width elements for total width with gaps *)
        let non_zero_widths = List.filter (fun w -> w > 0) assigned_widths in
        let total_children_w =
          List.fold_left ( + ) 0 non_zero_widths
          + (options.gap * max 0 (List.length non_zero_widths - 1))
        in
        let justify_offset =
          align_offset content_w total_children_w options.justify
        in
        let rec layout_h_children x_cursor acc last_was_visible = function
          | [] -> List.rev acc
          | (child, assigned_w) :: rest ->
              if assigned_w <= 0 then
                layout_h_children x_cursor acc false
                  rest (* Skip zero-size elements *)
              else
                let gap_to_add = if last_was_visible then options.gap else 0 in
                let child_x = x_cursor + gap_to_add in
                let _child_w, child_h =
                  Element.measure ~width:assigned_w child
                in
                let child_layout_h =
                  if options.align = `Stretch then content_h else child_h
                in
                let align_offset_y =
                  align_offset content_h child_layout_h options.align
                in
                let child_bounds =
                  Bounds.make ~x:child_x
                    ~y:(content_y + align_offset_y)
                    ~width:assigned_w ~height:child_layout_h
                in
                let computed_child = calculate child_bounds child in
                layout_h_children (child_x + assigned_w) (computed_child :: acc)
                  true rest
        in
        layout_h_children
          (content_x + justify_offset)
          [] false
          (List.combine children_elements assigned_widths)
    | `Vertical ->
        let assigned_heights =
          assign_main_sizes ~available_main:content_h ~main_is_width:false
            ~content_cross_size:content_w children_elements options.gap
        in
        (* Count only non-zero height elements for total height with gaps *)
        let non_zero_heights = List.filter (fun h -> h > 0) assigned_heights in
        let total_children_h =
          List.fold_left ( + ) 0 non_zero_heights
          + (options.gap * max 0 (List.length non_zero_heights - 1))
        in
        let justify_offset =
          align_offset content_h total_children_h options.justify
        in
        let rec layout_v_children y_cursor acc last_was_visible = function
          | [] -> List.rev acc
          | (child, assigned_h) :: rest ->
              if assigned_h <= 0 then
                layout_v_children y_cursor acc false
                  rest (* Skip zero-size elements *)
              else
                let gap_to_add = if last_was_visible then options.gap else 0 in
                let child_y = y_cursor + gap_to_add in
                let child_w, _ = Element.measure ~width:content_w child in
                let child_layout_w =
                  if options.align = `Stretch then content_w else child_w
                in
                let align_offset_x =
                  align_offset content_w child_layout_w options.align
                in
                let child_bounds =
                  Bounds.make
                    ~x:(content_x + align_offset_x)
                    ~y:child_y ~width:child_layout_w ~height:assigned_h
                in
                let computed_child = calculate child_bounds child in
                layout_v_children (child_y + assigned_h) (computed_child :: acc)
                  true rest
        in
        layout_v_children
          (content_y + justify_offset)
          [] false
          (List.combine children_elements assigned_heights)
  in
  {
    element;
    x = Bounds.x bounds + Element.Padding.left options.margin;
    y = Bounds.y bounds + Element.Padding.top options.margin;
    width = box_w;
    height = box_h;
    children = children_layouts;
  }

and calculate_z_stack bounds z_data =
  let element = Element.Z_stack z_data in
  let children = Element.Z_stack.children z_data in
  let align = Element.Z_stack.alignment z_data in
  let children_layouts =
    List.map
      (fun child ->
        let child_w, child_h = Element.measure child in
        let x_offset =
          match align with
          | Top_left | Left | Bottom_left -> 0
          | Top | Center | Bottom -> (Bounds.width bounds - child_w + 1) / 2
          | Top_right | Right | Bottom_right -> Bounds.width bounds - child_w
        in
        let y_offset =
          match align with
          | Top_left | Top | Top_right -> 0
          | Left | Center | Right -> (Bounds.height bounds - child_h + 1) / 2
          | Bottom_left | Bottom | Bottom_right ->
              Bounds.height bounds - child_h
        in
        let child_bounds =
          Bounds.make
            ~x:(Bounds.x bounds + x_offset)
            ~y:(Bounds.y bounds + y_offset)
            ~width:child_w ~height:child_h
        in
        calculate child_bounds child)
      children
  in
  {
    element;
    x = Bounds.x bounds;
    y = Bounds.y bounds;
    width = Bounds.width bounds;
    height = Bounds.height bounds;
    children = children_layouts;
  }

and calculate_flow bounds flow_data =
  let element = Element.Flow flow_data in
  let children = Element.Flow.children flow_data in
  let h_gap = Element.Flow.h_gap flow_data in
  let v_gap = Element.Flow.v_gap flow_data in
  let rec wrap_children x_cursor y_cursor current_line_h acc = function
    | [] -> acc
    | child :: rest ->
        let child_w, child_h = Element.measure child in
        let gap = if x_cursor > Bounds.x bounds then h_gap else 0 in
        let next_x = x_cursor + gap + child_w in
        if
          x_cursor > Bounds.x bounds
          && next_x > Bounds.x bounds + Bounds.width bounds
        then
          (* New line *)
          let gap_v = if y_cursor > Bounds.y bounds then v_gap else 0 in
          let new_y = y_cursor + current_line_h + gap_v in
          let child_bounds =
            Bounds.make ~x:(Bounds.x bounds) ~y:new_y ~width:child_w
              ~height:child_h
          in
          let computed = calculate child_bounds child in
          wrap_children
            (Bounds.x bounds + child_w)
            new_y child_h (computed :: acc) rest
        else
          (* Same line *)
          let child_bounds =
            Bounds.make ~x:(x_cursor + gap) ~y:y_cursor ~width:child_w
              ~height:child_h
          in
          let computed = calculate child_bounds child in
          wrap_children
            (x_cursor + gap + child_w)
            y_cursor
            (max current_line_h child_h)
            (computed :: acc) rest
  in
  let children_layouts =
    wrap_children (Bounds.x bounds) (Bounds.y bounds) 0 [] children |> List.rev
  in
  let computed_h =
    List.fold_left
      (fun max_y l -> max max_y (l.y + l.height))
      0 children_layouts
    |> fun h -> h - Bounds.y bounds
  in
  {
    element;
    x = Bounds.x bounds;
    y = Bounds.y bounds;
    width = Bounds.width bounds;
    height = computed_h;
    children = children_layouts;
  }

and calculate_grid bounds grid_data =
  let element = Element.Grid grid_data in
  let children = Element.Grid.children grid_data in
  let cols = Element.Grid.columns grid_data in
  let rows = Element.Grid.rows grid_data in
  let col_spacing = Element.Grid.col_spacing grid_data in
  let row_spacing = Element.Grid.row_spacing grid_data in

  (* Calculate natural size *)
  let natural_w =
    List.fold_left
      (fun acc col -> match col with `Fixed w -> acc + w | _ -> acc)
      0 cols
    + (col_spacing * max 0 (List.length cols - 1))
  in
  let natural_h =
    List.fold_left
      (fun acc row -> match row with `Fixed h -> acc + h | _ -> acc)
      0 rows
    + (row_spacing * max 0 (List.length rows - 1))
  in

  (* Resolve dimensions using resolve_dim like box does *)
  let grid_w = min natural_w (Bounds.width bounds) in
  let grid_h = min natural_h (Bounds.height bounds) in

  let calc_sizes defs available_space spacing =
    let total_fixed =
      List.fold_left
        (fun acc d -> match d with `Fixed s -> acc + s | _ -> acc)
        0 defs
    in
    let flex_weights =
      List.map (function `Flex f -> f | `Fixed _ -> 0) defs
    in
    let total_spacing = spacing * max 0 (List.length defs - 1) in
    let flex_space = max 0 (available_space - total_fixed - total_spacing) in
    let flex_amounts = distribute_fairly flex_space flex_weights in
    List.map2
      (fun def flex_amount ->
        match def with `Fixed s -> s | `Flex _ -> flex_amount)
      defs flex_amounts
  in
  let col_widths = calc_sizes cols grid_w col_spacing in
  let row_heights = calc_sizes rows grid_h row_spacing in

  let rec layout_cells ~x_cursor ~y_cursor ~col ~row acc = function
    | [] -> acc
    | child :: rest ->
        if col >= List.length col_widths then
          let new_y = y_cursor + List.nth row_heights row + row_spacing in
          layout_cells ~x_cursor:(Bounds.x bounds) ~y_cursor:new_y ~col:0
            ~row:(row + 1) acc (child :: rest)
        else if row >= List.length row_heights then acc
          (* Not enough rows defined *)
        else
          let w = List.nth col_widths col in
          let h = List.nth row_heights row in
          let child_bounds =
            Bounds.make ~x:x_cursor ~y:y_cursor ~width:w ~height:h
          in
          let computed = calculate child_bounds child in
          layout_cells
            ~x_cursor:(x_cursor + w + col_spacing)
            ~y_cursor ~col:(col + 1) ~row (computed :: acc) rest
  in
  let children_layouts =
    layout_cells ~x_cursor:(Bounds.x bounds) ~y_cursor:(Bounds.y bounds) ~col:0
      ~row:0 [] children
    |> List.rev
  in
  {
    element;
    x = Bounds.x bounds;
    y = Bounds.y bounds;
    width = grid_w;
    height = grid_h;
    children = children_layouts;
  }

and calculate_scroll bounds scroll_data =
  let element = Element.Scroll scroll_data in
  let child = Element.Scroll.child scroll_data in
  let h_offset = Element.Scroll.h_offset scroll_data in
  let v_offset = Element.Scroll.v_offset scroll_data in
  let child_w, child_h = Element.measure child in
  let viewport_w =
    Element.Scroll.width scroll_data
    |> Option.value ~default:(min (Bounds.width bounds) child_w)
  in
  let viewport_h =
    Element.Scroll.height scroll_data
    |> Option.value ~default:(min (Bounds.height bounds) child_h)
  in
  let visible_x = max 0 (min h_offset (child_w - viewport_w)) in
  let visible_y = max 0 (min v_offset (child_h - viewport_h)) in

  (* The child is laid out in its full size, but offset "behind" the viewport *)
  let child_bounds =
    Bounds.make
      ~x:(Bounds.x bounds - visible_x)
      ~y:(Bounds.y bounds - visible_y)
      ~width:child_w ~height:child_h
  in
  let computed_child = calculate child_bounds child in
  {
    element;
    x = Bounds.x bounds;
    y = Bounds.y bounds;
    width = viewport_w;
    height = viewport_h;
    children = [ computed_child ];
  }
