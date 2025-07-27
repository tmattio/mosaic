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
let geometry l = (l.x, l.y, l.width, l.height)
let children l = l.children

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
        Drawing.distribute_fairly extra_space
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
      let shrink_amounts =
        Drawing.distribute_fairly shortage weighted_shrinks
      in
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

let rec calculate_internal bounds element =
  match element with
  | Box b -> calculate_box bounds b
  | Z_stack z -> calculate_z_stack bounds z
  | Flow f -> calculate_flow bounds f
  | Grid g -> calculate_grid bounds g
  | Scroll s -> calculate_scroll bounds s
  | Canvas c -> calculate_canvas bounds c
  | Spacer _ ->
      (* Spacers should use full bounds dimensions *)
      {
        element;
        x = Bounds.x bounds;
        y = Bounds.y bounds;
        width = Bounds.width bounds;
        height = Bounds.height bounds;
        children = [];
      }
  | Text _ | Rich_text _ ->
      (* Text elements: expand width to full bounds to enable proper alignment *)
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

  (* Helper to insert spacers between children for fill mode *)
  let insert_spacers children =
    if List.length children <= 1 then children
    else
      let spacer = Element.spacer ~flex:1 0 in
      let rec insert = function
        | [] -> []
        | [ x ] -> [ x ]
        | x :: xs -> x :: spacer :: insert xs
      in
      insert children
  in

  let children_elements =
    if
      options.fill
      && (not (List.exists is_expandable children_elements))
      && children_elements <> []
    then insert_spacers children_elements
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
                  if options.align = `Stretch && Element.shrink_fact child = 0
                  then content_h
                  else child_h
                in
                let align_offset_y =
                  align_offset content_h child_layout_h options.align
                in
                let child_bounds =
                  Bounds.make ~x:child_x
                    ~y:(content_y + align_offset_y)
                    ~width:assigned_w ~height:child_layout_h
                in
                let computed_child = calculate_internal child_bounds child in
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
                  if options.align = `Stretch && Element.shrink_fact child = 0
                  then content_w
                  else child_w
                in
                let align_offset_x =
                  align_offset content_w child_layout_w options.align
                in
                let child_bounds =
                  Bounds.make
                    ~x:(content_x + align_offset_x)
                    ~y:child_y ~width:child_layout_w ~height:assigned_h
                in
                let computed_child = calculate_internal child_bounds child in
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
          | Top | Center | Bottom -> (Bounds.width bounds - child_w) / 2
          | Top_right | Right | Bottom_right -> Bounds.width bounds - child_w
        in
        let y_offset =
          match align with
          | Top_left | Top | Top_right -> 0
          | Left | Center | Right -> (Bounds.height bounds - child_h) / 2
          | Bottom_left | Bottom | Bottom_right ->
              Bounds.height bounds - child_h
        in
        let child_bounds =
          Bounds.make
            ~x:(Bounds.x bounds + x_offset)
            ~y:(Bounds.y bounds + y_offset)
            ~width:child_w ~height:child_h
        in
        calculate_internal child_bounds child)
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
  let expanded = Element.expand_children children in
  let h_gap = Element.Flow.h_gap flow_data in
  let v_gap = Element.Flow.v_gap flow_data in
  let line_start = Bounds.x bounds in
  let rec wrap_children x_cursor y_cursor current_line_h acc rem_expanded =
    match rem_expanded with
    | [] -> List.rev acc
    | ec :: rest ->
        let child = ec.elem in
        let is_new = ec.is_new_item in
        let is_hard = ec.is_hard_break in
        let child_w, _child_h = Element.measure ~width:max_int child in
        let min_w = Element.min_width child in
        let can_shrink = Element.shrink_fact child > 0 in
        let gap = if x_cursor > line_start then h_gap else 0 in
        let remaining =
          Bounds.x bounds + Bounds.width bounds - x_cursor - gap
        in
        let would_fit_min = min_w <= remaining in
        let is_curr_line_start = x_cursor = line_start in
        if is_hard && not is_curr_line_start then
          (* Force wrap for hard break *)
          let new_y = y_cursor + current_line_h + v_gap in
          wrap_children line_start new_y 0 acc (ec :: rest)
        else if (not would_fit_min) && not is_curr_line_start then
          (* Normal wrap *)
          let wrap_v = if is_new || is_hard then v_gap else 0 in
          let new_y = y_cursor + current_line_h + wrap_v in
          wrap_children line_start new_y 0 acc (ec :: rest)
        else
          (* first try to keep the preferred width, possibly shrinking *)
          let assigned_w =
            if child_w <= remaining then child_w
            else if can_shrink then max min_w remaining
            else child_w
          in
          let child_x = x_cursor + gap in
          if child_x + assigned_w > Bounds.x bounds + Bounds.width bounds then
            let wrap_v = if is_new || is_hard then v_gap else 0 in
            wrap_children line_start
              (y_cursor + current_line_h + wrap_v)
              0 acc (ec :: rest)
          else
            (* place the child normally *)
            let _, assigned_h = Element.measure ~width:assigned_w child in
            let child_bounds =
              Bounds.make ~x:child_x ~y:y_cursor ~width:assigned_w
                ~height:assigned_h
            in
            let computed = calculate_internal child_bounds child in
            let new_current_h = max current_line_h assigned_h in
            let new_x = child_x + assigned_w in
            wrap_children new_x y_cursor new_current_h (computed :: acc) rest
  in
  let children_layouts =
    wrap_children line_start (Bounds.y bounds) 0 [] expanded
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
  let columns = Element.Grid.columns grid_data in
  let rows = Element.Grid.rows grid_data in
  let col_spacing = Element.Grid.col_spacing grid_data in
  let row_spacing = Element.Grid.row_spacing grid_data in
  let num_cols = List.length columns in
  let num_rows = List.length rows in
  if num_cols = 0 || num_rows = 0 then
    {
      element;
      x = Bounds.x bounds;
      y = Bounds.y bounds;
      width = 0;
      height = 0;
      children = [];
    }
  else
    let num_cells = min (List.length children) (num_cols * num_rows) in
    let col_mins = Array.make num_cols 0 in
    let row_mins = Array.make num_rows 0 in
    List.iteri
      (fun i child ->
        if i < num_cells then (
          let r = i / num_cols in
          let c = i mod num_cols in
          col_mins.(c) <- max col_mins.(c) (Element.min_width child);
          row_mins.(r) <- max row_mins.(r) (Element.min_height child)))
      children;
    let col_mins_list = Array.to_list col_mins in
    let row_mins_list = Array.to_list row_mins in
    let col_widths =
      Drawing.compute_sizes ~defs:columns ~mins:col_mins_list
        ~available:(Bounds.width bounds) ~spacing:col_spacing
    in
    let row_heights =
      Drawing.compute_sizes ~defs:rows ~mins:row_mins_list
        ~available:(Bounds.height bounds) ~spacing:row_spacing
    in
    let grid_w =
      List.fold_left ( + ) 0 col_widths + (col_spacing * max 0 (num_cols - 1))
    in
    let grid_h =
      List.fold_left ( + ) 0 row_heights + (row_spacing * max 0 (num_rows - 1))
    in
    let rec layout_cells ~x_cursor ~y_cursor ~col ~row acc = function
      | [] -> acc
      | child :: rest ->
          if col >= num_cols then
            let new_y = y_cursor + List.nth row_heights row + row_spacing in
            layout_cells ~x_cursor:(Bounds.x bounds) ~y_cursor:new_y ~col:0
              ~row:(row + 1) acc (child :: rest)
          else if row >= num_rows then acc
          else
            let w = List.nth col_widths col in
            let h = List.nth row_heights row in
            let child_bounds =
              Bounds.make ~x:x_cursor ~y:y_cursor ~width:w ~height:h
            in
            let computed = calculate_internal child_bounds child in
            layout_cells
              ~x_cursor:(x_cursor + w + col_spacing)
              ~y_cursor ~col:(col + 1) ~row (computed :: acc) rest
    in
    let children_layouts =
      layout_cells ~x_cursor:(Bounds.x bounds) ~y_cursor:(Bounds.y bounds)
        ~col:0 ~row:0 [] children
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
  (* When child is smaller than viewport, offset should be 0 *)
  let visible_x =
    if child_w <= viewport_w then 0
    else max 0 (min h_offset (child_w - viewport_w))
  in
  let visible_y =
    if child_h <= viewport_h then 0
    else max 0 (min v_offset (child_h - viewport_h))
  in

  (* keep the child at its natural size – clipping is done while rendering *)
  let child_bounds =
    Bounds.make
      ~x:(Bounds.x bounds - visible_x)
      ~y:(Bounds.y bounds - visible_y)
      ~width:child_w ~height:child_h
  in
  let computed_child = calculate_internal child_bounds child in
  {
    element;
    x = Bounds.x bounds;
    y = Bounds.y bounds;
    width = viewport_w;
    height = viewport_h;
    children = [ computed_child ];
  }

and calculate_canvas bounds canvas_data =
  let w =
    Option.value (Canvas.width canvas_data) ~default:(Bounds.width bounds)
  in
  let h =
    Option.value (Canvas.height canvas_data) ~default:(Bounds.height bounds)
  in
  let element = Element.Canvas canvas_data in
  {
    element;
    x = Bounds.x bounds;
    y = Bounds.y bounds;
    width = w;
    height = h;
    children = [];
  }

let calculate ~x ~y ~width ~height element =
  let bounds = Bounds.make ~x ~y ~width ~height in
  calculate_internal bounds element
