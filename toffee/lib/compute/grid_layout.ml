(** mod.ml
    ---------------------------------------------------------------------------
    This module is a partial implementation of the CSS Grid Level 1
    specification https://www.w3.org/TR/css-grid-1
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

open Geometry
open Style

(* Re-export types *)
module Explicit_grid = Explicit_grid
module Implicit_grid = Implicit_grid
module Placement = Grid_placement
module Track_sizing = Grid_track_sizing
open Grid_cell_occupancy
open Grid_track
open Grid_item

(** Grid layout algorithm This consists of a few phases:
    - Resolving the explicit grid
    - Placing items (which also resolves the implicit grid)
    - Track (row/column) sizing
    - Alignment & Final item placement *)
let compute_grid_layout (type tree)
    (module Tree : Tree_intf.LayoutGridContainer
      with type t = tree
       and type grid_container_style = Style.style
       and type grid_item_style = Style.style) (tree : tree)
    (node : Node.Node_id.t) (inputs : Layout.Layout_input.t) =
  let open Layout.Layout_input in
  let { known_dimensions; parent_size; available_space; run_mode; _ } =
    inputs
  in

  let container_style = Tree.get_grid_container_style tree node in

  (* 1. Compute "available grid space"
     https://www.w3.org/TR/css-grid-1/#available-grid-space *)
  let aspect_ratio = container_style.aspect_ratio in
  let padding =
    Resolve.resolve_or_zero_rect_with_size
      Resolve.resolve_or_zero_length_percentage container_style.padding
      parent_size (fun _ptr basis ->
        Tree.resolve_calc_value tree ~ptr:() ~basis)
  in
  let border =
    Resolve.resolve_or_zero_rect_with_size
      Resolve.resolve_or_zero_length_percentage container_style.border
      parent_size (fun _ptr basis ->
        Tree.resolve_calc_value tree ~ptr:() ~basis)
  in
  let padding_border = Rect.add padding border in
  let padding_border_size = Rect.sum_axes padding_border in
  let box_sizing_adjustment =
    if container_style.box_sizing = Border_box then Size.zero
    else padding_border_size
  in

  let min_size =
    Size.maybe_apply_aspect_ratio
      (Size.maybe_add
         {
           width =
             Style.Dimension.maybe_resolve container_style.min_size.width
               parent_size.width (fun _ptr basis ->
                 Tree.resolve_calc_value tree ~ptr:() ~basis);
           height =
             Style.Dimension.maybe_resolve container_style.min_size.height
               parent_size.height (fun _ptr basis ->
                 Tree.resolve_calc_value tree ~ptr:() ~basis);
         }
         box_sizing_adjustment)
      aspect_ratio
  in
  let max_size =
    Size.maybe_apply_aspect_ratio
      (Size.maybe_add
         {
           width =
             Style.Dimension.maybe_resolve container_style.max_size.width
               parent_size.width (fun _ptr basis ->
                 Tree.resolve_calc_value tree ~ptr:() ~basis);
           height =
             Style.Dimension.maybe_resolve container_style.max_size.height
               parent_size.height (fun _ptr basis ->
                 Tree.resolve_calc_value tree ~ptr:() ~basis);
         }
         box_sizing_adjustment)
      aspect_ratio
  in
  let preferred_size =
    if inputs.sizing_mode = Layout.Sizing_mode.Inherent_size then
      Size.maybe_apply_aspect_ratio
        (Size.maybe_add
           {
             width =
               Style.Dimension.maybe_resolve container_style.size.width
                 parent_size.width (fun _ptr basis ->
                   Tree.resolve_calc_value tree ~ptr:() ~basis);
             height =
               Style.Dimension.maybe_resolve container_style.size.height
                 parent_size.height (fun _ptr basis ->
                   Tree.resolve_calc_value tree ~ptr:() ~basis);
           }
           box_sizing_adjustment)
        aspect_ratio
    else Size.none
  in

  (* Scrollbar gutters are reserved when the `overflow` property is set to `Overflow::Scroll`.
     However, the axis are switched (transposed) because a node that scrolls vertically needs
     *horizontal* space to be reserved for a scrollbar *)
  let scrollbar_gutter =
    let overflow = container_style.overflow in
    {
      x = (if overflow.y = Scroll then container_style.scrollbar_width else 0.0);
      y = (if overflow.x = Scroll then container_style.scrollbar_width else 0.0);
    }
  in

  (* TODO: make side configurable based on the `direction` property *)
  let content_box_inset =
    {
      left = padding_border.left;
      right = padding_border.right +. scrollbar_gutter.x;
      top = padding_border.top;
      bottom = padding_border.bottom +. scrollbar_gutter.y;
    }
  in

  let align_content =
    Option.value container_style.align_content ~default:Style.Alignment.Stretch
  in
  let justify_content =
    Option.value container_style.justify_content
      ~default:Style.Alignment.Stretch
  in
  let align_items = container_style.align_items in
  let justify_items = container_style.justify_items in

  (* Note: we avoid accessing the grid rows/columns methods more than once as this can
     cause an expensive-ish computation *)
  let grid_template_columns = container_style.grid_template_columns in
  let grid_template_rows = container_style.grid_template_rows in
  let grid_auto_columns = container_style.grid_auto_columns in
  let grid_auto_rows = container_style.grid_auto_rows in

  let constrained_available_space =
    let base =
      match (known_dimensions.width, known_dimensions.height) with
      | Some w, Some h ->
          {
            width = Available_space.Definite w;
            height = Available_space.Definite h;
          }
      | _ -> (
          match (preferred_size.width, preferred_size.height) with
          | Some w, Some h ->
              {
                width = Available_space.Definite w;
                height = Available_space.Definite h;
              }
          | Some w, None ->
              {
                width = Available_space.Definite w;
                height = available_space.height;
              }
          | None, Some h ->
              {
                width = available_space.width;
                height = Available_space.Definite h;
              }
          | None, None -> available_space)
    in
    let clamped =
      Size.maybe_clamp
        (Size.map base ~f:(function
          | Available_space.Definite v -> Some v
          | _ -> None))
        min_size max_size
    in
    Size.map clamped ~f:(function
      | Some v -> Available_space.Definite (max v 0.0)
      | None -> Available_space.Max_content)
  in

  let available_grid_space =
    {
      width =
        (match constrained_available_space.width with
        | Definite space ->
            Some (space -. content_box_inset.left -. content_box_inset.right)
        | _ -> None);
      height =
        (match constrained_available_space.height with
        | Definite space ->
            Some (space -. content_box_inset.top -. content_box_inset.bottom)
        | _ -> None);
    }
  in

  let outer_node_size =
    let base = Size.or_ known_dimensions preferred_size in
    Size.maybe_clamp base min_size max_size
    |> Size.map ~f:(function Some v -> Some (max v 0.0) | None -> None)
  in

  let inner_node_size =
    {
      width =
        Option.map
          (fun w -> w -. content_box_inset.left -. content_box_inset.right)
          outer_node_size.width;
      height =
        Option.map
          (fun h -> h -. content_box_inset.top -. content_box_inset.bottom)
          outer_node_size.height;
    }
  in

  (* Quick return for ComputeSize mode *)
  match (run_mode, outer_node_size.width, outer_node_size.height) with
  | Compute_size, Some width, Some height ->
      Layout.Layout_output.of_outer_size { width; height }
  | _ ->
      (* Get child styles *)
      (* Convert child_iter to list *)
      let child_count = Tree.child_count tree node in
      let rec collect_children acc i =
        if i >= child_count then List.rev acc
        else collect_children (Tree.get_child_id tree node i :: acc) (i + 1)
      in
      let children = collect_children [] 0 in
      let child_styles =
        children
        |> List.mapi (fun index child_node ->
               (index, child_node, Tree.get_grid_child_style tree child_node))
      in

      (* 2. Resolve the explicit grid *)
      let auto_fit_container_size =
        let base =
          match (outer_node_size.width, outer_node_size.height) with
          | Some w, Some h -> { width = Some w; height = Some h }
          | _ -> Size.or_ max_size min_size
        in
        {
          width =
            Option.map
              (fun w -> w -. content_box_inset.left -. content_box_inset.right)
              base.width;
          height =
            Option.map
              (fun h -> h -. content_box_inset.top -. content_box_inset.bottom)
              base.height;
        }
      in

      let style_gap_as_dimension =
        {
          width =
            (match container_style.gap.width with
            | Length_percentage.Length v -> Dimension.Length v
            | Length_percentage.Percent v -> Dimension.Percent v);
          height =
            (match container_style.gap.height with
            | Length_percentage.Length v -> Dimension.Length v
            | Length_percentage.Percent v -> Dimension.Percent v);
        }
      in

      let explicit_col_count =
        Explicit_grid.compute_explicit_grid_size_in_axis
          ~style_size:container_style.size
          ~style_max_size:container_style.max_size
          ~style_gap:style_gap_as_dimension
          ~template:(Array.of_list grid_template_columns)
          ~inner_container_size:auto_fit_container_size
          ~calc:(Tree.resolve_calc_value tree)
          ~axis:Horizontal
      in
      let explicit_row_count =
        Explicit_grid.compute_explicit_grid_size_in_axis
          ~style_size:container_style.size
          ~style_max_size:container_style.max_size
          ~style_gap:style_gap_as_dimension
          ~template:(Array.of_list grid_template_rows)
          ~inner_container_size:auto_fit_container_size
          ~calc:(Tree.resolve_calc_value tree)
          ~axis:Vertical
      in

      (* 3. Perform grid item placement (resolves the implicit grid) *)
      let grid_col_counts, grid_row_counts =
        Implicit_grid.compute_grid_size_estimate ~explicit_col_count
          ~explicit_row_count
          ~child_styles:(List.map (fun (_, _, style) -> style) child_styles)
      in

      let items = ref [] in
      let cell_occupancy_matrix =
        with_track_counts grid_col_counts grid_row_counts
      in

      let _ =
        Placement.place_grid_items ~cell_occupancy_matrix ~items
          ~children_styles:child_styles
          ~grid_auto_flow:container_style.grid_auto_flow
          ~align_items:
            (Option.value align_items
               ~default:(Style.Alignment.Stretch : Style.Alignment.align_items))
          ~justify_items:
            (Option.value justify_items
               ~default:
                 (Style.Alignment.Stretch : Style.Alignment.justify_items))
      in

      (* Initialize tracks *)
      let columns =
        let track_has_items idx =
          column_is_occupied cell_occupancy_matrix idx
        in
        Explicit_grid.initialize_grid_tracks ~counts:grid_col_counts
          ~track_template:(Array.of_list grid_template_columns)
          ~auto_tracks:(Array.of_list grid_auto_columns)
          ~gap:(Size.get container_style.gap Inline)
          ~track_has_items
      in

      let rows =
        let track_has_items idx = row_is_occupied cell_occupancy_matrix idx in
        Explicit_grid.initialize_grid_tracks ~counts:grid_row_counts
          ~track_template:(Array.of_list grid_template_rows)
          ~auto_tracks:(Array.of_list grid_auto_rows)
          ~gap:(Size.get container_style.gap Block)
          ~track_has_items
      in

      (* 4. Track sizing *)
      (* This is a simplified version - full implementation would be much more complex *)
      let get_track_size_estimate track available_space =
        let _ = available_space in
        (* Mark as intentionally unused *)
        if track.base_size > 0.0 then Some track.base_size
        else if track.growth_limit < Float.infinity then Some track.growth_limit
        else None
      in

      (* Size columns *)
      Track_sizing.track_sizing_algorithm ~axis:Inline
        ~axis_min_size:min_size.width ~axis_max_size:max_size.width
        ~axis_available_grid_space:available_grid_space.width
        ~axis_tracks:columns ~axis_counts:grid_col_counts
        ~items:(Array.of_list !items) ~get_track_size_estimate ~inner_node_size;

      (* Size rows *)
      Track_sizing.track_sizing_algorithm ~axis:Block
        ~axis_min_size:min_size.height ~axis_max_size:max_size.height
        ~axis_available_grid_space:available_grid_space.height ~axis_tracks:rows
        ~axis_counts:grid_row_counts ~items:(Array.of_list !items)
        ~get_track_size_estimate ~inner_node_size;

      (* 5. Compute final container size *)
      let total_column_size =
        Array.fold_left (fun acc track -> acc +. track.base_size) 0.0 columns
      in
      let total_row_size =
        Array.fold_left (fun acc track -> acc +. track.base_size) 0.0 rows
      in

      let resolved_inner_node_size =
        {
          width = Option.value inner_node_size.width ~default:total_column_size;
          height = Option.value inner_node_size.height ~default:total_row_size;
        }
      in

      (* 6. Align tracks *)
      if resolved_inner_node_size.width > total_column_size then
        Grid_alignment.align_tracks
          ~grid_container_content_box_size:resolved_inner_node_size.width
          ~padding:{ start = padding_border.left; end_ = padding_border.right }
          ~border:{ start = 0.0; end_ = 0.0 }
          ~tracks:columns ~track_alignment_style:justify_content;

      if resolved_inner_node_size.height > total_row_size then
        Grid_alignment.align_tracks
          ~grid_container_content_box_size:resolved_inner_node_size.height
          ~padding:{ start = padding_border.top; end_ = padding_border.bottom }
          ~border:{ start = 0.0; end_ = 0.0 }
          ~tracks:rows ~track_alignment_style:align_content;

      (* 7. Size and position items *)
      let container_alignment_styles =
        { Grid_alignment.horizontal = align_items; vertical = justify_items }
      in

      let content_size = ref Size.zero in

      List.iter
        (fun item ->
          (* Compute grid area *)
          let grid_area =
            {
              left =
                columns.(Grid_track_counts.oz_line_to_next_track grid_col_counts
                           item.column.start)
                  .offset;
              right =
                columns.(Grid_track_counts.oz_line_to_next_track grid_col_counts
                           item.column.end_)
                  .offset;
              top =
                rows.(Grid_track_counts.oz_line_to_next_track grid_row_counts
                        item.row.start)
                  .offset;
              bottom =
                rows.(Grid_track_counts.oz_line_to_next_track grid_row_counts
                        item.row.end_)
                  .offset;
            }
          in

          let item_style = Tree.get_grid_child_style tree item.node in
          let calc_fn = Tree.resolve_calc_value tree in
          let perform_layout_fn node size parent_size available_space
              sizing_mode margins_collapsible =
            Tree.compute_child_layout tree node
              {
                run_mode = Layout.Run_mode.Perform_layout;
                sizing_mode;
                axis = Layout.Requested_axis.Both;
                known_dimensions = size;
                parent_size;
                available_space;
                vertical_margins_are_collapsible = margins_collapsible;
              }
          in
          let set_layout_fn = Tree.set_unrounded_layout tree in

          let contribution, _, _ =
            Grid_alignment.align_and_position_item ~node:item.node
              ~order:(Int32.of_int item.source_order)
              ~grid_area ~container_alignment_styles
              ~baseline_shim:item.baseline_shim ~style:item_style ~calc:calc_fn
              ~perform_child_layout:perform_layout_fn
              ~set_unrounded_layout:set_layout_fn
          in

          content_size :=
            {
              width = max !content_size.width contribution.width;
              height = max !content_size.height contribution.height;
            })
        !items;

      (* Return final layout *)
      let container_size =
        {
          width =
            resolved_inner_node_size.width +. content_box_inset.left
            +. content_box_inset.right;
          height =
            resolved_inner_node_size.height +. content_box_inset.top
            +. content_box_inset.bottom;
        }
      in

      Layout.Layout_output.of_sizes_and_baselines ~size:container_size
        ~content_size:!content_size ~first_baselines:{ x = None; y = None }
