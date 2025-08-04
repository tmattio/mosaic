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
      let base_size =
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
      in
      let adjusted_size = Size.maybe_add base_size box_sizing_adjustment in
      Size.maybe_apply_aspect_ratio adjusted_size aspect_ratio
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
    let after_clamp =
      Size.map clamped ~f:(function
        | Some v -> Available_space.Definite (max v 0.0)
        | None -> Available_space.Max_content)
    in
    (* Apply padding_border_size as minimum constraint *)
    Size.map2 after_clamp padding_border_size ~f:(fun available_space pb_size ->
        match available_space with
        | Available_space.Definite v ->
            Available_space.Definite (Float.max v pb_size)
        | Available_space.Min_content -> Available_space.Min_content
        | Available_space.Max_content -> Available_space.Max_content)
  in

  let available_grid_space =
    {
      width =
        (match constrained_available_space.width with
        | Definite space ->
            Available_space.Definite
              (space -. content_box_inset.left -. content_box_inset.right)
        | Min_content -> Min_content
        | Max_content -> Max_content);
      height =
        (match constrained_available_space.height with
        | Definite space ->
            Available_space.Definite
              (space -. content_box_inset.top -. content_box_inset.bottom)
        | Min_content -> Min_content
        | Max_content -> Max_content);
    }
  in

  let outer_node_size =
    let base = Size.or_ known_dimensions preferred_size in
    let clamped = Size.maybe_clamp base min_size max_size in
    Size.maybe_max clamped (Size.map padding_border_size ~f:(fun v -> Some v))
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

      (* Helper to get box_generation_mode from display *)
      let box_generation_mode_of_display (d : Style.display) :
          Style.box_generation_mode =
        match d with
        | Style.None -> Style.None
        | Style.Block | Style.Flex | Style.Grid -> Style.Normal
      in

      (* 3. Perform grid item placement (resolves the implicit grid) *)
      (* Filter out absolutely positioned and hidden items for grid placement *)
      let in_flow_child_styles =
        child_styles
        |> List.filter (fun (_, _, style) ->
               box_generation_mode_of_display style.display <> Style.None
               && style.position <> Absolute)
      in

      let grid_col_counts, grid_row_counts =
        Implicit_grid.compute_grid_size_estimate ~explicit_col_count
          ~explicit_row_count
          ~child_styles:
            (List.map (fun (_, _, style) -> style) in_flow_child_styles)
      in

      let items = ref [] in
      let cell_occupancy_matrix =
        with_track_counts grid_col_counts grid_row_counts
      in

      let _ =
        Placement.place_grid_items ~cell_occupancy_matrix ~items
          ~children_styles:in_flow_child_styles
          ~grid_auto_flow:container_style.grid_auto_flow
          ~align_items:
            (Option.value align_items
               ~default:(Style.Alignment.Stretch : Style.Alignment.align_items))
          ~justify_items:
            (Option.value justify_items
               ~default:
                 (Style.Alignment.Stretch : Style.Alignment.justify_items))
      in

      (* Extract final track counts from cell_occupancy_matrix after placement *)
      let final_col_counts = track_counts cell_occupancy_matrix Horizontal in
      let final_row_counts = track_counts cell_occupancy_matrix Vertical in

      (* Initialize tracks using final counts *)
      let columns =
        let track_has_items idx =
          column_is_occupied cell_occupancy_matrix idx
        in
        Explicit_grid.initialize_grid_tracks ~counts:final_col_counts
          ~track_template:(Array.of_list grid_template_columns)
          ~auto_tracks:(Array.of_list grid_auto_columns)
          ~gap:(Size.get container_style.gap Inline)
          ~track_has_items
      in

      let rows =
        let track_has_items idx = row_is_occupied cell_occupancy_matrix idx in
        Explicit_grid.initialize_grid_tracks ~counts:final_row_counts
          ~track_template:(Array.of_list grid_template_rows)
          ~auto_tracks:(Array.of_list grid_auto_rows)
          ~gap:(Size.get container_style.gap Block)
          ~track_has_items
      in

      (* 4. Track sizing *)
      let get_track_size_estimate_for_columns track available_space _tree =
        Grid_helpers.MaxSizing.definite_value track.max_track_sizing_function
          available_space (fun _ptr basis ->
            Tree.resolve_calc_value tree ~ptr:() ~basis)
      in

      let get_track_size_estimate_for_rows track _available_space _tree =
        (* For rows, we use the base_size directly as per Rust implementation *)
        Some track.base_size
      in

      (* Check if we have baseline-aligned items *)
      let has_baseline_aligned_item =
        List.exists
          (fun item -> item.Grid_item.align_self = Style.Alignment.Baseline)
          !items
      in

      (* Create a module that implements LayoutPartialTreeExt for track sizing *)
      let module TreeExt = Tree_intf.LayoutPartialTreeExt (Tree) in
      let tree_module =
        (module TreeExt : Tree_intf.LayoutPartialTreeExt with type t = tree)
      in

      (* Resolve item track indexes before sizing *)
      Track_sizing.resolve_item_track_indexes (Array.of_list !items)
        final_col_counts final_row_counts;

      (* Determine if items cross flexible or intrinsic tracks *)
      Track_sizing.determine_if_item_crosses_flexible_or_intrinsic_tracks
        (Array.of_list !items) columns rows;

      (* Size columns *)
      Track_sizing.track_sizing_algorithm ~tree:tree_module ~tree_val:tree
        ~axis:Inline ~axis_min_size:min_size.width ~axis_max_size:max_size.width
        ~axis_alignment:
          (match container_style.justify_content with
          | Some jc -> jc
          | None -> Style.Alignment.Start)
        ~other_axis_alignment:
          (match container_style.align_content with
          | Some ac -> ac
          | None -> Style.Alignment.Start)
        ~available_grid_space ~inner_node_size ~axis_tracks:columns
        ~other_axis_tracks:rows ~items:(Array.of_list !items)
        ~get_track_size_estimate:get_track_size_estimate_for_columns
        ~has_baseline_aligned_item;

      (* Size rows *)
      Track_sizing.track_sizing_algorithm ~tree:tree_module ~tree_val:tree
        ~axis:Block ~axis_min_size:min_size.height
        ~axis_max_size:max_size.height
        ~axis_alignment:
          (match container_style.align_content with
          | Some ac -> ac
          | None -> Style.Alignment.Start)
        ~other_axis_alignment:
          (match container_style.justify_content with
          | Some jc -> jc
          | None -> Style.Alignment.Start)
        ~available_grid_space ~inner_node_size ~axis_tracks:rows
        ~other_axis_tracks:columns ~items:(Array.of_list !items)
        ~get_track_size_estimate:get_track_size_estimate_for_rows
        ~has_baseline_aligned_item;

      (* Update inner_node_size after initial column sizing *)
      let initial_column_sum =
        Array.fold_left (fun acc track -> acc +. track.base_size) 0.0 columns
      in
      let inner_node_size =
        {
          inner_node_size with
          width =
            (match inner_node_size.width with
            | Some w -> Some w
            | None -> Some initial_column_sum);
        }
      in

      (* Update inner_node_size after initial row sizing *)
      let initial_row_sum =
        Array.fold_left (fun acc track -> acc +. track.base_size) 0.0 rows
      in
      let inner_node_size =
        {
          inner_node_size with
          height =
            (match inner_node_size.height with
            | Some h -> Some h
            | None -> Some initial_row_sum);
        }
      in

      (* Clear available space caches before checking for re-runs *)
      List.iter (fun item -> item.available_space_cache <- None) !items;

      (* Check if column sizing needs to be re-run *)
      let has_percentage_column =
        Array.exists (fun track -> Grid_track.uses_percentage track) columns
      in
      let parent_width_indefinite =
        not (Available_space.is_definite available_space.width)
      in
      let rerun_column_sizing =
        parent_width_indefinite && has_percentage_column
      in

      (* TODO: Also check for min_content_contribution changes in items crossing intrinsic tracks *)

      (* Re-run column sizing if needed *)
      if rerun_column_sizing then (
        (* Clear intrinsic caches *)
        List.iter
          (fun item ->
            item.available_space_cache <- None;
            item.min_content_contribution_cache <-
              {
                width = None;
                height = item.min_content_contribution_cache.height;
              };
            item.max_content_contribution_cache <-
              {
                width = None;
                height = item.max_content_contribution_cache.height;
              };
            item.minimum_contribution_cache <-
              { width = None; height = item.minimum_contribution_cache.height })
          !items;

        (* Re-run column sizing with base_size callback *)
        Track_sizing.track_sizing_algorithm ~tree:tree_module ~tree_val:tree
          ~axis:Inline ~axis_min_size:min_size.width
          ~axis_max_size:max_size.width
          ~axis_alignment:
            (match container_style.justify_content with
            | Some jc -> jc
            | None -> Style.Alignment.Start)
          ~other_axis_alignment:
            (match container_style.align_content with
            | Some ac -> ac
            | None -> Style.Alignment.Start)
          ~available_grid_space ~inner_node_size ~axis_tracks:columns
          ~other_axis_tracks:rows ~items:(Array.of_list !items)
          ~get_track_size_estimate:(fun track _ _ -> Some track.base_size)
          ~has_baseline_aligned_item;

        (* Check if row sizing needs to be re-run *)
        let has_percentage_row =
          Array.exists (fun track -> Grid_track.uses_percentage track) rows
        in
        let parent_height_indefinite =
          not (Available_space.is_definite available_space.height)
        in
        let rerun_row_sizing = parent_height_indefinite && has_percentage_row in

        if rerun_row_sizing then (
          (* Clear intrinsic height caches *)
          List.iter
            (fun item ->
              item.available_space_cache <- None;
              item.min_content_contribution_cache <-
                {
                  width = item.min_content_contribution_cache.width;
                  height = None;
                };
              item.max_content_contribution_cache <-
                {
                  width = item.max_content_contribution_cache.width;
                  height = None;
                };
              item.minimum_contribution_cache <-
                { width = item.minimum_contribution_cache.width; height = None })
            !items;

          (* Re-run row sizing *)
          Track_sizing.track_sizing_algorithm ~tree:tree_module ~tree_val:tree
            ~axis:Block ~axis_min_size:min_size.height
            ~axis_max_size:max_size.height
            ~axis_alignment:
              (match container_style.align_content with
              | Some ac -> ac
              | None -> Style.Alignment.Start)
            ~other_axis_alignment:
              (match container_style.justify_content with
              | Some jc -> jc
              | None -> Style.Alignment.Start)
            ~available_grid_space ~inner_node_size ~axis_tracks:rows
            ~other_axis_tracks:columns ~items:(Array.of_list !items)
            ~get_track_size_estimate:(fun track _ _ -> Some track.base_size)
            ~has_baseline_aligned_item:false
          (* No baseline alignment in vertical axis *)));

      (* 5. Compute final container size *)
      let total_column_size =
        Array.fold_left (fun acc track -> acc +. track.base_size) 0.0 columns
      in
      let total_row_size =
        Array.fold_left (fun acc track -> acc +. track.base_size) 0.0 rows
      in

      (* Compute container size following taffy's approach *)
      let resolved_style_size = Size.or_ known_dimensions preferred_size in
      let container_border_box =
        {
          width =
            ( ( (match resolved_style_size.width with
                | Some w -> w
                | None ->
                    total_column_size +. content_box_inset.left
                    +. content_box_inset.right)
              |> fun w ->
                Math.FloatOption.maybe_clamp w min_size.width max_size.width )
            |> fun w -> max w padding_border_size.width );
          height =
            ( ( (match resolved_style_size.height with
                | Some h -> h
                | None ->
                    total_row_size +. content_box_inset.top
                    +. content_box_inset.bottom)
              |> fun h ->
                Math.FloatOption.maybe_clamp h min_size.height max_size.height
              )
            |> fun h -> max h padding_border_size.height );
        }
      in

      (* If only the container's size has been requested, return now *)
      if run_mode = Layout.Run_mode.Compute_size then
        Layout.Layout_output.of_outer_size container_border_box
      else
        let resolved_inner_node_size =
          {
            width =
              max 0.0
                (container_border_box.width -. content_box_inset.left
               -. content_box_inset.right);
            height =
              max 0.0
                (container_border_box.height -. content_box_inset.top
               -. content_box_inset.bottom);
          }
        in

        (* 6. Re-resolve percentage track base sizes *)
        (* In the case of an indefinitely sized container these resolve to zero during the "Initialise Tracks" step
         and therefore need to be re-resolved here based on the content-sized content box of the container *)
        if not (Available_space.is_definite available_grid_space.width) then
          Array.iter
            (fun column ->
              let min_size =
                Grid_helpers.MinSizing.definite_value
                  column.min_track_sizing_function
                  (Some resolved_inner_node_size.width) (fun _ptr basis ->
                    Tree.resolve_calc_value tree ~ptr:() ~basis)
              in
              let max_size =
                Grid_helpers.MaxSizing.definite_value
                  column.max_track_sizing_function
                  (Some resolved_inner_node_size.width) (fun _ptr basis ->
                    Tree.resolve_calc_value tree ~ptr:() ~basis)
              in
              column.base_size <-
                Math.FloatOption.maybe_clamp column.base_size min_size max_size)
            columns;

        if not (Available_space.is_definite available_grid_space.height) then
          Array.iter
            (fun row ->
              let min_size =
                Grid_helpers.MinSizing.definite_value
                  row.min_track_sizing_function
                  (Some resolved_inner_node_size.height) (fun _ptr basis ->
                    Tree.resolve_calc_value tree ~ptr:() ~basis)
              in
              let max_size =
                Grid_helpers.MaxSizing.definite_value
                  row.max_track_sizing_function
                  (Some resolved_inner_node_size.height) (fun _ptr basis ->
                    Tree.resolve_calc_value tree ~ptr:() ~basis)
              in
              row.base_size <-
                Math.FloatOption.maybe_clamp row.base_size min_size max_size)
            rows;

        (* 7. Align tracks *)
        (* Always align tracks, not just when there's extra space *)
        Grid_alignment.align_tracks
          ~grid_container_content_box_size:resolved_inner_node_size.width
          ~padding:{ start = padding.left; end_ = padding.right }
          ~border:{ start = border.left; end_ = border.right }
          ~tracks:columns ~track_alignment_style:justify_content;

        Grid_alignment.align_tracks
          ~grid_container_content_box_size:resolved_inner_node_size.height
          ~padding:{ start = padding.top; end_ = padding.bottom }
          ~border:{ start = border.top; end_ = border.bottom }
          ~tracks:rows ~track_alignment_style:align_content;

        (* 7. Size and position items *)
        let container_alignment_styles =
          { Grid_alignment.horizontal = align_items; vertical = justify_items }
        in

        let content_size = ref Size.zero in

        List.iter
          (fun item ->
            (* Compute grid area using the resolved track indexes *)
            let grid_area =
              {
                left = columns.(item.column_indexes.start + 1).offset;
                right = columns.(item.column_indexes.end_).offset;
                top = rows.(item.row_indexes.start + 1).offset;
                bottom = rows.(item.row_indexes.end_).offset;
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
                ~baseline_shim:item.baseline_shim ~style:item_style
                ~calc:calc_fn ~perform_child_layout:perform_layout_fn
                ~set_unrounded_layout:set_layout_fn
            in

            content_size :=
              {
                width = max !content_size.width contribution.width;
                height = max !content_size.height contribution.height;
              })
          !items;

        (* 8. Position hidden and absolutely positioned children *)
        List.iter
          (fun (index, child_node, child_style) ->
            (* Position hidden child *)
            if box_generation_mode_of_display child_style.display = Style.None
            then (
              Tree.set_unrounded_layout tree child_node
                (Layout.Layout.with_order (List.length !items + index));
              let _ =
                Tree.compute_child_layout tree child_node
                  {
                    run_mode = Layout.Run_mode.Perform_hidden_layout;
                    sizing_mode = Layout.Sizing_mode.Inherent_size;
                    axis = Layout.Requested_axis.Both;
                    known_dimensions = Size.none;
                    parent_size = Size.none;
                    available_space =
                      {
                        width = Style.Available_space.Max_content;
                        height = Style.Available_space.Max_content;
                      };
                    vertical_margins_are_collapsible = line_false;
                  }
              in
              ())
            else if child_style.position = Style.Absolute then
              (* Position absolutely positioned child *)
              (* Convert grid placement to origin zero coordinates *)
              let col_placement_oz =
                {
                  start =
                    Placement.to_origin_zero_placement
                      child_style.grid_column.start grid_col_counts.explicit;
                  end_ =
                    Placement.to_origin_zero_placement
                      child_style.grid_column.end_ grid_col_counts.explicit;
                }
              in
              let row_placement_oz =
                {
                  start =
                    Placement.to_origin_zero_placement
                      child_style.grid_row.start grid_row_counts.explicit;
                  end_ =
                    Placement.to_origin_zero_placement child_style.grid_row.end_
                      grid_row_counts.explicit;
                }
              in

              (* Resolve absolutely positioned grid tracks *)
              let maybe_col_indexes =
                Placement.resolve_absolutely_positioned_grid_tracks
                  col_placement_oz
              in
              let maybe_row_indexes =
                Placement.resolve_absolutely_positioned_grid_tracks
                  row_placement_oz
              in

              (* Convert to track vector indexes *)
              let maybe_col_start_index =
                match maybe_col_indexes.start with
                | Some line ->
                    let idx =
                      Grid_track_counts.oz_line_to_next_track grid_col_counts
                        line
                    in
                    if idx >= 0 && idx < Array.length columns then Some idx
                    else None
                | None -> None
              in
              let maybe_col_end_index =
                match maybe_col_indexes.end_ with
                | Some line ->
                    let idx =
                      Grid_track_counts.oz_line_to_next_track grid_col_counts
                        line
                    in
                    if idx >= 0 && idx < Array.length columns then Some idx
                    else None
                | None -> None
              in
              let maybe_row_start_index =
                match maybe_row_indexes.start with
                | Some line ->
                    let idx =
                      Grid_track_counts.oz_line_to_next_track grid_row_counts
                        line
                    in
                    if idx >= 0 && idx < Array.length rows then Some idx
                    else None
                | None -> None
              in
              let maybe_row_end_index =
                match maybe_row_indexes.end_ with
                | Some line ->
                    let idx =
                      Grid_track_counts.oz_line_to_next_track grid_row_counts
                        line
                    in
                    if idx >= 0 && idx < Array.length rows then Some idx
                    else None
                | None -> None
              in

              (* Calculate grid area based on resolved positions *)
              let grid_area =
                {
                  top =
                    (match maybe_row_start_index with
                    | Some idx -> rows.(idx).offset
                    | None -> border.top);
                  bottom =
                    (match maybe_row_end_index with
                    | Some idx -> rows.(idx).offset
                    | None ->
                        container_border_box.height -. border.bottom
                        -. scrollbar_gutter.y);
                  left =
                    (match maybe_col_start_index with
                    | Some idx -> columns.(idx).offset
                    | None -> border.left);
                  right =
                    (match maybe_col_end_index with
                    | Some idx -> columns.(idx).offset
                    | None ->
                        container_border_box.width -. border.right
                        -. scrollbar_gutter.x);
                }
              in

              let order = Int32.of_int (List.length !items + index) in
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
                Grid_alignment.align_and_position_item ~node:child_node ~order
                  ~grid_area ~container_alignment_styles ~baseline_shim:0.0
                  ~style:child_style ~calc:calc_fn
                  ~perform_child_layout:perform_layout_fn
                  ~set_unrounded_layout:set_layout_fn
              in

              content_size :=
                {
                  width = max !content_size.width contribution.width;
                  height = max !content_size.height contribution.height;
                })
          child_styles;

        (* Determine the grid container baseline *)
        let grid_container_baseline : float option =
          if List.length !items = 0 then None
          else
            (* Sort items by row start position *)
            let sorted_items =
              List.sort
                (fun a b -> compare a.row_indexes.start b.row_indexes.start)
                !items
            in

            (* Get the row index of the first row containing items *)
            let first_row = (List.hd sorted_items).row_indexes.start in

            (* Get all items in the first row *)
            let first_row_items =
              List.filter
                (fun item -> item.row_indexes.start = first_row)
                sorted_items
            in

            (* Check if any items in this row are baseline aligned *)
            let row_has_baseline_item =
              List.exists
                (fun item -> item.align_self = Style.Alignment.Baseline)
                first_row_items
            in

            (* Select the item to use for baseline *)
            let baseline_item =
              if row_has_baseline_item then
                List.find
                  (fun item -> item.align_self = Style.Alignment.Baseline)
                  first_row_items
              else List.hd first_row_items
            in

            (* Calculate baseline as y_position + baseline (or height if baseline is None) *)
            let baseline_value =
              baseline_item.y_position
              +.
              match baseline_item.baseline with
              | Some b -> b
              | None -> baseline_item.height
            in
            Some baseline_value
        in

        (* Return final layout *)
        Layout.Layout_output.of_sizes_and_baselines ~size:container_border_box
          ~content_size:!content_size
          ~first_baselines:{ x = None; y = grid_container_baseline }
