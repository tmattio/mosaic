(** track_sizing.ml
    ---------------------------------------------------------------------------
    Implements the track sizing algorithm
    https://www.w3.org/TR/css-grid-1/#layout-algorithm
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

open Geometry
open Grid_track
open Grid_item
open Grid_helpers
open Grid_grid_axis_helpers
open Grid_coordinates
open Grid_track_counts

module Abstract_axis = struct
  let other = abstract_other
end

module Size = struct
  include Size
end

type item_batcher = {
  axis : abstract_axis;  (** The axis in which the ItemBatcher is operating *)
  mutable index_offset : int;  (** The starting index of the current batch *)
  mutable current_span : int;  (** The span of the items in the current batch *)
  mutable current_is_flex : bool;
      (** Whether the current batch of items cross a flexible track *)
}
(** Takes an axis, and a list of grid items sorted firstly by whether they cross
    a flex track in the specified axis (items that don't cross a flex track
    first) and then by the number of tracks they cross in specified axis
    (ascending order). *)

let create_item_batcher axis =
  { axis; index_offset = 0; current_span = 1; current_is_flex = false }

(** Get next batch of items *)
let next_batch batcher items : (Grid_item.t array * bool) option =
  if batcher.current_is_flex || batcher.index_offset >= Array.length items then
    None
  else
    let item = items.(batcher.index_offset) in
    batcher.current_span <- span item batcher.axis;
    batcher.current_is_flex <- crosses_flexible_track item batcher.axis;

    let next_index_offset =
      if batcher.current_is_flex then Array.length items
      else
        match
          Array.find_index
            (fun item ->
              crosses_flexible_track item batcher.axis
              || span item batcher.axis > batcher.current_span)
            (Array.sub items batcher.index_offset
               (Array.length items - batcher.index_offset))
        with
        | Some idx -> batcher.index_offset + idx
        | None -> Array.length items
    in

    let batch =
      Array.sub items batcher.index_offset
        (next_index_offset - batcher.index_offset)
    in
    batcher.index_offset <- next_index_offset;

    Some (batch, batcher.current_is_flex)

(** This struct captures a bunch of variables which are used to compute the
    intrinsic sizes of children so that those variables don't have to be passed
    around all over the place below. It then has methods that implement the
    intrinsic sizing computations *)

(** Compute the available_space to be passed to the child sizing functions These
    are estimates based on either the max track sizing function or the
    provisional base size in the opposite axis to the one currently being sized.
*)
let available_space_for_measurer
    (measurer : _ Grid_item.intrinsic_size_measurer) item =
  available_space_cached item measurer.axis measurer.other_axis_tracks
    (Size.get measurer.inner_node_size (Abstract_axis.other measurer.axis))
    (fun track basis ->
      measurer.get_track_size_estimate track basis measurer.tree_val)

(** Compute the item's resolved margins for size contributions. Horizontal
    percentage margins always resolve to zero if the container size is
    indefinite as otherwise this would introduce a cyclic dependency. *)
let margins_axis_sums_with_baseline_shims_for_measurer (type tree)
    (measurer : tree Grid_item.intrinsic_size_measurer) item =
  let module Tree =
    (val measurer.tree : Tree_intf.LayoutPartialTreeExt with type t = tree)
  in
  let calc ~ptr ~basis =
    Tree.resolve_calc_value measurer.tree_val ~ptr ~basis
  in
  let inner_node_width = measurer.inner_node_size.width in
  let left =
    Resolve.resolve_or_zero_length_percentage_auto item.margin.left (Some 0.0)
      calc
  in
  let right =
    Resolve.resolve_or_zero_length_percentage_auto item.margin.right (Some 0.0)
      calc
  in
  let top =
    Resolve.resolve_or_zero_length_percentage_auto item.margin.top
      inner_node_width calc
    +. item.baseline_shim
  in
  let bottom =
    Resolve.resolve_or_zero_length_percentage_auto item.margin.bottom
      inner_node_width calc
  in
  Size.{ width = left +. right; height = top +. bottom }

(** Retrieve the item's min content contribution from the cache or compute it
    using the provided parameters *)
let min_content_contribution (measurer : _ Grid_item.intrinsic_size_measurer)
    item =
  let available_space = available_space_for_measurer measurer item in
  let margin_axis_sums =
    margins_axis_sums_with_baseline_shims_for_measurer measurer item
  in
  let contribution =
    min_content_contribution_cached item measurer.axis measurer available_space
      measurer.inner_node_size
  in
  contribution +. Size.get margin_axis_sums measurer.axis

(** Retrieve the item's max content contribution from the cache or compute it
    using the provided parameters *)
let max_content_contribution (measurer : _ Grid_item.intrinsic_size_measurer)
    item =
  let available_space = available_space_for_measurer measurer item in
  let margin_axis_sums =
    margins_axis_sums_with_baseline_shims_for_measurer measurer item
  in
  let contribution =
    max_content_contribution_cached item measurer.axis measurer available_space
      measurer.inner_node_size
  in
  contribution +. Size.get margin_axis_sums measurer.axis

(** The minimum contribution of an item is the smallest outer size it can have.
*)
let minimum_contribution (measurer : _ Grid_item.intrinsic_size_measurer) item
    axis_tracks =
  let available_space = available_space_for_measurer measurer item in
  let margin_axis_sums =
    margins_axis_sums_with_baseline_shims_for_measurer measurer item
  in
  let contribution =
    minimum_contribution_cached item measurer measurer.axis axis_tracks
      available_space measurer.inner_node_size
  in
  contribution +. Size.get margin_axis_sums measurer.axis

(** To make track sizing efficient we want to order tracks Here a placement is
    either a Line<i16> representing a row-start/row-end or a
    column-start/column-end *)
let cmp_by_cross_flex_then_span_then_start axis =
 fun item_a item_b ->
  match
    (crosses_flexible_track item_a axis, crosses_flexible_track item_b axis)
  with
  | false, true -> -1
  | true, false -> 1
  | _ -> (
      let placement_a = placement item_a axis in
      let placement_b = placement item_b axis in
      match
        Int.compare (Line_ext.span placement_a) (Line_ext.span placement_b)
      with
      | 0 -> OriginZeroLine.compare placement_a.start placement_b.start
      | c -> c)

(** When applying the track sizing algorithm and estimating the size in the
    other axis for content sizing items we should take into account
    align-content/justify-content if both the grid container and all items in
    the other axis have definite sizes. This function computes such a per-gutter
    additional size adjustment. *)
let compute_alignment_gutter_adjustment alignment axis_inner_node_size
    get_track_size_estimate tracks =
  if Array.length tracks <= 1 then 0.0
  else
    (* As items never cross the outermost gutters in a grid, we can simplify our calculations by treating
       AlignContent::Start and AlignContent::End the same *)
    let outer_gutter_weight =
      match alignment with
      | Style.Alignment.Start | Flex_start | End | Flex_end | Center -> 1
      | Stretch | Space_between -> 0
      | Space_around -> 1
      | Space_evenly -> 1
    in

    let inner_gutter_weight =
      match alignment with
      | Style.Alignment.Start | Flex_start | End | Flex_end | Center | Stretch
        ->
          0
      | Space_between -> 1
      | Space_around -> 2
      | Space_evenly -> 1
    in

    if inner_gutter_weight = 0 then 0.0
    else
      match axis_inner_node_size with
      | Some axis_inner_node_size ->
          let free_space =
            let track_size_sum =
              Array.fold_left
                (fun acc track ->
                  match
                    get_track_size_estimate track (Some axis_inner_node_size)
                  with
                  | Some size -> acc +. size
                  | None -> acc)
                0.0 tracks
            in
            max 0.0 (axis_inner_node_size -. track_size_sum)
          in

          let weighted_track_count =
            ((Array.length tracks - 3) / 2 * inner_gutter_weight)
            + (2 * outer_gutter_weight)
          in

          free_space
          /. float_of_int weighted_track_count
          *. float_of_int inner_gutter_weight
      | None -> 0.0

(** Convert origin-zero coordinates track placement in grid track vector indexes
*)
let resolve_item_track_indexes items column_counts row_counts =
  Array.iter
    (fun item ->
      item.column_indexes <-
        Geometry.Line.map
          (fun line ->
            OriginZeroLine.into_track_vec_index line
              ~negative_implicit:column_counts.negative_implicit
              ~explicit:column_counts.explicit
              ~positive_implicit:column_counts.positive_implicit)
          item.column;
      item.row_indexes <-
        Geometry.Line.map
          (fun line ->
            OriginZeroLine.into_track_vec_index line
              ~negative_implicit:row_counts.negative_implicit
              ~explicit:row_counts.explicit
              ~positive_implicit:row_counts.positive_implicit)
          item.row)
    items

(** Determine (in each axis) whether the item crosses any flexible tracks *)
let determine_if_item_crosses_flexible_or_intrinsic_tracks items columns rows =
  Array.iter
    (fun item ->
      let start_col, end_col = track_range_excluding_lines item Inline in
      let start_row, end_row = track_range_excluding_lines item Block in

      item.crosses_flexible_column <- false;
      item.crosses_intrinsic_column <- false;
      for i = start_col to end_col - 1 do
        if is_flexible columns.(i) then item.crosses_flexible_column <- true;
        if has_intrinsic_sizing_function columns.(i) then
          item.crosses_intrinsic_column <- true
      done;

      item.crosses_flexible_row <- false;
      item.crosses_intrinsic_row <- false;
      for i = start_row to end_row - 1 do
        if is_flexible rows.(i) then item.crosses_flexible_row <- true;
        if has_intrinsic_sizing_function rows.(i) then
          item.crosses_intrinsic_row <- true
      done)
    items

(** Whether it is a minimum or maximum size's space being distributed This
    controls behaviour of the space distribution algorithm when distributing
    beyond limits See "distributing space beyond limits" at
    https://www.w3.org/TR/css-grid-1/#extra-space *)
type intrinsic_contribution_type = Minimum | Maximum

(** Add any planned base size increases to the base size after a round of
    distributing space to base sizes Reset the planned base size increase to
    zero ready for the next round. *)
let flush_planned_base_size_increases tracks =
  Array.iter
    (fun track ->
      track.base_size <- track.base_size +. track.base_size_planned_increase;
      track.base_size_planned_increase <- 0.0)
    tracks

(** Add any planned growth limit increases to the growth limit after a round of
    distributing space to growth limits Reset the planned growth limit increase
    to zero ready for the next round. *)
let flush_planned_growth_limit_increases tracks set_infinitely_growable =
  Array.iter
    (fun track ->
      if track.growth_limit_planned_increase > 0.0 then (
        track.growth_limit <-
          (if track.growth_limit = Float.infinity then
             track.base_size +. track.growth_limit_planned_increase
           else track.growth_limit +. track.growth_limit_planned_increase);
        track.infinitely_growable <- set_infinitely_growable)
      else track.infinitely_growable <- false;
      track.growth_limit_planned_increase <- 0.0)
    tracks

(** 11.4 Initialise Track sizes Initialize each track's base size and growth
    limit. *)
let initialize_track_sizes calc axis_tracks axis_inner_node_size =
  Array.iter
    (fun track ->
      (* For each track, if the track's min track sizing function is:
       - A fixed sizing function
         Resolve to an absolute length and use that size as the track's initial base size.
         Note: Indefinite lengths cannot occur, as they're treated as auto.
       - An intrinsic sizing function
         Use an initial base size of zero. *)
      track.base_size <-
        (match
           MinSizing.definite_value track.min_track_sizing_function
             axis_inner_node_size calc
         with
        | Some v -> v
        | None -> 0.0);

      (* For each track, if the track's max track sizing function is:
       - A fixed sizing function
         Resolve to an absolute length and use that size as the track's initial growth limit.
       - An intrinsic sizing function
         Use an initial growth limit of infinity.
       - A flexible sizing function
         Use an initial growth limit of infinity. *)
      track.growth_limit <-
        (match
           MaxSizing.definite_value track.max_track_sizing_function
             axis_inner_node_size calc
         with
        | Some v -> v
        | None -> Float.infinity);

      (* In all cases, if the growth limit is less than the base size, increase the growth limit to match the base size. *)
      if track.growth_limit < track.base_size then
        track.growth_limit <- track.base_size)
    axis_tracks

(** 11.5.1 Shim baseline-aligned items so their intrinsic size contributions
    reflect their baseline alignment. *)
let resolve_item_baselines calc perform_child_layout axis items inner_node_size
    =
  (* Sort items by track in the other axis (row) start position so that we can iterate items in groups which
     are in the same track in the other axis (row) *)
  let other_axis = Abstract_axis.other axis in
  Array.sort
    (fun a b ->
      let a_placement = placement a other_axis in
      let b_placement = placement b other_axis in
      OriginZeroLine.compare a_placement.start b_placement.start)
    items;

  (* Iterate over grid rows *)
  let rec process_rows start_idx =
    if start_idx >= Array.length items then ()
    else
      (* Get the row index of the current row *)
      let current_row = (placement items.(start_idx) other_axis).start in

      (* Find the end of current row *)
      let end_idx = ref start_idx in
      while
        !end_idx < Array.length items
        && (placement items.(!end_idx) other_axis).start = current_row
      do
        incr end_idx
      done;

      let row_items = Array.sub items start_idx (!end_idx - start_idx) in

      (* Count how many items in *this row* are baseline aligned *)
      let row_baseline_item_count =
        Array.fold_left
          (fun count item ->
            if item.align_self = Style.Alignment.Baseline then count + 1
            else count)
          0 row_items
      in

      if row_baseline_item_count > 1 then (
        (* Compute the baselines of all items in the row *)
        Array.iter
          (fun item ->
            let measured_size_and_baselines =
              perform_child_layout item.node ~known_dimensions:Size.none
                ~parent_size:inner_node_size
                ~available_space:
                  {
                    width = Style.Available_space.Min_content;
                    height = Style.Available_space.Min_content;
                  }
                ~sizing_mode:Layout.Sizing_mode.Inherent_size
                ~vertical_margins_are_collapsible:Geometry.Line.false_
            in

            let baseline =
              measured_size_and_baselines.Layout.Layout_output.first_baselines.y
            in
            let height =
              measured_size_and_baselines.Layout.Layout_output.size.height
            in

            item.baseline <-
              Some
                ((match baseline with Some b -> b | None -> height)
                +. Resolve.resolve_or_zero_length_percentage_auto
                     item.margin.top inner_node_size.width calc))
          row_items;

        (* Compute the max baseline of all items in the row *)
        let row_max_baseline =
          Array.fold_left
            (fun max_b item ->
              max max_b (match item.baseline with Some b -> b | None -> 0.0))
            0.0 row_items
        in

        (* Compute the baseline shim for each item in the row *)
        Array.iter
          (fun item ->
            item.baseline_shim <-
              (row_max_baseline
              -. match item.baseline with Some b -> b | None -> 0.0))
          row_items);

      process_rows !end_idx
  in
  process_rows 0

(** Helper function for distributing space to tracks evenly Used by both
    distribute_item_space_to_base_size and maximise_tracks steps *)
let distribute_space_up_to_limits space_to_distribute tracks track_is_affected
    track_distribution_proportion track_affected_property track_limit =
  (* Define a small constant to avoid infinite loops due to rounding errors. Rather than stopping distributing
     extra space when it gets to exactly zero, we will stop when it falls below this amount *)
  let threshold = 0.01 in

  let space_to_distribute = ref space_to_distribute in

  while !space_to_distribute > threshold do
    let track_distribution_proportion_sum = ref 0.0 in
    let growable_tracks = ref [] in

    Array.iter
      (fun track ->
        if
          track_is_affected track
          && track_affected_property track +. track.item_incurred_increase
             < track_limit track
        then (
          growable_tracks := track :: !growable_tracks;
          track_distribution_proportion_sum :=
            !track_distribution_proportion_sum
            +. track_distribution_proportion track))
      tracks;

    if !track_distribution_proportion_sum = 0.0 then
      space_to_distribute := 0.0 (* Exit loop *)
    else
      (* Compute item-incurred increase for this iteration *)
      let min_increase_limit =
        List.fold_left
          (fun min_limit track ->
            let limit =
              (track_limit track -. track_affected_property track)
              /. track_distribution_proportion track
            in
            min min_limit limit)
          Float.infinity !growable_tracks
      in
      let iteration_item_incurred_increase =
        min min_increase_limit
          (!space_to_distribute /. !track_distribution_proportion_sum)
      in

      Array.iter
        (fun track ->
          if track_is_affected track then
            let increase =
              iteration_item_incurred_increase
              *. track_distribution_proportion track
            in
            if
              increase > 0.0
              && track_affected_property track +. increase
                 <= track_limit track +. threshold
            then (
              track.item_incurred_increase <-
                track.item_incurred_increase +. increase;
              space_to_distribute := !space_to_distribute -. increase))
        tracks
  done;

  !space_to_distribute

(** 11.5.1. Distributing Extra Space Across Spanned Tracks
    https://www.w3.org/TR/css-grid-1/#extra-space *)
let distribute_item_space_to_base_size is_flex use_flex_factor_for_distribution
    space tracks track_is_affected track_limit intrinsic_contribution_type =
  let distribute_item_space_to_base_size_inner space tracks track_is_affected
      track_distribution_proportion track_limit intrinsic_contribution_type =
    (* Skip this distribution if there is either
       - no space to distribute
       - no affected tracks to distribute space to *)
    if space = 0.0 || not (Array.exists track_is_affected tracks) then ()
    else
      (* Define get_base_size function. This is passed to the distribute_space_up_to_limits helper function
         to indicate that it is the base size that is being distributed to. *)
      let get_base_size track = track.base_size in

      (* 1. Find the space to distribute *)
      let track_sizes =
        Array.fold_left (fun acc track -> acc +. track.base_size) 0.0 tracks
      in
      let extra_space = max 0.0 (space -. track_sizes) in

      (* 2. Distribute space up to limits: *)
      let extra_space =
        distribute_space_up_to_limits extra_space tracks track_is_affected
          track_distribution_proportion get_base_size track_limit
      in

      (* 3. Distribute remaining span beyond limits (if any) *)
      (if extra_space > 0.000001 then
         (* When accommodating minimum contributions or accommodating min-content contributions:
           - any affected track that happens to also have an intrinsic max track sizing function;
           When accommodating max-content contributions:
           - any affected track that happens to also have a max-content max track sizing function *)
         let filter =
           match intrinsic_contribution_type with
           | Minimum ->
               fun track ->
                 MaxSizing.is_intrinsic track.max_track_sizing_function
           | Maximum ->
               fun track ->
                 MinSizing.is_max_content track.min_track_sizing_function
                 || MaxSizing.is_max_or_fit_content
                      track.max_track_sizing_function
         in

         (* If there are no such tracks (matching filter above), then use all affected tracks. *)
         let number_of_tracks =
           Array.fold_left
             (fun count track ->
               if track_is_affected track && filter track then count + 1
               else count)
             0 tracks
         in
         let filter = if number_of_tracks = 0 then fun _ -> true else filter in

         let _ =
           distribute_space_up_to_limits extra_space tracks
             (fun track -> track_is_affected track && filter track)
             track_distribution_proportion get_base_size track_limit
         in
         ());

      (* 4. For each affected track, if the track's item-incurred increase is larger than the track's planned increase
         set the track's planned increase to that value. *)
      Array.iter
        (fun track ->
          if track.item_incurred_increase > track.base_size_planned_increase
          then track.base_size_planned_increase <- track.item_incurred_increase;

          (* Reset the item_incurred increase ready for the next space distribution *)
          track.item_incurred_increase <- 0.0)
        tracks
  in

  if is_flex then
    let filter track = is_flexible track && track_is_affected track in
    if use_flex_factor_for_distribution then
      distribute_item_space_to_base_size_inner space tracks filter flex_factor
        track_limit intrinsic_contribution_type
    else
      distribute_item_space_to_base_size_inner space tracks filter
        (fun _ -> 1.0)
        track_limit intrinsic_contribution_type
  else
    distribute_item_space_to_base_size_inner space tracks track_is_affected
      (fun _ -> 1.0)
      track_limit intrinsic_contribution_type

(** 11.5.1. Distributing Extra Space Across Spanned Tracks This is simplified
    (and faster) version of the algorithm for growth limits
    https://www.w3.org/TR/css-grid-1/#extra-space *)
let distribute_item_space_to_growth_limit space tracks track_is_affected
    axis_inner_node_size =
  (* Skip this distribution if there is either
     - no space to distribute
     - no affected tracks to distribute space to *)
  if space = 0.0 || not (Array.exists track_is_affected tracks) then ()
  else
    (* 1. Find the space to distribute *)
    let track_sizes =
      Array.fold_left
        (fun acc track ->
          acc
          +.
          if track.growth_limit = Float.infinity then track.base_size
          else track.growth_limit)
        0.0 tracks
    in
    let extra_space = max 0.0 (space -. track_sizes) in

    (* 2. Distribute space up to limits:
       For growth limits the limit is either Infinity, or the growth limit itself. Which means that:
       - If there are any tracks with infinite limits then all space will be distributed to those track(s).
       - Otherwise no space will be distributed as part of this step *)
    let number_of_growable_tracks =
      Array.fold_left
        (fun count track ->
          if
            track_is_affected track
            && (track.infinitely_growable
               || fit_content_limited_growth_limit track axis_inner_node_size
                  = Float.infinity)
          then count + 1
          else count)
        0 tracks
    in

    (if number_of_growable_tracks > 0 then
       let item_incurred_increase =
         extra_space /. float_of_int number_of_growable_tracks
       in
       Array.iter
         (fun track ->
           if
             track_is_affected track
             && (track.infinitely_growable
                || fit_content_limited_growth_limit track axis_inner_node_size
                   = Float.infinity)
           then track.item_incurred_increase <- item_incurred_increase)
         tracks
     else
       (* 3. Distribute space beyond limits *)
       let _ =
         distribute_space_up_to_limits extra_space tracks track_is_affected
           (fun _ -> 1.0)
           (fun track ->
             if track.growth_limit = Float.infinity then track.base_size
             else track.growth_limit)
           (fun track -> fit_content_limit track axis_inner_node_size)
       in
       ());

    (* 4. For each affected track, if the track's item-incurred increase is larger than the track's planned increase
       set the track's planned increase to that value. *)
    Array.iter
      (fun track ->
        if track.item_incurred_increase > track.growth_limit_planned_increase
        then track.growth_limit_planned_increase <- track.item_incurred_increase;

        (* Reset the item_incurred increase ready for the next space distribution *)
        track.item_incurred_increase <- 0.0)
      tracks

(** 11.5 Resolve Intrinsic Track Sizes *)
let resolve_intrinsic_track_sizes (type tree)
    (tree : (module Tree_intf.LayoutPartialTreeExt with type t = tree))
    (tree_val : tree) calc axis axis_tracks other_axis_tracks items
    axis_available_grid_space inner_node_size get_track_size_estimate =
  (* Step 1. Shim baseline-aligned items so their intrinsic size contributions reflect their baseline alignment.
     Already done at this point. See resolve_item_baselines function. *)

  (* Step 2.
     The track sizing algorithm requires us to iterate through the items in ascending order of the number of
     tracks they span (first items that span 1 track, then items that span 2 tracks, etc).
     To avoid having to do multiple iterations of the items, we pre-sort them into this order. *)
  Array.sort (cmp_by_cross_flex_then_span_then_start axis) items;

  (* Step 2, Step 3 and Step 4
     2 & 3. Iterate over items that don't cross a flex track. Items should have already been sorted in ascending order
     of the number of tracks they span. Step 2 is the 1 track case and has an optimised implementation
     4. Next, repeat the previous step instead considering (together, rather than grouped by span size) all items
     that do span a track with a flexible sizing function while *)
  let axis_inner_node_size = Size.get inner_node_size axis in
  let flex_factor_sum =
    Array.fold_left (fun acc track -> acc +. flex_factor track) 0.0 axis_tracks
  in
  let item_sizer : tree Grid_item.intrinsic_size_measurer =
    {
      tree;
      tree_val;
      other_axis_tracks;
      axis;
      inner_node_size;
      get_track_size_estimate;
    }
  in

  let batched_item_iterator = create_item_batcher axis in
  let rec process_batches () =
    match next_batch batched_item_iterator items with
    | None -> ()
    | Some (batch, is_flex) ->
        (* 2. Size tracks to fit non-spanning items: For each track with an intrinsic track sizing function and not a flexible sizing function,
           consider the items in it with a span of 1: *)
        let batch_span = span batch.(0) axis in
        if (not is_flex) && batch_span = 1 then (
          Array.iter
            (fun item ->
              let track_index = (placement_indexes item axis).start + 1 in
              let track = axis_tracks.(track_index) in

              (* Handle base sizes *)
              let new_base_size =
                match track.min_track_sizing_function with
                | Min_content ->
                    max track.base_size
                      (min_content_contribution item_sizer item)
                | Percent _ when axis_inner_node_size = None ->
                    (* If the container size is indefinite and has not yet been resolved then percentage sized
                     tracks should be treated as min-content (this matches Chrome's behaviour and seems sensible) *)
                    max track.base_size
                      (min_content_contribution item_sizer item)
                | Max_content ->
                    max track.base_size
                      (max_content_contribution item_sizer item)
                | Auto ->
                    let space =
                      match axis_available_grid_space with
                      (* QUIRK: The spec says that:
                       
                       If the grid container is being sized under a min- or max-content constraint, use the items' limited
                       min-content contributions in place of their minimum contributions here.
                       
                       However, in practice browsers only seem to apply this rule if the item is not a scroll container
                       (note that overflow:hidden counts as a scroll container), giving the automatic minimum size of scroll
                       containers (zero) precedence over the min-content contributions. *)
                      | (Style.Available_space.Min_content | Max_content)
                        when not
                               (Grid_helpers.Overflow.is_scroll_container
                                  (Point.get item.overflow axis)) ->
                          let axis_minimum_size =
                            minimum_contribution item_sizer item axis_tracks
                          in
                          let axis_min_content_size =
                            min_content_contribution item_sizer item
                          in
                          let limit =
                            MaxSizing.definite_limit
                              track.max_track_sizing_function
                              axis_inner_node_size calc
                          in
                          let limited =
                            match limit with
                            | Some l -> min axis_min_content_size l
                            | None -> axis_min_content_size
                          in
                          max limited axis_minimum_size
                      | _ -> minimum_contribution item_sizer item axis_tracks
                    in
                    max track.base_size space
                | Length _ | Percent _ ->
                    (* Do nothing as it's not an intrinsic track sizing function *)
                    track.base_size
              in
              axis_tracks.(track_index).base_size <- new_base_size;

              (* Handle growth limits *)
              if MaxSizing.is_fit_content track.max_track_sizing_function then (
                (* If item is not a scroll container, then increase the growth limit to at least the
                 size of the min-content contribution *)
                (if
                   not
                     (Grid_helpers.Overflow.is_scroll_container
                        (Point.get item.overflow axis))
                 then
                   let min_content_contribution =
                     min_content_contribution item_sizer item
                   in
                   track.growth_limit_planned_increase <-
                     max track.growth_limit_planned_increase
                       min_content_contribution);

                (* Always increase the growth limit to at least the size of the *fit-content limited*
                 max-content contribution *)
                let fit_content_limit =
                  fit_content_limit track axis_inner_node_size
                in
                let max_content_contribution =
                  min
                    (max_content_contribution item_sizer item)
                    fit_content_limit
                in
                track.growth_limit_planned_increase <-
                  max track.growth_limit_planned_increase
                    max_content_contribution)
              else if
                MaxSizing.is_max_content_alike track.max_track_sizing_function
                || MaxSizing.uses_percentage track.max_track_sizing_function
                   && axis_inner_node_size = None
              then
                (* If the container size is indefinite and has not yet been resolved then percentage sized
                 tracks should be treated as auto (this matches Chrome's behaviour and seems sensible) *)
                track.growth_limit_planned_increase <-
                  max track.growth_limit_planned_increase
                    (max_content_contribution item_sizer item)
              else if MaxSizing.is_intrinsic track.max_track_sizing_function
              then
                track.growth_limit_planned_increase <-
                  max track.growth_limit_planned_increase
                    (min_content_contribution item_sizer item))
            batch;

          Array.iter
            (fun track ->
              if track.growth_limit_planned_increase > 0.0 then
                track.growth_limit <-
                  (if track.growth_limit = Float.infinity then
                     track.growth_limit_planned_increase
                   else
                     max track.growth_limit track.growth_limit_planned_increase);
              track.infinitely_growable <- false;
              track.growth_limit_planned_increase <- 0.0;
              if track.growth_limit < track.base_size then
                track.growth_limit <- track.base_size)
            axis_tracks;

          process_batches ())
        else
          let use_flex_factor_for_distribution =
            is_flex && flex_factor_sum <> 0.0
          in

          (* 1. For intrinsic minimums:
             First increase the base size of tracks with an intrinsic min track sizing function *)
          Array.iter
            (fun item ->
              if crosses_intrinsic_track item axis then
                (* ...by distributing extra space as needed to accommodate these items' minimum contributions. *)
                let space =
                  match axis_available_grid_space with
                  | (Style.Available_space.Min_content | Max_content)
                    when not
                           (Grid_helpers.Overflow.is_scroll_container
                              (Point.get item.overflow axis)) ->
                      let axis_minimum_size =
                        minimum_contribution item_sizer item axis_tracks
                      in
                      let axis_min_content_size =
                        min_content_contribution item_sizer item
                      in
                      let module Tree =
                        (val item_sizer.tree
                            : Tree_intf.LayoutPartialTreeExt with type t = tree)
                      in
                      let calc ~ptr ~basis =
                        Tree.resolve_calc_value item_sizer.tree_val ~ptr ~basis
                      in
                      let limit =
                        spanned_track_limit item axis axis_tracks
                          axis_inner_node_size calc
                      in
                      let limited =
                        match limit with
                        | Some l -> min axis_min_content_size l
                        | None -> axis_min_content_size
                      in
                      max limited axis_minimum_size
                  | _ -> minimum_contribution item_sizer item axis_tracks
                in
                let start_idx, end_idx =
                  track_range_excluding_lines item axis
                in
                let tracks =
                  Array.sub axis_tracks start_idx (end_idx - start_idx)
                in
                if space > 0.0 then
                  let has_intrinsic_min_track_sizing_function track =
                    match
                      MinSizing.definite_value track.min_track_sizing_function
                        axis_inner_node_size calc
                    with
                    | None -> true
                    | Some _ -> false
                  in
                  if
                    Grid_helpers.Overflow.is_scroll_container
                      (Point.get item.overflow axis)
                  then
                    let fit_content_limit track =
                      fit_content_limited_growth_limit track
                        axis_inner_node_size
                    in
                    distribute_item_space_to_base_size is_flex
                      use_flex_factor_for_distribution space tracks
                      has_intrinsic_min_track_sizing_function fit_content_limit
                      Minimum
                  else
                    distribute_item_space_to_base_size is_flex
                      use_flex_factor_for_distribution space tracks
                      has_intrinsic_min_track_sizing_function
                      (fun track -> track.growth_limit)
                      Minimum)
            batch;
          flush_planned_base_size_increases axis_tracks;

          (* 2. For content-based minimums:
             Next continue to increase the base size of tracks with a min track sizing function of min-content or max-content
             by distributing extra space as needed to account for these items' min-content contributions. *)
          let has_min_or_max_content_min_track_sizing_function track =
            MinSizing.is_min_or_max_content track.min_track_sizing_function
          in
          Array.iter
            (fun item ->
              let space = min_content_contribution item_sizer item in
              let start_idx, end_idx = track_range_excluding_lines item axis in
              let tracks =
                Array.sub axis_tracks start_idx (end_idx - start_idx)
              in
              if space > 0.0 then
                if
                  Grid_helpers.Overflow.is_scroll_container
                    (Point.get item.overflow axis)
                then
                  let fit_content_limit track =
                    fit_content_limited_growth_limit track axis_inner_node_size
                  in
                  distribute_item_space_to_base_size is_flex
                    use_flex_factor_for_distribution space tracks
                    has_min_or_max_content_min_track_sizing_function
                    fit_content_limit Minimum
                else
                  distribute_item_space_to_base_size is_flex
                    use_flex_factor_for_distribution space tracks
                    has_min_or_max_content_min_track_sizing_function
                    (fun track -> track.growth_limit)
                    Minimum)
            batch;
          flush_planned_base_size_increases axis_tracks;

          (* 3. For max-content minimums: *)
          if axis_available_grid_space = Style.Available_space.Max_content then (
            (* Whether a track:
               - has an Auto MIN track sizing function
               - Does not have a MinContent MAX track sizing function *)
            let has_auto_min_track_sizing_function track =
              MinSizing.is_auto track.min_track_sizing_function
              && not (MaxSizing.is_min_content track.max_track_sizing_function)
            in

            (* Whether a track has a MaxContent min track sizing function *)
            let has_max_content_min_track_sizing_function track =
              MinSizing.is_max_content track.min_track_sizing_function
            in

            Array.iter
              (fun item ->
                let axis_max_content_size =
                  max_content_contribution item_sizer item
                in
                let module Tree =
                  (val item_sizer.tree
                      : Tree_intf.LayoutPartialTreeExt with type t = tree)
                in
                let calc ~ptr ~basis =
                  Tree.resolve_calc_value item_sizer.tree_val ~ptr ~basis
                in
                let limit =
                  spanned_track_limit item axis axis_tracks axis_inner_node_size
                    calc
                in
                let space =
                  match limit with
                  | Some l -> min axis_max_content_size l
                  | None -> axis_max_content_size
                in
                let start_idx, end_idx =
                  track_range_excluding_lines item axis
                in
                let tracks =
                  Array.sub axis_tracks start_idx (end_idx - start_idx)
                in
                if space > 0.0 then
                  (* If any of the tracks spanned by the item have a MaxContent min track sizing function then
                   distribute space only to those tracks. Otherwise distribute space to tracks with an Auto min
                   track sizing function. *)
                  if
                    Array.exists has_max_content_min_track_sizing_function
                      tracks
                  then
                    distribute_item_space_to_base_size is_flex
                      use_flex_factor_for_distribution space tracks
                      has_max_content_min_track_sizing_function
                      (fun _ -> Float.infinity)
                      Maximum
                  else
                    let fit_content_limited_growth_limit track =
                      fit_content_limited_growth_limit track
                        axis_inner_node_size
                    in
                    distribute_item_space_to_base_size is_flex
                      use_flex_factor_for_distribution space tracks
                      has_auto_min_track_sizing_function
                      fit_content_limited_growth_limit Maximum)
              batch;
            flush_planned_base_size_increases axis_tracks);

          (* In all cases, continue to increase the base size of tracks with a min track sizing function of max-content by distributing
             extra space as needed to account for these items' max-content contributions. *)
          let has_max_content_min_track_sizing_function track =
            MinSizing.is_max_content track.min_track_sizing_function
          in
          Array.iter
            (fun item ->
              let axis_max_content_size =
                max_content_contribution item_sizer item
              in
              let space = axis_max_content_size in
              let start_idx, end_idx = track_range_excluding_lines item axis in
              let tracks =
                Array.sub axis_tracks start_idx (end_idx - start_idx)
              in
              if space > 0.0 then
                distribute_item_space_to_base_size is_flex
                  use_flex_factor_for_distribution space tracks
                  has_max_content_min_track_sizing_function
                  (fun track -> track.growth_limit)
                  Maximum)
            batch;
          flush_planned_base_size_increases axis_tracks;

          (* 4. If at this point any track's growth limit is now less than its base size, increase its growth limit to match its base size. *)
          Array.iter
            (fun track ->
              if track.growth_limit < track.base_size then
                track.growth_limit <- track.base_size)
            axis_tracks;

          (* If a track is a flexible track, then it has flexible max track sizing function
             It cannot also have an intrinsic max track sizing function, so these steps do not apply. *)
          if not is_flex then (
            (* 5. For intrinsic maximums: Next increase the growth limit of tracks with an intrinsic max track sizing function by
               distributing extra space as needed to account for these items' min-content contributions. *)
            let has_intrinsic_max_track_sizing_function track =
              not
                (MaxSizing.has_definite_value track.max_track_sizing_function
                   axis_inner_node_size)
            in
            Array.iter
              (fun item ->
                let axis_min_content_size =
                  min_content_contribution item_sizer item
                in
                let space = axis_min_content_size in
                let start_idx, end_idx =
                  track_range_excluding_lines item axis
                in
                let tracks =
                  Array.sub axis_tracks start_idx (end_idx - start_idx)
                in
                if space > 0.0 then
                  distribute_item_space_to_growth_limit space tracks
                    has_intrinsic_max_track_sizing_function
                    (Size.get inner_node_size axis))
              batch;
            (* Mark any tracks whose growth limit changed from infinite to finite in this step as infinitely growable for the next step. *)
            flush_planned_growth_limit_increases axis_tracks true;

            (* 6. For max-content maximums: Lastly continue to increase the growth limit of tracks with a max track sizing function of max-content
               by distributing extra space as needed to account for these items' max-content contributions. However, limit the growth of any
               fit-content() tracks by their fit-content() argument. *)
            let has_max_content_max_track_sizing_function track =
              MaxSizing.is_max_content_alike track.max_track_sizing_function
              || MaxSizing.uses_percentage track.max_track_sizing_function
                 && axis_inner_node_size = None
            in
            Array.iter
              (fun item ->
                let axis_max_content_size =
                  max_content_contribution item_sizer item
                in
                let space = axis_max_content_size in
                let start_idx, end_idx =
                  track_range_excluding_lines item axis
                in
                let tracks =
                  Array.sub axis_tracks start_idx (end_idx - start_idx)
                in
                if space > 0.0 then
                  distribute_item_space_to_growth_limit space tracks
                    has_max_content_max_track_sizing_function
                    (Size.get inner_node_size axis))
              batch;
            (* Mark any tracks whose growth limit changed from infinite to finite in this step as infinitely growable for the next step. *)
            flush_planned_growth_limit_increases axis_tracks false);

          process_batches ()
  in
  process_batches ();

  (* Step 5. If any track still has an infinite growth limit (because, for example, it had no items placed
     in it or it is a flexible track), set its growth limit to its base size.
     NOTE: this step is super-important to ensure that the "Maximise Tracks" step doesn't affect flexible tracks *)
  Array.iter
    (fun track ->
      if track.growth_limit = Float.infinity then
        track.growth_limit <- track.base_size)
    axis_tracks

(** 11.6 Maximise Tracks Distributes free space (if any) to tracks with FINITE
    growth limits, up to their limits. *)
let maximise_tracks axis_tracks axis_inner_node_size axis_available_grid_space =
  let used_space =
    Array.fold_left (fun acc track -> acc +. track.base_size) 0.0 axis_tracks
  in
  let free_space =
    Style.Available_space.compute_free_space axis_available_grid_space
      used_space
  in
  if free_space = Float.infinity then
    Array.iter (fun track -> track.base_size <- track.growth_limit) axis_tracks
  else if free_space > 0.0 then
    let _ =
      distribute_space_up_to_limits free_space axis_tracks
        (fun _ -> true)
        (fun _ -> 1.0)
        (fun track -> track.base_size)
        (fun track ->
          fit_content_limited_growth_limit track axis_inner_node_size)
    in
    Array.iter
      (fun track ->
        track.base_size <- track.base_size +. track.item_incurred_increase;
        track.item_incurred_increase <- 0.0)
      axis_tracks

(** 11.7.1. Find the Size of an fr This algorithm finds the largest size that an
    fr unit can be without exceeding the target size. It must be called with a
    set of grid tracks and some quantity of space to fill. *)
let find_size_of_fr tracks space_to_fill =
  (* Handle the trivial case where there is no space to fill
     Do not remove as otherwise the loop below will loop infinitely *)
  if space_to_fill = 0.0 then 0.0
  else
    (* If the product of the hypothetical fr size (computed below) and any flexible track's flex factor
       is less than the track's base size, then we must restart this algorithm treating all such tracks as inflexible.
       We therefore wrap the entire algorithm in a loop, with an hypothetical_fr_size of INFINITY such that the above
       condition can never be true for the first iteration. *)
    let hypothetical_fr_size = ref Float.infinity in
    let previous_iter_hypothetical_fr_size = ref 0.0 in
    let continue = ref true in
    while !continue do
      (* Let leftover space be the space to fill minus the base sizes of the non-flexible grid tracks.
         Let flex factor sum be the sum of the flex factors of the flexible tracks. If this value is less than 1, set it to 1 instead.
         We compute both of these in a single loop to avoid iterating over the data twice *)
      let used_space = ref 0.0 in
      let naive_flex_factor_sum = ref 0.0 in
      Array.iter
        (fun track ->
          (* Tracks for which flex_factor * hypothetical_fr_size < track.base_size are treated as inflexible *)
          if
            MaxSizing.is_fr track.max_track_sizing_function
            && flex_factor track *. !hypothetical_fr_size >= track.base_size
          then
            naive_flex_factor_sum := !naive_flex_factor_sum +. flex_factor track
          else used_space := !used_space +. track.base_size)
        tracks;
      let leftover_space = space_to_fill -. !used_space in
      let flex_factor = max !naive_flex_factor_sum 1.0 in

      (* Let the hypothetical fr size be the leftover space divided by the flex factor sum. *)
      previous_iter_hypothetical_fr_size := !hypothetical_fr_size;
      hypothetical_fr_size := leftover_space /. flex_factor;

      (* If the product of the hypothetical fr size and a flexible track's flex factor is less than the track's base size,
         restart this algorithm treating all such tracks as inflexible. *)
      let hypothetical_fr_size_is_valid =
        Array.for_all
          (fun track ->
            if MaxSizing.is_fr track.max_track_sizing_function then
              let track_flex_factor = Grid_track.flex_factor track in
              track_flex_factor *. !hypothetical_fr_size >= track.base_size
              || track_flex_factor *. !previous_iter_hypothetical_fr_size
                 < track.base_size
            else true)
          tracks
      in
      if hypothetical_fr_size_is_valid then continue := false
    done;

    (* Return the hypothetical fr size. *)
    !hypothetical_fr_size

(** 11.7. Expand Flexible Tracks This step sizes flexible tracks using the
    largest value it can assign to an fr without exceeding the available space.
*)
let expand_flexible_tracks (type tree)
    (tree : (module Tree_intf.LayoutPartialTreeExt with type t = tree))
    (tree_val : tree) axis axis_tracks items axis_min_size axis_max_size
    axis_available_space_for_expansion inner_node_size =
  (* First, find the grid's used flex fraction: *)
  let flex_fraction =
    match axis_available_space_for_expansion with
    (* If the free space is zero:
       The used flex fraction is zero.
       Otherwise, if the free space is a definite length:
       The used flex fraction is the result of finding the size of an fr using all of the grid tracks and
       a space to fill of the available grid space. *)
    | Style.Available_space.Definite available_space ->
        let used_space =
          Array.fold_left
            (fun acc track -> acc +. track.base_size)
            0.0 axis_tracks
        in
        let free_space = available_space -. used_space in
        if free_space <= 0.0 then 0.0
        else find_size_of_fr axis_tracks available_space
    (* If ... sizing the grid container under a min-content constraint the used flex fraction is zero. *)
    | Min_content -> 0.0
    (* Otherwise, if the free space is an indefinite length: *)
    | Max_content ->
        (* The used flex fraction is the maximum of: *)
        let flex_fraction =
          max
            (* For each flexible track, if the flexible track's flex factor is greater than one,
             the result of dividing the track's base size by its flex factor; otherwise, the track's base size. *)
            (Array.fold_left
               (fun max_val track ->
                 if MaxSizing.is_fr track.max_track_sizing_function then
                   let flex_factor = flex_factor track in
                   let value =
                     if flex_factor > 1.0 then track.base_size /. flex_factor
                     else track.base_size
                   in
                   max max_val value
                 else max_val)
               0.0 axis_tracks)
            (* For each grid item that crosses a flexible track, the result of finding the size of an fr using all the grid tracks
             that the item crosses and a space to fill of the item's max-content contribution. *)
            (Array.fold_left
               (fun max_val item ->
                 if crosses_flexible_track item axis then
                   let start_idx, end_idx =
                     track_range_excluding_lines item axis
                   in
                   let tracks =
                     Array.sub axis_tracks start_idx (end_idx - start_idx)
                   in
                   (* TODO: plumb estimate of other axis size (known_dimensions) in here rather than just passing Size::NONE? *)
                   (* Create a dummy measurer for the max content contribution calculation *)
                   let measurer : tree Grid_item.intrinsic_size_measurer =
                     {
                       tree;
                       tree_val;
                       other_axis_tracks = [||];
                       (* Empty for this calculation *)
                       get_track_size_estimate = (fun _ _ _ -> None);
                       axis;
                       inner_node_size;
                     }
                   in
                   let max_content_contribution =
                     max_content_contribution_cached item axis measurer
                       Size.none inner_node_size
                   in
                   let fr_size =
                     find_size_of_fr tracks max_content_contribution
                   in
                   max max_val fr_size
                 else max_val)
               0.0 items)
        in

        (* If using this flex fraction would cause the grid to be smaller than the grid container's min-width/height (or larger than the
           grid container's max-width/height), then redo this step, treating the free space as definite and the available grid space as equal
           to the grid container's inner size when it's sized to its min-width/height (max-width/height).
           (Note: min_size takes precedence over max_size) *)
        let hypothetical_grid_size =
          Array.fold_left
            (fun acc track ->
              if MaxSizing.is_fr track.max_track_sizing_function then
                let track_flex_factor = flex_factor track in
                acc +. max track.base_size (track_flex_factor *. flex_fraction)
              else acc +. track.base_size)
            0.0 axis_tracks
        in
        let axis_min_size =
          match axis_min_size with Some s -> s | None -> 0.0
        in
        let axis_max_size =
          match axis_max_size with Some s -> s | None -> Float.infinity
        in
        if hypothetical_grid_size < axis_min_size then
          find_size_of_fr axis_tracks axis_min_size
        else if hypothetical_grid_size > axis_max_size then
          find_size_of_fr axis_tracks axis_max_size
        else flex_fraction
  in

  (* For each flexible track, if the product of the used flex fraction and the track's flex factor is greater
     than the track's base size, set its base size to that product. *)
  Array.iter
    (fun track ->
      if MaxSizing.is_fr track.max_track_sizing_function then
        let track_flex_factor = flex_factor track in
        track.base_size <-
          max track.base_size (track_flex_factor *. flex_fraction))
    axis_tracks

(** 11.8. Stretch auto Tracks This step expands tracks that have an auto max
    track sizing function by dividing any remaining positive, definite free
    space equally amongst them. *)
let stretch_auto_tracks axis_tracks axis_min_size
    axis_available_space_for_expansion =
  let num_auto_tracks =
    Array.fold_left
      (fun count track ->
        if MaxSizing.is_auto track.max_track_sizing_function then count + 1
        else count)
      0 axis_tracks
  in
  if num_auto_tracks > 0 then
    let used_space =
      Array.fold_left (fun acc track -> acc +. track.base_size) 0.0 axis_tracks
    in

    (* If the free space is indefinite, but the grid container has a definite min-width/height
       use that size to calculate the free space for this step instead. *)
    let free_space =
      if Style.Available_space.is_definite axis_available_space_for_expansion
      then
        Style.Available_space.compute_free_space
          axis_available_space_for_expansion used_space
      else
        match axis_min_size with Some size -> size -. used_space | None -> 0.0
    in
    if free_space > 0.0 then
      let extra_space_per_auto_track =
        free_space /. float_of_int num_auto_tracks
      in
      Array.iter
        (fun track ->
          if MaxSizing.is_auto track.max_track_sizing_function then
            track.base_size <- track.base_size +. extra_space_per_auto_track)
        axis_tracks

(** Algorithm for resolving a grid container's track sizes.
    https://www.w3.org/TR/css-grid-1/#algo-track-sizing *)
let track_sizing_algorithm (type tree)
    ~(tree : (module Tree_intf.LayoutPartialTreeExt with type t = tree))
    ~(tree_val : tree) ~axis ~axis_min_size ~axis_max_size ~axis_alignment
    ~other_axis_alignment ~available_grid_space ~inner_node_size ~axis_tracks
    ~other_axis_tracks ~items ~get_track_size_estimate
    ~has_baseline_aligned_item =
  let module Tree =
    (val tree : Tree_intf.LayoutPartialTreeExt with type t = tree)
  in
  let calc ~ptr ~basis = Tree.resolve_calc_value tree_val ~ptr ~basis in
  (* 11.4 Initialise Track sizes
     Initialize each track's base size and growth limit. *)
  initialize_track_sizes calc axis_tracks (Size.get inner_node_size axis);

  (* 11.5.1 Shim item baselines *)
  if has_baseline_aligned_item then
    resolve_item_baselines calc
      (Tree.perform_child_layout tree_val)
      axis items inner_node_size;

  (* If all tracks have base_size = growth_limit, then skip the rest of this function.
     Note: this can only happen if both track sizing function have the same fixed track sizing function *)
  if
    Array.for_all
      (fun track -> track.base_size = track.growth_limit)
      axis_tracks
  then ()
  else (* Pre-computations for 11.5 Resolve Intrinsic Track Sizes *)
    (* Compute an additional amount to add to each spanned gutter when computing item's estimated size in the
       in the opposite axis based on the alignment, container size, and estimated track sizes in that axis *)
    let gutter_alignment_adjustment =
      compute_alignment_gutter_adjustment other_axis_alignment
        (Size.get inner_node_size (Abstract_axis.other axis))
        (fun track basis -> get_track_size_estimate track basis tree_val)
        other_axis_tracks
    in
    if Array.length other_axis_tracks > 3 then (
      let len = Array.length other_axis_tracks in
      for i = 2 to len - 1 do
        if i mod 2 = 0 then
          other_axis_tracks.(i).content_alignment_adjustment <-
            gutter_alignment_adjustment
      done;

      (* 11.5 Resolve Intrinsic Track Sizes *)
      resolve_intrinsic_track_sizes tree tree_val calc axis axis_tracks
        other_axis_tracks items
        (Size.get available_grid_space axis)
        inner_node_size get_track_size_estimate;

      (* 11.6. Maximise Tracks
       Distributes free space (if any) to tracks with FINITE growth limits, up to their limits. *)
      maximise_tracks axis_tracks
        (Size.get inner_node_size axis)
        (Size.get available_grid_space axis);

      (* For the purpose of the final two expansion steps ("Expand Flexible Tracks" and "Stretch auto Tracks"), we only want to expand
       into space generated by the grid container's size (as defined by either it's preferred size style or by it's parent node through
       something like stretch alignment), not just any available space. To do this we map definite available space to AvailableSpace::MaxContent
       in the case that inner_node_size is None *)
      let axis_available_space_for_expansion =
        match Size.get inner_node_size axis with
        | Some available_space -> Style.Available_space.Definite available_space
        | None -> (
            match Size.get available_grid_space axis with
            | Style.Available_space.Min_content -> Min_content
            | Max_content | Definite _ -> Max_content)
      in

      (* 11.7. Expand Flexible Tracks
       This step sizes flexible tracks using the largest value it can assign to an fr without exceeding the available space. *)
      expand_flexible_tracks tree tree_val axis axis_tracks items axis_min_size
        axis_max_size axis_available_space_for_expansion inner_node_size;

      (* 11.8. Stretch auto Tracks
       This step expands tracks that have an auto max track sizing function by dividing any remaining positive, definite free space equally amongst them. *)
      if axis_alignment = Style.Alignment.Stretch then
        stretch_auto_tracks axis_tracks axis_min_size
          axis_available_space_for_expansion)

(** Helper to compute track offsets from their sizes *)
let compute_track_offsets tracks
    (container_alignment_style : Style.Alignment.align_content option)
    inner_node_size =
  let content_alignment_style =
    match container_alignment_style with
    | None -> Style.Alignment.Start
    | Some style -> style
  in

  (* Calculate total track size *)
  let total_track_size =
    Array.fold_left (fun acc track -> acc +. track.base_size) 0.0 tracks
  in
  let free_space = max 0.0 (inner_node_size -. total_track_size) in

  (* Apply alignment *)
  let initial_offset =
    match content_alignment_style with
    | Start | Flex_start -> 0.0
    | End | Flex_end -> free_space
    | Center -> free_space /. 2.0
    | Stretch -> 0.0
    | Space_between -> 0.0
    | Space_around ->
        if Array.length tracks > 0 then
          free_space /. (2.0 *. float_of_int (Array.length tracks))
        else 0.0
    | Space_evenly ->
        if Array.length tracks > 0 then
          free_space /. float_of_int (Array.length tracks + 1)
        else 0.0
  in

  let gap_size =
    match content_alignment_style with
    | Space_between ->
        if Array.length tracks > 1 then
          free_space /. float_of_int (Array.length tracks - 1)
        else 0.0
    | Space_around ->
        if Array.length tracks > 0 then
          free_space /. float_of_int (Array.length tracks)
        else 0.0
    | Space_evenly ->
        if Array.length tracks > 0 then
          free_space /. float_of_int (Array.length tracks + 1)
        else 0.0
    | _ -> 0.0
  in

  (* Set offsets *)
  let offset = ref initial_offset in
  Array.iter
    (fun track ->
      track.offset <- !offset;
      offset := !offset +. track.base_size +. gap_size)
    tracks
