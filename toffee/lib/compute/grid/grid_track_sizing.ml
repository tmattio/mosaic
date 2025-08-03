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

(** Algorithm for resolving a grid container's track sizes.
    https://www.w3.org/TR/css-grid-1/#algo-track-sizing *)
let track_sizing_algorithm ~axis ~axis_min_size ~axis_max_size
    ~axis_available_grid_space ~axis_tracks ~axis_counts ~items
    ~get_track_size_estimate ~inner_node_size =
  (* TODO: Implement full track sizing algorithm
     axis_min_size, axis_max_size, axis_counts, and inner_node_size will be used in later steps *)
  let _ = (axis_min_size, axis_max_size, axis_counts, inner_node_size) in
  (* 1. Initialize Track Sizes *)
  Array.iter
    (fun track ->
      (* For each track, if the track's min track sizing function is:
       - A fixed sizing function: Resolve to an absolute length and use that size as the track's base size.
       - An intrinsic sizing function: Use an initial base size of zero. *)
      track.base_size <-
        (match track.min_track_sizing_function with
        | Length px -> px
        | Percent pct -> (
            match axis_available_grid_space with
            | Some s -> s *. pct
            | None -> 0.0)
        | _ -> 0.0);

      (* For each track, if the track's max track sizing function is:
       - A fixed sizing function: Resolve to an absolute length and use that size as the track's growth limit.
       - An intrinsic or flexible sizing function: Use an initial growth limit of infinity. *)
      track.growth_limit <-
        (match track.max_track_sizing_function with
        | Length px -> px
        | Percent pct -> (
            match axis_available_grid_space with
            | Some s -> s *. pct
            | None -> Float.infinity)
        | _ -> Float.infinity);

      (* In all cases, if the growth limit is less than the base size, increase the growth limit to match the base size. *)
      if track.growth_limit < track.base_size then
        track.growth_limit <- track.base_size)
    axis_tracks;

  (* 2. Resolve Intrinsic Track Sizes *)
  (* This would be a very large implementation following the spec exactly.
     For now, we'll use a simplified approach *)
  let axis_intrinsic_tracks =
    (fun pred arr -> Array.of_list (List.filter pred (Array.to_list arr)))
      (fun track -> has_intrinsic_sizing_function track)
      axis_tracks
  in

  if Array.length axis_intrinsic_tracks > 0 then (
    (* Sort items by span *)
    let sorted_items = Array.copy items in
    Array.sort
      (fun a b ->
        let a_flex = crosses_flexible_track a axis in
        let b_flex = crosses_flexible_track b axis in
        match Bool.compare a_flex b_flex with
        | 0 -> Int.compare (span a axis) (span b axis)
        | c -> c)
      sorted_items;

    (* Process items in batches *)
    let batcher = create_item_batcher axis in
    let rec process_batches () =
      match next_batch batcher sorted_items with
      | None -> ()
      | Some (batch, _is_flex) ->
          (* For each batch, compute contributions and distribute space *)
          Array.iter
            (fun item ->
              let _available_space =
                available_space_cached item axis axis_tracks
                  axis_available_grid_space get_track_size_estimate
              in
              (* TODO: min_content_contribution_cached needs to be refactored to not use tree object *)
              let contribution = 0.0 in

              (* Distribute contribution to tracks *)
              let track_range = track_range_excluding_lines item axis in
              let start_idx, end_idx = track_range in
              let spanned_tracks =
                Array.sub axis_tracks start_idx (end_idx - start_idx)
              in
              let track_count = float_of_int (Array.length spanned_tracks) in

              if track_count > 0.0 then
                let per_track = contribution /. track_count in
                Array.iter
                  (fun track ->
                    if track.base_size < per_track then
                      track.base_size <- per_track)
                  spanned_tracks)
            batch;
          process_batches ()
    in
    process_batches ());

  (* 3. Maximize Tracks *)
  Array.iter
    (fun track ->
      if track.growth_limit = Float.infinity then
        track.base_size <- track.growth_limit)
    axis_tracks;

  (* 4. Expand Flexible Tracks *)
  let flexible_tracks =
    (fun pred arr -> Array.of_list (List.filter pred (Array.to_list arr)))
      is_flexible axis_tracks
  in
  (if Array.length flexible_tracks > 0 then
     match axis_available_grid_space with
     | Some available_space ->
         let used_space =
           Array.fold_left
             (fun acc track -> acc +. track.base_size)
             0.0 axis_tracks
         in
         let free_space = available_space -. used_space in

         if free_space > 0.0 then
           let total_flex_factor =
             Array.fold_left
               (fun acc track -> acc +. flex_factor track)
               0.0 flexible_tracks
           in

           if total_flex_factor > 0.0 then
             Array.iter
               (fun track ->
                 let factor = flex_factor track in
                 if factor > 0.0 then
                   let share = free_space *. factor /. total_flex_factor in
                   track.base_size <- track.base_size +. share)
               flexible_tracks
     | None -> ());

  (* 5. Stretch auto tracks *)
  let auto_tracks =
    (fun pred arr -> Array.of_list (List.filter pred (Array.to_list arr)))
      (fun track ->
        match track.max_track_sizing_function with Auto -> true | _ -> false)
      axis_tracks
  in

  if Array.length auto_tracks > 0 then
    match axis_available_grid_space with
    | Some available_space ->
        let used_space =
          Array.fold_left
            (fun acc track -> acc +. track.base_size)
            0.0 axis_tracks
        in
        let free_space = available_space -. used_space in

        if free_space > 0.0 then
          let auto_track_count = float_of_int (Array.length auto_tracks) in
          let per_track = free_space /. auto_track_count in
          Array.iter
            (fun track -> track.base_size <- track.base_size +. per_track)
            auto_tracks
    | None -> ()

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
