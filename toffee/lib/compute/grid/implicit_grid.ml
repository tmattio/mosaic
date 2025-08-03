(** implicit_grid.ml
    ---------------------------------------------------------------------------
    This module is not required for spec compliance, but is used as a
    performance optimisation to reduce the number of allocations required when
    creating a grid.
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

open Geometry
open Style.Grid
open Grid_coordinates

(** Helper function for `compute_grid_size_estimate` Produces a conservative
    estimate of the greatest and smallest grid lines used by a single grid item

    Values are returned in origin-zero coordinates *)
let child_min_line_max_line_span
    (line : Style.Grid.grid_placement Geometry.line) explicit_track_count =
  (* 8.3.1. Grid Placement Conflict Handling
     A. If the placement for a grid item contains two lines, and the start line is further end-ward than the end line, swap the two lines.
     B. If the start line is equal to the end line, remove the end line.
     C. If the placement contains two spans, remove the one contributed by the end grid-placement property.
     D. If the placement contains only a span for a named line, replace it with a span of 1. *)

  (* Convert line into origin-zero coordinates before attempting to analyze *)
  let convert_to_origin_zero (placement : Style.Grid.grid_placement) =
    match placement with
    | Auto -> None
    | Line i ->
        Some
          (GridLine.into_origin_zero_line (GridLine.create i)
             explicit_track_count)
    | Span _ -> None
  in

  let oz_start = convert_to_origin_zero line.start in
  let oz_end = convert_to_origin_zero line.end_ in

  let min =
    match (line.start, line.end_, oz_start, oz_end) with
    (* Both tracks specified *)
    | Line _, Line _, Some track1, Some track2 ->
        (* See rules A and B above *)
        if OriginZeroLine.equal track1 track2 then track1
        else if OriginZeroLine.compare track1 track2 < 0 then track1
        else track2
    (* Start track specified *)
    | Line _, Auto, Some track, _ -> track
    | Line _, Span _, Some track, _ -> track
    (* End track specified *)
    | Auto, Line _, _, Some track -> track
    | Span span, Line _, _, Some track -> OriginZeroLine.sub_int track span
    (* Only spans or autos *)
    (* We ignore spans here by returning 0 which never effect the estimate as these are accounted for separately *)
    | _ -> OriginZeroLine.create 0
  in

  let max =
    match (line.start, line.end_, oz_start, oz_end) with
    (* Both tracks specified *)
    | Line _, Line _, Some track1, Some track2 ->
        (* See rules A and B above *)
        if OriginZeroLine.equal track1 track2 then
          OriginZeroLine.add_int track1 1
        else if OriginZeroLine.compare track1 track2 > 0 then track1
        else track2
    (* Start track specified *)
    | Line _, Auto, Some track, _ -> OriginZeroLine.add_int track 1
    | Line _, Span span, Some track, _ -> OriginZeroLine.add_int track span
    (* End track specified *)
    | Auto, Line _, _, Some track -> track
    | Span _, Line _, _, Some track -> track
    (* Only spans or autos *)
    (* We ignore spans here by returning 0 which never effect the estimate as these are accounted for separately *)
    | _ -> OriginZeroLine.create 0
  in

  (* Calculate span only for indefinitely placed items as we don't need for other items (whose required space will
     be taken into account by min and max) *)
  let span =
    match (line.start, line.end_) with
    | Auto, Auto -> 1
    | Auto, Span s -> s
    | Span s, Auto -> s
    | Span s1, Span _s2 -> s1 (* Rule C: remove span from end *)
    | _ -> 1
  in

  (min, max, span)

(** Iterate over children, producing an estimate of the min and max grid *lines*
    along with the span of each item

    Min and max grid lines are returned in origin-zero coordinates) The span is
    measured in tracks spanned *)
let get_known_child_positions children_styles explicit_col_count
    explicit_row_count =
  let col_min = ref (OriginZeroLine.create 0) in
  let col_max = ref (OriginZeroLine.create 0) in
  let col_max_span = ref 0 in
  let row_min = ref (OriginZeroLine.create 0) in
  let row_max = ref (OriginZeroLine.create 0) in
  let row_max_span = ref 0 in

  List.iter
    (fun child_style ->
      (* Note: that the children reference the lines in between (and around) the tracks not tracks themselves,
       and thus we must subtract 1 to get an accurate estimate of the number of tracks *)
      let child_col_min, child_col_max, child_col_span =
        child_min_line_max_line_span child_style.Style.grid_column
          explicit_col_count
      in
      let child_row_min, child_row_max, child_row_span =
        child_min_line_max_line_span child_style.Style.grid_row
          explicit_row_count
      in

      if OriginZeroLine.compare child_col_min !col_min < 0 then
        col_min := child_col_min;
      if OriginZeroLine.compare child_col_max !col_max > 0 then
        col_max := child_col_max;
      if child_col_span > !col_max_span then col_max_span := child_col_span;

      if OriginZeroLine.compare child_row_min !row_min < 0 then
        row_min := child_row_min;
      if OriginZeroLine.compare child_row_max !row_max > 0 then
        row_max := child_row_max;
      if child_row_span > !row_max_span then row_max_span := child_row_span)
    children_styles;

  (!col_min, !col_max, !col_max_span, !row_min, !row_max, !row_max_span)

(** Estimate the number of rows and columns in the grid This is used as a
    performance optimisation to pre-size vectors and reduce allocations. It also
    forms a necessary step in the auto-placement
    - The estimates for the explicit and negative implicit track counts are
      exact.
    - However, the estimates for the positive explicit track count is a lower
      bound as auto-placement can affect this in ways which are impossible to
      predict until the auto-placement algorithm is run.

    Note that this function internally mixes use of grid track numbers and grid
    line numbers *)
let compute_grid_size_estimate ~explicit_col_count ~explicit_row_count
    ~child_styles =
  (* Iterate over children, producing an estimate of the min and max grid lines (in origin-zero coordinates where)
     along with the span of each item *)
  let col_min, col_max, col_max_span, row_min, row_max, row_max_span =
    get_known_child_positions child_styles explicit_col_count explicit_row_count
  in

  (* Compute *track* count estimates for each axis from:
       - The explicit track counts
       - The origin-zero coordinate min and max grid line variables *)
  let negative_implicit_inline_tracks =
    OriginZeroLine.implied_negative_implicit_tracks col_min
  in
  let explicit_inline_tracks = explicit_col_count in
  let positive_implicit_inline_tracks =
    ref
      (OriginZeroLine.implied_positive_implicit_tracks col_max
         explicit_col_count)
  in
  let negative_implicit_block_tracks =
    OriginZeroLine.implied_negative_implicit_tracks row_min
  in
  let explicit_block_tracks = explicit_row_count in
  let positive_implicit_block_tracks =
    ref
      (OriginZeroLine.implied_positive_implicit_tracks row_max
         explicit_row_count)
  in

  (* In each axis, adjust positive track estimate if any items have a span that does not fit within
     the total number of tracks in the estimate *)
  let tot_inline_tracks =
    negative_implicit_inline_tracks + explicit_inline_tracks
    + !positive_implicit_inline_tracks
  in
  if tot_inline_tracks < col_max_span then
    positive_implicit_inline_tracks :=
      col_max_span - explicit_inline_tracks - negative_implicit_inline_tracks;

  let tot_block_tracks =
    negative_implicit_block_tracks + explicit_block_tracks
    + !positive_implicit_block_tracks
  in
  if tot_block_tracks < row_max_span then
    positive_implicit_block_tracks :=
      row_max_span - explicit_block_tracks - negative_implicit_block_tracks;

  let column_counts =
    Grid_track_counts.from_raw negative_implicit_inline_tracks
      explicit_inline_tracks
      !positive_implicit_inline_tracks
  in

  let row_counts =
    Grid_track_counts.from_raw negative_implicit_block_tracks
      explicit_block_tracks
      !positive_implicit_block_tracks
  in

  (column_counts, row_counts)
