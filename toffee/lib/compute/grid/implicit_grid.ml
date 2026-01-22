(* This module is not required for spec compliance, but is used as a performance
   optimisation to reduce the number of allocations required when creating a
   grid. *)

open Geometry
open Style
open Style.Grid

(* Helper functions for origin zero line *)
let implied_negative_implicit_tracks line = if line < 0 then abs line else 0

let implied_positive_implicit_tracks line explicit_track_count =
  if line > explicit_track_count then line - explicit_track_count else 0

(* Resolves the span for an indefinite placement (a placement that does not
   consist of two Lines). Should only be called on indefinite placements *)
let indefinite_span line =
  let open Origin_zero_placement in
  match (line.Line.start, line.Line.end_) with
  | Line _, Auto -> 1
  | Auto, Line _ -> 1
  | Auto, Auto -> 1
  | Line _, Span span -> span
  | Span span, Line _ -> span
  | Span span, Auto -> span
  | Auto, Span span -> span
  | Span span, Span _ -> span
  | Line _, Line _ ->
      failwith "indefinite_span should only be called on indefinite grid tracks"

(* Helper function for compute_grid_size_estimate Produces a conservative
   estimate of the greatest and smallest grid lines used by a single grid item

   Values are returned in origin-zero coordinates *)
let child_min_line_max_line_span line explicit_track_count =
  (* 8.3.1. Grid Placement Conflict Handling A. If the placement for a grid item
     contains two lines, and the start line is further end-ward than the end
     line, swap the two lines. B. If the start line is equal to the end line,
     remove the end line. C. If the placement contains two spans, remove the one
     contributed by the end grid-placement property. D. If the placement
     contains only a span for a named line, replace it with a span of 1. *)

  (* Convert line into origin-zero coordinates before attempting to analyze We
     ignore named lines here as they are accounted for separately *)
  let oz_line =
    Grid.Placement.Line.into_origin_zero_ignoring_named line
      explicit_track_count
  in

  let min =
    match (oz_line.Line.start, oz_line.Line.end_) with
    (* Both tracks specified *)
    | Origin_zero_placement.Line track1, Origin_zero_placement.Line track2 ->
        (* See rules A and B above *)
        if track1 = track2 then track1 else min track1 track2
    (* Start track specified *)
    | Origin_zero_placement.Line track, Origin_zero_placement.Auto -> track
    | Origin_zero_placement.Line track, Origin_zero_placement.Span _ -> track
    (* End track specified *)
    | Origin_zero_placement.Auto, Origin_zero_placement.Line track -> track
    | Origin_zero_placement.Span span, Origin_zero_placement.Line track ->
        track - span
    (* Only spans or autos *)
    (* We ignore spans here by returning 0 which never effect the estimate as these are accounted for separately *)
    | ( (Origin_zero_placement.Auto | Origin_zero_placement.Span _),
        (Origin_zero_placement.Auto | Origin_zero_placement.Span _) ) ->
        0
  in

  let max =
    match (oz_line.Line.start, oz_line.Line.end_) with
    (* Both tracks specified *)
    | Origin_zero_placement.Line track1, Origin_zero_placement.Line track2 ->
        (* See rules A and B above *)
        if track1 = track2 then track1 + 1 else max track1 track2
    (* Start track specified *)
    | Origin_zero_placement.Line track, Origin_zero_placement.Auto -> track + 1
    | Origin_zero_placement.Line track, Origin_zero_placement.Span span ->
        track + span
    (* End track specified *)
    | Origin_zero_placement.Auto, Origin_zero_placement.Line track -> track
    | Origin_zero_placement.Span _, Origin_zero_placement.Line track -> track
    (* Only spans or autos *)
    (* We ignore spans here by returning 0 which never effect the estimate as these are accounted for separately *)
    | ( (Origin_zero_placement.Auto | Origin_zero_placement.Span _),
        (Origin_zero_placement.Auto | Origin_zero_placement.Span _) ) ->
        0
  in

  (* Calculate span only for indefinitely placed items as we don't need for
     other items (whose required space will be taken into account by min and
     max) *)
  let span =
    match (oz_line.Line.start, oz_line.Line.end_) with
    | ( (Origin_zero_placement.Auto | Origin_zero_placement.Span _),
        (Origin_zero_placement.Auto | Origin_zero_placement.Span _) ) ->
        indefinite_span oz_line
    | _ -> 1
  in

  (min, max, span)

(* Iterate over children, producing an estimate of the min and max grid *lines*
   along with the span of each item

   Min and max grid lines are returned in origin-zero coordinates) The span is
   measured in tracks spanned *)
let get_known_child_positions children_seq explicit_col_count explicit_row_count
    =
  let col_min = ref 0 in
  let col_max = ref 0 in
  let col_max_span = ref 0 in
  let row_min = ref 0 in
  let row_max = ref 0 in
  let row_max_span = ref 0 in

  Seq.iter
    (fun child_style ->
      (* Note: children reference the lines in between (and around) the tracks
         not tracks themselves, and thus we must subtract 1 to get an accurate
         estimate of the number of tracks *)
      let child_col_min, child_col_max, child_col_span =
        child_min_line_max_line_span
          (Style.grid_column child_style)
          explicit_col_count
      in
      let child_row_min, child_row_max, child_row_span =
        child_min_line_max_line_span
          (Style.grid_row child_style)
          explicit_row_count
      in

      col_min := min !col_min child_col_min;
      col_max := max !col_max child_col_max;
      col_max_span := max !col_max_span child_col_span;
      row_min := min !row_min child_row_min;
      row_max := max !row_max child_row_max;
      row_max_span := max !row_max_span child_row_span)
    children_seq;

  (!col_min, !col_max, !col_max_span, !row_min, !row_max, !row_max_span)

(* Estimate the number of rows and columns in the grid This is used as a
   performance optimisation to pre-size vectors and reduce allocations. It also
   forms a necessary step in the auto-placement - The estimates for the explicit
   and negative implicit track counts are exact. - However, the estimates for
   the positive explicit track count is a lower bound as auto-placement can
   affect this in ways which are impossible to predict until the auto-placement
   algorithm is run.

   Note that this function internally mixes use of grid track numbers and grid
   line numbers *)
let compute_grid_size_estimate ~explicit_col_count ~explicit_row_count
    ~child_styles_iter =
  (* Iterate over children, producing an estimate of the min and max grid lines
     (in origin-zero coordinates where) along with the span of each item *)
  let col_min, col_max, col_max_span, row_min, row_max, row_max_span =
    get_known_child_positions child_styles_iter explicit_col_count
      explicit_row_count
  in

  (* Compute *track* count estimates for each axis from: - The explicit track
     counts - The origin-zero coordinate min and max grid line variables *)
  let negative_implicit_inline_tracks =
    implied_negative_implicit_tracks col_min
  in
  let explicit_inline_tracks = explicit_col_count in
  let positive_implicit_inline_tracks =
    ref (implied_positive_implicit_tracks col_max explicit_col_count)
  in
  let negative_implicit_block_tracks =
    implied_negative_implicit_tracks row_min
  in
  let explicit_block_tracks = explicit_row_count in
  let positive_implicit_block_tracks =
    ref (implied_positive_implicit_tracks row_max explicit_row_count)
  in

  (* In each axis, adjust positive track estimate if any items have a span that
     does not fit within the total number of tracks in the estimate *)
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
    Grid_track_counts.make ~negative_implicit:negative_implicit_inline_tracks
      ~explicit:explicit_inline_tracks
      ~positive_implicit:!positive_implicit_inline_tracks
  in

  let row_counts =
    Grid_track_counts.make ~negative_implicit:negative_implicit_block_tracks
      ~explicit:explicit_block_tracks
      ~positive_implicit:!positive_implicit_block_tracks
  in

  (column_counts, row_counts)
