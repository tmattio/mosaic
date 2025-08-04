(** placement.ml
    ---------------------------------------------------------------------------
    Implements placing items in the grid and resolving the implicit grid.
    https://www.w3.org/TR/css-grid-1/#placement
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

open Geometry
open Style
open Style.Grid

(* Style.Alignment is used for types in function signatures *)
open Grid_grid_axis_helpers
open Grid_cell_occupancy
open Grid_item
open Grid_coordinates

(** Helper functions for grid_auto_flow *)
module GridAutoFlow = struct
  let primary_axis = function
    | Row | Row_dense -> Absolute_axis.Horizontal
    | Column | Column_dense -> Absolute_axis.Vertical

  let is_dense = function
    | Row_dense | Column_dense -> true
    | Row | Column -> false
end

type 'a in_both_abs_axis = { horizontal : 'a; vertical : 'a }
(** Type for in-both-abs-axis placement *)

(** Type for origin zero grid placement *)
type origin_zero_grid_placement =
  | Auto
  | Line of OriginZeroLine.t
  | Span of int

(** Convert grid placement to origin zero placement *)
let to_origin_zero_placement (placement : grid_placement) explicit_track_count :
    origin_zero_grid_placement =
  match placement with
  | Auto -> Auto
  | Line i ->
      Line
        (GridLine.into_origin_zero_line (GridLine.create i) explicit_track_count)
  | Span s -> Span s

(** Resolve definite grid lines *)
let resolve_definite_grid_lines line =
  match (line.start, line.end_) with
  | Line start, Line end_ -> { start; end_ }
  | Line start, Span span -> { start; end_ = OriginZeroLine.add_int start span }
  | Span span, Line end_ -> { start = OriginZeroLine.sub_int end_ span; end_ }
  | Line start, Auto -> { start; end_ = OriginZeroLine.add_int start 1 }
  | Auto, Line end_ -> { start = OriginZeroLine.sub_int end_ 1; end_ }
  | _ -> failwith "resolve_definite_grid_lines called on indefinite grid lines"

(** Resolve indefinite grid tracks *)
let resolve_indefinite_grid_tracks line position =
  match (line.start, line.end_) with
  | Auto, Auto -> { start = position; end_ = OriginZeroLine.add_int position 1 }
  | Auto, Span span ->
      { start = position; end_ = OriginZeroLine.add_int position span }
  | Span span, Auto ->
      { start = position; end_ = OriginZeroLine.add_int position span }
  | _ -> resolve_definite_grid_lines line

(** Get indefinite span *)
let indefinite_span line =
  match (line.start, line.end_) with
  | Auto, Auto -> 1
  | Auto, Span s | Span s, Auto -> s
  | Span s1, Span _s2 -> s1 (* Rule: remove span from end *)
  | _ -> 1

(** Check if placement is definite *)
let is_definite line =
  match (line.start, line.end_) with
  | Line _, _ | _, Line _ -> true
  | _ -> false

(** Resolve absolutely positioned grid tracks For absolutely positioned items:
    - Tracks resolve to definite tracks
    - For Spans:
    - If the other position is a Track, they resolve to a definite track
      relative to the other track
    - Else resolve to None
    - Auto resolves to None

    When finally positioning the item, a value of None means that the item's
    grid area is bounded by the grid container's border box on that side. *)
let resolve_absolutely_positioned_grid_tracks line =
  match (line.start, line.end_) with
  | Line track1, Line track2 ->
      if track1 = track2 then
        { start = Some track1; end_ = Some (OriginZeroLine.add_int track1 1) }
      else
        let start_track = min track1 track2 in
        let end_track = max track1 track2 in
        { start = Some start_track; end_ = Some end_track }
  | Line track, Span span ->
      { start = Some track; end_ = Some (OriginZeroLine.add_int track span) }
  | Line track, Auto -> { start = Some track; end_ = None }
  | Span span, Line track ->
      { start = Some (OriginZeroLine.sub_int track span); end_ = Some track }
  | Auto, Line track -> { start = None; end_ = Some track }
  | _ -> { start = None; end_ = None }

(** Get axis from in_both_abs_axis *)
let get_axis placement axis =
  match axis with
  | Absolute_axis.Horizontal -> placement.horizontal
  | Absolute_axis.Vertical -> placement.vertical

(** Get grid placement for an axis *)
let grid_placement style axis =
  match axis with
  | Absolute_axis.Horizontal -> style.Style.grid_column
  | Absolute_axis.Vertical -> style.Style.grid_row

(** 8.5. Grid Item Placement Algorithm Place a single definitely placed item
    into the grid *)
let place_definite_grid_item placement primary_axis =
  (* Resolve spans to tracks *)
  let primary_span =
    resolve_definite_grid_lines (get_axis placement primary_axis)
  in
  let secondary_span =
    resolve_definite_grid_lines
      (get_axis placement (Absolute_axis.other_axis primary_axis))
  in
  (primary_span, secondary_span)

(** 8.5. Grid Item Placement Algorithm Step 2. Place remaining children with
    definite secondary axis positions *)
let place_definite_secondary_axis_item cell_occupancy_matrix placement auto_flow
    =
  let primary_axis = GridAutoFlow.primary_axis auto_flow in
  let secondary_axis = Absolute_axis.other_axis primary_axis in

  let secondary_axis_placement =
    resolve_definite_grid_lines (get_axis placement secondary_axis)
  in
  let primary_axis_grid_start_line =
    Grid_track_counts.implicit_start_line
      (track_counts cell_occupancy_matrix primary_axis)
  in
  let starting_position =
    if GridAutoFlow.is_dense auto_flow then primary_axis_grid_start_line
    else
      match
        last_of_type cell_occupancy_matrix primary_axis
          secondary_axis_placement.start AutoPlaced
      with
      | Some pos -> pos
      | None -> primary_axis_grid_start_line
  in

  let rec find_position position =
    let primary_axis_placement =
      resolve_indefinite_grid_tracks (get_axis placement primary_axis) position
    in

    let does_fit =
      line_area_is_unoccupied cell_occupancy_matrix primary_axis
        primary_axis_placement secondary_axis_placement
    in

    if does_fit then (primary_axis_placement, secondary_axis_placement)
    else find_position (OriginZeroLine.add_int position 1)
  in

  find_position starting_position

(** 8.5. Grid Item Placement Algorithm Step 4. Position the remaining grid
    items. *)
let place_indefinitely_positioned_item cell_occupancy_matrix placement auto_flow
    grid_position =
  let primary_axis = GridAutoFlow.primary_axis auto_flow in
  let secondary_axis = Absolute_axis.other_axis primary_axis in

  let primary_placement_style = get_axis placement primary_axis in
  let secondary_placement_style = get_axis placement secondary_axis in

  let secondary_span = indefinite_span secondary_placement_style in
  let has_definite_primary_axis_position =
    is_definite primary_placement_style
  in
  let primary_axis_grid_start_line =
    Grid_track_counts.implicit_start_line
      (track_counts cell_occupancy_matrix primary_axis)
  in
  let primary_axis_grid_end_line =
    Grid_track_counts.implicit_end_line
      (track_counts cell_occupancy_matrix primary_axis)
  in
  let secondary_axis_grid_start_line =
    Grid_track_counts.implicit_start_line
      (track_counts cell_occupancy_matrix secondary_axis)
  in

  let line_area_is_occupied primary_span secondary_span =
    not
      (line_area_is_unoccupied cell_occupancy_matrix primary_axis primary_span
         secondary_span)
  in

  let primary_idx, secondary_idx = grid_position in

  if has_definite_primary_axis_position then
    let primary_span = resolve_definite_grid_lines primary_placement_style in

    (* Compute secondary axis starting position for search *)
    let secondary_idx =
      if GridAutoFlow.is_dense auto_flow then
        (* If auto-flow is dense then we always search from the first track *)
        secondary_axis_grid_start_line
      else if OriginZeroLine.compare primary_span.start primary_idx < 0 then
        OriginZeroLine.add_int secondary_idx 1
      else secondary_idx
    in

    (* Item has fixed primary axis position: so we simply increment the secondary axis position
       until we find a space that the item fits in *)
    let rec find_secondary_position secondary_idx =
      let secondary_span =
        {
          start = secondary_idx;
          end_ = OriginZeroLine.add_int secondary_idx secondary_span;
        }
      in

      (* If area is occupied, increment the index and try again *)
      if line_area_is_occupied primary_span secondary_span then
        find_secondary_position (OriginZeroLine.add_int secondary_idx 1)
      else
        (* Once we find a free space, return that position *)
        (primary_span, secondary_span)
    in

    find_secondary_position secondary_idx
  else
    let primary_span = indefinite_span primary_placement_style in

    (* Item does not have any fixed axis, so we search along the primary axis until we hit the end of the already
       existent tracks, and then we reset the primary axis back to zero and increment the secondary axis index.
       We continue in this vein until we find a space that the item fits in. *)
    let rec find_position primary_idx secondary_idx =
      let primary_span =
        {
          start = primary_idx;
          end_ = OriginZeroLine.add_int primary_idx primary_span;
        }
      in
      let secondary_span =
        {
          start = secondary_idx;
          end_ = OriginZeroLine.add_int secondary_idx secondary_span;
        }
      in

      (* If the primary index is out of bounds, then increment the secondary index and reset the primary
         index back to the start of the grid *)
      let primary_out_of_bounds =
        OriginZeroLine.compare primary_span.end_ primary_axis_grid_end_line > 0
      in
      if primary_out_of_bounds then
        find_position primary_axis_grid_start_line
          (OriginZeroLine.add_int secondary_idx 1)
        (* If area is occupied, increment the primary index and try again *)
      else if line_area_is_occupied primary_span secondary_span then
        find_position (OriginZeroLine.add_int primary_idx 1) secondary_idx
      else
        (* Once we find a free space that's in bounds, return that position *)
        (primary_span, secondary_span)
    in

    find_position primary_idx secondary_idx

(** Record the grid item in both CellOccupancyMatrix and the GridItems list once
    a definite placement has been determined *)
let record_grid_placement cell_occupancy_matrix items node index style
    parent_align_items parent_justify_items primary_axis primary_span
    secondary_span placement_type =
  (* Mark area of grid as occupied *)
  mark_area_as cell_occupancy_matrix primary_axis primary_span secondary_span
    placement_type;

  (* Create grid item *)
  let col_span, row_span =
    match primary_axis with
    | Absolute_axis.Horizontal -> (primary_span, secondary_span)
    | Absolute_axis.Vertical -> (secondary_span, primary_span)
  in

  let item =
    new_with_placement_style_and_order ~node ~col_span ~row_span ~style
      ~parent_align_items ~parent_justify_items ~source_order:index
  in

  items := item :: !items

(** 8.5. Grid Item Placement Algorithm Place items into the grid, generating new
    rows/column into the implicit grid as required

    [Specification](https://www.w3.org/TR/css-grid-2/#auto-placement-algo) *)
let place_grid_items ~cell_occupancy_matrix ~items ~children_styles
    ~grid_auto_flow ~align_items ~justify_items =
  let primary_axis = GridAutoFlow.primary_axis grid_auto_flow in
  let secondary_axis = Absolute_axis.other_axis primary_axis in

  let explicit_col_count =
    (track_counts cell_occupancy_matrix Absolute_axis.Horizontal).explicit
  in
  let explicit_row_count =
    (track_counts cell_occupancy_matrix Absolute_axis.Vertical).explicit
  in

  let map_child_style_to_origin_zero_placement (index, node, style) =
    let origin_zero_placement =
      {
        horizontal =
          {
            start =
              to_origin_zero_placement style.grid_column.start
                explicit_col_count;
            end_ =
              to_origin_zero_placement style.grid_column.end_ explicit_col_count;
          };
        vertical =
          {
            start =
              to_origin_zero_placement style.grid_row.start explicit_row_count;
            end_ =
              to_origin_zero_placement style.grid_row.end_ explicit_row_count;
          };
      }
    in
    (index, node, origin_zero_placement, style)
  in

  (* 1. Place children with definite positions *)
  List.iter
    (fun child ->
      let _, _, style = child in
      let row_placement =
        {
          start =
            to_origin_zero_placement style.grid_row.start explicit_row_count;
          end_ = to_origin_zero_placement style.grid_row.end_ explicit_row_count;
        }
      in
      let col_placement =
        {
          start =
            to_origin_zero_placement style.grid_column.start explicit_col_count;
          end_ =
            to_origin_zero_placement style.grid_column.end_ explicit_col_count;
        }
      in
      if is_definite row_placement && is_definite col_placement then
        let index, child_node, child_placement, style =
          map_child_style_to_origin_zero_placement child
        in
        let row_span, col_span =
          place_definite_grid_item child_placement primary_axis
        in
        record_grid_placement cell_occupancy_matrix items child_node index style
          align_items justify_items primary_axis row_span col_span
          DefinitelyPlaced)
    children_styles;

  (* 2. Place remaining children with definite secondary axis positions *)
  List.iter
    (fun child ->
      let index, child_node, child_placement, style =
        map_child_style_to_origin_zero_placement child
      in
      let primary_definite =
        is_definite
          (match primary_axis with
          | Absolute_axis.Horizontal -> child_placement.horizontal
          | Absolute_axis.Vertical -> child_placement.vertical)
      in
      let secondary_definite =
        is_definite
          (match secondary_axis with
          | Absolute_axis.Horizontal -> child_placement.horizontal
          | Absolute_axis.Vertical -> child_placement.vertical)
      in
      if secondary_definite && not primary_definite then
        let primary_span, secondary_span =
          place_definite_secondary_axis_item cell_occupancy_matrix
            child_placement grid_auto_flow
        in
        record_grid_placement cell_occupancy_matrix items child_node index style
          align_items justify_items primary_axis primary_span secondary_span
          AutoPlaced)
    children_styles;

  (* 3. Determine the number of columns in the implicit grid *)
  (* This is already handled by the grid size estimation and expansion logic *)

  (* 4. Position the remaining grid items *)
  let primary_neg_tracks =
    (track_counts cell_occupancy_matrix primary_axis).negative_implicit
  in
  let secondary_neg_tracks =
    (track_counts cell_occupancy_matrix secondary_axis).negative_implicit
  in
  let grid_start_position =
    ( OriginZeroLine.create (-primary_neg_tracks),
      OriginZeroLine.create (-secondary_neg_tracks) )
  in
  let grid_position = ref grid_start_position in

  List.iter
    (fun child ->
      let index, child_node, child_placement, style =
        map_child_style_to_origin_zero_placement child
      in
      let secondary_definite =
        is_definite
          (match secondary_axis with
          | Absolute_axis.Horizontal -> child_placement.horizontal
          | Absolute_axis.Vertical -> child_placement.vertical)
      in
      if not secondary_definite then (
        (* Compute placement *)
        let primary_span, secondary_span =
          place_indefinitely_positioned_item cell_occupancy_matrix
            child_placement grid_auto_flow !grid_position
        in

        (* Record item *)
        record_grid_placement cell_occupancy_matrix items child_node index style
          align_items justify_items primary_axis primary_span secondary_span
          AutoPlaced;

        (* If using the "dense" placement algorithm then reset the grid position back to grid_start_position ready for the next item
         Otherwise set it to the position of the current item so that the next item it placed after it. *)
        grid_position :=
          if GridAutoFlow.is_dense grid_auto_flow then grid_start_position
          else (primary_span.end_, secondary_span.start)))
    children_styles;

  (* Return items in reverse order since we built the list by prepending *)
  List.rev !items
