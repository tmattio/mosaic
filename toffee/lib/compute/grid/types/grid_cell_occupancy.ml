(** cell_occupancy.ml
    ---------------------------------------------------------------------------
    Contains CellOccupancyMatrix used to track occupied cells during grid
    placement
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

open Grid_track_counts

(** The occupancy state of a single grid cell *)
type cell_occupancy_state =
  | Unoccupied  (** Indicates that a grid cell is unoccupied *)
  | DefinitelyPlaced
      (** Indicates that a grid cell is occupied by a definitely placed item *)
  | AutoPlaced
      (** Indicates that a grid cell is occupied by an item that was placed by
          the auto placement algorithm *)

type t = {
  mutable inner : cell_occupancy_state array array;
      (** The grid of occupancy states *)
  mutable columns : Grid_track_counts.t;
      (** The counts of implicit and explicit columns *)
  mutable rows : Grid_track_counts.t;
      (** The counts of implicit and explicit rows *)
}
(** A dynamically sized matrix (2d grid) which tracks the occupancy of each grid
    cell during auto-placement. It also keeps tabs on how many tracks there are
    and which tracks are implicit and which are explicit. *)

(** Create a CellOccupancyMatrix given a set of provisional track counts. The
    grid can expand as needed to fit more tracks, the provisional track counts
    represent a best effort attempt to avoid the extra allocations this
    requires. *)
let with_track_counts columns rows =
  let row_count = Grid_track_counts.len rows in
  let col_count = Grid_track_counts.len columns in
  { inner = Array.make_matrix row_count col_count Unoccupied; rows; columns }

(** Get the number of rows in the matrix *)
let rows t = Array.length t.inner

(** Get the number of columns in the matrix *)
let cols t = if rows t = 0 then 0 else Array.length t.inner.(0)

(** Determines whether the specified area fits within the tracks currently
    represented by the matrix *)
let is_area_in_range t primary_axis primary_range secondary_range =
  let open Geometry in
  let primary_start, primary_end = primary_range in
  let secondary_start, secondary_end = secondary_range in
  let primary_tracks =
    match primary_axis with Horizontal -> t.columns | Vertical -> t.rows
  in
  let secondary_tracks =
    match other_axis primary_axis with
    | Horizontal -> t.columns
    | Vertical -> t.rows
  in
  primary_start >= 0
  && primary_end <= Grid_track_counts.len primary_tracks
  && secondary_start >= 0
  && secondary_end <= Grid_track_counts.len secondary_tracks

(** Expands the grid (potentially in all 4 directions) in order to ensure that
    the specified range fits within the allocated space *)
let expand_to_fit_range t row_range col_range =
  let row_start, row_end = row_range in
  let col_start, col_end = col_range in

  (* Calculate number of rows and columns missing to accommodate ranges (if any) *)
  let req_negative_rows = max 0 (-row_start) in
  let req_positive_rows = max 0 (row_end - Grid_track_counts.len t.rows) in
  let req_negative_cols = max 0 (-col_start) in
  let req_positive_cols = max 0 (col_end - Grid_track_counts.len t.columns) in

  let old_row_count = rows t in
  let old_col_count = cols t in
  let new_row_count = old_row_count + req_negative_rows + req_positive_rows in
  let new_col_count = old_col_count + req_negative_cols + req_positive_cols in

  (* Create new matrix with expanded dimensions *)
  let new_matrix = Array.make_matrix new_row_count new_col_count Unoccupied in

  (* Copy existing data to the appropriate position in the new matrix *)
  for row = 0 to old_row_count - 1 do
    for col = 0 to old_col_count - 1 do
      new_matrix.(row + req_negative_rows).(col + req_negative_cols) <-
        t.inner.(row).(col)
    done
  done;

  (* Update self with new data *)
  t.inner <- new_matrix;
  t.rows <-
    {
      negative_implicit = t.rows.negative_implicit + req_negative_rows;
      explicit = t.rows.explicit;
      positive_implicit = t.rows.positive_implicit + req_positive_rows;
    };
  t.columns <-
    {
      negative_implicit = t.columns.negative_implicit + req_negative_cols;
      explicit = t.columns.explicit;
      positive_implicit = t.columns.positive_implicit + req_positive_cols;
    }

(** Returns the track counts of this CellOccunpancyMatrix in the relevant axis
*)
let track_counts t track_type =
  let open Geometry in
  match track_type with Horizontal -> t.columns | Vertical -> t.rows

(** Mark an area of the matrix as occupied, expanding the allocated space as
    necessary to accommodate the passed area. *)
let mark_area_as t primary_axis primary_span secondary_span value =
  let open Geometry in
  let row_span, column_span =
    match primary_axis with
    | Horizontal -> (secondary_span, primary_span)
    | Vertical -> (primary_span, secondary_span)
  in

  let col_range = oz_line_range_to_track_range t.columns column_span in
  let row_range = oz_line_range_to_track_range t.rows row_span in

  (* Check that if the resolved ranges fit within the allocated grid. And if they don't then expand the grid to fit
     and then re-resolve the ranges once the grid has been expanded as the resolved indexes may have changed *)
  let is_in_range = is_area_in_range t Horizontal col_range row_range in
  let col_range, row_range =
    if not is_in_range then (
      expand_to_fit_range t row_range col_range;
      let col_range = oz_line_range_to_track_range t.columns column_span in
      let row_range = oz_line_range_to_track_range t.rows row_span in
      (col_range, row_range))
    else (col_range, row_range)
  in

  let row_start, row_end = row_range in
  let col_start, col_end = col_range in
  for x = row_start to row_end - 1 do
    for y = col_start to col_end - 1 do
      t.inner.(x).(y) <- value
    done
  done

(** Determines whether a grid area specified by the bounding grid lines in
    OriginZero coordinates is entirely unnocupied. Returns true if all grid
    cells within the grid area are unoccupied, else false. *)
let rec line_area_is_unoccupied t primary_axis primary_span secondary_span =
  let open Geometry in
  let primary_counts = track_counts t primary_axis in
  let secondary_counts = track_counts t (other_axis primary_axis) in
  let primary_range =
    oz_line_range_to_track_range primary_counts primary_span
  in
  let secondary_range =
    oz_line_range_to_track_range secondary_counts secondary_span
  in
  track_area_is_unoccupied t primary_axis primary_range secondary_range

(** Determines whether a grid area specified by a range of indexes into this
    CellOccupancyMatrix is entirely unnocupied. Returns true if all grid cells
    within the grid area are unoccupied, else false. *)
and track_area_is_unoccupied t primary_axis primary_range secondary_range =
  let open Geometry in
  let row_range, col_range =
    match primary_axis with
    | Horizontal -> (secondary_range, primary_range)
    | Vertical -> (primary_range, secondary_range)
  in

  let row_start, row_end = row_range in
  let col_start, col_end = col_range in

  (* Search for occupied cells in the specified area. Out of bounds cells are considered unoccupied. *)
  let rec check_rows x =
    if x >= row_end then true
    else
      let rec check_cols y =
        if y >= col_end then true
        else if x >= 0 && x < rows t && y >= 0 && y < cols t then
          match t.inner.(x).(y) with
          | Unoccupied -> check_cols (y + 1)
          | _ -> false
        else check_cols (y + 1)
      in
      if check_cols col_start then check_rows (x + 1) else false
  in
  check_rows row_start

(** Determines whether the specified row contains any items *)
let row_is_occupied t row_index =
  if row_index >= rows t then false
  else Array.exists (fun cell -> cell <> Unoccupied) t.inner.(row_index)

(** Determines whether the specified column contains any items *)
let column_is_occupied t column_index =
  if column_index >= cols t then false
  else
    let rec check_rows row =
      if row >= rows t then false
      else if t.inner.(row).(column_index) <> Unoccupied then true
      else check_rows (row + 1)
    in
    check_rows 0

(** Given an axis and a track index Search backwards from the end of the track
    and find the last grid cell matching the specified state (if any) Return the
    index of that cell or None. *)
let last_of_type t track_type start_at kind =
  let open Geometry in
  let track_counts = track_counts t (other_axis track_type) in
  let track_computed_index = oz_line_to_next_track track_counts start_at in

  let maybe_index =
    match track_type with
    | Horizontal ->
        if track_computed_index >= 0 && track_computed_index < rows t then
          let row = t.inner.(track_computed_index) in
          let rec find_last idx last_found =
            if idx >= Array.length row then last_found
            else if row.(idx) = kind then find_last (idx + 1) (Some idx)
            else find_last (idx + 1) last_found
          in
          find_last 0 None
        else None
    | Vertical ->
        if track_computed_index >= 0 && track_computed_index < cols t then
          let rec find_last idx last_found =
            if idx >= rows t then last_found
            else if t.inner.(idx).(track_computed_index) = kind then
              find_last (idx + 1) (Some idx)
            else find_last (idx + 1) last_found
          in
          find_last 0 None
        else None
  in

  Option.map (fun idx -> track_to_prev_oz_line track_counts idx) maybe_index
