(** grid_track_counts.ml
    ---------------------------------------------------------------------------
    Contains TrackCounts used to keep track of the number of tracks in the
    explicit and implicit grids. Also contains coordinate conversion functions
    which depend on those counts.
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

open Grid_coordinates

type t = {
  negative_implicit : int;
      (** The number of tracks in the implicit grid before the explicit grid *)
  explicit : int;  (** The number of tracks in the explicit grid *)
  positive_implicit : int;
      (** The number of tracks in the implicit grid after the explicit grid *)
}
(** Stores the number of tracks in a given dimension. Stores separately the
    number of tracks in the implicit and explicit grids *)

let from_raw negative_implicit explicit positive_implicit =
  { negative_implicit; explicit; positive_implicit }

let default = { negative_implicit = 0; explicit = 0; positive_implicit = 0 }

(** Count the total number of tracks in the axis *)
let len t = t.negative_implicit + t.explicit + t.positive_implicit

(** The OriginZeroLine representing the start of the implicit grid *)
let implicit_start_line t = OriginZeroLine.create (-t.negative_implicit)

(** The OriginZeroLine representing the end of the implicit grid *)
let implicit_end_line t =
  OriginZeroLine.create (t.explicit + t.positive_implicit)

(** Conversion functions between OriginZero coordinates and CellOccupancyMatrix
    track indexes *)

(** Converts a grid line in OriginZero coordinates into the track immediately
    following that grid line as an index into the CellOccupancyMatrix. *)
let oz_line_to_next_track t (OriginZeroLine.OriginZeroLine index) =
  index + t.negative_implicit

(** Converts start and end grid lines in OriginZero coordinates into a range of
    tracks as indexes into the CellOccupancyMatrix *)
let oz_line_range_to_track_range t line =
  let open Geometry in
  let start = oz_line_to_next_track t line.start in
  let end_ = oz_line_to_next_track t line.end_ in
  (start, end_)

(** Converts a track as an index into the CellOccupancyMatrix into the grid line
    immediately preceding that track in OriginZero coordinates. *)
let track_to_prev_oz_line t index =
  OriginZeroLine.create (index - t.negative_implicit)

(** Converts a range of tracks as indexes into the CellOccupancyMatrix into
    start and end grid lines in OriginZero coordinates *)
let track_range_to_oz_line_range t (start, end_) =
  let open Geometry in
  { start = track_to_prev_oz_line t start; end_ = track_to_prev_oz_line t end_ }
