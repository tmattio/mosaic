(** coordinates.ml
    ---------------------------------------------------------------------------
    Taffy uses two coordinate systems to refer to grid lines (the gaps/gutters
    between rows/columns).
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

(** Represents a grid line position in "OriginZero" coordinates

    "OriginZero" coordinates are a normalized form:
    - The line at left hand (or top) edge of the explicit grid is line 0
    - The next line to the right (or down) is 1, and so on
    - The next line to the left (or up) is -1, and so on *)
module OriginZeroLine = struct
  type t = OriginZeroLine of int

  let create i = OriginZeroLine i
  let as_int (OriginZeroLine i) = i
  let compare (OriginZeroLine a) (OriginZeroLine b) = Int.compare a b
  let equal a b = compare a b = 0
  let ( + ) (OriginZeroLine a) (OriginZeroLine b) = OriginZeroLine (Int.add a b)
  let ( - ) (OriginZeroLine a) (OriginZeroLine b) = OriginZeroLine (Int.sub a b)
  let add_int (OriginZeroLine a) (b : int) = OriginZeroLine (Int.add a b)
  let sub_int (OriginZeroLine a) (b : int) = OriginZeroLine (Int.sub a b)

  (** Converts a grid line in OriginZero coordinates into the index of that same
      grid line in the GridTrackVec. *)
  let into_track_vec_index (OriginZeroLine i) ~negative_implicit ~explicit
      ~positive_implicit =
    assert (i >= -negative_implicit);
    assert (i <= Int.add explicit positive_implicit);
    2 * Int.add i negative_implicit

  (** The minimum number of negative implicit track there must be if a grid item
      starts at this line. *)
  let implied_negative_implicit_tracks (OriginZeroLine i) =
    if i < 0 then abs i else 0

  (** The minimum number of positive implicit track there must be if a grid item
      end at this line. *)
  let implied_positive_implicit_tracks (OriginZeroLine i) explicit_track_count =
    if i > explicit_track_count then Int.sub i explicit_track_count else 0
end

(** Represents a grid line position in "CSS Grid Line" coordinates

    "CSS Grid Line" coordinates are those used in grid-row/grid-column in the
    CSS grid spec:
    - The line at left hand (or top) edge of the explicit grid is line 1 (and
      counts up from there)
    - The line at the right hand (or bottom) edge of the explicit grid is -1
      (and counts down from there)
    - 0 is not a valid index *)
module GridLine = struct
  type t = GridLine of int

  let create i = GridLine i
  let as_int (GridLine i) = i

  (** Convert into OriginZero coordinates using the specified explicit track
      count *)
  let into_origin_zero_line (GridLine i) explicit_track_count =
    let explicit_line_count = explicit_track_count + 1 in
    let oz_line =
      if i > 0 then i - 1
      else if i < 0 then i + explicit_line_count
      else failwith "Grid line of zero is invalid"
    in
    OriginZeroLine.create oz_line
end

(** Extension for Line type when used with OriginZeroLine *)
module Line_ext = struct
  open Geometry

  (** The number of tracks between the start and end lines *)
  let span line =
    let (OriginZeroLine.OriginZeroLine start) = line.start in
    let (OriginZeroLine.OriginZeroLine end_) = line.end_ in
    max (end_ - start) 0
end

(** A trait for the different coordinates used to define grid lines. *)
module type GridCoordinate = sig
  type t
end
