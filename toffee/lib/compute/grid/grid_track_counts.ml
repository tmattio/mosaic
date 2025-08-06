(* Stores the number of tracks in a given dimension.
    Stores separately the number of tracks in the implicit and explicit grids
    
    Taffy uses two coordinate systems to refer to grid tracks (rows/columns):
    
    Both of these systems represent the entire implicit grid, not just the explicit grid.
    
    "CellOccupancyMatrix track indices":
        - These are indexes into the CellOccupancyMatrix
        - The CellOccupancyMatrix stores only tracks
        - 0 is the leftmost track of the implicit grid, and indexes count up there
    
    "GridTrackVec track indices":
        - The GridTrackVecs store both lines and tracks, so:
            - even indices (0, 2, 4, etc) represent lines
            - odd indices (1, 3, 5, etc) represent tracks
            - There is always an odd number of elements
        - Index 1 is the leftmost track of the implicit grid. Index 3 is the second leftmost track, etc.
        - Index 0 is the leftmost grid line. Index 2 is the second leftmost line, etc. *)

open Geometry
open Style

type t = Grid.track_counts

(* Create a track counts instance from raw track count numbers *)
let make ~negative_implicit ~explicit ~positive_implicit =
  Grid.{ negative_implicit; explicit; positive_implicit }

(* Count the total number of tracks in the axis *)
let len t =
  t.Grid.negative_implicit + t.Grid.explicit + t.Grid.positive_implicit

(* Get the number of negative implicit tracks *)
let negative_implicit t = t.Grid.negative_implicit

(* Get the number of explicit tracks *)
let explicit t = t.Grid.explicit

(* Get the number of positive implicit tracks *)
let positive_implicit t = t.Grid.positive_implicit

(* The OriginZeroLine representing the start of the implicit grid *)
let implicit_start_line t = -t.Grid.negative_implicit

(* The OriginZeroLine representing the end of the implicit grid *)
let implicit_end_line t = t.Grid.explicit + t.Grid.positive_implicit

(* Converts a grid line in OriginZero coordinates into the track immediately
    following that grid line as an index into the CellOccupancyMatrix *)
let oz_line_to_next_track t index = index + t.Grid.negative_implicit

(* Converts start and end grid lines in OriginZero coordinates into a range of tracks
    as indexes into the CellOccupancyMatrix *)
let oz_line_range_to_track_range t input =
  let open Line in
  let start = oz_line_to_next_track t input.start in
  let end_ = oz_line_to_next_track t input.end_ in
  (* Don't subtract 1 as output range is exclusive *)
  (start, end_)

(* Converts a track as an index into the CellOccupancyMatrix into the grid line immediately
    preceding that track in OriginZero coordinates *)
let track_to_prev_oz_line t index = index - t.Grid.negative_implicit

(* Converts a range of tracks as indexes into the CellOccupancyMatrix into
    start and end grid lines in OriginZero coordinates *)
let track_range_to_oz_line_range t (start, end_) =
  let start = track_to_prev_oz_line t start in
  let end_ = track_to_prev_oz_line t end_ in
  (* Don't add 1 as input range is exclusive *)
  Line.make start end_

(* Converts an OriginZero grid line into a track index for the CellOccupancyMatrix,
    or None if the line is outside the grid *)
let oz_line_to_track t (line : int) : int option =
  let track_index = line + t.Grid.negative_implicit in
  if track_index < 0 || track_index >= len t then None
  else
    (* Multiply by 2 to account for gutters - each line maps to an even index *)
    Some (2 * track_index)
