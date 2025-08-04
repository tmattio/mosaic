(* Grid-specific types and utilities for CSS Grid layout computation *)

(* Origin-zero coordinate types *)

type origin_zero_line = int
(** 0-indexed line coordinate used internally during grid layout *)

(** Grid placement in origin-zero coordinates *)
module Origin_zero_placement = struct
  type t = Auto | Line of origin_zero_line | Span of int
end

type track_counts = {
  negative_implicit : int;
      (* Number of implicit tracks before the explicit grid *)
  explicit : int; (* Number of explicit tracks *)
  positive_implicit : int;
      (* Number of implicit tracks after the explicit grid *)
}
(** Track counts for a grid axis *)

type grid_line = int
(** Grid line coordinate (CSS 1-indexed, negative allowed) *)

(* Coordinate conversion utilities *)

(** Convert CSS grid line to origin-zero line
    - Positive lines start at 1, so subtract 1 for 0-based index
    - Negative lines count from the end
    - Line 0 is invalid, treated as 0 *)
let grid_line_to_origin_zero_line (line : grid_line) explicit_track_count =
  if line > 0 then
    (* Positive lines start at 1, so subtract 1 for 0-based index *)
    line - 1
  else if line < 0 then
    (* Negative lines count from the end *)
    explicit_track_count + line + 1
  else
    (* Line 0 is invalid, treated as Auto *)
    0

(* TrackCounts utilities *)

(** Create track counts from explicit track count *)
let make_track_counts ~negative_implicit ~explicit ~positive_implicit =
  { negative_implicit; explicit; positive_implicit }

(** Get total track count *)
let total_track_count counts =
  counts.negative_implicit + counts.explicit + counts.positive_implicit

(** Convert origin-zero line to track index *)
let oz_line_to_track (line : origin_zero_line) (counts : track_counts) :
    int option =
  if line < 0 then None
  else if line < counts.negative_implicit then Some line
  else if line < counts.negative_implicit + counts.explicit then
    Some (line - counts.negative_implicit)
  else if line < total_track_count counts then
    Some (counts.explicit + line - counts.negative_implicit - counts.explicit)
  else None

(** Get the next track index from an origin-zero line *)
let oz_line_to_next_track (line : origin_zero_line) : int =
  if line < 0 then 0 else line

(** Convert an origin-zero line range to track range *)
let oz_line_range_to_track_range start_line end_line =
  let start_track = oz_line_to_next_track start_line in
  let end_track = oz_line_to_next_track end_line in
  (start_track, end_track)
