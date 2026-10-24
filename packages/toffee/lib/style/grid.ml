type origin_zero_line = int

module Origin_zero_placement = struct
  type t = Auto | Line of origin_zero_line | Span of int
end

type track_counts = {
  negative_implicit : int;
  explicit : int;
  positive_implicit : int;
}

type grid_line = int

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
  let implicit_start_line = -counts.negative_implicit in
  let end_line = counts.explicit + counts.positive_implicit in
  if line < implicit_start_line then None
  else if line >= end_line then None
  else
    let track_index = line + counts.negative_implicit in
    if track_index < total_track_count counts then Some track_index else None

(** Get the next track index from an origin-zero line *)
let oz_line_to_next_track (line : origin_zero_line) : int =
  if line < 0 then 0 else line

(** Convert an origin-zero line range to track range *)
let oz_line_range_to_track_range start_line end_line =
  let start_track = oz_line_to_next_track start_line in
  let end_track = oz_line_to_next_track end_line in
  (start_track, end_track)
