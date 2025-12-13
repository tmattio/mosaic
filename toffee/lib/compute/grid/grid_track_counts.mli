(** Track count management and coordinate system conversions for CSS Grid
    layout.

    This module maintains counts of tracks in the implicit and explicit grids
    and provides conversions between two coordinate systems used internally:

    - OriginZero coordinates: Normalized grid line indices where the left/top
      edge of the explicit grid is line 0, counting up to the right/down and
      down to the left/up (negative indices).

    - CellOccupancyMatrix track indices: Zero-based indices into the track
      occupancy matrix, where 0 is the leftmost track of the implicit grid.

    Invariant: The total track count equals
    [negative_implicit + explicit + positive_implicit].

    The module also handles conversion to GridTrackVec indices, where even
    indices represent lines and odd indices represent tracks. *)

open Geometry
open Style

type t = Grid.track_counts

(** {1 Construction} *)

val make : negative_implicit:int -> explicit:int -> positive_implicit:int -> t
(** [make ~negative_implicit ~explicit ~positive_implicit] creates track counts
    from raw numbers. *)

(** {1 Track Count Queries} *)

val len : t -> int
(** [len t] returns the total number of tracks across implicit and explicit
    grids. *)

val negative_implicit : t -> int
(** [negative_implicit t] returns the number of tracks in the negative implicit
    grid. *)

val explicit : t -> int
(** [explicit t] returns the number of tracks in the explicit grid. *)

val positive_implicit : t -> int
(** [positive_implicit t] returns the number of tracks in the positive implicit
    grid. *)

(** {1 Grid Boundaries} *)

val implicit_start_line : t -> int
(** [implicit_start_line t] returns the OriginZero line at the start of the
    implicit grid. Always negative or zero. *)

val implicit_end_line : t -> int
(** [implicit_end_line t] returns the OriginZero line at the end of the implicit
    grid. Always positive or zero. *)

(** {1 Coordinate Conversions} *)

val oz_line_to_next_track : t -> int -> int
(** [oz_line_to_next_track t line] converts an OriginZero grid line to the index
    of the track immediately following it in the CellOccupancyMatrix. *)

val oz_line_range_to_track_range : t -> int Line.t -> int * int
(** [oz_line_range_to_track_range t range] converts a line range in OriginZero
    coordinates to an exclusive track index range in the CellOccupancyMatrix. *)

val track_to_prev_oz_line : t -> int -> int
(** [track_to_prev_oz_line t index] converts a CellOccupancyMatrix track index
    to the OriginZero line immediately preceding it. *)

val track_range_to_oz_line_range : t -> int * int -> int Line.t
(** [track_range_to_oz_line_range t range] converts an exclusive track index
    range from the CellOccupancyMatrix to a line range in OriginZero
    coordinates. *)

val oz_line_to_track : t -> int -> int option
(** [oz_line_to_track t line] converts an OriginZero grid line to a GridTrackVec
    index (even indices). Returns [None] if the line is outside the implicit
    grid. The result is doubled to account for interleaved lines and tracks in
    the GridTrackVec representation. *)
