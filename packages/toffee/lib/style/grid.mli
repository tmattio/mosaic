(** Grid coordinate systems and track counting utilities.

    This module provides types and functions for managing CSS Grid layout
    coordinates and track counts. It handles conversion between CSS grid line
    coordinates (1-indexed, negative allowed) and origin-zero coordinates
    (0-indexed) used internally during layout computation.

    {1 Coordinate Systems}

    Two coordinate systems reference grid lines (gaps between rows/columns):

    - {b CSS Grid Line coordinates}: Used in [grid-row]/[grid-column] CSS
      properties. Line 1 is at the left/top edge of the explicit grid. Line -1
      is at the right/bottom edge. Line 0 is invalid and treated as [Auto].

    - {b Origin-zero coordinates}: Normalized internal form where line 0 is at
      the left/top edge of the explicit grid. Lines increment rightward/downward
      (1, 2, 3, ...) and decrement leftward/upward (-1, -2, -3, ...).

    All functions in this module operate on these coordinate systems to support
    grid placement and track resolution. *)

type origin_zero_line = int
(** [origin_zero_line] is a 0-indexed grid line coordinate used internally
    during grid layout computation.

    Origin-zero coordinates normalize CSS grid lines: line 0 is at the left/top
    edge of the explicit grid, positive values extend right/down, and negative
    values extend left/up. *)

(** Grid placement specification in origin-zero coordinates. *)
module Origin_zero_placement : sig
  type t =
    | Auto  (** Auto-placement according to grid auto-flow. *)
    | Line of origin_zero_line  (** Explicit line placement. *)
    | Span of int  (** Span the specified number of tracks. *)
end

type track_counts = {
  negative_implicit : int;
      (** Number of implicit tracks before the explicit grid. *)
  explicit : int;  (** Number of explicit tracks. *)
  positive_implicit : int;
      (** Number of implicit tracks after the explicit grid. *)
}
(** [track_counts] represents the distribution of tracks across the implicit and
    explicit grids in a single axis.

    The implicit grid extends the explicit grid to accommodate items placed
    outside it. Tracks are divided into three regions: negative implicit tracks
    (before the explicit grid), explicit tracks (defined by
    [grid-template-rows]/[grid-template-columns]), and positive implicit tracks
    (after the explicit grid). *)

type grid_line = int
(** [grid_line] is a CSS grid line coordinate (1-indexed, negative allowed).

    Positive values count from the start of the explicit grid (1, 2, 3, ...).
    Negative values count from the end (-1, -2, -3, ...). Line 0 is invalid per
    the CSS Grid specification and is treated as [Auto]. *)

(** {1 Coordinate Conversion} *)

val grid_line_to_origin_zero_line : grid_line -> int -> origin_zero_line
(** [grid_line_to_origin_zero_line line explicit_track_count] converts a CSS
    grid line to an origin-zero line coordinate.

    Positive lines are adjusted by subtracting 1 to convert from 1-indexed to
    0-indexed. Negative lines count backward from the end of the explicit grid.
    Line 0 is invalid and converted to 0.

    @param line The CSS grid line coordinate.
    @param explicit_track_count The number of explicit tracks in the axis. *)

(** {1 Track Count Operations} *)

val make_track_counts :
  negative_implicit:int -> explicit:int -> positive_implicit:int -> track_counts
(** [make_track_counts ~negative_implicit ~explicit ~positive_implicit] creates
    a track count record. *)

val total_track_count : track_counts -> int
(** [total_track_count counts] returns the total number of tracks across
    implicit and explicit grids. *)

val oz_line_to_track : origin_zero_line -> track_counts -> int option
(** [oz_line_to_track line counts] converts an origin-zero line to a track index
    for the track immediately following the line.

    Returns [None] if [line] lies outside the implicit grid bounds or refers to
    the final grid line. Track indices are 0-based relative to the start of the
    implicit grid and include negative implicit tracks. *)

val oz_line_to_next_track : origin_zero_line -> int
(** [oz_line_to_next_track line] returns the index of the track immediately
    following the given origin-zero line.

    Negative lines are clamped to 0. This function is used when resolving grid
    item placement. *)

val oz_line_range_to_track_range :
  origin_zero_line -> origin_zero_line -> int * int
(** [oz_line_range_to_track_range start_line end_line] converts an origin-zero
    line range to a track range (inclusive start, exclusive end).

    The returned pair represents [start_track, end_track] indices suitable for
    iterating over tracks in the range. *)
