(** Grid coordinate system conversions for layout computation.

    This module implements conversions between CSS Grid Line and OriginZero
    coordinate systems used internally by the grid layout algorithm. It provides
    utilities for translating grid line positions, computing track indices, and
    determining implicit track requirements.

    {1 Coordinate Systems}

    Two coordinate systems reference grid lines (gaps between rows/columns):

    - {b CSS Grid Line}: Used in CSS [grid-row]/[grid-column]. Line 1 is at the
      left/top edge of the explicit grid (counting up). Line -1 is at the
      right/bottom edge (counting down). Line 0 is invalid and raises an
      exception.

    - {b OriginZero}: Normalized internal form. Line 0 is at the left/top edge
      of the explicit grid. Positive values extend right/down (1, 2, 3, ...).
      Negative values extend left/up (-1, -2, -3, ...).

    {1 Track Vector Indexing}

    The [GridTrackVec] stores both tracks and grid lines in a flattened array
    where grid lines occupy even indices and tracks occupy odd indices. Track
    counts include negative implicit, explicit, and positive implicit regions.
    Conversion functions map OriginZero lines to track vector indices accounting
    for this structure. *)

open Geometry
open Style

type grid_line = int
(** [grid_line] is a CSS Grid Line coordinate (1-indexed, negative allowed).
    Line 0 is invalid. *)

val grid_line : int -> grid_line
(** [grid_line i] creates a grid line from an integer.

    @raise Failure if [i] is 0. *)

val grid_line_to_int : grid_line -> int
(** [grid_line_to_int gl] returns the underlying integer. *)

type origin_zero_line = int
(** [origin_zero_line] is a normalized grid line coordinate where 0 is the
    left/top edge of the explicit grid. *)

val grid_line_to_origin_zero_line : grid_line -> int -> origin_zero_line
(** [grid_line_to_origin_zero_line gl explicit_track_count] converts a CSS grid
    line to OriginZero coordinates.

    Algorithm:
    - Positive lines: subtract 1 (convert 1-indexed to 0-indexed).
    - Negative lines: add [explicit_track_count + 1] to wrap around.

    @raise Failure if [gl] is 0. *)

val try_origin_zero_line_to_track_vec_index :
  origin_zero_line -> Grid.track_counts -> int option
(** [try_origin_zero_line_to_track_vec_index ozl track_counts] converts an
    OriginZero line to a track vector index.

    Returns [None] if [ozl] lies outside the implicit grid bounds. Used for
    absolutely positioned items per CSS Grid spec: out-of-bounds references are
    treated as [auto] instead of creating new implicit tracks.

    The track vector index is computed as
    [2 * (ozl + track_counts.negative_implicit)] to account for interleaved grid
    lines (even indices) and tracks (odd indices). *)

val origin_zero_line_to_track_vec_index :
  origin_zero_line -> Grid.track_counts -> int
(** [origin_zero_line_to_track_vec_index ozl track_counts] converts an
    OriginZero line to a track vector index.

    @raise Failure if [ozl] is outside the implicit grid bounds. *)

val origin_zero_line_implied_negative_implicit_tracks : origin_zero_line -> int
(** [origin_zero_line_implied_negative_implicit_tracks ozl] computes the minimum
    number of negative implicit tracks required if a grid item starts at [ozl].

    Returns [abs ozl] for negative lines, 0 otherwise. *)

val origin_zero_line_implied_positive_implicit_tracks :
  origin_zero_line -> int -> int
(** [origin_zero_line_implied_positive_implicit_tracks ozl explicit_track_count]
    computes the minimum number of positive implicit tracks required if a grid
    item ends at [ozl].

    Returns [ozl - explicit_track_count] if [ozl] exceeds the explicit grid, 0
    otherwise. *)

val line_span : origin_zero_line Line.t -> int
(** [line_span line] computes the number of tracks between [line.start] and
    [line.end_].

    Returns [max (line.end_ - line.start) 0]. *)

val compare_origin_zero_line : origin_zero_line -> origin_zero_line -> int
(** [compare_origin_zero_line a b] compares two OriginZero lines. *)

val equal_origin_zero_line : origin_zero_line -> origin_zero_line -> bool
(** [equal_origin_zero_line a b] tests equality of two OriginZero lines. *)
