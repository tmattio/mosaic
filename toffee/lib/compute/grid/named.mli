(** Internal resolver for named grid lines and areas.

    This module resolves CSS Grid line names and area names into concrete line
    numbers during layout computation. It processes grid-template-areas,
    grid-template-columns, and grid-template-rows to build lookup tables mapping
    names to line indices.

    {1 Algorithm}

    Construction proceeds in three phases:

    + Process grid-template-areas to extract area boundaries and generate
      implicit line names (e.g., "header-start", "header-end") for each area.
    + Process grid-template-columns and grid-template-rows, iterating through
      line name sets and tracks. For repeat() tracks, expand repetitions using
      the computed auto-repetition count. Line name sets at repeat boundaries
      collapse with adjacent sets.
    + Sort and deduplicate line indices for each name to handle names appearing
      at multiple lines.

    {1 Invariants}

    - Line indices are 1-based, matching CSS Grid specification.
    - Each line name maps to a sorted, deduplicated list of line indices.
    - Area counts (area_column_count, area_row_count) represent the minimum grid
      size implied by areas.
    - Explicit counts (explicit_column_count, explicit_row_count) are set
      externally after grid sizing and used for fallback line resolution. *)

open Style
open Geometry

type t
(** Resolver mapping grid line names and area names to line numbers. *)

val create : Style.t -> int -> int -> t
(** [create style column_auto_repetitions row_auto_repetitions] initializes a
    resolver from grid style properties.

    Processes grid-template-areas, grid-template-columns, and grid-template-rows
    to build name-to-line mappings. Auto-repetition counts determine how many
    times auto-fill/auto-fit repeat() tracks expand.

    The explicit row/column counts are initialized to 0 and must be set later
    via {!set_explicit_column_count} and {!set_explicit_row_count}. *)

val area_column_count : t -> int
(** [area_column_count t] returns the number of columns implied by grid areas.
*)

val area_row_count : t -> int
(** [area_row_count t] returns the number of rows implied by grid areas. *)

val set_explicit_column_count : t -> int -> unit
(** [set_explicit_column_count t count] sets the explicit column count used for
    fallback line resolution. *)

val set_explicit_row_count : t -> int -> unit
(** [set_explicit_row_count t count] sets the explicit row count used for
    fallback line resolution. *)

val explicit_column_count : t -> int
(** [explicit_column_count t] returns the current explicit column count. *)

val explicit_row_count : t -> int
(** [explicit_row_count t] returns the current explicit row count. *)

val resolve_row_names : t -> Grid.Placement.t Line.t -> Grid.Placement.t Line.t
(** [resolve_row_names t line] resolves named lines in row-axis grid placement
    to line numbers.

    Converts Named_line placements to Line placements using the row line name
    map. Non-named placements pass through unchanged. *)

val resolve_column_names :
  t -> Grid.Placement.t Line.t -> Grid.Placement.t Line.t
(** [resolve_column_names t line] resolves named lines in column-axis grid
    placement to line numbers.

    Converts Named_line placements to Line placements using the column line name
    map. Non-named placements pass through unchanged. *)

val resolve_absolutely_positioned_grid_tracks :
  Grid.Origin_zero_placement.t Line.t -> int option Line.t
(** [resolve_absolutely_positioned_grid_tracks oz_placement] computes the final
    track range for absolutely positioned items.

    Handles all combinations of Line, Span, and Auto placements according to CSS
    Grid positioning rules. When both start and end are lines, ensures start <=
    end. Returns None boundaries for underspecified placements. *)
