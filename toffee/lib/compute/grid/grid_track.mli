(** Internal grid track representation for CSS Grid layout algorithm.

    Grid tracks (rows and columns) and gutters (gaps) are both represented as
    [t] values during track sizing. This module provides the data structure and
    operations used by the track sizing algorithm to compute final track sizes.

    The track sizing algorithm mutates several fields ([base_size],
    [growth_limit], scratch variables) during sizing passes. Tracks maintain
    invariants enforced by the sizing algorithm:

    - [base_size <= growth_limit] (except when [growth_limit = infinity])
    - Collapsed tracks have [is_collapsed = true] and zero-sized functions
    - Gutters between collapsed tracks are also collapsed *)

open Style

type grid_track_kind =
  | Track  (** Actual grid track. *)
  | Gutter  (** Gap between tracks. *)

type t = {
  kind : grid_track_kind;
  is_collapsed : bool;
      (** Collapsed tracks are excluded from sizing. Gutters between collapsed
          tracks are also collapsed. *)
  track_sizing_function : track_sizing_function;
      (** Min and max sizing constraints. *)
  mutable offset : float;  (** Distance from container start (pixels). *)
  mutable base_size : float;  (** Current track size. *)
  mutable growth_limit : float;
      (** Upper bound for track growth. Can be infinity. *)
  mutable content_alignment_adjustment : float;
      (** Scratch value for opposite-axis space estimation during content
          sizing. *)
  mutable item_incurred_increase : float;
      (** Scratch value during space distribution to avoid clobbering planned
          increases. *)
  mutable base_size_planned_increase : float;
      (** Scratch value for planned base_size increase during space
          distribution. *)
  mutable growth_limit_planned_increase : float;
      (** Scratch value for planned growth_limit increase during space
          distribution. *)
  mutable infinitely_growable : bool;
      (** Scratch value indicating track can grow without bound. See
          {{:https://www.w3.org/TR/css3-grid-layout/#infinitely-growable}CSS
           Grid spec}. *)
}
(** Internal sizing state for a single grid track or gutter.

    Mutable fields are updated during the track sizing algorithm's iterative
    passes. The algorithm maintains [base_size <= growth_limit] except when
    [growth_limit = infinity]. *)

val new_with_kind : grid_track_kind -> track_sizing_function -> t
(** [new_with_kind kind sizing_fn] creates a track with the specified kind and
    sizing function. All numeric fields are initialized to zero. *)

val create : track_sizing_function -> t
(** [create sizing_fn] creates an actual track (not a gutter) with the given
    sizing function. *)

val gutter : Compact_length.t -> t
(** [gutter size] creates a gutter track. Both min and max sizing functions are
    set to [size]. *)

val collapse : t -> t
(** [collapse t] returns a collapsed version of [t]. Sets [is_collapsed = true]
    and replaces the sizing function with zero. Used when tracks should be
    excluded from layout. *)

val is_flexible : t -> bool
(** [is_flexible t] returns [true] if the track has a flex ([fr]) max sizing
    function. *)

val uses_percentage : t -> bool
(** [uses_percentage t] returns [true] if either min or max sizing function is
    percentage-based. *)

val has_intrinsic_sizing_function : t -> bool
(** [has_intrinsic_sizing_function t] returns [true] if either min or max sizing
    function is intrinsic ([min-content], [max-content], [auto], [fit-content]).
*)

val fit_content_limit : t -> float option -> float
(** [fit_content_limit t axis_available_grid_space] returns the upper bound for
    [fit-content] tracks. Returns [infinity] for non-fit-content tracks or when
    space is indefinite. *)

val fit_content_limited_growth_limit : t -> float option -> float
(** [fit_content_limited_growth_limit t axis_available_grid_space] returns the
    growth limit clamped by the fit-content limit. Used during track sizing to
    cap growth of fit-content tracks. *)

val flex_factor : t -> float
(** [flex_factor t] returns the flex factor if [t] is a flex track, else [0.0].
*)
