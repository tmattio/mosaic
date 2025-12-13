(** Explicit grid track initialization from CSS styles.

    Implements CSS Grid auto-repeat evaluation and track instantiation. Explicit
    tracks are those defined in grid-template-rows/columns, including auto-fit
    and auto-fill repetitions. This module resolves repetition counts and builds
    the track list with gutters. *)

(** Auto-repeat fit strategy for computing repetition counts.

    Determines whether to maximize fit without overflow (for definite/max sizes)
    or minimize fit with overflow (for min sizes). *)
type auto_repeat_strategy =
  | Max_repetitions_that_do_not_overflow
      (** Use with definite or max size constraints. Returns the largest
          repetition count that does not overflow the container. *)
  | Min_repetitions_that_do_overflow
      (** Use with min size constraints. Returns the smallest repetition count
          that satisfies the minimum. *)

val compute_explicit_grid_size_in_axis :
  style:Style.t ->
  auto_fit_container_size:float option ->
  auto_fit_strategy:auto_repeat_strategy ->
  resolve_calc_value:Style.calc_resolver ->
  axis:Geometry.absolute_axis ->
  int * int
(** [compute_explicit_grid_size_in_axis style auto_fit_container_size
     auto_fit_strategy resolve_calc_value axis] computes explicit grid track
    counts.

    Returns [(num_auto_repetitions, total_explicit_track_count)] where the first
    element is the number of auto-fit/auto-fill repetitions resolved (0 if none)
    and the second is the total number of explicit tracks.

    Algorithm validates the template, then:

    + If no auto-repeat: count tracks from single definitions and fixed-count
      repetitions.
    + If auto-repeat present: compute repetitions based on container size, gap,
      and non-repeating track space.
    + Return (0, 0) if template is invalid (multiple auto-repeats, empty
      repetitions, or auto-repeat with non-fixed-size tracks).

    The repetition count is clamped to at least 1 if the first repetition
    overflows.

    Invariant: At most one auto-repeat definition per axis is valid. *)

val initialize_grid_tracks :
  tracks:Grid_track.t list ref ->
  counts:Grid_track_counts.t ->
  style:Style.t ->
  axis:Geometry.absolute_axis ->
  track_has_items:(int -> bool) ->
  unit
(** [initialize_grid_tracks tracks counts style axis track_has_items] populates
    [tracks] with grid tracks and gutters.

    Clears [tracks] and rebuilds it with:

    + Initial gutter (collapsed).
    + Negative implicit tracks (from grid-auto-rows/columns, cycling if needed).
    + Explicit tracks (from grid-template-rows/columns, expanding repetitions).
    + Positive implicit tracks.
    + Final gutter (collapsed).

    Auto-fit tracks without items are collapsed. Gutters are sized per gap
    style. All tracks alternate with gutters in the output list.

    Precondition: [counts] must match the actual track counts computed for the
    grid. *)
