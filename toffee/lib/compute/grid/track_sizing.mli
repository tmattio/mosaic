(** Grid track sizing algorithm implementation.

    Implements the CSS Grid track sizing algorithm per the CSS Grid Layout
    specification. https://www.w3.org/TR/css-grid-1/#layout-algorithm

    This is an internal module used by the grid layout algorithm. The track
    sizing algorithm determines final track sizes based on sizing functions,
    available space, and item contributions.

    {1 Algorithm Overview}

    The track sizing algorithm runs in phases:

    + Initialize track sizes based on fixed sizing functions
    + Resolve intrinsic track sizes by measuring items and distributing space
    + Maximize tracks by distributing remaining free space to growth limits
    + Expand flexible tracks using the computed flex fraction
    + Stretch auto tracks to fill remaining space

    Items are processed in batches ordered by span and flex track crossing.
    Space distribution respects track limits and uses planned increases to avoid
    clobbering state during iteration.

    {1 Key Invariants}

    - Tracks maintain [base_size <= growth_limit] except during intermediate
      states
    - Items crossing flex tracks are processed after non-flex-crossing items
    - Fit-content tracks clamp growth to their specified limit
    - Percentage tracks resolve to min-content when container size is indefinite
*)

open Geometry
open Style
open Tree

(** Whether space distribution is for minimum or maximum contributions.

    Controls behavior when distributing beyond track limits:
    - [Minimum] targets tracks with intrinsic max sizing functions
    - [Maximum] targets tracks with max-content min/max sizing functions *)
type intrinsic_contribution_type = Minimum | Maximum

val resolve_item_track_indexes :
  Grid_item.t array ->
  Style.Grid.track_counts ->
  Style.Grid.track_counts ->
  unit
(** [resolve_item_track_indexes items column_counts row_counts] converts
    origin-zero line coordinates to track vector indices.

    Negative implicit tracks are stored first, followed by explicit tracks, then
    positive implicit tracks. Each line index maps to an even position (gutters
    occupy odd indices). *)

val determine_if_item_crosses_flexible_or_intrinsic_tracks :
  Grid_item.t array -> Grid_track.t array -> Grid_track.t array -> unit
(** [determine_if_item_crosses_flexible_or_intrinsic_tracks items columns rows]
    sets each item's flags for whether it crosses flexible or intrinsic tracks.

    These flags are used to batch items during the track sizing algorithm. *)

val track_sizing_algorithm :
  (module LAYOUT_PARTIAL_TREE with type t = 't) ->
  't ->
  abstract_axis ->
  float option ->
  float option ->
  align_content ->
  align_content ->
  Available_space.t size ->
  float option size ->
  Grid_track.t array ->
  Grid_track.t array ->
  Grid_item.t array ->
  (Grid_track.t -> float option -> 't -> float option) ->
  bool ->
  unit
(** [track_sizing_algorithm tree axis axis_min_size axis_max_size axis_alignment
     other_axis_alignment available_grid_space inner_node_size axis_tracks
     other_axis_tracks items get_track_size_estimate has_baseline_aligned_item]
    executes the complete track sizing algorithm for one axis.

    Mutates [axis_tracks] by setting [base_size] to final track sizes. Requires
    items to have resolved track indexes and flex/intrinsic crossing flags.

    The algorithm skips intrinsic resolution if all tracks have identical base
    size and growth limit (i.e., all tracks use the same fixed sizing function).

    Baseline alignment requires prior computation of item baselines via layout
    measurements.

    @raise Invalid_argument
      if track indexes are out of bounds or items have unresolved placement. *)
