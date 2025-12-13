(** CSS Grid layout algorithm implementation.

    Implements CSS Grid Level 1 specification for two-dimensional grid layout.
    This is an internal module implementing the multi-phase grid layout
    algorithm: explicit grid resolution, auto-placement, track sizing, and
    alignment.

    Reference:
    {{:https://www.w3.org/TR/css-grid-1/}CSS Grid Layout Module Level 1}

    {1 Algorithm Phases}

    The grid layout algorithm executes in these phases:

    + {b Available grid space computation}: Resolve padding, border, scrollbar
      gutters, and container size constraints from styles.
    + {b Explicit grid resolution}: Compute track counts from
      grid-template-rows/columns, resolving auto-repeat (auto-fit/auto-fill).
    + {b Track count estimation}: Estimate implicit grid size from child
      placements before auto-placement expands the grid.
    + {b Grid item placement}: Match items to definite grid positions via
      explicit placement and auto-placement, expanding implicit grid as needed.
    + {b Track initialization}: Initialize explicit and implicit tracks with
      sizing functions from grid-template and grid-auto styles.
    + {b Track sizing}: Run the track sizing algorithm (inline axis first, then
      block axis) to resolve track base sizes and growth limits.
    + {b Percentage resolution}: Re-resolve percentage track sizes when
      container size becomes definite.
    + {b Track alignment}: Align tracks within the container using
      justify-content and align-content.
    + {b Item positioning}: Align and position grid items within their grid
      areas, computing baselines for the container.

    {1 Key Invariants}

    - Track sizing runs twice: once for the inline axis (columns), then for the
      block axis (rows). The inline pass determines container width, which may
      affect row sizing.
    - Track sizing may re-run if: (a) percentage tracks exist and container size
      was initially indefinite, or (b) intrinsic track content contributions
      change after the first pass.
    - Items crossing intrinsic or flexible tracks cache their content
      contributions to avoid redundant measurements during re-runs.
    - Auto-fit tracks collapse (size to zero) if they contain no items.
      Auto-fill tracks never collapse.
    - Grid items are sorted back into original source order before final
      positioning to match them with their style nodes.

    {1 Track Sizing Re-runs}

    Column sizing re-runs when:
    - Container width was indefinite and any column has percentage sizing.
    - Any item crossing an intrinsically-sized column has a changed min-content
      contribution.

    Row sizing re-runs (only after column re-run) when:
    - Container height was indefinite and any row has percentage sizing.
    - Any item crossing an intrinsically-sized row has a changed min-content
      contribution.

    Only one re-run per axis is performed; further changes are ignored.

    {1 Baseline Computation}

    Grid container baselines are computed from the first row containing items.
    If any item in that row is baseline-aligned, the first such item's baseline
    is used; otherwise, the first item's baseline (or bottom edge if none) is
    used.

    {1 Integration}

    This module coordinates submodules:
    - {!Explicit_grid}: Track template parsing and auto-repeat resolution
    - {!Implicit_grid}: Track count estimation from item placements
    - {!Placement}: Grid item placement and auto-placement
    - {!Track_sizing}: Track sizing algorithm implementation
    - {!Alignment}: Track and item alignment
    - {!Grid_item}: Item measurement and caching
    - {!Grid_track}: Track representation and utilities *)

val compute_grid_layout :
  (module Tree.LAYOUT_PARTIAL_TREE with type t = 't) ->
  tree:'t ->
  node:Tree.Node_id.t ->
  inputs:Tree.Layout_input.t ->
  Tree.Layout_output.t
(** [compute_grid_layout (module Tree) ~tree ~node ~inputs] computes CSS Grid
    layout for [node].

    Returns early with container size if both dimensions are known and
    [run_mode] is [Compute_size].

    Algorithm:

    + Compute available grid space by resolving padding, border,
      min/max/preferred size, and scrollbar gutters.
    + Resolve explicit grid size via
      {!Explicit_grid.compute_explicit_grid_size_in_axis}, resolving auto-repeat
      counts.
    + Create named line resolver from grid-template-areas and line names.
    + Estimate implicit grid size with
      {!Implicit_grid.compute_grid_size_estimate}.
    + Place grid items with {!Placement.place_grid_items}, expanding implicit
      grid as needed.
    + Initialize tracks with {!Explicit_grid.initialize_grid_tracks}, collapsing
      auto-fit tracks without items.
    + Resolve track indexes and determine which items cross flexible/intrinsic
      tracks.
    + Run {!Track_sizing.track_sizing_algorithm} for inline axis, then block
      axis.
    + Resolve percentage track base sizes if container was indefinite.
    + Re-run track sizing if percentage tracks exist or content contributions
      changed.
    + Align tracks with {!Alignment.align_tracks} per justify-content and
      align-content.
    + Align and position in-flow items, then absolutely positioned items,
      computing baselines.
    + Return layout output with container size, content size, and first
      baseline.

    The container border-box size is constrained by min/max size styles, clamped
    to at least padding + border, and defaults to content size when style size
    is indefinite.

    For indefinite containers, percentage track sizes initially resolve to zero,
    then re-resolve after content size is known. This may trigger column and row
    re-runs.

    Absolutely positioned items resolve grid-row/column-start/end to track
    indexes, using container edges as fallback for indefinite placements.

    Preconditions:
    - [node] has [Display.Grid] style.
    - [tree] provides access to child nodes and their styles.

    Postconditions:
    - All children (in-flow, absolutely positioned, hidden) have layouts
      computed and stored via [Tree.set_unrounded_layout].
    - Returned {!Tree.Layout_output} includes container size, content size
      contribution from children, and first baseline from the first row. *)

(** {1 Submodules} *)

module Implicit_grid = Implicit_grid
(** @inline *)

module Explicit_grid = Explicit_grid
(** @inline *)

module Grid_track_counts = Grid_track_counts
(** @inline *)

module Cell_occupancy = Cell_occupancy
(** @inline *)

module Grid_item = Grid_item
(** @inline *)

module Placement = Placement
(** @inline *)

module Named = Named
(** @inline *)
