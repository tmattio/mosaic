(** Layout computation result caching.

    This module provides memoization for layout computations. During layout,
    parent nodes often query child sizes multiple times under different
    constraints. The cache prevents redundant computation and ensures earlier
    results are not overwritten by later ones.

    {1 Cache Structure}

    The cache maintains two entry types:

    - {b Final layout entry}: Stores complete layout output including positions
      and baselines, computed during [Perform_layout] mode
    - {b Measure entries}: Nine slots for preliminary size measurements computed
      during [Compute_size] mode

    {1 Slot Allocation Strategy}

    Measure cache slots are determined by two factors:

    - How many dimensions are known: width, height, both, or neither
    - For unknown dimensions: whether the available space constraint is
      min-content or max-content

    Definite available space shares a cache slot with max-content because nodes
    are typically sized under one or the other, not both.

    Slot assignments:

    - Slot 0: Both width and height known
    - Slots 1-2: Width known, height unknown
    - Slot 1: Height max-content or definite
    - Slot 2: Height min-content
    - Slots 3-4: Height known, width unknown
    - Slot 3: Width max-content or definite
    - Slot 4: Width min-content
    - Slots 5-8: Neither dimension known
    - Slot 5: Both axes max-content or definite
    - Slot 6: Width max-content or definite, height min-content
    - Slot 7: Width min-content, height max-content or definite
    - Slot 8: Both axes min-content

    {1 Cache Matching}

    A cache entry matches a query when:

    - Known dimensions match the cached entry's known dimensions, OR known
      dimensions match the cached result size
    - For unknown dimensions, available space constraints are roughly equal
      (within 0.0001 for definite values)

    Hidden layouts ([Perform_hidden_layout] mode) are never cached. *)

val cache_size : int
(** [cache_size] is the number of measure cache slots (9). *)

type 'a cache_entry = {
  known_dimensions : float option Geometry.size;
  available_space : Available_space.t Geometry.size;
  content : 'a;
}
(** Cached layout result paired with its input constraints.

    Fields [known_dimensions] and [available_space] record the sizing
    constraints that produced [content]. *)

type t = {
  mutable final_layout_entry : Layout_output.t cache_entry option;
  mutable measure_entries : float Geometry.size cache_entry option array;
  mutable is_empty : bool;
}
(** Cache for layout computation results.

    Field [final_layout_entry] stores complete layout output with positions and
    baselines from [Perform_layout] mode. Field [measure_entries] stores
    preliminary size measurements from [Compute_size] mode. Field [is_empty]
    tracks whether any entries are populated. *)

val make : unit -> t
(** [make ()] creates an empty cache with no entries. *)

(** Result of a clear operation. *)
type clear_state =
  | Cleared  (** Some entries were removed. *)
  | Already_empty  (** Cache was already empty. *)

val compute_cache_slot :
  float option Geometry.size -> Available_space.t Geometry.size -> int
(** [compute_cache_slot known_dimensions available_space] returns the measure
    cache slot index for the given constraints.

    The slot is determined by which dimensions are known and the available space
    constraints for unknown dimensions.

    @return Value in range 0-8 corresponding to slot allocation strategy. *)

val is_roughly_equal : Available_space.t -> Available_space.t -> bool
(** [is_roughly_equal av1 av2] tests approximate equality of available space
    values.

    Definite values are equal when they differ by less than 0.0001. Min-content
    and max-content values are equal only to themselves. *)

val get :
  t ->
  known_dimensions:float option Geometry.size ->
  available_space:Available_space.t Geometry.size ->
  run_mode:Run_mode.t ->
  Layout_output.t option
(** [get t ~known_dimensions ~available_space ~run_mode] retrieves a cached
    layout result matching the given constraints.

    For [Perform_layout], searches the final layout entry. For [Compute_size],
    searches all measure entries. For [Perform_hidden_layout], always returns
    [None].

    A cache entry matches when known dimensions match either the cached known
    dimensions or the cached result size, and unknown dimensions have roughly
    equal available space constraints.

    @return [Some layout_output] if a matching entry exists, [None] otherwise.
*)

val store :
  t ->
  known_dimensions:float option Geometry.size ->
  available_space:Available_space.t Geometry.size ->
  run_mode:Run_mode.t ->
  Layout_output.t ->
  unit
(** [store t ~known_dimensions ~available_space ~run_mode layout_output] stores
    a layout result in the cache.

    For [Perform_layout], stores complete [layout_output] in the final layout
    entry, replacing any previous entry. For [Compute_size], stores only the
    size component in the measure entry slot determined by
    {!compute_cache_slot}. For [Perform_hidden_layout], performs no caching. *)

val clear : t -> clear_state
(** [clear t] removes all cached entries and reports the operation outcome.

    Resets both final layout entry and all measure entries to [None].

    @return [Cleared] if entries were present, [Already_empty] otherwise. *)

val is_empty : t -> bool
(** [is_empty t] tests whether the cache contains no entries.

    @return
      [true] if both final layout entry and all measure entries are [None],
      [false] otherwise. *)
