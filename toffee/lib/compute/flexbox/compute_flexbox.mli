(** Flexbox layout computation algorithm.

    Implements the CSS Flexbox specification
    (https://www.w3.org/TR/css-flexbox-1/) following the W3C algorithm
    structure. This is a direct port of taffy's flexbox implementation.

    {1 Algorithm Overview}

    The flexbox algorithm proceeds in stages following the W3C spec numbering:

    + Initial Setup: Generate flex items from children, compute constants from
      container style.
    + Line Length Determination: Determine available space, compute flex base
      sizes and hypothetical sizes, determine container main size.
    + Main Size Determination: Collect items into lines, resolve flexible
      lengths.
    + Cross Size Determination: Determine hypothetical cross sizes, calculate
      baselines, compute line cross sizes, handle align-content stretch,
      determine used cross sizes.
    + Main-Axis Alignment: Distribute remaining free space using
      justify-content.
    + Cross-Axis Alignment: Resolve cross-axis auto margins, determine container
      cross size, align lines, align items within lines.
    + Final Positioning: Resolve insets, compute absolute positions, accumulate
      scroll offsets, handle hidden items.

    {1 Key Invariants}

    - [flex_item.frozen] determines whether an item's size is locked during
      flexible length resolution.
    - [flex_item.violation] tracks deviation from target size; items are frozen
      when violations resolve.
    - [flex_line.items] is a slice into the main [flex_items] array, avoiding
      allocations.
    - [algo_constants] caches resolved style properties and container sizes
      updated during layout.
    - Main axis direction (row/column) determines which dimension (width/height)
      is used for main/cross sizing.

    {1 Performance Notes}

    - Uses array-based iteration over flex items and lines for cache locality.
    - Mutable fields in [flex_item], [flex_line], and [algo_constants] avoid
      allocations during iterative sizing.
    - Percentage gaps are re-resolved after container main size is determined.
    - Short-circuits in [compute_flexbox_layout] when size is fully determined
      and run mode is ComputeSize. *)

open Tree

val compute_flexbox_layout :
  (module LAYOUT_PARTIAL_TREE with type t = 't) ->
  't ->
  Node_id.t ->
  Layout_input.t ->
  Layout_output.t
(** [compute_flexbox_layout tree node inputs] computes flexbox layout for
    [node].

    Entry point for flexbox layout. Resolves size constraints, applies
    box-sizing adjustments, and delegates to internal algorithm stages.
    Short-circuits when container size is fully determined and
    [inputs.run_mode = ComputeSize]. *)
