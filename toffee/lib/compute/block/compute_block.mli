(** CSS Block layout algorithm implementation.

    Implements the CSS block layout algorithm for containers with block-level
    children. This module computes layout for nodes using display:block,
    handling margin collapsing, auto-sizing, and positioning of both in-flow and
    absolutely positioned children.

    {1 Algorithm Overview}

    Block layout proceeds in phases:

    + Generate item list from children, resolving styles and filtering
      display:none nodes
    + Determine container width from known dimensions or intrinsic content width
    + Perform final layout on in-flow children with margin collapsing
    + Layout absolutely positioned children in the containing block
    + Compute final content size and margin collapse state

    {1 Key Invariants}

    - Vertical margins collapse between adjacent in-flow block boxes under
      specific conditions (no padding/border separation, overflow:visible,
      position:relative)
    - Absolutely positioned children are removed from normal flow and sized
      relative to the containing block
    - Container width stretches children by default unless they are tables or
      have explicit width
    - Auto margins resolve to 0 for vertical margins of in-flow blocks, but
      expand to fill space for horizontal margins
    - Margin collapsing can occur through empty blocks with no padding, border,
      or height

    {1 Implementation Details}

    The algorithm handles margin collapsing via [Collapsible_margin_set]
    tracking. In-flow layout maintains two margin sets:
    [first_child_top_margin_set] for collapsing with container's top margin, and
    [active_collapsible_margin_set] for tracking bottom margins. Nodes can be
    "collapsed through" when they have no content, padding, border, or height
    constraints.

    Absolutely positioned children use static position as fallback when inset
    properties are auto. Static position is computed during in-flow layout pass
    even though the child is not placed in normal flow.

    Scrollbar gutters are reserved in the opposite axis (vertical scroll
    reserves horizontal space). Tables receive special treatment and do not
    participate in stretch-sizing. *)

open Tree

val compute_block_layout :
  (module LAYOUT_PARTIAL_TREE with type t = 't) ->
  't ->
  Node_id.t ->
  Layout_input.t ->
  Layout_output.t
(** [compute_block_layout tree_module tree node_id inputs] computes block layout
    for [node_id].

    Implements CSS block layout with margin collapsing, auto-sizing, and
    absolute positioning. Returns [Layout_output] containing final size, content
    size, and margin collapse state.

    Algorithm:

    + Resolve size constraints from style properties and parent size
    + Short-circuit if size fully determined and mode is Compute_size
    + Generate block items from children, filtering display:none
    + Determine container width (known dimension or intrinsic content-based)
    + Layout in-flow children with margin collapsing and auto-margins
    + Layout absolutely positioned children in containing block area
    + Compute whether node can be collapsed through
    + Return final size, content size, and margin sets

    The implementation tracks margin collapsing state through
    [Collapsible_margin_set]. Vertical margins of adjacent in-flow blocks
    collapse when separated only by collapsible space. Auto horizontal margins
    expand to fill available space. Absolute children use static position when
    inset is auto. *)
