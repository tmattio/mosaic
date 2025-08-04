(** Compute helpers - Helper functions used by layout algorithms *)

val compute_content_size_contribution :
  location:float Geometry.Point.t ->
  size:float Geometry.Size.t ->
  content_size:float Geometry.Size.t ->
  overflow:Style.Overflow.t Geometry.Point.t ->
  float Geometry.Size.t
(** Compute how much width/height a child contributes to its parent's intrinsic
    content size.

    @param location The position of the child relative to its parent
    @param size The border-box size of the child
    @param content_size
      The content size of the child (may overflow the border-box)
    @param overflow The overflow settings for the child (x and y axes)
    @return The size contribution to the parent's content size

    This function respects overflow settings - only visible overflow contributes
    to the parent's content size. For non-visible overflow modes (clip, hidden,
    scroll), only the border-box size is considered. *)

val apply_alignment_fallback :
  free_space:float ->
  num_items:int ->
  alignment_mode:Style.Align_content.t ->
  is_safe:bool ->
  Style.Align_content.t
(** Implement fallback alignment.

    In addition to the spec at {{:https://www.w3.org/TR/css-align-3/} CSS Align}
    this implementation follows the resolution of
    {{:https://github.com/w3c/csswg-drafts/issues/10154} Issue 10154}.

    @param free_space The amount of free space available
    @param num_items The number of items being aligned
    @param alignment_mode The requested alignment mode
    @param is_safe Whether safe alignment is requested
    @return The alignment mode to use after applying fallback rules

    Fallback occurs in two cases: 1. If there is only a single item being
    aligned and alignment is a distributed alignment keyword
    ({{:https://www.w3.org/TR/css-align-3/#distribution-values} distribution
      values}) 2. If free space is negative and "safe" alignment variants are
    requested, all fallback to Start alignment *)

val compute_alignment_offset :
  free_space:float ->
  num_items:int ->
  gap:float ->
  alignment_mode:Style.Align_content.t ->
  layout_is_flex_reversed:bool ->
  is_first:bool ->
  float
(** Generic alignment function that is used:
    - For both align-content and justify-content alignment
    - For both the Flexbox and CSS Grid algorithms

    @param free_space The amount of free space to distribute
    @param num_items The number of items being aligned
    @param gap The gap between items (should be 0.0 for CSS Grid)
    @param alignment_mode The alignment mode to apply
    @param layout_is_flex_reversed Whether the flex layout is reversed
    @param is_first Whether this is the first item
    @return The offset to apply for alignment

    CSS Grid does not apply gaps as part of alignment, so the gap parameter
    should always be set to zero for CSS Grid. *)
