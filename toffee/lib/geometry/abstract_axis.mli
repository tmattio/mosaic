(** CSS abstract axis types and operations.

    The abstract axis system is defined by the CSS Writing Modes specification
    (<https://www.w3.org/TR/css-writing-modes-3/#abstract-axes>). Abstract axes
    describe layout directions independent of physical screen orientation,
    allowing for different writing modes and text directions.

    Currently, toffee assumes horizontal writing mode (Inline = Horizontal,
    Block = Vertical). Future support for vertical writing modes will affect the
    mapping to absolute axes. *)

(** Abstract axis direction.

    [Inline] represents the axis in the inline dimension (horizontal in
    horizontal writing modes, vertical in vertical writing modes).

    [Block] represents the axis in the block dimension (vertical in horizontal
    writing modes, horizontal in vertical writing modes). *)
type t =
  | Inline  (** The inline axis (horizontal in horizontal writing modes) *)
  | Block  (** The block axis (vertical in horizontal writing modes) *)

val other : t -> t
(** [other axis] returns the perpendicular axis.

    If [axis] is [Inline], returns [Block]. If [axis] is [Block], returns
    [Inline]. *)

val to_absolute_naive : t -> Absolute_axis.t
(** [to_absolute_naive axis] converts to an absolute axis assuming horizontal
    writing mode.

    This conversion assumes [Inline] maps to [Horizontal] and [Block] maps to
    [Vertical]. This assumption holds for all current layouts but will change if
    toffee implements the [writing_mode] CSS property in the future.

    Returns [Absolute_axis.Horizontal] for [Inline] and [Absolute_axis.Vertical]
    for [Block]. *)
