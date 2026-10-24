(** CSS position property controlling normal flow participation.

    The position property determines whether a node's position is computed by
    the normal layout flow or positioned relative to its final layout position
    or parent container. *)

type t =
  | Relative
      (** The offset is computed relative to the final position given by the
          layout algorithm. Offsets do not affect the position of any other
          items. *)
  | Absolute
      (** The node is taken out of the normal flow of layout and positioned
          relative to its parent. *)

val default : t
(** [default] returns [Relative]. *)

val to_string : t -> string
(** [to_string position] converts [position] to its CSS string representation.
*)

val equal : t -> t -> bool
(** [equal a b] returns [true] if [a] and [b] represent the same position mode.
*)

val compare : t -> t -> int
(** [compare a b] returns a comparison result for use in ordered containers.

    The ordering is [Relative < Absolute]. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt position] prints [position] to the formatter [fmt]. *)
