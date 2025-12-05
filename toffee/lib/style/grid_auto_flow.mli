(** CSS grid-auto-flow property.

    Controls whether grid items are placed row-wise or column-wise, and whether
    the sparse or dense packing algorithm is used.

    The dense packing algorithm attempts to fill in holes earlier in the grid if
    smaller items come up later. This may cause items to appear out-of-order
    when doing so would fill in holes left by larger items.

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-flow}MDN:
     grid-auto-flow}. *)

type t =
  | Row
      (** Items are placed by filling each row in turn, adding new rows as
          necessary. *)
  | Column
      (** Items are placed by filling each column in turn, adding new columns as
          necessary. *)
  | Row_dense
      (** Items are placed by filling rows first, using the dense packing
          algorithm. *)
  | Column_dense
      (** Items are placed by filling columns first, using the dense packing
          algorithm. *)

val default : t
(** [default] returns [Row]. *)

val to_string : t -> string
(** [to_string flow] converts [flow] to its CSS string representation.

    Returns ["row"], ["column"], ["row dense"], or ["column dense"]. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality of grid auto flow values. *)

val compare : t -> t -> int
(** [compare a b] provides total ordering of grid auto flow values.

    The ordering is [Row] < [Column] < [Row_dense] < [Column_dense]. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt flow] prints [flow] to [fmt] using its CSS string representation. *)

val is_dense : t -> bool
(** [is_dense flow] returns [true] if [flow] uses the dense packing algorithm.

    Returns [true] for [Row_dense] and [Column_dense], [false] otherwise. *)

val primary_axis : t -> Geometry.Abstract_axis.t
(** [primary_axis flow] returns the axis along which items are placed first.

    Returns [Inline] for row-based flows ([Row] and [Row_dense]), and [Block]
    for column-based flows ([Column] and [Column_dense]). *)
