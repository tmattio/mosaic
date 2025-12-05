(** CSS flex-direction property for flexbox layout.

    The flex-direction property establishes the main axis of a flex container,
    determining the direction in which flex items are laid out. The main axis
    defines the primary direction of item flow, while the cross axis runs
    perpendicular to it.

    Items are justified along the main axis and aligned along the cross axis.

    Reference:
    {{:https://www.w3.org/TR/css-flexbox-1/#flex-direction-property}CSS Flexbox
     Level 1} *)

type t =
  | Row
      (** Main axis is horizontal, left to right. Defines +x as the main axis.
      *)
  | Column
      (** Main axis is vertical, top to bottom. Defines +y as the main axis. *)
  | Row_reverse
      (** Main axis is horizontal, right to left. Defines -x as the main axis.
      *)
  | Column_reverse
      (** Main axis is vertical, bottom to top. Defines -y as the main axis. *)

val default : t
(** [default] returns [Row]. *)

val is_row : t -> bool
(** [is_row t] returns [true] if [t] is [Row] or [Row_reverse]. *)

val is_column : t -> bool
(** [is_column t] returns [true] if [t] is [Column] or [Column_reverse]. *)

val is_reverse : t -> bool
(** [is_reverse t] returns [true] if [t] is [Row_reverse] or [Column_reverse].
*)

val main_axis : t -> Geometry.Absolute_axis.t
(** [main_axis t] returns the absolute axis corresponding to the main axis.

    - [Row] and [Row_reverse] return [Horizontal]
    - [Column] and [Column_reverse] return [Vertical] *)

val cross_axis : t -> Geometry.Absolute_axis.t
(** [cross_axis t] returns the absolute axis corresponding to the cross axis.

    The cross axis is always perpendicular to the main axis.

    - [Row] and [Row_reverse] return [Vertical]
    - [Column] and [Column_reverse] return [Horizontal] *)

val to_string : t -> string
(** [to_string t] returns the CSS string representation of [t].

    - [Row] returns ["row"]
    - [Column] returns ["column"]
    - [Row_reverse] returns ["row-reverse"]
    - [Column_reverse] returns ["column-reverse"] *)

val equal : t -> t -> bool
(** [equal a b] returns [true] if [a] and [b] are the same direction. *)

val compare : t -> t -> int
(** [compare a b] provides a total ordering over flex directions. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] pretty-prints [t] to [fmt] using its CSS string representation.
*)
