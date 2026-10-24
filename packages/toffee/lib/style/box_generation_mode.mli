(** CSS box generation mode.

    This module represents an abstracted version of the CSS [display] property
    where any value other than [none] is represented by [Normal]. It determines
    whether a node and its descendants should generate boxes in the layout tree.

    See
    {{:https://www.w3.org/TR/css-display-3/#box-generation}CSS Display Module
     Level 3 - Box Generation}. *)

type t =
  | Normal  (** The node generates a box in the regular way. *)
  | None
      (** The node and its descendants generate no boxes. They are hidden and do
          not participate in layout. *)

(** {1 Constants} *)

val default : t
(** [default] returns [Normal]. *)

(** {1 Utilities} *)

val equal : t -> t -> bool
(** [equal a b] returns [true] if [a] and [b] are the same box generation mode.
*)

val compare : t -> t -> int
(** [compare a b] compares two box generation modes. Returns [0] if equal, a
    negative integer if [a < b], or a positive integer if [a > b]. *)

val to_string : t -> string
(** [to_string mode] converts a box generation mode to its string
    representation. Returns ["normal"] or ["none"]. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt mode] prints a box generation mode to a formatter. *)
