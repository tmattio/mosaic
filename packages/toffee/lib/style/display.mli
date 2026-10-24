(** CSS display property controlling box generation and child layout algorithm.

    The display property determines how an element generates boxes and which
    layout algorithm applies to its children. *)

type t =
  | Block  (** Children follow the block layout algorithm. *)
  | Flex  (** Children follow the flexbox layout algorithm. *)
  | Grid  (** Children follow the CSS grid layout algorithm. *)
  | None  (** Node is hidden and generates no boxes. *)

val default : t
(** [default] returns [Flex]. *)

val to_string : t -> string
(** [to_string display] converts [display] to its CSS string representation. *)

val is_none : t -> bool
(** [is_none display] returns [true] if [display] is [None]. *)

val equal : t -> t -> bool
(** [equal a b] returns [true] if [a] and [b] represent the same display mode.
*)

val compare : t -> t -> int
(** [compare a b] returns a comparison result for use in ordered containers.

    The ordering is [Block < Flex < Grid < None]. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt display] prints [display] to the formatter [fmt]. *)
