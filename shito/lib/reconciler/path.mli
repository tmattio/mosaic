(** A positional path from the root to a node, represented as a list of child
    indices. The head of the list is the root-most index, i.e. [ [i0; i1; …] ].
*)

type t = int list

val root : t
val is_root : t -> bool

val push : int -> t -> t
(** [push idx p] appends [idx] at the end (toward the leaf). *)

val append : t -> t -> t
(** Concatenate two paths. *)

val pp : Format.formatter -> t -> unit
val to_string : t -> string
val compare : t -> t -> int
