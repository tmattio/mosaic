(** A sibling key identifying a node among its brothers. [None] means "unkeyed"
    and participates in index-based reconciliation. *)

type t = string option

val none : t
val of_string : string -> t
val is_some : t -> bool
val is_none : t -> bool
val equal : t -> t -> bool
val compare : t -> t -> int

val to_string : t -> string
(** [None] -> "" for debug output *)

val pp : Format.formatter -> t -> unit
