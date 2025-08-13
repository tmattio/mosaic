(** A module internal to Incremental. Users should see {!Incremental_intf}.

    Node ids are consecutive integers assigned to nodes as they are created. *)

type t = private int

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
val invariant : t -> unit
val next : unit -> t
val to_string : t -> string
