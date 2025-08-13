(** A module internal to Incremental. Users should see {!Incremental_intf}.

    [Kind.t] is a variant type with one constructor for each kind of node (const, var,
    map, bind, etc.). *)

include module type of struct
  include Types.Kind
end

val invariant : ('a -> unit) -> 'a t -> unit

val name : _ t -> string
val initial_num_children : _ t -> int

(** [slow_get_child t ~index] raises unless [0 <= index < max_num_children t]. It will
    also raise if the [index]'th child is currently undefined (e.g. a bind node with no
    current rhs). *)
val slow_get_child : _ t -> index:int -> Types.Node.Packed.t

val bind_rhs_child_index : int
val freeze_child_index : int
val if_branch_child_index : int
val join_rhs_child_index : int
val iteri_children : _ t -> f:(int -> Types.Node.Packed.t -> unit) -> unit
