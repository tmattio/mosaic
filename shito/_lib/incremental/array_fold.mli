(** A module internal to Incremental. Users should see {!Incremental_intf}.

    An [Array_fold.t] is a kind of DAG node. It is an immutable value that holds the
    children of type ['a] and can [compute] the fold to produce a value of type ['b]. *)

include module type of struct
  include Types.Array_fold
end

val invariant : ('a -> unit) -> ('b -> unit) -> ('a, 'b) t -> unit

val compute : (_, 'b) t -> 'b
