(** A module internal to Incremental. Users should see {!Incremental_intf}.

    An [At_intervals.t] is a kind of DAG node that changes at a regular time interval. *)

include module type of struct
  include Types.At_intervals
end

val invariant : t -> unit
