(** A timing-wheel alarm used to implement a time-dependent incremental: [at],
    [at_intervals], [snapshot], [step_function]. *)

type t = Types.Alarm.t

val invariant : t -> unit
val null : t
val get_null : unit -> t
