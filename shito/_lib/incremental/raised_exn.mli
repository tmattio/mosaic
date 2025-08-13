(** A module internal to Incremental. Users should see {!Incremental_intf}.

    A [Raised_exn.t] is an exception paired with the backtrace that was grabbed at the
    time the exception was raised. *)

type t

(** [create exn] makes a [t] using [exn] and the current backtrace. *)
val create : exn -> t

val reraise : t -> 'a
val reraise_with_message : t -> string -> 'a
