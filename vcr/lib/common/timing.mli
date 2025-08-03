(** Generic timing module for performance tracking *)

type t
(** Timing tracker *)

val create : unit -> t
(** Create a new timing tracker *)

val start : t -> string -> unit
(** Start timing a named phase *)

val stop : t -> string -> float
(** Stop timing a named phase and return its duration.
    @raise Failure if the phase was not started *)

val with_timing : t -> string -> (unit -> 'a) -> 'a
(** Time the execution of a function *)

val record : t -> string -> float -> unit
(** Manually record a timing *)

val get_total : t -> float
(** Get total elapsed time since creation *)

val print : ?title:string -> t -> unit
(** Print timing summary to stderr.
    @param title Title for the summary (default: "TIMING SUMMARY") *)
