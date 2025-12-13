(** Frame dumping helpers.

    These helpers serialize the current {!Screen.t} grid to ANSI files and can
    optionally dump the hit grid used for pointer picking. They are used by the
    Matrix runtime when periodic dumps are configured but remain callable
    directly for bespoke tooling. *)

val on_frame :
  ?dir:string ->
  ?pattern:string ->
  ?hits:bool ->
  every:int ->
  unit ->
  Screen.t ->
  unit
(** [on_frame ?dir ?pattern ?hits ~every ()] builds a callback that dumps every
    [every] frames. The callback may be reused across frames and keeps its own
    index counter. Set [hits] to request the auxiliary files described above.

    @raise Invalid_argument if [every <= 0]. *)

val snapshot : ?dir:string -> ?pattern:string -> ?hits:bool -> Screen.t -> unit
(** [snapshot ?dir ?pattern ?hits screen] writes a single dump immediately. It
    shares the same global index as {!on_frame}-built callbacks, which keeps
    filenames monotonic even when mixed. *)
