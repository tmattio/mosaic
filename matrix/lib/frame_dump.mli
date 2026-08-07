(** Frame dumping to disk.

    Serializes {!Screen.t} grids to ANSI files and optionally dumps the hit
    grid. Internal to the Matrix runtime: reachable through
    {!Matrix.configure_frame_dump} and {!Matrix.dump_frame}, not exported as a
    standalone module. *)

val snapshot : ?dir:string -> ?pattern:string -> ?hits:bool -> Screen.t -> unit
(** [snapshot screen] writes a single dump immediately. Successive snapshots
    share a global frame index so filenames remain monotonic. *)
