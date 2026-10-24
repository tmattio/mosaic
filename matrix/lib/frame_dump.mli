(** Frame dumping to disk.

    Writes the grid a {!Matrix_screen.t} is drawing to an ANSI file and, when
    asked, its hit grid to a text file beside it. A private module of the Matrix
    runtime, which implements {!Matrix.dump_frame} with it. *)

val snapshot :
  ?dir:string -> ?pattern:string -> ?hits:bool -> Matrix_screen.t -> unit
(** [snapshot screen] writes a single dump immediately. Successive snapshots
    share a global frame index so filenames remain monotonic. *)
