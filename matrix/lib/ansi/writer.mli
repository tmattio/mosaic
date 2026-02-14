(** Low-level buffer writer for escape sequence emission. *)

type t
(** A mutable buffer writer. Wraps a [bytes] buffer and manages a write
    position. Not thread-safe. *)

(** {1 Construction} *)

val make : bytes -> t
(** [make buf] creates a writer targeting [buf]. The buffer must be large enough
    to contain all generated output. *)

val make_counting : unit -> t
(** [make_counting ()] creates a counting-mode writer that tracks output length
    without writing any bytes. *)

(** {1 Inspection} *)

val len : t -> int
(** [len w] returns the number of bytes written so far. *)

val pos : t -> int
(** [pos w] returns the current write position (same as {!len}). *)

val reset_pos : t -> unit
(** [reset_pos w] resets the write position to zero. The underlying buffer is
    not cleared. *)

val slice : t -> bytes
(** [slice w] returns a fresh copy of the bytes written so far. *)

(** {1 Writing} *)

val write_char : t -> char -> unit
(** [write_char w c] appends character [c] to the buffer. In counting mode,
    increments the position without writing. *)

val write_string : t -> string -> unit
(** [write_string w s] appends string [s] to the buffer. *)

val write_subbytes : t -> bytes -> int -> int -> unit
(** [write_subbytes w buf off len] appends [len] bytes from [buf] starting at
    offset [off]. *)
