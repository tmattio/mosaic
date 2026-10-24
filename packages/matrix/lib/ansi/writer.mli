(** Byte writers, the buffers escape sequences are written to.

    A {e writer} appends bytes to its {e storage} and counts them ({!len}).
    {!reset_pos} empties it and keeps the storage, so that one writer serves
    every frame a program draws. Its storage is {e growable} or {e fixed}. A
    growable writer ({!create}) replaces its storage with a larger one when a
    write needs more room, up to [Sys.max_string_length] bytes. A fixed writer
    ({!of_bytes}) writes into bytes the caller gives it and never replaces them,
    so the caller decides how much memory output may take, such as what a device
    accepts in one write.

    A write either appends all its bytes or appends none and raises. An escape
    sequence is made of several writes, so after {!Buffer_full} the output ends
    wherever a sequence was cut. A caller that writes into a fixed writer
    remembers {!len} at each point it may stop at, uses only the bytes up to the
    last such point, and writes the rest again into a larger writer, or into
    this one once it is emptied.

    A writer is mutable and must not be used by two domains in parallel.

    This writes a sequence and reads it back:
    {[
    let w = Ansi.Writer.create 64 in
    Ansi.emit (Ansi.cursor_position ~row:1 ~col:1) w;
    assert (Ansi.Writer.contents w = "\027[1;1H")
    ]} *)

type t
(** The type for writers. *)

exception Buffer_full
(** Raised by a write that does not fit in the rest of a fixed writer's storage.
*)

val create : int -> t
(** [create n] is an empty growable writer whose storage starts at [n] bytes.

    Raises [Invalid_argument] if [n] is negative or greater than
    [Sys.max_string_length]. *)

val of_bytes : bytes -> t
(** [of_bytes b] is an empty fixed writer whose storage is [b]. Output goes into
    [b] from index [0], and {!buffer} is [b]. The caller reads the output in
    [b], and may reuse [b] after {!reset_pos}. *)

val len : t -> int
(** [len w] is the number of bytes written to [w] since it was created or last
    emptied. *)

val reset_pos : t -> unit
(** [reset_pos w] empties [w] and keeps its storage. *)

val contents : t -> string
(** [contents w] is a copy of the bytes written to [w]. *)

val buffer : t -> bytes
(** [buffer w] is [w]'s storage, whose first [len w] bytes are the output. It is
    borrowed, for handing the output to a synchronous consumer without a copy.
    The caller must not modify it, nor keep it across a write or {!reset_pos},
    since a growable writer may replace it. *)

(** {1:writing Writing}

    A write appends its bytes to the output. A write to a fixed writer raises
    {!Buffer_full} if its bytes do not fit in the rest of the storage, and a
    write to a growable writer raises [Invalid_argument] if the output would
    exceed [Sys.max_string_length] bytes. A write that raises writes nothing. *)

val write_char : t -> char -> unit
(** [write_char w c] appends [c] to [w]. *)

val write_string : t -> string -> unit
(** [write_string w s] appends [s] to [w]. *)

val write_subbytes : t -> bytes -> int -> int -> unit
(** [write_subbytes w b off len] appends the [len] bytes of [b] from [off] to
    [w].

    Raises [Invalid_argument] if [off] and [len] do not designate a range of
    [b]. *)
