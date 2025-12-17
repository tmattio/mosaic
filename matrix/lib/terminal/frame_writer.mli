(** Threaded I/O writer with double buffering.

    The writer owns the render buffers internally. In threaded mode, it uses
    double buffering - one buffer for rendering, one being written - and swaps
    them on submit. In direct mode, a single buffer is used with synchronous
    writes.

    To avoid runtime allocations, ensure the initial [size] passed to {!create}
    is large enough for the largest expected frame. *)

type t

val create : fd:Unix.file_descr -> size:int -> use_thread:bool -> t
(** [create ~fd ~size ~use_thread] creates a frame writer with internal buffers
    of the given [size]. *)

val render_buffer : t -> bytes
(** [render_buffer t] returns the buffer to render into. This buffer remains
    valid until the next call to {!present}. *)

val present : t -> int -> unit
(** [present t len] presents the first [len] bytes of the render buffer to
    the output.

    In Threaded mode:
    - Blocks if the writer is currently busy (backpressure).
    - Swaps the render buffer with the write buffer internally.
    - Wakes up the writer thread.
    - The next call to {!render_buffer} returns the new (swapped) buffer.

    In Direct mode:
    - Writes immediately to the FD.
    - The render buffer remains unchanged. *)

val drain : t -> unit
(** [drain t] blocks until any in-flight write has completed.
    No-op for direct writers. *)

val submit_string : t -> string -> unit
(** [submit_string t s] writes a string through the frame writer. This drains
    any pending writes first to ensure proper ordering, then writes the string.
    Use this for control sequences (sync, cursor positioning, etc.) that must
    be serialized with frame data to prevent output interleaving. *)

val close : t -> unit
(** [close t] stops the thread and flushes pending writes if possible. *)
