(** Threaded I/O writer with buffer swapping.

    In steady state, the threaded writer achieves zero-copy output by swapping
    buffers between the application and worker thread. However, if a frame
    exceeds the spare buffer's capacity, a new buffer is allocated to
    accommodate it. To avoid runtime allocations, ensure the initial [size]
    passed to {!create} is large enough for the largest expected frame. *)

type t

val create : fd:Unix.file_descr -> size:int -> use_thread:bool -> t
(** [create ~fd ~size ~use_thread] creates a frame writer. [size] is the initial
    size of the buffers (should match screen render buffer size). *)

val drain : t -> unit
(** Block until any in-flight write has completed. No-op for direct writers. *)

val last_error : t -> Unix.error option
(** [last_error t] returns the first I/O error encountered by the worker thread
    since the last call to {!clear_error}, or [None] if no error occurred. No-op
    for direct writers (always returns [None]). *)

val clear_error : t -> unit
(** [clear_error t] clears any stored error. No-op for direct writers. *)

val submit : t -> bytes -> int -> bytes
(** [submit t buffer len] submits a frame buffer to be written.

    In Threaded mode:
    - Blocks if the writer is currently busy (Backpressure).
    - Swaps [buffer] with the writer's spare buffer.
    - Wakes up the writer thread.
    - Returns the spare buffer (which the caller reuses for the next frame).

    In Direct mode:
    - Writes immediately to the FD.
    - Returns [buffer] unchanged. *)

val close : t -> unit
(** [close t] stops the thread and flushes pending writes if possible. *)
