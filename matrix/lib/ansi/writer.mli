(** Byte writers for escape sequence emission.

    A writer appends bytes to storage and retains it across {!reset_pos} calls.
    Its storage either grows as output requires ({!create}) or is the caller's,
    of a size the caller fixed ({!of_bytes}): the first bounds output by the
    largest string OCaml can hold, the second by the memory the caller chose to
    give it. Writers are mutable and not thread-safe. *)

type t
(** The type for byte writers. *)

exception Buffer_full
(** Raised by a write that does not fit a writer of {!of_bytes}. *)

val create : int -> t
(** [create capacity] is an empty writer with the given initial capacity.
    Storage grows as needed. Raises [Invalid_argument] if [capacity] is negative
    or exceeds [Sys.max_string_length]. *)

val of_bytes : bytes -> t
(** [of_bytes b] is an empty writer whose storage is [b], which it never
    replaces: output goes into [b] from index [0], and a write that does not fit
    in the rest of [b] raises {!Buffer_full} and writes nothing. {!buffer} is
    [b]; [b] is the caller's to read, and to reuse after {!reset_pos}.

    Use it when the memory output may take is the caller's decision: a buffer
    allocated once and reused for every frame, or output that must not exceed
    what the device accepts at once. A write fails whole, but an escape sequence
    is made of several writes, so after {!Buffer_full} the output ends wherever
    the sequence was cut: drop the bytes written since the last point the caller
    chose, by remembering {!len} there, and write that output again into a
    larger buffer, or into this one once emptied. *)

val len : t -> int
(** [len w] is the number of bytes written so far. *)

val reset_pos : t -> unit
(** [reset_pos w] empties [w], retaining its capacity. *)

val contents : t -> string
(** [contents w] is a copy of the bytes written so far. *)

val buffer : t -> bytes
(** [buffer w] is borrowed storage containing output in the first [len w] bytes.
    Do not mutate it or retain it across writes or {!reset_pos}. Use it with a
    synchronous byte consumer to avoid copying the output. *)

val write_char : t -> char -> unit
(** [write_char w c] appends [c]. *)

val write_string : t -> string -> unit
(** [write_string w s] appends [s]. *)

val write_subbytes : t -> bytes -> int -> int -> unit
(** [write_subbytes w b off len] appends the given slice of [b]. Raises
    [Invalid_argument] if the slice is outside [b]. *)

(** Writes to a writer of {!create} raise [Invalid_argument] if the output would
    exceed [Sys.max_string_length]. *)
