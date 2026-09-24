(** Growable byte writers for escape sequence emission.

    Writers own their storage and retain capacity across {!reset_pos} calls.
    They are mutable and not thread-safe. *)

type t
(** The type for byte writers. *)

val create : int -> t
(** [create capacity] is an empty writer with the given initial capacity.
    Storage grows as needed. Raises [Invalid_argument] if [capacity] is negative
    or exceeds [Sys.max_string_length]. *)

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

(** Writes raise [Invalid_argument] if the output would exceed
    [Sys.max_string_length]. *)
