(** The storage of the graphemes a packed cell cannot hold, which the grids made
    with [Matrix_grid.create_like] share.

    A store holds UTF-8 {e payloads} in {e slots}, numbered from 1. A slot has a
    {e generation}, which changes each time the slot is freed and reused, and
    the pair [(idx, gen)] of a slot and its generation is a {e handle}. A handle
    is {e stale} once its slot is freed, and reads nothing. A generation has
    {!generation_bits} bits, so a stale handle names its slot again after 128
    reuses, and a caller that counts its handles never keeps a stale one. A slot
    is {e live} while its reference count is positive, and {!intern} shares live
    payloads.

    A caller that stores a handle in cells counts it with {!incref} and
    {!decref}, which must balance. A slot whose count falls to [0] is freed.

    A store is mutable and must not be used by two domains in parallel, except
    {!defer_release}, which a finaliser may call from any thread.

    A store holds at most [2{^ 36} - 1] slots, the most a packed cell can name
    ({!index_bits}). The store's own arrays take 64 bytes a slot, 4 TiB for that
    many, so memory runs out first on every machine with less. An operation that
    allocates a slot when all are taken raises [Failure]. *)

type t
(** The type for grapheme stores. *)

val index_bits : int
(** [index_bits] is [36], the number of bits of a slot number. *)

val generation_bits : int
(** [generation_bits] is [7], the number of bits of a generation. *)

val create : unit -> t
(** [create ()] is a new empty grapheme store. *)

val defer_release : t -> (unit -> unit) -> unit
(** [defer_release t release] queues [release], a finaliser's release of
    references, to run at the start of the next {!intern}, {!copy},
    {!live_count} or {!slot_count}. Releasing then, and never while a caller
    holds a handle it has not counted, keeps that handle from going stale. *)

val intern : t -> string -> off:int -> len:int -> int
(** [intern t s ~off ~len] is the slot of the [len] bytes of [s] from [off]. It
    is the slot of an equal live payload if [t] has one, and a new slot with
    reference count [0] otherwise. {!generation} completes it into a handle.
    [off] and [len] must designate a substring of [s], which is not checked.

    Raises [Failure] if it allocates a slot when all are taken. *)

val live_count : t -> int
(** [live_count t] is the number of live slots, those {!intern} shares. *)

val slot_count : t -> int
(** [slot_count t] is the number of slots ever allocated, those freed for reuse
    included. *)

val valid : t -> idx:int -> gen:int -> bool
(** [valid t ~idx ~gen] is [true] iff [(idx, gen)] is a handle of [t] that is
    not stale. *)

val generation : t -> int -> int
(** [generation t idx] is the generation of slot [idx]. [idx] must be a slot of
    [t], which is not checked. *)

val incref : t -> idx:int -> gen:int -> unit
(** [incref t ~idx ~gen] adds one to the reference count of [(idx, gen)], and
    does nothing if the handle is stale. A count that rises to [1] makes the
    slot live. *)

val decref : t -> idx:int -> gen:int -> unit
(** [decref t ~idx ~gen] takes one from the reference count of [(idx, gen)], and
    does nothing if the handle is stale. A count that falls to [0] frees the
    slot for reuse. *)

val length : t -> idx:int -> gen:int -> int
(** [length t ~idx ~gen] is the length in bytes of the payload of [(idx, gen)],
    and [0] if the handle is stale. *)

val blit : t -> idx:int -> gen:int -> bytes -> pos:int -> int
(** [blit t ~idx ~gen dst ~pos] writes the payload of [(idx, gen)] into [dst]
    from [pos] and is the number of bytes written. It writes nothing and is [0]
    if the handle is stale or the payload does not fit in [dst].

    Raises [Invalid_argument] if [pos < 0] or [pos > Bytes.length dst]. *)

val to_string : t -> idx:int -> gen:int -> string
(** [to_string t ~idx ~gen] is the payload of [(idx, gen)], and [""] if the
    handle is stale. *)

val copy : src:t -> idx:int -> gen:int -> dst:t -> int option
(** [copy ~src ~idx ~gen ~dst] is [Some i], where [i] is a new slot of [dst]
    with reference count [0] that holds the payload of [(idx, gen)] in [src],
    and [None] if the handle is stale. The caller completes [i] into a handle
    with {!generation} and counts it with {!incref}.

    Raises [Failure] if it allocates a slot when all are taken. *)

val equal : t -> idx1:int -> gen1:int -> t -> idx2:int -> gen2:int -> bool
(** [equal s1 ~idx1 ~gen1 s2 ~idx2 ~gen2] is [true] iff neither handle is stale
    and their payloads are equal. It allocates nothing. *)
