(** Ring buffer for bounded collections *)

type 'a t
(** The type of ring buffers containing elements of type ['a] *)

val create : int -> 'a -> 'a t
(** [create capacity default] creates a new ring buffer with the given capacity.
    The buffer is initially empty but the underlying array is filled with
    [default]. *)

val push : 'a t -> 'a -> unit
(** [push t item] adds an item to the ring buffer. If the buffer is full, the
    oldest item is overwritten. *)

val size : 'a t -> int
(** [size t] returns the current number of items in the buffer *)

val capacity : 'a t -> int
(** [capacity t] returns the maximum capacity of the buffer *)

val is_empty : 'a t -> bool
(** [is_empty t] returns true if the buffer contains no items *)

val is_full : 'a t -> bool
(** [is_full t] returns true if the buffer is at capacity *)

val clear : 'a t -> unit
(** [clear t] removes all items from the buffer *)

val get : 'a t -> int -> 'a
(** [get t idx] returns the item at index [idx] (0-based from oldest). Raises
    [Invalid_argument] if index is out of bounds. *)

val to_list : 'a t -> 'a list
(** [to_list t] returns all items in the buffer as a list, oldest first *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f t] applies function [f] to all items in the buffer, oldest first *)

val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
(** [fold f init t] folds function [f] over all items in the buffer, oldest
    first *)
