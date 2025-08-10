(** Layout snapshot for element geometry observation

    This module provides a way to record and query the final rendered positions
    and dimensions of UI elements that have been tagged with keys. The snapshot
    is populated during rendering and can be queried by the runtime for
    hit-testing, focus management, and other position-aware operations. *)

type rect = {
  x : int;  (** Absolute column position *)
  y : int;  (** Absolute row position *)
  w : int;  (** Width in columns *)
  h : int;  (** Height in rows *)
}
(** Rectangle representing absolute screen coordinates *)

type entry = {
  rect : rect;  (** Absolute bounding box *)
  z_index : int;  (** Stacking order (higher = on top) *)
  clipping : rect option;  (** Clipping rectangle if element is clipped *)
}
(** Entry for a keyed element *)

type t
(** The snapshot type *)

val create : unit -> t
(** Create a new empty snapshot *)

val record : t -> Attr.key -> entry -> unit
(** Record an element's layout information *)

val get : t -> Attr.key -> entry option
(** Get the layout information for a keyed element *)

val clear : t -> unit
(** Clear all recorded entries *)

val iter : t -> (Attr.key -> entry -> unit) -> unit
(** Iterate over all recorded entries *)

val fold : t -> init:'a -> f:('a -> Attr.key -> entry -> 'a) -> 'a
(** Fold over all recorded entries *)

val keys : t -> Attr.key list
(** Get all keys in the snapshot *)

val hit_test : t -> x:int -> y:int -> Attr.key option
(** Find the topmost element at the given position *)

val hit_test_all : t -> x:int -> y:int -> (Attr.key * entry) list
(** Find all elements at the given position, sorted by z-index (topmost first)
*)

val point_in_rect : x:int -> y:int -> rect -> bool
(** Check if a point is within a rectangle *)

val size : t -> int
(** Get the number of entries in the snapshot *)

val set_current : t option -> unit
(** Set the current snapshot for recording during rendering *)

val get_current : unit -> t option
(** Get the current snapshot being recorded *)

val with_recording : t -> (unit -> 'a) -> 'a
(** Execute a function with a snapshot active for recording *)
