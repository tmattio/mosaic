(** Container that holds an item in each absolute axis.

    This module provides a simple two-field record for storing values that vary
    by absolute axis (horizontal and vertical). It is primarily used in CSS Grid
    layout for properties like track counts, gaps, and placements that differ by
    axis. *)

type 'a t = { horizontal : 'a; vertical : 'a }
(** The type of a container holding a value for each absolute axis. *)

val make : horizontal:'a -> vertical:'a -> 'a t
(** [make ~horizontal ~vertical] creates a container. *)

val get : 'a t -> Absolute_axis.t -> 'a
(** [get t axis] returns the value for the specified axis. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f t] applies [f] to both values. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f t1 t2] applies [f] to corresponding axis values. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal eq t1 t2] tests equality using [eq] on both axes. *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** [compare cmp t1 t2] compares using [cmp] with lexicographic ordering.

    Compares horizontal values first; if equal, compares vertical values. *)

val to_string : ('a -> string) -> 'a t -> string
(** [to_string f t] converts to a string using [f] for values. *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** [pp f fmt t] pretty-prints [t] using [f] for values. *)
