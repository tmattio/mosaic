(** UI Layout Calculation Engine.

    This module provides a pure, stateless function to calculate the layout for
    an element tree. It translates the declarative [Element.t] into a recursive
    [Layout.t] tree, which contains the solved geometry for every element. *)

type t
(** An abstract, recursive type representing the computed layout for a single
    element and its children. This is the primary output of the layout engine.
*)

val element : t -> Element.t
(** [element layout] returns the original [Element.t] associated with this
    layout. *)

val calculate : x:int -> y:int -> width:int -> height:int -> Element.t -> t
(** [calculate bounds element] is the main entry point for the layout engine. It
    is a pure function that recursively computes the layout for the given
    element and all of its descendants within the provided bounds. *)

val geometry : t -> int * int * int * int
(** [geometry layout] returns the full computed geometry as a tuple (x, y,
    width, height). *)

val children : t -> t list
(** [children layout] returns the list of computed layouts for the element's
    direct children. *)
