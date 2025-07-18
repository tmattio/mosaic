(** UI Layout Calculation Engine.

    This module provides a pure, stateless function to calculate the layout for
    an element tree. It translates the declarative [Element.t] into a recursive
    [Layout.t] tree, which contains the solved geometry for every element. *)

(** {1 Layout Bounds} *)

(** The Bounds module defines the geometric constraints for a layout
    calculation. *)
module Bounds : sig
  type t
  (** An abstract type representing the rectangular area available for layout.
  *)

  val make : x:int -> y:int -> width:int -> height:int -> t
  (** [make ~x ~y ~width ~height] creates a new bounds object. *)

  val x : t -> int
  (** Accessors for the properties of the bounds. *)

  val y : t -> int
  val width : t -> int
  val height : t -> int
end

(** {1 The Computed Layout Tree} *)

type t
(** An abstract, recursive type representing the computed layout for a single
    element and its children. This is the primary output of the layout engine.
*)

(** {2 Accessors for a Computed Layout} *)

val element : t -> Element.t
(** [element layout] returns the original [Element.t] associated with this
    layout. *)

val x : t -> int
(** [x layout] returns the computed absolute x-coordinate of the element's
    bounding box. *)

val y : t -> int
(** [y layout] returns the computed absolute y-coordinate of the element's
    bounding box. *)

val width : t -> int
(** [width layout] returns the computed final width of the element. *)

val height : t -> int
(** [height layout] returns the computed final height of the element. *)

val geometry : t -> int * int * int * int
(** [geometry layout] returns the full computed geometry as a tuple (x, y,
    width, height). *)

val children : t -> t list
(** [children layout] returns the list of computed layouts for the element's
    direct children. *)

(** {1 Main Calculation Function} *)

val calculate : Bounds.t -> Element.t -> t
(** [calculate bounds element] is the main entry point for the layout engine. It
    is a pure function that recursively computes the layout for the given
    element and all of its descendants within the provided bounds. *)
