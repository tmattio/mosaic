(** Absolute horizontal and vertical axis representation.

    This module provides a simple representation of the two absolute geometric
    axes in 2D space: horizontal (x-axis) and vertical (y-axis). Unlike abstract
    axes which depend on writing mode, absolute axes have fixed orientation.

    Absolute axes are used throughout the layout engine to index into geometric
    primitives like {!Size}, {!Rect}, and {!Point} in a direction-agnostic way.
*)

type t =
  | Horizontal  (** The horizontal axis (x-axis) *)
  | Vertical  (** The vertical axis (y-axis) *)

val other : t -> t
(** [other axis] returns the perpendicular axis. Useful for iterating over both
    axes or selecting the cross-axis in layout computations. *)
