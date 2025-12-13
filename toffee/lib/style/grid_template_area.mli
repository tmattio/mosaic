(** CSS grid template areas.

    A grid template area defines a named rectangular region in a CSS grid. Named
    areas allow grid items to be positioned using semantic identifiers rather
    than numeric line indices. Multiple grid items can reference the same area
    name.

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-areas}MDN:
     grid-template-areas}. *)

type t = {
  name : string;
      (** The name identifying this grid area. Area names must match across all
          rows they span to form a valid rectangular region. *)
  row_start : int;
      (** The starting row line index in grid coordinates. Uses 1-based
          indexing. *)
  row_end : int;
      (** The ending row line index in grid coordinates. Exclusive bound using
          1-based indexing. *)
  column_start : int;
      (** The starting column line index in grid coordinates. Uses 1-based
          indexing. *)
  column_end : int;
      (** The ending column line index in grid coordinates. Exclusive bound
          using 1-based indexing. *)
}
(** A named grid area.

    Invariant: [row_start < row_end] and [column_start < column_end] for valid
    rectangular regions. *)

(** {1 Constructors} *)

val make :
  name:string ->
  row_start:int ->
  row_end:int ->
  column_start:int ->
  column_end:int ->
  t
(** [make ~name ~row_start ~row_end ~column_start ~column_end] creates a grid
    area.

    Coordinates use 1-based indexing. The [row_end] and [column_end] values are
    exclusive bounds.

    {b Precondition}: [row_start < row_end] and [column_start < column_end].
    This is not validated; invalid coordinates may cause incorrect layout
    behavior. *)

(** {1 Accessors} *)

val name : t -> string
(** [name t] returns the area's name identifier. *)

val row_start : t -> int
(** [row_start t] returns the starting row line index. *)

val row_end : t -> int
(** [row_end t] returns the ending row line index. *)

val column_start : t -> int
(** [column_start t] returns the starting column line index. *)

val column_end : t -> int
(** [column_end t] returns the ending column line index. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality of grid areas.

    Two areas are equal if all their fields match. *)

val compare : t -> t -> int
(** [compare a b] provides total ordering of grid areas.

    Comparison proceeds lexicographically: name, then row_start, then row_end,
    then column_start, then column_end. *)
