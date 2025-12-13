(** CSS Grid item placement specification.

    This module defines how grid items are positioned within a CSS Grid
    container using line-based placement. It supports CSS grid line coordinates,
    named grid lines, and span-based placement.

    Grid lines use CSS coordinates where positive indices start from 1 at the
    grid's start edge and negative indices start from -1 at the grid's end edge.
    Line index 0 is invalid and treated as [Auto].

    See
    {{:https://www.w3.org/TR/css-grid-1/#line-placement}CSS Grid Line Placement}.
*)

type grid_line = int
(** Grid line coordinate in CSS Grid coordinates.

    Positive values start from 1 at the grid's start edge. Negative values start
    from -1 at the grid's end edge. The value 0 is invalid and treated as
    [Auto]. *)

(** Grid placement specification for a single axis.

    Defaults to [Auto]. *)
type t =
  | Auto  (** Place item according to auto-placement algorithm. *)
  | Line of grid_line  (** Place item at specified line index. *)
  | Named_line of string * int
      (** Place item at specified named line. The string is the line name and
          the int is the occurrence count (e.g., the 2nd line named "header").
      *)
  | Span of int  (** Item spans specified number of tracks. *)
  | Named_span of string * int
      (** Item spans until the nth line named with the given string. *)

(** {1 Construction} *)

val auto : t
(** [auto] creates an automatic placement. Equivalent to [Auto]. *)

val line : int -> t
(** [line index] creates a line-based placement at [index].

    Line 0 is invalid and will be treated as [Auto] during layout. *)

val span : int -> t
(** [span count] creates a span-based placement spanning [count] tracks. *)

val named_line : string -> int -> t
(** [named_line name index] creates a placement at the [index]th occurrence of
    the named line [name]. *)

val named_span : string -> int -> t
(** [named_span name count] creates a span until the [count]th occurrence of the
    named line [name]. *)

val default : t
(** [default] returns [Auto]. *)

(** {1 Predicates} *)

val is_definite : t -> bool
(** [is_definite placement] returns [true] if the placement is definite.

    A placement is definite if it specifies a non-zero line index or a named
    line. [Auto] and [Span] values are indefinite. Line 0 is invalid and treated
    as [Auto], thus indefinite. *)

(** {1 Conversion} *)

val into_origin_zero_placement_ignoring_named :
  t -> int -> Grid.Origin_zero_placement.t
(** [into_origin_zero_placement_ignoring_named placement explicit_track_count]
    converts [placement] to origin-zero coordinates, ignoring named lines.

    Named lines and named spans are converted to [Auto]. Line 0 is converted to
    [Auto]. Other line placements are converted using [explicit_track_count] to
    translate CSS grid coordinates (1-indexed, negatives from end) to
    origin-zero coordinates (0-indexed from start). *)

val into_origin_zero_placement : t -> int -> Grid.Origin_zero_placement.t
(** [into_origin_zero_placement placement explicit_track_count] converts
    [placement] to origin-zero coordinates.

    Line 0 is converted to [Auto]. Other line placements are converted using
    [explicit_track_count] to translate CSS grid coordinates to origin-zero
    coordinates.

    @raise Failure
      if [placement] is [Named_line] or [Named_span]. Named lines must be
      resolved before calling this function. *)

(** {1 Comparison and display} *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality of two placements. *)

val compare : t -> t -> int
(** [compare a b] provides a total ordering on placements.

    Ordering: [Auto < Line < Named_line < Span < Named_span]. Within each
    variant, values are compared structurally. *)

val to_string : t -> string
(** [to_string placement] converts [placement] to a string representation. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt placement] pretty-prints [placement] to [fmt]. *)

(** {1 Line operations}

    Operations on [Geometry.Line.t] containing grid placements. These work on
    pairs of start and end placements along a single axis. *)

module Line : sig
  val into_origin_zero_ignoring_named :
    t Geometry.Line.t -> int -> Grid.Origin_zero_placement.t Geometry.Line.t
  (** [into_origin_zero_ignoring_named line explicit_track_count] converts both
      start and end placements to origin-zero coordinates, ignoring named lines.

      Applies {!into_origin_zero_placement_ignoring_named} to both the start and
      end placements. *)

  val into_origin_zero :
    t Geometry.Line.t -> int -> Grid.Origin_zero_placement.t Geometry.Line.t
  (** [into_origin_zero line explicit_track_count] converts both start and end
      placements to origin-zero coordinates.

      Applies {!into_origin_zero_placement} to both the start and end
      placements.

      @raise Failure
        if either placement is [Named_line] or [Named_span]. Named lines must be
        resolved before calling this function. *)

  val is_definite : t Geometry.Line.t -> bool
  (** [is_definite line] returns [true] if the line placement is definite.

      A line placement is definite if at least one of the start or end positions
      is a non-zero line index or a named line. *)
end
