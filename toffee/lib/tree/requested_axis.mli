(** Axis that layout algorithms can be requested to compute a size for.

    [Requested_axis] specifies which dimensions a layout algorithm should
    compute. Unlike {!Geometry.Absolute_axis}, which represents only horizontal
    or vertical axes, [Requested_axis] includes a [Both] variant to indicate
    that both dimensions should be computed simultaneously.

    This type is used in {!Layout_input.t} to control which axis or axes a node
    should compute during layout. Single-axis requests are used when querying
    intrinsic sizes, while [Both] is used for full layout passes. *)

(** The type representing which axis or axes to compute. *)
type t =
  | Horizontal  (** Compute the horizontal axis. *)
  | Vertical  (** Compute the vertical axis. *)
  | Both  (** Compute both axes simultaneously. *)

(** {1 Conversion Functions} *)

val of_absolute_axis : Geometry.Absolute_axis.t -> t
(** [of_absolute_axis axis] converts an absolute axis to a requested axis.

    Maps [Horizontal] to [Horizontal] and [Vertical] to [Vertical]. *)

val to_absolute_axis : t -> Geometry.Absolute_axis.t option
(** [to_absolute_axis t] converts a requested axis to an absolute axis.

    Returns [Some Horizontal] or [Some Vertical] for single-axis requests, and
    [None] for [Both]. *)

(** {1 String Conversion} *)

val to_string : t -> string
(** [to_string t] returns a string representation of the requested axis.

    Returns ["Horizontal"], ["Vertical"], or ["Both"]. *)

(** {1 Comparison and Equality} *)

val compare : t -> t -> int
(** [compare a b] compares two requested axes.

    The ordering is [Horizontal < Vertical < Both]. *)

val equal : t -> t -> bool
(** [equal a b] tests equality of two requested axes. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] pretty-prints a requested axis to a formatter. *)
