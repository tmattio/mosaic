(** An abstract line with start and end positions.

    A line represents any type that has a start and an end, used throughout the
    layout engine to represent pairs of values along an axis (e.g., padding,
    margin, or insets in a single direction). *)

type 'a t = { start : 'a; end_ : 'a }
(** The type of a line with start and end values of type ['a].

    The [end_] field uses an underscore suffix to avoid conflict with the OCaml
    keyword [end]. *)

(** {1 Constants} *)

val both_true : bool t
(** A line with both start and end set to [true]. *)

val both_false : bool t
(** A line with both start and end set to [false]. *)

(** {1 Creation} *)

val make : 'a -> 'a -> 'a t
(** [make start end_] creates a line. *)

(** {1 Transformations} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f line] applies [f] to both start and end values. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f line1 line2] applies [f] to corresponding components. *)

(** {1 Operations} *)

val sum : float t -> float
(** [sum line] returns the sum of start and end values.

    This is typically used when computing total padding or margins along an
    axis. *)

(** {1 Comparison and display} *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** [compare cmp a b] compares two lines using [cmp].

    Compares [start] fields first, then [end_] fields if start values are equal.
*)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal eq a b] tests equality of two lines using [eq].

    Returns [true] if both start and end values are equal according to [eq]. *)

val to_string : ('a -> string) -> 'a t -> string
(** [to_string f line] converts [line] to a string representation using [f] to
    format individual values. *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** [pp f fmt line] pretty-prints [line] to [fmt] using [f] to format individual
    values. *)
