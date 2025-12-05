(** CSS Grid track repetition counts.

    This module defines how track definitions repeat in CSS Grid layouts.
    Repetitions can be explicit (repeat N times) or automatic (fill/fit the
    container).

    See
    {{:https://www.w3.org/TR/css-grid-1/#auto-repeat}CSS Grid Level 1 -
     Auto-repeat} for the specification of auto-repeated track definitions. *)

type t =
  | Count of int
      (** Repeat the track definition exactly N times. N must be positive. *)
  | Auto_fill
      (** Generate tracks to fill the container. See
          {{:https://developer.mozilla.org/en-US/docs/Web/CSS/repeat#auto-fill}MDN
           auto-fill}. *)
  | Auto_fit
      (** Generate tracks to fit the container. See
          {{:https://developer.mozilla.org/en-US/docs/Web/CSS/repeat#auto-fit}MDN
           auto-fit}. *)

(** {1 Constructors} *)

val count : int -> t
(** [count n] creates a repetition count of exactly [n] times.

    @raise Invalid_argument if [n] is not positive. *)

val auto_fill : t
(** [auto_fill] creates an auto-fill repetition. *)

val auto_fit : t
(** [auto_fit] creates an auto-fit repetition. *)

(** {1 Predicates} *)

val is_auto : t -> bool
(** [is_auto t] returns [true] if [t] is [Auto_fill] or [Auto_fit]. *)

(** {1 Comparison and Equality} *)

val equal : t -> t -> bool
(** [equal a b] returns [true] if [a] and [b] represent the same repetition
    count. *)

val compare : t -> t -> int
(** [compare a b] provides a total ordering over repetition counts. [Count]
    values are ordered first by their integer value, followed by [Auto_fill],
    then [Auto_fit]. *)

(** {1 Conversion} *)

val to_string : t -> string
(** [to_string t] converts [t] to its CSS representation. [Count n] yields the
    integer as a string, [Auto_fill] yields ["auto-fill"], and [Auto_fit] yields
    ["auto-fit"]. *)

val of_string : string -> (t, string) result
(** [of_string s] parses [s] as a repetition count. Accepts ["auto-fit"],
    ["auto-fill"], or a positive integer string. Returns [Error msg] if [s] is
    invalid. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] outputs [t] to [fmt] using {!to_string}. *)
