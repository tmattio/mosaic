(** Controls whether flex items wrap onto multiple lines.

    Defaults to {!No_wrap}.

    See
    {{:https://www.w3.org/TR/css-flexbox-1/#flex-wrap-property}CSS Flexbox
     Specification}. *)

type t =
  | No_wrap  (** Items stay on a single line *)
  | Wrap  (** Items wrap to multiple lines as needed *)
  | Wrap_reverse  (** Items wrap to multiple lines in reverse direction *)

val default : t
(** [default] returns [No_wrap]. *)

val to_string : t -> string
(** [to_string t] returns the CSS string representation of [t]. *)

val equal : t -> t -> bool
(** [equal a b] returns [true] if [a] and [b] are the same wrap mode. *)

val compare : t -> t -> int
(** [compare a b] compares [a] and [b] for use in maps and sets. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] prints [t] to formatter [fmt]. *)
