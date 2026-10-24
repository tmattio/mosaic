(** CSS overflow property.

    The overflow property controls how content that overflows a container
    affects layout. In toffee, overflow primarily impacts the automatic minimum
    size calculation for Flexbox and CSS Grid items and controls scrollbar space
    reservation.

    Overflow has two key layout effects:

    - The automatic minimum size of Flexbox and Grid items with non-[Visible]
      overflow is [0] rather than content-based.
    - [Scroll] overflow reserves space for a scrollbar, controlled by the
      [scrollbar_width] property.

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow}CSS overflow
     documentation}. *)

type t =
  | Visible
      (** Content-based automatic minimum size. Overflowing content contributes
          to parent scroll region. *)
  | Clip
      (** Content-based automatic minimum size. Overflowing content does not
          contribute to parent scroll region. *)
  | Hidden
      (** Zero automatic minimum size. Overflowing content does not contribute
          to parent scroll region. *)
  | Scroll
      (** Zero automatic minimum size with scrollbar space reserved. Overflowing
          content does not contribute to parent scroll region. *)

val default : t
(** [default] returns [Visible]. *)

val to_string : t -> string
(** [to_string overflow] converts [overflow] to its string representation. *)

val is_container : t -> bool
(** [is_container overflow] returns [true] if [overflow] contains its contents.

    Returns [true] for [Hidden] and [Scroll], [false] for [Visible] and [Clip].
*)

val to_automatic_min_size : t -> Dimension.t
(** [to_automatic_min_size overflow] returns the automatic minimum size for
    [overflow].

    Returns [Dimension.zero] for [Hidden] and [Scroll], [Dimension.auto] for
    [Visible] and [Clip]. This determines the automatic minimum size of Flexbox
    and CSS Grid items. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality. *)

val compare : t -> t -> int
(** [compare a b] returns a total ordering. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt overflow] formats [overflow] for display. *)
