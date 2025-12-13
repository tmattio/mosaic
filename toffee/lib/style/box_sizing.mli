(** CSS box-sizing property.

    Specifies whether size styles for a node apply to its content box or border
    box.

    The content box is the node's inner size excluding padding, border, and
    margin. The border box is the node's outer size including padding and border
    but excluding margin.

    This property affects the interpretation of the following size styles:

    - [size]
    - [min_size]
    - [max_size]
    - [flex_basis] (in flexbox layout)

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing} MDN
     box-sizing documentation}. *)

type t =
  | Border_box
      (** Size styles specify the box's border box.

          The size includes padding and border but excludes margin. This is the
          default behavior and matches the CSS [box-sizing: border-box]
          property. *)
  | Content_box
      (** Size styles specify the box's content box.

          The size excludes padding, border, and margin. Matches the CSS
          [box-sizing: content-box] property. *)

val default : t
(** [default] returns [Border_box]. *)

val to_string : t -> string
(** [to_string box_sizing] converts the box sizing mode to its CSS string
    representation.

    Returns ["border-box"] for [Border_box] and ["content-box"] for
    [Content_box]. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality. *)

val compare : t -> t -> int
(** [compare a b] compares box sizing modes for use in ordered containers. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] prints the box sizing mode to the formatter. *)
