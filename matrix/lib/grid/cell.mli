(** Cell representation and styling for the VTE module. *)

type style = {
  bold : bool;
  faint : bool;
  italic : bool;
  underline : bool;
  double_underline : bool;
  fg : Ansi.color;
  bg : Ansi.color;
  reversed : bool;
  link : string option;
  strikethrough : bool;
  overline : bool;
  blink : bool;
}
(** Represents the graphical styling of a single terminal cell. A consumer of
    this library will read these fields to determine how to render it. *)

type attr = style

type t = { glyph : string; width : int; attrs : attr }
(** Represents a single cell on the terminal grid, containing a UTF-8 grapheme
    and its associated attributes. The width field indicates the display width:
    0 for combining characters, 1 for normal characters, 2 for wide characters.
*)

val default_style : style
(** The default style attributes for a new terminal or after a reset. *)

val empty : t option
(** A value representing an empty (unoccupied) cell, equivalent to [None].
    Provided for convenience. *)

val apply_sgr_attr : style -> Ansi.attr -> style
(** [apply_sgr_attr style attr] applies an ANSI SGR attribute to a style,
    returning the updated style. *)
