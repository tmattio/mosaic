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

type t = { char : Uchar.t; style : style }
(** Represents a single cell on the terminal grid, containing a Unicode
    character and its associated style. *)

val default_style : style
(** The default style attributes for a new terminal or after a reset. *)

val empty : t option
(** A value representing an empty (unoccupied) cell, equivalent to [None].
    Provided for convenience. *)

val apply_sgr_attr : style -> Ansi.attr -> style
(** [apply_sgr_attr style attr] applies an ANSI SGR attribute to a style,
    returning the updated style. *)
