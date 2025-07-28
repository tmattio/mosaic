(** HarfBuzz OCaml bindings for text shaping *)

(** Text direction *)
type direction =
  | LTR  (** Left to right *)
  | RTL  (** Right to left *)
  | TTB  (** Top to bottom *)
  | BTT  (** Bottom to top *)

(** Script identification *)
type script =
  | Script_common  (** Common characters *)
  | Script_latin  (** Latin script *)
  | Script_greek  (** Greek script *)
  | Script_cyrillic  (** Cyrillic script *)
  | Script_arabic  (** Arabic script *)
  | Script_hebrew  (** Hebrew script *)
  | Script_devanagari  (** Devanagari script *)
  | Script_han  (** Han (Chinese) script *)
  | Script_hiragana  (** Hiragana script *)
  | Script_katakana  (** Katakana script *)

type glyph_info = {
  codepoint : int;  (** Glyph ID in the font *)
  x_advance : int;  (** Horizontal advance after drawing this glyph *)
  y_advance : int;  (** Vertical advance after drawing this glyph *)
  x_offset : int;  (** Horizontal offset from current position *)
  y_offset : int;  (** Vertical offset from current position *)
}
(** Information about a shaped glyph *)

type font_metrics = {
  ascender : int;  (** Height above baseline *)
  descender : int;  (** Depth below baseline (negative) *)
  line_gap : int;  (** Recommended gap between lines *)
}
(** Font metrics *)

type t
(** Main harfbuzz context *)

val create_from_ft_face : Freetype.face -> t
(** [create_from_ft_face ft_face] creates a new HarfBuzz context from a FreeType
    face *)

val shape_text : t -> string -> glyph_info array
(** [shape_text t text] shapes the given text and returns glyph information *)

val get_metrics : t -> font_metrics
(** [get_metrics t] returns font metrics *)

val text_width : t -> string -> int
(** [text_width t text] returns the total advance width of shaped text *)

val line_height : t -> int
(** [line_height t] returns the recommended line height *)

(** {2 Additional bindings} *)

type font
(** Type aliases for internal types *)

type buffer

val set_direction : buffer -> direction -> unit
(** [set_direction buffer dir] sets the text direction for the buffer *)

val set_script : buffer -> script -> unit
(** [set_script buffer script] sets the script for the buffer *)

val set_language : buffer -> string -> unit
(** [set_language buffer lang] sets the language for the buffer (BCP 47 format)
*)

val get_length : buffer -> int
(** [get_length buffer] returns the number of items in the buffer *)

val shape_with_features : font -> buffer -> string array -> unit
(** [shape_with_features font buffer features] shapes text with OpenType
    features. Features are strings like "kern", "liga", "smcp", etc. *)

val get_scale : font -> int * int
(** [get_scale font] returns the (x_scale, y_scale) of the font *)

val set_scale : font -> int -> int -> unit
(** [set_scale font x_scale y_scale] sets the scale of the font *)
