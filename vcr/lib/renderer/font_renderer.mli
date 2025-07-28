(** Common font rendering functionality *)

type font_set = {
  regular : Freetype.t;
  bold : Freetype.t;
  italic : Freetype.t;
  bold_italic : Freetype.t;
}

type font_renderer =
  | Bitmap of {
      fonts : font_set;
      char_width : int;
      char_height : int;
    }
  | Freetype of font_set

val create_font_renderer : string option -> int -> font_renderer
(** [create_font_renderer font_path font_size] creates a font renderer.
    If [font_path] is None, uses built-in bitmap font. *)

val get_char_dimensions : font_renderer -> int * int
(** [get_char_dimensions renderer] returns (char_width, char_height) *)

val select_font : font_renderer -> Vte.Cell.style -> Freetype.t
(** [select_font renderer style] selects the appropriate font variant based on style *)

val render_char : 
  (int * int * int) array -> int -> int -> int -> int -> 
  Uchar.t -> (int * int * int) -> (int * int * int) -> int -> int -> unit
(** [render_char pixels width height x y char fg_color bg_color char_width char_height]
    renders a character at the given position *)