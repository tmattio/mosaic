(** FreeType 2 OCaml bindings *)

type library
(** FreeType library handle *)

type face
(** Font face handle *)

(** Glyph loading flags *)
type load_flag =
  | Load_default
  | Load_no_scale
  | Load_no_hinting
  | Load_render
  | Load_no_bitmap
  | Load_force_autohint
  | Load_monochrome
  | Load_linear_design
  | Load_target_normal
  | Load_target_light
  | Load_target_mono
  | Load_target_lcd
  | Load_target_lcd_v

(** Rendering modes *)
type render_mode =
  | Render_normal
  | Render_light
  | Render_mono
  | Render_lcd
  | Render_lcd_v

type glyph_metrics = {
  bitmap_left : int;  (** Left bearing of the bitmap *)
  bitmap_top : int;  (** Top bearing of the bitmap *)
  advance_x : int;  (** Horizontal advance *)
  advance_y : int;  (** Vertical advance *)
  width : int;  (** Bitmap width *)
  height : int;  (** Bitmap height *)
}
(** Glyph metrics after rendering *)

type face_metrics = {
  ascender : int;
  descender : int;
  height : int;
  max_advance_width : int;
  max_advance_height : int;
}
(** Face metrics *)

type glyph_metrics_detailed = {
  width : int;
  height : int;
  hori_bearing_x : int;
  hori_bearing_y : int;
  hori_advance : int;
  vert_bearing_x : int;
  vert_bearing_y : int;
  vert_advance : int;
}
(** Detailed glyph metrics (all values in 26.6 fixed point) *)

(** Kerning modes *)
type kerning_mode =
  | Kerning_default  (** Scaled and grid-fitted kerning *)
  | Kerning_unfitted  (** Scaled but not grid-fitted *)
  | Kerning_unscaled  (** Return kerning in font units *)

type t
(** Main FreeType context *)

val create : font_path:string -> pixel_size:int -> t
(** [create ~font_path ~pixel_size] creates a new FreeType context *)

val create_from_memory : font_data:string -> pixel_size:int -> t
(** [create_from_memory ~font_data ~pixel_size] creates a new FreeType context
    from font data in memory *)

val load_and_render_char : t -> int -> string * glyph_metrics * int
(** [load_and_render_char t char_code] loads and renders a character, returning
    bitmap and metrics *)

val load_and_render_glyph : t -> int -> string * glyph_metrics * int
(** [load_and_render_glyph t glyph_index] loads and renders a glyph by index *)

val get_char_index : t -> int -> int
(** [get_char_index t char_code] gets the glyph index for a character *)

val get_metrics : t -> face_metrics
(** [get_metrics t] returns face metrics *)

val render_text :
  t ->
  pixels:bytes ->
  width:int ->
  height:int ->
  x:int ->
  y:int ->
  color:int * int * int ->
  string ->
  unit
(** [render_text t ~pixels ~width ~height ~x ~y ~color text] renders text to RGB
    buffer *)

(** {2 Low-level interface} *)

val init : unit -> library
(** Initialize FreeType library *)

val new_face : library -> string -> int -> face
(** Load a font face *)

val new_memory_face : library -> string -> int -> face
(** Load a font face from memory *)

val set_pixel_sizes : face -> int -> int -> unit
(** Set pixel sizes *)

val get_face : t -> face
(** Get face from context *)

(** {2 Additional bindings} *)

val has_kerning : face -> bool
(** [has_kerning face] returns true if the font has kerning information *)

val get_kerning : face -> int -> int -> kerning_mode -> int * int
(** [get_kerning face left right mode] returns kerning between two glyph
    indices. Returns (x, y) in 26.6 fixed point format *)

val num_glyphs : face -> int
(** [num_glyphs face] returns the number of glyphs in the font *)

val get_glyph_bitmap_bigarray :
  face ->
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t
  * int
  * int
  * int
  * int
  * int
  * int
(** [get_glyph_bitmap_bigarray face] returns the current glyph bitmap as a
    bigarray for zero-copy access. Returns (bitmap, left, top, advance_x,
    advance_y, width, height). advance_x and advance_y are in 26.6 fixed point
    format. *)

val get_glyph_metrics : face -> glyph_metrics_detailed
(** [get_glyph_metrics face] returns detailed metrics for the current glyph *)

(** {2 Fixed point conversion} *)

val from_26_6 : int -> int
(** Convert from 26.6 fixed point to pixels *)

val to_26_6 : int -> int
(** Convert from pixels to 26.6 fixed point *)
