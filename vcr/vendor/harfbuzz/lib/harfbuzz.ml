(* HarfBuzz OCaml bindings *)

type font
type buffer

type direction =
  | LTR (* Left to right *)
  | RTL (* Right to left *)
  | TTB (* Top to bottom *)
  | BTT (* Bottom to top *)

(* Common scripts - only a subset *)
type script =
  | Script_common
  | Script_latin
  | Script_greek
  | Script_cyrillic
  | Script_arabic
  | Script_hebrew
  | Script_devanagari
  | Script_han
  | Script_hiragana
  | Script_katakana

type glyph_info = {
  codepoint : int; (* Glyph ID *)
  x_advance : int; (* How much to advance X after drawing this glyph *)
  y_advance : int; (* How much to advance Y after drawing this glyph *)
  x_offset : int; (* Offset from current position to draw this glyph *)
  y_offset : int; (* Offset from current position to draw this glyph *)
}

type font_metrics = { ascender : int; descender : int; line_gap : int }

(* External functions *)
external font_create_from_ft_face : Freetype.face -> font
  = "hb_ft_font_create_from_face"

external buffer_create : unit -> buffer = "caml_hb_buffer_create"
external buffer_add_utf8 : buffer -> string -> unit = "caml_hb_buffer_add_utf8"

external buffer_set_direction : buffer -> int -> unit
  = "caml_hb_buffer_set_direction"

external buffer_set_language : buffer -> string -> unit
  = "caml_hb_buffer_set_language"

external buffer_set_script : buffer -> int -> unit = "caml_hb_buffer_set_script"
external buffer_get_length : buffer -> int = "caml_hb_buffer_get_length"
external shape : font -> buffer -> unit = "caml_hb_shape"

external shape_with_features : font -> buffer -> string array -> unit
  = "caml_hb_shape_with_features"

external buffer_get_glyph_infos_and_positions : buffer -> glyph_info array
  = "hb_buffer_get_glyph_infos_and_positions"

external buffer_clear : buffer -> unit = "caml_hb_buffer_clear"
external font_get_metrics : font -> font_metrics = "hb_font_get_metrics"
external font_get_scale : font -> int * int = "caml_hb_font_get_scale"
external font_set_scale : font -> int -> int -> unit = "caml_hb_font_set_scale"

(* Helper functions *)
let set_direction buffer dir =
  let dir_val = match dir with LTR -> 0 | RTL -> 1 | TTB -> 2 | BTT -> 3 in
  buffer_set_direction buffer dir_val

let script_to_int = function
  | Script_common -> 0x5A796E20 (* "Zyyy" *)
  | Script_latin -> 0x4C61746E (* "Latn" *)
  | Script_greek -> 0x47726B20 (* "Grek" *)
  | Script_cyrillic -> 0x43797272 (* "Cyrl" *)
  | Script_arabic -> 0x41726162 (* "Arab" *)
  | Script_hebrew -> 0x48656272 (* "Hebr" *)
  | Script_devanagari -> 0x44657661 (* "Deva" *)
  | Script_han -> 0x48616E20 (* "Hani" *)
  | Script_hiragana -> 0x48697261 (* "Hira" *)
  | Script_katakana -> 0x4B616E61 (* "Kana" *)

let set_script buffer script = buffer_set_script buffer (script_to_int script)

(* High-level interface *)
type t = { font : font; buffer : buffer }

let create_from_ft_face ft_face =
  let font = font_create_from_ft_face ft_face in
  let buffer = buffer_create () in
  { font; buffer }

let shape_text t text =
  buffer_clear t.buffer;
  buffer_add_utf8 t.buffer text;
  set_direction t.buffer LTR;
  shape t.font t.buffer;
  buffer_get_glyph_infos_and_positions t.buffer

let get_metrics t = font_get_metrics t.font

(* Get advance width for a string *)
let text_width t text =
  let glyphs = shape_text t text in
  Array.fold_left (fun acc g -> acc + g.x_advance) 0 glyphs

(* Calculate line height *)
let line_height t =
  let metrics = get_metrics t in
  metrics.ascender - metrics.descender + metrics.line_gap

(* Re-export some functions for the interface *)
let set_language = buffer_set_language
let get_length = buffer_get_length
let shape_with_features = shape_with_features
let get_scale = font_get_scale
let set_scale = font_set_scale
