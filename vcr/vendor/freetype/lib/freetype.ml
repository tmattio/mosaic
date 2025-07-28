(* FreeType 2 OCaml bindings *)

type library
type face

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

type render_mode =
  | Render_normal
  | Render_light
  | Render_mono
  | Render_lcd
  | Render_lcd_v

type kerning_mode = Kerning_default | Kerning_unfitted | Kerning_unscaled

type glyph_metrics = {
  bitmap_left : int;
  bitmap_top : int;
  advance_x : int;
  advance_y : int;
  width : int;
  height : int;
}

type face_metrics = {
  ascender : int;
  descender : int;
  height : int;
  max_advance_width : int;
  max_advance_height : int;
}

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

(* External functions *)
external init : unit -> library = "ft_init"
external new_face : library -> string -> int -> face = "ft_new_face"

external new_memory_face : library -> string -> int -> face
  = "ft_new_memory_face"

external set_pixel_sizes : face -> int -> int -> unit = "ft_set_pixel_sizes"
external load_char : face -> int -> int -> unit = "ft_load_char"
external load_glyph : face -> int -> int -> unit = "ft_load_glyph"
external render_glyph : face -> int -> unit = "ft_render_glyph"

external get_glyph_bitmap : face -> string * glyph_metrics * int
  = "ft_get_glyph_bitmap"

external get_face_metrics : face -> face_metrics = "ft_get_face_metrics"
external get_char_index : face -> int -> int = "ft_get_char_index"
external get_kerning : face -> int -> int -> int -> int * int = "ft_get_kerning"
external has_kerning : face -> bool = "ft_has_kerning"
external num_glyphs : face -> int = "ft_num_glyphs"

external get_glyph_bitmap_bigarray :
  face ->
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t
  * int
  * int
  * int
  * int
  * int
  * int = "ft_get_glyph_bitmap_bigarray"

external get_glyph_metrics_raw :
  face -> int * int * int * int * int * int * int * int = "ft_get_glyph_metrics"

(* Helper functions *)
let load_flag_to_int = function
  | Load_default -> 0x0
  | Load_no_scale -> 0x1
  | Load_no_hinting -> 0x2
  | Load_render -> 0x4
  | Load_no_bitmap -> 0x8
  | Load_force_autohint -> 0x20
  | Load_monochrome -> 0x1000
  | Load_linear_design -> 0x2000
  | Load_target_normal -> 0x0000  (* FT_LOAD_TARGET_NORMAL *)
  | Load_target_light -> 0x10000  (* FT_LOAD_TARGET_LIGHT *)
  | Load_target_mono -> 0x20000   (* FT_LOAD_TARGET_MONO *)
  | Load_target_lcd -> 0x30000    (* FT_LOAD_TARGET_LCD *)
  | Load_target_lcd_v -> 0x40000  (* FT_LOAD_TARGET_LCD_V *)

let render_mode_to_int = function
  | Render_normal -> 0
  | Render_light -> 1
  | Render_mono -> 2
  | Render_lcd -> 3
  | Render_lcd_v -> 4

let kerning_mode_to_int = function
  | Kerning_default -> 0
  | Kerning_unfitted -> 1
  | Kerning_unscaled -> 2

(* Fixed point conversion helpers *)
let from_26_6 v = v / 64
let to_26_6 v = v * 64

(* High-level interface *)
type t = {
  library : library; [@warning "-69"]
  face : face;
  size : int; [@warning "-69"]
}

let create ~font_path ~pixel_size =
  let face_index = 0 in
  let library = init () in
  let face = new_face library font_path face_index in
  set_pixel_sizes face pixel_size pixel_size;
  { library; face; size = pixel_size }

let create_from_memory ~font_data ~pixel_size =
  let face_index = 0 in
  let library = init () in
  let face = new_memory_face library font_data face_index in
  set_pixel_sizes face pixel_size pixel_size;
  { library; face; size = pixel_size }

let load_and_render_char t char_code =
  (* Load the glyph with target normal for antialiasing *)
  load_char t.face char_code (load_flag_to_int Load_target_normal);
  (* Render with antialiasing (Render_normal = antialiased grayscale) *)
  render_glyph t.face (render_mode_to_int Render_normal);
  get_glyph_bitmap t.face

let load_and_render_glyph t glyph_index =
  (* Load the glyph with target normal for antialiasing *)
  load_glyph t.face glyph_index (load_flag_to_int Load_target_normal);
  (* Render with antialiasing (Render_normal = antialiased grayscale) *)
  render_glyph t.face (render_mode_to_int Render_normal);
  get_glyph_bitmap t.face

let get_char_index t char_code = get_char_index t.face char_code
let get_metrics t = get_face_metrics t.face
let get_face t = t.face

(* Wrapper for get_kerning with proper type conversion *)
let get_kerning face left right mode =
  get_kerning face left right (kerning_mode_to_int mode)

(* Convert raw metrics tuple to record *)
let get_glyph_metrics face =
  let w, h, hbx, hby, ha, vbx, vby, va = get_glyph_metrics_raw face in
  {
    width = w;
    height = h;
    hori_bearing_x = hbx;
    hori_bearing_y = hby;
    hori_advance = ha;
    vert_bearing_x = vbx;
    vert_bearing_y = vby;
    vert_advance = va;
  }

(* Render text to RGB buffer *)
let render_text t ~pixels ~width ~height ~x ~y ~color text =
  let r, g, b = color in
  let cur_x = ref x in

  String.iter
    (fun ch ->
      let char_code = Char.code ch in
      (* Load with target normal for antialiasing *)
      load_char t.face char_code (load_flag_to_int Load_target_normal);
      (* Render with antialiasing *)
      render_glyph t.face (render_mode_to_int Render_normal);
      let bitmap_data, metrics, pitch = get_glyph_bitmap t.face in

      (* Blit the glyph bitmap to the pixel buffer *)
      for row = 0 to metrics.height - 1 do
        for col = 0 to metrics.width - 1 do
          let px = !cur_x + metrics.bitmap_left + col in
          let py = y - metrics.bitmap_top + row in

          if px >= 0 && px < width && py >= 0 && py < height then
            let alpha = Char.code bitmap_data.[(row * pitch) + col] in
            if alpha > 0 then (
              let idx = ((py * width) + px) * 3 in
              let a = float_of_int alpha /. 255.0 in
              Bytes.set pixels idx
                (char_of_int
                   (int_of_float
                      (float_of_int (Char.code (Bytes.get pixels idx))
                       *. (1.0 -. a)
                      +. (float_of_int r *. a))));
              Bytes.set pixels (idx + 1)
                (char_of_int
                   (int_of_float
                      (float_of_int (Char.code (Bytes.get pixels (idx + 1)))
                       *. (1.0 -. a)
                      +. (float_of_int g *. a))));
              Bytes.set pixels (idx + 2)
                (char_of_int
                   (int_of_float
                      (float_of_int (Char.code (Bytes.get pixels (idx + 2)))
                       *. (1.0 -. a)
                      +. (float_of_int b *. a)))))
        done
      done;

      cur_x := !cur_x + metrics.advance_x)
    text
