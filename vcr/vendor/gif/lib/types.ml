type gif_error =
  | Palette_too_large of int
  | Color_out_of_range of int * int * int
  | Invalid_dimensions of int * int
  | Invalid_palette_index of int
  | Io_error of exn
  | Decode_error of string
  | Invalid_argument of string

let string_of_error = function
  | Palette_too_large n -> Printf.sprintf "Palette too large: %d" n
  | Color_out_of_range (r, g, b) ->
      Printf.sprintf "Color out of range: (%d, %d, %d)" r g b
  | Invalid_dimensions (w, h) -> Printf.sprintf "Invalid dimensions: %dx%d" w h
  | Invalid_palette_index i -> Printf.sprintf "Invalid palette index: %d" i
  | Io_error exn -> Printf.sprintf "IO error: %s" (Printexc.to_string exn)
  | Decode_error s -> Printf.sprintf "Decode error: %s" s
  | Invalid_argument s -> Printf.sprintf "Invalid argument: %s" s

type color = { r : int; g : int; b : int }

let rgb r g b = { r; g; b }

type palette = color array

type disposal_method =
  | No_disposal
  | Do_not_dispose
  | Restore_background
  | Restore_previous

type centiseconds = int

type frame = {
  width : int;
  height : int;
  x_offset : int;
  y_offset : int;
  delay_cs : centiseconds;
  disposal : disposal_method;
  transparent_index : int option;
  pixels : bytes;
  local_palette : palette option;
}

type t = {
  width : int;
  height : int;
  global_palette : palette;
  background_index : int;
  loop_count : int option;
  frames : frame list;
}

type dithering = No_dither | Floyd_steinberg

let gif_header = Bytes.unsafe_of_string "GIF89a"
let trailer = '\x3B'
let image_separator = '\x2C'
let extension_introducer = '\x21'
let graphic_control_label = '\xF9'
let application_extension_label = '\xFF'
