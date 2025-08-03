(** Core types shared across GIF modules *)

(** Errors that may be returned by any function in the library. *)
type gif_error =
  | Palette_too_large of int
  | Color_out_of_range of int * int * int
  | Invalid_dimensions of int * int
  | Invalid_palette_index of int
  | Io_error of exn
  | Decode_error of string
  | Invalid_argument of string

val string_of_error : gif_error -> string
(** Convert a gif_error to a human-readable string *)

type color = { r : int; g : int; b : int }
(** RGB color. Each component must be in the range 0-255. *)

val rgb : int -> int -> int -> color
(** [rgb r g b] creates a color. All values must be in range 0-255. *)

type palette = color array
(** Type representing a color palette. A palette contains up to 256 colors. *)

(** Frame disposal method. Controls how the frame is treated after its delay
    time has elapsed. *)
type disposal_method =
  | No_disposal  (** No disposal specified. The decoder is free to choose. *)
  | Do_not_dispose
      (** Leave the frame in place. The next frame will be drawn on top. *)
  | Restore_background  (** Clear the frame area to the background color. *)
  | Restore_previous
      (** Restore the area to what it looked like before this frame was drawn.
      *)

type centiseconds = int
(** Time duration in centiseconds (1/100th of a second). *)

type frame = {
  width : int;  (** Frame width in pixels. Must be positive. *)
  height : int;  (** Frame height in pixels. Must be positive. *)
  x_offset : int;  (** X position within canvas. Must be non-negative. *)
  y_offset : int;  (** Y position within canvas. Must be non-negative. *)
  delay_cs : centiseconds;  (** Display duration. Range: 0-65535. *)
  disposal : disposal_method;  (** How to handle this frame after display. *)
  transparent_index : int option;  (** Color index to treat as transparent. *)
  pixels : bytes;  (** Indexed pixel data in row-major order. *)
  local_palette : palette option;
      (** Frame-specific palette. When present, overrides the global palette. *)
}
(** Animation frame. Represents a single frame in a GIF animation. *)

type t = {
  width : int;
  height : int;
  global_palette : palette;
  background_index : int;
  loop_count : int option;
  frames : frame list;
}
(** GIF data structure. Represents a complete GIF with its metadata and frames.
*)

(** Dithering algorithm for color quantization *)
type dithering =
  | No_dither  (** Direct color mapping without dithering *)
  | Floyd_steinberg  (** Floyd-Steinberg error diffusion *)

val gif_header : bytes
(** GIF format constants - exposed for low-level operations *)

val trailer : char
val image_separator : char
val extension_introducer : char
val graphic_control_label : char
val application_extension_label : char
