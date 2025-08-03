(** Palette operations and color quantization *)

open Types

(** {1 Palette operations} *)

val of_array : color array -> (palette, gif_error) result
(** [of_array colors] creates a palette from a color array. Returns [Error] if
    the array is empty, has more than 256 colors, or contains colors with
    components outside 0-255. *)

val to_array : palette -> color array
(** [to_array p] returns a copy of the palette's colors. *)

val size : palette -> int
(** [size p] returns the number of colors in the palette. *)

val get : palette -> int -> color option
(** [get p idx] returns the color at index [idx], or [None] if out of bounds. *)

val get_exn : palette -> int -> color
(** [get_exn p idx] returns the color at index [idx].
    @raise Invalid_argument if index is out of bounds. *)

val get_opt : palette -> int -> color option
(** [get_opt p idx] is an alias for [get p idx]. *)

(** {1 Color quantization} *)

(** Color quantization utilities *)
module Quantize : sig
  val find_nearest_color : int * int * int -> palette -> int
  (** [find_nearest_color (r,g,b) palette] finds the best matching color. Uses
      Manhattan distance (sum of absolute differences) to find the palette entry
      closest to the given RGB values. Time complexity: O(n) where n is palette
      size. *)

  val create_palette :
    (int * int * int) array -> int -> (palette, gif_error) result
  (** [create_palette rgb_data max_colors] generates an optimal color palette.
      Uses a median-cut algorithm to select up to [max_colors] representative
      colors from the input data. If the input contains fewer unique colors than
      [max_colors], all unique colors are returned. Time complexity: O(n log n +
      k log k) where n is input size, k is unique colors.
      @return
        [Ok palette] optimized for the input data, or [Error _] if max_colors >
        256 or input colors are out of range. *)
end

val rgb_to_indexed :
  ?dither:dithering ->
  width:int ->
  (int * int * int) array ->
  palette ->
  (bytes, gif_error) result
(** [rgb_to_indexed ?dither ~width rgb_data palette] converts RGB pixels to
    palette indices. Maps each RGB pixel to the nearest color in the palette.
    When dithering is enabled, applies Floyd-Steinberg error diffusion.
    @param dither Dithering algorithm to use. Default: No_dither.
    @param width Image width in pixels (needed for dithering).
    @param rgb_data Source pixels as RGB triplets in row-major order.
    @param palette Color table to map against.
    @return
      [Ok data] with indexed pixel data (one byte per pixel), or [Error _]. *)
