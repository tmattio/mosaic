(** GIF encoder library.

    This library provides a pure OCaml implementation for encoding animated
    GIFs, supporting features like multiple frames, local color tables,
    transparency, and various disposal methods. The encoder follows the GIF89a
    specification and produces compact output using LZW compression.

    All color values must be in the range 0-255. Frame dimensions and positions
    must fit within the canvas size. The library validates all inputs to prevent
    invalid GIF generation.

    {1 Basic Usage}

    Create a simple animated GIF:
    {[
      let palette = [| Gif.rgb 0 0 0; Gif.rgb 255 0 0; Gif.rgb 0 255 0 |] in
      let frame1 = {
        width = 100; height = 100;
        left = 0; top = 0;
        delay = 50; (* 0.5 seconds *)
        disposal = No_disposal;
        transparent_index = None;
        data = Bytes.create 10000; (* Indexed pixel data *)
        local_color_table = None;
      } in
      let gif_data = Gif.encode ~width:100 ~height:100 ~frames:[frame1] palette
    ]}

    {1 Standards Compliance}

    This library implements the GIF89a specification, including:
    - LZW compression with adaptive code size (RFC 1951)
    - NETSCAPE2.0 application extension for looping
    - Graphics Control Extension for timing and transparency
    - Support for both global and local color tables *)

type t
(** GIF data structure.

    Represents a complete GIF with its metadata and frames. *)

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
(** RGB color.

    Each component must be in the range 0-255. Values outside this range will
    cause invalid GIF generation. *)

val rgb : int -> int -> int -> color
(** [rgb r g b] creates a color.

    All values must be in the range 0-255. The function does not validate
    inputs; invalid values will cause errors during GIF encoding. *)

(** {2 Palette operations} *)

type palette
(** Opaque type representing a color palette.

    A palette contains up to 256 colors. This opaque type ensures palette
    validation and allows for future optimizations without breaking the API. *)

val palette_of_array : color array -> (palette, gif_error) result
(** [palette_of_array colors] creates a palette from a color array.

    @return
      [Ok palette] if the array has 1-256 colors with valid components, or
      [Error _] if the array is empty, too large, or contains invalid colors. *)

val palette_to_array : palette -> color array
(** [palette_to_array p] returns a copy of the palette's colors. *)

val palette_size : palette -> int
(** [palette_size p] returns the number of colors in the palette. *)

val palette_get : palette -> int -> color option
(** [palette_get p idx] returns the color at index [idx], or [None] if out of
    bounds. *)

(** {2 Frames} *)

(** Frame disposal method.

    Controls how the frame is treated after its delay time has elapsed and
    before the next frame is rendered. This affects animation appearance and can
    be used for optimization. *)
type disposal_method =
  | No_disposal
      (** No disposal specified. The decoder is free to choose. Most decoders
          treat this as [Do_not_dispose]. *)
  | Do_not_dispose
      (** Leave the frame in place. The next frame will be drawn on top. Useful
          for accumulative animations or when frames share content. *)
  | Restore_background
      (** Clear the frame area to the background color before drawing the next
          frame. Useful for frames that need a clean slate. *)
  | Restore_previous
      (** Restore the area to what it looked like before this frame was drawn.
          Useful for temporary overlays. Note: Requires decoder buffering. *)

type centiseconds = int
(** Time duration in centiseconds (1/100th of a second). *)

type frame = {
  width : int;
      (** Frame width in pixels. Must be positive and
          [x_offset + width <= canvas_width]. *)
  height : int;
      (** Frame height in pixels. Must be positive and
          [y_offset + height <= canvas_height]. *)
  x_offset : int;  (** X position within canvas. Must be non-negative. *)
  y_offset : int;  (** Y position within canvas. Must be non-negative. *)
  delay_cs : centiseconds;
      (** Display duration. Range: 0-65535. 0 means no delay (not recommended
          for animations). *)
  disposal : disposal_method;  (** How to handle this frame after display. *)
  transparent_index : int option;
      (** Color index to treat as transparent. Must be within the palette
          bounds. When [None], all pixels are opaque. *)
  pixels : bytes;
      (** Indexed pixel data in row-major order. Length must equal
          [width * height]. Each byte is an index into the color table (local if
          present, else global). *)
  local_palette : palette option;
      (** Frame-specific palette. When present, overrides the global palette.
          Limited to 256 colors. Useful for frames with distinct color needs. *)
}
(** Animation frame.

    Represents a single frame in a GIF animation. Frames can be positioned
    anywhere within the canvas and may have their own color tables. *)

val create_frame :
  width:int ->
  height:int ->
  x_offset:int ->
  y_offset:int ->
  delay_cs:centiseconds ->
  ?disposal:disposal_method ->
  ?transparent_index:int ->
  ?local_palette:palette ->
  pixels:bytes ->
  unit ->
  frame
(** [create_frame ~width ~height ~x_offset ~y_offset ~delay_cs ?disposal
     ?transparent_index ?local_palette ~pixels ()] creates a new frame.

    Validates all parameters and returns an error if any are invalid. The pixel
    data must match the specified dimensions. *)

(** {2 GIF data structure operations} *)

val gif_width : t -> int
(** [gif_width gif] returns the canvas width in pixels. *)

val gif_height : t -> int
(** [gif_height gif] returns the canvas height in pixels. *)

val gif_frames : t -> frame list
(** [gif_frames gif] returns the list of frames in display order. *)

val gif_global_palette : t -> palette
(** [gif_global_palette gif] returns the global color palette. *)

val gif_background_index : t -> int
(** [gif_background_index gif] returns the background color index. *)

val gif_loop_count : t -> int option
(** [gif_loop_count gif] returns the loop count (None = not specified, Some 0 =
    forever). *)

val gif_create :
  width:int ->
  height:int ->
  palette:palette ->
  ?background_index:int ->
  ?loop_count:int ->
  frames:frame list ->
  unit ->
  (t, gif_error) result
(** [gif_create ~width ~height ~palette ?background_index ?loop_count ~frames
     ()] creates a GIF data structure.

    This validates all parameters and frames but doesn't encode anything yet. *)

(** {2 Encoder API}

    The main encoding API is the [encode] function that works with the GIF data
    structure. For memory-efficient streaming, use [create_streaming_encoder]
    and related functions. *)

val encode : t -> (string, gif_error) result
(** [encode gif] encodes a GIF data structure to binary format.

    @return [Ok data] containing the binary GIF data. *)

val encode_streaming :
  t -> writer:(bytes -> int -> int -> unit) -> (unit, gif_error) result
(** [encode_streaming gif writer] encodes a GIF directly to a writer function.

    This is more memory-efficient than [encode] as it doesn't accumulate the
    entire GIF in memory.

    @param gif The GIF data structure to encode.
    @param writer Function to write bytes. Called as [writer buf offset length].
    @return [Ok ()] on success, or [Error _] if encoding fails. *)

(** {2 Streaming Encoder API}

    For true frame-by-frame streaming, use these functions. *)

type streaming_encoder
(** Opaque type representing a streaming GIF encoder state. *)

val create_streaming_encoder :
  width:int ->
  height:int ->
  palette:palette ->
  ?background_index:int ->
  ?loop_count:int ->
  unit ->
  (streaming_encoder, gif_error) result
(** [create_streaming_encoder ~width ~height ~palette ?background_index
     ?loop_count ()] creates a new streaming encoder.

    The GIF header with the provided palette will be written when the first
    frame is added.

    @param width Canvas width in pixels
    @param height Canvas height in pixels
    @param palette The color palette to use for encoding
    @param background_index
      Index of the background color in the palette (default: 0)
    @param loop_count
      Number of loops (None = no loop extension, Some 0 = forever)
    @return [Ok encoder] on success, or [Error _] if parameters are invalid. *)

val write_frame_streaming :
  streaming_encoder ->
  frame ->
  writer:(bytes -> int -> int -> unit) ->
  (unit, gif_error) result
(** [write_frame_streaming encoder frame writer] writes a single frame to the
    GIF.

    On the first frame, this will write the GIF header before writing the frame.

    @param encoder The streaming encoder state
    @param frame The frame to write
    @param writer Function to write bytes. Called as [writer buf offset length].
    @return [Ok ()] on success, or [Error _] if encoding fails. *)

val finalize_streaming_encoder :
  streaming_encoder ->
  writer:(bytes -> int -> int -> unit) ->
  (unit, gif_error) result
(** [finalize_streaming_encoder encoder writer] finalizes the GIF by writing the
    trailer.

    This must be called to produce a valid GIF file.

    @param encoder The streaming encoder state
    @param writer Function to write bytes. Called as [writer buf offset length].
    @return [Ok ()] on success, or [Error _] if finalization fails. *)

(** Dithering algorithm for color quantization *)
type dithering =
  | No_dither  (** Direct color mapping without dithering *)
  | Floyd_steinberg  (** Floyd-Steinberg error diffusion *)

(** Color quantization utilities.

    Provides functions for reducing full-color images to indexed color by
    selecting representative colors and mapping pixels to the nearest match. *)
module Quantize : sig
  val find_nearest_color : int * int * int -> palette -> int
  (** [find_nearest_color (r,g,b) palette] finds the best matching color.

      Uses Manhattan distance (sum of absolute differences) to find the palette
      entry closest to the given RGB values. This metric is fast and avoids
      overflow compared to Euclidean distance.

      Time complexity: O(n) where n is palette size. *)

  val create_palette :
    (int * int * int) array -> int -> (palette, gif_error) result
  (** [create_palette rgb_data max_colors] generates an optimal color palette.

      Uses a median-cut algorithm to select up to [max_colors] representative
      colors from the input data. The algorithm recursively subdivides the color
      space to minimize quantization error.

      If the input contains fewer unique colors than [max_colors], all unique
      colors are returned. Colors are weighted by frequency to produce better
      results for common colors.

      Time complexity: O(n log n + k log k) where n is input size, k is unique
      colors.

      @param rgb_data Array of RGB triplets from the source image.
      @param max_colors Maximum palette size (typically 256 for GIF).
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
    palette indices.

    Maps each RGB pixel to the nearest color in the palette using
    [Quantize.find_nearest_color]. When dithering is enabled, applies error
    diffusion to improve visual quality.

    Time complexity: O(n × p) where n is pixel count, p is palette size.

    @param dither Dithering algorithm to use. Default: No_dither.
    @param width Image width in pixels (needed for dithering).
    @param rgb_data Source pixels as RGB triplets in row-major order.
    @param palette Color table to map against.
    @return
      [Ok data] with indexed pixel data (one byte per pixel), or [Error _] if
      input colors are out of range. *)

val decode : string -> (t, gif_error) result
(** [decode data] parses GIF structure from binary data.

    Validates the GIF format and extracts metadata without decompressing image
    data. Supports both GIF87a and GIF89a formats.

    The decoder verifies:
    - Header magic bytes
    - Logical screen descriptor
    - Extension blocks (graphics control, application, etc.)
    - Image descriptor blocks
    - Trailer byte

    @param data Complete GIF file contents as a binary string.
    @return
      [Ok info] with parsed GIF structure, or [Error (Decode_error msg)] if the
      data is malformed, truncated, or contains unsupported features.

    {[
      let gif_data = In_channel.with_open_bin "test.gif" In_channel.input_all in
      let info = Decode.decode gif_data in
      Printf.printf "GIF: %dx%d with %d frames\n" info.width info.height
        (List.length info.frames)
    ]} *)

val decode_frames : string -> (frame list, gif_error) result
(** [decode_frames gif_data] decodes a GIF and returns just the frames.

    Convenience function that decodes the GIF and extracts the frame list,
    discarding metadata. Useful when you only need the pixel data.

    @param gif_data Complete GIF file contents as a binary string.
    @return
      [Ok frames] with decompressed pixel data, or [Error _] if the data is
      malformed. *)
