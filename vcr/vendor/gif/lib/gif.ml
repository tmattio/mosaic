(** GIF library - Main module that re-exports all functionality *)

(** {1 Core types} *)

include Types

(** {1 Palette operations} *)

let palette_of_array = Palette.of_array
let palette_to_array = Palette.to_array
let palette_size = Palette.size
let palette_get = Palette.get

(** {1 Frame operations} *)

let create_frame ~width ~height ~x_offset ~y_offset ~delay_cs ?disposal
    ?transparent_index ?local_palette ~pixels () =
  {
    width;
    height;
    x_offset;
    y_offset;
    delay_cs;
    disposal = Option.value disposal ~default:No_disposal;
    transparent_index;
    pixels;
    local_palette;
  }

(** {1 GIF data structure operations} *)

let gif_width gif = gif.width
let gif_height gif = gif.height
let gif_frames gif = gif.frames
let gif_global_palette gif = gif.global_palette
let gif_background_index gif = gif.background_index
let gif_loop_count gif = gif.loop_count

let gif_create ~width ~height ~palette ?(background_index = 0) ?loop_count
    ~frames () =
  if width <= 0 || width > 65535 then Error (Invalid_dimensions (width, height))
  else if height <= 0 || height > 65535 then
    Error (Invalid_dimensions (width, height))
  else if background_index < 0 || background_index >= Palette.size palette then
    Error (Invalid_palette_index background_index)
  else if frames = [] then
    Error (Invalid_argument "GIF must have at least one frame")
  else
    (* Validate all frames *)
    let rec validate_frames = function
      | [] -> Ok ()
      | frame :: rest -> (
          match
            Encoder.validate_frame ~encoder_width:width ~encoder_height:height
              ~global_palette:palette frame
          with
          | Error _ as e -> e
          | Ok () -> validate_frames rest)
    in
    match validate_frames frames with
    | Error _ as e -> e
    | Ok () ->
        Ok
          {
            width;
            height;
            global_palette = palette;
            background_index;
            loop_count;
            frames;
          }

(** {1 Encoding} *)

let encode = Encoder.encode
let encode_streaming = Encoder.encode_streaming

type streaming_encoder = Encoder.streaming_encoder

let create_streaming_encoder = Encoder.create_streaming_encoder
let write_frame_streaming = Encoder.write_frame_streaming
let finalize_streaming_encoder = Encoder.finalize_streaming_encoder

(** {1 Decoding} *)

let decode = Decoder.decode
let decode_frames = Decoder.decode_frames

(** {1 Color quantization} *)

module Quantize = Palette.Quantize

let rgb_to_indexed = Palette.rgb_to_indexed
