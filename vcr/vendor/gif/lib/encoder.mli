(** GIF encoder implementation *)

open Types

(** {1 In-memory encoding} *)

val encode : t -> (string, gif_error) result
(** [encode gif] encodes a GIF data structure to binary format.
    @return [Ok data] containing the binary GIF data, or [Error _] on failure.
*)

val encode_streaming :
  t -> writer:(bytes -> int -> int -> unit) -> (unit, gif_error) result
(** [encode_streaming gif writer] encodes a GIF directly to a writer function.
    This is more memory-efficient than [encode] as it doesn't accumulate the
    entire GIF in memory.
    @param gif The GIF data structure to encode.
    @param writer Function to write bytes. Called as [writer buf offset length].
    @return [Ok ()] on success, or [Error _] if encoding fails. *)

(** {1 Frame-by-frame streaming encoder} *)

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
     ?loop_count ()] creates a new streaming encoder. The GIF header with the
    provided palette will be written when the first frame is added.
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
    GIF. On the first frame, this will write the GIF header before writing the
    frame.
    @param encoder The streaming encoder state
    @param frame The frame to write
    @param writer Function to write bytes. Called as [writer buf offset length].
    @return [Ok ()] on success, or [Error _] if encoding fails. *)

val finalize_streaming_encoder :
  streaming_encoder ->
  writer:(bytes -> int -> int -> unit) ->
  (unit, gif_error) result
(** [finalize_streaming_encoder encoder writer] finalizes the GIF by writing the
    trailer. This must be called to produce a valid GIF file.
    @param encoder The streaming encoder state
    @param writer Function to write bytes. Called as [writer buf offset length].
    @return [Ok ()] on success, or [Error _] if finalization fails. *)

(** {1 Frame validation} *)

val validate_frame :
  encoder_width:int ->
  encoder_height:int ->
  global_palette:palette ->
  frame ->
  (unit, gif_error) result
(** [validate_frame ~encoder_width ~encoder_height ~global_palette frame]
    validates that a frame is compatible with the encoder settings.
    @return [Ok ()] if valid, or [Error _] describing the validation failure. *)
