(** LZW compression and decompression for GIF encoding/decoding *)

open Types

(** {1 LZW Encoder} *)

module Encoder : sig
  val compress : bytes -> int -> string
  (** [compress data color_bits] compresses pixel data using LZW algorithm.
      @param data The indexed pixel data to compress
      @param color_bits
        The number of bits per color (determines initial code size)
      @return The compressed data as a string (without sub-block packaging) *)

  val package_subblocks : string -> string
  (** [package_subblocks data] packages compressed data into GIF sub-blocks.
      Each sub-block has a size byte (max 255) followed by data bytes. The
      sequence ends with a 0-byte block terminator.
      @param data The compressed data to package
      @return The packaged data ready for GIF output *)
end

(** {1 LZW Decoder} *)

module Decoder : sig
  val decompress : string -> int -> (string, gif_error) result
  (** [decompress compressed_data min_code_size] decompresses LZW data.
      @param compressed_data The compressed data (without sub-block structure)
      @param min_code_size The minimum code size (typically color_bits + 1)
      @return [Ok decompressed] with the decompressed pixel data, or [Error _]
  *)
end
