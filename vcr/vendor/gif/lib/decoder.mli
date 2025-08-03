(** GIF decoder implementation *)

open Types

val decode : string -> (t, gif_error) result
(** [decode data] parses GIF structure from binary data. Validates the GIF
    format and extracts metadata without decompressing image data. Supports both
    GIF87a and GIF89a formats.

    The decoder verifies:
    - Header magic bytes
    - Logical screen descriptor
    - Extension blocks (graphics control, application, etc.)
    - Image descriptor blocks
    - Trailer byte

    @param data Complete GIF file contents as a binary string.
    @return
      [Ok info] with parsed GIF structure, or [Error (Decode_error msg)] if the
      data is malformed, truncated, or contains unsupported features. *)

val decode_frames : string -> (frame list, gif_error) result
(** [decode_frames gif_data] decodes a GIF and returns just the frames.
    Convenience function that decodes the GIF and extracts the frame list,
    discarding metadata. Useful when you only need the pixel data.
    @param gif_data Complete GIF file contents as a binary string.
    @return
      [Ok frames] with decompressed pixel data, or [Error _] if the data is
      malformed. *)
