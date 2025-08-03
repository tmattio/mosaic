(** Unicode character width calculation module.

    This module provides efficient width calculation for Unicode characters,
    with special handling for emoji, zero-width joiners, variation selectors,
    and East Asian width properties. *)

val char_width : ?east_asian:bool -> Uchar.t -> int
(** Calculate the display width of a single Unicode character.

    @param east_asian
      If true, ambiguous width characters are treated as width 2. Defaults to
      false.
    @param uchar The Unicode character to measure
    @return
      The display width in terminal columns (-1 for control, 0-2 for others) *)

val string_width : ?east_asian:bool -> string -> int
(** Calculate the display width of a string using proper grapheme segmentation.

    This handles complex emoji sequences, ZWJ sequences, variation selectors,
    and multi-codepoint grapheme clusters according to Unicode standards.

    @param east_asian
      If true, ambiguous width characters are treated as width 2. Defaults to
      false.
    @param s The UTF-8 encoded string
    @return The total display width in terminal columns *)
