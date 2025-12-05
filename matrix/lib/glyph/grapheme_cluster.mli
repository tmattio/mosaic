(** UAX #29 Grapheme Cluster Boundary Implementation.

    This module provides a high-performance, zero-allocation state machine for
    determining Unicode grapheme cluster boundaries. It is optimized
    specifically for terminal rendering engines to allow identifying
    user-perceived characters (including Emoji ZWJ sequences and Flags) without
    the overhead of generic segmentation libraries. *)

val next_boundary : ?ignore_zwj:bool -> string -> int -> int -> int
(** [next_boundary ?ignore_zwj str start limit] returns the byte offset of the
    first byte after the grapheme cluster that begins at [start].

    The [start] offset must be a valid UTF-8 character boundary. The [limit]
    parameter sets the maximum byte offset to scan (typically
    [String.length str]). Returns [limit] if the end of the string is reached.
    When [ignore_zwj] is [true], ZWJ never forms emoji ZWJ sequences (forces a
    break after ZWJ). Defaults to [false].

    REQUIRES: [start] must point to a valid UTF-8 character boundary. *)

val width_of_cp : int -> int
(** [width_of_cp codepoint] returns the width classification for [codepoint].

    Returns:
    - [0] for zero-width marks (combining characters, zero-width joiners)
    - [1] for single-column characters (narrow/default width)
    - [2] for double-column characters (wide/fullwidth, East Asian Wide)
    - [3] for control characters (C0, C1, DEL)

    This is per-codepoint width, not per-grapheme. ZWJ sequences and regional
    indicators require higher-level logic (see {!Glyph.encode}). Classification
    based on UAX #11 East Asian Width property. *)
