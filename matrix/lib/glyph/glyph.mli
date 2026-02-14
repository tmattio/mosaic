(** High-performance, reference-counted Unicode glyph storage.

    This library provides a 32-bit packed representation of visual characters
    ("glyphs") optimized for terminal emulators. It distinguishes between:

    - {b Simple}: ASCII characters (0-127) stored directly. Zero allocation,
      zero lookup.
    - {b Complex}: Unicode grapheme clusters interned in a pool. Stored as Pool
      Index + Generation ID + Width + Flags.

    {1 Quick Start}

    Create a pool, encode a string, and process glyphs via callback:

    {[
      let pool = create_pool () in
      encode pool ~width_method:`Unicode ~tab_width:2 "Hello 👋 World"
        (fun glyph -> Printf.printf "%s " (to_string pool glyph))
      (* Output: H e l l o 👋 W o r l d *)
    ]}

    Multi-column characters emit one Start glyph followed by Continuation
    glyphs. Control and zero-width sequences are skipped.

    {1 Memory Safety}

    The pool uses manual reference counting with automatic slot recycling. To
    prevent "use-after-free" errors where a Grid might hold an old ID that
    points to a recycled slot containing different data, IDs include a
    {b Generation Counter}. Accessing a glyph with a stale generation returns
    safe defaults (empty/zero width).

    {1 Width Calculation}

    Implements robust width calculation logic (UAX #11), correctly handling:
    - ZWJ sequences (Emoji composition)
    - Regional Indicators (Flags)
    - Variation Selectors
    - Skin tone modifiers *)

type pool
(** Storage context for interned grapheme clusters.

    NOT thread-safe. Use separate pools per thread or external synchronization.
*)

type t = int
(** A packed integer representing a visual glyph.

    Uses 31 bits of the native int for glyph data, compatible with both 32-bit
    and 64-bit OCaml. Always unboxed (immediate value) with zero allocation
    overhead. *)

type width_method = [ `Unicode | `Wcwidth | `No_zwj ]
(** Width calculation method for grapheme clusters.

    - [`Unicode]: Full UAX #29 segmentation with ZWJ emoji composition. Use for
      correct emoji and flag rendering.
    - [`Wcwidth]: Per-codepoint width calculation without grapheme clustering.
      Use for legacy compatibility with wcwidth-based systems.
    - [`No_zwj]: UAX #29 segmentation that forces a break after ZWJ (no emoji
      ZWJ sequences), but keeps the full grapheme-aware width logic (RI pairs,
      VS16, Indic virama). *)

(** {1 Lifecycle} *)

val create_pool : unit -> pool
(** [create_pool ()] creates a new empty pool with initial capacity for 4096
    glyphs. *)

val clear : pool -> unit
(** [clear pool] resets the pool, invalidating ALL existing glyph references.

    Glyphs must not be used after [clear]; behavior is undefined because IDs may
    be recycled with the same generation. Does not free memory, only resets
    internal cursors for reuse. *)

(** {1 Reference Counting} *)

val incref : pool -> t -> unit
(** [incref pool glyph] increments the reference count for [glyph].

    No-op for Simple glyphs or stale Complex glyphs with mismatched generations.
*)

val decref : pool -> t -> unit
(** [decref pool glyph] decrements the reference count for [glyph].

    When count reaches 0, the slot is recycled and its generation is
    incremented. No-op for Simple glyphs or stale Complex glyphs. *)

(** {1 Encoding & Creation} *)

val encode :
  pool ->
  width_method:width_method ->
  tab_width:int ->
  string ->
  (t -> unit) ->
  unit
(** [encode pool ~width_method ~tab_width str f] streams glyphs from [str] via
    callback [f].

    The callback [(fun glyph -> ...)] is invoked inline for each glyph in string
    order. Multi-column characters emit one Start glyph followed by
    [(width - 1)] Continuation glyphs. Control characters and zero-width
    sequences are skipped.

    ASCII characters become Simple glyphs immediately. Unicode clusters are
    interned and become Complex glyphs.

    {b UTF-8 handling}: Invalid byte sequences are replaced with U+FFFD
    (replacement character) per Unicode best practices. Each invalid byte
    consumes exactly one byte and produces one replacement glyph. *)

val intern :
  pool ->
  ?width_method:width_method ->
  ?tab_width:int ->
  ?width:int ->
  ?off:int ->
  ?len:int ->
  string ->
  t
(** [intern pool ?width_method ?tab_width ?width ?off ?len str] creates a single
    glyph from [str].

    Returns the empty glyph (0) for control characters or zero-width sequences.
    Tab characters use [tab_width] (default 2) for display width. Providing
    [width] skips width calculation for performance.

    {b Note}: Multi-byte strings are stored as a single glyph with cumulative
    width. For example, [intern pool "ab"] creates one glyph with width 2. Use
    [encode] when you need per-character segmentation.

    {b UTF-8 handling}: Invalid byte sequences are replaced with U+FFFD
    (replacement character). *)

val iter_grapheme_info :
  width_method:width_method ->
  tab_width:int ->
  string ->
  (offset:int -> len:int -> width:int -> unit) ->
  unit
(** [iter_grapheme_info ~width_method ~tab_width str f] walks grapheme clusters
    in [str] and calls [f] with their byte [offset], byte [len], and display
    [width].

    Uses the same width calculation and ZWJ handling as [encode]. Graphemes
    whose width resolves to 0 (control / zero-width sequences) are skipped.

    {b UTF-8 handling}: Invalid byte sequences are treated as individual
    replacement characters (U+FFFD). *)

val intern_char : pool -> int -> t
(** [intern_char pool codepoint] creates a glyph from a single Unicode
    codepoint.

    Returns the empty glyph (0) for control or zero-width codepoints. *)

(** {1 Introspection} *)

val width : ?tab_width:int -> t -> int
(** [width ?tab_width glyph] returns the display width of [glyph].

    Returns 1 for ASCII Simple glyphs. For Complex glyphs (Start or
    Continuation), returns the full cluster width (1-4). Tab characters use
    [tab_width] (default 2). *)

val is_start : t -> bool
(** [is_start glyph] returns [true] if [glyph] is the start of a character
    (Simple or Complex Start), [false] otherwise. *)

val is_continuation : t -> bool
(** [is_continuation glyph] returns [true] if [glyph] is a placeholder for a
    multi-column character, [false] otherwise. *)

val is_empty : t -> bool
(** [is_empty glyph] returns [true] if [glyph] is 0.

    The empty glyph represents control characters, zero-width sequences, and
    U+0000. *)

val is_simple : t -> bool
(** [is_simple glyph] returns [true] if [glyph] is a direct ASCII character that
    requires no pool lookup, [false] otherwise. *)

(** {1 Data Retrieval} *)

val blit : pool -> t -> bytes -> int -> int
(** [blit pool glyph buf offset] copies UTF-8 bytes of [glyph] to [buf] at
    [offset].

    Returns the number of bytes written. The empty glyph (0) is treated as
    U+0000 and writes 1 byte. Returns 0 for stale Complex IDs or insufficient
    buffer space. *)

val copy : pool -> t -> pool -> t
(** [copy src_pool glyph dst_pool] copies [glyph] from [src_pool] to [dst_pool].

    Returns [glyph] unchanged if it is a Simple glyph, or a new Complex glyph
    interned in [dst_pool]. Returns the empty glyph (0) for stale IDs. *)

val length : pool -> t -> int
(** [length pool glyph] returns the byte length of the UTF-8 sequence for
    [glyph].

    Returns 1 for Simple glyphs (including the empty glyph, which encodes as
    U+0000), the actual byte length for Complex glyphs, and 0 for stale Complex
    IDs. *)

val to_string : pool -> t -> string
(** [to_string pool glyph] allocates a new string containing the UTF-8 sequence
    for [glyph].

    Returns a single-character string for Simple glyphs (including ["\000"] for
    the empty glyph), the full sequence for Complex glyphs, and an empty string
    for stale Complex IDs. *)

(** {1 Utilities} *)

val iter_graphemes : (offset:int -> len:int -> unit) -> string -> unit
(** [iter_graphemes f str] calls [f ~offset ~len] for each grapheme cluster in
    [str].

    Uses full UAX #29 segmentation (same as [width_method:`Unicode]). Useful for
    counting or indexing user-perceived characters without allocating.

    {b UTF-8 handling}: Invalid byte sequences are treated as individual
    replacement characters (U+FFFD). *)

val measure : width_method:width_method -> tab_width:int -> string -> int
(** [measure ~width_method ~tab_width str] calculates the total display width of
    [str].

    Control characters contribute 0 to the total width.

    {b UTF-8 handling}: Invalid byte sequences are replaced with U+FFFD
    (replacement character), each contributing width 1. *)

(** {1 Text Segmentation}

    Line break and word wrap break detection. *)

val iter_wrap_breaks :
  ?width_method:width_method ->
  (byte_offset:int -> grapheme_offset:int -> unit) ->
  string ->
  unit
(** [iter_wrap_breaks ?width_method f s] computes word-wrap break opportunities
    in [s] and calls [f ~byte_offset ~grapheme_offset] for each break, in order
    from start to end.

    - [byte_offset]: 0-indexed UTF-8 byte position pointing to the start of the
      next grapheme after the break.
    - [grapheme_offset]: 0-indexed count of grapheme clusters preceding this
      break.

    Breaks occur after graphemes containing:
    - ASCII: space, tab, hyphen, path separators, punctuation, and brackets
    - Unicode: NBSP, ZWSP, soft hyphen, typographic spaces, and other space-like
      codepoints

    The optional [width_method] argument controls grapheme boundary detection:
    [`Unicode] treats ZWJ sequences as single graphemes, [`No_zwj] breaks them
    apart.

    Example:
    {[
      iter_wrap_breaks
        (fun ~byte_offset ~grapheme_offset ->
          Printf.printf "break at byte %d, grapheme %d\n" byte_offset
            grapheme_offset)
        "hi 👋!"
    ]} *)

type line_break_kind = [ `LF | `CR | `CRLF ]
(** Kind of line terminator. *)

val iter_line_breaks :
  (pos:int -> kind:line_break_kind -> unit) -> string -> unit
(** [iter_line_breaks f s] detects line terminators (LF U+000A, CR U+000D, and
    CRLF sequences) in [s] and calls [f ~pos ~kind] for each, in order from
    start to end.

    - [pos]: 0-indexed byte position of the line terminator. For [`CRLF], this
      is the index of the LF byte; for [`LF] and [`CR], the respective byte.
    - [kind]: The type of line terminator.

    CRLF sequences are reported once as [`CRLF] at the LF position, not as
    separate CR and LF breaks.

    {[
      iter_line_breaks
        (fun ~pos ~kind ->
          let s =
            match kind with `LF -> "LF" | `CR -> "CR" | `CRLF -> "CRLF"
          in
          Printf.printf "%s at %d\n" s pos)
        "a\r\nb\n"
      (* Output: CRLF at 2 *)
      (* Output: LF at 4 *)
    ]} *)
