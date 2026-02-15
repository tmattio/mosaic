(** High-performance, reference-counted Unicode glyph storage.

    This library provides a packed integer representation of visual characters
    ("glyphs") optimized for terminal emulators. It distinguishes between:

    - {b Simple}: Single Unicode scalars (U+0000 - U+10FFFF) stored directly.
      Zero allocation, zero lookup.
    - {b Complex}: Multi-codepoint grapheme clusters interned in a pool. Stored
      as Pool Index + Generation ID + Width + Flags.

    {1 Quick Start}

    Create a pool, encode a string, and process glyphs via callback:

    {[
      let pool = Pool.create () in
      Pool.encode pool ~width_method:`Unicode ~tab_width:2
        (fun glyph -> Printf.printf "%s " (Pool.to_string pool glyph))
        "Hello 👋 World"
      (* Output: H e l l o 👋 W o r l d *)
    ]}

    Multi-column characters emit one Start glyph followed by Continuation
    glyphs. Control and zero-width sequences are skipped.

    {1 Memory Safety}

    The pool uses manual reference counting with automatic slot recycling. To
    prevent "use-after-free" errors where a Grid might hold an old ID that
    points to a recycled slot containing different data, IDs include a
    {b Generation Counter}. Accessing a glyph with a stale generation returns
    safe defaults ({!empty}/zero width).

    {1 Width Calculation}

    Implements robust width calculation logic (UAX #11), correctly handling:
    - ZWJ sequences (Emoji composition)
    - Regional Indicators (Flags)
    - Variation Selectors
    - Skin tone modifiers *)

type t = private int
(** A packed integer representing a visual glyph.

    Always unboxed (immediate value) with zero allocation overhead. The type is
    [private] to prevent construction of invalid glyph values — use {!of_uchar},
    {!Pool.intern}, {!Pool.encode}, {!empty}, or {!space} to create glyphs.
    Reading the integer representation (e.g., for storage in Bigarray) is
    permitted; use {!unsafe_of_int} when loading from external storage.

    {b Encoding.} Currently uses a 63-bit layout: type flags (2 bits),
    left/right extent (4 bits), generation counter (7 bits), and pool index (18
    bits). Single Unicode scalars (U+0000 - U+10FFFF) are stored directly as
    Simple glyphs; multi-codepoint clusters are interned in the pool as Complex
    glyphs. *)

type width_method = [ `Unicode | `Wcwidth | `No_zwj ]
(** Width calculation method for grapheme clusters.

    - [`Unicode]: Full UAX #29 segmentation with ZWJ emoji composition. Use for
      correct emoji and flag rendering.
    - [`Wcwidth]: Per-codepoint width calculation without grapheme clustering.
      Use for legacy compatibility with wcwidth-based systems.
    - [`No_zwj]: UAX #29 segmentation that forces a break after ZWJ (no emoji
      ZWJ sequences), but keeps the full grapheme-aware width logic (RI pairs,
      VS16, Indic virama). *)

type line_break_kind = [ `LF | `CR | `CRLF ]
(** Kind of line terminator. *)

(** {1 Constants} *)

val empty : t
(** The empty glyph ([0]). Represents control characters, zero-width sequences,
    and U+0000. This is the only glyph for which {!is_empty} returns [true]. *)

val space : t
(** The space glyph (U+0020, width 1). Used as the default "blank cell" content
    in terminal grids. *)

(** {1 Creation (pool-free)} *)

val of_uchar : Uchar.t -> t
(** [of_uchar u] creates a glyph from a single Unicode codepoint.

    Returns {!empty} for control or zero-width codepoints. Single codepoints are
    stored directly in the packed integer with no pool allocation. *)

(** {1 Predicates} *)

val is_empty : t -> bool
(** [is_empty glyph] returns [true] if [glyph] is {!empty}.

    The empty glyph represents control characters, zero-width sequences, and
    U+0000. *)

val is_inline : t -> bool
(** [is_inline glyph] returns [true] if [glyph] needs no pool lookup.
    Performance hint for skipping reference counting on inline glyphs. *)

val is_start : t -> bool
(** [is_start glyph] returns [true] if [glyph] is the start of a character
    (Simple or Complex Start), [false] otherwise. *)

val is_continuation : t -> bool
(** [is_continuation glyph] returns [true] if [glyph] is a placeholder for a
    multi-column character, [false] otherwise. *)

val is_complex : t -> bool
(** [is_complex glyph] returns [true] if [glyph] is a pool-backed grapheme
    (Complex Start or Complex Continuation), [false] otherwise. *)

(** {1 Properties} *)

val width : ?tab_width:int -> t -> int
(** [width ?tab_width glyph] returns the display width of [glyph].

    Returns 1 for ASCII Simple glyphs. For Complex glyphs (Start or
    Continuation), returns the full cluster width (1-4). Tab characters use
    [tab_width] (default 2). *)

val cell_width : t -> int
(** [cell_width glyph] returns the display width of a single cell.

    Returns 0 for {!empty} and continuation cells. For start cells (Simple or
    Complex Start), returns the character's display width (1 for most
    characters, 2 for wide CJK/emoji). Tab glyphs return 1.

    Unlike {!width}, which returns the full cluster width for continuation
    cells, this returns 0 for continuations (they occupy no additional columns
    beyond the start cell). *)

val left_extent : t -> int
(** [left_extent glyph] returns the left extent of a continuation glyph
    (distance to the start cell). Returns 0 for Simple and Complex Start glyphs.
*)

val right_extent : t -> int
(** [right_extent glyph] returns the right extent of a glyph (distance to the
    rightmost continuation cell). For Complex Start, this is [width - 1]. *)

val codepoint : t -> int
(** [codepoint glyph] extracts the Unicode codepoint from a Simple glyph.

    Returns the raw codepoint (U+0000 - U+10FFFF). Behavior is undefined for
    Complex glyphs. *)

val pool_payload : t -> int
(** [pool_payload glyph] extracts the pool identity (index + generation bits)
    from a Complex glyph. Used as a deduplication key in grapheme trackers.

    Returns 0 for Simple glyphs. *)

val pool_index : t -> int
(** [pool_index glyph] extracts the pool slot index from a Complex glyph.

    Returns 0 for Simple glyphs and for continuation cells of Simple wide
    characters. *)

(** {1 Construction} *)

val make_continuation : code:t -> left:int -> right:int -> t
(** [make_continuation ~code ~left ~right] creates a continuation cell that
    references the same pool entry as [code] with the given left/right extents.

    [left] and [right] are clamped to \[0..3\]. If [code] is a Simple glyph, the
    continuation carries no pool reference (index 0). *)

(** {1 Converting} *)

val to_int : t -> int
(** [to_int glyph] returns the raw integer representation of [glyph].

    Use when storing glyphs in Bigarray or other integer-based containers. *)

val unsafe_of_int : int -> t
(** [unsafe_of_int n] interprets [n] as a glyph without validation.

    {b Unsafe.} The caller must ensure [n] is a valid glyph encoding (e.g.,
    produced by {!to_int} or read from a Bigarray that stores glyphs). Using an
    invalid integer may cause incorrect behavior in pool operations. *)

(** {1 Pool}

    Storage context for interned grapheme clusters. Pools manage the lifecycle
    of complex glyphs (multi-codepoint grapheme clusters) through manual
    reference counting with generation-based use-after-free protection.

    NOT thread-safe. Use separate pools per thread or external synchronization.
*)

module Pool : sig
  type glyph := t

  type t
  (** A pool for interning complex grapheme clusters. *)

  val create : unit -> t
  (** [create ()] creates a new empty pool with initial capacity for 4096
      glyphs.

      @raise Failure if the pool exceeds 262K interned graphemes. *)

  val clear : t -> unit
  (** [clear pool] resets the pool, invalidating ALL existing glyph references.

      {b Important.} Glyphs must not be used after [clear]; behavior is
      undefined because IDs may be recycled with the same generation. Does not
      free memory, only resets internal cursors for reuse. *)

  val incref : t -> glyph -> unit
  (** [incref pool glyph] increments the reference count for [glyph].

      No-op for Simple glyphs or stale Complex glyphs with mismatched
      generations. *)

  val decref : t -> glyph -> unit
  (** [decref pool glyph] decrements the reference count for [glyph].

      When count reaches 0, the slot is recycled and its generation is
      incremented. No-op for Simple glyphs or stale Complex glyphs. *)

  val intern :
    t -> ?width_method:width_method -> ?tab_width:int -> string -> glyph
  (** [intern pool str] creates a single glyph from [str].

      Returns {!empty} for control characters or zero-width sequences. Tab
      characters use [tab_width] (default 2) for display width.

      Defaults: [width_method = `Unicode], [tab_width = 2].

      {b Note.} Multi-byte strings are stored as a single glyph with cumulative
      width. For example, [intern pool "ab"] creates one glyph with width 2. Use
      {!encode} when you need per-character segmentation.

      {b UTF-8 handling.} Invalid byte sequences are replaced with U+FFFD
      (replacement character).

      @raise Failure if the pool exceeds 262K interned graphemes. *)

  val intern_sub :
    t ->
    ?width_method:width_method ->
    ?tab_width:int ->
    string ->
    pos:int ->
    len:int ->
    width:int ->
    glyph
  (** [intern_sub pool str ~pos ~len ~width] creates a glyph from the substring
      [str.[pos] .. str.[pos + len - 1]] with precomputed display [width].

      This is a performance variant of {!intern} for hot paths where the width
      and substring bounds are already known, avoiding redundant width
      calculation and [String.sub] allocation.

      @raise Failure if the pool exceeds 262K interned graphemes. *)

  val encode :
    t ->
    ?width_method:width_method ->
    ?tab_width:int ->
    (glyph -> unit) ->
    string ->
    unit
  (** [encode pool ?width_method ?tab_width f str] streams glyphs from [str] via
      callback [f].

      Defaults: [width_method = `Unicode], [tab_width = 2].

      The callback [(fun glyph -> ...)] is invoked inline for each glyph in
      string order. Multi-column characters emit one Start glyph followed by
      [(width - 1)] Continuation glyphs. Control characters and zero-width
      sequences are skipped.

      Single codepoints become Simple glyphs immediately. Multi-codepoint
      grapheme clusters are interned and become Complex glyphs.

      {b UTF-8 handling.} Invalid byte sequences are replaced with U+FFFD
      (replacement character) per Unicode best practices. Each invalid byte
      consumes exactly one byte and produces one replacement glyph.

      @raise Failure if the pool exceeds 262K interned graphemes. *)

  val iter_grapheme_info :
    ?width_method:width_method ->
    ?tab_width:int ->
    (offset:int -> len:int -> width:int -> unit) ->
    string ->
    unit
  (** [iter_grapheme_info ?width_method ?tab_width f str] walks grapheme
      clusters in [str] and calls [f] with their byte [offset], byte [len], and
      display [width].

      Defaults: [width_method = `Unicode], [tab_width = 2].

      Uses the same width calculation and ZWJ handling as {!encode}. Graphemes
      whose width resolves to 0 (control / zero-width sequences) are skipped.

      {b UTF-8 handling.} Invalid byte sequences are treated as individual
      replacement characters (U+FFFD). *)

  val length : t -> glyph -> int
  (** [length pool glyph] returns the byte length of the UTF-8 sequence for
      [glyph].

      Returns 1 for Simple glyphs (including {!empty}, which encodes as U+0000),
      the actual byte length for Complex glyphs, and 0 for stale Complex IDs. *)

  val blit : t -> glyph -> bytes -> pos:int -> int
  (** [blit pool glyph buf ~pos] copies UTF-8 bytes of [glyph] to [buf] at
      [pos].

      Returns the number of bytes written. {!empty} is treated as U+0000 and
      writes 1 byte. Returns 0 for stale Complex IDs or insufficient buffer
      space. *)

  val copy : src:t -> glyph -> dst:t -> glyph
  (** [copy ~src glyph ~dst] copies [glyph] from [src] pool to [dst] pool.

      Returns [glyph] unchanged if it is a Simple glyph (single codepoint), or a
      new Complex glyph interned in [dst]. Returns {!empty} for stale IDs.

      {b Important.} Glyphs obtained from one pool must not be used with a
      different pool. Use [copy] to transfer glyphs between pools. *)

  val to_string : t -> glyph -> string
  (** [to_string pool glyph] allocates a new string containing the UTF-8
      sequence for [glyph].

      Returns a single-character string for Simple glyphs (including ["\000"]
      for {!empty}), the full sequence for Complex glyphs, and an empty string
      for stale Complex IDs. *)
end

(** {1 String utilities}

    Pool-free string measurement and iteration functions. These work on raw
    strings and do not require a glyph pool. *)

module String : sig
  val measure : ?width_method:width_method -> ?tab_width:int -> string -> int
  (** [measure ?width_method ?tab_width str] calculates the total display width
      of [str].

      Defaults: [width_method = `Unicode], [tab_width = 2].

      Control characters contribute 0 to the total width.

      {b UTF-8 handling.} Invalid byte sequences are replaced with U+FFFD
      (replacement character), each contributing width 1. *)

  val grapheme_count : string -> int
  (** [grapheme_count str] returns the number of user-perceived characters
      (grapheme clusters) in [str].

      Uses full UAX #29 segmentation. *)

  val iter_graphemes : (offset:int -> len:int -> unit) -> string -> unit
  (** [iter_graphemes f str] calls [f ~offset ~len] for each grapheme cluster in
      [str].

      Uses full UAX #29 segmentation (same as [width_method:`Unicode]). Useful
      for counting or indexing user-perceived characters without allocating.

      {b UTF-8 handling.} Invalid byte sequences are treated as individual
      replacement characters (U+FFFD). *)

  val iter_wrap_breaks :
    ?width_method:width_method ->
    (byte_offset:int -> grapheme_offset:int -> unit) ->
    string ->
    unit
  (** [iter_wrap_breaks ?width_method f s] computes word-wrap break
      opportunities in [s] and calls [f ~byte_offset ~grapheme_offset] for each
      break, in order from start to end.

      - [byte_offset]: 0-indexed UTF-8 byte position pointing to the start of
        the next grapheme after the break.
      - [grapheme_offset]: 0-indexed count of grapheme clusters preceding this
        break.

      Breaks occur after graphemes containing:
      - ASCII: space, tab, hyphen, path separators, punctuation, and brackets
      - Unicode: NBSP, ZWSP, soft hyphen, typographic spaces, and other
        space-like codepoints

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
end
