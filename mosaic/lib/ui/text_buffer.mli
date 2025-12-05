(** Styled text buffer with line wrapping and syntax highlighting.

    [Text_buffer] is an efficient, styled multi-line text container optimized
    for terminal UIs. It stores text as sequences of styled chunks, tracks
    logical and visual line boundaries, and supports highlight overlays for
    syntax coloring and selections.

    {1 Overview}

    A buffer maintains:
    - Text content as Unicode grapheme clusters (via {!Glyph})
    - Per-grapheme styling (foreground, background, attributes, links)
    - Logical line metadata (newline-delimited lines)
    - Virtual line metadata (wrapped lines)
    - Highlight overlays (syntax highlighting, selections)

    All operations preserve UTF-8 correctness and handle multi-column characters
    (emoji, CJK) correctly.

    {1 Usage Basics}

    Create a buffer and write styled chunks:
    {[
      let buffer =
        Text_buffer.create ~capacity:1024 ~width_method:`Unicode ()
      in
      let chunk =
        Text_buffer.Chunk.
          {
            text = Bytes.of_string "Hello, world!";
            fg = Some (Ansi.Color.of_rgb 255 255 255);
            bg = None;
            attrs = Ansi.Attr.empty;
            link = None;
          }
      in
      ignore (Text_buffer.write_chunk buffer chunk);
      Text_buffer.finalise buffer
    ]}

    Build virtual (wrapped) lines for rendering:
    {[
      let snapshot =
        Text_buffer.build_virtual_lines buffer ~wrap_mode:`Word
          ~wrap_width:(Some 80)
      in
      Array.iter
        (fun vline ->
          Printf.printf "Line %d: cols %d-%d\n" vline.line_index
            vline.source_col_offset
            (vline.source_col_offset + vline.width))
        snapshot.lines
    ]}

    {1 Key Concepts}

    {2 Logical vs Virtual Lines}

    - {b Logical lines}: Newline-delimited lines in the source text.
    - {b Virtual lines}: Visual lines after wrapping. One logical line may span
      multiple virtual lines.

    [line_info] and [logical_line_info] report character offsets and widths for
    logical lines. [build_virtual_lines] computes virtual lines from logical
    lines based on wrapping parameters.

    {2 Character Offsets}

    Line starts in {!line_info} and {!logical_line_info} are global character
    offsets across all logical lines. A newline contributes a weight of 1
    between logical lines. For example, with text ["abc\ndef"], line 0 starts at
    offset 0 (width 3), and line 1 starts at offset 4 (3 + 1 for newline).

    {2 Styles and Highlights}

    Base styles are set per chunk at write time. Highlights overlay additional
    styles (e.g., for syntax highlighting or selections) without modifying chunk
    data. Highlights are resolved by priority; higher priority wins. For equal
    priorities, the most recently added highlight takes precedence.

    Use transactions ({!start_highlights_transaction},
    {!end_highlights_transaction}) to batch highlight updates and defer
    expensive style span rebuilds.

    {2 Tab Handling}

    Tabs are expanded to the next tab stop (multiples of [tab_width]). The
    [tab_indicator] controls visual representation (e.g.,
    [U+2192 RIGHTWARDS ARROW]). Tab expansion affects visual width calculations
    and wrapping.

    {1 Performance and Constraints}

    - Writing chunks is O(n) in chunk length; uses grapheme clustering for
      correct Unicode segmentation.
    - Finalizing styles is O(buffer length); results are cached until the buffer
      is modified.
    - Virtual line computation is O(buffer length); results are cached per
      (wrap_mode, wrap_width) pair until the buffer is modified.
    - Highlight transactions batch span rebuilds; use for bulk highlight
      updates.

    Width calculations use {!Glyph.width_method} set at creation or via
    {!set_width_method}. Changing width method invalidates caches.

    {1 Standards and Compatibility}

    Line breaking follows UAX #14 (Unicode Line Breaking Algorithm) for word
    wrap. Character width follows UAX #11 (East Asian Width) when using
    [`Unicode] or [`Wcwidth] modes.

    CRLF and CR-only line endings are normalized to LF internally. *)

(** {1 Types} *)

type wrap_mode = [ `Char | `Word ]
(** Wrapping mode for virtual lines.

    - [`Char]: Break at any character boundary when exceeding wrap width.
    - [`Word]: Break at word boundaries (spaces, punctuation) when possible;
      fall back to character breaks if no word boundary is available. *)

type line_info = { starts : int array; widths : int array; max_width : int }
(** Metadata for logical or virtual lines.

    - [starts]: Global character offset for each line start. Offsets account for
      newline weights (1 per newline).
    - [widths]: Visual width of each line in terminal columns.
    - [max_width]: Maximum width across all lines. *)

type style_snapshot = {
  fg : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  fg_mask :
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
  bg : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  bg_mask :
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
  attrs : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  links : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
}
(** Low-level snapshot of resolved styles.

    Colors are stored as RGBA float32 arrays (4 channels per color). Masks
    indicate presence ([1]) or absence ([0]) of explicit colors. [attrs] stores
    packed {!Ansi.Attr.t} values. [links] stores hyperlink indices ([-1] for no
    link). *)

type highlight = {
  col_start : int;
  col_end : int;
  style : Ansi.Style.t;
  priority : int;
  ref_id : int;
}
(** Highlight overlay for a column range on a single line.

    - [col_start], [col_end]: Half-open column range [[col_start, col_end)].
    - [style]: Style to apply over this range.
    - [priority]: Priority for resolving overlaps. Higher values win.
    - [ref_id]: Identifier for batch removal via {!remove_highlights_by_ref}. *)

type style_span = { col : int; next_col : int; style : Ansi.Style.t option }
(** Resolved style span for a column range.

    [style] is [None] if no highlight applies, or [Some style] for the
    highest-priority highlight covering [[col, next_col)]. *)

type t
(** The buffer type. *)

(** {1:modules Nested Modules} *)

module Selection : sig
  type linear = { start : int; stop : int; style : Ansi.Style.t }
  (** Linear selection over a global character range [[start, stop)]. *)

  type local = {
    anchor_x : int;
    anchor_y : int;
    focus_x : int;
    focus_y : int;
    style : Ansi.Style.t;
  }
  (** Local (2D) selection with anchor and focus points in (x, y) coordinates.
  *)

  type t = None | Linear of linear | Local of local  (** Selection state. *)
end

module Chunk : sig
  type t = {
    text : bytes;
    fg : Ansi.Color.t option;
    bg : Ansi.Color.t option;
    attrs : Ansi.Attr.t;
    link : string option;
  }
  (** Styled text chunk for writing to the buffer.

      - [text]: UTF-8 encoded bytes. CRLF and CR-only line endings are
        normalized to LF.
      - [fg], [bg]: Foreground and background colors. [None] uses buffer
        defaults.
      - [attrs]: Text attributes (bold, italic, underline, etc.).
      - [link]: Hyperlink URL. [None] for no hyperlink. *)
end

module Virtual_line : sig
  type t = {
    line_index : int;
    start_index : int;
    length : int;
    width : int;
    char_offset : int;
    source_col_offset : int;
  }
  (** Metadata for a single virtual (wrapped) line.

      - [line_index]: Index of the source logical line.
      - [start_index]: Global character offset in the buffer.
      - [length]: Number of characters in this virtual line.
      - [width]: Visual width in terminal columns.
      - [char_offset]: Cumulative character offset from buffer start (includes
        newline weights).
      - [source_col_offset]: Column offset within the source logical line. *)
end

type virtual_lines_snapshot = {
  lines : Virtual_line.t array;
  line_starts : int array;
  line_widths : int array;
  line_first_vline : int array;
  line_vline_counts : int array;
}
(** Snapshot of virtual line metadata.

    - [lines]: All virtual lines in order.
    - [line_starts]: Character offsets for each virtual line (same as
      [lines.(i).char_offset]).
    - [line_widths]: Visual widths for each virtual line.
    - [line_first_vline]: Index of first virtual line for each logical line.
    - [line_vline_counts]: Count of virtual lines per logical line. *)

(** {1:lifecycle Lifecycle} *)

val create :
  ?glyph_pool:Glyph.pool ->
  capacity:int ->
  width_method:Glyph.width_method ->
  unit ->
  t
(** [create ~capacity ~width_method ()] creates an empty buffer.

    - [glyph_pool]: Optional shared glyph pool. If omitted, creates a new pool.
    - [capacity]: Initial capacity for grapheme storage. Grows automatically.
    - [width_method]: Width calculation method for Unicode graphemes.

    @raise Invalid_argument if [capacity < 1]. *)

val reset : t -> unit
(** [reset t] clears all content, highlights, and metadata.

    Invalidates existing glyph references and resets internal state. Cached
    virtual lines and style spans are discarded. Does not free memory; capacity
    remains unchanged. *)

(** {1:writing Writing Content} *)

val write_chunk : t -> Chunk.t -> int
(** [write_chunk t chunk] appends [chunk] to the buffer and returns the number
    of graphemes written.

    The chunk is encoded using the current [width_method] and [tab_width].
    Graphemes are segmented per UAX #29. CRLF and CR-only line endings are
    normalized to LF.

    Modifying the buffer invalidates cached virtual lines and style spans. *)

val finalise : t -> unit
(** [finalise t] rebuilds internal styles if dirty.

    Call before accessing {!drawing_chars}, {!drawing_styles}, or
    {!drawing_widths} to ensure consistent state. Idempotent; safe to call
    multiple times. *)

(** {1:configuration Configuration} *)

val set_width_method : t -> Glyph.width_method -> unit
(** [set_width_method t method_] updates the width computation method.

    Existing encoded widths are not recomputed. Prefer setting the method before
    populating the buffer to avoid inconsistent widths. *)

val set_tab_width : t -> int -> unit
(** [set_tab_width t width] sets the tab stop interval.

    [width] is clamped to a minimum of 2 and rounded up to the nearest even
    integer for alignment. Changes mark the buffer dirty. *)

val tab_width : t -> int
(** [tab_width t] returns the current tab width. *)

val set_tab_indicator : t -> int32 option -> unit
(** [set_tab_indicator t code] sets the visual indicator for tab characters.

    [code] is a Unicode codepoint (e.g., [Some 0x2192l] for
    [U+2192 RIGHTWARDS ARROW]). [None] disables the indicator. *)

val set_tab_indicator_color : t -> Ansi.Color.t option -> unit
(** [set_tab_indicator_color t color] sets the color for tab indicators. *)

val tab_indicator : t -> int32 option
(** [tab_indicator t] returns the current tab indicator codepoint, or [None]. *)

val tab_indicator_color : t -> Ansi.Color.t option
(** [tab_indicator_color t] returns the current tab indicator color, or [None].
*)

val set_default_fg : t -> Ansi.Color.t option -> unit
(** [set_default_fg t color] sets the default foreground color.

    Used when chunks do not specify a foreground color. Marks styles dirty. *)

val set_default_bg : t -> Ansi.Color.t option -> unit
(** [set_default_bg t color] sets the default background color.

    Used when chunks do not specify a background color. Marks styles dirty. *)

val set_default_attrs : t -> Ansi.Attr.t option -> unit
(** [set_default_attrs t attrs] sets the default text attributes.

    Used when chunks do not specify attributes. Marks styles dirty. *)

(** {1:metadata Metadata and Queries} *)

val length : t -> int
(** [length t] returns the number of graphemes in the buffer. *)

val line_count : t -> int
(** [line_count t] returns the number of logical lines (newline-delimited). *)

val version : t -> int
(** [version t] returns a monotonic version counter.

    Incremented whenever the buffer is modified. Useful for cache invalidation.
*)

val line_info : t -> line_info
(** [line_info t] returns metadata for logical lines.

    Equivalent to {!logical_line_info}. Line starts are global character offsets
    with newline weights. *)

val logical_line_info : t -> line_info
(** [logical_line_info t] returns metadata for logical (unwrapped) lines.

    Line starts are global character offsets across all logical lines, with
    newline weights of 1. For example, text ["abc\ndef"] yields starts
    [[|0; 4|]] (line 0 at 0, line 1 at 0 + 3 + 1). *)

val build_virtual_lines :
  t -> wrap_mode:wrap_mode -> wrap_width:int option -> virtual_lines_snapshot
(** [build_virtual_lines t ~wrap_mode ~wrap_width] computes virtual (wrapped)
    lines.

    - [wrap_mode]: [`Char] for character wrapping, [`Word] for word wrapping.
    - [wrap_width]: Maximum line width in columns. [None] disables wrapping.

    Results are cached per (wrap_mode, wrap_width) pair. Cache invalidates when
    the buffer is modified.

    Tab expansion affects wrapping boundaries. Word wrapping respects UAX #14
    break opportunities. *)

(** {1:highlights Highlights} *)

val start_highlights_transaction : t -> unit
(** [start_highlights_transaction t] begins a highlight transaction.

    Transactions defer expensive style span rebuilds until
    {!end_highlights_transaction} is called. Transactions nest; rebuilds occur
    only when the outermost transaction ends. *)

val end_highlights_transaction : t -> unit
(** [end_highlights_transaction t] ends a highlight transaction.

    Rebuilds dirty style spans if this is the outermost transaction. No-op if no
    transaction is active. *)

val add_highlight :
  t ->
  line_idx:int ->
  col_start:int ->
  col_end:int ->
  style:Ansi.Style.t ->
  priority:int ->
  ref_id:int ->
  unit
(** [add_highlight t ~line_idx ~col_start ~col_end ~style ~priority ~ref_id]
    adds a highlight to line [line_idx].

    - [line_idx]: Logical line index.
    - [col_start], [col_end]: Half-open column range [[col_start, col_end)].
      No-op if [col_end <= col_start].
    - [style]: Style to apply.
    - [priority]: Priority for overlap resolution. Higher values win.
    - [ref_id]: Identifier for batch removal via {!remove_highlights_by_ref}.

    If a transaction is active, span rebuild is deferred. Otherwise, spans are
    rebuilt immediately. *)

val add_highlight_by_char_range :
  t ->
  char_start:int ->
  char_end:int ->
  style:Ansi.Style.t ->
  priority:int ->
  ref_id:int ->
  unit
(** [add_highlight_by_char_range t ~char_start ~char_end ~style ~priority
     ~ref_id] adds a highlight over a global character range.

    Character offsets are converted to line and column coordinates. Currently
    only supports single-line ranges; multi-line ranges are ignored. *)

val remove_highlights_by_ref : t -> ref_id:int -> unit
(** [remove_highlights_by_ref t ~ref_id] removes all highlights with the given
    [ref_id].

    If a transaction is active, span rebuilds are deferred. Otherwise, spans are
    rebuilt immediately for affected lines. *)

val clear_line_highlights : t -> line_idx:int -> unit
(** [clear_line_highlights t ~line_idx] removes all highlights from line
    [line_idx].

    Clears both highlights and resolved style spans. *)

val clear_all_highlights : t -> unit
(** [clear_all_highlights t] removes all highlights and resets transaction
    state.

    Clears highlights, style spans, and dirty line tracking. Resets transaction
    depth to 0. *)

val line_spans : t -> line_idx:int -> style_span list
(** [line_spans t ~line_idx] returns resolved style spans for line [line_idx].

    Spans are sorted by column. Returns [[]] if [line_idx] is out of bounds or
    has no highlights. *)

(** {1:drawing Drawing Data} *)

val drawing_chars :
  t -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
(** [drawing_chars t] returns the character codes array.

    Calls {!finalise} if needed. Array length is {!length}. Codes are either
    Unicode codepoints (< 128 or standard), tab placeholders ([9l]), newlines
    ([10l]), or glyph pool IDs. *)

val drawing_styles : t -> style_snapshot
(** [drawing_styles t] returns the resolved styles snapshot.

    Calls {!finalise} if needed. Styles reflect chunk styles merged with buffer
    defaults. *)

val drawing_widths :
  t -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** [drawing_widths t] returns the visual widths array.

    Calls {!finalise} if needed. Array length is {!length}. Widths are in
    terminal columns. Newlines have width 0; tabs have placeholder width 1
    (expand to tab stops at render time). *)

val link_at_index : t -> int -> string option
(** [link_at_index t idx] resolves a hyperlink index from {!drawing_styles}.

    Returns [Some url] if [idx] is valid and points to a link, [None] otherwise.
*)

(** {1:view View Snapshot} *)

module View : sig
  type snapshot
  (** Immutable snapshot of buffer contents and styles. *)

  val create : t -> snapshot
  (** [create buffer] finalizes [buffer] if needed and returns a snapshot view.

      The snapshot provides fast, indexed access to characters, widths, and
      styles without triggering rebuilds. *)

  val code : snapshot -> int -> int32
  (** [code view idx] returns the character code at index [idx].

      Codes are Unicode codepoints, tab placeholders ([9l]), newlines ([10l]),
      or glyph pool IDs.

      @raise Invalid_argument if [idx] is out of bounds. *)

  val width : snapshot -> int -> int
  (** [width view idx] returns the visual width at index [idx].

      Widths are in terminal columns. Newlines have width 0; tabs have
      placeholder width 1.

      @raise Invalid_argument if [idx] is out of bounds. *)

  val attrs : snapshot -> int -> Ansi.Attr.t
  (** [attrs view idx] returns the text attributes at index [idx].

      @raise Invalid_argument if [idx] is out of bounds. *)

  val fg_opt : snapshot -> int -> Ansi.Color.t option
  (** [fg_opt view idx] returns the foreground color at index [idx], or [None]
      if unset.

      @raise Invalid_argument if [idx] is out of bounds. *)

  val bg_opt : snapshot -> int -> Ansi.Color.t option
  (** [bg_opt view idx] returns the background color at index [idx], or [None]
      if unset.

      @raise Invalid_argument if [idx] is out of bounds. *)

  val fg_with_default : snapshot -> int -> Ansi.Color.t -> Ansi.Color.t
  (** [fg_with_default view idx default] returns the foreground color at index
      [idx], or [default] if unset.

      @raise Invalid_argument if [idx] is out of bounds. *)

  val bg_with_default : snapshot -> int -> Ansi.Color.t -> Ansi.Color.t
  (** [bg_with_default view idx default] returns the background color at index
      [idx], or [default] if unset.

      @raise Invalid_argument if [idx] is out of bounds. *)

  val raw_link : snapshot -> int -> int32
  (** [raw_link view idx] returns the raw hyperlink index at index [idx].

      Returns [-1l] if no link is present. Use {!link_at_index} to resolve
      indices to URLs.

      @raise Invalid_argument if [idx] is out of bounds. *)
end

(** {1:text_extraction Text Extraction} *)

val get_plain_text : t -> string
(** [get_plain_text t] extracts all text as a plain UTF-8 string.

    Newlines are preserved; tabs are emitted as tab characters. Styling is
    discarded. Grapheme clusters are converted to UTF-8. *)

val get_text_range : t -> start:int -> stop:int -> string
(** [get_text_range t ~start ~stop] extracts text from the half-open range
    [[start, stop)].

    [start] and [stop] are clamped to [[0, length t]]. Returns empty string if
    [start >= stop]. *)

val grapheme_pool : t -> Glyph.pool
(** [grapheme_pool t] returns the glyph pool used by the buffer. *)
