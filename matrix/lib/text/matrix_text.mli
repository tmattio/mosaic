(** Terminal text measurement and segmentation.

    This module contains the Unicode text operations needed by terminal
    renderers: grapheme cluster iteration, display-width measurement, and
    lightweight break discovery. It does not own cells, styles, or interned
    storage.

    {!iter_graphemes} and {!grapheme_count} always follow Unicode grapheme
    boundaries, independently of terminal width policy. Width-dependent position
    operations instead follow the terminal's display units: clusters in
    [`Unicode] mode and codepoints in [`Wcwidth] mode. Use Unicode boundaries
    for editing text; use display positions for fitting and placing it. *)

type width_method = [ `Unicode | `Wcwidth ]
(** The type for width calculation methods.

    - [`Unicode] uses full grapheme segmentation with emoji ZWJ composition.
    - [`Wcwidth] sums per-codepoint wcwidth-style widths inside each string. *)

type line_break_kind = [ `LF | `CR | `CRLF ]
(** The type for line terminators. *)

type position = { byte_offset : int; grapheme_count : int; columns_used : int }
(** The type for a byte position found by display width.

    [grapheme_count] is a cluster count for [`Unicode] and a codepoint count for
    [`Wcwidth]. *)

type grapheme = { byte_offset : int; byte_length : int; width : int }
(** The type for a grapheme or codepoint span.

    Values returned in [`Wcwidth] mode describe one codepoint. *)

val uchar_width : tab_width:int -> Uchar.t -> int
(** [uchar_width ~tab_width u] is the display width of [u] in isolation.
    Controls and combining characters contribute [0]. Width policies agree on
    individual scalars; measuring a sequence requires {!measure}. *)

val measure : width_method:width_method -> tab_width:int -> string -> int
(** [measure ~width_method ~tab_width s] is the display width of [s]. Control
    characters contribute [0].

    Invalid UTF-8 byte sequences are treated as U+FFFD, each contributing width
    [1]. *)

val measure_sub :
  width_method:width_method ->
  tab_width:int ->
  string ->
  pos:int ->
  len:int ->
  int
(** [measure_sub ~width_method ~tab_width s ~pos ~len] is like {!measure} but
    operates on [s.[pos] .. s.[pos + len - 1]] without allocating. The result is
    [0] when [len <= 0]. UTF-8 decoding is bounded by the slice: a codepoint cut
    by either boundary is treated as malformed, just as for {!measure} on the
    corresponding substring.

    Raises [Invalid_argument] if [len > 0] and [pos] and [len] do not designate
    a valid substring of [s]. *)

val grapheme_count : string -> int
(** [grapheme_count s] is the number of grapheme clusters in [s]. *)

val iter_graphemes : (offset:int -> len:int -> unit) -> string -> unit
(** [iter_graphemes f s] calls [f ~offset ~len] for each grapheme cluster in
    [s]. Invalid UTF-8 byte sequences are treated as individual replacement
    characters. *)

val iter_grapheme_info :
  width_method:width_method ->
  tab_width:int ->
  (offset:int -> len:int -> width:int -> unit) ->
  string ->
  unit
(** [iter_grapheme_info ~width_method ~tab_width f s] calls
    [f ~offset ~len ~width] for each non-zero-width Unicode grapheme cluster in
    [s]. The width policy changes the reported width, not the boundaries. *)

val iter_wrap_breaks :
  (break_byte_offset:int -> next_byte_offset:int -> grapheme_offset:int -> unit) ->
  string ->
  unit
(** [iter_wrap_breaks f s] calls
    [f ~break_byte_offset ~next_byte_offset ~grapheme_offset] for each word-wrap
    break opportunity in [s], in order.

    [break_byte_offset] is the byte position of the grapheme containing the
    break character. [next_byte_offset] is the byte position where scanning
    resumes. [grapheme_offset] is the zero-based grapheme index of the break
    grapheme. *)

val iter_line_breaks :
  (pos:int -> kind:line_break_kind -> unit) -> string -> unit
(** [iter_line_breaks f s] calls [f ~pos ~kind] for each line terminator in [s],
    in order. For [`CRLF], [pos] is the position of the LF byte. *)

val find_wrap_pos :
  width_method:width_method ->
  tab_width:int ->
  string ->
  max_columns:int ->
  position
(** [find_wrap_pos ~width_method ~tab_width s ~max_columns] is the longest
    prefix of [s] whose display width is at most [max_columns].

    If the next grapheme would exceed [max_columns], the result points to the
    start of that grapheme. In [`Wcwidth] mode, movement is by codepoint rather
    than by grapheme cluster, and stops as soon as the column budget is reached,
    before any following zero-width codepoints. *)

val find_pos :
  width_method:width_method ->
  tab_width:int ->
  ?include_start_before:bool ->
  string ->
  columns:int ->
  position
(** [find_pos ~width_method ~tab_width s ~columns] is the byte position in [s]
    corresponding to display column [columns].

    By default, a wide grapheme is included only if its end column is at or
    before [columns]. If [include_start_before] is [true], a wide grapheme is
    included if its start column is before [columns]. *)

val width_at :
  width_method:width_method -> tab_width:int -> string -> byte_offset:int -> int
(** [width_at ~width_method ~tab_width s ~byte_offset] is the display width of
    the grapheme starting at [byte_offset]. Returns [0] if [byte_offset] is out
    of bounds.

    In [`Wcwidth] mode, the result is the width of the codepoint starting at
    [byte_offset]. *)

val prev_grapheme :
  width_method:width_method ->
  tab_width:int ->
  string ->
  byte_offset:int ->
  grapheme option
(** [prev_grapheme ~width_method ~tab_width s ~byte_offset] is the grapheme
    immediately before [byte_offset], if any.

    If [byte_offset] falls inside a grapheme, that containing grapheme is
    returned. In [`Wcwidth] mode, the result is the previous non-zero-width
    codepoint. *)

module Layout : sig
  (** Source-backed terminal rows. Styles do not affect segmentation. *)

  type affinity =
    | Upstream
    | Downstream
        (** The preceding or following side of an ambiguous visual boundary. *)

  type caret = { grapheme : int; affinity : affinity }
  (** A source grapheme boundary and its visual side. *)

  type wrap = [ `None | `Char | `Word ]
  (** No wrapping, grapheme wrapping, or word wrapping with grapheme fallback.
  *)

  module Source : sig
    (** Immutable bytes with Unicode grapheme and logical-line indexing.
        Zero-width clusters remain indexed. CRLF is one cluster and two bytes.
        Invalid UTF-8 follows the decoder's replacement-unit boundaries without
        changing the bytes. Width-metric caches are local to a source;
        concurrent access requires synchronization. *)

    type t
    (** An indexed source, independent of wrapping width. *)

    val of_string : string -> t
    (** [of_string s] indexes [s] without normalizing its bytes. *)

    val text : t -> string
    (** [text t] is the original string. *)

    val length : t -> int
    (** [length t] is the number of source graphemes. *)

    val byte_offset : t -> int -> int
    (** [byte_offset t g] maps a grapheme boundary to a byte offset, clamping
        [g] to the source bounds. *)

    val grapheme_of_byte : t -> affinity:affinity -> int -> int
    (** [grapheme_of_byte t ~affinity b] clamps [b] to the source bounds and
        snaps interior bytes backward for [Upstream], forward for [Downstream].
    *)

    val line_count : t -> int
    (** [line_count t] includes empty and trailing empty lines; it is at least
        one. *)

    val line_start : t -> int -> int
    (** [line_start t line] is the first grapheme boundary of the clamped line.
    *)

    val line_end : t -> int -> int
    (** [line_end t line] is the boundary before the clamped line's terminator.
    *)

    val line_of_offset : t -> int -> int
    (** [line_of_offset t g] is the logical line containing clamped boundary
        [g]. *)

    val columns :
      ?width_method:width_method ->
      ?tab_width:int ->
      t ->
      first:int ->
      last:int ->
      int
    (** [columns t ~first ~last] is the signed cell-width difference between
        clamped source boundaries. Width defaults to [`Unicode], tabs to a
        constant two cells (clamped to at least one). Both width methods retain
        Unicode grapheme boundaries. *)

    val slice : t -> first:int -> last:int -> string
    (** [slice t ~first ~last] copies the exact bytes between clamped grapheme
        boundaries; reversed or empty ranges return [""] . *)
  end

  type t
  (** Immutable geometry retaining its source. Coordinates are absolute visual
      rows and terminal columns, before scrolling and clipping. *)

  val create :
    ?width_method:width_method ->
    ?tab_width:int ->
    ?truncate:bool ->
    Source.t ->
    width:int ->
    wrap:wrap ->
    t
  (** [create source ~width ~wrap] lays out [source]. Defaults are Unicode
      width, constant two-cell tabs and no truncation. Nonpositive width keeps
      one unwrapped row per logical line. Overwide clusters occupy one overflow
      row. Truncation applies only without wrapping and uses a trailing one-cell
      ellipsis covering the omitted source suffix. *)

  val measure :
    ?width_method:width_method ->
    ?tab_width:int ->
    ?truncate:bool ->
    Source.t ->
    width:int ->
    wrap:wrap ->
    int * int
  (** [measure source ~width ~wrap] returns row count and maximum width using
      {!create}'s algorithm without retaining rows. *)

  val source : t -> Source.t
  (** [source t] is the indexed source retained by [t]. *)

  val row_count : t -> int
  (** [row_count t] is the visual row count, at least one. *)

  val max_width : t -> int
  (** [max_width t] is the greatest row width in cells. *)

  val row_start : t -> int -> int
  (** [row_start t row] is the first source boundary of the clamped visual row.
  *)

  val row_end : t -> int -> int
  (** [row_end t row] is the last source boundary, excluding line terminators
      and including a truncated suffix. The row index is clamped. *)

  val row_width : t -> int -> int
  (** [row_width t row] is the clamped row's display width. *)

  val logical_line : t -> int -> int
  (** [logical_line t row] is the clamped visual row's logical-line index. *)

  val continuation : t -> int -> int
  (** [continuation t row] is zero on a logical line's first row and increases
      on each wrapped continuation. The row index is clamped. *)

  val iter_rows :
    t ->
    first:int ->
    count:int ->
    (row:int -> logical_line:int -> continuation:int -> unit) ->
    unit
  (** [iter_rows t ~first ~count f] visits bounded absolute rows in order.
      [first] is clamped; negative [count] visits nothing. *)

  val locate : t -> caret -> int * int
  (** [locate t caret] returns its row and column. Source positions are clamped.
      Affinity distinguishes soft-wrap sides and the two ellipsis edges. *)

  val hit : ?affinity:affinity -> t -> row:int -> column:int -> caret
  (** [hit t ~row ~column] is the nearest insertion boundary, with ties forward.
      Coincident zero-width boundaries use [affinity], default [Downstream]. Row
      affinity is retained at soft wraps. Out-of-range rows snap to source
      start/end. Ambiguous boundaries have canonical, not invertible, mapping.
  *)

  val coverage : t -> row:int -> column:int -> int * int
  (** [coverage t ~row ~column] is the half-open source-grapheme range occupying
      the cell, including wide continuations and omitted suffixes. Blanks have
      empty coverage. This does not decide whether a gesture selects the cell.
  *)

  val iter_cells :
    t ->
    row:int ->
    first_column:int ->
    last_column:int ->
    (first:int -> last:int -> column:int -> width:int -> ellipsis:bool -> unit) ->
    unit
  (** [iter_cells t ~row ~first_column ~last_column f] seeks and visits drawable
      clusters intersecting the half-open column interval. Callback ranges are
      source graphemes; an ellipsis covers its omitted suffix. Zero-width runs
      are skipped through the index. Out-of-range rows visit nothing. *)
end
