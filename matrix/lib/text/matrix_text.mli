(** Terminal text measurement and segmentation.

    This module contains the Unicode text operations needed by terminal
    renderers: grapheme cluster iteration, display-width measurement, and
    lightweight break discovery. It does not own cells, styles, or interned
    storage.

    {!iter_graphemes} and {!grapheme_count} always follow Unicode grapheme
    boundaries, independently of terminal width policy. Width-dependent
    operations instead follow the terminal's display units, which a {!Segmenter}
    defines. Use Unicode boundaries for editing text; use display units for
    fitting and placing it. *)

type width_method = [ `Unicode | `Unicode_wide | `Wcwidth ]
(** The type for width calculation methods.

    - [`Unicode] uses full grapheme segmentation with emoji ZWJ composition.
    - [`Unicode_wide] uses grapheme segmentation and promotes clusters with
      additional visible components to at least two cells, matching modern
      Ghostty's width policy.
    - [`Wcwidth] gives each scalar its wcwidth-style width, and places the
      zero-width scalars after a scalar of positive width with it. *)

type line_break_kind = [ `LF | `CR | `CRLF ]
(** The type for line terminators. *)

type position = { byte_offset : int; units : int; columns_used : int }
(** The type for a byte position found by display width.

    [units] counts the display units of positive width before [byte_offset]. *)

val uchar_width : tab_width:int -> Uchar.t -> int
(** [uchar_width ~tab_width u] is the display width of [u] in isolation.
    Controls and combining characters contribute [0]. Width policies agree on
    individual scalars; measuring a sequence requires {!measure}. *)

(** {1:segmenter Display units}

    A terminal receives its output in pieces of any size and must cut it into
    the same display units as if it had arrived whole. A segmenter carries what
    that takes from one scalar to the next: the state of the grapheme cluster
    boundary rules of {{:https://www.unicode.org/reports/tr29/}UAX #29}, which
    look only backwards, so that a boundary is known when the scalar after it
    arrives; and the width of the unit in progress.

    The units are extended grapheme clusters for [`Unicode] and [`Unicode_wide].
    For [`Wcwidth] they are what a terminal that measures each character with
    [wcwidth] places: a scalar of positive width and the zero-width scalars that
    follow it. In every method a control character (General_Category Cc: C0, DEL
    and C1, TAB included) is a unit of its own, of width [0]: it is a function
    to perform, not a character to display. CR LF is therefore two units, where
    UAX #29 makes one cluster of it (GB3).

    Within a unit, {!Segmenter.width} never decreases as scalars are added. A
    terminal places a unit before it knows that the unit is complete, and widens
    it in place as it grows; it could not take back a character that already
    wrapped the line. So a later scalar never narrows a unit: VS15 (U+FE0E),
    which asks for the text presentation of an emoji
    ({{:https://www.unicode.org/reports/tr51/}UTS #51} §4), does not narrow an
    emoji already two cells wide.

    {!measure}, {!iter_units}, {!find_wrap_pos}, {!find_pos} and {!width_at}
    measure these units, with a TAB [tab_width] cells wide. *)

module Segmenter : sig
  type t
  (** The type for the segmentation of one stream. *)

  val create : width_method -> t
  (** [create m] is a segmenter for units of method [m], before the first
      scalar. *)

  val reset : t -> unit
  (** [reset s] ends the unit in progress: the next scalar starts a unit, as at
      the start of a text (UAX #29 GB1). *)

  val add : t -> Uchar.t -> bool
  (** [add s u] adds the scalar [u] to the stream and is [true] iff [u] starts a
      unit. Afterwards [u] belongs to the unit in progress, so {!width} is that
      of the unit [u] started or continued, not of the unit it closed: a caller
      that needs the closed unit's width reads {!width} before [add].
      Allocation-free. *)

  val may_continue : width_method -> Uchar.t -> Uchar.t -> bool
  (** [may_continue m u v] is [false] when [v] starts a unit of method [m] after
      any text that ends with [u], and [true] when [v] continues the unit that
      ends with [u] in some text. It reads the two scalars only: a unit may
      continue by a rule that looks further back than [u], as two regional
      indicators do after an even number of them and not after an odd number,
      and then only {!add}, which has seen that text, tells. Allocation-free. *)

  val ascii_run : t -> string -> pos:int -> len:int -> int
  (** [ascii_run s str ~pos ~len] is the number [k] of bytes from [pos] that are
      complete units of one printable ASCII character (U+0020 to U+007E) each:
      each starts a unit, and is followed within the slice by another printable
      ASCII byte, which therefore starts the next unit. It is [0] when no such
      run starts at [pos], in particular when the unit in progress absorbs the
      byte at [pos], as a Prepend character does (UAX #29 GB9b). Afterwards [s]
      is as if the [k] scalars had been added with {!add}; the byte at [pos + k]
      is not added. So a writer places [k] cells at once and goes on with
      {!add}, and the units do not depend on how the stream was cut.
      Allocation-free.

      Raises [Invalid_argument] if [pos] and [len] do not designate a valid
      substring of [str]. *)

  val width : t -> int
  (** [width s] is the display width of the unit in progress: [0] for a control,
      before the first scalar, and while the unit has nothing to display. It
      never decreases until the next unit starts. *)
end

(** {1:measuring Measuring} *)

val measure : width_method:width_method -> tab_width:int -> string -> int
(** [measure ~width_method ~tab_width s] is the sum of the widths of the display
    units of [s]. Control characters contribute [0], except TAB, which
    contributes [tab_width].

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

val iter_units :
  width_method:width_method ->
  tab_width:int ->
  (offset:int -> len:int -> width:int -> unit) ->
  string ->
  unit
(** [iter_units ~width_method ~tab_width f s] calls [f ~offset ~len ~width] for
    each display unit of [s] of positive width, in order. A TAB is [tab_width]
    wide. Invalid UTF-8 byte sequences are treated as U+FFFD. *)

val iter_wrap_breaks :
  (break_byte_offset:int -> next_byte_offset:int -> grapheme_offset:int -> unit) ->
  string ->
  unit
(** [iter_wrap_breaks f s] calls
    [f ~break_byte_offset ~next_byte_offset ~grapheme_offset] for each word-wrap
    break opportunity in [s], in order. Adjacent CJK graphemes allow line
    breaks, except before Kana combining marks and punctuation U+30A0 and
    U+30FB. No intercharacter break follows a zero-width grapheme.

    [break_byte_offset] is the byte position of the grapheme immediately before
    the break. [next_byte_offset] is the byte position where scanning resumes.
    [grapheme_offset] is the zero-based index of that preceding grapheme. *)

val iter_word_breaks :
  (break_byte_offset:int -> next_byte_offset:int -> grapheme_offset:int -> unit) ->
  string ->
  unit
(** [iter_word_breaks f s] is {!iter_wrap_breaks} without CJK intercharacter
    opportunities. Use it for editor word movement and deletion: CJK runs remain
    words, with breaks at ASCII/CJK transitions, whitespace, and supported
    punctuation. Offsets follow the same convention as {!iter_wrap_breaks}. *)

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

    If the next display unit would exceed [max_columns], the result points to
    the start of that unit. Units of no width that fit are included. *)

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
    the display unit starting at [byte_offset]. Returns [0] if [byte_offset] is
    out of bounds. *)

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
