(** Mutable grid of terminal cells.

    A grid is a two-dimensional framebuffer where each cell stores character
    content, foreground and background colors, text attributes, and a hyperlink.
    Backed by bigarrays for cache-friendly access.

    Single codepoints are stored directly as packed integers. Multi-codepoint
    grapheme clusters (ZWJ emoji, combining characters) are stored in grid-owned
    grapheme storage. Wide characters span multiple cells: a start cell followed
    by continuation markers.

    Drawing composites colors with source-over alpha blending; copying preserves
    stored colors. A stack of {e scissor} clipping regions constrains drawing.
    Wide characters are span-atomic: if the start cell is accepted by a clipping
    region, continuation cells are written with it to keep the row well-formed.
*)

(** {1:types Types} *)

type t
(** Mutable terminal contents. Every mutation preserves complete grapheme spans;
    stored text and hyperlinks belong to the grid. *)

type region = { x : int; y : int; width : int; height : int }
(** The type for rectangular areas in cell coordinates. Covers cells satisfying
    [x <= col < x + width] and [y <= row < y + height]. Non-positive [width] or
    [height] yields an empty region. *)

(** {1:constructors Constructors} *)

val create :
  width:int -> height:int -> ?width_method:Matrix_text.width_method -> unit -> t
(** [create ~width ~height ()] is a grid with all cells set to spaces
    (terminal-default foreground, transparent background) with:
    - [width_method] grapheme width method. Defaults to [`Unicode].

    Raises [Invalid_argument] if [width <= 0] or [height <= 0]. *)

val create_like : t -> width:int -> height:int -> t
(** [create_like g ~width ~height] is a fresh empty grid that shares [g]'s
    grapheme storage and width method.

    Sharing storage lets {!cells_equal} compare graphemes without decoding and
    {!blit} copy them without re-interning. Use this for double-buffered
    rendering where frames are diffed against each other.

    Raises [Invalid_argument] if [width <= 0] or [height <= 0]. *)

(** {1:properties Properties} *)

val width : t -> int
(** [width g] is the grid width in cells. *)

val height : t -> int
(** [height g] is the grid height in cells. *)

val width_method : t -> Matrix_text.width_method
(** [width_method g] is the current width computation method. *)

val set_width_method : t -> Matrix_text.width_method -> unit
(** [set_width_method g m] changes the width method for subsequent {!draw_text}
    calls. Existing cell widths are not updated. *)

val active_height : t -> int
(** [active_height g] is the number of rows from the top containing non-blank
    content (character code other than 0 or space). Returns [0] for an empty or
    cleared grid. *)

(** {1:cell_access Cell access}

    Linear index [idx] is row-major: [idx = y * width + x]. *)

val idx : t -> x:int -> y:int -> int
(** [idx g ~x ~y] is the flat index for cell [(x, y)]. No bounds checking. *)

val get_codepoint : t -> int -> int
(** [get_codepoint g idx] is the Unicode scalar at [idx], [0] for empty cells,
    or [-1] for a multi-codepoint grapheme or continuation cell. *)

val get_attrs : t -> int -> int
(** [get_attrs g idx] is the packed attribute integer at [idx]. *)

val get_link : t -> int -> string
(** [get_link g idx] is the hyperlink URL at [idx], or [""] for no link. *)

val get_fg : t -> int -> Ansi.Color.t
(** [get_fg g idx] is the foreground color stored at [idx], including terminal
    emission intent. *)

val get_bg : t -> int -> Ansi.Color.t
(** [get_bg g idx] is the background color stored at [idx], including terminal
    emission intent. *)

val get_text : t -> int -> string
(** [get_text g idx] is the grapheme at [idx] as a string. Returns [""] for
    empty or continuation cells. *)

val cell_text_length : t -> int -> int
(** [cell_text_length g idx] is the number of UTF-8 bytes needed to encode the
    cell content at [idx]. *)

val blit_cell_text : t -> int -> bytes -> pos:int -> int
(** [blit_cell_text g idx dst ~pos] writes the UTF-8 bytes for the cell content
    at [idx] into [dst] starting at [pos] and returns the number of bytes
    written. Returns [0] if [dst] has insufficient space. *)

val get_style : t -> int -> Ansi.Style.t
(** [get_style g idx] reconstructs the style at [idx]. *)

(** {1:predicates Predicates} *)

val is_empty : t -> int -> bool
(** [is_empty g idx] is [true] iff the cell is the null/empty sentinel.
    Different from a blank space cell (U+0020), which is the default content
    after {!create} and {!clear}. *)

val is_continuation : t -> int -> bool
(** [is_continuation g idx] is [true] iff the cell is the trailing part of a
    wide character. *)

val cell_width : t -> int -> int
(** [cell_width g idx] is the display width of the cell (0 for
    empty/continuation, 1–2 for start). *)

val cells_equal : t -> int -> t -> int -> bool
(** [cells_equal g1 idx1 g2 idx2] is [true] iff both cells have identical text,
    span position, colors, attributes, and hyperlink URLs. Grids need not share
    storage. Shared storage allows direct comparison without lookup. *)

(** {1:manipulation Manipulation} *)

val resize : t -> width:int -> height:int -> unit
(** [resize g ~width ~height] resizes the grid, preserving existing contents
    where possible. Cells outside the new bounds are released. No-op if
    dimensions are unchanged.

    Raises [Invalid_argument] if [width <= 0] or [height <= 0]. *)

val resize_clear : ?color:Ansi.Color.t -> t -> width:int -> height:int -> unit
(** [resize_clear g ~width ~height] resizes [g] and clears all cells with
    {!clear} when dimensions change. If dimensions are unchanged, this is a
    no-op; use {!clear} for an explicit same-size clear.

    Raises [Invalid_argument] if [width <= 0] or [height <= 0]. *)

val clear : ?color:Ansi.Color.t -> t -> unit
(** [clear ~color g] resets all cells to spaces with white foreground and
    [color] background. [color] defaults to transparent. Releases all stored
    grapheme references. The scissor stack is preserved. *)

val blit : src:t -> dst:t -> unit
(** [blit ~src ~dst] copies all cell data from [src] to [dst], resizing [dst] to
    match. Graphemes are copied into [dst]'s storage as needed. No alpha
    blending. *)

val copy : t -> t
(** [copy g] is a deep copy of [g]. The scissor stack starts empty on the copy.
*)

val blit_region :
  src:t ->
  dst:t ->
  src_x:int ->
  src_y:int ->
  width:int ->
  height:int ->
  dst_x:int ->
  dst_y:int ->
  unit
(** [blit_region ~src ~dst ~src_x ~src_y ~width ~height ~dst_x ~dst_y] copies a
    rectangular region from [src] to [dst].

    The region is clamped to valid bounds in both grids. Negative coordinates
    shift the region inward. Respects the scissor on [dst]. Colors are copied
    unchanged, independently of the destination opacity. Same-grid overlapping
    regions are handled as copies. Wide-character fragments whose start or
    continuation is outside the copied region are written as spaces. Never
    resizes [dst]. *)

val draw_region :
  src:t ->
  dst:t ->
  src_x:int ->
  src_y:int ->
  width:int ->
  height:int ->
  dst_x:int ->
  dst_y:int ->
  unit
(** [draw_region ~src ~dst ~src_x ~src_y ~width ~height ~dst_x ~dst_y]
    composites a rectangular region of [src] over [dst]. Bounds, clipping, and
    grapheme spans follow {!blit_region}. Source colors are multiplied by the
    destination opacity; fully transparent cells leave the destination
    unchanged. Self-composition snapshots the source before drawing. *)

val fill_rect :
  t -> x:int -> y:int -> width:int -> height:int -> color:Ansi.Color.t -> unit
(** [fill_rect g ~x ~y ~width ~height ~color] fills a rectangle:
    - Transparent (alpha = 0): leaves cells unchanged.
    - Semi-transparent: blends over existing background.
    - Opaque: overwrites entirely (space cell, terminal-default foreground,
      [color] background).

    Clipped to grid bounds and scissor. *)

val clear_rect :
  ?color:Ansi.Color.t -> t -> x:int -> y:int -> width:int -> height:int -> unit
(** [clear_rect g ~x ~y ~width ~height] clears a rectangle to spaces, default
    foreground, and [color] background. [color] defaults to transparent.

    Clipped to grid bounds and scissor. *)

(** {1:drawing Drawing} *)

val draw_text :
  ?style:Ansi.Style.t ->
  ?tab_width:int ->
  ?width_method:Matrix_text.width_method ->
  t ->
  x:int ->
  y:int ->
  text:string ->
  unit
(** [draw_text ~style ~tab_width ~width_method g ~x ~y ~text] draws single-line
    text. Text is segmented using [width_method], defaulting to the grid's
    {!width_method}. This override does not change the grid configuration. Wide
    characters occupy multiple cells with continuation markers. Newlines are
    skipped; tabs expand to [tab_width] spaces (default [2]).

    When the resolved background has alpha < 1.0, colors are blended with
    existing cells. A space on a translucent background preserves the existing
    cell content and tints its colors.

    Respects the active scissor for start cells. If a wide character's start
    cell is visible, its continuation cells are written as well to preserve the
    grid's wide-span invariant. *)

module Border = Border
(** Box-drawing border character sets. *)

val draw_box :
  t ->
  x:int ->
  y:int ->
  width:int ->
  height:int ->
  ?border:Border.t ->
  ?sides:Border.side list ->
  ?style:Ansi.Style.t ->
  ?fill:Ansi.Color.t ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  ?title_style:Ansi.Style.t ->
  unit ->
  unit
(** [draw_box g ~x ~y ~width ~height ()] draws a box with Unicode borders and:
    - [border] character set. Defaults to {!Border.single}.
    - [sides] to draw. Defaults to {!Border.all}.
    - [style] for border characters. Defaults to {!Ansi.Style.default}.
    - [fill] interior and border cell background color. When absent, no fill is
      applied.
    - [title] on the top border when [\`Top] is included and
      [width >= title_width + 4].
    - [title_alignment] defaults to [\`Left] with 2-cell padding.
    - [title_style] for the title text.

    Respects the scissor. *)

type line_symbols = {
  h : string;  (** Horizontal segment. *)
  v : string;  (** Vertical segment. *)
  diag_up : string;  (** Diagonal up-right. *)
  diag_down : string;  (** Diagonal down-right. *)
}
(** The type for line drawing symbol sets. *)

val default_line_symbols : line_symbols
(** Unicode box-drawing: ["─"], ["│"], ["╱"], ["╲"]. *)

val ascii_line_symbols : line_symbols
(** ASCII: ["-"], ["|"], ["/"], ["\\"]. *)

val draw_line :
  t ->
  x1:int ->
  y1:int ->
  x2:int ->
  y2:int ->
  ?style:Ansi.Style.t ->
  ?symbols:line_symbols ->
  ?kind:[ `Line | `Braille ] ->
  unit ->
  unit
(** [draw_line g ~x1 ~y1 ~x2 ~y2 ()] draws a line using Bresenham's algorithm
    with:
    - [\`Line] (default) uses box-drawing characters with per-step symbol
      selection based on direction.
    - [\`Braille] uses 2×4 dot patterns that merge with existing braille cells,
      allowing multiple lines to share cells.

    Respects the scissor. *)

(** {1:set_uchar Direct cell write} *)

val set_uchar :
  t ->
  x:int ->
  y:int ->
  uchar:Uchar.t ->
  fg:Ansi.Color.t ->
  bg:Ansi.Color.t ->
  attrs:Ansi.Attr.t ->
  ?link:string ->
  ?blend:bool ->
  unit ->
  unit
(** [set_uchar g ~x ~y ~uchar ~fg ~bg ~attrs ()] draws one Unicode scalar.
    [blend] defaults to [false], copying colors unchanged. [~blend:true] applies
    the opacity stack and composites colors over existing cells.

    A start outside the grid or scissor is skipped. Existing overlapping spans
    are removed and continuation cells are written with the start, even beyond
    the scissor. A scalar that would cross the right edge writes styled spaces
    instead. Width is the Unicode scalar width; controls and zero-width scalars
    write an empty cell without extending the previous grapheme. Use
    {!draw_text} for multi-codepoint graphemes. Drawn cells do not expose
    storage handles; use {!blit_region} to copy existing content. *)

(** {1:clipping Clipping} *)

val push_clip : t -> region -> unit
(** [push_clip g r] pushes a clipping region. The effective clip is the
    intersection of [r] with the current clip. *)

val pop_clip : t -> unit
(** [pop_clip g] pops the most recent clip. No-op if the stack is empty. *)

val clear_clip : t -> unit
(** [clear_clip g] removes all clipping regions. *)

val clip_region : t -> region
(** [clip_region g] is the effective scissor intersected with grid bounds. An
    empty intersection has zero width or height. *)

val intersects_clip : t -> region -> bool
(** [intersects_clip g r] is [true] iff the positive-area region [r] intersects
    both [g]'s bounds and its effective clipping region. *)

val clip : t -> region -> (unit -> 'a) -> 'a
(** [clip g r f] runs [f ()] with [r] as the active clip, popping it on return
    (even on exception). *)

(** {1:opacity Opacity stack}

    Hierarchical opacity for UI trees. Drawing operations multiply color alpha
    by the product of all stacked opacities. Push/pop pairs must be balanced. *)

val push_opacity : t -> float -> unit
(** [push_opacity g o] pushes [o] (clamped to \[0.0, 1.0\]). *)

val pop_opacity : t -> unit
(** [pop_opacity g] pops the most recent opacity. No-op if the stack is empty.
*)

val current_opacity : t -> float
(** [current_opacity g] is the product of all stacked opacities, or [1.0] if the
    stack is empty. *)

(** {1:scrolling Scrolling} *)

val scroll : t -> top:int -> bottom:int -> int -> unit
(** [scroll g ~top ~bottom n] scrolls rows \[[top]..[bottom]\] by [n] lines.
    Positive [n] scrolls content up (new blank lines at bottom). Negative [n]
    scrolls down. Zero is a no-op. *)

(** {1:diagnostics Diagnostics} *)

val grapheme_stats : t -> int * int
(** [grapheme_stats g] is [(live, slots)] for [g]'s grapheme store: the number
    of live interned payloads and the number of store slots ever allocated
    (including freed slots awaiting reuse). The store is shared between grids
    created with {!create_like}. Intended for tests and devtools; [slots]
    staying bounded under churn indicates slots are reclaimed and reused. *)

(** {1:converting Converting} *)

val to_text : ?trim:bool -> t -> string
(** [to_text ~trim g] renders [g]'s visible text: one line per row, one grapheme
    per cell, continuation cells of wide graphemes skipped, empty cells as
    spaces. Trailing spaces on each row are dropped when [trim] is [true]
    (default). Styling is discarded; see {!to_ansi} for styled output. *)

val to_ansi : ?reset:bool -> ?complete_rows:bool -> ?rows:int -> t -> string
(** [to_ansi ~reset ~complete_rows ?rows g] serializes the first [rows] rows of
    [g] with ANSI styles. [rows] defaults to the grid height; [0] produces an
    empty string. By default, rows are separated by LF without a final newline.
    [complete_rows] (default [false]) instead erases each terminal row before
    writing it and terminates every row with CRLF, including the last row.
    [reset] (default [true]) adds a final SGR reset. Raises [Invalid_argument]
    if [rows] is negative or exceeds the grid height. *)
