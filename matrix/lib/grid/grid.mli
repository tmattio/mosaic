(** Mutable grid of terminal cells with rich text support.

    Grid provides a high-level framebuffer API for terminal applications, built
    on top of efficient Bigarray storage. It manages terminal cells with Unicode
    grapheme clusters, colors, text attributes, and alpha blending, enabling
    rich terminal UIs with full color support and proper handling of multi-width
    characters.

    {1 Overview}

    A grid is a two-dimensional array of terminal cells, where each cell stores:
    - A character code or grapheme cluster reference
    - Foreground and background colors (RGBA)
    - Text attributes (bold, italic, underline, etc.)
    - Display width (for multi-cell characters like CJK)

    Grids are mutable and support efficient in-place updates, making them
    suitable for interactive terminal applications with frequent redraws.

    {1 Usage Basics}

    Create a grid and draw text:
    {[
      let grid = Grid.create ~width:80 ~height:24 () in
      Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello, world!";
      Grid.draw_text
        ~style:(Ansi.Style.make ~fg:Ansi.Color.red ~bold:true ())
        grid ~x:0 ~y:1 ~text:"Error: Operation failed"
    ]}

    Fill regions and copy content:
    {[
      Grid.fill_rect grid ~x:10 ~y:5 ~width:20 ~height:3
        ~color:(Ansi.Color.of_rgb 100 100 100);
      let other = Grid.create ~width:80 ~height:24 () in
      Grid.blit ~src:grid ~dst:other
    ]}

    Use clipping for constrained drawing:
    {[
      Grid.with_scissor grid { x = 10; y = 5; width = 30; height = 10 }
        (fun () -> Grid.draw_text grid ~x:0 ~y:0 ~text:"This text is clipped")
    ]}

    {1 Key Concepts}

    {2 Character Encoding}

    The grid uses a compact encoding scheme for characters:
    - Unicode scalar values (single codepoints) are stored directly as 32-bit
      codes with embedded width and flag information
    - Multi-codepoint grapheme clusters (emoji with ZWJ, combining characters)
      are stored via the glyph pool with reference counting
    - Wide characters (e.g., CJK, emoji) span multiple cells: a start cell
      followed by continuation markers

    This hybrid approach minimizes memory usage and glyph pool lookups while
    supporting full Unicode.

    {2 Glyph Pool}

    The glyph pool provides reference-counted storage for multi-codepoint
    grapheme clusters. Multiple cells can reference the same grapheme, reducing
    memory overhead for repeated text. The pool automatically manages reference
    counts during cell updates and grid resize operations.

    {2 Width Computation}

    Character display width is calculated using one of three methods:
    - [`Wcwidth]: POSIX wcwidth(3) compatible (fastest, limited emoji support)
    - [`Unicode]: Full Unicode-aware width with proper emoji handling
    - [`No_zwj]: Unicode-aware but splits zero-width joiner sequences

    The width method affects how {!draw_text} positions characters and can be
    changed dynamically with {!set_width_method}.

    {2 Alpha Blending}

    When {!respect_alpha} is enabled, colors with alpha < 1.0 are blended with
    existing cell colors using perceptual alpha mixing. This enables
    semi-transparent overlays and smooth color transitions. The blending uses a
    perceptual curve that preserves visual contrast better than linear alpha
    compositing. Note that this is not physically correct alpha compositing; the
    output alpha value is the source alpha, not a composited value.

    Use {!set_cell_alpha} to write cells with alpha blending regardless of the
    {!respect_alpha} setting, or set {!respect_alpha} to enable blending for all
    cell writes.

    {2 Scissor Clipping}

    The scissor stack enables hierarchical clipping regions. Drawing operations
    outside the active scissor rectangle are silently ignored. Scissors are
    useful for implementing scrollable regions, panels, and constrained layouts.

    {1 Performance Considerations}

    - Grid resizing is O(cells_outside_new_bounds) for releasing truncated
      cells, plus O(new_width × new_height) for allocation if storage grows
    - Cell updates are O(1) but may trigger glyph pool reference counting
    - Blitting is O(src_width × src_height) with bounds checking
    - Text drawing is O(grapheme_count) with width calculations per grapheme
    - Scissor checks are O(1) per cell write
    - Zero-allocation cell accessors ({!get_fg_r}, {!get_bg_r}, etc.) enable
      efficient rendering loops without tuple allocation

    {1 Invariants}

    - Grid dimensions are strictly positive (zero-sized grids are invalid)
    - Cell indices are row-major: [index = y * width + x]
    - Color planes store RGBA as 4 consecutive normalized floats per cell (range
      [0.0, 1.0])
    - Wide characters use continuation codes in trailing cells
    - Scissor stack operations are balanced (push/pop pairs) *)

type t
(** Mutable grid of terminal cells.

    An opaque handle storing terminal cell data with efficient memory layout.
    Query dimensions via {!width} and {!height}; access cells via {!get_code},
    {!get_style}, and {!get_text}. *)

type clip_rect = { x : int; y : int; width : int; height : int }
(** Clipping rectangle in cell coordinates.

    Coordinates describe the top-left cell ([x], [y]) and positive extents in
    cells. The rectangle covers all cells satisfying [x <= col < x + width] and
    [y <= row < y + height]. Supplying non-positive [width] or [height] yields
    an empty rectangle and therefore clips all subsequent writes. *)

(** {1 Grid Creation} *)

val create :
  width:int ->
  height:int ->
  ?glyph_pool:Glyph.pool ->
  ?width_method:Glyph.width_method ->
  ?respect_alpha:bool ->
  unit ->
  t
(** [create ~width ~height ?glyph_pool ?width_method ?respect_alpha ()] creates
    a grid.

    @param width Grid width in cells. Must be strictly positive (> 0).
    @param height Grid height in cells. Must be strictly positive (> 0).
    @param glyph_pool
      Optional glyph pool for grapheme storage. If omitted, a fresh pool is
      allocated. Sharing a pool across multiple grids reduces memory for common
      text.
    @param width_method
      Grapheme width computation method. Defaults to [`Unicode] for
      compatibility with most terminals.
    @param respect_alpha
      Whether to honor alpha blending when writing cells. Defaults to [false].
      Enable for semi-transparent overlays.

    All cells are initialized as spaces with opaque white foreground and
    transparent black background, matching the result of {!clear} with default
    color. The scissor stack is empty.

    @raise Invalid_argument if [width <= 0] or [height <= 0]. *)

(** {1 Grid Properties} *)

val glyph_pool : t -> Glyph.pool
(** [glyph_pool t] returns the glyph pool used by [t]. Sharing pools across
    grids enables efficient {!blit} operations. *)

val width_method : t -> Glyph.width_method
(** [width_method t] returns the current width computation method. *)

val set_width_method : t -> Glyph.width_method -> unit
(** [set_width_method t method_] changes the width computation method.

    Subsequent {!draw_text} calls use the new method. Existing cell widths are
    not updated; only new text rendering is affected.

    Use this to switch between terminals with different emoji support or to
    experiment with width calculation strategies. *)

val respect_alpha : t -> bool
(** [respect_alpha t] returns whether alpha blending is enabled for {!set_cell}
    and cross-grid {!blit_region}.

    When [true], these operations blend colors based on source alpha values.
    When [false], they overwrite destination colors opaquely. Other operations
    manage blending internally based on color alpha values. *)

val set_respect_alpha : t -> bool -> unit
(** [set_respect_alpha t enabled] sets the alpha blending mode.

    See {!respect_alpha}. The flag only affects future operations; existing
    cells are unchanged. *)

val width : t -> int
(** [width t] returns the grid width in cells. *)

val height : t -> int
(** [height t] returns the grid height in cells. *)

val active_height : t -> int
(** [active_height t] returns the number of rows from the top that contain
    non-blank content cells.

    A content cell is one whose character code is neither 0 nor a space
    (U+0020). Pure background fills (spaces with background color) do not
    contribute to [active_height], making this suitable for sizing inline
    primary rendering regions. Returns 0 when the grid has no content.

    Note: After {!clear}, cells contain spaces (U+0020) with styling, not
    character code 0, so cleared grids have [active_height] = 0. *)

(** {1 Cell Access} *)

val get_code : t -> int -> int
(** [get_code t idx] returns the cell code at linear index [idx].

    For use with Grid operations like {!set_cell}. Cell codes are aligned with
    {!Glyph.t}, so they can also be passed to glyph pool operations.
    Zero-allocation. *)

val get_glyph : t -> int -> Glyph.t
(** [get_glyph t idx] returns the glyph value at linear index [idx].

    For use with glyph pool operations like {!Glyph.blit}, {!Glyph.length}, and
    {!Glyph.to_string}. Zero-allocation. *)

val get_attrs : t -> int -> int
(** [get_attrs t idx] returns the packed attribute integer at [idx]. *)

val get_link : t -> int -> int32
(** [get_link t idx] returns the internal hyperlink ID at [idx]. *)

val get_fg_r : t -> int -> float
(** [get_fg_r t idx] returns the red component of foreground color at [idx] in
    the range 0.0 to 1.0. *)

val get_fg_g : t -> int -> float
(** [get_fg_g t idx] returns the green component of foreground color at [idx] in
    the range 0.0 to 1.0. *)

val get_fg_b : t -> int -> float
(** [get_fg_b t idx] returns the blue component of foreground color at [idx] in
    the range 0.0 to 1.0. *)

val get_fg_a : t -> int -> float
(** [get_fg_a t idx] returns the alpha component of foreground color at [idx] in
    the range 0.0 to 1.0. *)

val get_bg_r : t -> int -> float
(** [get_bg_r t idx] returns the red component of background color at [idx] in
    the range 0.0 to 1.0. *)

val get_bg_g : t -> int -> float
(** [get_bg_g t idx] returns the green component of background color at [idx] in
    the range 0.0 to 1.0. *)

val get_bg_b : t -> int -> float
(** [get_bg_b t idx] returns the blue component of background color at [idx] in
    the range 0.0 to 1.0. *)

val get_bg_a : t -> int -> float
(** [get_bg_a t idx] returns the alpha component of background color at [idx] in
    the range 0.0 to 1.0. *)

val get_text : t -> int -> string
(** [get_text t idx] decodes the grapheme at [idx] into a string. Returns an
    empty string for empty cells or continuation cells. Note: This allocates a
    new string. *)

val get_style : t -> int -> Ansi.Style.t
(** [get_style t idx] reconstructs the high-level style object for the cell at
    [idx]. Note: This allocates a new [Ansi.Style.t]. *)

val get_background : t -> int -> Ansi.Color.t
(** [get_background t idx] reads the background color stored at [idx]
    (row-major). *)

val is_empty : t -> int -> bool
(** [is_empty t idx] returns true if the cell contains the null code (0). *)

val is_continuation : t -> int -> bool
(** [is_continuation t idx] returns true if the cell is the trailing part of a
    wide character. *)

val is_simple : t -> int -> bool
(** [is_simple t idx] returns true if the cell contains a simple character
    (ASCII or single Unicode scalar) that doesn't require glyph pool lookup. *)

val cell_width : t -> int -> int
(** [cell_width t idx] returns the display width of the cell (0, 1, or more). *)

val cells_equal : t -> int -> t -> int -> bool
(** [cells_equal t1 idx1 t2 idx2] returns true if cells at [idx1] in [t1] and
    [idx2] in [t2] have identical content and styling. Colors are compared with
    small epsilon for float rounding tolerance. *)

val hyperlink_url : t -> int32 -> string option
(** [hyperlink_url t id] resolves [id] (from {!get_link}) to a URL if present.
    Returns [None] when [id] represents "no hyperlink" or the identifier is
    unknown. *)

val hyperlink_url_direct : t -> int32 -> string
(** [hyperlink_url_direct t id] resolves [id] to a URL string.

    Returns the empty string [""] when [id] represents "no hyperlink" or the
    identifier is unknown. This is a zero-allocation alternative to
    {!hyperlink_url} for performance-critical render loops. *)

(** {1 Grid Manipulation} *)

val resize : t -> width:int -> height:int -> unit
(** [resize t ~width ~height] resizes the grid, preserving existing contents
    where possible.

    @param width New width in cells. Must be strictly positive (> 0).
    @param height New height in cells. Must be strictly positive (> 0).

    If the new dimensions are smaller, cells outside the new bounds are released
    (glyph pool references are decremented). If larger, new cells are
    zero-initialized.

    Resizing to identical dimensions is a no-op.

    @raise Invalid_argument if [width <= 0] or [height <= 0].

    Time complexity: O(cells_outside_new_bounds) for releasing truncated cells,
    plus O(new_width × new_height) for allocation if storage grows. *)

val clear : ?color:Ansi.Color.t -> t -> unit
(** [clear ?color t] resets all cells to the given color.

    @param color Fill color. Defaults to transparent black (RGBA = 0, 0, 0, 0).

    All cells are set to character code 32 (space), foreground set to opaque
    white and background set to [color], attributes cleared, and widths reset to
    1. RGBA raw planes store normalized floats in [0.0, 1.0]. Glyph pool
    references from existing cells are released. The scissor stack is preserved.

    This creates "visible" blank cells that match the terminal's cleared state,
    where spaces with appropriate styling represent empty areas rather than
    invisible content.

    Time complexity: O(width × height). *)

val blit : src:t -> dst:t -> unit
(** [blit ~src ~dst] copies the full contents of [src] into [dst].

    Resizes [dst] to match [src] dimensions, then copies all cell data
    (characters, colors, attributes, hyperlinks). Existing grapheme references
    in [dst] are released before copying.

    If [src] and [dst] share the same {!glyph_pool}, character codes and link
    identifiers are copied verbatim and the destination's grapheme tracker is
    rebuilt, reusing the underlying pool entries. If they use different pools,
    each distinct grapheme is copied once into [dst]'s pool and reused for all
    matching cells.

    The scissor stack on [dst] is not modified, and no alpha blending is
    performed because the copy operates directly on the backing buffers.

    Time complexity: O(src_width × src_height). *)

val copy : t -> t
(** [copy t] creates a deep copy of [t], sharing the underlying glyph pool.

    Metadata such as width method and {!respect_alpha} is preserved, while the
    scissor stack starts empty on the copy. Use this for snapshots before
    diffing or serialization. *)

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

    @param src_x Source region left edge (inclusive).
    @param src_y Source region top edge (inclusive).
    @param width Region width in cells, clamped to non-negative values.
    @param height Region height in cells, clamped to non-negative values.
    @param dst_x Destination region left edge.
    @param dst_y Destination region top edge.

    The function clamps the region to valid bounds in both grids. Negative
    source or destination coordinates shift the region inward by adjusting
    offsets and shrinking [width] / [height]. Cells outside the overlapping
    rectangle are ignored.

    Complex graphemes and hyperlinks are preserved where possible: destination
    cells in the target rectangle are released, then the grapheme data and link
    URLs from [src] are copied. When [src] and [dst] share the same
    {!glyph_pool}, character codes are copied verbatim and reference counts are
    adjusted. When the pools differ, each distinct grapheme is re-interned once
    into [dst]'s pool and reused for all matching cells.

    The scissor stack on [dst] clips writes; coordinates outside the active
    scissor are not modified. When [src] and [dst] are the same grid,
    overlapping regions are handled correctly by copying rows in the appropriate
    direction. When they are different grids, the copy proceeds cell by cell.
    Colors are blended with the destination when either [dst.respect_alpha] is
    true or the source colors have alpha < 1.0; otherwise cells are overwritten.

    The operation never resizes [dst]; regions that extend beyond either grid's
    bounds are truncated.

    Time complexity: O(width × height) over the visible part of the region. *)

val fill_rect :
  t -> x:int -> y:int -> width:int -> height:int -> color:Ansi.Color.t -> unit
(** [fill_rect t ~x ~y ~width ~height ~color] fills a rectangle.

    @param x Rectangle left edge.
    @param y Rectangle top edge.
    @param width Rectangle width, clamped to non-negative values.
    @param height Rectangle height, clamped to non-negative values.
    @param color Base color applied to the region.

    Behavior:
    - If [color] has alpha close to 0 (fully transparent), cells in the
      rectangle are reset to a "blank" state: character code 32 (space),
      attributes cleared and hyperlinks removed, but each cell's existing
      background color is preserved.
    - If [color] has [0 < alpha < 1], the region is updated via per-cell alpha
      compositing: the new background color is blended over the existing
      background, and a space glyph with reset attributes is drawn.
    - If [color] is fully opaque, all cells in the rectangle are rewritten in
      bulk with character code 32 (space), foreground pinned to opaque white,
      background set to [color], attributes cleared and width set to 1. Existing
      complex graphemes in the region are released first so reference counts
      remain correct.

    The rectangle is clipped to grid bounds and respects the active scissor.
    Time complexity: O(width × height) for the clipped region. *)

(** {1 Drawing} *)

val draw_text :
  ?style:Ansi.Style.t ->
  ?tab_width:int ->
  t ->
  x:int ->
  y:int ->
  text:string ->
  unit
(** [draw_text ?style t ~x ~y ~text] draws a single-line text string starting at
    [(x, y)].

    @param style
      Optional text style. Defaults to {!Ansi.Style.default} (terminal default
      colors, no attributes).
    @param tab_width
      Width of a tab stop in cells. Values ≤ 0 fall back to 2. Only used when
      [text] contains the tab character.
    @param x Starting column (may be negative for partial rendering).
    @param y Row (must be in \[0, height\) or drawing is skipped).
    @param text UTF-8 encoded text to render.

    The text is segmented into grapheme clusters using Unicode extended grapheme
    cluster boundaries. Each grapheme's display width is calculated using the
    current {!width_method}. Wide characters (width ≥ 2) occupy multiple cells
    with continuation markers in trailing cells.

    Newlines are skipped without advancing the cursor position. Tabs are
    expanded to spaces using [tab_width] (default 2). Use multiple {!draw_text}
    calls for multi-line text.

    Drawing respects the active scissor region; cells outside the scissor are
    not modified. Alpha blending occurs when the resolved background color
    (explicit style background if present, otherwise the destination cell
    background) has alpha < 0.999; in that case colors are alpha-composited with
    the existing cell instead of simply overwriting them. Ordinary glyphs
    replace the previous text; when the character being drawn is a space on a
    partially transparent background, the existing glyph is preserved and its
    colors are tinted instead. Foreground-only transparency is ignored because
    terminals render the glyph itself, not a tinted version of the destination
    background.

    Graphemes that do not fully fit in the remaining columns clear the rest of
    the row with styled spaces instead of drawing a truncated cluster. This
    matches how hardware terminals treat wide glyphs.

    Graphemes are stored directly as character codes if single-width ASCII
    (32-126 except 127), otherwise interned in the glyph pool.

    Time complexity: O(grapheme_count × width_calculation_cost). Width
    calculation is O(1) for ASCII, O(codepoint_count) for complex emoji. *)

module Border = Border

val draw_box :
  t ->
  x:int ->
  y:int ->
  width:int ->
  height:int ->
  border_chars:Border.t ->
  border_sides:Border.side list ->
  border_style:Ansi.Style.t ->
  bg_color:Ansi.Color.t ->
  should_fill:bool ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  ?title_style:Ansi.Style.t ->
  unit ->
  unit
(** [draw_box t ~x ~y ~width ~height ~border_chars ~border_sides ~border_style
     ~bg_color ~should_fill ?title ?title_style ()] draws a box with Unicode
    borders.

    @param x Left edge of the box.
    @param y Top edge of the box.
    @param width Box width in cells (must be ≥ 2 for visible borders).
    @param height Box height in cells (must be ≥ 2 for visible borders).
    @param border_chars
      Character set for drawing borders (corners, edges, etc.).
    @param border_sides List of sides to draw (empty list draws no borders).
    @param border_style
      Style for border characters. Note: any background color in [border_style]
      is ignored; border cells use [bg_color] for their background.
    @param bg_color
      Background color for the interior (when [should_fill] is true) and for
      border cells.
    @param should_fill
      If [true], fills the interior with [bg_color]. When [false], leaves
      interior cells untouched.
    @param title Optional title text displayed on the top border.
    @param title_alignment
      Optional title alignment: [`Left] (default), [`Center], or [`Right].
      Left/right use a 2‑cell padding from the edge.
    @param title_style
      Optional style for title text. Defaults to [border_style].

    Drawing respects the scissor stack and silently skips cells outside the grid
    bounds. When [should_fill] is [true], the box interior is filled with
    [bg_color] before borders are rendered. Titles are placed on the top border
    only when [`Top] is included and [width >= title width + 4]; otherwise they
    are dropped. The title is padded by two cells on each side, clamped to the
    box width, and defaults to [border_style] when [title_style] is omitted. *)

type line_glyphs = {
  h : string;  (** Horizontal segment glyph (e.g., ["─"]). *)
  v : string;  (** Vertical segment glyph (e.g., ["│"]). *)
  diag_up : string;  (** Diagonal up-right glyph (e.g., ["╱"]). *)
  diag_down : string;  (** Diagonal down-right glyph (e.g., ["╲"]). *)
}
(** Glyph set for line rendering.

    Used by {!draw_line} to customize which characters are used for different
    line segment orientations. *)

val default_line_glyphs : line_glyphs
(** Default Unicode box-drawing glyphs: ["─"], ["│"], ["╱"], ["╲"]. *)

val ascii_line_glyphs : line_glyphs
(** ASCII-compatible glyphs: ["-"], ["|"], ["/"], ["\\"]. *)

val draw_line :
  t ->
  x1:int ->
  y1:int ->
  x2:int ->
  y2:int ->
  ?style:Ansi.Style.t ->
  ?glyphs:line_glyphs ->
  ?kind:[ `Line | `Braille ] ->
  unit ->
  unit
(** [draw_line t ~x1 ~y1 ~x2 ~y2 ?style ?glyphs ?kind ()] draws a line from
    [(x1, y1)] to [(x2, y2)].

    Uses Bresenham's line algorithm for efficient rasterization.

    @param style Line style. Default is {!Ansi.Style.default}.
    @param glyphs
      Glyph set for line segments. Default is {!default_line_glyphs}. Ignored
      when [kind] is [`Braille].
    @param kind
      Rendering mode. [`Line] uses box-drawing characters with per-step glyph
      selection based on the Bresenham step direction (horizontal, vertical, or
      diagonal). [`Braille] uses 2×4 Braille dot patterns for higher resolution.
      Default is [`Line].

    In [`Line] mode, each step of the algorithm chooses the appropriate glyph
    based on whether it moves horizontally, vertically, or both (diagonal). This
    produces visually correct lines at all slopes, including shallow angles
    where most segments should be horizontal with occasional vertical drops.

    In [`Braille] mode, dots are accumulated into a buffer and then merged with
    any existing Braille patterns in the grid cells. This allows multiple line
    segments to share cells without overwriting each other's dots.

    Drawing respects the scissor stack; cells outside the active scissor are not
    modified. *)

(** {1 Direct Cell Access} *)

val set_cell :
  t ->
  x:int ->
  y:int ->
  code:int ->
  fg:Ansi.Color.t ->
  bg:Ansi.Color.t ->
  attrs:Ansi.Attr.t ->
  ?link:string ->
  unit ->
  unit
(** [set_cell t ~x ~y ~code ~fg ~bg ~attrs] writes a single cell
    unconditionally.

    Cells falling outside the grid or the active scissor are ignored. Any
    existing grapheme that spans this cell is cleaned up so continuation cells
    and reference counts remain consistent. When you write the start cell of a
    multi-column grapheme, you are responsible for writing the matching
    continuation cells yourself; prefer {!draw_text} unless you need this
    low-level control. *)

val set_cell_alpha :
  t ->
  x:int ->
  y:int ->
  code:int ->
  fg:Ansi.Color.t ->
  bg:Ansi.Color.t ->
  attrs:Ansi.Attr.t ->
  ?link:string ->
  unit ->
  unit
(** [set_cell_alpha t ~x ~y ~code ~fg ~bg ~attrs] writes a single cell using
    alpha blending unconditionally.

    Blends [fg] and [bg] with the existing cell colors regardless of
    {!respect_alpha}, ensuring translucent overlays behave consistently even
    when the grid normally treats writes as opaque. Cells falling outside the
    grid or the active scissor are ignored.

    As with {!set_cell}, this function only updates the cell at [(x, y)].
    Multi-column graphemes must be written with matching continuation cells by
    the caller; existing graphemes spanning this cell are cleaned up so pool
    reference counts remain correct. *)

(** {1 Scissor Clipping} *)

val push_scissor : t -> clip_rect -> unit
(** [push_scissor t rect] pushes a clipping rectangle onto the stack.

    Subsequent drawing operations ({!set_cell_alpha}, {!draw_text},
    {!fill_rect}) are clipped to [rect]. The most recently pushed scissor
    becomes active, completely replacing any previous scissor. Supplying a
    rectangle with non-positive width or height effectively disables rendering
    until it is popped.

    The scissor remains active until removed with {!pop_scissor} or
    {!clear_scissor}. *)

val pop_scissor : t -> unit
(** [pop_scissor t] removes the most recently pushed clipping rectangle.

    If the scissor stack is empty, this is a no-op. After popping, the previous
    scissor (if any) becomes active. *)

val clear_scissor : t -> unit
(** [clear_scissor t] removes all clipping rectangles.

    Drawing operations are no longer clipped. Equivalent to popping all
    scissors. *)

val with_scissor : t -> clip_rect -> (unit -> 'a) -> 'a
(** [with_scissor t rect f] executes [f] with [rect] as the active scissor.

    Pushes [rect], calls [f ()], then pops [rect] before returning. The scissor
    is popped even if [f] raises an exception.

    This is the recommended way to use scissor clipping for scoped drawing. *)

(** {1 Scrolling Operations}

    Efficient scrolling primitives for terminal emulation. These are optimized
    for the common case of scrolling content within a rectangular region,
    automatically handling overlapping memory operations. *)

val scroll_up : t -> top:int -> bottom:int -> n:int -> unit
(** [scroll_up t ~top ~bottom ~n] scrolls the region \[top..bottom\] up by [n]
    lines.

    Lines scrolled off the top are lost. New blank lines (filled with
    transparent black) appear at the bottom. This is equivalent to the terminal
    operation that happens when output reaches the bottom of the screen.

    @param top Top row of scroll region (inclusive, 0-based).
    @param bottom Bottom row of scroll region (inclusive, 0-based).
    @param n Number of lines to scroll (clamped to region height).

    Complexity: O(width × height) with optimized bulk memory operations. Writes
    are clipped by the active scissor because the implementation delegates to
    {!blit_region} and {!fill_rect}.

    Preconditions:
    - [0 <= top <= bottom < height]
    - [n > 0]

    Invalid parameters (including [n <= 0]) are ignored (no-op). *)

val scroll_down : t -> top:int -> bottom:int -> n:int -> unit
(** [scroll_down t ~top ~bottom ~n] scrolls the region \[top..bottom\] down by
    [n] lines.

    Lines scrolled off the bottom are lost. New blank lines appear at the top.
    This is the reverse of {!scroll_up}, used for reverse index (RI) terminal
    operations.

    @param top Top row of scroll region (inclusive, 0-based).
    @param bottom Bottom row of scroll region (inclusive, 0-based).
    @param n Number of lines to scroll (clamped to region height).

    Complexity: O(width × height) with optimized bulk memory operations. Handles
    overlapping memory regions correctly and respects the active scissor.

    Preconditions: Same as {!scroll_up}. Passing [n <= 0] leaves the grid
    unchanged. *)

(** {1 Grid Comparison} *)

val diff_cells : t -> t -> (int * int) array
(** [diff_cells prev curr] returns the array of cell coordinates that differ
    between [prev] and [curr]. Cells outside either grid's bounds are treated as
    empty cells. The function iterates over the union of both grids' dimensions,
    so any cell that exists in one grid but not the other, or has different
    content, is reported.

    Colors are compared using a small epsilon to ignore float32 rounding noise
    in the RGBA planes. The returned array is sorted by row, then column. *)

(** {1 Snapshotting} *)

val snapshot : ?reset:bool -> t -> string
(** [snapshot ?reset grid] renders the entire grid to a string containing full
    ANSI escape sequences (as if rendered with `Full mode).

    The output includes:
    - All rows from row 0 to [height-1]
    - Proper style handling (truecolor RGB, attributes, hyperlinks)
    - Correct rendering of wide/complex graphemes
    - A final reset sequence if [reset] is [true] (default)

    This is useful for debugging, logging, tests, or generating static ANSI
    output from a grid without a [Screen.t]. It matches exactly what
    [Screen.render ~full:true] would produce (ignoring cursor/mouse state). *)
