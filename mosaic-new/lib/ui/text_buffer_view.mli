(** Viewport and wrapping control for text buffers.

    Text_buffer_view provides viewport-aware rendering and text wrapping over
    {!Text_buffer}. It transforms logical lines into virtual lines based on
    wrapping configuration and manages viewport clipping for efficient
    rendering.

    {1 Overview}

    A view maintains:

    - Wrapping configuration ([wrap_mode], [wrap_width])
    - Viewport bounds (optional region to render)
    - Tab display options (indicator character and color)
    - Selection state (linear or local coordinate-based)
    - Cached virtual line data (updated lazily on access)

    Virtual lines are computed on-demand and cached until the underlying buffer
    changes or wrapping parameters are modified.

    {1 Wrapping}

    Text wrapping transforms logical lines into visual lines that fit within
    [wrap_width]:

    - [`None]: No wrapping; lines extend indefinitely.
    - [`Char]: Break at character boundaries when exceeding [wrap_width].
    - [`Word]: Break at word boundaries when exceeding [wrap_width], falling
      back to character boundaries for words longer than [wrap_width].

    When [wrap_mode] is [`None], [wrap_width] is ignored. Negative or zero
    [wrap_width] values are normalized to 1.

    {1 Viewports}

    A viewport defines a rectangular region [(x, y, width, height)] for
    rendering. Virtual lines outside this region are excluded from
    {!virtual_lines}. The viewport affects:

    - Vertical clipping: Only lines within \[y, y + height) are returned.
    - Horizontal offset: In [`None] wrap mode, [x] shifts the column origin.

    Viewports are optional; [None] renders all content without clipping.

    {1 Selections}

    Selections are specified in one of two forms:

    - Linear: A character range [(start, stop)] over the buffer's flat character
      array.
    - Local: Anchor and focus coordinates
      [(anchor_x, anchor_y, focus_x, focus_y)] in viewport-relative positions.

    Local selections are converted to linear ranges based on virtual line layout
    and viewport offsets. *)

type wrap_mode = [ `None | `Char | `Word ]
(** Text wrapping behavior. *)

type viewport = { x : int; y : int; width : int; height : int }
(** Viewport rectangle for rendering. All dimensions are non-negative after
    normalization. *)

type measure_info = { line_count : int; max_width : int }
(** Wrapped line metrics: total virtual lines and maximum visual width. *)

type t
(** Viewport state over a text buffer. *)

val create : Text_buffer.t -> t
(** [create buffer] creates a view over [buffer].

    Initial state:

    - Wrap mode: [`Word]
    - Wrap width: [None]
    - Viewport: [None] (unbounded)
    - Tab indicator: [None]
    - Selection: [None] *)

val buffer : t -> Text_buffer.t
(** [buffer view] returns the underlying text buffer. *)

val set_wrap_mode : t -> wrap_mode -> unit
(** [set_wrap_mode view mode] updates the wrapping strategy.

    Virtual lines are recomputed on the next access if [mode] differs from the
    current setting. *)

val set_wrap_width : t -> int option -> unit
(** [set_wrap_width view width] sets the wrapping width.

    [None] disables width-based wrapping. Non-positive values are clamped to 1.
    When [wrap_mode] is [`None], this setting has no effect.

    Virtual lines are recomputed on the next access if [width] changes. *)

val wrap_mode : t -> wrap_mode
(** [wrap_mode view] returns the current wrapping strategy. *)

val wrap_width : t -> int option
(** [wrap_width view] returns the wrapping width after normalization. *)

val set_viewport : t -> viewport option -> unit
(** [set_viewport view viewport] configures the rendering region.

    [None] disables viewport clipping. Dimensions are clamped to non-negative
    values when constructing the viewport. *)

val set_viewport_size : t -> width:int -> height:int -> unit
(** [set_viewport_size view ~width ~height] updates viewport dimensions.

    If no viewport exists, one is created at [(0, 0)] with the specified size.
    Negative values are clamped to 0. *)

val viewport : t -> viewport option
(** [viewport view] returns the current viewport, or [None] if unbounded. *)

val set_tab_indicator : t -> int option -> unit
(** [set_tab_indicator view codepoint] sets the character used to visualize tab
    characters.

    [None] disables tab indicators. This affects rendering only; it does not
    modify buffer contents. *)

val tab_indicator : t -> int option
(** [tab_indicator view] returns the tab indicator codepoint, or [None]. *)

val set_tab_indicator_color : t -> Ansi.Color.t option -> unit
(** [set_tab_indicator_color view color] sets the color for tab indicators.

    [None] uses the default text color. *)

val tab_indicator_color : t -> Ansi.Color.t option
(** [tab_indicator_color view] returns the tab indicator color, or [None]. *)

val virtual_lines : t -> Text_buffer.Virtual_line.t array
(** [virtual_lines view] computes and returns virtual lines within the viewport.

    Virtual lines are lazily computed and cached. The cache invalidates when the
    buffer version changes or wrapping parameters are modified.

    If a viewport is active, only lines within \[y, y + height) are returned.
    Otherwise, all virtual lines are returned. *)

val line_info : t -> Text_buffer.line_info
(** [line_info view] returns line metadata for virtual lines within the
    viewport.

    Line starts are global character offsets. Widths reflect visual column
    spans. [max_width] is the maximum width among viewport-visible lines, or the
    logical [max_width] if no viewport is set.

    Returned arrays are copies; modifications do not affect the view. *)

val logical_line_info : t -> Text_buffer.line_info
(** [logical_line_info view] returns line metadata for unwrapped logical lines
    from the underlying buffer.

    This is equivalent to [Text_buffer.logical_line_info view.buffer]. *)

val measure_info : t -> measure_info
(** [measure_info view] returns wrapped line metrics (count and max width)
    ignoring viewport slicing. *)

val measure_for_dimensions : t -> width:int -> height:int -> measure_info
(** [measure_for_dimensions view ~width ~height] returns wrapped line metrics
    after applying the given dimensions as wrapping hints.

    - When [wrap_mode] is [`None], [width] is ignored.
    - A non-positive [width] disables wrapping (treated as [None]).
    - [height] is accepted for API consistency but does not affect the result;
      vertical measurement depends only on line count after wrapping. *)

val find_visual_line_index : t -> logical_row:int -> logical_col:int -> int
(** [find_visual_line_index view ~logical_row ~logical_col] maps a logical
    position to a virtual line index.

    Virtual lines are recomputed if necessary. [logical_row] is clamped to valid
    bounds. [logical_col] is matched against virtual line column ranges to
    determine which wrapped segment contains the position.

    Returns the index of the virtual line containing the position, or the last
    line index if [logical_col] exceeds all line widths. *)

val selection : t -> Text_buffer.Selection.t
(** [selection view] returns the current selection state. *)

val set_selection : t -> Text_buffer.Selection.t -> unit
(** [set_selection view selection] updates the selection.

    If [selection] is [Local local], the local coordinates are cached separately
    for later adjustment. *)

val clear_selection : t -> unit
(** [clear_selection view] resets the selection to [None]. *)

val set_local_selection :
  t ->
  anchor_x:int ->
  anchor_y:int ->
  focus_x:int ->
  focus_y:int ->
  style:Ansi.Style.t ->
  bool
(** [set_local_selection view ~anchor_x ~anchor_y ~focus_x ~focus_y ~style] sets
    a local selection using viewport-relative coordinates.

    Coordinates are adjusted by the viewport offset and converted to a linear
    character range based on virtual line layout. Returns [true] if the
    selection changed, [false] otherwise.

    If the computed character range is empty or invalid, the selection is set to
    [None]. *)

val reset_local_selection : t -> unit
(** [reset_local_selection view] clears the cached local selection coordinates.

    The selection itself is not modified; use {!clear_selection} to fully reset
    the selection state. *)

val selection_bounds : t -> (int * int) option
(** [selection_bounds view] computes the character range of the current
    selection.

    Returns [Some (start, stop)] where [start < stop], or [None] if no valid
    selection exists. For local selections, the bounds are computed from virtual
    line positions and viewport offsets. *)

val selection_style : t -> Ansi.Style.t option
(** [selection_style view] returns the style applied to the selection, or [None]
    if no selection exists. *)

val get_selected_text : t -> string
(** [get_selected_text view] extracts the text within the current selection.

    Returns the empty string if no selection exists or the selection range is
    invalid. *)

val position_to_index : t -> x:int -> y:int -> int
(** [position_to_index view ~x ~y] maps viewport-relative coordinates to a
    buffer character index.

    [x] and [y] are adjusted by the viewport offset. [y] is clamped to valid
    virtual line indices. Within the selected line, [x] is matched to the
    character whose visual column range contains it, accounting for tab
    expansion and zero-width characters.

    Returns the character index at the position, or the line-end index if [x]
    exceeds the line width. *)
