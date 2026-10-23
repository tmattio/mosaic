(** Double-buffered terminal screen.

    [Screen] builds terminal frames by mutating a {e next} buffer, diffs it
    against the {e current} buffer to generate minimal ANSI escape sequences,
    then swaps the two. Grid transforms can be applied after building and before
    rendering.

    {2:double_buf Double buffering}

    The screen maintains two grid buffers. During {!build} the caller populates
    the next buffer. During {!render} the next buffer is diffed against current
    to produce ANSI output, then the buffers swap; the swapped-in next buffer is
    cleared before it is next observed or mutated, so a frame pays for exactly
    one clear. Content must be redrawn every frame.

    {2:hit_test Hit testing}

    A {!Hit_grid.t} is maintained alongside each visual grid, mapping screen
    coordinates to integer element IDs. Call {!Hit_grid.add} during frame
    building to register clickable regions. Query coordinates with {!query_hit}
    after rendering.

    {2:invariants Invariants}
    - The {!Matrix_grid.t} passed to {!build} is the next buffer. After
      {!render} the buffers swap and the now-next buffer is observably cleared:
      the clear is applied lazily by the next {!build}, {!next_grid},
      {!next_hit_grid}, {!add_hit_region}, or {!active_height}.
    - Visual and hit buffers swap on {!render}: regions registered via
      {!Hit_grid.add} become queryable only after the following render.
    - {!set_cursor} updates cursor intent only; rendering does not emit terminal
      mode changes. *)

module Hit_grid = Hit_grid

(** {1:types Types} *)

type t
(** The type for screens. Pairs visual and hit buffers with retained diff state
    and rendering statistics. *)

type stats = {
  frame_count : int;  (** Frames rendered since creation or {!reset}. *)
  total_cells : int;  (** Sum of cells diffed across frames. *)
  total_bytes : int;  (** Sum of ANSI bytes emitted. *)
}
(** The type for cumulative rendering statistics. *)

type frame_metrics = {
  frame_count : int;  (** One-based frame index. *)
  cells : int;  (** Cells diffed. *)
  bytes : int;  (** ANSI bytes written. *)
  cursor_visible : bool;  (** Cursor state at render time. *)
}
(** The type for per-frame metrics. A screen reads no clock: the time a frame
    takes and the time between frames are its caller's to measure. *)

(** {1:constructors Constructors} *)

val create :
  ?width_method:Matrix_text.width_method ->
  ?cursor_visible:bool ->
  ?explicit_width:bool ->
  unit ->
  t
(** [create ~width_method ~cursor_visible ~explicit_width ()] is a screen with:
    - [width_method] is the character width computation method. Defaults to
      [`Unicode].
    - [cursor_visible] is the initial cursor visibility. Defaults to [true].
    - [explicit_width] enables explicit-width OSC sequences for graphemes.
      Defaults to [false].

    Grids start at 1x1 and are resized automatically on the first {!build}. *)

(** {1:building Frame building} *)

val build :
  t -> width:int -> height:int -> (Matrix_grid.t -> Hit_grid.t -> unit) -> unit
(** [build t ~width ~height f] builds a frame.

    Resizes buffers to [width] x [height] when both are positive, clears what a
    previous build drew since the last {!render} — each build owns the complete
    frame, so a second build before a present replaces the superseded pass —
    then calls [f] with them for in-place mutation. Buffers that no build has
    drawn into since they were cleared are not cleared again, even if
    {!next_grid} handed them out: cells drawn through it stay under the build's.

    When [width <= 0] or [height <= 0], [f] is not called; the buffers are only
    cleared.

    {b Warning.} The {!Matrix_grid.t} and {!Hit_grid.t} passed to [f] must not
    be mutated after the next {!render} -- they become the diff baseline. *)

val add_hit_region :
  t -> x:int -> y:int -> width:int -> height:int -> id:int -> unit
(** [add_hit_region t ~x ~y ~width ~height ~id] registers a hit region on the
    next hit grid. Convenience wrapper around {!Hit_grid.add} for use outside
    the {!build} callback. Regions outside grid bounds are clipped. Negative
    dimensions are clamped to zero. *)

(** {1:rendering Rendering} *)

type scroll_hint = {
  top : int;  (** First row of the scroll region (0-indexed). *)
  bottom : int;  (** Last row of the scroll region (0-indexed, inclusive). *)
  delta : int;
      (** Rows to shift. Positive shifts content up (user scrolled down). *)
}
(** A scroll optimisation hint from a scrollable container. When provided, the
    renderer shifts the previous buffer and emits DECSTBM + hardware scroll
    sequences so only the newly-revealed edge rows need cell-level diffing.
    Without the hint, scrolling rewrites every row in the viewport. A shift by
    the region's height or more keeps no row and is ignored. *)

type viewport = {
  y : int;  (** Zero-based terminal row offset of the rendered surface. *)
  height : int;  (** Number of rows in the rendered surface. *)
}
(** A terminal viewport rendered by the screen.

    [None] in {!render} and {!render_into} means render the full next grid at
    the screen's current {!row_offset}. [Some v] renders the first [v.height]
    grid rows at terminal row offset [v.y]. *)

val render :
  ?full:bool -> ?scroll_hint:scroll_hint -> ?viewport:viewport -> t -> string
(** [render ~full ~scroll_hint ~viewport t] is the ANSI output for the current
    frame.

    Diffs next against current (or renders all cells when [full] is [true]),
    then swaps buffers. Hit regions registered during this frame become
    queryable via {!query_hit}.
    - [full] renders all cells regardless of changes. Defaults to [false].
    - [scroll_hint] when provided, emits DECSTBM hardware scroll before diffing.
      The hint is a rectangular diff optimization; the caller is responsible for
      using it only when temporary scroll-region changes are safe for the
      current terminal transaction.
    - [viewport] renders into an explicit terminal surface. Rows outside the
      viewport are not presented and do not become active hit regions.

    [render] copies the retained output into a fresh string every call; hot
    paths should prefer {!render_into}. *)

val render_into :
  ?full:bool ->
  ?scroll_hint:scroll_hint ->
  ?viewport:viewport ->
  t ->
  Ansi.Writer.t ->
  unit
(** [render_into ~full ~scroll_hint ~viewport t writer] is like {!render}, but
    appends output directly to [writer]. The caller owns and resets the writer;
    its existing contents are preserved. Frame byte metrics count only the
    appended output. Reuse the writer to retain output capacity across frames.

    If a write raises, as one to a writer of {!Ansi.Writer.of_bytes} raises
    {!Ansi.Writer.Buffer_full} when its bytes are full, the exception passes
    through and the frame is not committed: the screen is as before the call,
    and the next render emits the same frame. The bytes appended before the
    exception are an unfinished frame, never to be sent. *)

(** {1:screen_state Screen state} *)

val set_explicit_width : t -> bool -> unit
(** [set_explicit_width t b] enables or disables explicit-width OSC emission for
    graphemes. When enabled and the terminal supports it, OSC sequences
    specifying the exact width of multi-width characters are emitted to prevent
    terminal-side width mismatch. *)

type cursor = {
  position : (int * int) option;
      (** Zero-based [(x, y)] cell position, or [None] to leave the cursor at
          the renderer/runtime default position. *)
  style : Ansi.cursor_style;  (** Cursor shape. *)
  blinking : bool;  (** [true] iff the cursor blinks. *)
  color : (int * int * int) option;
      (** [Some (r, g, b)] or [None] for the terminal default. *)
  visible : bool;  (** [true] iff the cursor is logically visible. *)
}
(** The type for cursor state. *)

val set_cursor : t -> cursor -> unit
(** [set_cursor t c] sets the desired cursor state. Colour components outside
    \[[0]; [255]\] are clamped. *)

val cursor : t -> cursor
(** [cursor t] is the current desired cursor state. *)

(** {1:capabilities Capabilities} *)

val apply_capabilities :
  t ->
  explicit_width:bool ->
  explicit_cursor_positioning:bool ->
  hyperlinks:bool ->
  color_depth:Ansi.Sgr_state.color_depth ->
  unit
(** [apply_capabilities t ~explicit_width ~explicit_cursor_positioning
     ~hyperlinks ~color_depth] applies terminal capability flags to [t].
    - [explicit_width]: whether the terminal supports explicit-width OSC
      sequences.
    - [explicit_cursor_positioning]: whether to reposition the cursor after wide
      graphemes as a fallback when [explicit_width] is [false]. Prevents column
      drift in terminals that miscalculate grapheme display widths.
    - [hyperlinks]: whether the terminal supports OSC 8 hyperlinks.
    - [color_depth]: terminal color emission depth. *)

val set_width_method : t -> Matrix_text.width_method -> unit
(** [set_width_method t m] sets the grapheme width computation method on both
    the current and next buffers. Use this after capability changes that affect
    width calculation to keep buffers consistent across swaps. *)

val width_method : t -> Matrix_text.width_method
(** [width_method t] is the grapheme width computation method of [t]'s buffers.
    Unlike reading it from {!next_grid}, it leaves the next buffer as it is. *)

(** {1:layout Layout} *)

val resize : t -> width:int -> height:int -> unit
(** [resize t ~width ~height] resizes all internal buffers to [width] x
    [height]. If the dimensions are unchanged, this is a no-op so the presented
    diff baseline remains intact. Otherwise the baseline no longer says what the
    terminal shows, so the next render paints every cell; hit grids are cleared.

    Raises [Invalid_argument] if [width <= 0] or [height <= 0].

    {b Note.} Normally unnecessary -- {!build} resizes automatically. *)

val reset : t -> unit
(** [reset t] clears the next buffer, empties hit grids and zeros statistics.
    The current buffer (diff baseline) is left intact so the next render can
    efficiently clear previously rendered content. Configuration is preserved.
*)

(** {1:statistics Statistics} *)

val stats : t -> stats
(** [stats t] is cumulative rendering statistics since creation or the last
    {!reset}. *)

val last_metrics : t -> frame_metrics
(** [last_metrics t] is the metrics for the most recent frame. *)

(** {1:direct Direct access}

    Direct access to internal buffers for advanced use cases. These functions
    bypass the builder API. *)

val next_grid : t -> Matrix_grid.t
(** [next_grid t] is [t]'s next buffer grid. On the first access after a
    {!render}, any content left from the presented frame is cleared before the
    grid is returned. Handing it out does not clear it again: what is drawn
    through it is the frame, and a {!build} that follows draws over it, as it
    does over a cleared grid. A second {!build} replaces the first.

    {b Warning.} Do not mutate the returned grid after the next {!render} -- it
    becomes the diff baseline. *)

val current_grid : t -> Matrix_grid.t
(** [current_grid t] is [t]'s presented grid: the committed terminal state as of
    the most recent {!render}. Blank before the first render.

    {b Warning.} Do not mutate the returned grid -- it is the diff baseline. *)

val next_hit_grid : t -> Hit_grid.t
(** [next_hit_grid t] is [t]'s next hit grid, cleared as {!next_grid}'s is.

    {b Warning.} Do not mutate the returned hit grid after the next {!render} --
    it becomes the active hit grid for {!query_hit}. *)

val query_hit : t -> x:int -> y:int -> int
(** [query_hit t ~x ~y] is the element ID at [(x, y)] in the {e current} hit
    grid (i.e. regions from the most recent {!render}). Returns [0] if out of
    bounds or no region is registered. *)

val set_row_offset : t -> int -> unit
(** [set_row_offset t n] sets the vertical origin offset applied to all
    subsequent renders. Negative values are clamped to zero. Useful for inline
    primary-screen rendering. *)

val row_offset : t -> int
(** [row_offset t] is [t]'s current vertical origin offset. *)

val invalidate_presented : t -> unit
(** [invalidate_presented t] sets the presented baseline to blank cells, so the
    next render paints every cell of the frame that is not blank.

    Call this after physically erasing the terminal region to keep the
    renderer's baseline in sync with the actual terminal state.

    {b Note.} Does not emit any escape sequences. *)

val active_height : t -> int
(** [active_height t] is {!Matrix_grid.active_height} of the next buffer: the
    rows down to the last one that shows anything. Useful for sizing inline
    rendering regions. *)
