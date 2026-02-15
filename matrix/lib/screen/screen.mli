(** Terminal rendering with zero-allocation frame building.

    Provides a declarative, composable API for building and rendering terminal
    frames with efficient in-place buffer mutation, differential rendering, and
    optional post-processing effects.

    {1 Overview}

    This module manages terminal rendering through a double-buffered
    architecture. Frames are built by mutating a next buffer, then diffed
    against the current buffer to generate minimal ANSI escape sequences.
    Post-processing transforms can be registered to apply effects like
    animations or visual filters before diffing occurs.

    The API is designed for zero allocations during frame building. {!build}
    returns the same screen value, so you can compose operations with [|>] while
    still mutating the next buffer in-place.

    {1 Usage Basics}

    Create a screen and build frames:
    {[
      let screen = create () in
      let output =
        build screen ~width:80 ~height:24 (fun grid hits ->
            Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello, world!")
        |> render
      in
      Unix.write Unix.stdout (Bytes.of_string output) 0 (String.length output)
    ]}

    Register interaction regions for mouse handling:
    {[
      build screen ~width:80 ~height:24 (fun grid hits ->
          Grid.draw_text grid ~x:10 ~y:5 ~text:"[ OK ]";
          Hit_grid.add hits ~x:10 ~y:5 ~width:6 ~height:1 ~id:42)
      |> render
    ]}

    Chain post-processing effects:
    {[
      let frame =
        build screen ~width:80 ~height:24 draw_ui
        |> post_process fade_effect |> post_process blur_effect
      in
      render frame
    ]}

    {1 Key Concepts}

    {2 Double Buffering}

    The screen maintains two grid buffers: current and next. During {!build},
    you populate the next buffer. During {!render}, the next buffer is diffed
    against current to generate minimal ANSI output, then the buffers swap. This
    enables efficient incremental rendering - only changed cells produce output.

    {2 Hit Testing}

    The screen maintains hit grids alongside visual grids, mapping screen
    coordinates to element IDs for mouse interaction. Call {!Hit_grid.add}
    during frame building to register clickable regions. Query coordinates with
    {!query_hit} to identify which element was clicked.

    {2 Post-Processing}

    Post-processors are persistent functions that transform the next buffer
    after building but before diffing. They receive the grid and delta time
    since the last frame, enabling time-based animations. Processors run in
    insertion order each frame until removed.

    {1 Invariants}

    - The [Grid.t] passed to {!build} and {!grid} is the **next** buffer. After
      each {!render}, the buffers swap and the (now-next) buffer is cleared, so
      you start from a blank frame every time. If you want to preserve content
      across frames, copy it explicitly (for example with {!Grid.blit}) before
      mutating.
    - Visual and hit buffers swap on {!render}: regions added via
      {!Hit_grid.add} (or {!add_hit_region}) become queryable only after the
      following render, and mutating a previously returned grid/hit grid after
      rendering corrupts the diff baseline.
    - Post-processors always run after the builder and before diffing, even if
      the frame would otherwise render no changes. Their [~delta] argument is
      the milliseconds since the last {!render} call (0. for the first frame).
    - Input toggles set through {!set_mouse_enabled} and {!set_cursor_visible}
      update state only; rendering does not emit terminal mode changes.

    {1 Performance Characteristics}

    Frame building: O(1) allocations (zero with careful usage). Rendering:
    O(changed cells) for diffing, O(output size) for ANSI generation.
    Post-processing: O(processors × grid cells) worst case, typically O(changed
    cells). Hit testing: O(1) lookup, O(region area) registration.

    The functional API compiles to direct pointer passing. [build] returns the
    same screen so you can compose operations with [|>] while still mutating the
    next buffer in-place. *)

module Hit_grid = Hit_grid

(** {1 Types} *)

type t
(** Opaque screen state.

    Manages double-buffered grids, hit grids, post-processing pipeline,
    statistics collection, and frame rate control. Create with {!create}. *)

type stats = { frame_count : int; total_cells : int; total_bytes : int }
(** Cumulative rendering statistics.

    - [frame_count]: Total frames rendered since the screen was created (or
      since {!reset}).
    - [total_cells]: Sum of all cells diffed across frames.
    - [total_bytes]: Sum of all ANSI bytes emitted (via {!render},
      [render_full], or streaming variants). *)

type frame_metrics = {
  frame_count : int;
  cells : int;
  bytes : int;
  frame_time_ms : float;
  interval_ms : float;
  reset_ms : float;
  overall_frame_ms : float;
  frame_callback_ms : float;
  stdout_ms : float;
  mouse_enabled : bool;
  cursor_visible : bool;
  timestamp_s : float;
}
(** Snapshot of the most recent frame.

    - [frame_count]: Index (1-based) of the emitted frame.
    - [cells]: Cells diffed in the frame.
    - [bytes]: ANSI bytes written for the frame.
    - [frame_time_ms]: Diff/render duration in milliseconds.
    - [interval_ms]: Time since the previous render.
    - [reset_ms]: Buffer swap/reset duration.
    - [overall_frame_ms]: Wall-clock duration of [render_frame].
    - [frame_callback_ms]: Time spent executing the frame builder callback.
    - [stdout_ms]: Time spent writing the rendered bytes to the output.
    - [mouse_enabled]/[cursor_visible]: Desired input flags captured at render.
    - [timestamp_s]: [Unix.gettimeofday] timestamp when rendering finished. *)

(** {1 Screen Creation} *)

val create :
  ?glyph_pool:Glyph.pool ->
  ?width_method:Glyph.width_method ->
  ?respect_alpha:bool ->
  ?mouse_enabled:bool ->
  ?cursor_visible:bool ->
  ?explicit_width:bool ->
  unit ->
  t
(** [create ?glyph_pool ?width_method ?respect_alpha ?mouse_enabled
     ?cursor_visible ?explicit_width ()] creates a screen.

    @param glyph_pool
      Glyph pool for storing multi-width grapheme clusters. Defaults to a fresh
      pool. Share a pool across screens to reduce memory for common text.
    @param width_method
      Character width computation method. Defaults to [`Unicode]. See
      {!Glyph.width_method} for options.
    @param respect_alpha
      Whether to apply alpha blending when drawing cells. Defaults to [false].
      Enable for semi-transparent overlays at a small performance cost.
    @param mouse_enabled Whether mouse input is considered enabled.
    @param cursor_visible Initial cursor visibility. Defaults to [true].
    @param explicit_width
      Emit explicit-width OSC sequences for graphemes when [true]. Defaults to
      [false].

    The screen's grids start at minimal (1×1) dimensions and are resized
    automatically on the first {!build} call. Statistics and post-processing
    state are initialized from the provided options. *)

(** {1 Frame Building} *)

val build : t -> width:int -> height:int -> (Grid.t -> Hit_grid.t -> unit) -> t
(** [build screen ~width ~height f] builds a frame with visual and hit regions.

    Resizes the screen's buffers to [width] × [height] (when both dimensions are
    positive), clears the hit grid, then calls [f] with the next grid and hit
    grid for in-place mutation. Returns a frame handle for rendering or further
    transformation.

    @param width
      Frame width in cells. When [width <= 0], [f] is not called, the visual
      buffer is left untouched, and only the hit grid is cleared.
    @param height
      Frame height in cells. When [height <= 0], behavior matches the
      [width <= 0] case.
    @param f
      Builder function receiving [grid] and [hits]. All mutations affect the
      screen's next buffer, which becomes current on {!render}. Treat the
      arguments as ephemeral handles: if you retain them, do not mutate them
      after the next {!render}, because the buffers become the diff baseline.

    Resizing preserves existing grid content where dimensions overlap. Since
    {!render} clears the next buffer after swapping, you typically start each
    frame with a blank grid. The hit grid is always cleared before invoking [f].

    {4 Example}

    Build a frame with clickable button:
    {[
      build screen ~width:80 ~height:24 (fun grid hits ->
          Grid.draw_text grid ~x:10 ~y:5 ~text:"[ OK ]";
          Hit_grid.add hits ~x:10 ~y:5 ~width:6 ~height:1 ~id:42)
    ]} *)

(** {1 Shared Resources} *)

val glyph_pool : t -> Glyph.pool
(** [glyph_pool t] returns the glyph pool shared by the screen's grids. *)

(** {1 Frame Transformation} *)

val post_process : (Grid.t -> delta:float -> unit) -> t -> t
(** [post_process f frame] registers a persistent post-processing transform.

    The function [f] is called during each {!render} after frame building but
    before diffing. It receives the next grid and delta time in milliseconds
    since the last render (0. on the first frame). Processors run in insertion
    order and persist across frames until removed, even if no grid cells change
    in a given frame.

    @param f
      Post-processing function. Receives [grid] (the next buffer) and [~delta]
      (milliseconds since last frame). Mutates [grid] in-place.

    {4 Example}

    Chain fade and noise effects:
    {[
      let fade_fn grid ~delta:_ = apply_fade grid in
      let noise_fn grid ~delta = animated_noise grid ~delta in
      build screen ~width:80 ~height:24 draw_scene
      |> post_process fade_fn |> post_process noise_fn |> render
    ]} *)

val remove_post_process : (Grid.t -> delta:float -> unit) -> t -> t
(** [remove_post_process f frame] unregisters a post-processor.

    Removes every occurrence of [f] using physical equality ([==]). Returns
    [frame] for chaining.

    Store function references to enable removal:
    {[
      let blur grid ~delta:_ = apply_blur grid in
      let frame = post_process blur frame in
      (* Later... *)
      remove_post_process blur frame
    ]} *)

val clear_post_processes : t -> t
(** [clear_post_processes frame] removes all post-processing functions.

    Returns [frame] for chaining. *)

val add_hit_region :
  t -> x:int -> y:int -> width:int -> height:int -> id:int -> t
(** [add_hit_region frame ~x ~y ~width ~height ~id] registers a hit region.

    Convenience wrapper around {!Hit_grid.add} for registering regions after
    frame building. Useful for overlays, popups rendered directly via {!grid},
    or any situation where hit regions must change without rebuilder access.

    @param x Region left edge (cell coordinates).
    @param y Region top edge (cell coordinates).
    @param width Region width in cells (negative clamped to zero).
    @param height Region height in cells (negative clamped to zero).
    @param id
      Unique identifier for this region. Use 0 to clear regions. Queries return
      this ID when coordinates fall within the region.

    Regions outside grid bounds are clipped. Returns [frame] for chaining.

    {4 Example}

    Add multiple clickable regions:
    {[
      build_visual screen ~width:80 ~height:24 draw_base
      |> add_hit_region ~x:0 ~y:0 ~width:10 ~height:1 ~id:1
      |> add_hit_region ~x:0 ~y:2 ~width:10 ~height:1 ~id:2
    ]} *)

(** {1 Rendering} *)

val render : ?full:bool -> ?height_limit:int -> t -> string
(** [render ?full ?height_limit frame] generates ANSI escape sequences for
    terminal output.

    Applies post-processors with delta time (milliseconds since last render),
    performs differential rendering (or full rendering if [full] is [true]),
    then swaps buffers so the next grid becomes the current grid. Hit regions
    registered during this frame become queryable via {!query_hit} after this
    call returns.

    @param full
      When [true], renders all cells regardless of changes. When [false]
      (default), renders only cells that differ from the current buffer.
    @param height_limit
      When provided, limits rendering to the first [height_limit] rows of the
      grid.

    Returns the ANSI output as a string. Allocates a 65536-byte internal buffer.
    For large outputs, use {!render_to_bytes} with a sized buffer. *)

val render_to_bytes : ?full:bool -> ?height_limit:int -> t -> Bytes.t -> int
(** [render_to_bytes ?full ?height_limit frame bytes] renders into the given
    bytes buffer.

    Applies post-processors with delta time, performs differential rendering (or
    full rendering if [full] is [true]), then swaps buffers so the next grid
    becomes the current grid.

    @param full
      When [true], renders all cells regardless of changes. When [false]
      (default), renders only cells that differ from the current buffer.
    @param height_limit
      When provided, limits rendering to the first [height_limit] rows of the
      grid. This is useful for clamping output to a smaller viewport without
      resizing buffers.
    @param bytes
      Output buffer for ANSI escape sequences. Must be large enough to hold the
      rendered output. The underlying writer may raise or truncate if the buffer
      is too small.

    Returns the number of bytes written to [bytes]. *)

(** {1 Screen State} *)

val set_mouse_enabled : t -> bool -> unit
(** [set_mouse_enabled screen enabled] updates the desired mouse-enabled state.
*)

val set_cursor_visible : t -> bool -> unit
(** [set_cursor_visible screen visible] updates the desired cursor visibility
    state stored on the screen. *)

val set_explicit_width : t -> bool -> unit
(** [set_explicit_width screen enabled] toggles explicit-width OSC emission for
    graphemes.

    When enabled and the terminal supports it, emits OSC sequences specifying
    the exact width of multi-width characters to prevent terminal-side width
    mismatch. Useful for ensuring consistent display of wide characters (e.g.,
    emoji, CJK glyphs) across terminals with varying Unicode width tables. *)

val set_cursor_position : t -> row:int -> col:int -> unit
(** [set_cursor_position screen ~row ~col] configures the desired cursor
    coordinates.

    Coordinates are 1-based terminal coordinates where [(1, 1)] is the top-left
    corner. The [row_offset] (set via {!set_row_offset}) is applied by the
    caller when emitting cursor movement. *)

val clear_cursor_position : t -> unit
(** [clear_cursor_position screen] clears any requested cursor position so the
    terminal cursor remains wherever the diff body last moved it. *)

val set_cursor_style :
  t -> style:[ `Block | `Line | `Underline ] -> blinking:bool -> unit
(** [set_cursor_style screen ~style ~blinking] configures the cursor's visual
    style.

    @param style
      Cursor shape: [`Block] for a filled block, [`Line] for a vertical bar, or
      [`Underline] for a horizontal line.
    @param blinking Whether the cursor blinks.

    {4 Example}

    Configure a non-blinking vertical bar cursor:
    {[
      set_cursor_style screen ~style:`Line ~blinking:false
    ]} *)

val set_cursor_color : t -> r:int -> g:int -> b:int -> unit
(** [set_cursor_color screen ~r ~g ~b] sets the cursor color via OSC 12. Inputs
    outside [0,255] are clamped. *)

val reset_cursor_color : t -> unit
(** [reset_cursor_color screen] restores the terminal's default cursor color. *)

(** {1 Cursor State} *)

type cursor_info = {
  row : int;
  col : int;
  has_position : bool;
  style : [ `Block | `Line | `Underline ];
  blinking : bool;
  color : (int * int * int) option;
  visible : bool;
}
(** Snapshot of the desired cursor state. *)

val cursor_info : t -> cursor_info
(** [cursor_info screen] returns the current desired cursor state. *)

val apply_capabilities :
  t ->
  explicit_width:bool ->
  explicit_cursor_positioning:bool ->
  hyperlinks:bool ->
  unit
(** [apply_capabilities screen ~explicit_width ~explicit_cursor_positioning
      ~hyperlinks]
    applies terminal capability flags.

    @param explicit_width
      Whether the terminal supports explicit-width OSC sequences. When [true]
      and {!set_explicit_width} is enabled, OSC sequences are emitted.
    @param explicit_cursor_positioning
      Whether to reposition the cursor after wide graphemes as a fallback when
      [explicit_width] is unavailable. Prevents column drift in terminals that
      miscalculate grapheme display widths. Only effective when [explicit_width]
      is [false].
    @param hyperlinks
      Whether the terminal supports OSC 8 hyperlinks. When [true], hyperlink
      attributes are rendered as clickable links. *)

val set_width_method : t -> Glyph.width_method -> unit
(** [set_width_method screen method_] sets the grapheme width computation method
    on BOTH the current and next buffers.

    Use this after terminal capability changes that affect width calculation
    semantics to keep buffers consistent across swaps. *)

val resize : t -> width:int -> height:int -> unit
(** [resize screen ~width ~height] resizes all internal buffers.

    Resizes both current and next grids plus hit grids to [width] × [height].
    Grid contents are preserved where dimensions overlap; new cells are
    initialized to spaces with a transparent background. Hit grids are resized
    and cleared. This operation updates dimensions in place without
    auto-clearing the framebuffer content that remains.

    Normally unnecessary — {!build} resizes automatically. Use this to pre-size
    buffers for a known terminal size.

    @param width New width in cells. Must be strictly positive.
    @param height New height in cells. Must be strictly positive.

    Raises [Invalid_argument] if [width <= 0] or [height <= 0]. *)

val reset : t -> unit
(** [reset screen] clears buffers and statistics.

    Clears the next grid to transparent black, empties hit grids, zeros
    statistics, and resets frame timing. The current grid (diff baseline) is
    left intact so the next render can efficiently clear previously rendered
    content. Useful for clearing artifacts or starting a fresh rendering session
    without losing knowledge of what was last on-screen.

    Post-processors and configuration are preserved. *)

(** {1 Statistics and Debugging} *)

val stats : t -> stats
(** [stats screen] returns cumulative rendering statistics since creation or the
    last {!reset}. *)

val last_metrics : t -> frame_metrics
(** [last_metrics screen] returns metrics for the most recent frame. *)

val record_runtime_metrics :
  t ->
  frame_callback_ms:float ->
  overall_frame_ms:float ->
  stdout_ms:float ->
  unit
(** [record_runtime_metrics screen ~frame_callback_ms ~overall_frame_ms
     ~stdout_ms] supplements the most recent metrics with runtime measurements.

    This is intended for higher-level runtimes (e.g. {!Matrix.run}) that can
    measure draw-call duration, total wall-clock time, and output flush time on
    behalf of the screen. Values are in milliseconds. *)

(** {1 Direct Access}

    Direct access to internal buffers for advanced use cases. These functions
    bypass the builder API, useful for performance-critical code, gradual
    migration, or debugging. *)

val grid : t -> Grid.t
(** [grid frame] returns the next buffer grid.

    This is the grid that will be rendered on the next {!render} call. You can
    draw to it directly using {!Grid} functions, bypassing {!build}.

    Mutations persist until the next {!Grid.clear} or {!reset}. Do not mutate
    the returned grid after calling {!render}, because it becomes the current
    buffer used for diff baselines. *)

val hit_grid : t -> Hit_grid.t
(** [hit_grid frame] returns the next hit grid.

    This is the hit grid that will be active after the next {!render}. You can
    register or query hit regions directly using {!Hit_grid} functions. As with
    {!grid}, avoid mutating the returned value once {!render} runs, because it
    becomes the visible baseline for subsequent queries. *)

val query_hit : t -> x:int -> y:int -> int
(** [query_hit frame ~x ~y] returns the element ID at coordinates [(x, y)].

    Queries the CURRENT frame's hit grid (i.e. regions registered during the
    most recent {!render}). Call this after rendering to observe the hit regions
    that are visible on screen. Returns 0 if out of bounds or no region is
    registered at [(x, y)]. *)

val set_row_offset : t -> int -> unit
(** [set_row_offset screen offset] sets the vertical origin offset applied to
    all subsequent renders. Useful when drawing on the primary screen without
    the alternate buffer. Negative offsets are clamped to zero. *)

val row_offset : t -> int
(** [row_offset screen] returns the current vertical origin offset. *)

val invalidate_presented : t -> unit
(** [invalidate_presented screen] clears the "previously presented" buffer,
    making the diff renderer treat all cells as changed on the next render.

    Call this after physically erasing the terminal region (e.g., in primary/
    inline mode) to keep the renderer's baseline in sync with the actual
    terminal state. This maintains the invariant that the [current] buffer
    matches what's on the terminal.

    Does NOT emit any terminal escape sequences - it only affects internal
    state. *)

val active_height : t -> int
(** [active_height screen] computes the effective number of rows containing
    non-blank content in the next buffer. Background-only fills (spaces) are
    ignored. Useful for sizing inline primary rendering regions. *)
