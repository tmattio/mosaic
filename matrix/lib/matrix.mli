(** Immediate-mode runtime for terminal user interfaces.

    Matrix owns the terminal while an application runs: it switches the TTY into
    raw mode, negotiates mouse/keyboard protocols, builds frames against a
    double-buffered grid, and diffs the grid to emit minimal ANSI output. The
    module re-exports the lower-level subsystems (ANSI generation, grids,
    terminal I/O, input parsing, images) and exposes an immediate-mode runtime
    entry point via {!create}.

    {1 Overview}

    Immediate mode: create a runtime, then either use the high-level {!run}
    loop, or drive the loop manually.

    If driving manually: 1. Call {!prepare} to start a frame (clears buffers,
    updates layout). 2. Read events via {!Terminal.read}. 3. Render into {!grid}
    and {!hits}. 4. Call {!submit} to flush the frame to the terminal. *)

(** {1 Sub-Libraries} *)

module Ansi = Ansi
(** ANSI escape sequence generation. *)

module Glyph = Glyph
(** Glyph management for text rendering. *)

module Grid = Grid
(** Cell-based drawing grid. *)

module Input = Input
(** Terminal input event parsing. *)

module Screen = Screen
(** Zero-allocation frame rendering. *)

module Terminal = Terminal
(** Low-level terminal control. *)

(** {1 Declarative Image API} *)

module Image = Image
(** Declarative image rendering and manipulation. *)

(** {1 Types} *)

type kitty_keyboard = [ `Auto | `Disabled | `Enabled of int ]
(** Kitty keyboard protocol configuration used by {!create}.

    - [`Auto] (default) enables the protocol when the terminal advertises
      support. Modify-other-keys stays enabled otherwise so Ctrl+Alt chords keep
      working even on legacy terminals.
    - [`Disabled] never negotiates the protocol, which avoids the extra escape
      sequences on terminals with broken Kitty implementations.
    - [`Enabled flags] forces activation with the provided bitmask. Flags mirror
      the Kitty protocol specification (see
      {{:https://sw.kovidgoyal.net/kitty/keyboard-protocol/}Kitty keyboard
       protocol} for exact bits). *)

type mode = [ `Alt | `Primary_split | `Primary_inline ]
(** Presentation mode for the renderer:

    - [`Alt] (default) uses the terminal's alternate screen buffer where the app
      fully owns the screen and content is restored on exit.
    - [`Primary_split] reserves a bottom-aligned render region on the primary
      screen; scrollback above remains visible.
    - [`Primary_inline] renders inline below the shell prompt and grows on
      demand, inserting blank lines as needed to avoid overwriting shell output.
*)

type debug_overlay_corner =
  [ `Top_left | `Top_right | `Bottom_left | `Bottom_right ]
(** Corner to anchor the debug overlay when enabled. *)

type app
(** Opaque application runtime handle returned by {!create}. *)

(** {1 Lifecycle} *)

val create :
  ?mode:mode ->
  ?raw_mode:bool ->
  ?target_fps:float option ->
  ?respect_alpha:bool ->
  ?mouse_enabled:bool ->
  ?mouse:Terminal.mouse_mode option ->
  ?bracketed_paste:bool ->
  ?focus_reporting:bool ->
  ?kitty_keyboard:kitty_keyboard ->
  ?exit_on_ctrl_c:bool ->
  ?debug_overlay:bool ->
  ?debug_overlay_corner:debug_overlay_corner ->
  ?debug_overlay_capacity:int ->
  ?frame_dump_every:int ->
  ?frame_dump_dir:string ->
  ?frame_dump_pattern:string ->
  ?frame_dump_hits:bool ->
  ?cursor_visible:bool ->
  ?explicit_width:bool ->
  ?render_thread:bool ->
  ?input_timeout:float option ->
  ?resize_debounce:float option ->
  ?initial_caps:Terminal.capabilities ->
  ?output:[ `Fd of Unix.file_descr | `Stdout ] ->
  ?signal_handlers:bool ->
  unit ->
  app
(** [create ()] starts an immediate-mode runtime and returns its handle.

    The runtime owns the terminal until {!close} is called, managing raw mode,
    alternate screen, input protocols, and frame rendering. All configuration is
    specified via optional parameters organized below by category.

    {4 Display Mode}

    @param mode
      Presentation mode for the renderer. Defaults to [`Alt] (alternate screen).
      See {!mode} for full description of [`Alt], [`Primary_split], and
      [`Primary_inline] modes.
    @param raw_mode
      Whether to switch the TTY into raw mode (disables line buffering and
      echo). Defaults to [true]. Set to [false] for cooked mode if the
      application manages terminal configuration externally.

    {4 Rendering Configuration}

    @param respect_alpha
      Whether to honor alpha blending when rendering cells. Defaults to [false].
      Enable for semi-transparent overlays.
    @param cursor_visible
      Initial cursor visibility. Defaults to [true]. The cursor is hidden
      automatically in [`Alt] mode.
    @param explicit_width
      Whether to use explicit wcwidth values instead of querying the terminal.
      Defaults to the terminal's reported capability. Override when the
      terminal's width computation is known to be incompatible.
    @param render_thread
      Whether to use a dedicated thread for output. Defaults to [true] on macOS
      and [false] on Linux. Enable to prevent blocking the main loop on slow
      terminals, but adds threading overhead.
    @param initial_caps
      Optional seed capabilities passed directly to [Terminal.open_terminal]
      (useful for tests or environments that want to bypass probing).
    @param output
      Output target. Defaults to [`Stdout]. Use [`Fd fd] to write to a specific
      file descriptor (e.g., for splitting output from logs). In
      [`Primary_inline] and [`Primary_split] modes, Matrix captures [stdout]
      into a pipe so stray prints do not corrupt the render region; captured
      output is replayed above the TUI and flushed on shutdown.

    {4 Frame Timing}

    @param target_fps
      Optional FPS cap in Hz. Defaults to [Some 30.]. Limits the maximum frame
      rate to prevent excessive CPU usage. Set to [None] for uncapped rendering.
    @param resize_debounce
      Debounce window in seconds for resize events. Defaults to [Some 0.1].
      Ignored in [`Primary_split] mode. Set to [None] to apply resizes
      immediately.

    {4 Input Configuration}

    @param mouse_enabled Whether to enable mouse tracking. Defaults to [true].
    @param mouse
      Explicit mouse tracking mode. Defaults to [None], which selects [`Sgr_any]
      when [mouse_enabled] is [true]. See {!Terminal.mouse_mode} for available
      modes.
    @param bracketed_paste
      Whether to enable bracketed paste mode. Defaults to [true] (enabled if the
      terminal supports it). Prevents accidental command execution when pasting
      text.
    @param focus_reporting
      Whether to enable focus-in/focus-out events. Defaults to [true] (enabled
      if the terminal supports it).
    @param kitty_keyboard
      Kitty keyboard protocol configuration. Defaults to [`Auto] (auto-detect
      support). See {!kitty_keyboard} for full description.
    @param exit_on_ctrl_c
      Whether to treat Ctrl+C as an exit signal. Defaults to [true]. Set to
      [false] to handle Ctrl+C as a normal input event.
    @param input_timeout
      Timeout in seconds for input polling when no cadence is active. Defaults
      to [None] (block indefinitely). Useful for implementing application-level
      timeouts.

    {4 Debug and Diagnostics}

    @param debug_overlay
      Whether to show the built-in debug overlay. Defaults to [false].
    @param debug_overlay_corner
      Corner to anchor the debug overlay. Defaults to [`Bottom_right].
    @param debug_overlay_capacity
      Maximum number of metrics samples retained by the debug overlay. Defaults
      to [120].
    @param frame_dump_every
      Dump every Nth frame to disk. Defaults to [0] (disabled). Set to a
      positive integer to enable periodic frame dumps for debugging.
    @param frame_dump_dir
      Directory for frame dumps. Defaults to [None] (uses current working
      directory).
    @param frame_dump_pattern
      Filename pattern for frame dumps. Defaults to [None] (uses default
      pattern).
    @param frame_dump_hits
      Whether to include hit grid in frame dumps. Defaults to [false].

    {4 Signal Handling}

    @param signal_handlers
      Whether to install signal handlers for graceful shutdown on SIGTERM,
      SIGINT, SIGQUIT, and SIGABRT. Defaults to [true]. Set to [false] if your
      application manages its own signal handling. When enabled, these signals
      will trigger terminal cleanup before exiting. You can also call
      {!install_signal_handlers} manually if you create with
      [~signal_handlers:false] but later decide you want them.

    The runtime starts immediately with the terminal in the configured state.
    Call {!run} for automatic event loop management, or drive the loop manually
    with {!prepare}, {!Terminal.read}, and {!submit}. *)

val run :
  ?on_frame:(app -> dt:float -> unit) ->
  ?on_input:(app -> Input.t -> unit) ->
  ?on_resize:(app -> cols:int -> rows:int -> unit) ->
  on_render:(app -> unit) ->
  app ->
  unit
(** [run ?on_frame ?on_input ?on_resize ~on_render app] drives an immediate-mode
    loop.

    Matrix manages the lifecycle automatically: 1. Polls for events and invokes
    [on_input] / [on_resize]. 2. Calls {!prepare} to clear buffers. 3. Invokes
    [on_render] (users should draw to {!grid} here). 4. Calls {!submit} to flush
    output.

    [on_frame] runs before [prepare] with the elapsed seconds since the last
    render. The loop exits when {!running} becomes [false] and closes the
    runtime; exceptions close the runtime before propagating. *)

val prepare : app -> unit
(** [prepare app] starts a new frame lifecycle.

    It clears the internal {!grid} and {!hits} buffers and updates layout
    calculations based on the current terminal size. This must be called before
    drawing into the grid.

    {b Note:} If you use {!run}, this is called automatically. *)

val grid : app -> Grid.t
(** [grid app] returns the mutable grid for the current frame.

    This accessor is side-effect free. To clear the grid for a new frame, call
    {!prepare}. *)

val hits : app -> Screen.Hit_grid.t
(** [hits app] returns the hit grid for the current frame.

    This accessor is side-effect free. To clear the grid for a new frame, call
    {!prepare}. *)

val submit : app -> unit
(** [submit app] diffs the current frame against the previous one and flushes
    ANSI output to the terminal. Call this after you have finished drawing into
    {!grid}. *)

val close : app -> unit
(** [close app] tears down the runtime and restores the terminal. Safe to call
    multiple times. *)

val install_signal_handlers : unit -> unit
(** [install_signal_handlers ()] installs signal handlers for graceful shutdown
    on SIGTERM, SIGINT, SIGQUIT, and SIGABRT. This is called automatically by
    {!create} unless [~signal_handlers:false] is passed. Calling this function
    multiple times is safe; handlers are only installed once.

    When a signal is received, all registered shutdown handlers are run (which
    includes closing any active Matrix runtimes to restore terminal state), then
    the process exits with code [128 + signal_number].

    If you need custom signal handling, pass [~signal_handlers:false] to
    {!create} and manage signals yourself, ensuring you call {!close} before
    exiting. *)

val stop : app -> unit
(** [stop app] marks the runtime as stopped. The run loop will exit on the next
    tick. *)

val start : app -> unit
(** [start app] explicitly resumes the render cadence (if it was paused) and
    marks the control state as explicitly started. *)

val pause : app -> unit
(** [pause app] stops the render cadence but leaves the terminal configured. A
    subsequent {!start} will resume. *)

val auto : app -> unit
(** [auto app] switches to automatic cadence control, idling when there are no
    live requests and resuming when {!request_live} is called. *)

val suspend : app -> unit
(** [suspend app] pauses rendering and restores the terminal to cooked mode,
    disabling negotiated input modes. {!resume} reapplies configuration. *)

val resume : app -> unit
(** [resume app] reapplies terminal configuration after {!suspend} and restores
    the previous control state (auto or explicit). *)

val request_live : app -> unit
(** [request_live app] signals that live work is pending. When transitioning
    from idle it restarts the render cadence automatically. *)

val drop_live : app -> unit
(** [drop_live app] decrements the live counter; when it reaches zero in auto
    mode the cadence idles. *)

val running : app -> bool
(** [running app] reports if the event loop is active. *)

val request_redraw : app -> unit
(** [request_redraw app] marks the frame dirty for the next loop iteration.

    When no cadence is active (no [target_fps]), Matrix also marks frames dirty
    whenever it reads new input. With a cadence, the request ensures the next
    scheduled frame redraws immediately, but the cadence still bounds the
    maximum frame rate. *)

(** {1 Diagnostics} *)

val set_debug_overlay :
  ?corner:debug_overlay_corner -> app -> enabled:bool -> unit
(** [set_debug_overlay ?corner app ~enabled] toggles the built-in debug overlay.

    When [corner] is provided the overlay is re-anchored; enabling requests a
    redraw. *)

val toggle_debug_overlay : ?corner:debug_overlay_corner -> app -> unit
(** [toggle_debug_overlay ?corner app] flips overlay visibility and optionally
    re-anchors it. *)

val configure_frame_dump :
  ?every:int -> ?dir:string -> ?pattern:string -> ?hits:bool -> app -> unit
(** [configure_frame_dump ?every ?dir ?pattern ?hits app] updates the periodic
    frame-dump schedule. [every <= 0] disables periodic dumps. *)

val dump_frame : ?hits:bool -> ?dir:string -> ?pattern:string -> app -> unit
(** [dump_frame ?hits ?dir ?pattern app] writes the current frame to disk
    immediately. *)

(** {1 Terminal Information} *)

val size : app -> int * int
(** [size app] returns the current dynamic-region dimensions as [(cols, rows)].
*)

val pixel_resolution : app -> (int * int) option
(** [pixel_resolution app] returns the last known pixel resolution reported by
    the terminal as [(width, height)], or [None] if unknown or not yet queried.
    Matrix queries at startup and on every resize; not all terminals answer. *)

val terminal : app -> Terminal.t
(** [terminal app] returns the underlying terminal handle. *)

val capabilities : app -> Terminal.capabilities
(** [capabilities app] returns the current terminal capabilities. *)

(** {1 Static Output} *)

val static_write : app -> string -> unit
(** [static_write app text] writes [text] to the primary screen above the
    renderer.

    Static output is ignored in [`Alt] mode. In primary-screen modes it adjusts
    bookkeeping so the dynamic render region stays on-screen. *)

val static_print : app -> string -> unit
(** [static_print app text] behaves like {!static_write} but ensures [text] ends
    with a newline. *)

val static_clear : app -> unit
(** [static_clear app] clears previously written static content and restores the
    screen to occupy the full terminal height. *)

(** {1 Cursor Control} *)

val set_cursor : ?visible:bool -> ?style:Terminal.cursor_style -> app -> unit
(** [set_cursor ?visible ?style app] updates cursor visibility and style. *)

val set_cursor_style :
  app -> style:Terminal.cursor_style -> blinking:bool -> unit
(** [set_cursor_style app ~style ~blinking] sets the cursor style and blink flag
    explicitly. *)

val set_cursor_position : app -> row:int -> col:int -> unit
(** [set_cursor_position app ~row ~col] moves the terminal cursor to 1-based
    coordinates within the dynamic render region. *)

val set_cursor_color : app -> r:float -> g:float -> b:float -> a:float -> unit
(** [set_cursor_color app ~r ~g ~b ~a] sets the terminal cursor color. *)
