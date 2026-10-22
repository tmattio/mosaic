(** Immediate-mode terminal application runtime.

    [Matrix] owns the terminal while an application runs: it negotiates input
    protocols, builds frames against a double-buffered {!Grid.t}, and diffs the
    grid to emit minimal ANSI output.

    The module re-exports the lower-level subsystems as submodules and provides
    an immediate-mode event loop via {!create} and {!run}.

    {1:sub_libraries Sub-libraries}

    {!modules:Ansi Grid Input Screen Terminal Text} *)

(** {1:sub Sub-libraries} *)

module Ansi = Ansi
(** ANSI escape sequence generation. *)

module Grid = Matrix_grid
(** Cell-based drawing grid. *)

module Input = Matrix_input
(** Terminal input decoding. *)

module Screen = Matrix_screen
(** Double-buffered frame rendering. *)

module Terminal = Matrix_terminal
(** Terminal protocol state machine. *)

module Text = Matrix_text
(** Terminal text measurement and segmentation. *)

(** {1:types Types} *)

type kitty_keyboard = [ `Auto | `Off ]
(** The type for kitty keyboard protocol configurations.
    - [`Auto] pushes the protocol's flags once the terminal answers its query,
      and asks for the flags again so that {!capabilities} says which hold.
      Without an answer the runtime sets xterm's modifyOtherKeys instead, so
      that chords such as Ctrl+Alt with a letter stay distinct.
    - [`Off] never uses the protocol. *)

type mode = [ `Alt | `Primary ]
(** The type for presentation modes.
    - [`Alt] uses the alternate screen buffer. The application fully owns the
      screen; content is restored on exit.
    - [`Primary] renders an inline live viewport on the primary screen. Rows
      above the viewport are static transcript rows and enter native scrollback.
      When the input is a terminal, the viewport starts at the cursor, and after
      a resize or a {!resume} the runtime finds it again from a cursor position
      report: terminals move their rows as they resize. *)

(** {1:events Events} *)

(** The type for what the terminal says to the application. The input is decoded
    by {!Input.Decoder}; replies to the runtime's queries never reach the
    application, but those that are application facts do, as {!Resize} and
    {!Color_scheme}. *)
type event =
  | Key of Input.Keystroke.t  (** A key pressed or auto-repeated. *)
  | Key_release of Input.Keystroke.release
      (** A key released, reported only when {!create}'s [key_releases] asks. *)
  | Text of string
      (** Text the terminal reports without a key, such as text an input method
          composed. *)
  | Mouse of Input.Mouse_report.t
      (** A mouse report. In [`Primary] mode its row counts from the top of the
          live region. *)
  | Paste_start  (** A bracketed paste begins. *)
  | Paste_data of string
      (** Content of the paste, exactly as sent, in pieces as it arrives. The
          runtime holds none of it and sets it no limit: the application keeps
          what it wants. *)
  | Paste_end of [ `Complete | `Cut ]
      (** The paste ends at its end marker ([`Complete]), or is cut by the end
          of the input or {!abort_paste}. *)
  | Focus_in  (** The terminal window gained focus. *)
  | Focus_out  (** The terminal window lost focus. *)
  | Resize of int * int
      (** The terminal's size, as [(cols, rows)]: from the backend's window-size
          signal, each time, or from the terminal's own report (mode 2048) when
          it differs from the size last reported. *)
  | Color_scheme of [ `Dark | `Light ]
      (** The terminal's color scheme, in reply to a query or on a change. *)
  | Invalid of string
      (** Bytes that are not UTF-8, one ill-formed subsequence. *)

val pp_event : Format.formatter -> event -> unit
(** [pp_event] formats an event for debugging. *)

(** Backend I/O. Matrix owns input decoding, protocol negotiation, and frame
    scheduling. Backends provide device operations and a wait that can also
    deliver native events such as window resizes. *)
module Backend : sig
  type t = {
    now : unit -> float;
        (** [now ()] is monotonic time in seconds, with an arbitrary origin. *)
    wake : unit -> unit;
        (** [wake ()] interrupts the current or next [wait]. Wakeups remain
            pending until consumed by [wait]. It must support calls from other
            threads or domains when {!request_redraw} is used there, including a
            wake already in flight when [cleanup] runs. *)
    write_output : bytes -> int -> int -> unit;
        (** [write_output buf off len] writes the complete slice before
            returning. The backend must not retain [buf]. *)
    read_input : bytes -> int -> int -> int;
        (** [read_input buf off len] reads at most [len] bytes into [buf] at
            [off]. Called only after [wait] reports readable input. [0] denotes
            permanent end-of-input; [-1] means input is no longer ready.
            Positive results denote bytes read. *)
    wait : timeout:float option -> on_event:(event -> unit) -> bool;
        (** [wait ~timeout ~on_event] waits for readable input, a wakeup, native
            events, or the timeout in seconds. [None] has no timeout; [Some 0.]
            polls without blocking. Native events, such as {!Resize}, are passed
            to [on_event]. Returns [true] iff [read_input] can proceed without
            waiting. Matrix decodes the bytes, and a wait that returns with
            nothing to read tells it how long the terminal has been silent; the
            backend never decodes terminal input. *)
    input_is_tty : bool;  (** Whether the input device is a terminal. *)
    output_is_tty : bool;
        (** Whether the output device accepts terminal modes. *)
    terminal_size : unit -> int * int;
        (** [terminal_size ()] is the terminal size as [(cols, rows)]. *)
    set_raw_mode : bool -> unit;
        (** [set_raw_mode enabled] switches terminal input into raw mode or
            restores its original state. Repeated requests are idempotent;
            non-terminal input is a no-op. *)
    flush_input : unit -> unit;
        (** [flush_input ()] discards pending device input. *)
    protect : (unit -> unit) -> unit;
        (** [protect f] runs [f ()] with cancellation deferred until it returns
            or raises. Used only for cleanup; synchronous backends call [f]
            directly. *)
    cleanup : unit -> unit;
        (** [cleanup ()] releases resources owned by the backend. Matrix calls
            it once on failed attachment or close. *)
    env : Terminal.Caps.env;
        (** The environment of the session the terminal runs, from which the
            runtime guesses what the terminal can do until it answers. *)
  }
  (** The I/O operations and device properties supplied to {!attach}. *)
end

type debug_overlay_corner =
  [ `Top_left | `Top_right | `Bottom_left | `Bottom_right ]
(** The type for debug overlay anchor corners. *)

type app
(** The type for live applications. All frame, query, and control functions
    require this handle. Created by {!create} or {!attach}. *)

(** {1:lifecycle Lifecycle} *)

val create :
  ?mode:mode ->
  ?raw_mode:bool ->
  ?target_fps:float option ->
  ?mouse_enabled:bool ->
  ?mouse:Terminal.mouse_mode option ->
  ?bracketed_paste:bool ->
  ?kitty_keyboard:kitty_keyboard ->
  ?key_releases:bool ->
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
  ?input_timeout:float option ->
  ?resize_debounce:float option ->
  ?output:[ `Stdout | `Fd of Unix.file_descr ] ->
  ?signal_handlers:bool ->
  ?caps:Terminal.capabilities ->
  ?min_tui_height:int ->
  ?start_idle:bool ->
  ?pace_redraws:bool ->
  unit ->
  app
(** [create ()] is a live application with Unix I/O wired in.

    Sets up the terminal, enables raw mode, installs signal handlers, and
    returns a ready-to-run handle. Construction is transactional: if setup
    raises, every terminal mode and backend resource acquired so far is released
    before the original exception is propagated.

    {b Display:}
    - [mode] presentation mode. Defaults to [`Alt].
    - [raw_mode] whether to switch the TTY into raw mode. Defaults to [true].

    {b Rendering:}
    - [cursor_visible] initial cursor visibility. Defaults to [true] in [`Alt]
      mode, [false] in [`Primary].
    - [explicit_width] whether to use built-in wcwidth instead of querying the
      terminal. Defaults to [false].

    {b Frame timing:}
    - [target_fps] optional FPS cap in Hz. Defaults to [Some 30.]. [None] for
      uncapped. With [None], construction starts demand-driven: one-shot redraws
      render immediately, and {!start} or {!request_live} enables continuous
      uncapped rendering.
    - [pace_redraws] whether one-shot {!request_redraw}s coalesce to
      [target_fps]. Defaults to [true]: an event storm renders at most once per
      frame interval. Set [false] under a virtual clock, where coalescing buys
      nothing and advancing time to the next frame boundary would move the very
      clock timers read; one-shot redraws then render immediately. Live
      animation still paces off [target_fps] either way.
    - [resize_debounce] debounce window in seconds for resize events. Defaults
      to [Some 0.1].

    {b Primary mode:}
    - [min_tui_height] minimum height in rows reserved for the dynamic TUI
      region. Static content will not grow past this floor. Defaults to [1].
    - [start_idle] when [true], the render loop starts in idle state even when
      [target_fps] is set. The loop begins running only when {!request_live} is
      called. One-shot redraws via {!request_redraw} still work while idle.
      Defaults to [false].

    {b Input:}
    - [mouse_enabled] whether to enable mouse tracking. Defaults to [true].
    - [mouse] explicit mouse tracking mode. Defaults to [None] (selects
      [`Sgr_any] when [mouse_enabled] is [true]).
    - [bracketed_paste] whether to enable bracketed paste. Defaults to [true].
    - [kitty_keyboard] Kitty keyboard protocol configuration. Defaults to
      [`Auto].
    - [key_releases] whether to ask the kitty keyboard protocol for key releases
      ({!Key_release}), which also has every key reported as a sequence.
      Defaults to [false].
    - [exit_on_ctrl_c] whether pressing Ctrl+C exits. Defaults to [true].

    Focus reports (mode 1004), color scheme reports (2031) and resize reports
    (2048) are always set: a terminal ignores a mode it does not have.
    - [input_timeout] timeout in seconds for input polling when no cadence is
      active. Defaults to [None] (block indefinitely).

    {b Unix:}
    - [output] output target. Defaults to [`Stdout].
    - [signal_handlers] whether the application owns SIGTERM, SIGINT, SIGQUIT,
      SIGABRT, and SIGHUP until {!close}. The previous dispositions are restored
      on close. Defaults to [true]. Signal dispositions are process-global, so
      simultaneously live applications must be closed in reverse construction
      order.
    - [caps] the capabilities to assume where the terminal does not answer, in
      place of what the environment says ({!Terminal.Caps.assume}). By default
      the environment is read once, at creation.

    {b Diagnostics:}
    - [debug_overlay] show the debug overlay. Defaults to [false].
    - [debug_overlay_corner] anchor corner. Defaults to [`Bottom_right].
    - [debug_overlay_capacity] maximum metric samples retained.
    - [frame_dump_every] dump every Nth frame. Defaults to [0] (disabled).
    - [frame_dump_dir] directory for frame dumps.
    - [frame_dump_pattern] filename pattern for frame dumps.
    - [frame_dump_hits] include hit grid in dumps. Defaults to [false]. *)

val run :
  ?on_frame:(app -> dt:float -> unit) ->
  ?on_input:(app -> event -> unit) ->
  ?on_resize:(app -> cols:int -> rows:int -> unit) ->
  ?primary_required_rows:(app -> int option) ->
  on_render:(app -> unit) ->
  app ->
  unit
(** [run ~on_render app] drives the immediate-mode event loop.

    Each iteration:
    - Polls for events and invokes [on_input] / [on_resize].
    - Calls [on_frame] with the elapsed seconds since the last render.
    - Calls {!prepare} to size and clear the frame, with the rows
      [primary_required_rows] asks for.
    - Invokes [on_render] (draw to {!grid} here).
    - Calls {!submit} to diff and flush output.

    [primary_required_rows] runs before the frame is drawn, so it states the
    rows of the frame to come, from the application's state.

    [on_resize] reports the usable surface as [(cols, rows)] — {!size}, the live
    viewport in [`Primary] mode and the full terminal in [`Alt] mode. It fires
    once before the first render and then whenever the applied size changes,
    including debounced resizes applied after their event; resize reports that
    do not change the applied size produce no callback. [on_input] still
    receives raw {!Resize} terminal dimensions.

    The loop exits when {!running} becomes [false]. Calling [run] on an already
    stopped application closes it without restarting; call {!start} first to
    restart an open application. Closed applications remain closed. Stopping or
    suspending in [on_frame] skips [on_render]; doing so in [on_render] skips
    presentation. Pausing permits the current frame to finish. Permanent
    end-of-input finalizes the input decoder and closes the runtime. Exceptions
    close the runtime before propagating. *)

(** {1:frame Frame building} *)

val prepare : ?primary_required_rows:int -> app -> unit
(** [prepare app] starts a new frame: it lays the frame out and clears the
    {!grid} and {!hits} buffers at its size, {!size}.

    In [`Primary] mode the queued static writes go to the transcript, and the
    live region grows to [primary_required_rows] rows when it has fewer, at most
    the terminal's height, by scrolling the rows above it into the terminal's
    scrollback. The frame drawn next is as tall as the live region, so it shows
    the rows it asked for. [primary_required_rows] is ignored in [`Alt] mode.

    {b Note.} Called automatically by {!run}. *)

val grid : app -> Grid.t
(** [grid app] is the mutable grid for the current frame. *)

val current_grid : app -> Grid.t
(** [current_grid app] is the presented grid: the committed terminal state as of
    the most recent frame. Blank before the first frame. Do not mutate. *)

val hits : app -> Screen.Hit_grid.t
(** [hits app] is the hit grid for the current frame. *)

val screen : app -> Screen.t
(** [screen app] is the runtime's screen. The runtime owns its sizing and
    presentation: {!prepare} clears and resizes it, {!submit} diffs and swaps
    it. Embedding renderers build frames into it (via {!Screen.build} or the
    frame accessors above) during [on_render] so the runtime presents their
    output with a single diff. Do not call {!Screen.render} on it yourself. *)

val submit : app -> unit
(** [submit app] diffs the current frame against the previous one and flushes
    ANSI output, after the terminal operations of the layout {!prepare} chose.
    Call after drawing into {!grid}. Stopped, suspended and closed applications
    emit no frame output.

    {b Note.} Called automatically by {!run}. *)

(** {1:control Control} *)

val close : app -> unit
(** [close app] idempotently tears down terminal protocols and raw mode, then
    releases backend resources. *)

val stop : app -> unit
(** [stop app] marks the runtime as stopped. The {!run} loop exits on the next
    tick. *)

val start : app -> unit
(** [start app] requests continuous rendering and restarts a stopped loop. While
    suspended it changes demand only; {!resume} must restore terminal ownership
    before any frame is rendered. Closed applications remain closed. *)

val pause : app -> unit
(** [pause app] stops continuous rendering without changing terminal ownership.
    One-shot redraws remain available while the terminal is active. Calling
    [pause] while suspended preserves this demand after {!resume}; {!start}
    requests continuous rendering again. *)

val suspend : ?leave_alt:bool -> app -> unit
(** [suspend app] pauses rendering and restores the terminal to cooked mode
    (mouse, bracketed paste, focus, color scheme and resize reports, kitty
    keyboard, modify-other-keys and metaSendsEscape off; raw mode off; pending
    input flushed, and a paste in progress ended with [Paste_end `Cut]). When
    [leave_alt] is [true] (default [false], preserving existing callers) it also
    exits the alternate-screen buffer, so a full-screen foreground child owns
    the primary screen and its scrollback. {!resume} reapplies configuration.

    It leaves the screen to a program run meanwhile, with the cursor shown. In
    [`Primary] mode the live region is erased, after the static rows already
    placed by a frame, and the cursor waits at its start: the program's output
    goes on from the transcript, and {!resume} starts the live region below it.
    In [`Alt] mode the screen is erased and the cursor waits at its top left,
    unless [leave_alt].

    Idempotent: calling [suspend] while already suspended (or after {!close}) is
    a no-op. Live requests and {!start}/{!pause} can change rendering demand
    while suspended without reclaiming terminal ownership. *)

val resume : app -> unit
(** [resume app] reapplies terminal configuration after {!suspend}: it re-enters
    the alternate-screen buffer iff the matching {!suspend} left it, discards
    any input received while suspended (stale keystrokes never reach the app),
    and requests a full repaint on the next frame. In [`Primary] mode the live
    region starts again at the cursor, from a cursor position report, below what
    the program run meanwhile wrote, which joins the transcript. Cadence follows
    the latest rendering demand and live count; resuming does not restart a
    stopped loop. *)

val request_live : app -> unit
(** [request_live app] increments live demand. In automatic mode, the first
    request enables continuous rendering. Requests made while suspended take
    effect on {!resume}; explicit pause and stop remain in effect. *)

val drop_live : app -> unit
(** [drop_live app] decrements the live counter. When it reaches zero in auto
    mode the cadence idles. *)

val running : app -> bool
(** [running app] is [true] while the app may continue its event loop. It is
    initially [true] and becomes [false] on {!stop} or {!close}. Suspension and
    paused rendering do not terminate the loop. *)

val now : app -> float
(** [now app] is monotonic time in seconds as reported by [app]'s clock. Its
    origin is arbitrary; use differences for durations and this same clock for
    {!schedule_wakeup} deadlines. *)

val request_redraw : app -> unit
(** [request_redraw app] marks the frame dirty for the next iteration. Outside
    live mode, dirty frames render as soon as the target-fps pacing allows, so a
    burst of requests coalesces instead of rendering back-to-back.

    Requests may come from other threads or domains when the backend's [wake]
    supports this. Requests during suspension or after closure are ignored.
    Other app operations remain owned by the event-loop thread. Requests made
    during the render callback remain pending for the next frame. *)

val schedule_wakeup : app -> float option -> unit
(** [schedule_wakeup app deadline] arms (or, with [None], clears) the loop's
    one-shot wakeup. When the loop reaches [deadline] (a {!now} time), it runs
    the [on_frame] callback without rendering — the mechanism time-based work
    (timers) uses to advance off the clock while the loop is otherwise idle. One
    slot: a new call replaces the previous deadline, and a due deadline clears
    itself before the callback runs, so the callback re-arms if it still needs
    waking. Whatever the callback dispatches must {!request_redraw} to be
    presented. *)

val redraw_requested : app -> bool
(** [redraw_requested app] is [true] iff a redraw is pending. Lets an alternate
    backend distinguish a frame gated on the render cadence from a loop with no
    pending work. *)

val abort_paste : app -> unit
(** [abort_paste app] ends the bracketed paste in progress, if any, with
    [Paste_end `Cut], for an application that will wait no longer for its end
    marker: a terminal that stalls mid-paste stops the data, and the paste goes
    on until its end marker or the end of the input. The bytes after it are
    decoded as input. *)

val request_immediate_redraw : app -> unit
(** [request_immediate_redraw app] requests a one-shot frame without waiting for
    the configured frame cadence. Subsequent frames continue at the configured
    cadence. *)

(** {1:queries Terminal queries} *)

val mode : app -> mode
(** [mode app] is the presentation mode set at creation time. *)

val size : app -> int * int
(** [size app] is the current dynamic-region dimensions as [(cols, rows)]. In
    [`Primary] mode this is the live viewport, not the full terminal. *)

val full_size : app -> int * int
(** [full_size app] is the full terminal dimensions [(cols, rows)], ignoring the
    primary-mode render offset. In [`Alt] mode this is identical to {!size}. *)

val effective_size : app -> int * int
(** [effective_size app] is the dynamic-region dimensions [(cols, rows)] that
    will be in effect once {!prepare} flushes the pending static writes, before
    it grows the region to the rows a frame requires. In [`Alt] mode, when the
    static queue is empty, and so in [on_render], this equals {!size}. *)

val pixel_resolution : app -> (int * int) option
(** [pixel_resolution app] is the last known pixel resolution as
    [(width, height)], or [None] if the terminal has not reported one. *)

val terminal : app -> Terminal.t
(** [terminal app] is the underlying terminal handle. *)

val capabilities : app -> Terminal.capabilities
(** [capabilities app] is the current terminal capabilities. *)

(** {1:static Static output}

    Static output is primary-screen transcript output placed before the live
    viewport. Insertion may move the live viewport downward until it reaches its
    minimum height. Once pinned, inserted rows push the rows above the viewport
    into the terminal's scrollback and leave the viewport its height. Transcript
    rows are never overwritten: they leave the screen only into the scrollback.
    These functions are ignored in [`Alt] mode. *)

val static_write :
  ?preserve_live_region:bool -> ?rows:int -> app -> Grid.t -> unit
(** [static_write ?preserve_live_region ?rows app grid] queues a snapshot of the
    first [rows] grid rows before the live viewport. [rows] defaults to the full
    grid height, including blank or background-only rows. The snapshot is
    serialized immediately; subsequent grid mutations do not change it. Each row
    replaces the complete terminal row and ends with CRLF. Rows never wrap if
    the terminal becomes narrower before the queued output is presented; excess
    cells overwrite the rightmost column, following terminal margin behavior.
    The rows go out with the next frame {!prepare} lays out: one written while a
    frame is drawn goes out with the frame after it.

    Raises [Invalid_argument] if the grid is wider than the current terminal, or
    [rows] is outside the grid height. [rows = 0] queues nothing.

    When [preserve_live_region] is [true], the live region keeps its current
    height: the transcript, these rows included, scrolls up into terminal
    history to make room. Otherwise the rows take theirs from the live region,
    down to [min_tui_height]. *)

val static_replace :
  ?preserve_live_region:bool -> ?rows:int -> app -> Grid.t -> unit
(** [static_replace ?preserve_live_region ?rows app grid] queues an atomic
    replacement of all primary static content with a snapshot of [grid]. Row
    selection, snapshot ownership, and validation match {!static_write}.
    [rows = 0] clears the transcript without inserting rows. The replacement and
    next live frame are emitted in one transaction.

    {b Warning.} Replacement clears the terminal's entire scrollback, including
    shell output from before the application. Support for [CSI 3 J] varies
    across terminals. This function is ignored in [`Alt] mode. *)

val static_clear : app -> unit
(** [static_clear app] clears the primary screen immediately, discards pending
    static output, and resets the live viewport to the full terminal height. It
    is an immediate terminal reset rather than a queued frame operation. *)

(** {1:scroll Scroll optimisation} *)

val set_scroll_hint : app -> Screen.scroll_hint -> unit
(** [set_scroll_hint app hint] sets a scroll hint for the current frame.
    Consumed by the next {!submit} call. The runtime currently applies hints
    only in [`Alt] mode. In [`Primary] mode hints are discarded because primary
    static output already owns the frame's temporary scroll-region protocol.

    Use this when a scrollable container's viewport shifts: the renderer applies
    DECSTBM hardware scroll so only the newly-revealed edge rows need cell-level
    diffing. Without the hint, scrolling rewrites every row in the viewport. *)

(** {1:cursor Cursor control} *)

val set_cursor : ?visible:bool -> ?style:Ansi.cursor_style -> app -> unit
(** [set_cursor ?visible ?style app] updates cursor visibility and/or style. *)

val set_cursor_style : app -> style:Ansi.cursor_style -> blinking:bool -> unit
(** [set_cursor_style app ~style ~blinking] sets cursor shape and blink. *)

val set_cursor_position : app -> row:int -> col:int -> unit
(** [set_cursor_position app ~row ~col] moves the cursor to 1-based coordinates
    within the dynamic render region. *)

val set_cursor_color : app -> r:float -> g:float -> b:float -> unit
(** [set_cursor_color app ~r ~g ~b] sets the cursor color. Components are in
    \[[0.];[1.]\]. *)

(** {1:diagnostics Diagnostics} *)

val set_debug_overlay :
  ?corner:debug_overlay_corner -> app -> enabled:bool -> unit
(** [set_debug_overlay app ~enabled] shows or hides the debug overlay. [corner]
    defaults to the value given to {!create}. *)

val toggle_debug_overlay : ?corner:debug_overlay_corner -> app -> unit
(** [toggle_debug_overlay app] flips overlay visibility. *)

val configure_frame_dump :
  ?every:int -> ?dir:string -> ?pattern:string -> ?hits:bool -> app -> unit
(** [configure_frame_dump app] updates the periodic frame-dump schedule. See the
    diagnostics parameters of {!create}. *)

val dump_frame : ?hits:bool -> ?dir:string -> ?pattern:string -> app -> unit
(** [dump_frame app] writes the current frame to disk immediately. *)

(** {1:custom Custom backends}

    {!attach} creates an application wired to caller-provided I/O callbacks
    instead of Unix. Use this in tests or with alternative runtimes (e.g. Eio).
*)

val attach :
  ?mode:mode ->
  ?raw_mode:bool ->
  ?target_fps:float option ->
  ?mouse_enabled:bool ->
  ?mouse:Terminal.mouse_mode option ->
  ?bracketed_paste:bool ->
  ?kitty_keyboard:kitty_keyboard ->
  ?key_releases:bool ->
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
  ?input_timeout:float option ->
  ?resize_debounce:float option ->
  ?min_tui_height:int ->
  ?start_idle:bool ->
  ?pace_redraws:bool ->
  ?caps:Terminal.capabilities ->
  backend:Backend.t ->
  unit ->
  app
(** [attach ~backend ()] creates an application over [backend]. It owns the
    input decoder and terminal protocol state, negotiates capabilities, obtains
    the initial dimensions, and preserves input received during startup.

    Optional parameters have the same meaning as in {!create}. Raw mode and
    startup queries require [backend.input_is_tty] and [raw_mode]; terminal mode
    output requires [backend.output_is_tty].

    [attach] takes ownership of backend cleanup. Failed construction restores
    acquired terminal modes and raw mode, calls [backend.cleanup] once, and
    propagates the original exception and backtrace. On success, {!close}
    releases these resources. The caller owns signal handling and must arrange
    to close the application when its enclosing scope exits. *)
