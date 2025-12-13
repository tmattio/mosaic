(** Deterministic terminal control with capability detection, protocol
    negotiation, and structured input parsing.

    {!Terminal} keeps protocol state symmetric: anything enabled via the handle
    is unwound by {!close} or {!reset_state}. Resize events are delivered via a
    SIGWINCH handler. For non-TTY outputs, no escape sequences are emitted but
    internal state tracking remains active. Calls that emit escape sequences may
    raise {!Error} if the underlying descriptor refuses writes.

    {1 Overview}

    - Create a handle with {!open_terminal}. It probes capabilities by default.
      Pass [?probe:false] for non-interactive or sandboxed outputs.
    - Use {!switch_mode} or {!with_mode} to choose [`Raw], [`Cooked], or a
      custom termios configuration.
    - Enable protocols with {!set_mouse_mode}, {!enable_bracketed_paste}, and
      related functions.
    - Consume structured events with {!next_event} or {!poll_events} and render
      output with {!write} or {!write_bytes}.

    {1 Capability model}

    {!open_terminal} seeds capabilities using environment heuristics and
    platform quirks. {!query_capabilities} refines the record by issuing
    DECRQM/DA/XTVersion queries . {!capabilities} returns the latest known view
    and reflects all responses processed so far.

    {1 Usage basics}

    {@ocaml[
      let run () =
        let term = Terminal.open_terminal () in
        Fun.protect
          ~finally:(fun () -> Terminal.close term)
          (fun () ->
            Terminal.switch_mode term `Raw;
            Terminal.set_mouse_mode term `Sgr_any;
            Terminal.enable_bracketed_paste term true;
            match Terminal.next_event term with
            | Some (Input.Key { key = Input.Enter; _ }) -> print_endline "Enter"
            | Some
                (Input.Mouse
                   (Input.Mouse.Button_press (x, y, Input.Mouse.Left, _))) ->
                Printf.printf "Click at %d,%d\n%!" x y
            | Some (Input.Resize (cols, rows)) ->
                Printf.printf "Resized to %dx%d\n%!" cols rows
            | _ -> ())
    ]}

    {1 Performance}

    - All writes retry on [EINTR]/[EAGAIN] before failing with {!Error}.
    - {!next_event} supports timeouts, folds capability responses into
      {!capabilities}, and filters them out of the public {!Input.t} stream.

    {b Note}: This module opens {!Unix}, which imports all Unix types and
    functions into the namespace. *)

open Unix

exception Error of string
(** Raised for terminal I/O failures.

    {!Error} wraps an underlying [Unix_error] as a human-readable message via
    {!Unix.error_message}. [EINTR] and [EAGAIN] are retried internally, so
    seeing this exception indicates a hard failure such as a closed descriptor
    or an unrecoverable write error. *)

(** {1 Modes and protocols} *)

type mode =
  [ `Raw  (** Disable canonical mode and echo; enable non-blocking input. *)
  | `Cooked  (** Restore canonical terminal behaviour. *)
  | `Custom of terminal_io -> terminal_io  (** Custom termios transformation. *)
  ]
(** Terminal input mode.

    Modes control how incoming bytes are buffered and processed:

    - [`Raw]: minimal processing; input is delivered byte-by-byte as soon as it
      arrives. Required for interactive applications that depend on individual
      key events.
    - [`Cooked]: standard line editing and echo. Suitable for simple,
      line-oriented prompts.
    - [`Custom f]: caller-provided termios transformation. {!switch_mode} reads
      the current termios, applies [f], and installs the result.

    For non-TTY descriptors, mode changes are recorded but do not call into
    [tcgetattr]/[tcsetattr]. *)

type mouse_mode =
  [ `Off
  | `X10
  | `Normal
  | `Button
  | `Any
  | `Sgr_normal
  | `Sgr_button
  | `Sgr_any ]
(** Mouse tracking protocols.

    - [`Off]: disable mouse tracking.
    - [`X10]: basic press-only tracking (legacy; coordinates limited to 223×223
      cells).
    - [`Normal]: press and release events with standard xterm encoding (1000).
    - [`Button]: press, release, and drag events (1002).
    - [`Any]: all motion events, including motion with no buttons pressed
      (1003).
    - [`Sgr_normal]: SGR-extended press/release only (1006 + 1000).
    - [`Sgr_button]: SGR-extended with drag events (1006 + 1000 + 1002).
    - [`Sgr_any]: SGR-extended with all motion (1006 + 1000 + 1002 + 1003).

    SGR modes use the SGR extension (1006) which supports terminals larger than
    223×223 cells and provides more precise button/modifier reporting. *)

type unicode_width = [ `Wcwidth | `Unicode ]
(** Unicode width calculation method.

    - [`Wcwidth]: use platform [wcwidth()]-like behavior. Compatible with older
      setups but inaccurate for some emoji and combining characters.
    - [`Unicode]: use modern Unicode width tables.

    {!set_unicode_width} updates both terminal configuration (when supported)
    and the cached {!capabilities}. *)

type cursor_style = [ `Block | `Line | `Underline ]
(** Cursor visual style.

    These shapes correspond to the standard DECSCUSR cursor styles supported by
    most modern terminals. *)

type cursor_position = { x : int; y : int; visible : bool }
(** Cursor position and visibility.

    - [x], [y] are 1-based coordinates; [(1, 1)] is the top-left corner.
    - [visible] reflects the last requested visibility via {!move_cursor},
      {!set_cursor_visible}, or {!reset_state}.

    The record reflects the library's tracked position; it is not updated by
    reading the terminal directly. This record is not synchronized; concurrent
    modifications via {!move_cursor} may lead to stale reads. *)

(** {1 Capabilities} *)

type capabilities = {
  term : string;
  rgb : bool;
  kitty_keyboard : bool;
  kitty_graphics : bool;
  bracketed_paste : bool;
  focus_tracking : bool;
  unicode_width : unicode_width;
  sgr_pixels : bool;
  color_scheme_updates : bool;
  explicit_width : bool;
  scaled_text : bool;
  sixel : bool;
  sync : bool;
  hyperlinks : bool;
}
(** Terminal capabilities.

    Detection happens in two stages:

    - {b Environment stage}: {!open_terminal} inspects [$TERM], [$COLORTERM],
      [$KITTY_WINDOW_ID], platform quirks (e.g. ConPTY), and overrides such as
      [MATRIX_FORCE_UNICODE]. No escape sequences are emitted.
    - {b Probe stage}: {!query_capabilities} (or [~probe:true] in
      {!open_terminal}) sends DECRQM, DA1, cursor-position, Kitty, and XTVersion
      queries and folds responses into the record.

    Summary of fields:

    - [term]: raw [$TERM] value.
    - [rgb]: 24-bit colour support.
    - [kitty_keyboard], [kitty_graphics]: Kitty protocol support.
    - [bracketed_paste]: bracketed paste mode support (wraps pasted content in
      special markers).
    - [focus_tracking]: focus event reporting support (enables [Focus]/[Blur]
      events).
    - [sync]: synchronized output support (DECRQM 2026; allows flicker-free
      updates).
    - [color_scheme_updates]: color scheme update notification support (DECRQM
      2031).
    - [unicode_width]: active width mode (see {!unicode_width}).
    - [sgr_pixels]: SGR pixel-position mouse support.
    - [explicit_width], [scaled_text]: explicit cell width/scaled text support,
      derived from proprietary cursor-position probes.
    - [sixel]: SIXEL graphics support.
    - [hyperlinks]: OSC 8 hyperlink support.

    Boolean fields from probing are monotonic: once a probe confirms a feature,
    subsequent probes will not disable it. However, environment-based policy
    overrides may disable certain features to ensure correct behavior in
    specific contexts (e.g., Windows ConPTY disables Kitty/SIXEL, VS Code
    disables Kitty keyboard/graphics, tmux forces wcwidth mode). The
    [unicode_width] field may change bidirectionally based on DECRQM 2027
    responses. *)

type terminal_info = { name : string; version : string; from_xtversion : bool }
(** Best-known terminal program name and version.

    Populated from XTVersion when available; otherwise derived from
    TERM_PROGRAM/TERM_PROGRAM_VERSION. [from_xtversion] indicates whether the
    current values came from an XTVersion payload. *)

(** {1 Session handle} *)

type t
(** Terminal session handle.

    A value of type [t] encapsulates:

    - input and output descriptors
    - capability and terminal metadata
    - an internal parser for decoding {!Input.t} events
    - protocol state such as mouse mode, cursor style, and alternate-screen
      status

    Always close the handle with {!close} to restore terminal settings. *)

(** {1 Basic usage} *)

val open_terminal :
  ?probe:bool ->
  ?probe_timeout:float ->
  ?input:file_descr ->
  ?output:file_descr ->
  ?initial_caps:capabilities ->
  unit ->
  t
(** [open_terminal ?probe ?probe_timeout ?input ?output ?initial_caps ()]
    creates a terminal session.

    The handle starts in cooked mode with no extra protocols enabled. The
    function:

    - determines whether [input] and [output] are TTYs;
    - snapshots the current termios for [input] (when it is a TTY) so they can
      be restored by {!close};
    - installs a global SIGWINCH handler (shared across all terminal sessions;
      each session enqueues its own resize events independently) that enqueues
      {!Input.Resize} events and wakes the event loop; and
    - on Windows, enables VT100 emulation and sets the console output codepage
      to UTF-8. {b Note}: the codepage change affects the entire process, not
      just this terminal handle. Failures are silently ignored as this is a
      compatibility enhancement.

    Capability detection:

    - When [initial_caps] is [None], seeds capabilities from environment
      heuristics using the detected [$TERM].
    - When [initial_caps] is [Some caps], that record is used directly and
      normalised to attach {!terminal_info}.

    Probing behaviour:

    - If [?probe] is [true] (default), {!query_capabilities} runs immediately
      with [~timeout:(default 0.2)]. This emits escape sequences on TTYs.
    - If [?probe] is [false], only environment heuristics are used; callers may
      probe later with {!query_capabilities}.

    For non-TTY descriptors, capability state is still tracked but no escape
    sequences are sent.

    @param input defaults to [Unix.stdin]
    @param output defaults to [Unix.stdout]
    @param probe_timeout probe window in seconds
    @raise Unix.Unix_error if creating internal pipes or reading termios fails
*)

val close : t -> unit
(** [close t] restores terminal state and disables all protocols.

    - Restores the saved termios for the input descriptor (if any).
    - Disables mouse tracking, bracketed paste, focus reporting, Kitty keyboard,
      modifyOtherKeys, Unicode mode, and alternate screen.
    - Resets visual state (cursor visibility, SGR attributes, cursor
      colour/style, window title) and exits the alternate screen if active.
    - Removes the SIGWINCH handler registered by {!open_terminal}.

    Errors during cleanup (e.g. termios restoration on a closed descriptor) are
    caught and ignored so cleanup always makes best effort progress. *)

val flush_input : t -> unit
(** [flush_input t] discards unread input on the terminal descriptor.

    If [input] is a TTY, calls [tcflush fd TCIFLUSH]. For non-TTY descriptors,
    this is a no-op. Failures are ignored as they indicate the descriptor is
    already closed or in an invalid state, which is not critical for this
    operation. *)

(** {1 Capabilities API} *)

val capabilities : t -> capabilities
(** [capabilities t] returns the current capability record.

    The record evolves over time as {!query_capabilities} and the internal
    parser process replies. Callers that care about the latest state should read
    it at the point of use rather than caching it. *)

val query_capabilities : ?timeout:float -> t -> unit
(** [query_capabilities ?timeout t] actively probes the terminal and updates
    {!capabilities} and {!terminal_info}.

    Probing is skipped if either [input] or [output] is not a TTY. Replies are
    parsed interleaved with normal input; capability events are folded into the
    internal state and not exposed as {!Input.t} events. This function blocks
    for the duration of the timeout while waiting for responses.

    @param timeout maximum time to spend probing; defaults to 0.2 seconds *)

val terminal_info : t -> terminal_info
(** [terminal_info t] returns the current terminal metadata. *)

val terminal_name : t -> string
(** [terminal_name t] is [terminal_info t].name. *)

val terminal_version : t -> string
(** [terminal_version t] is [terminal_info t].version. *)

val query_pixel_resolution : t -> unit
(** [query_pixel_resolution t] requests the terminal pixel resolution.

    Sends CSI 14 t ([\027\[14t]). Terminals that support it respond with CSI 4 ;
    height ; width t. The response is parsed by the internal capability handler
    and cached in {!pixel_resolution}. Capability events generated from this
    response are filtered out of the public {!Input.t} stream.

    This function is non-blocking (fire-and-forget); the response will be
    processed asynchronously when it arrives. The query is idempotent and safe
    on terminals that ignore it.

    @raise Error if emitting the query fails *)

val pixel_resolution : t -> (int * int) option
(** [pixel_resolution t] returns the last known pixel resolution as
    [Some (width, height)] or [None] if unknown.

    The value is updated by {!query_pixel_resolution} and by any spontaneous
    pixel-resolution reports parsed from the input stream. *)

(** {1 Modes and protocols} *)

val mode : t -> mode
(** [mode t] returns the current input mode.

    This is the last mode set via {!switch_mode} or {!with_mode}. The initial
    mode is [`Cooked]. *)

val switch_mode : t -> mode -> unit
(** [switch_mode t mode] applies a new terminal mode.

    For TTY inputs:

    - [`Raw] installs a raw termios with canonical mode, echo, and signals
      disabled and sets the descriptor to non-blocking.
    - [`Cooked] restores the termios snapshot captured at {!open_terminal}.
    - [`Custom f] applies [f] to the current termios and installs the result.

    For non-TTY inputs, only the internal mode field is updated.

    @raise Unix.Unix_error if termios operations or non-blocking toggles fail *)

val with_mode : t -> mode -> (unit -> 'a) -> 'a
(** [with_mode t mode f] runs [f] under [mode] and restores the previous mode.

    - Applies [mode] using {!switch_mode}.
    - Calls [f ()].
    - Restores the original mode in a [Fun.protect] [~finally] block.

    @return the result of [f ()]
    @raise Unix.Unix_error if switching into or out of [mode] fails *)

(** {1 Screens and buffers} *)

val enter_alternate_screen : t -> unit
(** [enter_alternate_screen t] switches to the alternate screen buffer.

    For TTY outputs:

    - Sends DECSET 1049 to activate the alternate screen.
    - Automatically hides the cursor and sets internal visibility to false.
      Full-screen applications should explicitly manage cursor visibility via
      {!set_cursor_visible}.

    For non-TTY outputs, only updates internal state. Calling this when already
    on the alternate screen is a no-op. *)

val leave_alternate_screen : t -> unit
(** [leave_alternate_screen t] returns to the primary screen buffer.

    For TTY outputs, sends DECRST 1049. The cursor visibility is left unchanged
    so applications can decide whether to reveal it.

    Idempotent: calling when already on the primary screen has no effect. *)

val set_mouse_mode : t -> mouse_mode -> unit
(** [set_mouse_mode t mode] configures mouse event tracking.

    - Disables all existing mouse modes first to avoid protocol conflicts.
    - Enables the requested protocol and, for SGR modes, turns on both SGR and
      the corresponding legacy xterm modes.

    For non-TTY outputs, only updates internal state. Setting the current mode
    is a no-op. *)

val mouse_mode : t -> mouse_mode
(** [mouse_mode t] returns the current mouse tracking mode.

    The initial mode is always [`Off]. *)

val enable_bracketed_paste : t -> bool -> unit
(** [enable_bracketed_paste t enable] toggles bracketed paste mode.

    When enabled, pasted content is wrapped in special markers so the parser can
    emit a single [Paste] event instead of raw characters.

    For TTY outputs, sends DECSET/DECRST 2004. Idempotent. *)

val enable_focus_reporting : t -> bool -> unit
(** [enable_focus_reporting t enable] toggles focus event reporting.

    When enabled, the terminal sends notifications on focus gain/loss, which the
    parser turns into [Focus]/[Blur] {!Input.t} events.

    For TTY outputs, sends DECSET/DECRST 1004. Idempotent. *)

val enable_kitty_keyboard : ?flags:int -> t -> bool -> unit
(** [enable_kitty_keyboard ?flags t enable] toggles the Kitty keyboard protocol.

    The Kitty protocol adds richer key information: press/release/repeat events,
    alternate representations, and associated text.

    @param flags
      bitmask controlling reported features (default [0b00001]):
      - bit 0 (0b00001): report event types (press/repeat/release)
      - bit 1 (0b00010): report alternate keys
      - bit 2 (0b00100): report all keys as escape sequences
      - bit 3 (0b01000): report associated text

    Enabling sends a “push” sequence with [flags]. Disabling sends a “pop”
    sequence to restore the previous keyboard mode. Changing flags while enabled
    sends a new push.

    For non-TTY outputs, only internal state is updated. *)

val enable_modify_other_keys : t -> bool -> unit
(** [enable_modify_other_keys t enable] toggles xterm modifyOtherKeys mode.

    When enabled, modified versions of otherwise ambiguous keys (e.g.
    [Ctrl+Enter], [Ctrl+Escape]) are encoded as CSI-tilde sequences with
    parameters [27;mod;code~]. This is useful when Kitty keyboard protocol is
    not active.

    Idempotent and no-op for non-TTY outputs. *)

val modify_other_keys_enabled : t -> bool
(** [modify_other_keys_enabled t] reports whether modifyOtherKeys mode is
    currently enabled. *)

val set_unicode_width : t -> unicode_width -> unit
(** [set_unicode_width t width] sets the Unicode width mode and updates
    {!capabilities}.

    For terminals that support a Unicode-width mode toggle, this also enables or
    disables the corresponding escape sequence. On terminals that ignore the
    toggle, only the cached {!capabilities} are updated. *)

val set_cursor_visible : t -> bool -> unit
(** [set_cursor_visible t visible] shows or hides the cursor.

    For TTY outputs, sends DECTCEM sequences. Idempotent: requesting the current
    visibility has no effect. *)

val cursor_visible : t -> bool
(** [cursor_visible t] returns the last requested cursor visibility.

    This is purely the tracked state; it does not query the terminal. *)

val cursor_position : t -> cursor_position
(** [cursor_position t] returns the tracked cursor position and visibility.

    Coordinates reflect the last call to {!move_cursor} or {!reset_state};
    visibility also incorporates {!set_cursor_visible}. *)

val cursor_style_state : t -> cursor_style * bool
(** [cursor_style_state t] returns the last requested cursor style and blinking
    flag. *)

val cursor_color : t -> float * float * float * float
(** [cursor_color t] returns the last requested cursor colour as [(r, g, b, a)]
    in the range [0.0, 1.0]. *)

val set_title : t -> string -> unit
(** [set_title t title] sets the terminal window title.

    For TTY outputs, sends OSC 0 with [title]. Passing an empty string typically
    clears the title. For non-TTY outputs, this is a no-op. *)

(** {1 I/O} *)

val wake : t -> unit
(** [wake t] writes to the internal wakeup pipe.

    This causes a blocked {!next_event} call to resume and re-check timers and
    queues. It is safe to call even if the loop is not currently blocked; extra
    bytes are drained automatically. *)

val size : t -> int * int
(** [size t] returns the terminal size in character cells as [(cols, rows)].

    Uses [TIOCGWINSZ] on Unix or the Windows console APIs. If detection fails,
    the fallback [(80, 24)] is returned. *)

val write : t -> string -> unit
(** [write t s] writes string [s] to the terminal output.

    Performs a complete write, retrying on [EINTR]/[EAGAIN]. For TTY outputs,
    behaviour matches normal terminal writes. For non-TTY outputs, data may be
    buffered until {!flush} is called.

    @raise Error if the write ultimately fails *)

val write_bytes : t -> bytes -> unit
(** [write_bytes t b] writes [b] to the terminal output.

    Equivalent to {!write} but accepts [bytes]. Retries on [EINTR]/[EAGAIN] and
    raises {!Error} on hard failures. *)

val output_fd : t -> file_descr
(** [output_fd t] returns the output file descriptor associated with the
    session.

    Useful for integrating with [Unix.select] or other multiplexing. The
    descriptor must not be closed directly; use {!close} instead. *)

val flush : t -> unit
(** [flush t] ensures buffered output has been written.

    For TTY outputs, this is a no-op since writes are unbuffered. For non-TTY
    outputs, this is also a no-op by design; explicit synchronization (via
    Unix.fsync) should be performed by the caller if required. *)

val reset_state : t -> unit
(** [reset_state t] resets terminal visual/protocol state to defaults.

    - Disables mouse tracking, bracketed paste, focus events, Kitty keyboard,
      modifyOtherKeys, and Unicode mode.
    - Returns from the alternate screen if active.
    - Resets SGR attributes, cursor colour/style, window title, and cursor
      visibility.

    For TTY outputs, emits the required escape sequences. For non-TTY outputs,
    only updates internal state.

    This does not restore the original termios; use {!close} for full cleanup.

    @raise Error if emitting reset sequences fails *)

(** {1 Events} *)

val read : ?timeout:float -> t -> (Input.t -> unit) -> bool
(** [read ?timeout t on_event] reads input events and passes each to [on_event].

    - Drains any queued events from previous reads.
    - Flushes parser timeouts (e.g., lone Escape keys).
    - Reads available bytes from the terminal and parses them.
    - Folds capability responses into {!capabilities} and {!terminal_info},
      excluding them from the event stream.

    @param timeout
      controls blocking behavior:
      - [timeout < 0.] (default) blocks until at least one event arrives
      - [timeout = 0.] non-blocking; returns immediately with available events
      - [timeout > 0.] waits up to that many seconds for events

    @return [true] if any events were read, [false] on timeout or no data. *)

val poll : t -> (Input.t -> unit) -> bool
(** [poll t on_event] is equivalent to [read ~timeout:0. t on_event].

    Non-blocking: drains all currently available events without waiting. *)

(** {1 Cursor and appearance} *)

val query_cursor_position : ?timeout:float -> t -> (int * int) option
(** [query_cursor_position ?timeout t] asks the terminal for the current cursor
    position and returns [Some (row, col)] if a response is received before
    [timeout] seconds (default 0.05), or [None] otherwise.

    The function:

    - sends a cursor-position report request (CPR);
    - feeds incoming bytes through the internal parser, updating capabilities
      and enqueuing any user events; and
    - scans for a cursor-position capability reply.

    Events consumed while waiting are not replayed, so callers that cannot drop
    events should avoid this helper and instead process CPR replies manually.

    Returns [None] when [output] is not a TTY. *)

val move_cursor : ?visible:bool -> t -> row:int -> col:int -> unit
(** [move_cursor t ?visible ~row ~col] moves the cursor to 1-based coordinates.

    Updates the tracked cursor state and emits a positioning escape when output
    is a TTY. Coordinates are clamped to at least 1. By default, [visible] is
    [true]; pass [~visible:false] to move the cursor without showing it. *)

val set_cursor_style : t -> cursor_style -> blinking:bool -> unit
(** [set_cursor_style t style ~blinking] sets the cursor shape and blinking
    behaviour.

    For TTY outputs, sends DECSCUSR sequences. For non-TTY outputs, only updates
    internal state. The cursor must be visible for visual changes to appear on
    screen. *)

val set_cursor_color : t -> r:float -> g:float -> b:float -> a:float -> unit
(** [set_cursor_color t ~r ~g ~b ~a] sets the cursor colour.

    - Components are clamped to [0.0, 1.0] and converted to 8-bit values.
    - NaN components are treated as [0.0].
    - The alpha channel is tracked but not sent to the terminal.

    For TTY outputs, sends OSC 12 with the computed RGB value. The cursor must
    be visible for changes to take effect. *)
