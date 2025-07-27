(** Terminal device control and I/O operations.

    Terminal provides low-level control over terminal devices including mode
    switching, screen management, mouse tracking, and color detection. The
    module ensures proper state restoration through automatic cleanup and
    supports both TTY and non-TTY file descriptors for testing flexibility.

    {1 Overview}

    The terminal handle encapsulates input/output file descriptors and maintains
    terminal state. Create a terminal, configure its features, then release it:

    {[
      let term = Terminal.create Unix.stdin Unix.stdout in
      Terminal.set_mode term `Raw;
      Terminal.hide_cursor term;
      (* Your application logic here *)
      Terminal.show_cursor term;
      Terminal.release term
    ]}

    Or use {!with_terminal} for automatic cleanup:

    {[
      Terminal.with_terminal Unix.stdin Unix.stdout (fun term ->
          Terminal.set_mode term `Raw;
          Terminal.enable_alternate_screen term
          (* Application runs here *)
          (* Terminal automatically restored on exit *))
    ]}

    {1 Key Concepts}

    {2 Terminal Modes}

    Terminal input processing has three primary modes:
    - {b Raw}: Direct character input without echo or line buffering
    - {b Cooked}: Normal line-oriented mode with echo and editing
    - {b Custom}: Fine-grained control via termios settings

    {2 State Management}

    The module tracks terminal state at multiple levels:
    - {b Original state}: Captured at creation, restored on release
    - {b Saved state}: Explicitly saved/restored for nested operations
    - {b Current state}: Active mode and feature settings

    {2 Feature Detection}

    Not all terminals support all features. Use {!supports_feature} to check
    capabilities before enabling advanced features:

    {[
      if Terminal.supports_feature term `Truecolor then
        (* Use 24-bit colors *)
      else
        (* Fall back to 256 colors *)
    ]}

    {1 Features}

    - Terminal mode control (raw, cooked, custom)
    - State preservation and restoration
    - Screen and cursor management
    - Mouse tracking with extended coordinate support
    - Color detection (dark/light background, truecolor)
    - Window resize handling via SIGWINCH
    - Alternate screen buffer switching
    - Bracketed paste and focus reporting
    - Kitty keyboard protocol support
    - Cross-platform support (Unix/Windows via VT processing)
    - Non-TTY operation for testing

    {1 Invariants}

    - Terminal state is always restored on {!release}
    - Non-TTY descriptors accept all operations but ignore control sequences
    - Mode changes take effect immediately
    - Feature detection results are cached after first query
    - Only one resize handler per terminal is active at a time *)

(** {1 Types} *)

type t
(** Terminal device handle.

    Encapsulates input/output file descriptors and terminal state. The handle
    tracks both the original terminal settings (for restoration on release) and
    any explicitly saved states. Thread-safe for single writer.

    Invariant: Original terminal state is captured at creation and restored on
    release, even if the program exits abnormally. *)

type mode =
  [ `Raw
    (** Raw mode for character-at-a-time input.

        Disables:
        - Echo (typed characters don't appear)
        - Line buffering (input available immediately)
        - Signal generation (Ctrl+C doesn't send SIGINT)
        - Input/output processing (CR-LF translation)

        Enables:
        - Non-blocking I/O by default

        This mode is standard for TUI applications that need direct control over
        input handling. *)
  | `Cooked
    (** Normal line-oriented mode.

        Enables:
        - Echo (typed characters appear)
        - Line buffering (input available after Enter)
        - Signal generation (Ctrl+C sends SIGINT)
        - Canonical input processing

        This is the default terminal mode for shell interaction. *)
  | `Custom of Unix.terminal_io -> Unix.terminal_io
    (** Custom termios transformation.

        Receives current termios settings and returns modified settings. Useful
        for partial raw modes or specific protocol requirements.

        Example: Raw mode without disabling signals
        {[
          let custom_mode termios =
            {
              termios with
              c_echo = false;
              c_icanon = false;
              c_isig = true;
              (* Keep signals enabled *)
            }
          in
          Terminal.set_mode term (`Custom custom_mode)
        ]} *) ]
(** Terminal input processing mode.

    Mode changes take effect immediately and persist until explicitly changed or
    the terminal is restored. In raw mode, remember to handle special characters
    (like Ctrl+C) in your application since they won't generate signals. *)

type mouse_mode =
  [ `None  (** No mouse reporting (default). *)
  | `Normal
    (** Reports button press/release events.

        Legacy X10 mode limited to coordinates (223, 223) due to protocol
        constraints. Use SGR variants for extended coordinates. *)
  | `Button
    (** Reports button events and motion while buttons pressed.

        Useful for drag operations. Limited to (223, 223) in legacy mode. *)
  | `Any
    (** Reports all mouse events including motion without buttons.

        High event volume - use for hover effects or precise tracking. Limited
        to (223, 223) in legacy mode. *)
  | `SgrNormal
    (** SGR mode: button events with extended coordinates.

        No coordinate limits. Preferred over legacy `Normal. *)
  | `SgrButton
    (** SGR mode: button events and drag motion.

        No coordinate limits. Preferred over legacy `Button. *)
  | `SgrAny
    (** SGR mode: all events including passive motion.

        No coordinate limits. Preferred over legacy `Any. *) ]
(** Mouse tracking mode.

    SGR modes support coordinates beyond (223, 223) and should be preferred for
    modern applications. Legacy modes exist for compatibility. Mouse events are
    reported through escape sequences parsed by the Input module. *)

type feature =
  [ `AlternateScreen  (** Alternate screen buffer support *)
  | `Mouse  (** Basic mouse support *)
  | `Truecolor  (** 24-bit color support *)
  | `Kitty  (** Kitty keyboard protocol *)
  | `BracketedPaste  (** Bracketed paste mode *)
  | `FocusReporting  (** Focus in/out reporting *) ]
(** Terminal capability features.

    Use {!supports_feature} to check if a terminal supports specific
    capabilities before enabling them. Feature detection uses environment
    variables, terminal queries, and known terminal databases. *)

exception Terminal_error of string
(** Raised on terminal operation failures.

    Common causes:
    - System call failures (permissions, invalid descriptors)
    - Attempting operations on closed terminals
    - Platform-specific errors (e.g., VT processing on Windows) *)

(** {1 Creation and Lifecycle} *)

val create : ?tty:bool -> Unix.file_descr -> Unix.file_descr -> t
(** [create ?tty input output] creates a terminal handle.

    Captures the current terminal state for restoration on release. On Windows,
    attempts to enable VT processing for ANSI escape sequence support.

    The terminal starts in raw mode on TTY devices. Use {!set_mode} to change to
    cooked mode if needed.

    @param tty
      Force TTY status. [true] treats descriptors as TTY regardless of actual
      status. [false] forces non-TTY mode. Default: autodetect using
      {!Unix.isatty}
    @param input File descriptor for reading input
    @param output File descriptor for writing output
    @return Terminal handle that must be released with {!release}

    @raise Terminal_error if terminal operations fail during initialization

    {4 Examples}

    {[
      let term = Terminal.create Unix.stdin Unix.stdout
    ]} *)

val with_terminal :
  ?tty:bool -> Unix.file_descr -> Unix.file_descr -> (t -> 'a) -> 'a
(** [with_terminal ?tty input output f] creates a terminal and ensures cleanup.

    Creates a terminal, runs [f] with it, then releases the terminal even if [f]
    raises an exception. This is the recommended way to use terminals as it
    guarantees proper cleanup.

    @param tty Force TTY status (default: autodetect)
    @param input Input file descriptor
    @param output Output file descriptor
    @param f Function to run with the terminal
    @return Result of calling [f]

    @raise Terminal_error if terminal initialization fails
    @raise any exception raised by [f]

    {4 Examples}

    {[
      Terminal.with_terminal Unix.stdin Unix.stdout (fun term ->
          Terminal.set_mode term `Raw;
          Terminal.clear_screen term;
          run_app term (* Terminal restored even if run_app raises *))
    ]} *)

val release : t -> unit
(** [release t] restores terminal state and releases resources.

    Restores the terminal to its original state captured at creation time. This
    includes:
    - Terminal mode (raw/cooked)
    - Cursor visibility
    - Screen buffer (returns to main if alternate was used)
    - Mouse mode (disabled)
    - Special modes (Kitty keyboard, bracketed paste, etc.)

    Safe to call multiple times - subsequent calls have no effect. Should be
    called even on error paths to ensure proper cleanup. *)

(** {1 State Management} *)

val save_state : t -> unit
(** [save_state t] captures current terminal configuration.

    Saves the current terminal mode and settings for later restoration with
    {!restore_state}. Can be called multiple times - each call overwrites the
    previous saved state.

    Useful for temporary mode changes or nested terminal operations.

    {4 Examples}

    {[
      Terminal.save_state term;
      Terminal.set_mode term `Raw;
      Terminal.write_string term "Password: ";
      let password = read_password term in
      Terminal.restore_state term;
      password
    ]} *)

val restore_state : t -> unit
(** [restore_state t] returns terminal to last saved state.

    Restores the terminal configuration captured by the most recent
    {!save_state} call. Has no effect if {!save_state} was never called.

    Safe to call multiple times - repeated calls have no effect unless
    {!save_state} is called again.

    Note: This restores to the last saved state, not the original state. Use
    {!release} to restore to the original terminal state. *)

(** {1 Terminal Configuration} *)

val set_mode : t -> mode -> unit
(** [set_mode t mode] changes terminal input processing mode.

    Mode changes take effect immediately. The previous mode is lost unless
    explicitly saved with {!save_state}. On non-TTY descriptors, this function
    has no effect.

    Raw mode automatically enables non-blocking I/O. Cooked mode restores
    blocking I/O.

    @param t Terminal handle
    @param mode New terminal mode to apply

    @raise Terminal_error if the mode change fails

    {4 Examples}

    {[
      Terminal.set_mode term `Raw
      (* Input now available character by character *)
    ]}

    {[
      Terminal.set_mode term
        (`Custom
           (fun termios ->
             {
               termios with
               c_echo = false;
               c_icanon = false;
               c_isig = true;
               (* Keep signals enabled *)
             }))
    ]} *)

val size : t -> int * int
(** [size t] returns current terminal dimensions.

    @return [(width, height)] in characters
    @raise Terminal_error if size cannot be determined

    {4 Examples}

    {[
      let width, height = Terminal.size term in
      Printf.printf "Terminal: %d×%d\n" width height
    ]} *)

val is_tty : Unix.file_descr -> bool
(** [is_tty fd] checks if a file descriptor is a terminal.

    @return [true] if [fd] is a terminal, [false] otherwise

    {4 Examples}

    Check before creating terminal:
    {[
      if Terminal.is_tty Unix.stdin && Terminal.is_tty Unix.stdout then
        (* Safe to use terminal features *)
        let term = Terminal.create Unix.stdin Unix.stdout in
        ...
      else
        (* Fall back to simple I/O *)
        ...
    ]} *)

(** {1 Input/Output Operations} *)

val input_fd : t -> Unix.file_descr
(** [input_fd t] returns the input file descriptor.

    Useful for select/poll operations or when interfacing with other I/O
    libraries. *)

val output_fd : t -> Unix.file_descr
(** [output_fd t] returns the output file descriptor.

    Useful for select/poll operations or when interfacing with other I/O
    libraries. *)

val write : t -> bytes -> int -> int -> unit
(** [write t buf ofs len] writes bytes to the terminal.

    Writes [len] bytes from [buf] starting at offset [ofs]. Automatically
    handles partial writes by retrying until all data is written.

    @param t Terminal handle
    @param buf Buffer containing data to write
    @param ofs Starting offset in buffer
    @param len Number of bytes to write

    @raise Invalid_argument if [ofs] and [len] don't specify a valid substring
    @raise Terminal_error on write failure *)

val write_string : t -> string -> unit
(** [write_string t s] writes a UTF-8 string to the terminal.

    Convenience function that converts the string to bytes and writes it.
    Handles partial writes automatically.

    @param t Terminal handle
    @param s UTF-8 encoded string to write

    @raise Terminal_error on write failure

    {4 Examples}

    {[
      Terminal.write_string term "Hello, World!\n"
    ]} *)

val read : t -> bytes -> int -> int -> int
(** [read t buf ofs len] reads bytes from the terminal.

    Reads up to [len] bytes into [buf] starting at offset [ofs]. In raw mode
    with non-blocking I/O, may return 0 if no input is available.

    @param t Terminal handle
    @param buf Buffer to store read data
    @param ofs Starting offset in buffer
    @param len Maximum bytes to read
    @return Number of bytes actually read (0 to [len])

    @raise Invalid_argument if [ofs] and [len] don't specify valid range
    @raise Terminal_error on read failure

    {4 Examples}

    {[
      let buf = Bytes.create 1024 in
      let n = Terminal.read term buf 0 1024 in
      if n > 0 then process_input (Bytes.sub_string buf 0 n)
    ]} *)

val wait_for_input : t -> float -> bool
(** [wait_for_input t timeout] waits for input availability.

    Blocks until input is available or timeout expires. Useful for implementing
    event loops without busy waiting.

    @param t Terminal handle
    @param timeout Maximum seconds to wait (negative means infinite)
    @return [true] if input is available, [false] if timeout expired

    {4 Examples}

    {[
      if Terminal.wait_for_input term 0.1 then
        (* Input available *)
        let n = Terminal.read term buf 0 1024 in
        ...
    ]} *)

(** {1 Screen Management} *)

val enable_alternate_screen : t -> unit
(** [enable_alternate_screen t] switches to alternate screen buffer.

    Saves the current screen content and switches to a blank alternate buffer.
    The original screen content is restored when {!disable_alternate_screen} is
    called or the terminal is released.

    This is standard practice for full-screen TUI applications to avoid
    disrupting the user's shell session.

    No effect on non-TTY descriptors.

    {4 Examples}

    {[
      Terminal.enable_alternate_screen term;
      Terminal.clear_screen term;
      (* Application uses full screen *)
      Terminal.disable_alternate_screen term
      (* User's shell content restored *)
    ]} *)

val disable_alternate_screen : t -> unit
(** [disable_alternate_screen t] returns to main screen buffer.

    Restores the screen content that was saved when {!enable_alternate_screen}
    was called. The alternate screen content is discarded.

    No effect if alternate screen is not active or on non-TTY descriptors. *)

(** {1 Mouse Support} *)

val set_mouse_mode : t -> mouse_mode -> unit
(** [set_mouse_mode t mode] configures mouse event reporting.

    Mouse events are reported as escape sequences that should be parsed by the
    Input module. SGR modes support unlimited coordinates while legacy modes are
    limited to (223, 223).

    No effect on non-TTY descriptors.

    @param t Terminal handle
    @param mode Mouse reporting mode

    {4 Examples}

    {[
      if Terminal.supports_feature term `Mouse then
        Terminal.set_mouse_mode term `SgrNormal
      (* Now receiving button press/release events *)
    ]} *)

val enable_mouse_sgr : t -> unit
(** [enable_mouse_sgr t] enables SGR mouse protocol.

    SGR (Select Graphic Rendition) protocol removes the coordinate limitations
    of legacy mouse modes. Should be used with SGR mouse modes for extended
    coordinate support.

    No effect on non-TTY descriptors. *)

val disable_mouse_sgr : t -> unit
(** [disable_mouse_sgr t] disables SGR mouse protocol.

    Returns to legacy mouse coordinate encoding. Coordinates beyond (223, 223)
    will be clamped or produce invalid sequences.

    No effect on non-TTY descriptors. *)

(** {1 Advanced Features} *)

val enable_focus_reporting : t -> unit
(** [enable_focus_reporting t] enables focus change notifications.

    When enabled, the terminal sends escape sequences when it gains or loses
    focus. Parse these with the Input module to receive {!Input.Focus_in} and
    {!Input.Focus_out} events.

    No effect on non-TTY descriptors. *)

val disable_focus_reporting : t -> unit
(** [disable_focus_reporting t] disables focus change notifications.

    Stops the terminal from sending focus event sequences.

    No effect on non-TTY descriptors. *)

val enable_bracketed_paste : t -> unit
(** [enable_bracketed_paste t] enables bracketed paste mode.

    When enabled, pasted text is wrapped in special escape sequences that allow
    applications to distinguish pasted text from typed input. This prevents
    pasted text from triggering commands or keyboard shortcuts.

    The Input module parses these sequences and provides the pasted text in
    {!Input.Paste} events.

    No effect on non-TTY descriptors. *)

val disable_bracketed_paste : t -> unit
(** [disable_bracketed_paste t] disables bracketed paste mode.

    Returns to normal paste behavior where pasted text is indistinguishable from
    typed input. Pasted content may trigger keyboard shortcuts or commands.

    No effect on non-TTY descriptors. *)

val enable_kitty_keyboard : t -> unit
(** [enable_kitty_keyboard t] enables Kitty keyboard protocol.

    The Kitty protocol provides enhanced keyboard reporting with:
    - Distinct sequences for all key combinations (e.g., Shift+Enter vs Enter)
    - Key press/release/repeat events
    - Associated text for key events
    - Unambiguous encoding of all keys

    Required for applications that need to detect modified keys or key release
    events. The Input module automatically parses Kitty protocol sequences.

    No effect on non-TTY descriptors. *)

val disable_kitty_keyboard : t -> unit
(** [disable_kitty_keyboard t] disables Kitty keyboard protocol.

    Returns to standard keyboard reporting where many key combinations are
    indistinguishable (e.g., Shift+Enter sends same sequence as Enter).

    Should be called before {!release} if Kitty protocol was enabled, though
    {!release} will disable it automatically.

    No effect on non-TTY descriptors. *)

(** {1 Cursor Control} *)

val show_cursor : t -> unit
(** [show_cursor t] makes the cursor visible.

    Shows the cursor at its current position. The cursor typically blinks
    depending on terminal settings. Call this before accepting user input.

    No effect on non-TTY descriptors. *)

val hide_cursor : t -> unit
(** [hide_cursor t] makes the cursor invisible.

    Hides the cursor but maintains its position. Commonly used during rendering
    to prevent cursor flicker. Remember to show the cursor before accepting user
    input.

    No effect on non-TTY descriptors. *)

val move_cursor : t -> int -> int -> unit
(** [move_cursor t row col] positions the cursor.

    Moves cursor to specified position using 1-based coordinates where (1, 1) is
    the top-left corner.

    @param t Terminal handle
    @param row Target row (1-based)
    @param col Target column (1-based)

    {4 Examples}

    {[
      Terminal.move_cursor term 1 1 (* Top-left corner *)
    ]} *)

(** {1 Screen Operations} *)

val clear_screen : t -> unit
(** [clear_screen t] clears the entire screen.

    Erases all content and moves the cursor to position (1, 1).

    No effect on non-TTY descriptors. *)

val set_title : t -> string -> unit
(** [set_title t title] sets the terminal window title.

    Updates the terminal window or tab title. Support varies by terminal
    emulator. Some terminals may ignore this or have it disabled for security.

    No effect on non-TTY descriptors.

    @param t Terminal handle
    @param title New window title

    {4 Examples}

    {[
      Terminal.set_title term (Printf.sprintf "Editor - %s" filename)
    ]} *)

val bell : t -> unit
(** [bell t] sounds the terminal bell.

    Triggers an audible or visual bell depending on terminal settings. Often
    used for alerts or errors.

    No effect on non-TTY descriptors. *)

val flush : t -> unit
(** [flush t] ensures all output reaches the terminal.

    Forces any buffered output to be written immediately. Call this after
    rendering operations to ensure the display updates. *)

(** {1 Window Management} *)

val set_resize_handler : t -> (int * int -> unit) -> unit
(** [set_resize_handler t handler] registers a terminal resize callback.

    The handler is called with (width, height) when the terminal is resized.
    Only one handler per terminal is active - setting a new handler replaces the
    previous one.

    Uses SIGWINCH signal internally on Unix systems.

    @param t Terminal handle
    @param handler Function called with new dimensions on resize

    {4 Examples}

    {[
      Terminal.set_resize_handler term (fun (w, h) ->
          Terminal.clear_screen term;
          draw_ui term w h;
          Terminal.flush term)
    ]} *)

val remove_resize_handlers : t -> unit
(** [remove_resize_handlers t] unregisters resize callbacks.

    Removes any resize handler set for this terminal. The terminal will no
    longer receive resize notifications. *)

(** {1 Color and Feature Detection} *)

val set_dark_background : t -> dark:bool -> unit
(** [set_dark_background t ~dark] manually sets background color mode.

    Overrides automatic background detection. Use when you know the terminal's
    background color and want to ensure correct color choices.

    @param t Terminal handle
    @param dark [true] for dark backgrounds, [false] for light backgrounds

    {4 Examples}

    {[
      let dark = Config.get_bool "dark_mode" in
      Terminal.set_dark_background term ~dark
    ]} *)

val has_dark_background : t -> bool
(** [has_dark_background t] detects if terminal has dark background.

    Uses multiple detection methods in order: 1. Manual setting via
    {!set_dark_background} 2. COLORFGBG environment variable 3. OSC 11 terminal
    query (if TTY) 4. TERM environment variable heuristics 5. Default to dark
    ([true])

    The result is cached after first call.

    @return [true] if background is dark, [false] if light

    {4 Examples}

    {[
      let fg_color =
        if Terminal.has_dark_background term then "\027[97m" else "\027[30m"
    ]} *)

val has_truecolor_support : t -> bool
(** [has_truecolor_support t] detects 24-bit color support.

    Checks for truecolor (16 million color) support using:
    - COLORTERM environment variable
    - Terminal type detection
    - Known terminal capabilities

    The result is cached after first call.

    @return [true] if terminal supports 24-bit color *)

val supports_feature : t -> feature -> bool
(** [supports_feature t feature] checks terminal capabilities.

    Uses environment variables, terminal queries, and terminal type databases to
    determine feature support. Always returns [false] for non-TTY descriptors.

    Results are cached per feature after first check.

    @param t Terminal handle
    @param feature Feature to check
    @return [true] if supported, [false] otherwise

    {4 Examples}

    {[
      if Terminal.supports_feature term `Mouse then
        Terminal.set_mouse_mode term `SgrNormal;

      if Terminal.supports_feature term `Kitty then
        Terminal.enable_kitty_keyboard term
    ]} *)

val set_non_blocking : t -> bool -> unit
(** [set_non_blocking t enabled] controls non-blocking I/O mode.

    When enabled, read/write operations return immediately rather than blocking.
    Raw mode automatically enables non-blocking I/O, while cooked mode disables
    it.

    @param t Terminal handle
    @param enabled [true] for non-blocking, [false] for blocking I/O
    @raise Terminal_error if the system call fails

    {4 Examples}

    {[
      Terminal.set_non_blocking term true;
      let rec loop () =
        match Terminal.read term buf 0 1024 with
        | 0 -> Unix.sleep 0.01; loop ()
        | n -> process_input (Bytes.sub buf 0 n); loop ()
    ]} *)

(** {1 Testing Support} *)

val create_from_strings : string -> t * (unit -> string) * (unit -> unit)
(** [create_from_strings input] creates a mock terminal for testing.

    Creates a terminal that reads from the provided string and writes to an
    internal buffer. Control sequences have no effect.

    @param input String to use as input data
    @return
      [(term, get_output, close)] where:
      - [term] is the mock terminal handle
      - [get_output ()] returns all output written so far
      - [close ()] closes the mock file descriptors

    {4 Examples}

    {[
      let input = "hello\nworld\n" in
      let term, get_output, close = Terminal.create_from_strings input in
      let buf = Bytes.create 5 in
      let n = Terminal.read term buf 0 5 in
      assert (n = 5);
      assert (Bytes.sub_string buf 0 5 = "hello");
      Terminal.write_string term "processed";
      assert (get_output () = "processed");
      close ()
    ]} *)

(** {1 Platform Notes}

    {2 Windows Support}

    On Windows 10 and later, VT (Virtual Terminal) processing is automatically
    enabled during terminal creation to support ANSI escape sequences. This
    provides compatibility with Unix-style terminal control.

    Older Windows versions may have limited support for terminal features.
    Feature detection will return appropriate values for the platform.

    {2 Non-TTY Operation}

    When file descriptors are not terminals (pipes, files, etc.), the module
    operates in a degraded mode:
    - All operations are accepted but control sequences have no effect
    - {!is_tty} returns [false]
    - {!supports_feature} always returns [false]
    - Read/write operations work normally
    - Useful for testing and output redirection *)
