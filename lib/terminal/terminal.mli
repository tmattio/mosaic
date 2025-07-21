(** Terminal device control and I/O operations.

    This module provides low-level terminal manipulation including mode control,
    screen management, mouse support, and proper state restoration. Handles both
    TTY and non-TTY file descriptors for testing flexibility. Supports Windows
    via VT enabling.

    Terminal state is saved on creation and restored on release. Mode changes
    are reversible. All features test TTY status before applying. Non-TTY
    descriptors work for testing but ignore control sequences. *)

type t
(** [t] represents a terminal device handle.

    Encapsulates input/output file descriptors and terminal state. Tracks
    original settings for restoration. Thread-safe for single writer. *)

type mode =
  [ `Raw
    (** Disables echo, line buffering, and signal generation - suitable for TUI
        apps *)
  | `Cooked  (** Normal line-oriented mode with echo *)
  | `Custom of Unix.terminal_io -> Unix.terminal_io
    (** Allows precise termios control. *) ]
(** [mode] controls terminal input processing behavior. *)

type mouse_mode =
  [ `None  (** No mouse reporting. *)
  | `Normal  (** Basic clicks and motion. *)
  | `Button  (** Motion only on button press. *)
  | `Any  (** All motion events. *)
  | `SgrNormal
    (** SGR mode for extended coordinates - basic clicks and motion. *)
  | `SgrButton  (** SGR mode - motion only on button press. *)
  | `SgrAny  (** SGR mode - all motion events. *) ]

exception Terminal_error of string
(** Raised on terminal operations failures. *)

val create : ?tty:bool -> Unix.file_descr -> Unix.file_descr -> t
(** [create ?tty input output] creates a terminal handle from file descriptors.

    Saves current terminal state for later restoration. Autodetects TTY status
    if not specified. Non-TTY descriptors supported for testing but control
    sequences have no effect.

    @param tty Force TTY status instead of autodetection (default: autodetect)
    @param input File descriptor for reading input
    @param output File descriptor for writing output

    Example: Creates terminal from stdin/stdout.
    {[
      let term = Terminal.create Unix.stdin Unix.stdout
    ]} *)

val with_terminal :
  ?tty:bool -> Unix.file_descr -> Unix.file_descr -> (t -> 'a) -> 'a
(** Creates, runs function, and releases terminal automatically. *)

val release : t -> unit
(** [release t] restores original terminal settings and releases resources.

    Critical for proper cleanup. Restores mode, cursor visibility, and screen
    buffer. Safe to call multiple times. Should be called even on error paths.
*)

val save_state : t -> unit
(** [save_state t] captures current terminal configuration.

    Saves mode, cursor visibility, and other settings. Can save multiple times.
    Latest save used by [restore_state]. Useful for nested mode changes. *)

val restore_state : t -> unit
(** [restore_state t] returns terminal to last saved state.

    Restores mode and settings from most recent [save_state]. No effect if no
    state saved. Safe to call multiple times. *)

val set_mode : t -> mode -> unit
(** [set_mode t mode] changes terminal input processing mode.

    Applies immediately. Previous mode lost unless saved. Raw mode typical for
    TUI apps. No effect on non-TTY descriptors. Changes persist until explicitly
    changed or restored. *)

val size : t -> int * int
(** Gets current size (width, height). *)

val is_tty : Unix.file_descr -> bool
(** Checks if fd is TTY. *)

val input_fd : t -> Unix.file_descr
val output_fd : t -> Unix.file_descr
val write : t -> bytes -> int -> int -> unit

val write_string : t -> string -> unit
(** Writes UTF-8 string. *)

val read : t -> bytes -> int -> int -> int
(** Reads from input. *)

val wait_for_input : t -> float -> bool
(** Waits for input with timeout. Returns true if ready. *)

val enable_alternate_screen : t -> unit
(** [enable_alternate_screen t] switches to alternate screen buffer.

    Saves current screen content and shows blank screen. Used by full-screen
    apps. Screen content restored by [disable_alternate_screen]. No effect on
    non-TTY. *)

val disable_alternate_screen : t -> unit
(** [disable_alternate_screen t] returns to main screen buffer.

    Restores screen content saved by [enable_alternate_screen]. Alternate screen
    content is lost. No effect on non-TTY. *)

val set_mouse_mode : t -> mouse_mode -> unit
(** [set_mouse_mode t mode] configures mouse reporting mode. *)

val enable_focus_reporting : t -> unit
val disable_focus_reporting : t -> unit

val enable_bracketed_paste : t -> unit
(** [enable_bracketed_paste t] enables bracketed paste mode.

    Wraps pasted text in escape sequences for safe handling. Prevents paste from
    triggering commands. No effect on non-TTY. *)

val disable_bracketed_paste : t -> unit
(** [disable_bracketed_paste t] disables bracketed paste mode.

    Pasted text appears as normal typed input. Cannot distinguish from
    keystrokes. *)

val enable_kitty_keyboard : t -> unit
(** [enable_kitty_keyboard t] enables enhanced keyboard reporting.

    Allows terminals to send distinct escape sequences for key combinations like
    Shift+Enter. Required for detecting modified keys. No effect on non-TTY. *)

val disable_kitty_keyboard : t -> unit
(** [disable_kitty_keyboard t] disables enhanced keyboard reporting.

    Returns to standard keyboard mode. Should be called before release if
    enabled. *)

val show_cursor : t -> unit
(** [show_cursor t] makes cursor visible.

    Reverses [hide_cursor]. Cursor blinks at current position. No effect on
    non-TTY. *)

val hide_cursor : t -> unit
(** [hide_cursor t] makes cursor invisible.

    Cursor position still tracked but not shown. Reduces flicker during
    rendering. Remember to show cursor before user input. *)

val clear_screen : t -> unit

val move_cursor : t -> int -> int -> unit
(** Moves to row, col (1-based). *)

val set_title : t -> string -> unit
val bell : t -> unit

val flush : t -> unit
(** [flush t] ensures all output reaches the terminal.

    Forces buffered output to terminal. Important after rendering to ensure
    display updates immediately. *)

val set_resize_handler : t -> (int * int -> unit) -> unit
(** Sets per-terminal resize handler. *)

val remove_resize_handlers : t -> unit
(** Removes all handlers for this terminal. *)

val set_dark_background : t -> dark:bool -> unit
(** [set_dark_background t ~dark] manually sets the terminal background state.

    This overrides automatic detection. Use when you know the terminal's
    background color and want to ensure correct adaptive color rendering.

    @param dark [true] for dark backgrounds, [false] for light backgrounds *)

val has_dark_background : t -> bool
(** [has_dark_background t] returns whether the terminal has a dark background.

    Uses the following detection methods in order: 1. Previously set value via
    [set_dark_background] 2. COLORFGBG environment variable 3. OSC 11 query to
    terminal (if TTY) 4. TERM environment variable heuristics 5. Default to dark
    (true) *)

val has_truecolor_support : t -> bool
(** Detects truecolor support. *)

type feature =
  [ `AlternateScreen  (** Alternate screen buffer support *)
  | `Mouse  (** Basic mouse support *)
  | `Truecolor  (** 24-bit color support *)
  | `Kitty  (** Kitty keyboard protocol *)
  | `BracketedPaste  (** Bracketed paste mode *)
  | `FocusReporting  (** Focus in/out reporting *) ]

val supports_feature : t -> feature -> bool
(** [supports_feature t feature] checks if terminal supports a specific feature.

    Uses environment variables, terminal queries, and heuristics to determine
    feature support. Returns false for non-TTY descriptors.

    @param t Terminal handle
    @param feature Feature to check support for
    @return true if feature is supported, false otherwise *)

val enable_mouse_sgr : t -> unit
(** [enable_mouse_sgr t] enables SGR mouse mode for extended coordinates.

    SGR mode allows mouse coordinates beyond 223 columns/rows. Should be
    combined with a regular mouse mode like `Normal or `Any. No effect on
    non-TTY. *)

val disable_mouse_sgr : t -> unit
(** [disable_mouse_sgr t] disables SGR mouse mode.

    Returns to standard mouse coordinate reporting. No effect on non-TTY. *)

val set_non_blocking : t -> bool -> unit
(** [set_non_blocking t enabled] controls non-blocking I/O mode.

    When enabled, read/write operations won't block. Useful for event loops.
    Automatically enabled in raw mode, disabled in cooked mode.

    @param t Terminal handle
    @param enabled true to enable non-blocking I/O, false to disable
    @raise Terminal_error if system call fails *)

(** {2 Testing Support} *)

val create_from_strings : string -> t * (unit -> string) * (unit -> unit)
(** Creates mock terminal; returns term, get_output, close_mock. *)

(** {2 Windows Support}

    On Windows 10+, VT processing is automatically enabled. The
    terminal_enable_vt function attempts to enable ANSI escape sequence
    processing. Older Windows versions may have limited support for terminal
    features. *)
