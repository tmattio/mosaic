(** Terminal device control and I/O operations.

    This module provides low-level terminal manipulation including mode control,
    screen management, mouse support, and proper state restoration. Handles both
    TTY and non-TTY file descriptors for testing flexibility.

    Terminal state is saved on creation and restored on release. Mode changes
    are reversible. All features test TTY status before applying. Non-TTY
    descriptors work for testing but ignore control sequences. Signal handlers
    are process-global and must be managed carefully. *)

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

(** {2 Terminal Features} *)

val enable_alternate_screen : t -> unit
(** [enable_alternate_screen t] switches to alternate screen buffer.

    Saves current screen content and shows blank screen. Used by full-screen
    apps. Screen content restored by [disable_alternate_screen]. No effect on
    non-TTY. *)

val disable_alternate_screen : t -> unit
(** [disable_alternate_screen t] returns to main screen buffer.

    Restores screen content saved by [enable_alternate_screen]. Alternate screen
    content is lost. No effect on non-TTY. *)

val enable_mouse : t -> unit
(** [enable_mouse t] enables mouse event reporting.

    Terminal sends escape sequences for mouse actions. Required for mouse
    support. Use Event_source to parse mouse events. No effect on non-TTY. *)

val disable_mouse : t -> unit
(** [disable_mouse t] disables mouse event reporting.

    Returns mouse to normal selection mode. Terminal stops sending mouse
    escapes. Should be called before [release] if mouse was enabled. *)

val enable_bracketed_paste : t -> unit
(** [enable_bracketed_paste t] enables bracketed paste mode.

    Wraps pasted text in escape sequences for safe handling. Prevents paste from
    triggering commands. No effect on non-TTY. *)

val disable_bracketed_paste : t -> unit
(** [disable_bracketed_paste t] disables bracketed paste mode.

    Pasted text appears as normal typed input. Cannot distinguish from
    keystrokes. *)

val show_cursor : t -> unit
(** [show_cursor t] makes cursor visible.

    Reverses [hide_cursor]. Cursor blinks at current position. No effect on
    non-TTY. *)

val hide_cursor : t -> unit
(** [hide_cursor t] makes cursor invisible.

    Cursor position still tracked but not shown. Reduces flicker during
    rendering. Remember to show cursor before user input. *)

val size : t -> int * int
(** [size t] returns current terminal dimensions as [(width, height)].

    Queries terminal for size in character cells. Returns (80, 24) for non-TTY.
    May change during execution - see [set_sigwinch_handler] for resize events.

    @raise Unix.Unix_error on query failure (rare) *)

val is_tty : Unix.file_descr -> bool
(** [is_tty fd] tests whether file descriptor is a terminal device.

    True for interactive terminals, false for pipes, files, etc. Determines
    whether control sequences will have effect. *)

val input_fd : t -> Unix.file_descr
(** [input_fd t] returns the input file descriptor.

    Useful for select/poll operations. Do not close directly - use [release]. *)

val output_fd : t -> Unix.file_descr
(** [output_fd t] returns the output file descriptor.

    Useful for select/poll operations. Do not close directly - use [release]. *)

val write : t -> bytes -> int -> int -> unit
(** [write t buf ofs len] writes bytes to terminal output.

    Low-level write operation. Handles partial writes internally. Most users
    should use higher-level rendering functions.

    @param buf Byte buffer containing data
    @param ofs Starting offset in buffer
    @param len Number of bytes to write *)

val flush : t -> unit
(** [flush t] ensures all output reaches the terminal.

    Forces buffered output to terminal. Important after rendering to ensure
    display updates immediately. *)

(** {2 Signal Handling} *)

type sigwinch_handler = int * int -> unit
(** [sigwinch_handler] receives new terminal dimensions on resize.

    Called with (width, height) when terminal window changes size. Runs in
    signal handler context - keep operations minimal. *)

val set_sigwinch_handler : sigwinch_handler option -> unit
(** [set_sigwinch_handler handler] installs global terminal resize handler.

    Only one handler active at a time. Pass [None] to remove handler. Handler
    called on SIGWINCH signal with new dimensions. Process-global effect - use
    carefully in libraries.

    Example: Updates layout on terminal resize.
    {[
      Terminal.set_sigwinch_handler
        (Some (fun (w, h) -> Printf.eprintf "Resized to %dx%d\n" w h))
    ]} *)

(** {2 Testing Support} *)

val create_from_strings : string -> t * (unit -> string)
(** [create_from_strings input] creates a mock terminal for testing.

    Returns terminal handle and function to retrieve output. Input string
    consumed as if typed. Output captured instead of displaying. No actual
    terminal required. Control sequences processed but not executed.

    Example: Tests terminal interaction.
    {[
      let term, get_output = Terminal.create_from_strings "hello\n" in
      Terminal.write term (Bytes.of_string "prompt> ") 0 8;
      Terminal.flush term;
      let output = get_output () in
      assert (output = "prompt> ")
    ]} *)
