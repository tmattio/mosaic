(** A Virtual Terminal Emulator (VTE).

    This module provides an in-memory virtual terminal model. It processes a
    stream of bytes, interprets ANSI escape codes, and maintains the state of a
    character grid, including text styles, colors, and cursor position.

    The primary workflow is: 1. Create a VTE instance with {!create}. 2. Feed
    raw byte data from a process or file to it using {!feed}. 3. Query the state
    using functions like {!get_cell}, {!cursor_pos}, and {!title} to render the
    terminal display. *)

module Cell = Cell
(** Cell representation and styling *)

module Grid = Grid
(** Grid management and diffing *)

(** {1 Core Types} *)

type t
(** An abstract type representing the complete state of the virtual terminal,
    including the grid, cursor, and parser state. *)

(** {1 Lifecycle and Configuration} *)

val create : ?scrollback:int -> rows:int -> cols:int -> unit -> t
(** [create ?scrollback ~rows ~cols ()] creates a new VTE instance.
    @param scrollback
      The number of lines to keep in the scrollback buffer. Defaults to 1000.
    @param rows The number of rows (height) in the terminal grid.
    @param cols The number of columns (width) in the terminal grid. *)

val feed : t -> bytes -> int -> int -> unit
(** [feed t bytes ofs len] processes a chunk of a byte stream. It parses ANSI
    escape codes and printable characters from the given [bytes] buffer (from
    offset [ofs] of length [len]) and updates the internal state of [t]
    accordingly. *)

val reset : t -> unit
(** [reset t] resets the terminal state to its initial configuration. This
    clears the screen and scrollback, moves the cursor to (0,0), and resets all
    graphical attributes to their defaults. *)

(** {1 State Queries} *)

val get_cell : t -> row:int -> col:int -> Cell.t option
(** [get_cell t ~row ~col] returns the cell at the specified 0-indexed
    coordinates. Returns [None] if the coordinates are out of bounds or if the
    cell is empty. This is the primary function for reading grid content for
    rendering. *)

val rows : t -> int
(** [rows t] returns the number of rows in the terminal grid. *)

val cols : t -> int
(** [cols t] returns the number of columns in the terminal grid. *)

val cursor_pos : t -> int * int
(** [cursor_pos t] returns the current 0-indexed (row, col) of the cursor. *)

val is_cursor_visible : t -> bool
(** [is_cursor_visible t] returns [true] if the cursor should be displayed, and
    [false] if it is hidden. *)

val set_cursor_visible : t -> bool -> unit
(** [set_cursor_visible t visible] sets whether the cursor should be visible. *)

val is_cursor_key_mode : t -> bool
(** [is_cursor_key_mode t] returns [true] if cursor keys are in application
    mode. *)

val is_insert_mode : t -> bool
(** [is_insert_mode t] returns [true] if insert mode is active. *)

val is_auto_newline_mode : t -> bool
(** [is_auto_newline_mode t] returns [true] if automatic newline mode is active.
*)

val is_auto_wrap_mode : t -> bool
(** [is_auto_wrap_mode t] returns [true] if automatic wrap mode is active. *)

val title : t -> string
(** [title t] returns the current terminal window title as set by an OSC
    (Operating System Command) sequence. *)

val is_dirty : t -> bool
(** [is_dirty t] returns [true] if the terminal state has changed since the last
    call to [clear_dirty]. This is useful for determining when to capture frames
    or refresh the display. *)

val clear_dirty : t -> unit
(** [clear_dirty t] marks the terminal as clean, resetting the dirty flag. This
    should be called after capturing or rendering the current state. *)

(** {1 Grid Manipulation} *)

val get_grid : t -> Grid.t
(** [get_grid t] returns the current grid state. Useful for diffing and advanced
    rendering operations. *)

val scroll_up : t -> int -> unit
(** [scroll_up t n] scrolls the grid content up by [n] lines. New blank lines
    are inserted at the bottom. Lines scrolled off the top are added to the
    scrollback buffer. *)

val scroll_down : t -> int -> unit
(** [scroll_down t n] scrolls the grid content down by [n] lines. New blank
    lines are inserted at the top. Lines scrolled off the bottom are discarded.
*)

(** {1 Utilities} *)

val to_string_grid : t -> string
(** [to_string_grid t] returns a string representation of the visible grid
    content, stripping all styling information. Each line is trimmed of trailing
    whitespace. This is useful for debugging or creating plain-text snapshots.
*)
