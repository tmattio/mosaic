(** Low-level rendering primitives for terminal output.

    This module provides cell-based rendering with style support, efficient
    diffing, and Unicode handling. Manages a 2D buffer of styled cells and
    generates optimal ANSI sequences for terminal updates.

    Buffers maintain fixed dimensions after creation. Cells support combining
    characters and wide characters. Coordinates are zero-based from top-left.
    Out-of-bounds access is safe but ignored. Rendering preserves Unicode
    normalization and respects terminal width constraints. *)

(** {1 Cell Attributes} *)

type attr = {
  fg : Ansi.color option;
  bg : Ansi.color option;
  bold : bool;
  dim : bool;
  italic : bool;
  underline : bool;
  blink : bool;
  reverse : bool;
  strikethrough : bool;
  uri : string option;  (** Hyperlink URI *)
}
(** [attr] represents basic terminal text attributes. *)

val default_attr : attr
(** [default_attr] has no colors set and all flags false. *)

type cell = {
  chars : Uchar.t list;  (** Base character followed by combining characters *)
  attr : attr;
  width : int;  (** Display width (0 for continuation, 2 for wide chars) *)
}
(** [cell] represents a single terminal cell with content and style.

    Supports Unicode combining characters for accents and marks. Width indicates
    display columns: 1 for normal, 2 for wide (CJK), 0 for continuation cells.
    Empty cells have empty chars list and default style. *)

val empty_cell : cell
(** [empty_cell] is a blank cell with default style.

    Contains no characters, default style, width 1. Used for clearing and
    initialization. *)

(** {1 Buffer} *)

type buffer
(** [buffer] is a 2D grid of styled cells.

    Fixed-size after creation. Mutable for efficient updates. Coordinates are
    (x,y) with origin at top-left (0,0). *)

val create : int -> int -> buffer
(** [create width height] creates a new buffer filled with empty cells.

    Dimensions must be positive. All cells initialized to [empty_cell]. Memory
    allocated upfront for all cells.

    @raise Invalid_argument if width or height <= 0 *)

val clear : buffer -> unit
(** [clear buffer] resets all cells to empty state.

    Efficient batch operation. Preserves buffer dimensions. Equivalent to
    filling with [empty_cell]. *)

val dimensions : buffer -> int * int
(** [dimensions buffer] returns buffer size as [(width, height)].

    Dimensions are immutable after creation. Useful for boundary checking. *)

(** {2 Clipping Support} *)

module Clip : sig
  type t
  (** [t] represents a rectangular clipping region *)

  val make : int -> int -> int -> int -> t
  (** [make x y width height] creates a clipping rectangle.

      Coordinates are in buffer space. The rectangle is immutable. *)

  val x : t -> int
  (** [x rect] returns the x-coordinate of the rectangle's top-left corner *)

  val y : t -> int
  (** [y rect] returns the y-coordinate of the rectangle's top-left corner *)

  val width : t -> int
  (** [width rect] returns the width of the rectangle *)

  val height : t -> int
  (** [height rect] returns the height of the rectangle *)

  val intersect : t -> t -> t
  (** [intersect c1 c2] returns the intersection of two clipping rectangles. *)

  val intersect_opt : t option -> t option -> t option
  (** [intersect_opt c1 c2] returns the intersection of two optional clipping
      rectangles. Returns None if either is None. *)

  val contains : t -> int -> int -> bool
  (** [contains rect x y] checks if point [(x, y)] is inside the rectangle.
      Returns [true] if the point is inside, [false] otherwise. *)
end

val full_clip : buffer -> Clip.t
(** [full_clip buffer] returns a clip rect covering the entire buffer *)

(** {2 Cell Access} *)

val get : buffer -> int -> int -> cell
(** [get buffer x y] retrieves the cell at position [(x, y)].

    Returns [empty_cell] for out-of-bounds coordinates. Safe for any integer
    coordinates. O(1) access time. *)

val set : ?clip:Clip.t -> buffer -> int -> int -> cell -> unit
(** [set ?clip buffer x y cell] places [cell] at position [(x, y)].

    Out-of-bounds writes are ignored. If [clip] is provided, writes outside the
    clip rectangle are also ignored. Handles wide characters by setting
    continuation cells automatically. O(1) update time. *)

val set_char : ?clip:Clip.t -> buffer -> int -> int -> Uchar.t -> attr -> unit
(** [set_char ?clip buffer x y char style] places a styled character.

    Convenience for single character updates. Handles wide characters correctly.
    Sets continuation cells for 2-column characters. Out-of-bounds and
    out-of-clip writes are ignored. *)

val set_string : ?clip:Clip.t -> buffer -> int -> int -> string -> attr -> unit
(** [set_string ?clip buffer x y str style] writes styled UTF-8 string
    horizontally.

    Starts at [(x, y)] and continues rightward. Handles Unicode correctly.
    Clipped at buffer edge and clip rectangle if provided. Wide characters use 2
    columns. Newlines ignored.

    Example: Writes colored label.
    {[
      Render.set_string buffer 10 5 "Hello" (Style.fg Red)
    ]} *)

val set_multiline_string :
  ?clip:Clip.t -> buffer -> int -> int -> string -> attr -> int
(** [set_multiline_string ?clip buffer x y str style] writes multi-line string.

    Splits [str] by newlines and writes each line starting at [(x, y + i)].
    Returns the number of lines written. *)

(** {1 Rendering} *)

type cursor_pos = [ `Hide | `Move of int * int  (** (x, y) position *) ]
(** [cursor_pos] controls cursor visibility and position after rendering.

    `Hide makes cursor invisible during updates to prevent flicker. `Move
    positions cursor at specific coordinates for input fields. *)

type patch =
  | Change of { row : int; col : int; new_cell : cell }
  | Clear of { row : int; col : int; width : int; height : int }
      (** [patch] represents a change between buffer states.

          [Change] for updated cells, [Clear] for areas to erase (used for
          shrinking buffers). *)

val diff : buffer -> buffer -> patch list
(** [diff old new] computes minimal changes between buffers.

    Handles dimension changes: adds changes for new areas, clears for removed
    areas. Order is top-to-bottom, left-to-right for efficient rendering. *)

(** Rendering mode for different terminal contexts *)
type render_mode =
  | Absolute  (** Use absolute positioning (for alt-screen mode) *)
  | Relative
      (** Use relative positioning with aggressive style resets (for
          non-alt-screen mode) *)

val render_patches :
  ?cursor_pos:cursor_pos -> ?mode:render_mode -> patch list -> string
(** [render_patches ?cursor_pos ?mode patches] optimally renders multiple
    changes.

    Minimizes cursor movements. Handles clears efficiently. Coalesces style
    changes.

    @param cursor_pos Final cursor state (default: `Hide)
    @param mode Rendering mode (default: `Absolute) *)

val render_full :
  ?cursor_pos:cursor_pos -> ?mode:render_mode -> buffer -> string
(** [render_full ?cursor_pos ?mode buffer] renders complete buffer contents.

    Generates escape sequences for entire screen. Optimizes by coalescing
    styles.

    @param cursor_pos Final cursor state (default: `Hide)
    @param mode Rendering mode (default: `Absolute) *)

(** {1 Text Utilities} *)

val measure_string : string -> int
(** [measure_string s] calculates the display width of UTF-8 string [s].

    Handles wide characters (2 columns), combining marks (0 columns), and
    control characters. Essential for accurate terminal layout. ANSI escapes
    should be stripped first.

    Example: Measures CJK text width.
    {[
      let width = Render.measure_string "Hello 世界" (* 10 columns *)
    ]} *)

val truncate_string : string -> int -> string
(** [truncate_string s width] cuts string [s] to fit in [width] columns.

    Preserves valid UTF-8. Avoids splitting wide characters or combining marks.
    Returns shorter string if needed. Never exceeds target width. *)

val pad_string : string -> int -> string
(** [pad_string s width] extends string [s] with spaces to [width] columns.

    No effect if string already meets or exceeds width. Padding added at end.
    Handles wide characters correctly when calculating padding needed. *)

val expand_tabs : string -> int -> string
(** [expand_tabs s tab_width] returns a new string with all tab characters
    replaced by the appropriate number of spaces. *)

val unicode_substring : string -> int -> string
(** [unicode_substring str max_cells] returns a prefix of a string that fits
    within a given number of terminal cells, respecting Unicode character
    widths. *)

val truncate_string_with_ellipsis : string -> int -> string -> string
(** [truncate_string_with_ellipsis s max_width ellipsis] truncates with custom
    ellipsis.

    Ensures total width including ellipsis fits in [max_width]. If string fits,
    returns unchanged. If truncation needed, removes enough characters for
    ellipsis. Handles wide character ellipsis.

    Example: Truncates long path with ellipsis.
    {[
      let short =
        Render.truncate_string_with_ellipsis "/very/long/path/to/file.txt" 20
          "..."
      (* Result: "/very/long/pa..." *)
    ]} *)
