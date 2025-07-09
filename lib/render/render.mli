(** Rendering and buffering for terminal output *)

(** {1 Cells and Styles} *)

type style = {
  fg : Ansi.color option;
  bg : Ansi.color option;
  bold : bool;
  dim : bool;
  italic : bool;
  underline : bool;
  double_underline : bool;
  blink : bool;
  reverse : bool;
  strikethrough : bool;
  overline : bool;
  uri : string option;  (** Hyperlink URI *)
}

val default_style : style

type cell = {
  chars : Uchar.t list;  (** Base character followed by combining characters *)
  style : style;
  width : int;  (** Display width (0 for continuation, 2 for wide chars) *)
}

val empty_cell : cell

(** {1 Buffer} *)

type buffer
(** A 2D buffer of cells *)

val create : int -> int -> buffer
(** [create width height] creates a new buffer *)

val clear : buffer -> unit
(** Clear the buffer to empty cells *)

val dimensions : buffer -> int * int
(** Get buffer dimensions (width, height) *)

val get : buffer -> int -> int -> cell
(** [get buffer x y] gets the cell at position (x, y) *)

val set : buffer -> int -> int -> cell -> unit
(** [set buffer x y cell] sets the cell at position (x, y) *)

val set_char : buffer -> int -> int -> Uchar.t -> style -> unit
(** [set_char buffer x y char style] sets a character with style *)

val set_string : buffer -> int -> int -> string -> style -> unit
(** [set_string buffer x y str style] writes a string starting at (x, y) *)

(** {1 Rendering} *)

type cursor_pos = [ `Hide | `Move of int * int  (** (x, y) position *) ]
(** Cursor position control for rendering functions *)

type patch = { row : int; col : int; old_cell : cell; new_cell : cell }

val diff : buffer -> buffer -> patch list
(** [diff old_buffer new_buffer] computes the minimal set of changes required to
    transform [old_buffer] into [new_buffer]. The resulting list of patches can
    be passed to [render_patches] to produce an optimized sequence of terminal
    commands for an efficient, flicker-free update.

    @raise Invalid_argument if the buffer dimensions do not match. *)

val render_patch : patch -> string
(** [render_patch patch] generates ANSI escape sequences for a single patch.
    Note: This function creates a new string for each patch and is less
    efficient than [render_patches] for multiple patches. It should primarily be
    used for debugging or when rendering a single patch. *)

val render_patches : ?cursor_pos:cursor_pos -> patch list -> string
(** [render_patches ?cursor_pos patches] optimally renders a list of patches. If
    [cursor_pos] is provided:
    - [`Hide] will hide the cursor
    - [`Move (x, y)] will position the cursor at (x, y) and show it *)

val render_full : ?cursor_pos:cursor_pos -> buffer -> string
(** [render_full ?cursor_pos buffer] renders the entire buffer. If [cursor_pos]
    is provided:
    - [`Hide] will hide the cursor
    - [`Move (x, y)] will position the cursor at (x, y) and show it *)

(** {1 Utilities} *)

val measure_string : string -> int
(** [measure_string s] returns the display width of a UTF-8 string *)

val truncate_string : string -> int -> string
(** [truncate_string s width] truncates string to fit within width *)

val pad_string : string -> int -> string
(** [pad_string s width] pads string with spaces to reach the specified width *)

val truncate_string_with_ellipsis : string -> int -> string -> string
(** [truncate_string_with_ellipsis s max_width ellipsis] truncates string and
    appends ellipsis if it exceeds max_width. The ellipsis itself counts towards
    the max_width. *)
