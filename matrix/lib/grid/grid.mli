(** Grid management for the VTE module.

    This module provides operations for managing the character grid that
    represents the terminal display. *)

module Cell = Cell
(** @inline *)

type rect = { row : int; col : int; width : int; height : int }
(** A rectangular region in the grid *)

type t
(** The type of a grid with damage tracking *)

val create : rows:int -> cols:int -> ?east_asian_context:bool -> unit -> t
(** [create ~rows ~cols ?east_asian_context ()] creates a new empty grid with
    the specified dimensions.
    @param east_asian_context
      If true, ambiguous width characters are treated as width 2. Defaults to
      false. *)

val rows : t -> int
(** [rows grid] returns the number of rows in the grid *)

val cols : t -> int
(** [cols grid] returns the number of columns in the grid *)

val get : t -> row:int -> col:int -> Cell.t option
(** [get grid ~row ~col] returns the cell at the specified position, or [None]
    if the position is out of bounds *)

val set : t -> row:int -> col:int -> Cell.t option -> unit
(** [set grid ~row ~col cell] sets the cell at the specified position. If cell
    is None, sets the cell to Empty. Does nothing if the position is out of
    bounds *)

val set_grapheme :
  ?link:string ->
  ?east_asian_context:bool ->
  t ->
  row:int ->
  col:int ->
  glyph:string ->
  attrs:Ansi.Style.t ->
  unit
(** [set_grapheme grid ~row ~col ~glyph ~attrs ?link ?east_asian_context] sets a
    grapheme cluster at the specified position, automatically handling wide
    characters by setting continuation cells. If the style contains RGBA colors,
    they will be automatically blended with the existing cell colors using alpha
    compositing before being written as opaque RGB colors.
    @param link Optional hyperlink URL to associate with this cell
    @param east_asian_context
      If provided, overrides the grid's default setting. If true, ambiguous
      width characters are treated as width 2 Does nothing if the position is
      out of bounds. *)

val set_text :
  ?link:string ->
  ?east_asian_context:bool ->
  ?max_width:int ->
  t ->
  row:int ->
  col:int ->
  text:string ->
  attrs:Ansi.Style.t ->
  int
(** [set_text grid ~row ~col ~text ~attrs ?link ?east_asian_context ?max_width]
    sets text at the specified position, automatically handling grapheme
    segmentation and wide characters. Returns the number of columns advanced.
    @param text UTF-8 encoded text (may contain multiple graphemes)
    @param link Optional hyperlink URL to associate with all cells of this text
    @param east_asian_context
      If provided, overrides the grid's default setting. If true, ambiguous
      width characters are treated as width 2
    @param max_width
      Maximum number of columns to write (text will be truncated at grapheme
      boundaries)
    @return The total width of all graphemes written *)

val clear : ?style:Ansi.Style.t -> t -> unit
(** [clear ?style grid] clears all cells in the grid. If [style] is provided,
    cells are set to empty but with the given style (background color).
    Otherwise, cells are set to empty with default style. Using a style is
    useful for initializing a grid with a specific background color for proper
    alpha blending. *)

val fill_space : ?style:Ansi.Style.t -> t -> unit
(** [fill_space ?style grid] fills all cells in the grid with space characters.
    If [style] is provided, spaces are set with the given style. Otherwise,
    spaces are set with default style. This is useful for initializing a buffer
    where unwritten areas should be spaces rather than empty cells, ensuring
    proper diff behavior for UI transitions. *)

val clear_line : t -> int -> int -> unit
(** [clear_line grid row from_col] clears all cells in the specified row
    starting from [from_col] to the end of the line *)

val clear_rect :
  t -> row_start:int -> row_end:int -> col_start:int -> col_end:int -> unit
(** [clear_rect grid ~row_start ~row_end ~col_start ~col_end] clears all cells
    in the specified rectangular region (inclusive) *)

val copy_row : t -> int -> Cell.t array
(** [copy_row grid row] returns a copy of the specified row, or an empty array
    if the row is out of bounds *)

val set_row : t -> int -> Cell.t array -> unit
(** [set_row grid row new_row] replaces the specified row with a new row. The
    new row must have the same number of columns as the grid *)

val make_empty_row : cols:int -> Cell.t array
(** [make_empty_row ~cols] creates a new empty row with the specified number of
    columns, filled with Empty cells *)

val swap : t * t -> unit
(** [swap (grid1, grid2)] swaps the contents of two grids. Both grids must have
    the same dimensions *)

val resize : t -> rows:int -> cols:int -> unit
(** [resize grid ~rows ~cols] resizes the grid to the new dimensions. Content is
    preserved as much as possible. If the new size is smaller, content is
    truncated. If larger, new cells are empty. *)

val blit : src:t -> src_rect:rect -> dst:t -> dst_pos:int * int -> unit
(** [blit ~src ~src_rect ~dst ~dst_pos] efficiently copies a rectangular region
    from the source grid to the destination grid at the specified position. The
    operation handles boundary conditions and clipping automatically. Only the
    affected destination rows are marked as dirty and have their hashes updated.
*)

val with_updates : t -> (t -> unit) -> unit
(** [with_updates grid f] executes function [f] with row hash updates deferred.
    This is useful for batch operations where multiple cells are modified. Row
    hashes are recalculated only once at the end for all dirty rows,
    significantly improving performance for bulk updates. *)

(** {2 Damage Tracking} *)

val flush_damage : t -> rect list
(** [flush_damage grid] returns the list of dirty rectangles since the last
    flush and clears the damage tracking. The second component is unit. *)

val to_string : t -> string
(** [to_string grid] returns a string representation of the grid content,
    stripping all styling information. Each line is trimmed of trailing
    whitespace *)

(** {2 Grid Diffing} *)

type dirty_region = {
  min_row : int;
  max_row : int;
  min_col : int;
  max_col : int;
}
(** A rectangular region that has changed *)

val diff_rows : t -> t -> int list
(** [diff_rows prev curr] returns a list of row indices that have changed.
    Useful for line-based terminal updates. *)

val diff_regions : t -> t -> dirty_region list
(** [diff_regions prev curr] returns a list of rectangular regions that changed.
    Useful for optimized screen redraws. *)

val diff_cells : t -> t -> (int * int) list
(** [diff_cells prev curr] returns a list of (row, col) coordinates for cells
    that changed. Useful for precise cell-level updates. *)

val diff_regions_detailed : t -> t -> (dirty_region * (int * int) list) list
(** [diff_regions_detailed prev curr] returns regions with the specific cells
    that changed within each region. Useful when you need both region bounds and
    exact cells. *)

val compute_dirty_regions : bool array -> int -> dirty_region list
(** [compute_dirty_regions dirty_rows cols] converts an array of dirty row flags
    into a list of rectangular regions that need to be redrawn. *)

val compute_update_regions : (int * int) list -> dirty_region list
(** [compute_update_regions changed_cells] computes minimal bounding regions
    from a list of changed cell coordinates. *)

val diff : t -> t -> dirty_region list
(** [diff prev curr] performs a complete diff operation between two grids,
    combining row hash comparison and dirty region computation into a single
    efficient operation. This is the recommended way to find changes between
    grids as it performs all necessary steps internally. *)

(** {2 Utilities} *)

val copy : t -> t
(** [copy grid] creates a deep copy of the grid with all cells duplicated. The
    returned grid is independent from the original. *)

val char_width : ?east_asian:bool -> Uchar.t -> int
(** Calculate the display width of a single Unicode character.

    @param east_asian
      If true, ambiguous width characters are treated as width 2. Defaults to
      false.
    @param uchar The Unicode character to measure
    @return
      The display width in terminal columns (-1 for control, 0-2 for others) *)

val string_width : ?east_asian:bool -> string -> int
(** Calculate the display width of a string using proper grapheme segmentation.

    This handles complex emoji sequences, ZWJ sequences, variation selectors,
    and multi-codepoint grapheme clusters according to Unicode standards.

    @param east_asian
      If true, ambiguous width characters are treated as width 2. Defaults to
      false.
    @param s The UTF-8 encoded string
    @return The total display width in terminal columns *)

(** {2 Pretty-printing} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt grid] pretty-prints the grid structure for debugging. Shows
    dimensions and non-empty cells with their content. *)

val pp_rect : Format.formatter -> rect -> unit
(** [pp_rect fmt rect] pretty-prints a rectangular region. *)

val pp_dirty_region : Format.formatter -> dirty_region -> unit
(** [pp_dirty_region fmt region] pretty-prints a dirty region. *)

(** {2 Equality} *)

val equal_rect : rect -> rect -> bool
(** [equal_rect r1 r2] returns true if the two rectangles are equal. *)

val equal_dirty_region : dirty_region -> dirty_region -> bool
(** [equal_dirty_region r1 r2] returns true if the two dirty regions are equal.
*)
