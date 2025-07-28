(** Grid management for the VTE module.

    This module provides operations for managing the character grid that
    represents the terminal display. *)

module Cell = Cell
(** @inline *)

type rect = { row : int; col : int; width : int; height : int }
(** A rectangular region in the grid *)

type t
(** The type of a grid with damage tracking *)

val create : rows:int -> cols:int -> t
(** [create ~rows ~cols] creates a new empty grid with the specified dimensions
*)

val rows : t -> int
(** [rows grid] returns the number of rows in the grid *)

val cols : t -> int
(** [cols grid] returns the number of columns in the grid *)

val get : t -> row:int -> col:int -> Cell.t option
(** [get grid ~row ~col] returns the cell at the specified position, or [None]
    if the position is out of bounds *)

val set : t -> row:int -> col:int -> Cell.t option -> unit
(** [set grid ~row ~col cell] sets the cell at the specified position. Does
    nothing if the position is out of bounds *)

val clear : t -> unit
(** [clear grid] clears all cells in the grid *)

val clear_line : t -> int -> int -> unit
(** [clear_line grid row from_col] clears all cells in the specified row
    starting from [from_col] to the end of the line *)

val clear_rect :
  t -> row_start:int -> row_end:int -> col_start:int -> col_end:int -> unit
(** [clear_rect grid ~row_start ~row_end ~col_start ~col_end] clears all cells
    in the specified rectangular region (inclusive) *)

val copy_row : t -> int -> Cell.t option array
(** [copy_row grid row] returns a copy of the specified row *)

val set_row : t -> int -> Cell.t option array -> unit
(** [set_row grid row new_row] replaces the specified row with a new row. The
    new row must have the same number of columns as the grid *)

val make_empty_row : cols:int -> Cell.t option array
(** [make_empty_row ~cols] creates a new empty row with the specified number of
    columns *)

val swap : t * t -> unit
(** [swap (grid1, grid2)] swaps the contents of two grids. Both grids must have
    the same dimensions *)

(** {2 Damage Tracking} *)

val flush_damage : t -> rect list * unit
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

val find_dirty_rows : t -> t -> bool array
(** [find_dirty_rows prev_grid curr_grid] returns an array indicating which rows
    have changed between the previous and current grid states. *)

val compute_dirty_regions : bool array -> int -> dirty_region list
(** [compute_dirty_regions dirty_rows cols] converts an array of dirty row flags
    into a list of rectangular regions that need to be redrawn. *)

val diff : t -> t -> bool array * (dirty_region * (int * int) list) list
(** [diff prev_grid curr_grid] computes the differences between two grid states.
    Returns:
    - An array of dirty row flags
    - A list of dirty regions with their changed cell coordinates *)

val compute_update_regions : (int * int) list -> dirty_region list
(** [compute_update_regions changed_cells] computes minimal bounding regions
    from a list of changed cell coordinates. *)
