(** Grid diffing functionality for optimized rendering *)

type region = { min_row : int; max_row : int; min_col : int; max_col : int }
(** A rectangular region defined by its bounds *)

val empty_region : region
(** An empty region with no cells *)

val is_empty_region : region -> bool
(** Check if a region is empty *)

val update_region : region -> int -> int -> region
(** [update_region r row col] expands region [r] to include cell at [row, col]
*)

val cell_equal : Vte.Cell.t option -> Vte.Cell.t option -> bool
(** [cell_equal c1 c2] returns true if cells are visually equivalent *)

val diff_grids :
  prev:Vte.Cell.t option array array ->
  curr:Vte.Cell.t option array array ->
  rows:int ->
  cols:int ->
  region
(** [diff_grids ~prev ~curr ~rows ~cols] returns the region containing all
    changed cells *)
