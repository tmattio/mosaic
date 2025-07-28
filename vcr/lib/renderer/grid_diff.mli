(** Cell-level diff for efficient terminal rendering *)

type dirty_region = {
  min_row : int;
  max_row : int;
  min_col : int;
  max_col : int;
}
(** A rectangular region that has changed *)

val find_dirty_rows :
  Vte.cell option array array -> Vte.cell option array array -> bool array
(** [find_dirty_rows prev_grid curr_grid] returns an array indicating which rows
    have changed between the previous and current grid states. *)

val compute_dirty_regions : bool array -> int -> dirty_region list
(** [compute_dirty_regions dirty_rows cols] converts an array of dirty row flags
    into a list of rectangular regions that need to be redrawn. *)

val diff :
  Vte.cell option array array ->
  Vte.cell option array array ->
  int ->
  bool array * (dirty_region * (int * int) list) list
(** [diff prev_grid curr_grid cols] computes the differences between two grid
    states. Returns:
    - An array of dirty row flags
    - A list of dirty regions with their changed cell coordinates *)

val compute_update_regions : (int * int) list -> dirty_region list
(** [compute_update_regions changed_cells] computes minimal bounding regions
    from a list of changed cell coordinates. *)
