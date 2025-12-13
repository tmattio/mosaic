(** Performance optimization for grid size estimation.

    This module estimates grid dimensions before auto-placement to pre-size
    vectors and reduce allocations. It is not required for CSS Grid spec
    compliance but improves performance by analyzing child placements to predict
    implicit grid bounds. *)

val compute_grid_size_estimate :
  explicit_col_count:int ->
  explicit_row_count:int ->
  child_styles_iter:Style.t Seq.t ->
  Grid_track_counts.t * Grid_track_counts.t
(** [compute_grid_size_estimate ~explicit_col_count ~explicit_row_count
     ~child_styles_iter] estimates grid size before auto-placement.

    Returns [(column_counts, row_counts)] with exact counts for explicit and
    negative implicit tracks. The positive implicit track count is a lower bound
    since auto-placement can expand the grid unpredictably.

    Algorithm:

    + Scan all children to find minimum/maximum origin-zero grid lines and
      maximum span per axis
    + Compute negative implicit tracks from minimum lines (exact)
    + Compute positive implicit tracks from maximum lines (lower bound)
    + Adjust positive tracks if any span exceeds total track count

    The function mixes grid track numbers and grid line numbers internally for
    efficiency. *)
