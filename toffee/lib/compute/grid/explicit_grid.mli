(* Helper functions for initialising GridTrack's from styles
   This mainly consists of evaluating GridAutoTracks *)

(* The auto-repeat fit strategy to use *)
type auto_repeat_strategy =
  | Max_repetitions_that_do_not_overflow
      (** If the grid container has a definite size or max size in the relevant
          axis:
          - then the number of repetitions is the largest possible positive
            integer that does not cause the grid to overflow the content box of
            its grid container. *)
  | Min_repetitions_that_do_overflow
      (** Otherwise, if the grid container has a definite min size in the
          relevant axis:
          - then the number of repetitions is the smallest possible positive
            integer that fulfills that minimum requirement *)

(* Compute the number of rows and columns in the explicit grid *)
val compute_explicit_grid_size_in_axis :
  style:Style.t ->
  auto_fit_container_size:float option ->
  auto_fit_strategy:auto_repeat_strategy ->
  resolve_calc_value:Style.calc_resolver ->
  axis:Geometry.absolute_axis ->
  int * int

(* Resolve the track sizing functions of explicit tracks, automatically created tracks, and gutters
   given a set of track counts and all of the relevant styles *)
val initialize_grid_tracks :
  tracks:Grid_track.t list ref ->
  counts:Grid_track_counts.t ->
  style:Style.t ->
  axis:Geometry.absolute_axis ->
  track_has_items:(int -> bool) ->
  unit
