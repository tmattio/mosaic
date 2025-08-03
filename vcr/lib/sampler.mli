(** Frame sampler - converts event log to frames at target FPS *)

type frame = {
  timestamp : float;  (** Time in simulation *)
  grid : Grid.t;  (** Terminal grid state at this time *)
  cursor_row : int;  (** Cursor row position *)
  cursor_col : int;  (** Cursor column position *)
  cursor_visible : bool;  (** Whether cursor should be shown *)
  dirty_regions : Grid.dirty_region list;
      (** Regions that changed since last frame *)
  cursor_moved : bool;  (** Whether cursor position changed *)
}
(** A single frame with timing information *)

(** Sample strategy for static screens *)
type strategy =
  | Drop  (** Drop frames with no changes, extend previous frame delay *)

val sample :
  fps:float ->
  strategy:strategy ->
  initial_cols:int ->
  initial_rows:int ->
  Event.log ->
  frame list
(** Sample frames from event log at given FPS
    @param fps Target frames per second
    @param strategy How to handle static screens
    @param initial_cols Initial terminal columns
    @param initial_rows Initial terminal rows
    @param events Event log to sample from
    @return List of frames with timestamps *)

val calculate_delays : frame list -> (frame * int) list
(** Calculate frame delays in centiseconds
    @param frames List of frames with timestamps
    @return List of (frame, delay_cs) pairs *)
