(** A panel of a run's frame timing and memory, drawn in a corner of a frame.

    An overlay keeps the timing of the frames an application records
    ({!record}): the last frame's, and the averages over the last frames. It
    draws them in a corner of a frame's grid ({!draw}), with the metrics of the
    last frame the screen presented and the statistics of the garbage collector,
    and the memory allocated since it last drew. It reads no clock and asks the
    collector nothing itself: the application gives it the times and the
    statistics, and draws it in the frames it owns.

    An application times each frame it draws and draws the overlay over its
    view:
    {[
    let draw_frame overlay app view =
      let start = Matrix.clock () in
      Matrix.draw app (fun grid ->
          view grid;
          Matrix.Debug_overlay.draw overlay
            ~metrics:(Matrix.Screen.last_metrics (Matrix.screen app))
            ~gc:(Gc.quick_stat ()) grid);
      Matrix.Debug_overlay.record overlay ~start ~ns:(Matrix.clock () - start)
    ]} *)

type corner = [ `Top_left | `Top_right | `Bottom_left | `Bottom_right ]
(** The type for the corners of a grid. *)

type t
(** The type for overlays. *)

val create : ?corner:corner -> ?capacity:int -> unit -> t
(** [create ()] is an overlay with no frame recorded, drawn in [corner] (default
    [`Bottom_right]), whose averages are over the last [capacity] frames
    recorded (default [120]). With a [capacity] of [0] or less it shows no
    average. *)

val record : ?app:int -> t -> start:int -> ns:int -> unit
(** [record o ~start ~ns] records a frame drawn from the time [start] for [ns]
    nanoseconds. The time between two recorded frames' [start]s is the frame
    interval [o] shows, from which it shows the frame rate. [app], when given,
    is the nanoseconds the application spent since the frame before outside
    drawing, which [o] shows in its [app] row. *)

val draw :
  t ->
  metrics:Matrix_screen.frame_metrics ->
  gc:Gc.stat ->
  Matrix_grid.t ->
  unit
(** [draw o ~metrics ~gc grid] draws [o]'s panel in its corner of [grid]: the
    last frame recorded and the averages, the screen's [metrics] and the
    collector's [gc], with what the collector allocated since the last [draw] of
    [o]. *)
