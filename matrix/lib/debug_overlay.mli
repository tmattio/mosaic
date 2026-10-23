(** Debug overlay with runtime telemetry.

    Paints a semi-transparent box in a screen corner showing frame metrics from
    {!Matrix_screen.last_metrics}, the render times the runtime measured with
    rolling averages of them and of the interval between renders, and per-frame
    GC statistics from {!Gc.quick_stat}.

    Internal to the Matrix runtime: reachable through
    {!Matrix.set_debug_overlay} and {!Matrix.toggle_debug_overlay}, not exported
    as a standalone module. *)

(** {1:types Types} *)

type corner = [ `Top_left | `Top_right | `Bottom_left | `Bottom_right ]
(** The type for overlay anchor corners. *)

(** {1:rendering Rendering} *)

val on_frame :
  ?corner:corner ->
  ?padding:int ->
  ?gap:int ->
  ?capacity:int ->
  unit ->
  Matrix_screen.t ->
  render_start:int ->
  render_ns:int ->
  unit
(** [on_frame ()] is a callback that draws the overlay onto [screen]'s grid.
    Call once per frame after the UI has painted, with the start of the last
    render on the runtime's clock and its duration, in nanoseconds.

    The callback keeps internal state (rolling averages, GC deltas); reuse the
    same callback for the overlay's lifetime.
    - [corner] anchor position. Defaults to [`Bottom_right].
    - [padding] outer padding in cells.
    - [gap] gap between metric lines.
    - [capacity] maximum number of metric samples retained. *)
