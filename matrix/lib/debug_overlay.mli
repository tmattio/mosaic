(** Debug overlay with built-in runtime telemetry.

    The overlay paints a translucent box in any corner of the screen, reports
    the most recent frame metrics from {!Screen.last_metrics}, keeps rolling
    averages for frame time and interval, and samples GC statistics on every
    frame using {!Gc.quick_stat}.

    It is wired into the Matrix runtime (see {!Matrix.set_debug_overlay}) but
    remains available as a standalone callback for custom render pipelines. *)

type corner = [ `Top_left | `Top_right | `Bottom_left | `Bottom_right ]
(** Screen corners that can host the overlay. *)

val on_frame :
  ?corner:corner ->
  ?padding:int ->
  ?gap:int ->
  ?capacity:int ->
  unit ->
  Screen.t ->
  unit
(** [on_frame ?corner ?padding ?gap ?capacity () screen] draws the overlay onto
    [screen]'s grid. Call it once per frame after your UI has painted. The
    function keeps internal state across frames (rolling averages and GC delta
    tracking), so reuse the returned callback for the overlay's lifetime. *)
