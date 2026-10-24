(** Sparklines, small charts of the most recent values of a series.

    A sparkline ({!t}) keeps the last [capacity] values pushed to it, each
    raised to at least 0, and draws as many of the most recent ones as fit its
    width, one per column, aligned right. It scales values so that its
    {e maximum} fills its height. With [auto_max] the maximum rises to each
    larger value pushed, and without it a larger value is cut at the top. A
    sparkline is mutable.

    This keeps the last minute of a value sampled each second, and draws the
    most recent 20 samples as bars 3 rows high.

    {[
    let cpu = Matrix_charts.Sparkline.create ~capacity:60 ()

    let on_sample grid v =
      Matrix_charts.Sparkline.push cpu v;
      Matrix_charts.Sparkline.draw cpu ~kind:`Bars grid ~width:20 ~height:3
    ]} *)

type t
(** The type for sparklines. *)

type kind = [ `Bars | `Braille ]
(** The type for how a sparkline is drawn. [`Bars] draws a bar per value, its
    top in eighths of a cell (▁ to █). [`Braille] draws a line through the
    values with 2 by 4 Braille dots per cell. *)

(** {1:create Constructors} *)

val create :
  ?style:Ansi.Style.t ->
  ?auto_max:bool ->
  ?max_value:float ->
  capacity:int ->
  unit ->
  t
(** [create ?style ?auto_max ?max_value ~capacity ()] is an empty sparkline that
    keeps [capacity] values, at least 1. It draws in [style],
    {!Ansi.Style.default} by default. [auto_max] defaults to [true]. [max_value]
    is the initial maximum, [1.] when absent or not positive. *)

(** {1:mutating Mutating} *)

val clear : t -> unit
(** [clear s] removes the values of [s] and keeps its capacity, style and
    maximum. *)

val push : t -> float -> unit
(** [push s v] adds [max 0. v] as the most recent value of [s], and drops the
    oldest value when [s] is full. With [auto_max], it raises the maximum to [v]
    when [v] is larger. *)

val push_all : t -> float list -> unit
(** [push_all s vs] is [List.iter (push s) vs]. *)

val set_max : t -> float -> unit
(** [set_max s m] sets the maximum of [s] to [m], or to [1.] when [m] is not
    positive. *)

(** {1:rendering Rendering} *)

val draw :
  t ->
  kind:kind ->
  ?columns_only:bool ->
  ?x:int ->
  ?y:int ->
  Matrix_grid.t ->
  width:int ->
  height:int ->
  unit
(** [draw s ~kind ?columns_only ?x ?y grid ~width ~height] draws [s] in the
    region of [width] by [height] cells, each at least 1, whose top-left cell is
    [(x, y)], [(0, 0)] by default. Unless [columns_only] holds, [false] by
    default, it first fills the region with the background of its style, if the
    style has one. *)

val draw_values :
  ?style:Ansi.Style.t ->
  kind:kind ->
  ?x:int ->
  ?y:int ->
  float list ->
  Matrix_grid.t ->
  width:int ->
  height:int ->
  unit
(** [draw_values ?style ~kind ?x ?y vs grid ~width ~height] is {!draw} of a
    sparkline in [style], {!Ansi.Style.default} by default, of capacity [width]
    and with [auto_max], to which [vs] were pushed. *)
