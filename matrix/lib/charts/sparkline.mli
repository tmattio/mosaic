(** Sparkline charts for compact time series visualization.

    Sparklines render recent data points in a fixed-width buffer using either
    bar glyphs or Braille dots. The rendering adapts to available grid
    dimensions.

    {1 Usage}

    Create a sparkline with a fixed capacity, push values incrementally, and
    draw to a grid:
    {[
      let sp = Sparkline.create ~capacity:50 () in
      Sparkline.push sp 42.5;
      Sparkline.draw sp ~kind:`Bars grid ~width:50 ~height:5
    ]}

    For one-off rendering without state:
    {[
      Sparkline.draw_values ~kind:`Braille [ 1.; 2.; 3.; 5.; 8. ] grid ~width:10
        ~height:3
    ]} *)

type t
(** A sparkline with a circular buffer for recent values.

    The buffer has fixed capacity and automatically evicts old values when full.
*)

type kind = [ `Bars | `Braille ]
(** Rendering style.

    - [`Bars]: Uses block glyphs (▁▂▃▄▅▆▇█) for vertical bars, supporting
      fractional heights.
    - [`Braille]: Uses Braille patterns for higher resolution line rendering. *)

val create :
  ?style:Ansi.Style.t ->
  ?auto_max:bool ->
  ?max_value:float ->
  capacity:int ->
  unit ->
  t
(** [create ~capacity ()] creates a sparkline buffer.

    The buffer holds up to [capacity] values in a circular queue. When full, the
    oldest value is replaced by new pushes.

    @param style Rendering style for glyphs. Default is {!Ansi.Style.default}.
    @param auto_max
      When [true], [max_value] adapts dynamically as larger values are pushed.
      Default is [true].
    @param max_value
      Initial maximum for scaling. Values exceeding this are clipped unless
      [auto_max] is [true]. Defaults to [1.0] if [None] or negative.
    @param capacity
      Buffer size. Must be at least [1]; values less than [1] are clamped to
      [1].

    Invariant: Negative values are clamped to [0.0] on push. *)

val clear : t -> unit
(** [clear t] empties the buffer.

    The capacity and configuration remain unchanged. Subsequent draws render
    nothing until new values are pushed. *)

val push : t -> float -> unit
(** [push t v] appends [v] to the buffer.

    If the buffer is full, the oldest value is evicted. Negative [v] is clamped
    to [0.0]. If [auto_max] is [true] and [v] exceeds [max_value], [max_value]
    is updated to [v]. *)

val push_all : t -> float list -> unit
(** [push_all t vs] pushes each value in [vs] sequentially. Equivalent to
    [List.iter (push t) vs]. *)

val set_max : t -> float -> unit
(** [set_max t m] sets the scaling maximum to [m].

    If [m] is less than or equal to [0.0], it is set to [1.0]. This overrides
    [auto_max] temporarily until a larger value is pushed (if [auto_max] is
    [true]). *)

val draw :
  t ->
  kind:kind ->
  ?columns_only:bool ->
  ?x:int ->
  ?y:int ->
  Grid.t ->
  width:int ->
  height:int ->
  unit
(** [draw t ~kind grid ~width ~height] renders the sparkline to [grid].

    Only the most recent [width] values are shown, right-aligned. Values are
    scaled by [max_value]; bars fill from bottom upward.

    @param kind Rendering style. See {!type-kind}.
    @param columns_only
      When [true], skips background fill (style's [bg] is ignored). Default is
      [false].
    @param x Horizontal offset in [grid]. Default is [0].
    @param y Vertical offset in [grid]. Default is [0].
    @param width
      Grid width. Determines how many recent values are visible. Clamped to at
      least [1].
    @param height
      Grid height. Bar heights scale proportionally. Clamped to at least [1].

    [`Bars] uses block glyphs for vertical bars. [`Braille] draws connected line
    segments using 2x4 Braille dot grids per cell, employing Bresenham's line
    algorithm for anti-aliased appearance. *)

val draw_values :
  ?style:Ansi.Style.t ->
  kind:kind ->
  ?x:int ->
  ?y:int ->
  float list ->
  Grid.t ->
  width:int ->
  height:int ->
  unit
(** [draw_values ~kind vs grid ~width ~height] renders [vs] directly without
    state.

    Creates a temporary sparkline with [capacity] equal to [width], pushes all
    [vs], then draws. Useful for one-off rendering.

    @param style Rendering style. Default is {!Ansi.Style.default}.
    @param kind Rendering style. See {!type-kind}.
    @param x Horizontal offset in [grid]. Default is [0].
    @param y Vertical offset in [grid]. Default is [0].

    {4 Example}

    Render a quick trend:
    {[
      Sparkline.draw_values ~kind:`Bars [ 10.; 20.; 15.; 25. ] grid ~width:10
        ~height:3
    ]} *)
