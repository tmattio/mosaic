(** Minimal, canvas-first charting API for Mosaic.

    Build a plot by layering marks, then obtain a draw function to pass as the
    [~draw] callback of a [Mosaic_ui.canvas] node. *)

module Plot : sig
  type t
  type line_kind = [ `Line | `Braille | `Wave | `Points of string ]
  type heatmap_agg = [ `Last | `Avg | `Max ]
  type scatter_kind = [ `Cell | `Braille ]
  type margins = { top : int; right : int; bottom : int; left : int }

  (** {1 Coordinate Transforms}

      Types for converting between screen coordinates (pixels/cells) and data
      coordinates. Returned by {!draw} for use in interactive charts. *)

  type plot_rect = { x : int; y : int; width : int; height : int }
  (** Plot area bounds in screen coordinates, excluding margins and axes. *)

  type transforms = {
    px_to_data : int -> int -> (float * float) option;
        (** [px_to_data px py] converts screen coordinates to data coordinates.
            Returns [None] if the point is outside the plot area. *)
    data_to_px : float -> float -> int * int;
        (** [data_to_px x y] converts data coordinates to screen coordinates. *)
    plot_rect : plot_rect;
        (** The plot area bounds for hit testing. *)
  }

  val make : ?margins:margins -> ?axes:bool -> ?grid:bool -> unit -> t
  (** [make ?margins ?axes ?grid ()] creates an empty plot.

      Defaults:
      - [margins]: [{ top = 0; right = 0; bottom = 1; left = 2 }]
      - [axes]: [true]
      - [grid]: [false] *)

  val palette : Ansi.Color.t list -> t -> t
  (** [palette colors plot] sets the series color palette used when a series
      does not specify an explicit style. *)

  val x_domain : float * float -> t -> t
  (** [x_domain (min,max) plot] sets numeric X domain, disabling auto-infer. *)

  val y_domain : float * float -> t -> t
  (** [y_domain (min,max) plot] sets numeric Y domain, disabling auto-infer. *)

  val x_view : float * float -> t -> t
  (** [x_view (min,max) plot] sets the X viewing window used for scaling. When
      set, scaling uses this range instead of the full domain. *)

  val y_view : float * float -> t -> t
  (** [y_view (min,max) plot] sets the Y viewing window used for scaling. When
      set, scaling uses this range instead of the full domain. *)

  val x_band : ?padding:float -> string list -> t -> t
  (** [x_band ?padding cats plot] uses a band (categorical) scale on X.
      [padding] is 0.0..1.0, default 0.1. *)

  val y_band : ?padding:float -> string list -> t -> t
  (** [y_band ?padding cats plot] uses a band (categorical) scale on Y. *)

  val axes :
    ?style:Ansi.Style.t ->
    ?x_ticks:int ->
    ?y_ticks:int ->
    ?x_label:(int -> float -> string) ->
    ?y_label:(int -> float -> string) ->
    t ->
    t
  (** [axes ?style ?x_ticks ?y_ticks ?x_label ?y_label plot] enables axes and
      optionally configures tick density and label formatters for each axis. If
      unspecified, defaults keep current behavior. *)

  val grid :
    ?style:Ansi.Style.t ->
    ?x:bool ->
    ?y:bool ->
    ?x_step:int ->
    ?y_step:int ->
    t ->
    t
  (** [grid ?style ?x ?y ?x_step ?y_step plot] toggles/sets grid lines; defaults
      to both axes. Optionally control the grid step (in cells) independently
      for X/Y. *)

  val line :
    ?style:Ansi.Style.t ->
    ?kind:line_kind ->
    x:('a -> float) ->
    y:('a -> float) ->
    'a list ->
    t ->
    t
  (** [line ?style ?kind ~x ~y data plot] adds a line/scatter series. *)

  val line_opt :
    ?style:Ansi.Style.t ->
    ?kind:line_kind ->
    x:('a -> float) ->
    y:('a -> float option) ->
    'a list ->
    t ->
    t
  (** [line_opt] treats [None] as a gap (discontinuous segments). *)

  val scatter :
    ?style:Ansi.Style.t ->
    ?glyph:string ->
    ?kind:scatter_kind ->
    x:('a -> float) ->
    y:('a -> float) ->
    'a list ->
    t ->
    t
  (** [scatter ?style ?glyph ?kind ~x ~y data] adds a point-only series.
      [kind=`Cell] (default) draws 1-cell glyphs; [kind=`Braille] uses a braille
      grid (2x horizontal, 4x vertical) for higher precision. *)

  val bar_y :
    ?style:Ansi.Style.t ->
    x:('a -> string) ->
    y:('a -> float) ->
    'a list ->
    t ->
    t
  (** [bar_y ?style ~x ~y data] vertical bars: categories on X, values on Y. *)

  val bar_x :
    ?style:Ansi.Style.t ->
    y:('a -> string) ->
    x:('a -> float) ->
    'a list ->
    t ->
    t
  (** [bar_x ?style ~y ~x data] horizontal bars: categories on Y, values on X.
  *)

  val bars_y_stacked :
    ?gap:int ->
    ?bar_width:int ->
    (string * (float * Ansi.Style.t) list) list ->
    t ->
    t
  (** [bars_y_stacked ?gap ?bar_width data] vertical stacked bars. *)

  val bars_x_stacked :
    ?gap:int ->
    ?bar_width:int ->
    (string * (float * Ansi.Style.t) list) list ->
    t ->
    t
  (** [bars_x_stacked] horizontal stacked bars. *)

  val rule_y : ?style:Ansi.Style.t -> float -> t -> t
  (** [rule_y ?style v plot] adds a horizontal reference line at Y = [v]. *)

  val rule_x : ?style:Ansi.Style.t -> float -> t -> t
  (** [rule_x ?style v plot] adds a vertical reference line at X = [v]. *)

  val heatmap :
    ?color_scale:Ansi.Color.t list ->
    ?value_range:float * float ->
    ?auto_value_range:bool ->
    ?shaded:bool ->
    ?agg:heatmap_agg ->
    x:('a -> float) ->
    y:('a -> float) ->
    value:('a -> float) ->
    'a list ->
    t ->
    t
  (** [heatmap] draws value intensity as shaded cells, colored by scale. *)

  val circle :
    ?style:Ansi.Style.t ->
    ?kind:[ `Line | `Braille ] ->
    cx:('a -> float) ->
    cy:('a -> float) ->
    r:('a -> float) ->
    'a list ->
    t ->
    t
  (** [circle ?style ?kind ~cx ~cy ~r data] draws circles for each datum. *)

  val candles :
    ?bullish:Ansi.Style.t ->
    ?bearish:Ansi.Style.t ->
    time:('a -> float) ->
    open_:('a -> float) ->
    high:('a -> float) ->
    low:('a -> float) ->
    close:('a -> float) ->
    'a list ->
    t ->
    t
  (** [candles] adds OHLC candlesticks. *)

  val shade_x : ?style:Ansi.Style.t -> min:float -> max:float -> t -> t
  (** [shade_x ?style ~min ~max plot] shades the vertical region between X =
      [min] and X = [max] in plot coordinates. Draw it before series to appear
      behind lines/bars. *)

  val column_background : ?style:Ansi.Style.t -> float -> t -> t
  (** [column_background ?style x plot] shades the vertical column at X = [x].
  *)

  val draw : t -> Mosaic_ui.Canvas.t -> width:int -> height:int -> transforms
  (** [draw plot canvas ~width ~height] draws the plot and returns coordinate
      transforms for interactive features. Use the returned {!transforms} to
      convert mouse positions to data coordinates. *)

  val draw_into :
    t -> canvas:Mosaic_ui.Canvas.t -> width:int -> height:int -> transforms
  (** [draw_into plot ~canvas ~width ~height] draws the plot immediately and
      returns coordinate transforms. *)

  (** {1 Interactive Overlays}

      Drawing helpers for interactive chart features like tooltips and
      crosshairs. These take {!transforms} returned by {!draw}. *)

  val draw_crosshair :
    ?style:Ansi.Style.t ->
    transforms ->
    Mosaic_ui.Canvas.t ->
    x:float ->
    y:float ->
    unit
  (** [draw_crosshair ?style transforms canvas ~x ~y] draws horizontal and
      vertical lines intersecting at the data point [(x, y)]. *)

  val draw_marker :
    ?style:Ansi.Style.t ->
    ?glyph:string ->
    transforms ->
    Mosaic_ui.Canvas.t ->
    x:float ->
    y:float ->
    unit
  (** [draw_marker ?style ?glyph transforms canvas ~x ~y] draws a marker glyph
      at the data point [(x, y)]. Default glyph is ["●"]. *)

  val draw_tooltip :
    ?style:Ansi.Style.t ->
    ?anchor:[ `Left | `Right ] ->
    transforms ->
    Mosaic_ui.Canvas.t ->
    x:float ->
    y:float ->
    string list ->
    unit
  (** [draw_tooltip ?style ?anchor transforms canvas ~x ~y lines] draws a
      tooltip with the given [lines] near the data point [(x, y)]. The tooltip
      automatically repositions to stay within the plot bounds. *)

  (** {1 Viewport Helpers}

      Pure functions for manipulating viewport domains. Use these with
      {!x_view} and {!y_view} to implement zoom and pan. *)

  val zoom : float * float -> factor:float -> float * float
  (** [zoom domain ~factor] scales the domain around its center.
      [factor > 1.0] zooms in (smaller range), [factor < 1.0] zooms out. *)

  val zoom_around : float * float -> center:float -> factor:float -> float * float
  (** [zoom_around domain ~center ~factor] scales the domain around a specific
      data coordinate. Useful for zoom-to-cursor when you have the data
      coordinates from {!transforms.px_to_data}. *)

  val pan : float * float -> delta:float -> float * float
  (** [pan domain ~delta] shifts the domain by [delta] in data units.
      Positive delta shifts right/up, negative shifts left/down. *)

  val bounds : 'a list -> f:('a -> float) -> float * float
  (** [bounds data ~f] computes the min/max range of [f] applied to [data].
      Returns [(0., 1.)] for empty lists. Useful for computing auto-fit
      domains. *)
end

type spark_kind = [ `Bars | `Line | `Braille ]

val sparkline :
  ?style:Ansi.Style.t ->
  ?kind:spark_kind ->
  float list ->
  Mosaic_ui.Canvas.t ->
  width:int ->
  height:int ->
  unit
(** [sparkline ?style ?kind values] returns a compact draw function suitable for
    a single-row canvas. *)

module Sparkline : sig
  type t
  (** Model storing sparkline state and rendering options. *)

  val make :
    width:int ->
    height:int ->
    ?style:Ansi.Style.t ->
    ?auto_max:bool ->
    ?max_value:float ->
    ?data:float list ->
    unit ->
    t
  (** [make ~width ~height ?style ?auto_max ?max_value ?data ()] creates a
      sparkline model with a fixed-capacity ring buffer sized to [width]. Values
      are clamped to [0.0] on push. When [auto_max] is [true] (default), pushing
      a value greater than the current [max_value] updates scaling. *)

  val clear : t -> unit
  (** [clear t] clears the internal buffer. The expected maximum is preserved.
  *)

  val push : t -> float -> unit
  (** [push t v] appends a value to the buffer (clamped to [0.0]). When capacity
      is reached, the oldest value is discarded. *)

  val push_all : t -> float list -> unit
  (** [push_all t vs] appends all values in order. *)

  val set_max : t -> float -> unit
  (** [set_max t m] sets the expected maximum value used for scaling. *)

  val resize : t -> width:int -> height:int -> unit
  (** [resize t ~width ~height] resizes capacity and drawing height, preserving
      the most recent values up to the new [width]. *)

  val draw :
    t ->
    kind:[ `Bars | `Braille ] ->
    ?columns_only:bool ->
    Mosaic_ui.Canvas.t ->
    width:int ->
    height:int ->
    unit
  (** [draw t ~kind ?columns_only canvas ~width ~height] draws the model into
      [canvas] aligned to the right edge, scaling values into [height]. When
      [columns_only] is [true], only the columns/line are styled; when [false]
      (default), the canvas background is filled if the style has a background
      color. *)
end

val line :
  ?style:Ansi.Style.t ->
  (float * float) list ->
  Mosaic_ui.Canvas.t ->
  width:int ->
  height:int ->
  unit
(** [line ?style points] one-off line chart draw function for quick usage. *)

type time_series_point = { time : float; value : float }

val time_series :
  ?styles:Ansi.Style.t list ->
  (string * time_series_point list) list ->
  Mosaic_ui.Canvas.t ->
  width:int ->
  height:int ->
  unit

val line_with_gaps :
  ?styles:Ansi.Style.t list ->
  (string * (float * float option) list) list ->
  Mosaic_ui.Canvas.t ->
  width:int ->
  height:int ->
  unit

type bar_segment = {
  value : float;
  style : Ansi.Style.t;
  label : string option;
}

type bar = { label : string; segments : bar_segment list }

val bar :
  ?orientation:[ `Vertical | `Horizontal ] ->
  ?gap:int ->
  ?bar_width:int ->
  ?min_value:float ->
  ?max_value:float ->
  ?show_axis:bool ->
  ?axis_style:Ansi.Style.t ->
  ?label_style:Ansi.Style.t ->
  bar list ->
  Mosaic_ui.Canvas.t ->
  width:int ->
  height:int ->
  unit

type ohlc_point = {
  time : float;
  open_ : float;
  high : float;
  low : float;
  close : float;
}

val candlestick :
  ?bullish:Ansi.Style.t ->
  ?bearish:Ansi.Style.t ->
  ohlc_point list ->
  Mosaic_ui.Canvas.t ->
  width:int ->
  height:int ->
  unit

type heat_point = { x : float; y : float; value : float }

val heatmap :
  ?color_scale:Ansi.Color.t list ->
  ?value_range:float * float ->
  ?auto_value_range:bool ->
  ?shaded:bool ->
  ?agg:Plot.heatmap_agg ->
  heat_point list ->
  Mosaic_ui.Canvas.t ->
  width:int ->
  height:int ->
  unit

val legend :
  (string * Ansi.Style.t) list ->
  Mosaic_ui.Canvas.t ->
  width:int ->
  height:int ->
  unit

val stacked_legend :
  (string * Ansi.Style.t) list ->
  Mosaic_ui.Canvas.t ->
  width:int ->
  height:int ->
  unit

(* Convenience axis label formatters for time-based X axes (UTC). *)
val x_label_mmdd : int -> float -> string
val x_label_hhmmss : int -> float -> string

(* Global heatmap defaults *)
val heatmap_default_scale : unit -> Ansi.Color.t list
val set_heatmap_default_scale : Ansi.Color.t list -> unit
