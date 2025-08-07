(** Draw charts on the terminal with Mosaic.

    This module provides functions to create various types of charts as
    [Ui.element] values, which can then be composed into a larger user
    interface.

    Example Usage:
    {[
      let line_data =
        [ { x = 0.0; y = 1.0 }; { x = 1.0; y = 3.0 }; { x = 2.0; y = 2.0 } ]
      in
      let my_chart =
        Mosaic_charts.line
          ~series_styles:[ Style.fg Style.red ]
          [ ("Series A", line_data) ]
      in

      let ui = Ui.vbox [ Ui.text "My Awesome Chart"; Ui.panel my_chart ] in

      Ui.print ui
    ]} *)

(** {1 Common Data Types} *)

type point = { x : float; y : float }
(** A single data point in a 2D Cartesian coordinate system. *)

type time_series_point = { time : float; value : float }
(** A data point for time series charts, where [time] is a Unix timestamp. *)

type ohlc_point = {
  time : float;
  open_ : float;
  high : float;
  low : float;
  close : float;
}
(** A data point for Open-High-Low-Close (candlestick) charts. *)

type bar_segment = { value : float; style : Ui.Style.t; label : string option }
(** A single segment within a stacked bar. *)

type bar = { label : string; segments : bar_segment list }
(** A single bar, which can be composed of multiple segments for stacking. *)

type heat_point = { x : float; y : float; value : float }
(** A single data point for a heatmap, with a value for color mapping. *)

(** {1 Chart Elements} *)

(** The rendering style for line-based charts. *)
type line_render_kind =
  | Lines  (** Render using box-drawing characters. *)
  | Braille  (** Render using high-resolution braille patterns. *)
  | Points of string  (** Render as a scatter plot with the given character. *)

type sparkline_render_kind = [ `Bars | `Line | `Braille ]
(** The rendering style for sparkline charts. *)

val line :
  ?width:Ui.dimension ->
  ?height:Ui.dimension ->
  ?x_range:float * float ->
  ?y_range:float * float ->
  ?show_axes:bool ->
  ?axis_style:Ui.Style.t ->
  ?label_style:Ui.Style.t ->
  ?series_styles:Ui.Style.t list ->
  ?show_grid:bool ->
  ?grid_style:Ui.Style.t ->
  ?render_kind:line_render_kind ->
  (string * point list) list ->
  Ui.element
(** [line data] creates a line chart.

    @param x_range
      Manually specifies the X-axis bounds. If [None], bounds are calculated
      from the data.
    @param y_range
      Manually specifies the Y-axis bounds. If [None], bounds are calculated
      from the data.
    @param show_axes If [true] (default), draws X and Y axes with labels.
    @param series_styles A list of styles to cycle through for each data series.
    @param render_kind The method used to draw the lines (default: [Lines]).
    @param data
      A list of named data series. Each series is a tuple of
      [(name, point list)]. *)

val time_series :
  ?width:Ui.dimension ->
  ?height:Ui.dimension ->
  ?time_range:float * float ->
  ?y_range:float * float ->
  ?show_axes:bool ->
  ?axis_style:Ui.Style.t ->
  ?label_style:Ui.Style.t ->
  ?x_label_format:string ->
  ?series_styles:Ui.Style.t list ->
  ?render_kind:line_render_kind ->
  (string * time_series_point list) list ->
  Ui.element
(** [time_series data] creates a line chart with a time-based X-axis.

    @param time_range Manually specifies the time bounds as Unix timestamps.
    @param y_range Manually specifies the Y-axis value bounds.
    @param x_label_format
      The format string for time labels on the X-axis (see [strftime]). Defaults
      to a sensible date/time format.
    @param data A list of named time series. *)

val bar :
  ?width:Ui.dimension ->
  ?height:Ui.dimension ->
  ?orientation:[ `Vertical | `Horizontal ] ->
  ?max_value:float ->
  ?min_value:float ->
  ?bar_width:int ->
  ?gap:int ->
  ?show_axes:bool ->
  ?axis_style:Ui.Style.t ->
  ?label_style:Ui.Style.t ->
  bar list ->
  Ui.element
(** [bar data] creates a bar chart. Can be stacked by providing multiple
    segments per bar.

    @param orientation The direction of the bars (default: [`Vertical]).
    @param max_value
      The value corresponding to the full height/width of the chart. If [None],
      it's calculated from the data.
    @param bar_width The width of each bar in characters (for vertical charts).
    @param gap The space between bars in characters. *)

val sparkline :
  ?width:int ->
  ?range:float * float ->
  ?style:Ui.Style.t ->
  ?render_kind:sparkline_render_kind ->
  float list ->
  Ui.element
(** [sparkline data] creates a compact, inline chart without axes or labels. It
    renders within a single line of text height.

    @param width The total width of the sparkline in characters.
    @param range
      The min/max values for scaling. If [None], calculated from data.
    @param style The style for the chart elements.
    @param render_kind The method used to draw the chart (default: [`Bars]). *)

val heatmap :
  ?width:Ui.dimension ->
  ?height:Ui.dimension ->
  ?x_range:float * float ->
  ?y_range:float * float ->
  ?value_range:float * float ->
  ?color_scale:Ui.Style.color list ->
  ?show_axes:bool ->
  ?axis_style:Ui.Style.t ->
  ?label_style:Ui.Style.t ->
  heat_point list ->
  Ui.element
(** [heatmap data] creates a chart where data values are mapped to a color
    gradient on a 2D grid.

    @param value_range
      The min/max values for the color mapping. If [None], calculated from data.
    @param color_scale
      A list of colors representing the gradient from min to max value. Defaults
      to a greyscale gradient. *)

val candlestick :
  ?width:Ui.dimension ->
  ?height:Ui.dimension ->
  ?time_range:float * float ->
  ?y_range:float * float ->
  ?show_axes:bool ->
  ?axis_style:Ui.Style.t ->
  ?label_style:Ui.Style.t ->
  ?bullish_style:Ui.Style.t ->
  ?bearish_style:Ui.Style.t ->
  ohlc_point list ->
  Ui.element
(** [candlestick data] creates an OHLC/candlestick chart for financial data.

    @param bullish_style
      Style for candles where [close > open] (default: green).
    @param bearish_style Style for candles where [close < open] (default: red).
*)

(** {2 Helper Functions} *)

val legend : (string * Ui.Style.t) list -> Ui.element
(** [legend items] creates a horizontal legend from a list of (label, style) pairs.
    The style is used to color a marker (line or box) before each label. *)

val line_with_gaps :
  ?width:Ui.dimension ->
  ?height:Ui.dimension ->
  ?x_range:float * float ->
  ?y_range:float * float ->
  ?show_axes:bool ->
  ?axis_style:Ui.Style.t ->
  ?label_style:Ui.Style.t ->
  ?series_styles:Ui.Style.t list ->
  ?show_grid:bool ->
  ?grid_style:Ui.Style.t ->
  ?render_kind:line_render_kind ->
  (string * (float * float option) list) list ->
  Ui.element
(** [line_with_gaps data] creates a line chart that supports data gaps.
    Each point is a pair of (x, y option) where None represents a missing value. *)
