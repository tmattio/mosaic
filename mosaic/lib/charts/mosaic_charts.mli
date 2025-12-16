(** Canvas-first charts for Mosaic terminal UIs.

    {1 Overview}

    Mosaic.charts provides a composable charting library optimized for terminal
    rendering. Charts are immutable specifications compiled into a {!Layout.t}
    for efficient drawing and interaction.

    Design principles:

    - Build an immutable {!t} by layering marks (lines, bars, scatter, heatmaps,
      etc.).
    - Keep viewport state in a separate {!View.t} (zoom and pan belong in your
      model).
    - Compute a {!Layout.t} for a given [view] and [width×height].
    - Draw using {!draw}, which returns the {!Layout.t} used.
    - Use {!Layout} + {!Hit} + {!Overlay} for hover tooltips, crosshairs,
      snapping, and zoom-to-cursor.

    The API is intentionally "compiled": interaction happens against a computed
    layout, not ad-hoc transforms you manually wire together.

    {1 Usage}

    Create a chart, add marks, and draw:
    {[
      let data = [| (0., 10.); (1., 20.); (2., 15.) |] in
      let chart =
        empty ()
        |> line ~x:fst ~y:snd data
        |> scatter ~x:fst ~y:snd data
      in
      let layout = draw chart canvas ~width:80 ~height:24 in
      (* Use layout for hit-testing or overlays *)
    ]}

    Zoom and pan with {!View.t}:
    {[
      let view =
        View.empty |> View.set_x (Some (View.window ~min:0. ~max:10.))
      in
      draw ~view chart canvas ~width:80 ~height:24
    ]}

    Add tooltips on hover (pattern match on payload variant):
    {[
      match Layout.hit_test layout ~px ~py with
      | Some { Hit.payload = Hit.XY { x; y }; _ } ->
          Overlay.tooltip layout canvas ~x ~y [ "Value: " ^ string_of_float y ]
      | Some { payload = Hit.Bar { category; value }; _ } ->
          Overlay.tooltip layout canvas ~x:0. ~y:value
            [ category ^ ": " ^ string_of_float value ]
      | Some { payload = Hit.Heat { x; y; value }; _ } ->
          Overlay.tooltip layout canvas ~x ~y
            [ "Heat: " ^ string_of_float value ]
      | _ -> ()
    ]} *)

module Theme : sig
  (** Color and style themes for charts.

      Themes control the visual appearance of chart elements. Each theme
      provides default styles for axes, grids, labels, and interactive overlays.
  *)

  type t = {
    palette : Ansi.Color.t list;  (** Colors for marks, cycled by index. *)
    background : Ansi.Color.t option;
        (** Chart background. [None] for terminal default. *)
    axes : Ansi.Style.t;  (** Axis lines and tick marks. *)
    grid : Ansi.Style.t;  (** Background grid lines. *)
    labels : Ansi.Style.t;  (** Axis tick labels. *)
    tooltip : Ansi.Style.t;  (** Tooltip text and background. *)
    tooltip_border : Ansi.Style.t option;
        (** Tooltip border. [None] for no border. *)
    crosshair : Ansi.Style.t;  (** Crosshair overlay lines. *)
    marker : Ansi.Style.t;  (** Point markers in overlays. *)
  }
  (** A complete theme specification.

      Themes are applied when creating a chart with {!empty} or modified with
      {!with_theme}. Individual marks can override theme colors by providing
      explicit [style] parameters. *)

  val dark : t
  (** [dark] is a dark terminal theme with muted grids and bright accents.

      Default palette includes cyan, magenta, yellow, green, blue, and red. *)

  val light : t
  (** [light] is a light terminal theme optimized for bright backgrounds.

      Uses darker axes and labels for contrast. *)

  val default : t
  (** [default] is an alias for {!dark}. *)
end

module Format : sig
  (** Number and date formatting utilities for axis labels.

      Formatters follow the signature [int -> float -> string], where the first
      argument is the tick index. *)

  val float : ?precision:int -> unit -> int -> float -> string
  (** [float ~precision ()] creates a float formatter.

      Uses general format ([g] specifier) for compact scientific notation when
      appropriate.

      @param precision
        Number of significant digits. Default is [3]. Valid range is [0] to [6];
        higher values are supported but may be verbose. *)

  val mmdd_utc : int -> float -> string
  (** [mmdd_utc _ v] formats [v] as UTC date in [MM/DD] format.

      Expects [v] as Unix timestamp (seconds since epoch). *)

  val hhmmss_utc : int -> float -> string
  (** [hhmmss_utc _ v] formats [v] as UTC time in [HH:MM:SS] format.

      Expects [v] as Unix timestamp. *)
end

module Scale : sig
  (** Axis scaling strategies for continuous and categorical data.

      Scales map data values to pixel coordinates. Use {!Numeric} for continuous
      axes (line, scatter) and {!Band} for categorical axes (bar charts). *)

  type numeric_domain = [ `Auto | `Domain of float * float ]
  (** Domain specification for numeric scales.

      - [`Auto]: Infer domain from mark data extents.
      - [`Domain (min, max)]: Use explicit bounds. If [min > max], they are
        swapped. *)

  (** Scale configuration.

      Applied separately to X and Y axes via {!with_x_scale} and
      {!with_y_scale}. *)
  type t =
    | Auto
        (** Infer scale type from marks. Chooses {!Band} if categorical marks
            exist, otherwise {!Numeric}. *)
    | Numeric of { domain : numeric_domain; clamp : bool }
        (** Continuous linear scale. [clamp] restricts view windows to domain
            bounds. *)
    | Band of { categories : string list option; padding : float }
        (** Categorical scale with evenly spaced bands. [padding] is the
            fraction of the axis reserved for gaps (0.0 to 1.0). *)

  val numeric : ?domain:numeric_domain -> ?clamp:bool -> unit -> t
  (** [numeric ~domain ~clamp ()] creates a numeric scale.

      @param domain Domain strategy. Default is [`Auto].
      @param clamp Restrict view windows to domain. Default is [true]. *)

  val band : ?categories:string list -> ?padding:float -> unit -> t
  (** [band ~categories ~padding ()] creates a categorical scale.

      @param categories
        Explicit category list. [None] infers categories from bar marks in order
        of appearance.
      @param padding
        Fraction of axis for inter-band gaps (0.0 to 1.0). Values outside this
        range are clamped. Default is [0.1]. *)
end

module Axis : sig
  (** Axis rendering configuration.

      Controls tick marks, labels, and formatting for X and Y axes. *)

  type formatter = int -> float -> string
  (** Formatter signature: [index -> value -> label].

      The [index] argument is the tick index (0-based), [value] is the data
      coordinate. *)

  type t = {
    show : bool;  (** Render axis and ticks. *)
    ticks : int;
        (** Target number of ticks. Actual count may vary for alignment. *)
    format : formatter;  (** Label formatter. *)
    style : Ansi.Style.t;  (** Style for axis line. *)
    tick_style : Ansi.Style.t;  (** Style for tick marks. *)
    label_style : Ansi.Style.t;  (** Style for tick labels. *)
    tick_length : int;  (** Tick mark length in cells. *)
    label_padding : int;  (** Spacing between ticks and labels in cells. *)
  }
  (** Axis configuration. *)

  val hidden : t
  (** [hidden] disables axis rendering.

      All fields are zeroed or set to defaults. *)

  val default : t
  (** [default] provides standard axis rendering.

      Enables axis with [6] ticks, default float formatter, [1]-cell tick marks,
      and [1]-cell label padding. *)

  val with_ticks : int -> t -> t
  (** [with_ticks n t] sets target tick count to [n].

      Negative [n] is clamped to [0]. *)

  val with_format : formatter -> t -> t
  (** [with_format f t] sets the label formatter to [f]. *)

  val with_style : Ansi.Style.t -> t -> t
  (** [with_style s t] sets axis line style to [s]. *)

  val with_tick_style : Ansi.Style.t -> t -> t
  (** [with_tick_style s t] sets tick mark style to [s]. *)

  val with_label_style : Ansi.Style.t -> t -> t
  (** [with_label_style s t] sets label style to [s]. *)

  val with_tick_length : int -> t -> t
  (** [with_tick_length len t] sets tick mark length to [len].

      Negative [len] is clamped to [0]. *)

  val with_label_padding : int -> t -> t
  (** [with_label_padding pad t] sets label padding to [pad].

      Negative [pad] is clamped to [0]. *)
end

module Grid : sig
  (** Background grid configuration.

      Grids render lines aligned to axis ticks or at fixed intervals. *)

  type t = {
    show : bool;  (** Enable grid rendering. *)
    x : bool;  (** Draw vertical grid lines. *)
    y : bool;  (** Draw horizontal grid lines. *)
    style : Ansi.Style.t;  (** Grid line style. *)
    x_step : int option;
        (** Fixed vertical line spacing in pixels. [None] aligns to X axis
            ticks. *)
    y_step : int option;
        (** Fixed horizontal line spacing in pixels. [None] aligns to Y axis
            ticks. *)
  }
  (** Grid configuration. *)

  val hidden : t
  (** [hidden] disables grid rendering. *)

  val default : t
  (** [default] enables grid with dimmed style, aligned to axis ticks.

      Both [x] and [y] are [true]; [x_step] and [y_step] are [None]. *)

  val with_style : Ansi.Style.t -> t -> t
  (** [with_style s t] sets grid line style to [s]. *)

  val with_x : bool -> t -> t
  (** [with_x b t] enables or disables vertical grid lines. *)

  val with_y : bool -> t -> t
  (** [with_y b t] enables or disables horizontal grid lines. *)

  val with_x_step : int option -> t -> t
  (** [with_x_step step t] sets vertical line spacing.

      [None] aligns to X axis ticks. [Some n] uses fixed [n]-pixel spacing. *)

  val with_y_step : int option -> t -> t
  (** [with_y_step step t] sets horizontal line spacing.

      [None] aligns to Y axis ticks. [Some n] uses fixed [n]-pixel spacing. *)
end

module View : sig
  (** Viewport windowing for zoom and pan.

      A {!t} restricts the visible data range on each axis. Views are
      independent of layout and belong in your application model. *)

  type window = { min : float; max : float }
  (** A data range on one axis.

      Invariant: [min] and [max] are automatically swapped if [min > max] during
      construction. *)

  type t = { x : window option; y : window option }
  (** Viewport specification.

      [None] uses the full data domain. [Some w] restricts the visible range to
      [w]. *)

  val empty : t
  (** [empty] shows the full data extent on both axes.

      Both [x] and [y] are [None]. *)

  val set_x : window option -> t -> t
  (** [set_x w t] sets the X-axis view window to [w]. *)

  val set_y : window option -> t -> t
  (** [set_y w t] sets the Y-axis view window to [w]. *)

  val window : min:float -> max:float -> window
  (** [window ~min ~max] creates a view window.

      If [min > max], they are swapped. The range is expanded slightly if [min]
      and [max] are nearly equal (within [1e-12]) to avoid degenerate domains.
  *)

  val zoom : window -> factor:float -> window
  (** [zoom w ~factor] scales [w] around its center.

      A [factor] greater than [1.0] zooms in (narrows the window). [factor] less
      than or equal to [0.0] is clamped to [1.0]. *)

  val zoom_around : window -> center:float -> factor:float -> window
  (** [zoom_around w ~center ~factor] scales [w] around [center].

      The ratio of distances from [center] to [min] and [max] is preserved.
      [factor] less than or equal to [0.0] is clamped to [1.0]. If
      [w.max - w.min] is nearly zero, [w] is returned unchanged. *)

  val pan : window -> delta:float -> window
  (** [pan w ~delta] shifts [w] by [delta].

      Both [min] and [max] are offset by [delta]. Positive [delta] pans right
      (or up for Y-axis). *)

  val clamp : domain:window -> window -> window
  (** [clamp ~domain w] restricts [w] to fit within [domain].

      If [w]'s range exceeds [domain], [w] is shrunk. If [w] is outside
      [domain], it is shifted to overlap maximally. *)
end

module Mark : sig
  (** Chart marks (visual encodings of data).

      Marks are graphical primitives like lines, scatter points, bars, and
      heatmaps. Each mark type operates on typed data via accessor functions. *)

  type id = string
  (** Optional identifier for marks.

      Used in {!Hit.t} to distinguish marks during interaction. *)

  type t
  (** An abstract mark.

      Marks are polymorphic over their data type, allowing type-safe accessors.
  *)

  type line_kind = [ `Line | `Braille | `Wave | `Points of string ]
  (** Line rendering style.

      - [`Line]: Standard cell-based line drawing.
      - [`Braille]: Higher resolution using Braille dot patterns.
      - [`Wave]: Sine wave approximation for smooth curves.
      - [`Points s]: Custom glyph [s] at each data point. *)

  type scatter_kind = [ `Cell | `Braille ]
  (** Scatter point rendering style.

      - [`Cell]: One glyph per cell.
      - [`Braille]: Higher resolution using Braille dots. *)

  type heatmap_agg = [ `Last | `Avg | `Max ]
  (** Aggregation strategy when multiple values map to the same cell.

      - [`Last]: Use the most recent value.
      - [`Avg]: Average all values.
      - [`Max]: Use the maximum value. *)

  type heatmap_render =
    | Cells
    | Dense_bilinear
    | Shaded
        (** Heatmap rendering mode.

            - [Cells]: Discrete colored cells.
            - [Dense_bilinear]: Bilinear interpolation for smooth gradients.
            - [Shaded]: Shaded block glyphs for intensity. *)

  type bar_segment = {
    value : float;  (** Segment height or width. *)
    style : Ansi.Style.t;  (** Segment color. *)
    label : string option;  (** Optional label for tooltip. *)
  }
  (** A segment within a stacked bar. *)

  type stacked_bar = { category : string; segments : bar_segment list }
  (** A stacked bar composed of multiple segments.

      Segments are rendered top-to-bottom or left-to-right. *)

  type ohlc = {
    time : float;  (** Time coordinate (X-axis). *)
    open_ : float;  (** Opening price. *)
    high : float;  (** High price. *)
    low : float;  (** Low price. *)
    close : float;  (** Closing price. *)
  }
  (** OHLC (Open-High-Low-Close) candlestick data. *)

  val line :
    ?id:id ->
    ?style:Ansi.Style.t ->
    ?kind:line_kind ->
    x:('a -> float) ->
    y:('a -> float) ->
    'a array ->
    t
  (** [line ~x ~y data] creates a line mark.

      Connects consecutive points with lines. Points are rendered in array
      order.

      @param id Optional identifier for hit-testing.
      @param style Line color. Defaults to theme palette.
      @param kind Rendering style. Default is [`Line]. *)

  val line_opt :
    ?id:id ->
    ?style:Ansi.Style.t ->
    ?kind:line_kind ->
    x:('a -> float) ->
    y:('a -> float option) ->
    'a array ->
    t
  (** [line_opt ~x ~y data] creates a line mark with optional Y values.

      Points where [y] returns [None] are skipped, creating gaps in the line. *)

  val scatter :
    ?id:id ->
    ?style:Ansi.Style.t ->
    ?glyph:string ->
    ?kind:scatter_kind ->
    x:('a -> float) ->
    y:('a -> float) ->
    'a array ->
    t
  (** [scatter ~x ~y data] creates a scatter mark.

      Plots individual points at each coordinate.

      @param glyph Character(s) to render at each point. Default is ["∙"].
      @param kind Rendering style. Default is [`Cell]. *)

  val bars_y :
    ?id:id ->
    ?style:Ansi.Style.t ->
    x:('a -> string) ->
    y:('a -> float) ->
    'a array ->
    t
  (** [bars_y ~x ~y data] creates vertical bars.

      Bars extend from [0] to [y] for each category [x]. Requires a
      {!Scale.Band} on the X-axis. *)

  val bars_x :
    ?id:id ->
    ?style:Ansi.Style.t ->
    y:('a -> string) ->
    x:('a -> float) ->
    'a array ->
    t
  (** [bars_x ~y ~x data] creates horizontal bars.

      Bars extend from [0] to [x] for each category [y]. Requires a
      {!Scale.Band} on the Y-axis. *)

  val stacked_bars_y :
    ?id:id -> ?gap:int -> ?bar_width:int -> stacked_bar array -> t
  (** [stacked_bars_y data] creates vertical stacked bars.

      Each bar is composed of segments stacked from bottom to top. Segment
      values are cumulative.

      @param gap
        Spacing between bars in pixels. Default is [1]. Negative values are
        clamped to [0].
      @param bar_width
        Explicit bar width in pixels. [None] auto-sizes based on band width. *)

  val stacked_bars_x :
    ?id:id -> ?gap:int -> ?bar_height:int -> stacked_bar array -> t
  (** [stacked_bars_x data] creates horizontal stacked bars.

      Each bar is composed of segments stacked from left to right.

      @param gap
        Spacing between bars in pixels. Default is [1]. Negative values are
        clamped to [0].
      @param bar_height
        Explicit bar height in pixels. [None] auto-sizes based on band height.
  *)

  val rule_y : ?id:id -> ?style:Ansi.Style.t -> float -> t
  (** [rule_y y] creates a horizontal rule at Y-coordinate [y].

      Draws a line spanning the full plot width. *)

  val rule_x : ?id:id -> ?style:Ansi.Style.t -> float -> t
  (** [rule_x x] creates a vertical rule at X-coordinate [x].

      Draws a line spanning the full plot height. *)

  val heatmap :
    ?id:id ->
    ?color_scale:Ansi.Color.t list ->
    ?value_range:float * float ->
    ?auto_value_range:bool ->
    ?agg:heatmap_agg ->
    ?render:heatmap_render ->
    x:('a -> float) ->
    y:('a -> float) ->
    value:('a -> float) ->
    'a array ->
    t
  (** [heatmap ~x ~y ~value data] creates a heatmap.

      Maps [value] to color intensity at each [(x, y)] coordinate.

      @param color_scale Color gradient. Default is theme palette.
      @param value_range
        Explicit [min, max] for color mapping. [None] infers from data.
      @param auto_value_range
        Adjust [value_range] dynamically. Default is [true].
      @param agg Aggregation for overlapping cells. Default is [`Last].
      @param render Rendering mode. Default is [Cells]. *)

  val candles :
    ?id:id -> ?bullish:Ansi.Style.t -> ?bearish:Ansi.Style.t -> ohlc array -> t
  (** [candles data] creates a candlestick chart.

      Renders OHLC data with wicks (high/low) and bodies (open/close). Body
      color indicates direction.

      @param bullish Style for up candles ([close > open]). Default is green.
      @param bearish Style for down candles ([close < open]). Default is red. *)

  val circle :
    ?id:id ->
    ?style:Ansi.Style.t ->
    ?kind:[ `Line | `Braille ] ->
    cx:('a -> float) ->
    cy:('a -> float) ->
    r:('a -> float) ->
    'a array ->
    t
  (** [circle ~cx ~cy ~r data] creates circle marks.

      Draws circles centered at [(cx, cy)] with radius [r] for each datum.

      @param kind Rendering style. Default is [`Line]. *)

  val shade_x :
    ?id:id -> ?style:Ansi.Style.t -> min:float -> max:float -> unit -> t
  (** [shade_x ~min ~max ()] creates a vertical shaded region.

      Fills the area between X-coordinates [min] and [max]. If [min > max], they
      are swapped. *)

  val column_background : ?id:id -> ?style:Ansi.Style.t -> float -> t
  (** [column_background x] creates a vertical background column at [x].

      Fills a single column spanning the plot height. Useful for highlighting
      time ranges. *)
end

module Hit : sig
  (** Hit-testing results for interactive charts.

      Hit-testing identifies data points near mouse or cursor positions. *)

  type policy = [ `Nearest_px | `Nearest_x | `Nearest_y ]
  (** Distance metric for hit-testing.

      - [`Nearest_px]: Euclidean distance in pixels.
      - [`Nearest_x]: Horizontal distance only (vertical ignored).
      - [`Nearest_y]: Vertical distance only (horizontal ignored). *)

  type kind =
    [ `Line | `Scatter | `Bars | `Stacked_bars | `Heatmap | `Candles | `Circle ]
  (** Mark type that was hit. *)

  (** Mark-specific data at the hit location. *)
  type payload =
    | XY of { x : float; y : float }
        (** Continuous data point (line, scatter, circle). *)
    | Bar of { category : string; value : float }  (** Categorical bar. *)
    | Stacked_bar of {
        category : string;
        segment_index : int;  (** Segment index within the stack (0-based). *)
        value : float;  (** This segment's value. *)
        total : float;  (** Sum of all segments in the bar. *)
      }  (** Segment within a stacked bar. *)
    | Heat of { x : float; y : float; value : float }  (** Heatmap cell. *)
    | OHLC of {
        time : float;
        open_ : float;
        high : float;
        low : float;
        close : float;
      }  (** Candlestick. *)

  type t = {
    mark_id : string option;  (** Mark identifier, if provided. *)
    kind : kind;  (** Type of mark. *)
    index : int;  (** Data index in the original array. *)
    px : int;  (** Pixel X-coordinate of the hit (snapped to data). *)
    py : int;  (** Pixel Y-coordinate of the hit (snapped to data). *)
    distance_px : float;  (** Distance from query point to hit, in pixels. *)
    payload : payload;  (** Mark-specific data. *)
  }
  (** A hit-testing result.

      Returned by {!Layout.hit_test} when a mark is within the search radius. *)
end

module Layout : sig
  (** Compiled chart layout for rendering and interaction.

      A {!t} captures the resolved dimensions, scales, and coordinate mappings
      for a given chart, view, and canvas size. Layouts are immutable and
      efficient to query. *)

  type t
  (** A compiled layout.

      Created by {!layout} or returned by {!draw}. Contains all information
      needed for coordinate transforms and hit-testing. *)

  type rect = { x : int; y : int; width : int; height : int }
  (** A rectangular region in pixel coordinates. *)

  val size : t -> int * int
  (** [size t] returns the full canvas dimensions [(width, height)]. *)

  val plot_rect : t -> rect
  (** [plot_rect t] returns the inner plot area, excluding axes and margins. *)

  val is_inside_plot : t -> px:int -> py:int -> bool
  (** [is_inside_plot t ~px ~py] checks if [(px, py)] is within the plot area.
  *)

  val x_domain : t -> View.window
  (** [x_domain t] returns the full X-axis data domain.

      This is the extent of all mark data, independent of the view window. *)

  val y_domain : t -> View.window
  (** [y_domain t] returns the full Y-axis data domain. *)

  val x_view : t -> View.window
  (** [x_view t] returns the effective X-axis view window used for rendering.

      If a view was provided, this is the clamped window; otherwise, it equals
      [x_domain t]. *)

  val y_view : t -> View.window
  (** [y_view t] returns the effective Y-axis view window. *)

  val data_of_px : t -> px:int -> py:int -> (float * float) option
  (** [data_of_px t ~px ~py] converts pixel coordinates to data coordinates.

      Returns [None] if [(px, py)] is outside the plot area. For numeric scales,
      returns continuous coordinates. For band scales, returns band centers. *)

  val px_of_data : t -> x:float -> y:float -> int * int
  (** [px_of_data t ~x ~y] converts data coordinates to pixel coordinates.

      Values outside the view window are clamped to the plot edges. *)

  val x_category_of_px : t -> px:int -> string option
  (** [x_category_of_px t ~px] returns the X-axis category at pixel [px].

      Only valid for band scales. Returns [None] if [px] is outside the plot
      area or the scale is numeric. *)

  val y_category_of_px : t -> py:int -> string option
  (** [y_category_of_px t ~py] returns the Y-axis category at pixel [py].

      Only valid for band scales. *)

  val px_of_x_category : t -> string -> int option
  (** [px_of_x_category t cat] returns the pixel X-coordinate of category
      [cat]'s center.

      Only valid for band scales. Returns [None] if [cat] is not in the scale.
  *)

  val py_of_y_category : t -> string -> int option
  (** [py_of_y_category t cat] returns the pixel Y-coordinate of category
      [cat]'s center. *)

  type axis = [ `X | `Y | `Both ]
  (** Axis selector for zoom and pan operations. *)

  val clamp_view : t -> View.t -> View.t
  (** [clamp_view t v] restricts [v] to valid domain bounds.

      For numeric scales, clamps windows to [x_domain] and [y_domain]. Band
      scales ignore clamping. *)

  val zoom_view_around_px :
    t -> view:View.t -> axis:axis -> px:int -> py:int -> factor:float -> View.t
  (** [zoom_view_around_px t ~view ~axis ~px ~py ~factor] zooms [view] around
      pixel [(px, py)].

      The data coordinate at [(px, py)] remains fixed. [factor] greater than
      [1.0] zooms in. Only numeric scales are zoomed; band scales remain
      unchanged.

      @param axis Which axes to zoom. [`Both] zooms both axes independently. *)

  val pan_view_by_px : t -> view:View.t -> dx:int -> dy:int -> View.t
  (** [pan_view_by_px t ~view ~dx ~dy] pans [view] by [dx, dy] pixels.

      Positive [dx] pans right; positive [dy] pans down. The resulting view is
      clamped to domain bounds. *)

  val plot_center_px : t -> int * int
  (** [plot_center_px t] returns the center of the plot area in pixel
      coordinates.

      Returns [(px, py)] at the center of {!plot_rect}. *)

  val zoom_view_around_center :
    t -> view:View.t -> axis:axis -> factor:float -> View.t
  (** [zoom_view_around_center t ~view ~axis ~factor] zooms [view] around the
      plot center.

      Convenience wrapper around {!zoom_view_around_px} that uses
      {!plot_center_px} as the zoom focus. Useful for keyboard-based zoom
      operations. *)

  val hit_test :
    ?radius:int -> ?policy:Hit.policy -> t -> px:int -> py:int -> Hit.t option
  (** [hit_test t ~px ~py] finds the nearest mark data point to [(px, py)].

      Returns [None] if no marks are within [radius] or [(px, py)] is outside
      the plot area.

      @param radius Search radius in pixels. Default is [3].
      @param policy Distance metric. Default is [`Nearest_px].

      Hit-testing scans all marks and selects the closest by [policy]. For
      overlapping hits, the last-added mark wins. *)
end

module Overlay : sig
  (** Interactive overlays for crosshairs, markers, and tooltips.

      Overlays render on top of charts to provide visual feedback during
      interaction. *)

  val crosshair :
    ?style:Ansi.Style.t ->
    Layout.t ->
    Mosaic_ui.Canvas.t ->
    x:float ->
    y:float ->
    unit
  (** [crosshair layout canvas ~x ~y] draws crosshair lines at data coordinates
      [(x, y)].

      Draws a vertical line at [x] and a horizontal line at [y], each spanning
      the full plot area. Lines are clipped to the plot rectangle.

      @param style Line style. Default is theme's [crosshair] style. *)

  val marker :
    ?style:Ansi.Style.t ->
    ?glyph:string ->
    Layout.t ->
    Mosaic_ui.Canvas.t ->
    x:float ->
    y:float ->
    unit
  (** [marker layout canvas ~x ~y] draws a marker glyph at data coordinates
      [(x, y)].

      The marker is only rendered if [(x, y)] maps to a pixel within the plot
      area.

      @param style Marker style. Default is theme's [marker] style.
      @param glyph Character(s) to render. Default is ["●"]. *)

  type tooltip_anchor = [ `Auto | `Left | `Right | `Top | `Bottom ]
  (** Tooltip positioning hint.

      - [`Auto]: Equivalent to [`Right].
      - [`Left]: Render tooltip to the left of the data point.
      - [`Right]: Render tooltip to the right of the data point.
      - [`Top]: Render tooltip above the data point.
      - [`Bottom]: Render tooltip below the data point.

      The tooltip is adjusted to stay within the plot area. *)

  val tooltip :
    ?style:Ansi.Style.t ->
    ?border:Ansi.Style.t ->
    ?padding:int ->
    ?anchor:tooltip_anchor ->
    Layout.t ->
    Mosaic_ui.Canvas.t ->
    x:float ->
    y:float ->
    string list ->
    unit
  (** [tooltip layout canvas ~x ~y lines] draws a tooltip box at data
      coordinates [(x, y)].

      The tooltip displays [lines] with optional border and padding. Lines are
      padded to equal width for consistent background fill. The box is
      positioned relative to [(x, y)] according to [anchor], then adjusted to
      fit within the plot area.

      @param style
        Text and background style. Default is theme's [tooltip] style.
      @param border
        Border style. Default is theme's [tooltip_border]. [None] renders no
        border.
      @param padding
        Internal padding in cells. Default is [1]. Negative values are clamped
        to [0].
      @param anchor Positioning hint. Default is [`Auto]. *)
end

module Legend : sig
  (** Chart legend rendering.

      Legends display mark labels with colored markers. *)

  type item = { label : string; style : Ansi.Style.t; marker : string }
  (** A legend entry.

      The [marker] is typically a colored glyph (e.g., ["■"] or ["●"]). *)

  val draw :
    ?direction:[ `Horizontal | `Vertical ] ->
    ?gap:int ->
    item list ->
    Mosaic_ui.Canvas.t ->
    width:int ->
    height:int ->
    unit
  (** [draw items canvas ~width ~height] renders a legend.

      Items are laid out in [direction] order, with spacing controlled by [gap].

      @param direction Layout direction. Default is [`Vertical].
      @param gap
        Spacing between items in cells. Default is [0]. For [`Horizontal],
        defaults to [2] if [0]. Negative values are clamped to [0]. *)
end

module Sparkline = Sparkline
(** Sparkline charts. See {!module-Sparkline} for details. *)

type t
(** An immutable chart specification.

    Charts are built by layering marks and configuration. Use {!empty} to start,
    then compose with {!with_theme}, {!with_x_scale}, {!add}, etc. *)

type frame = { margins : int * int * int * int; inner_padding : int }
(** Frame configuration for layout spacing.

    - [margins]: [(top, right, bottom, left)] in cells. Reserves space for axes.
    - [inner_padding]: Padding between plot area and frame in cells. *)

val default_frame : frame
(** [default_frame] has zero margins and padding.

    Axes and labels are rendered in the plot area without reserved space. *)

val empty : ?theme:Theme.t -> unit -> t
(** [empty ()] creates an empty chart.

    @param theme Chart theme. Default is {!Theme.default}.

    The chart uses {!Scale.Auto} for both axes, {!Axis.default} for rendering,
    and {!Grid.hidden} for no grid. *)

val with_theme : Theme.t -> t -> t
(** [with_theme theme t] applies [theme] to [t].

    Theme styles are merged with existing axis and grid configurations. Explicit
    styles on marks are preserved. *)

val with_frame : frame -> t -> t
(** [with_frame frame t] sets the layout frame to [frame].

    [inner_padding] is clamped to non-negative values. *)

val with_x_scale : Scale.t -> t -> t
(** [with_x_scale scale t] sets the X-axis scale to [scale]. *)

val with_y_scale : Scale.t -> t -> t
(** [with_y_scale scale t] sets the Y-axis scale to [scale]. *)

val with_axes : ?x:Axis.t -> ?y:Axis.t -> t -> t
(** [with_axes ~x ~y t] configures axis rendering.

    Theme defaults are applied to provided axes. [None] preserves existing axis
    configuration. *)

val with_grid : Grid.t -> t -> t
(** [with_grid grid t] sets the grid configuration to [grid].

    Theme defaults are applied to [grid]. *)

val add : Mark.t -> t -> t
(** [add mark t] appends [mark] to [t]'s mark list.

    Marks are rendered in the order added. Later marks draw on top of earlier
    ones. *)

val line :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  ?kind:Mark.line_kind ->
  x:('a -> float) ->
  y:('a -> float) ->
  'a array ->
  t ->
  t
(** [line ~x ~y data t] adds a line mark to [t].

    Convenience for [add (Mark.line ~x ~y data) t]. See {!Mark.line} for
    details. *)

val line_opt :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  ?kind:Mark.line_kind ->
  x:('a -> float) ->
  y:('a -> float option) ->
  'a array ->
  t ->
  t
(** [line_opt ~x ~y data t] adds a line mark with optional y values to [t].

    Points with [y a = None] create gaps in the line. Convenience for
    [add (Mark.line_opt ~x ~y data) t]. *)

val scatter :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  ?glyph:string ->
  ?kind:Mark.scatter_kind ->
  x:('a -> float) ->
  y:('a -> float) ->
  'a array ->
  t ->
  t
(** [scatter ~x ~y data t] adds a scatter mark to [t].

    Convenience for [add (Mark.scatter ~x ~y data) t]. *)

val bars_y :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  x:('a -> string) ->
  y:('a -> float) ->
  'a array ->
  t ->
  t
(** [bars_y ~x ~y data t] adds vertical bars to [t].

    Convenience for [add (Mark.bars_y ~x ~y data) t]. *)

val bars_x :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  y:('a -> string) ->
  x:('a -> float) ->
  'a array ->
  t ->
  t
(** [bars_x ~y ~x data t] adds horizontal bars to [t].

    Convenience for [add (Mark.bars_x ~y ~x data) t]. *)

val stacked_bars_y :
  ?id:Mark.id -> ?gap:int -> ?bar_width:int -> Mark.stacked_bar array -> t -> t
(** [stacked_bars_y data t] adds vertically stacked bars to [t].

    Convenience for [add (Mark.stacked_bars_y data) t]. *)

val stacked_bars_x :
  ?id:Mark.id -> ?gap:int -> ?bar_height:int -> Mark.stacked_bar array -> t -> t
(** [stacked_bars_x data t] adds horizontally stacked bars to [t].

    Convenience for [add (Mark.stacked_bars_x data) t]. *)

val rule_y : ?id:Mark.id -> ?style:Ansi.Style.t -> float -> t -> t
(** [rule_y y t] adds a horizontal rule at y position [y].

    Convenience for [add (Mark.rule_y y) t]. *)

val rule_x : ?id:Mark.id -> ?style:Ansi.Style.t -> float -> t -> t
(** [rule_x x t] adds a vertical rule at x position [x].

    Convenience for [add (Mark.rule_x x) t]. *)

val heatmap :
  ?id:Mark.id ->
  ?color_scale:Ansi.Color.t list ->
  ?value_range:float * float ->
  ?auto_value_range:bool ->
  ?agg:Mark.heatmap_agg ->
  ?render:Mark.heatmap_render ->
  x:('a -> float) ->
  y:('a -> float) ->
  value:('a -> float) ->
  'a array ->
  t ->
  t
(** [heatmap ~x ~y ~value data t] adds a heatmap to [t].

    Convenience for [add (Mark.heatmap ~x ~y ~value data) t]. *)

val candles :
  ?id:Mark.id ->
  ?bullish:Ansi.Style.t ->
  ?bearish:Ansi.Style.t ->
  Mark.ohlc array ->
  t ->
  t
(** [candles data t] adds candlesticks to [t].

    Convenience for [add (Mark.candles data) t]. *)

val circle :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  ?kind:[ `Line | `Braille ] ->
  cx:('a -> float) ->
  cy:('a -> float) ->
  r:('a -> float) ->
  'a array ->
  t ->
  t
(** [circle ~cx ~cy ~r data t] adds circle marks to [t].

    Convenience for [add (Mark.circle ~cx ~cy ~r data) t]. *)

val shade_x :
  ?id:Mark.id -> ?style:Ansi.Style.t -> min:float -> max:float -> unit -> t -> t
(** [shade_x ~min ~max () t] adds a vertical shaded region from [min] to [max].

    Convenience for [add (Mark.shade_x ~min ~max ()) t]. *)

val column_background : ?id:Mark.id -> ?style:Ansi.Style.t -> float -> t -> t
(** [column_background x t] adds a vertical background column at x position [x].

    Convenience for [add (Mark.column_background x) t]. *)

val layout : ?view:View.t -> t -> width:int -> height:int -> Layout.t
(** [layout t ~width ~height] compiles [t] into a {!Layout.t}.

    Resolves scales, infers domains from mark data, and computes coordinate
    mappings for the given canvas size.

    @param view Viewport restriction. [None] uses full data extent. *)

val draw :
  ?view:View.t -> t -> Mosaic_ui.Canvas.t -> width:int -> height:int -> Layout.t
(** [draw t canvas ~width ~height] renders [t] to [canvas] and returns the
    computed layout.

    First computes a {!Layout.t} via {!layout}, then draws axes, grid, and all
    marks. The returned layout can be used for hit-testing and overlays.

    @param view Viewport restriction. [None] uses full data extent. *)
