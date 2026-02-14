(** Terminal charts for Matrix.

    {1 Overview}

    Matrix_charts provides a composable charting library optimized for terminal
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
      let layout = draw chart grid ~width:80 ~height:24 in
      (* Use layout for hit-testing or overlays *)
    ]}

    Zoom and pan with {!View.t}:
    {[
      let view =
        View.empty |> View.set_x (Some (View.window ~min:0. ~max:10.))
      in
      draw ~view chart grid ~width:80 ~height:24
    ]}

    Add tooltips on hover (pattern match on payload variant):
    {[
      match Layout.hit_test layout ~px ~py with
      | Some { Hit.payload = Hit.XY { x; y }; _ } ->
          Overlay.tooltip layout grid ~x ~y [ "Value: " ^ string_of_float y ]
      | Some { payload = Hit.Bar { category; value }; _ } ->
          Overlay.tooltip layout grid ~x:0. ~y:value
            [ category ^ ": " ^ string_of_float value ]
      | Some { payload = Hit.Heat { x; y; value }; _ } ->
          Overlay.tooltip layout grid ~x ~y [ "Heat: " ^ string_of_float value ]
      | _ -> ()
    ]}

    {1 Coordinate Systems}

    The library uses two distinct coordinate systems:

    {2 Cell coordinates ([px], [py])}

    Cell coordinates identify terminal cells (columns and rows). These are
    integer coordinates used throughout the {!Layout} module for hit-testing,
    panning, and position queries.

    - [px]: Horizontal position in terminal columns (0 = left edge).
    - [py]: Vertical position in terminal rows (0 = top edge).
    - Cell [(0, 0)] is the top-left corner of the chart area.

    Cell coordinates map directly to the {!Grid.t} positions. When handling
    mouse input, use terminal cursor coordinates as [px]/[py].

    {2 Data coordinates ([x], [y])}

    Data coordinates are floating-point values in your data's domain. These are
    used when specifying mark positions, view windows, and overlay anchors.

    - [x]: Horizontal position in data units (e.g., time, category index).
    - [y]: Vertical position in data units (e.g., price, count).

    Convert between coordinate systems using:
    - {!Layout.data_of_px}: cell → data
    - {!Layout.px_of_data}: data → cell

    {2 Sub-cell rendering}

    Some marks support sub-cell resolution (e.g., [`Braille2x4] renders at 2×4
    dots per cell). Sub-cell rendering is purely visual — all coordinates in the
    API remain in cell units. Hit-testing snaps to the nearest cell, not to
    individual braille dots or block quadrants.

    Example: A braille-rendered line at data point [(5.0, 10.0)] might render
    dots at sub-cell positions, but {!Layout.hit_test} returns the enclosing
    cell's [px]/[py] and the exact data coordinates in {!type-Hit.payload}. *)

module Charset : sig
  (** Character sets for chart rendering.

      Charsets define the glyphs used for borders, axes, grids, and marks.
      Different charsets provide varying levels of visual fidelity depending on
      terminal capabilities. *)

  type line_pattern = [ `Solid | `Dashed | `Dotted ]
  (** Line rendering pattern.

      - [`Solid]: Continuous line (e.g., "─").
      - [`Dashed]: Dashed line (e.g., "╌" or "┄").
      - [`Dotted]: Dotted line (e.g., "┈" or "·"). *)

  type frame = {
    tl : string;  (** Top-left corner. *)
    tr : string;  (** Top-right corner. *)
    bl : string;  (** Bottom-left corner. *)
    br : string;  (** Bottom-right corner. *)
    h : string;  (** Horizontal line. *)
    v : string;  (** Vertical line. *)
    tee_up : string;  (** T-junction pointing up (┴). *)
    tee_down : string;  (** T-junction pointing down (┬). *)
    tee_left : string;  (** T-junction pointing left (┤). *)
    tee_right : string;  (** T-junction pointing right (├). *)
    cross : string;  (** Four-way intersection (┼). *)
  }
  (** Frame characters for borders and boxes. *)

  type t = {
    frame : frame;  (** Border and box drawing characters. *)
    axis_h : string;  (** Horizontal axis line. *)
    axis_v : string;  (** Vertical axis line. *)
    tick_h : string;  (** Horizontal tick mark on vertical axis. *)
    tick_v : string;  (** Vertical tick mark on horizontal axis. *)
    grid_h_solid : string;  (** Horizontal grid line (solid). *)
    grid_v_solid : string;  (** Vertical grid line (solid). *)
    grid_h_dashed : string;  (** Horizontal grid line (dashed). *)
    grid_v_dashed : string;  (** Vertical grid line (dashed). *)
    grid_h_dotted : string;  (** Horizontal grid line (dotted). *)
    grid_v_dotted : string;  (** Vertical grid line (dotted). *)
    point_default : string;  (** Default scatter point glyph (e.g., "∙"). *)
    point_heavy : string;  (** Heavy point marker (e.g., "●"). *)
    bar_fill : string;  (** Bar fill character (e.g., "█"). *)
    shade_levels : string array;
        (** Shade gradient for density rendering (e.g.,
            [|" "; "░"; "▒"; "▓"; "█"|]). *)
    tooltip_frame : frame;  (** Frame characters for tooltips. *)
    diag_up : string;  (** Diagonal line going up (e.g., "╱" or "/"). *)
    diag_down : string;  (** Diagonal line going down (e.g., "╲" or "\\"). *)
  }
  (** A complete character set specification.

      Charsets are applied via {!Theme.t} to control all glyph rendering. *)

  val ascii : t
  (** [ascii] uses only ASCII characters for maximum compatibility.

      Suitable for terminals without Unicode support. Uses characters like [+],
      [-], [|], [#] for borders and fills. *)

  val unicode_light : t
  (** [unicode_light] uses light Unicode box-drawing characters.

      Uses thin lines (─│┌┐└┘┼) for a clean, minimal appearance. *)

  val unicode_heavy : t
  (** [unicode_heavy] uses heavy Unicode box-drawing characters.

      Uses thick lines (━┃┏┓┗┛╋) for bold, prominent borders. *)

  val unicode_rounded : t
  (** [unicode_rounded] uses rounded Unicode box-drawing characters.

      Uses rounded corners (╭╮╰╯) with light lines for a softer appearance. *)

  val default : t
  (** [default] is an alias for {!unicode_light}. *)
end

module Theme : sig
  (** Color and style themes for charts.

      Themes control the visual appearance of chart elements. Each theme
      provides default styles for axes, grids, labels, and interactive overlays.
  *)

  type t = {
    palette : Ansi.Color.t array;
        (** Colors for marks, cycled by index. O(1) access. *)
    background : Ansi.Color.t option;
        (** Chart background. [None] for terminal default. *)
    axes : Ansi.Style.t;  (** Axis lines and tick marks. *)
    border : Ansi.Style.t;  (** Frame and bounding box style. *)
    grid : Ansi.Style.t;  (** Background grid lines. *)
    grid_minor : Ansi.Style.t;  (** Minor grid lines (dimmer than major). *)
    labels : Ansi.Style.t;  (** Axis tick labels. *)
    tooltip : Ansi.Style.t;  (** Tooltip text and background. *)
    tooltip_border : Ansi.Style.t option;
        (** Tooltip border. [None] for no border. *)
    crosshair : Ansi.Style.t;  (** Crosshair overlay lines. *)
    marker : Ansi.Style.t;  (** Point markers in overlays. *)
    charset : Charset.t;  (** Character set for rendering. *)
  }
  (** A complete theme specification.

      Themes are applied when creating a chart with {!empty} or modified with
      {!with_theme}. Individual marks can override theme colors by providing
      explicit [style] parameters. *)

  val dark : t
  (** [dark] is a dark terminal theme with muted grids and bright accents.

      Default palette includes cyan, magenta, yellow, green, blue, and red. Uses
      {!Charset.unicode_light} for glyphs. *)

  val light : t
  (** [light] is a light terminal theme optimized for bright backgrounds.

      Uses darker axes and labels for contrast. Uses {!Charset.unicode_light}
      for glyphs. *)

  val default : t
  (** [default] is an alias for {!dark}. *)

  val with_charset : Charset.t -> t -> t
  (** [with_charset charset t] returns [t] with the given [charset]. *)
end

module Label_format : sig
  (** Number and date formatting utilities for axis labels.

      Renamed from [Format] to avoid confusion with [Stdlib.Format].

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

module Transform : sig
  (** Data smoothing and transformation utilities.

      These functions transform [(x, y)] data arrays, preserving X values while
      smoothing Y values. Apply before passing data to chart marks.

      {[
        let smoothed = Transform.ema 0.1 raw_data in
        chart |> line ~x:fst ~y:snd smoothed
      ]} *)

  val ema : float -> (float * float) array -> (float * float) array
  (** [ema alpha data] applies exponential moving average.

      Each output Y is [alpha * y + (1 - alpha) * prev_smoothed].

      @param alpha
        Smoothing factor in [(0, 1)]. Lower values = more smoothing. Typical
        values: [0.1] (heavy), [0.3] (moderate), [0.5] (light). *)

  val sma : int -> (float * float) array -> (float * float) array
  (** [sma window data] applies simple moving average.

      Each output Y is the mean of the surrounding [window] points. Initial
      points use partial windows.

      @param window Number of points to average. Must be positive. *)

  val gaussian : float -> (float * float) array -> (float * float) array
  (** [gaussian sigma data] applies Gaussian smoothing.

      Convolves data with a Gaussian kernel of the given standard deviation.
      Preserves endpoints better than EMA/SMA.

      @param sigma
        Standard deviation in index units. Typical values: [1.0] to [5.0]. *)
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
    | Log of { base : float; domain : numeric_domain; clamp : bool }
        (** Logarithmic scale. Useful for data spanning multiple orders of
            magnitude. [base] is typically 10 or e. Values <= 0 are handled
            gracefully. *)
    | Band of { categories : string list option; padding : float }
        (** Categorical scale with evenly spaced bands. [padding] is the
            fraction of the axis reserved for gaps (0.0 to 1.0). *)

  val numeric : ?domain:numeric_domain -> ?clamp:bool -> unit -> t
  (** [numeric ~domain ~clamp ()] creates a numeric scale.

      @param domain Domain strategy. Default is [`Auto].
      @param clamp Restrict view windows to domain. Default is [true]. *)

  val log : ?base:float -> ?domain:numeric_domain -> ?clamp:bool -> unit -> t
  (** [log ~base ~domain ~clamp ()] creates a logarithmic scale.

      @param base
        Log base. Default is [10.0]. Common values: [10.0], [2.0], [e].
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

  type line = [ `None | `Axis_only | `Frame ]
  (** Axis line rendering mode.

      - [`None]: No axis line is drawn.
      - [`Axis_only]: Draw only the axis line at the edge of the plot.
      - [`Frame]: Draw a complete frame around the plot area. *)

  type title = { text : string; style : Ansi.Style.t option }
  (** Axis title configuration. *)

  type t = {
    show : bool;  (** Render axis and ticks. *)
    line : line;  (** Axis line rendering mode. *)
    ticks : int;
        (** Target number of ticks. Actual count may vary for alignment. *)
    format : formatter;  (** Label formatter. *)
    style : Ansi.Style.t option;
        (** Style for axis line. [None] uses theme default. *)
    tick_style : Ansi.Style.t option;
        (** Style for tick marks. [None] uses theme default. *)
    label_style : Ansi.Style.t option;
        (** Style for tick labels. [None] uses theme default. *)
    tick_length : int;  (** Tick mark length in cells. *)
    label_padding : int;  (** Spacing between ticks and labels in cells. *)
    title : title option;  (** Optional axis title. *)
  }
  (** Axis configuration.

      Style fields default to [None], which means the theme's default styles
      will be used. Set explicit styles to override theme defaults. *)

  val hidden : t
  (** [hidden] disables axis rendering.

      All fields are zeroed or set to defaults. *)

  val default : t
  (** [default] provides standard axis rendering.

      Enables axis with [6] ticks, default float formatter, [1]-cell tick marks,
      and [1]-cell label padding. Uses [`Axis_only] line mode. *)

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

  val with_line : line -> t -> t
  (** [with_line mode t] sets axis line rendering mode to [mode]. *)

  val with_title : ?style:Ansi.Style.t -> string -> t -> t
  (** [with_title text t] sets the axis title to [text].

      @param style Title style. [None] uses the theme's label style. *)
end

module Gridlines : sig
  (** Background grid configuration.

      Grids render lines aligned to axis ticks or at fixed intervals. *)

  type t = {
    show : bool;  (** Enable grid rendering. *)
    x : bool;  (** Draw vertical grid lines. *)
    y : bool;  (** Draw horizontal grid lines. *)
    style : Ansi.Style.t;  (** Grid line style. *)
    pattern : Charset.line_pattern;  (** Line pattern (solid/dashed/dotted). *)
    x_step : int option;
        (** Fixed vertical line spacing in pixels. [None] aligns to X axis
            ticks. *)
    y_step : int option;
        (** Fixed horizontal line spacing in pixels. [None] aligns to Y axis
            ticks. *)
    minor : int option;
        (** Minor grid frequency. [Some n] draws minor grids every [n] major
            steps. [None] disables minor grids. *)
    minor_style : Ansi.Style.t option;
        (** Style for minor grid lines. [None] uses theme's [grid_minor]. *)
  }
  (** Grid configuration. *)

  val hidden : t
  (** [hidden] disables grid rendering. *)

  val default : t
  (** [default] enables grid with dotted pattern, aligned to axis ticks.

      Both [x] and [y] are [true]; [pattern] is [`Dotted]; [x_step] and [y_step]
      are [None]. *)

  val with_style : Ansi.Style.t -> t -> t
  (** [with_style s t] sets grid line style to [s]. *)

  val with_pattern : Charset.line_pattern -> t -> t
  (** [with_pattern p t] sets grid line pattern to [p]. *)

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

  val with_minor : int option -> t -> t
  (** [with_minor n t] sets minor grid frequency.

      [Some n] draws minor grids every [n] major steps. [None] disables. *)

  val with_minor_style : Ansi.Style.t option -> t -> t
  (** [with_minor_style s t] sets minor grid style to [s]. *)
end

module View : sig
  (** Viewport windowing for zoom and pan.

      A {!t} restricts the visible data range on each axis. Views are
      independent of layout and belong in your application model. *)

  type window = { min : float; max : float }
  (** A data range on one axis.

      Invariant: [min] and [max] are automatically swapped if [min > max] during
      construction. *)

  type t = { x : window option; y : window option; y2 : window option }
  (** Viewport specification.

      [None] uses the full data domain. [Some w] restricts the visible range to
      [w]. The [y2] field controls the secondary Y-axis independently from [y].
  *)

  val empty : t
  (** [empty] shows the full data extent on all axes.

      All fields ([x], [y], [y2]) are [None]. *)

  val set_x : window option -> t -> t
  (** [set_x w t] sets the X-axis view window to [w]. *)

  val set_y : window option -> t -> t
  (** [set_y w t] sets the primary Y-axis view window to [w]. *)

  val set_y2 : window option -> t -> t
  (** [set_y2 w t] sets the secondary Y-axis view window to [w]. *)

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

module Raster : sig
  (** Sub-cell resolution modes for high-fidelity rendering. *)

  type resolution = [ `Cell | `Wave | `Block2x2 | `Braille2x4 ]
  (** Raster resolution for mark rendering.

      - [`Cell]: One data point per terminal cell using diagonal line characters
        (╱╲).
      - [`Wave]: Smooth curves using Unicode box-drawing characters (╭╮╰╯─│).
        Produces visually appealing connected lines.
      - [`Block2x2]: Two data points per cell using quadrant blocks (▘▝▖▗).
      - [`Braille2x4]: Eight data points per cell using Braille patterns. *)
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

  type scatter_mode = [ `Cell | `Braille | `Density ]
  (** Scatter point rendering mode.

      - [`Cell]: One glyph per cell.
      - [`Braille]: Higher resolution using Braille dots.
      - [`Density]: Heat-style density rendering using shade levels. *)

  type heatmap_agg = [ `Last | `Avg | `Max ]
  (** Aggregation strategy when multiple values map to the same cell.

      - [`Last]: Use the most recent value.
      - [`Avg]: Average all values.
      - [`Max]: Use the maximum value. *)

  (** Heatmap rendering mode. *)
  type heatmap_mode =
    | Cells_fg  (** Colored foreground glyph (e.g., "█"). *)
    | Cells_bg  (** Colored background with space glyph. *)
    | Halfblock_fg_bg
        (** Two values per row using "▀" with fg/bg colors. Doubles vertical
            resolution. *)
    | Shaded  (** Shaded block glyphs for intensity (works without color). *)
    | Dense_bilinear  (** Bilinear interpolation onto target resolution. *)

  type bar_mode = [ `Cell | `Half_block ]
  (** Bar rendering mode.

      - [`Cell]: Standard cell-based bars.
      - [`Half_block]: Uses ▄▀ for 2x vertical resolution (vertical bars) or ▌▐
        for 2x horizontal resolution (horizontal bars). *)

  type area_baseline = [ `Zero | `Value of float ]
  (** Area chart baseline specification.

      - [`Zero]: Fill from y=0 to data values.
      - [`Value v]: Fill from y=v to data values. *)

  (** Histogram binning method.

      - [Bins n]: Divide data range into [n] equal-width bins.
      - [Width w]: Use bins of width [w].
      - [Edges arr]: Use explicit bin edges (length = num_bins + 1). *)
  type bin_method =
    | Bins of int  (** Fixed number of bins *)
    | Width of float  (** Fixed bin width *)
    | Edges of float array  (** Explicit bin edges *)

  type histogram_normalize = [ `Count | `Density | `Probability ]
  (** Histogram normalization mode.

      - [`Count]: Raw bin counts.
      - [`Density]: count / (total * bin_width) for density estimation.
      - [`Probability]: count / total for probability distribution. *)

  type candle_body = [ `Filled | `Hollow ]
  (** Candlestick body style.

      - [`Filled]: Solid filled body using block characters.
      - [`Hollow]: Empty body using box-drawing characters. *)

  type candle_width = [ `One | `Two ]
  (** Candlestick body width.

      - [`One]: Single-column body (compact).
      - [`Two]: Two-column body (enables better hollow box rendering). *)

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

  type y_axis_selector = [ `Y1 | `Y2 ]
  (** Selector for which Y-axis a mark should use.

      - [`Y1]: Primary Y-axis (left side, default).
      - [`Y2]: Secondary Y-axis (right side).

      Use with {!with_y2_scale} and {!with_y2_axis} to configure dual-axis
      charts. Marks default to [`Y1] if not specified. *)

  val line :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?resolution:Raster.resolution ->
    ?pattern:Charset.line_pattern ->
    ?glyph:string ->
    ?y_axis:y_axis_selector ->
    x:('a -> float) ->
    y:('a -> float) ->
    'a array ->
    t
  (** [line ~x ~y data] creates a line mark.

      Connects consecutive points with lines. Points are rendered in array
      order.

      @param id Optional identifier for hit-testing.
      @param label Optional label for auto-legend generation.
      @param style Line color. Defaults to theme palette.
      @param resolution Sub-cell resolution. Default is [`Cell].
      @param pattern Line pattern. Default is [`Solid].
      @param glyph
        When set, draws this glyph at each data point instead of connecting
        lines.
      @param y_axis Which Y-axis to use. Default is [`Y1]. *)

  val line_opt :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?resolution:Raster.resolution ->
    ?pattern:Charset.line_pattern ->
    ?glyph:string ->
    ?y_axis:y_axis_selector ->
    x:('a -> float) ->
    y:('a -> float option) ->
    'a array ->
    t
  (** [line_opt ~x ~y data] creates a line mark with optional Y values.

      Points where [y] returns [None] are skipped, creating gaps in the line.
      See {!line} for parameter documentation. *)

  val scatter :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?glyph:string ->
    ?mode:scatter_mode ->
    ?y_axis:y_axis_selector ->
    x:('a -> float) ->
    y:('a -> float) ->
    'a array ->
    t
  (** [scatter ~x ~y data] creates a scatter mark.

      Plots individual points at each coordinate.

      @param label Optional label for auto-legend generation.
      @param glyph
        Character(s) to render at each point. Default is the charset's
        [point_default] (e.g., ["∙"] for Unicode, ["*"] for ASCII).
      @param mode
        Rendering mode. Default is [`Cell]. Use [`Density] for overplotting
        visualization.
      @param y_axis Which Y-axis to use. Default is [`Y1]. *)

  val bars_y :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?mode:bar_mode ->
    x:('a -> string) ->
    y:('a -> float) ->
    'a array ->
    t
  (** [bars_y ~x ~y data] creates vertical bars.

      Bars extend from [0] to [y] for each category [x]. Requires a
      {!Scale.Band} on the X-axis.

      @param label Optional label for auto-legend generation.
      @param render
        Bar resolution. Default is [`Cell]. [`Half_block] uses ▄▀ for smoother
        tops. *)

  val bars_x :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?mode:bar_mode ->
    y:('a -> string) ->
    x:('a -> float) ->
    'a array ->
    t
  (** [bars_x ~y ~x data] creates horizontal bars.

      Bars extend from [0] to [x] for each category [y]. Requires a
      {!Scale.Band} on the Y-axis.

      @param render
        Bar resolution. Default is [`Cell]. [`Half_block] uses ▌▐ for smoother
        edges. *)

  val stacked_bars_y :
    ?id:id ->
    ?gap:int ->
    ?bar_width:int ->
    ?mode:bar_mode ->
    stacked_bar array ->
    t
  (** [stacked_bars_y data] creates vertical stacked bars.

      Each bar is composed of segments stacked from bottom to top. Segment
      values are cumulative.

      @param gap
        Spacing between bars in pixels. Default is [1]. Negative values are
        clamped to [0].
      @param bar_width
        Explicit bar width in pixels. [None] auto-sizes based on band width.
      @param render Bar resolution. Default is [`Cell]. *)

  val stacked_bars_x :
    ?id:id ->
    ?gap:int ->
    ?bar_height:int ->
    ?mode:bar_mode ->
    stacked_bar array ->
    t
  (** [stacked_bars_x data] creates horizontal stacked bars.

      Each bar is composed of segments stacked from left to right.

      @param gap
        Spacing between bars in pixels. Default is [1]. Negative values are
        clamped to [0].
      @param bar_height
        Explicit bar height in pixels. [None] auto-sizes based on band height.
      @param render Bar resolution. Default is [`Cell]. *)

  val rule_y :
    ?id:id ->
    ?style:Ansi.Style.t ->
    ?pattern:Charset.line_pattern ->
    ?y_axis:y_axis_selector ->
    float ->
    t
  (** [rule_y y] creates a horizontal rule at Y-coordinate [y].

      Draws a line spanning the full plot width.

      @param pattern Line pattern. Default is [`Solid].
      @param y_axis Which Y-axis to use. Default is [`Y1]. *)

  val rule_x :
    ?id:id -> ?style:Ansi.Style.t -> ?pattern:Charset.line_pattern -> float -> t
  (** [rule_x x] creates a vertical rule at X-coordinate [x].

      Draws a line spanning the full plot height.

      @param pattern Line pattern. Default is [`Solid]. *)

  val heatmap :
    ?id:id ->
    ?color_scale:Ansi.Color.t array ->
    ?value_range:float * float ->
    ?auto_value_range:bool ->
    ?agg:heatmap_agg ->
    ?mode:heatmap_mode ->
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
      @param render Rendering mode. Default is [Cells_fg]. *)

  val candles :
    ?id:id ->
    ?bullish:Ansi.Style.t ->
    ?bearish:Ansi.Style.t ->
    ?width:candle_width ->
    ?body:candle_body ->
    ?y_axis:y_axis_selector ->
    ohlc array ->
    t
  (** [candles data] creates a candlestick chart.

      Renders OHLC data with wicks (high/low) and bodies (open/close). Body
      color indicates direction.

      @param bullish Style for up candles ([close > open]). Default is green.
      @param bearish Style for down candles ([close < open]). Default is red.
      @param width Body width. Default is [`One].
      @param body Body style. Default is [`Filled].
      @param y_axis Which Y-axis to use. Default is [`Y1]. *)

  val circle :
    ?id:id ->
    ?style:Ansi.Style.t ->
    ?resolution:Raster.resolution ->
    ?y_axis:y_axis_selector ->
    cx:('a -> float) ->
    cy:('a -> float) ->
    r:('a -> float) ->
    'a array ->
    t
  (** [circle ~cx ~cy ~r data] creates circle marks.

      Draws circles centered at [(cx, cy)] with radius [r] for each datum.

      @param render Resolution mode. Default is [`Cell].
      @param y_axis Which Y-axis to use. Default is [`Y1]. *)

  val shade_x :
    ?id:id -> ?style:Ansi.Style.t -> min:float -> max:float -> unit -> t
  (** [shade_x ~min ~max ()] creates a vertical shaded region.

      Fills the area between X-coordinates [min] and [max]. If [min > max], they
      are swapped. *)

  val column_background : ?id:id -> ?style:Ansi.Style.t -> float -> t
  (** [column_background x] creates a vertical background column at [x].

      Fills a single column spanning the plot height. Useful for highlighting
      time ranges. *)

  val area :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?baseline:area_baseline ->
    ?resolution:Raster.resolution ->
    ?y_axis:y_axis_selector ->
    x:('a -> float) ->
    y:('a -> float) ->
    'a array ->
    t
  (** [area ~x ~y data] creates a filled area chart.

      Fills the region between the data line and a baseline.

      @param baseline
        Bottom of fill region. Default is [`Zero]. Use [`Value v] for a custom
        baseline.
      @param resolution
        Rendering resolution. [`Cell] uses full blocks, [`Braille2x4] provides
        sub-cell precision.
      @param y_axis Which Y-axis to use. Default is [`Y1]. *)

  val fill_between :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?resolution:Raster.resolution ->
    ?y_axis:y_axis_selector ->
    x:('a -> float) ->
    y_low:('a -> float) ->
    y_high:('a -> float) ->
    'a array ->
    t
  (** [fill_between ~x ~y_low ~y_high data] fills the region between two lines.

      Useful for showing confidence intervals, error bounds, or ranges.

      @param resolution
        Rendering resolution. [`Cell] uses medium-shade blocks, [`Braille2x4]
        provides sub-cell precision.
      @param y_axis Which Y-axis to use. Default is [`Y1]. *)

  val histogram :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?bins:bin_method ->
    ?normalize:histogram_normalize ->
    x:('a -> float) ->
    'a array ->
    t
  (** [histogram ~x data] creates a histogram from continuous data.

      Automatically bins the data and creates vertical bars showing the
      distribution.

      @param bins Binning method. Default is [Bins 10].
      @param normalize Output values. Default is [`Count]. *)
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
      for a given chart, view, and grid size. Layouts are immutable and
      efficient to query. *)

  type t
  (** A compiled layout.

      Created by {!layout} or returned by {!draw}. Contains all information
      needed for coordinate transforms and hit-testing. *)

  type rect = { x : int; y : int; width : int; height : int }
  (** A rectangular region in pixel coordinates. *)

  val size : t -> int * int
  (** [size t] returns the full grid dimensions [(width, height)]. *)

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

  val y2_domain : t -> View.window option
  (** [y2_domain t] returns the secondary Y-axis data domain.

      Returns [None] if no Y2 scale is configured. *)

  val x_view : t -> View.window
  (** [x_view t] returns the effective X-axis view window used for rendering.

      If a view was provided, this is the clamped window; otherwise, it equals
      [x_domain t]. *)

  val y_view : t -> View.window
  (** [y_view t] returns the effective Y-axis view window. *)

  val y2_view : t -> View.window option
  (** [y2_view t] returns the secondary Y-axis view window.

      Returns [None] if no Y2 scale is configured. *)

  val y_axis_title_width : t -> int
  (** [y_axis_title_width t] returns the width reserved for the Y-axis title.

      Returns [0] if no Y-axis title is configured. *)

  val y2_axis_width : t -> int
  (** [y2_axis_width t] returns the width reserved for the secondary Y-axis.

      Returns [0] if no Y2 axis is configured. *)

  val has_y2 : t -> bool
  (** [has_y2 t] returns [true] if the layout has a secondary Y-axis configured.

      This is [true] when both {!with_y2_scale} and {!with_y2_axis} have been
      called on the chart specification. *)

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
    ?pattern:Charset.line_pattern ->
    Layout.t ->
    Grid.t ->
    x:float ->
    y:float ->
    unit
  (** [crosshair layout grid ~x ~y] draws crosshair lines at data coordinates
      [(x, y)].

      Draws a vertical line at [x] and a horizontal line at [y], each spanning
      the full plot area. Lines are clipped to the plot rectangle.

      @param style Line style. Default is theme's [crosshair] style.
      @param pattern Line pattern. Default is [`Solid]. *)

  val marker :
    ?style:Ansi.Style.t ->
    ?glyph:string ->
    Layout.t ->
    Grid.t ->
    x:float ->
    y:float ->
    unit
  (** [marker layout grid ~x ~y] draws a marker glyph at data coordinates
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

  type tooltip_border = [ `Theme | `None | `Style of Ansi.Style.t ]
  (** Tooltip border configuration.

      - [`Theme]: Use the theme's default tooltip border.
      - [`None]: No border.
      - [`Style s]: Use a custom border style [s]. *)

  val tooltip :
    ?style:Ansi.Style.t ->
    ?border:tooltip_border ->
    ?padding:int ->
    ?anchor:tooltip_anchor ->
    Layout.t ->
    Grid.t ->
    x:float ->
    y:float ->
    string list ->
    unit
  (** [tooltip layout grid ~x ~y lines] draws a tooltip box at data coordinates
      [(x, y)].

      The tooltip displays [lines] with optional border and padding. Lines are
      padded to equal width for consistent background fill. The box is
      positioned relative to [(x, y)] according to [anchor], then adjusted to
      fit within the plot area.

      @param style
        Text and background style. Default is theme's [tooltip] style.
      @param border
        Border configuration. Default is [`Theme] which uses theme's
        [tooltip_border]. Use [`None] to explicitly disable the border.
      @param padding
        Internal padding in cells. Default is [1]. Negative values are clamped
        to [0].
      @param anchor Positioning hint. Default is [`Auto]. *)

  type h_anchor = [ `Left | `Center | `Right ]
  (** Horizontal text alignment.

      - [`Left]: Anchor at left edge of text.
      - [`Center]: Anchor at center of text.
      - [`Right]: Anchor at right edge of text. *)

  type v_anchor = [ `Top | `Middle | `Bottom ]
  (** Vertical text alignment.

      - [`Top]: Anchor at top of text.
      - [`Middle]: Anchor at vertical center.
      - [`Bottom]: Anchor at bottom of text. *)

  val text :
    ?style:Ansi.Style.t ->
    ?anchor:h_anchor ->
    ?v_anchor:v_anchor ->
    Layout.t ->
    Grid.t ->
    x:float ->
    y:float ->
    string ->
    unit
  (** [text layout grid ~x ~y label] draws text at data coordinates [(x, y)].

      The text is positioned according to [anchor] (horizontal) and [v_anchor]
      (vertical). The text is only rendered if it falls within the plot area.

      @param style Text style. Default is theme's [labels] style.
      @param anchor Horizontal alignment. Default is [`Left].
      @param v_anchor Vertical alignment. Default is [`Middle]. *)

  type arrow_head = [ `None | `Arrow | `Dot ]
  (** Arrow head style.

      - [`None]: No head (plain line).
      - [`Arrow]: Directional arrow head.
      - [`Dot]: Circular dot at endpoint. *)

  val arrow :
    ?style:Ansi.Style.t ->
    ?head:arrow_head ->
    Layout.t ->
    Grid.t ->
    x1:float ->
    y1:float ->
    x2:float ->
    y2:float ->
    unit
  (** [arrow layout grid ~x1 ~y1 ~x2 ~y2] draws an arrow from [(x1, y1)] to
      [(x2, y2)] in data coordinates.

      The line is drawn using a simple step pattern. The arrow head (if any) is
      rendered at [(x2, y2)].

      @param style Line style. Default is theme's [labels] style.
      @param head Arrow head style. Default is [`Arrow]. *)
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
    Grid.t ->
    width:int ->
    height:int ->
    unit
  (** [draw items grid ~width ~height] renders a legend.

      Items are laid out in [direction] order, with spacing controlled by [gap].

      @param direction Layout direction. Default is [`Vertical].
      @param gap
        Spacing between items in cells. Default is [0]. For [`Horizontal],
        defaults to [2] if [0]. Negative values are clamped to [0]. *)

  val items_of_layout : Layout.t -> item list
  (** [items_of_layout layout] extracts legend items from marks that have
      labels.

      Returns a list of legend items for marks with [~label] set. Use this to
      auto-generate a legend from the chart without manually constructing items.
      Only marks with labels are included. *)
end

module Sparkline = Sparkline
(** Sparkline charts. See {!module-Sparkline} for details. *)

type t
(** An immutable chart specification.

    Charts are built by layering marks and configuration. Use {!empty} to start,
    then compose with {!with_theme}, {!with_x_scale}, {!add}, etc. *)

type frame_config = { margins : int * int * int * int; inner_padding : int }
(** Manual frame configuration for layout spacing.

    - [margins]: [(top, right, bottom, left)] in cells. Reserves space for axes.
    - [inner_padding]: Padding between plot area and frame in cells. *)

type frame =
  | Auto
  | Manual of frame_config
      (** Frame mode for layout spacing.

          - [Auto]: Automatically compute margins based on axis requirements.
          - [Manual cfg]: Use explicit margin and padding configuration. *)

type title = { text : string; style : Ansi.Style.t option }
(** Chart title configuration. *)

val default_frame : frame
(** [default_frame] is [Auto].

    Auto mode computes margins from axis label widths and tick sizes. *)

val manual_frame :
  ?margins:int * int * int * int -> ?inner_padding:int -> unit -> frame
(** [manual_frame ?margins ?inner_padding ()] creates a manual frame config.

    @param margins [(top, right, bottom, left)] in cells. Default [(0,0,0,0)].
    @param inner_padding Padding between plot area and frame. Default [0]. *)

val empty : ?theme:Theme.t -> unit -> t
(** [empty ()] creates an empty chart.

    @param theme Chart theme. Default is {!Theme.default}.

    The chart uses {!constructor-Scale.Auto} for both axes, {!Axis.default} for
    rendering, and {!Gridlines.hidden} for no grid. *)

val with_theme : Theme.t -> t -> t
(** [with_theme theme t] applies [theme] to [t].

    Theme styles are merged with existing axis and grid configurations. Explicit
    styles on marks are preserved. *)

val with_frame : frame -> t -> t
(** [with_frame frame t] sets the layout frame to [frame].

    For [Manual] frames, [inner_padding] is clamped to non-negative values. *)

val with_title : ?style:Ansi.Style.t -> string -> t -> t
(** [with_title text t] sets the chart title to [text].

    The title is rendered centered above the plot area.

    @param style Title style. [None] uses the theme's label style. *)

val with_x_scale : Scale.t -> t -> t
(** [with_x_scale scale t] sets the X-axis scale to [scale]. *)

val with_y_scale : Scale.t -> t -> t
(** [with_y_scale scale t] sets the Y-axis scale to [scale]. *)

val with_y2_scale : Scale.t -> t -> t
(** [with_y2_scale scale t] sets the secondary Y-axis scale.

    Use with marks that have [~y_axis:`Y2] to plot data on an independent
    right-side axis. The scale domain is computed from Y2 marks only. *)

val with_axes : ?x:Axis.t -> ?y:Axis.t -> t -> t
(** [with_axes ~x ~y t] configures axis rendering.

    Theme defaults are applied to provided axes. [None] preserves existing axis
    configuration. *)

val with_y2_axis : Axis.t -> t -> t
(** [with_y2_axis axis t] configures the secondary Y-axis rendering.

    The Y2 axis is drawn on the right side of the plot. Theme defaults are
    applied. Use with {!with_y2_scale} to enable dual-axis charts. *)

val with_grid : Gridlines.t -> t -> t
(** [with_grid grid t] sets the grid configuration to [grid].

    Theme defaults are applied to [grid]. *)

val add : Mark.t -> t -> t
(** [add mark t] appends [mark] to [t]'s mark list.

    Marks are rendered in the order added. Later marks draw on top of earlier
    ones. *)

val line :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?resolution:Raster.resolution ->
  ?pattern:Charset.line_pattern ->
  ?glyph:string ->
  ?y_axis:Mark.y_axis_selector ->
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
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?resolution:Raster.resolution ->
  ?pattern:Charset.line_pattern ->
  ?glyph:string ->
  ?y_axis:Mark.y_axis_selector ->
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
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?glyph:string ->
  ?mode:Mark.scatter_mode ->
  ?y_axis:Mark.y_axis_selector ->
  x:('a -> float) ->
  y:('a -> float) ->
  'a array ->
  t ->
  t
(** [scatter ~x ~y data t] adds a scatter mark to [t].

    Convenience for [add (Mark.scatter ~x ~y data) t]. *)

val bars_y :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?mode:Mark.bar_mode ->
  x:('a -> string) ->
  y:('a -> float) ->
  'a array ->
  t ->
  t
(** [bars_y ~x ~y data t] adds vertical bars to [t].

    Convenience for [add (Mark.bars_y ~x ~y data) t]. *)

val bars_x :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?mode:Mark.bar_mode ->
  y:('a -> string) ->
  x:('a -> float) ->
  'a array ->
  t ->
  t
(** [bars_x ~y ~x data t] adds horizontal bars to [t].

    Convenience for [add (Mark.bars_x ~y ~x data) t]. *)

val stacked_bars_y :
  ?id:Mark.id ->
  ?gap:int ->
  ?bar_width:int ->
  ?mode:Mark.bar_mode ->
  Mark.stacked_bar array ->
  t ->
  t
(** [stacked_bars_y data t] adds vertically stacked bars to [t].

    Convenience for [add (Mark.stacked_bars_y data) t]. *)

val stacked_bars_x :
  ?id:Mark.id ->
  ?gap:int ->
  ?bar_height:int ->
  ?mode:Mark.bar_mode ->
  Mark.stacked_bar array ->
  t ->
  t
(** [stacked_bars_x data t] adds horizontally stacked bars to [t].

    Convenience for [add (Mark.stacked_bars_x data) t]. *)

val rule_y :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  ?pattern:Charset.line_pattern ->
  ?y_axis:Mark.y_axis_selector ->
  float ->
  t ->
  t
(** [rule_y y t] adds a horizontal rule at y position [y].

    Convenience for [add (Mark.rule_y y) t]. *)

val rule_x :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  ?pattern:Charset.line_pattern ->
  float ->
  t ->
  t
(** [rule_x x t] adds a vertical rule at x position [x].

    Convenience for [add (Mark.rule_x x) t]. *)

val heatmap :
  ?id:Mark.id ->
  ?color_scale:Ansi.Color.t array ->
  ?value_range:float * float ->
  ?auto_value_range:bool ->
  ?agg:Mark.heatmap_agg ->
  ?mode:Mark.heatmap_mode ->
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
  ?width:Mark.candle_width ->
  ?body:Mark.candle_body ->
  ?y_axis:Mark.y_axis_selector ->
  Mark.ohlc array ->
  t ->
  t
(** [candles data t] adds candlesticks to [t].

    Convenience for [add (Mark.candles data) t]. *)

val circle :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  ?resolution:Raster.resolution ->
  ?y_axis:Mark.y_axis_selector ->
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

val area :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?baseline:Mark.area_baseline ->
  ?resolution:Raster.resolution ->
  ?y_axis:Mark.y_axis_selector ->
  x:('a -> float) ->
  y:('a -> float) ->
  'a array ->
  t ->
  t
(** [area ~x ~y data t] adds a filled area chart.

    Fills the region between the data line and a baseline. Useful for showing
    cumulative values or emphasizing the magnitude of a time series.

    @param baseline
      Bottom of fill region. Default is [`Zero]. Use [`Value v] for a custom
      baseline.
    @param resolution
      Rendering resolution. [`Cell] uses full blocks, [`Braille2x4] provides
      sub-cell precision.
    @param y_axis Which Y-axis to use. Default is [`Y1]. *)

val fill_between :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?resolution:Raster.resolution ->
  ?y_axis:Mark.y_axis_selector ->
  x:('a -> float) ->
  y_low:('a -> float) ->
  y_high:('a -> float) ->
  'a array ->
  t ->
  t
(** [fill_between ~x ~y_low ~y_high data t] fills the region between two lines.

    Useful for showing confidence intervals, error bounds, or ranges between two
    related series.

    @param resolution
      Rendering resolution. [`Cell] uses medium-shade blocks, [`Braille2x4]
      provides sub-cell precision.
    @param y_axis Which Y-axis to use. Default is [`Y1]. *)

val histogram :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?bins:Mark.bin_method ->
  ?normalize:Mark.histogram_normalize ->
  x:('a -> float) ->
  'a array ->
  t ->
  t
(** [histogram ~x data t] creates a histogram from continuous data.

    Automatically bins the data and renders vertical bars showing the
    distribution. Useful for visualizing data distributions.

    @param bins Binning method. Default is [Bins 10].
    @param normalize Output values. Default is [`Count]. *)

val layout :
  ?view:View.t -> ?x:int -> ?y:int -> t -> width:int -> height:int -> Layout.t
(** [layout t ~width ~height] compiles [t] into a {!Layout.t}.

    Resolves scales, infers domains from mark data, and computes coordinate
    mappings for the given grid size.

    @param view Viewport restriction. [None] uses full data extent.
    @param x Horizontal offset in the grid. Default is [0].
    @param y Vertical offset in the grid. Default is [0]. *)

val draw :
  ?view:View.t ->
  ?x:int ->
  ?y:int ->
  t ->
  Grid.t ->
  width:int ->
  height:int ->
  Layout.t
(** [draw t grid ~width ~height] renders [t] to [grid] and returns the computed
    layout.

    First computes a {!Layout.t} via {!layout}, then draws axes, gridlines, and
    all marks. The returned layout can be used for hit-testing and overlays.

    @param view Viewport restriction. [None] uses full data extent.
    @param x Horizontal offset in the grid. Default is [0].
    @param y Vertical offset in the grid. Default is [0]. *)
