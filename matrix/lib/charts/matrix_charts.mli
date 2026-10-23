(** Charts drawn on a {!Matrix_grid.t}.

    A {e chart} ({!t}) is an immutable description of what to draw. It holds a
    list of marks, a scale and an axis for the x axis, the y axis and an
    optional second y axis, gridlines, a frame, a title and a theme. A {e mark}
    ({!Mark.t}) draws one data set in one form, such as a line, bars or a
    heatmap. Marks are drawn in the order they were added, so a later mark
    covers an earlier one.

    Positions come in two kinds. {e Cell coordinates} [(px, py)] are the integer
    column and row of a grid cell, with [(0, 0)] the grid's top-left cell and
    [py] growing downwards. {e Data coordinates} [(x, y)] are floats in the
    units of the data. The {e plot} is the rectangle of cells where marks are
    drawn, inside the axes, labels and margins. A mark drawn at a sub-cell
    resolution ({!Raster.resolution}) places dots finer than a cell, and every
    position the functions take or return is still a whole cell.

    Each axis has a {e domain}, the range of data values the chart covers, and a
    {e view}, the part of the domain the plot shows. The domain is the extent of
    the marks' data on that axis unless the axis's scale fixes it
    ({!Scale.numeric_domain}). The view is the whole domain unless a {!View.t}
    narrows it. The program keeps its {!View.t} in its own state, passes it to
    each draw, and replaces it with the view a zoom or a pan returns.

    A {e layout} ({!Layout.t}) is a chart resolved for one region of the grid
    and one view. It holds the plot's rectangle, each axis's domain, view and
    ticks, and the marks with their styles. It maps cell coordinates to data
    coordinates and back, finds the data nearest a cell, and is what the
    overlays ({!Overlay}) draw with.

    The life cycle of a chart in an interactive program usually goes like this:
    + Build a {!t} from marks, with {!make}, or with {!empty} and {!add}.
    + Keep a {!View.t} in the program's state, {!View.empty} at first.
    + Call {!draw}, which draws the chart and returns its layout.
    + Draw overlays, such as a crosshair or a tooltip, on the same grid with
      that layout.
    + On input, ask the layout for the data under the pointer
      ({!Layout.hit_test}) or for the next view ({!Layout.zoom_view_around_px},
      {!Layout.pan_view_by_px}), and draw again.

    Unless a value's doc says otherwise:
    - Charts, marks, layouts and settings are immutable, and only the functions
      that take a {!Matrix_grid.t} write to it.
    - A count, length or padding below 0 is taken as 0.
    - A style left [None] is the theme's style for that element ({!Theme.t}).
    - Data values that are not finite (NaN and infinities) are left out of the
      domains inferred from the data.

    This draws [samples], an array of [(time, value)] pairs, as a line over
    gridlines in the 60 by 15 cells at the grid's top-left. It returns the
    layout a mouse handler needs to find the sample under the pointer.

    {[
    let draw_samples grid view samples =
      let chart =
        Matrix_charts.(
          empty () |> with_grid Gridlines.default |> line ~x:fst ~y:snd samples)
      in
      Matrix_charts.draw ~view chart grid ~width:60 ~height:15
    ]} *)

(** {1:charset Charset} *)

module Charset : sig
  (** Glyph sets for drawing charts.

      A charset gives the glyphs of axis lines, ticks, gridlines, points, bars,
      shades and tooltip borders. {!ascii} uses ASCII alone, and the other
      presets use Unicode box-drawing and block characters. *)

  type line_pattern = [ `Solid | `Dashed | `Dotted ]
  (** The type for line patterns. A gridline, rule or crosshair of pattern [p]
      is drawn with the charset's [grid_h_p] or [grid_v_p] glyph. *)

  type frame = {
    tl : string;  (** Top-left corner. *)
    tr : string;  (** Top-right corner. *)
    bl : string;  (** Bottom-left corner. *)
    br : string;  (** Bottom-right corner. *)
    h : string;  (** Horizontal segment. *)
    v : string;  (** Vertical segment. *)
    tee_up : string;  (** Upward tee junction. *)
    tee_down : string;  (** Downward tee junction. *)
    tee_left : string;  (** Leftward tee junction. *)
    tee_right : string;  (** Rightward tee junction. *)
    cross : string;  (** Cross junction. *)
  }
  (** The type for box-drawing glyph sets. *)

  type t = {
    frame : frame;
        (** Axis lines, the corners where axes meet, lines drawn at [`Cell]
            resolution and arrows. *)
    axis_h : string;
        (** Horizontal axis glyph. {!Matrix_charts.draw} draws axis lines with
            [frame.h] instead. *)
    axis_v : string;
        (** Vertical axis glyph. {!Matrix_charts.draw} draws axis lines with
            [frame.v] instead. *)
    tick_h : string;  (** Tick marks of the y axes. *)
    tick_v : string;  (** Tick marks of the x axis. *)
    grid_h_solid : string;  (** Horizontal line, [`Solid]. *)
    grid_v_solid : string;  (** Vertical line, [`Solid]. *)
    grid_h_dashed : string;  (** Horizontal line, [`Dashed]. *)
    grid_v_dashed : string;  (** Vertical line, [`Dashed]. *)
    grid_h_dotted : string;  (** Horizontal line, [`Dotted]. *)
    grid_v_dotted : string;  (** Vertical line, [`Dotted]. *)
    point_default : string;  (** Default glyph of a scatter point. *)
    point_heavy : string;
        (** Default glyph of {!Overlay.marker} and of an arrow's [`Dot] head. *)
    bar_fill : string;  (** Fill of bars, areas, histograms and circles. *)
    shade_levels : string array;
        (** Shades from lightest to heaviest. Fills between curves use the
            middle one. *)
    tooltip_frame : frame;  (** Tooltip borders. *)
    wave_frame : frame;  (** Line bends at [`Wave] resolution. *)
    diag_up : string;  (** Upward diagonal of an arrow. *)
    diag_down : string;  (** Downward diagonal of an arrow. *)
  }
  (** The type for charsets. *)

  val ascii : t
  (** [ascii] uses ASCII characters alone. *)

  val unicode_light : t
  (** [unicode_light] uses light box-drawing characters, with rounded tooltip
      corners. *)

  val unicode_heavy : t
  (** [unicode_heavy] uses heavy box-drawing characters. *)

  val unicode_rounded : t
  (** [unicode_rounded] uses light box-drawing characters with rounded corners.
  *)

  val default : t
  (** [default] is {!unicode_light}. *)
end

(** {1:theme Theme} *)

module Theme : sig
  (** Themes, which give a chart its colors, styles and glyphs.

      A theme holds the {e palette} that marks without a style take their color
      from, the chart's background, the style of each element that a chart, axis
      or overlay leaves unstyled, and a {!Charset.t}. *)

  type t = {
    palette : Ansi.Color.t array;
        (** Colors given in turn to the marks that have no style ({!Mark}),
            starting again from the first after the last. With an empty palette
            those marks have a transparent foreground. *)
    background : Ansi.Color.t option;
        (** Fill of the chart's region before drawing. [None] leaves the cells
            as they are. *)
    axes : Ansi.Style.t;  (** Axis lines and tick marks. *)
    border : Ansi.Style.t;
        (** Style for a border around the chart. {!Matrix_charts.draw} draws no
            such border. *)
    grid : Ansi.Style.t;
        (** Gridlines whose style is {!Ansi.Style.default} ({!Gridlines}). *)
    grid_minor : Ansi.Style.t;  (** Minor gridlines. *)
    labels : Ansi.Style.t;
        (** Tick labels, axis titles, the chart title, and the text and arrows
            of {!Overlay}. *)
    tooltip : Ansi.Style.t;  (** Tooltip text and background. *)
    tooltip_border : Ansi.Style.t option;
        (** Tooltip border. [None] draws no border. *)
    crosshair : Ansi.Style.t;  (** Crosshair lines. *)
    marker : Ansi.Style.t;  (** Markers of {!Overlay.marker}. *)
    charset : Charset.t;  (** Glyphs. *)
  }
  (** The type for themes. *)

  val dark : t
  (** [dark] is a theme for terminals with a dark background. *)

  val light : t
  (** [light] is a theme for terminals with a light background. *)

  val default : t
  (** [default] is {!dark}. *)

  val with_charset : Charset.t -> t -> t
  (** [with_charset cs t] is [t] with charset [cs]. *)
end

(** {1:label_format Label formatting} *)

module Label_format : sig
  (** Tick label formatters.

      A formatter has the type {!Axis.formatter}. It receives the index of the
      tick, from 0, and the tick's value. The formatters below ignore the index.
  *)

  val float : ?precision:int -> unit -> int -> float -> string
  (** [float ?precision ()] formats a value as [Printf]'s [%.*g] does, with
      [precision] significant digits, [3] by default. *)

  val mmdd_utc : int -> float -> string
  (** [mmdd_utc i v] formats the Unix time [v], in seconds, as its month and day
      in UTC, [MM/DD]. A negative [v] is a time before 1970, so [-1.] is
      [12/31]. *)

  val hhmmss_utc : int -> float -> string
  (** [hhmmss_utc i v] formats the Unix time [v], in seconds, as its time of day
      in UTC, [HH:MM:SS], rounded down to the second. A negative [v] is a time
      before 1970, so [-1.] is [23:59:59]. *)
end

(** {1:transform Transform} *)

module Transform : sig
  (** Smoothing of [(x, y)] series.

      A transform keeps each point's [x] and smooths the [y] values. It leaves
      its input unmodified, and an empty input gives an empty output. *)

  val ema : float -> (float * float) array -> (float * float) array
  (** [ema alpha data] is the exponential moving average of [data]. The first
      point is kept, and each later [y] becomes
      [alpha *. y +. (1. -. alpha) *. prev], where [prev] is the previous
      smoothed value. [alpha] must be in \[0, 1\]. A larger [alpha] follows
      recent values more closely. *)

  val sma : int -> (float * float) array -> (float * float) array
  (** [sma window data] is the simple moving average of [data] over the last
      [window] points. Each of the first [window - 1] points has the average of
      the points up to it. A [window] longer than [data] is taken as its length,
      and the result is empty when [window <= 0]. *)

  val gaussian : float -> (float * float) array -> (float * float) array
  (** [gaussian sigma data] is [data] smoothed by a Gaussian kernel of standard
      deviation [sigma] and radius [ceil (3 *. sigma)], both counted in points.
      Near the ends the kernel's weights are renormalized over the points that
      exist. It is [data] itself when [sigma <= 0.]. *)
end

(** {1:scale Scale} *)

module Scale : sig
  (** Scales, which map an axis's data values to cells.

      A numeric scale maps values linearly, a logarithmic scale maps their
      logarithms linearly, and a band scale divides the axis into equal bands,
      one per category. A {e clamping} scale keeps the view inside the domain,
      both when a chart is laid out and when its view is zoomed or panned
      ({!Layout.clamp_view}). *)

  type numeric_domain = [ `Auto | `Domain of float * float ]
  (** The type for domains. [`Auto] is the extent of the marks' data on the
      axis, and [`Domain (a, b)] is {!View.val-window} [~min:a ~max:b]. *)

  type t =
    | Auto
        (** {!Band} on the x axis when a mark draws vertical bars, on a y axis
            when a mark draws horizontal bars ({!Mark.bar},
            {!Mark.val-stacked_bar}), and a clamping {!Numeric} scale of domain
            [`Auto] otherwise. *)
    | Numeric of { domain : numeric_domain; clamp : bool }
        (** A linear scale, which clamps when [clamp] holds. *)
    | Log of { base : float; domain : numeric_domain; clamp : bool }
        (** A logarithmic scale in [base], which clamps when [clamp] holds. A
            value at or below 0 maps as [1e-10], and a domain or view bound at
            or below 0 is replaced by a small positive one. *)
    | Band of { categories : string list option; padding : float }
        (** A band scale. [categories] gives the categories in order, and [None]
            takes them from the bar marks drawn along the axis, in the order
            they first appear. A bar whose category is not among them is not
            drawn. [padding], from 0 to 0.95, is the fraction of the axis left
            empty, half before the first band and half after the last. *)

  val numeric : ?domain:numeric_domain -> ?clamp:bool -> unit -> t
  (** [numeric ?domain ?clamp ()] is [Numeric { domain; clamp }]. [domain]
      defaults to [`Auto] and [clamp] to [true]. *)

  val log : ?base:float -> ?domain:numeric_domain -> ?clamp:bool -> unit -> t
  (** [log ?base ?domain ?clamp ()] is [Log { base; domain; clamp }]. [base]
      defaults to [10.], and a [base] at or below [1.] is taken as [10.].
      [domain] defaults to [`Auto] and [clamp] to [true]. *)

  val band : ?categories:string list -> ?padding:float -> unit -> t
  (** [band ?categories ?padding ()] is [Band { categories; padding }].
      [categories] defaults to [None], and [padding] to [0.1], clamped to \[0,
      0.95\]. *)
end

(** {1:axis Axis} *)

module Axis : sig
  (** Axes, which draw a line, tick marks, tick labels and a title beside the
      plot.

      An axis puts its ticks at about [ticks] round values of its view, or at
      the center of each band of a band scale. Each tick has a mark
      [tick_length] cells long and, [label_padding] cells beyond it, a label. An
      x-axis label that would overlap the previous one is left out.

      An axis's styles left [None] take the chart's theme when the chart takes
      the axis ({!Matrix_charts.empty}, {!Matrix_charts.make},
      {!Matrix_charts.with_axes}, {!Matrix_charts.with_y2_axis}). The line and
      tick marks take {!Theme.t.axes}, and the labels {!Theme.t.labels}. *)

  type formatter = int -> float -> string
  (** The type for tick label formatters, applied to the tick's index, from 0,
      and its value ({!Label_format}). *)

  type line = [ `None | `Axis_only | `Frame ]
  (** The type for axis lines. [`None] draws no line and [`Axis_only] the axis
      line. [`Frame] draws the axis line and one edge of a frame around the
      plot. The y axis draws the top edge and the x axis the right edge, and
      each draws the other's line when the other axis is hidden. *)

  type title = { text : string; style : Ansi.Style.t option }
  (** The type for axis titles. A [style] of [None] is the axis's label style.
  *)

  type t = {
    show : bool;
        (** Whether the line, ticks and labels are drawn and given space. *)
    line : line;  (** Axis line. *)
    ticks : int;
        (** Number of ticks aimed at. Ticks fall on round values, so the number
            drawn may differ. *)
    format : formatter;  (** Tick label formatter. *)
    style : Ansi.Style.t option;  (** Style of the axis line. *)
    tick_style : Ansi.Style.t option;  (** Style of the tick marks. *)
    label_style : Ansi.Style.t option;  (** Style of the tick labels. *)
    tick_length : int;  (** Length of a tick mark, in cells. *)
    label_padding : int;  (** Cells between a tick mark and its label. *)
    title : title option;
        (** Axis title. A y-axis title is drawn down 3 columns kept left of the
            axis, one grapheme per row, and an x-axis title is centered below
            the labels. *)
  }
  (** The type for axes. *)

  val hidden : t
  (** [hidden] is an axis with [show = false], no line, [0] ticks, and tick
      length and label padding [0]. *)

  val default : t
  (** [default] is a shown axis with an [`Axis_only] line, [6] ticks, tick
      length and label padding [1], labels formatted as [%.3g], unset styles and
      no title. *)

  val with_ticks : int -> t -> t
  (** [with_ticks n a] is [a] with [ticks = max 0 n]. *)

  val with_format : formatter -> t -> t
  (** [with_format fmt a] is [a] with tick label formatter [fmt]. *)

  val with_style : Ansi.Style.t -> t -> t
  (** [with_style s a] is [a] with axis line style [s]. *)

  val with_tick_style : Ansi.Style.t -> t -> t
  (** [with_tick_style s a] is [a] with tick mark style [s]. *)

  val with_label_style : Ansi.Style.t -> t -> t
  (** [with_label_style s a] is [a] with tick label style [s]. *)

  val with_tick_length : int -> t -> t
  (** [with_tick_length n a] is [a] with [tick_length = max 0 n]. *)

  val with_label_padding : int -> t -> t
  (** [with_label_padding n a] is [a] with [label_padding = max 0 n]. *)

  val with_line : line -> t -> t
  (** [with_line l a] is [a] with axis line [l]. *)

  val with_title : ?style:Ansi.Style.t -> string -> t -> t
  (** [with_title ?style text a] is [a] with the title [text] in [style]. *)
end

(** {1:gridlines Gridlines} *)

module Gridlines : sig
  (** Gridlines, drawn across the plot behind the marks.

      With no step, a gridline runs through each tick of its axis, and on a band
      axis along each band's edge. A step of [n] draws a gridline every [n]
      cells from the plot's left or top edge instead, and a step below 1 draws
      none. Minor gridlines divide the space between two major gridlines of a
      numeric or logarithmic axis without a step.

      A gridline style equal to {!Ansi.Style.default} takes {!Theme.t.grid} when
      the chart takes the gridlines ({!Matrix_charts.with_grid}). *)

  type t = {
    show : bool;  (** Whether any gridline is drawn. *)
    x : bool;  (** Whether vertical gridlines are drawn. *)
    y : bool;  (** Whether horizontal gridlines are drawn. *)
    style : Ansi.Style.t;  (** Style of the major gridlines. *)
    pattern : Charset.line_pattern;  (** Pattern of every gridline. *)
    x_step : int option;
        (** [Some n] draws a vertical gridline every [n] cells. [None] draws one
            at each x tick. *)
    y_step : int option;
        (** [Some n] draws a horizontal gridline every [n] cells. [None] draws
            one at each y tick. *)
    minor : int option;
        (** [Some n] divides each space between two major gridlines into [n]
            parts with [n - 1] minor gridlines. [None] draws none. *)
    minor_style : Ansi.Style.t option;
        (** Style of the minor gridlines. [None] is {!Theme.t.grid_minor}. *)
  }
  (** The type for gridline settings. *)

  val hidden : t
  (** [hidden] is {!default} with [show = false] and the style
      {!Ansi.Style.default}. *)

  val default : t
  (** [default] draws dotted, dimmed vertical and horizontal gridlines at every
      tick, with no minor gridlines. *)

  val with_style : Ansi.Style.t -> t -> t
  (** [with_style s g] is [g] with major gridline style [s]. *)

  val with_pattern : Charset.line_pattern -> t -> t
  (** [with_pattern p g] is [g] with gridline pattern [p]. *)

  val with_x : bool -> t -> t
  (** [with_x b g] is [g] with [x = b]. *)

  val with_y : bool -> t -> t
  (** [with_y b g] is [g] with [y = b]. *)

  val with_x_step : int option -> t -> t
  (** [with_x_step n g] is [g] with [x_step = n]. *)

  val with_y_step : int option -> t -> t
  (** [with_y_step n g] is [g] with [y_step = n]. *)

  val with_minor : int option -> t -> t
  (** [with_minor n g] is [g] with [minor = n]. *)

  val with_minor_style : Ansi.Style.t option -> t -> t
  (** [with_minor_style s g] is [g] with [minor_style = s]. *)
end

(** {1:view View} *)

module View : sig
  (** Views, the windows of data a program shows.

      A {e window} ({!type-window}) is a range of data values on one axis, and a
      view ({!t}) holds a window, or [None] for the whole domain, for each axis.
      A program keeps its view in its own state, passes it to
      {!Matrix_charts.draw}, and replaces it with the view a zoom or a pan
      returns ({!Layout.zoom_view_around_px}, {!Layout.pan_view_by_px}).

      Every window the functions below return has [min < max]. A window narrower
      than [1e-12] is widened on both sides to at least that width. *)

  type window = { min : float; max : float }
  (** The type for windows, from [min] to [max]. *)

  type t = { x : window option; y : window option; y2 : window option }
  (** The type for views. [None] on an axis shows its whole domain. *)

  val empty : t
  (** [empty] is the view with every axis [None]. *)

  val set_x : window option -> t -> t
  (** [set_x w v] is [v] with x window [w]. *)

  val set_y : window option -> t -> t
  (** [set_y w v] is [v] with y window [w]. *)

  val set_y2 : window option -> t -> t
  (** [set_y2 w v] is [v] with the second y axis's window [w]. *)

  val window : min:float -> max:float -> window
  (** [window ~min ~max] is the window from [min] to [max], swapped when
      [min > max] and widened when narrower than [1e-12]. *)

  val zoom : window -> factor:float -> window
  (** [zoom w ~factor] is [w] scaled by [1 /. factor] about its center, so a
      [factor] above [1.] zooms in and one below zooms out. A [factor] at or
      below [0.] is taken as [1.]. *)

  val zoom_around : window -> center:float -> factor:float -> window
  (** [zoom_around w ~center ~factor] is [w] scaled by [1 /. factor] about
      [center], which keeps its relative position in the window. A [factor] at
      or below [0.] is taken as [1.], and a window narrower than [1e-12] is
      returned unchanged. *)

  val pan : window -> delta:float -> window
  (** [pan w ~delta] is [w] moved by [delta] data units. *)

  val clamp : domain:window -> window -> window
  (** [clamp ~domain w] is [w] moved the least distance that puts it inside
      [domain], or [domain] when [w] is at least as wide. *)
end

(** {1:raster Raster} *)

module Raster : sig
  (** Sub-cell resolutions. *)

  type resolution = [ `Cell | `Wave | `Block2x2 | `Braille2x4 ]
  (** The type for the resolutions of lines, areas, fills and circles.
      - [`Cell] draws whole-cell glyphs.
      - [`Wave] draws a line with the bends of {!Charset.t.wave_frame}.
      - [`Block2x2] places 2 by 2 dots per cell, drawn as quadrant blocks.
      - [`Braille2x4] places 2 by 4 dots per cell, drawn as Braille patterns,
        which the terminal's font must have.

      Areas and fills draw [`Wave] and [`Block2x2] as [`Cell], and circles draw
      every resolution but [`Braille2x4] as [`Cell]. *)
end

(** {1:mark Mark} *)

module Mark : sig
  (** Marks, which each draw one data set.

      A mark constructor applies its accessors, such as [~x] and [~y], to each
      element of its array once, when the mark is made, and keeps the values
      they return. Later changes to the array do not reach the mark, except for
      {!val-stacked_bar}, which keeps the array it is given.

      The constructors share these optional arguments:
      - [id] names the mark in hit-test results ({!Hit.t.mark_id}).
      - [label] names it in the legend ({!Legend.items_of_layout}).
      - [style] is its style. A mark without one takes the next color of the
        theme's palette, in the order the marks were added. Heatmaps, stacked
        bars and candles have colors of their own and leave the palette to the
        other marks, and a shade or a column background without a style has a
        dark gray background.
      - [y_axis], [`Y1] by default, is the y axis the mark is drawn against. A
        mark without this argument is drawn against the primary y axis.
      - [resolution], [`Cell] by default, is its {!Raster.resolution}.
      - [pattern], [`Solid] by default, is its {!Charset.line_pattern}. *)

  (** {2:types Types} *)

  type id = string
  (** The type for mark identifiers. *)

  type t
  (** The type for marks. A chart takes them with {!Matrix_charts.add} or
      {!Matrix_charts.make}. *)

  type direction = [ `Vertical | `Horizontal ]
  (** The type for directions. A [`Vertical] bar grows up from 0 along the y
      axis, and a [`Horizontal] bar grows right from 0 along the x axis. A
      [`Vertical] rule is a vertical line at an x value, and a [`Horizontal]
      rule a horizontal line at a y value. *)

  type scatter_mode = [ `Cell | `Braille | `Density ]
  (** The type for how scatter points are drawn.
      - [`Cell] draws a glyph per point.
      - [`Braille] draws a dot per point, at 2 by 4 dots per cell.
      - [`Density] shades each cell by its number of points, relative to the
        fullest cell, with {!Charset.t.shade_levels}. *)

  type heatmap_agg = [ `Last | `Avg | `Max ]
  (** The type for how a heatmap combines the values that fall in one cell.
      [`Last] keeps the last value, [`Avg] their average and [`Max] the largest.
  *)

  (** The type for how a heatmap is drawn. *)
  type heatmap_mode =
    | Cells_fg  (** A {!Charset.t.bar_fill} glyph of the value's color. *)
    | Cells_bg  (** A background of the value's color. *)
    | Halfblock_fg_bg
        (** Two values per cell, as the colors of its upper and lower halves. *)
    | Shaded
        (** A shade glyph of {!Charset.t.shade_levels} in the value's color,
            with values interpolated bilinearly between the data points. *)
    | Dense_bilinear
        (** A background of the value's color in every cell of the plot, with
            values interpolated bilinearly between the data points. *)

  type bar_mode = [ `Cell | `Half_block ]
  (** The type for how bar ends are drawn. [`Cell] rounds a bar to whole cells,
      and [`Half_block] ends it with a partial block glyph, in eighths of a
      cell. *)

  type candle_body = [ `Filled | `Hollow ]
  (** The type for candle bodies. [`Filled] fills every body. [`Hollow] draws
      the body of a rising candle ([close >= open]) narrower, which shows at
      width [`Two]. *)

  type candle_width = [ `One | `Two ]
  (** The type for candle widths, in cells. *)

  type area_baseline = [ `Zero | `Value of float ]
  (** The type for area baselines. [`Zero] fills to [y = 0] and [`Value v] to
      [y = v]. *)

  (** The type for histogram binnings. *)
  type bin_method =
    | Bins of int
        (** [n] equal bins over the data's range. An [n] below 1 is taken as 1.
        *)
    | Width of float
        (** Bins of width [w] from the smallest value. A [w] at or below 0 is
            taken as a tenth of the data's range. *)
    | Edges of float array
        (** The bins between consecutive edges, which must increase. With fewer
            than 2 edges it is [Bins 10]. *)

  type histogram_normalize = [ `Count | `Density | `Probability ]
  (** The type for histogram heights. [`Count] is the number of values in the
      bin, [`Probability] that number divided by the number of values, and
      [`Density] that number divided by the number of values and the bin's
      width, so that the bars' areas sum to 1. *)

  type bar_segment = {
    value : float;
    style : Ansi.Style.t;
    label : string option;
  }
  (** The type for stacked bar segments. A [value] below 0 is drawn as 0.
      Neither the chart nor the legend draws [label]. *)

  type stacked_bar = { category : string; segments : bar_segment list }
  (** The type for stacked bars, with segments stacked from 0 in list order. *)

  type ohlc = {
    time : float;  (** Position on the x axis, often a Unix time. *)
    open_ : float;
    high : float;
    low : float;
    close : float;
  }
  (** The type for a period's open, high, low and close values. *)

  type y_axis_selector = [ `Y1 | `Y2 ]
  (** The type for y axes. [`Y2] is the second y axis that
      {!Matrix_charts.with_y2_scale} adds. A chart may take a [`Y2] mark before
      that scale, and must have the scale when it is laid out or drawn
      ({!Matrix_charts.val-layout}). *)

  (** {2:constructors Constructors} *)

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
  (** [line ~x ~y data] is a line through the points of [data] in array order. A
      point whose [y] is not finite breaks the line there. At [`Cell] resolution
      a [glyph] is drawn at each point, with no line between, and at other
      resolutions it is ignored. *)

  val line_gaps :
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
  (** [line_gaps ~x ~y data] is {!line}, with the line broken at each point
      where [y] is [None]. *)

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
  (** [scatter ~x ~y data] is a point for each element of [data], drawn by
      [mode], [`Cell] by default. In [`Cell] mode each point is [glyph],
      {!Charset.t.point_default} by default. *)

  val bar :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?direction:direction ->
    ?mode:bar_mode ->
    category:('a -> string) ->
    value:('a -> float) ->
    'a array ->
    t
  (** [bar ~category ~value data] is a bar of length [value] for each element of
      [data], in the band of its [category]. Bars are drawn only when the
      category axis, x for [`Vertical] and y for [`Horizontal], has a band
      scale, which {!Scale.type-t.constructor-Auto} gives it. The value axis's
      inferred domain includes 0. [direction] defaults to [`Vertical] and [mode]
      to [`Half_block]. *)

  val stacked_bar :
    ?id:id ->
    ?direction:direction ->
    ?gap:int ->
    ?size:int ->
    ?mode:bar_mode ->
    stacked_bar array ->
    t
  (** [stacked_bar data] is a bar for each element of [data], its segments
      stacked in their own styles. Bars are drawn on a band axis as {!bar}'s
      are. Without [size], a bar is as wide as its band less [gap + 1] cells,
      and at least 1 cell. [size] sets the width in cells, at most the band's
      width less 1. [gap] defaults to [1], [direction] to [`Vertical] and [mode]
      to [`Half_block]. *)

  val rule :
    ?id:id ->
    ?style:Ansi.Style.t ->
    ?direction:direction ->
    ?pattern:Charset.line_pattern ->
    ?y_axis:y_axis_selector ->
    float ->
    t
  (** [rule v] is a line across the plot at [y = v] when [direction] is
      [`Horizontal], its default, or at [x = v] when it is [`Vertical]. A
      vertical rule ignores [y_axis]. The inferred domain of its axis includes
      [v]. *)

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
  (** [heatmap ~x ~y ~value data] colors the cell of each point of [data] by its
      [value], drawn by [mode], [Cells_fg] by default. Values that fall in one
      cell are combined by [agg], [`Last] by default. [color_scale] lists colors
      from the lowest to the highest value, each for an equal part of the value
      range. When empty, its default, it is the theme's palette. The value range
      is [value_range] when given, else the range of the values when
      [auto_value_range] holds, its default, and else \[0, 1\]. A heatmap is
      drawn against the primary y axis. *)

  val candles :
    ?id:id ->
    ?bullish:Ansi.Style.t ->
    ?bearish:Ansi.Style.t ->
    ?width:candle_width ->
    ?body:candle_body ->
    ?y_axis:y_axis_selector ->
    ohlc array ->
    t
  (** [candles data] is a candlestick for each element of [data], in order of
      [time]. A candle with [close >= open] is drawn in [bullish], green by
      default, and the others in [bearish], red by default. [width] defaults to
      [`One] and [body] to [`Filled]. *)

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
  (** [circle ~cx ~cy ~r data] is the outline of a circle for each element of
      [data], centered at [(cx, cy)], of radius [r] in the data units of each
      axis. It is drawn as an ellipse of cells when the two axes scale
      differently. A negative [r] is taken as 0, which draws the center cell. *)

  val shade :
    ?id:id -> ?style:Ansi.Style.t -> min:float -> max:float -> unit -> t
  (** [shade ~min ~max ()] fills the plot's columns from [x = min] to [x = max],
      swapped when [min > max], with blanks in [style]. *)

  val column_bg : ?id:id -> ?style:Ansi.Style.t -> float -> t
  (** [column_bg x] fills the plot's column at [x] with blanks in [style]. *)

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
  (** [area ~x ~y data] fills, in the column of each point of [data], the cells
      between [y] and [baseline], [`Zero] by default. The inferred y domain
      includes the baseline. *)

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
  (** [fill_between ~x ~y_low ~y_high data] fills, in the column of each point
      of [data], the cells between [y_low] and [y_high] with the charset's
      middle shade. *)

  val histogram :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?bins:bin_method ->
    ?normalize:histogram_normalize ->
    x:('a -> float) ->
    'a array ->
    t
  (** [histogram ~x data] is a histogram of the values [x] reads, binned by
      [bins], [Bins 10] by default, when the mark is made, with heights by
      [normalize], [`Count] by default. A value outside explicit edges counts in
      the first or last bin. *)
end

(** {1:hit Hit testing} *)

module Hit : sig
  (** Hit-test results.

      {!Layout.hit_test} looks for the data point, bar or candle nearest a cell,
      and a {!t} gives what it found in data and in cell coordinates. *)

  type policy = [ `Nearest_px | `Nearest_x | `Nearest_y ]
  (** The type for distances between cells, in cells. [`Nearest_px] is the
      Euclidean distance, [`Nearest_x] the horizontal distance and [`Nearest_y]
      the vertical distance. *)

  type kind =
    [ `Line | `Scatter | `Bars | `Stacked_bars | `Heatmap | `Candles | `Circle ]
  (** The type for the kinds of marks a hit test finds. Areas, fills,
      histograms, rules, shades and column backgrounds are left out of hit
      tests. *)

  (** The type for the data of a hit. *)
  type payload =
    | XY of { x : float; y : float }
        (** A point of a line or a scatter, or a circle's center. *)
    | Bar of { category : string; value : float }  (** A bar. *)
    | Stacked_bar of {
        category : string;
        segment_index : int;
        value : float;
        total : float;
      }
        (** A segment of a stacked bar, with its value, at least 0, and the
            total of its bar. *)
    | Heat of { x : float; y : float; value : float }  (** A heatmap point. *)
    | OHLC of {
        time : float;
        open_ : float;
        high : float;
        low : float;
        close : float;
      }  (** A candle. *)

  type t = {
    mark_id : string option;  (** The {!Mark.id} of the hit mark, if any. *)
    kind : kind;  (** Kind of the hit mark. *)
    index : int;
        (** Index of the hit element in the mark's data array. For a stacked bar
            it is the bar's index, and the payload's [segment_index] gives the
            segment. *)
    px : int;  (** Column of the hit. *)
    py : int;
        (** Row of the hit. A point's hit is its cell, and a bar's or a candle's
            the cell of its extent nearest the query. *)
    distance_px : float;
        (** Distance from the query to [(px, py)] by the policy, in cells. It is
            [0.] inside a bar. *)
    payload : payload;  (** Data of the hit element. *)
  }
  (** The type for hit-test results. *)
end

(** {1:layout Layout} *)

module Layout : sig
  (** Layouts, which are charts resolved for one region and one view.

      {!Matrix_charts.draw} and {!Matrix_charts.val-layout} make a layout. It
      fixes the plot's rectangle, each axis's domain and view, the ticks and the
      marks' styles, and maps cell coordinates to data coordinates and back. Its
      conversions use the primary y axis. *)

  type t
  (** The type for layouts. *)

  type rect = { x : int; y : int; width : int; height : int }
  (** The type for rectangles of cells, [width] columns and [height] rows from
      the top-left cell [(x, y)]. *)

  (** {2:geometry Geometry} *)

  val size : t -> int * int
  (** [size l] is the [(width, height)] of the chart's region, at least 1 each.
  *)

  val plot_rect : t -> rect
  (** [plot_rect l] is the plot's rectangle, inside the axes, labels, titles and
      margins. *)

  val is_inside_plot : t -> px:int -> py:int -> bool
  (** [is_inside_plot l ~px ~py] is [true] iff [(px, py)] is a cell of the plot.
  *)

  (** {2:domain Domains and views}

      A band axis's domain and view are \[0, n\] for its [n] categories. *)

  val x_domain : t -> View.window
  (** [x_domain l] is the x axis's domain. *)

  val y_domain : t -> View.window
  (** [y_domain l] is the y axis's domain. *)

  val y2_domain : t -> View.window option
  (** [y2_domain l] is the second y axis's domain, or [None] when the chart has
      no second y scale. *)

  val x_view : t -> View.window
  (** [x_view l] is the x axis's view. *)

  val y_view : t -> View.window
  (** [y_view l] is the y axis's view. *)

  val y2_view : t -> View.window option
  (** [y2_view l] is the second y axis's view, or [None] when the chart has no
      second y scale. *)

  val y_axis_title_width : t -> int
  (** [y_axis_title_width l] is the number of columns kept for the y-axis title,
      3 when there is one and 0 otherwise. *)

  val y2_axis_width : t -> int
  (** [y2_axis_width l] is the number of columns kept for the second y axis's
      line, ticks and labels. *)

  val has_y2 : t -> bool
  (** [has_y2 l] is [true] iff the chart has both a second y scale and a second
      y axis. *)

  (** {2:converting Coordinate conversion} *)

  val data_of_px : t -> px:int -> py:int -> (float * float) option
  (** [data_of_px l ~px ~py] is the data point at cell [(px, py)], or [None]
      when the cell is outside the plot. *)

  val px_of_data : t -> x:float -> y:float -> int * int
  (** [px_of_data l ~x ~y] is the cell of the data point [(x, y)]. A point
      outside the view maps to the nearest cell of the plot's edge. *)

  (** {2:categories Category lookup}

      These look up the categories of a band scale. On an axis with another
      scale they are [None]. *)

  val x_category_of_px : t -> px:int -> string option
  (** [x_category_of_px l ~px] is the category whose band holds column [px]. A
      column in the padding takes the nearest band, and a column outside the
      plot is [None]. *)

  val y_category_of_px : t -> py:int -> string option
  (** [y_category_of_px l ~py] is the category whose band holds row [py]. A row
      in the padding takes the nearest band, and a row outside the plot is
      [None]. *)

  val px_of_x_category : t -> string -> int option
  (** [px_of_x_category l c] is the center column of [c]'s band, or [None] when
      the x axis has no category [c]. *)

  val py_of_y_category : t -> string -> int option
  (** [py_of_y_category l c] is the center row of [c]'s band, or [None] when the
      y axis has no category [c]. *)

  (** {2:view_manip Zooming and panning}

      These take the program's view and return the next one. Each first applies
      {!clamp_view}, and each keeps the windows of a clamping axis inside its
      domain. The second y axis moves with the y axis. *)

  type axis = [ `X | `Y | `Both ]
  (** The type for the axes a zoom applies to. [`Y] covers both y axes. *)

  val clamp_view : t -> View.t -> View.t
  (** [clamp_view l v] is [v] with the window of each clamping numeric or
      logarithmic axis moved inside its domain ({!View.clamp}), the window of
      each other numeric or logarithmic axis kept, and the window of a band axis
      [None]. *)

  val zoom_view_around_px :
    t -> view:View.t -> axis:axis -> px:int -> py:int -> factor:float -> View.t
  (** [zoom_view_around_px l ~view ~axis ~px ~py ~factor] zooms the windows of
      [axis] by [factor] ({!View.zoom_around}) about the data point at cell
      [(px, py)], or about the view's center when the cell is outside the plot.
      The second y axis zooms about the center of its own view. *)

  val pan_view_by_px : t -> view:View.t -> dx:int -> dy:int -> View.t
  (** [pan_view_by_px l ~view ~dx ~dy] moves the x window by [dx] cells and the
      y windows by [dy] cells, where a cell is the window's span divided by the
      plot's width or height less one. A positive [dx] or [dy] moves a window
      toward larger values. *)

  val plot_center_px : t -> int * int
  (** [plot_center_px l] is the cell [(x + width / 2, y + height / 2)] of the
      plot's rectangle ({!plot_rect}). *)

  val zoom_view_around_center :
    t -> view:View.t -> axis:axis -> factor:float -> View.t
  (** [zoom_view_around_center l ~view ~axis ~factor] is {!zoom_view_around_px}
      at the cell {!plot_center_px}. *)

  (** {2:hit_testing Hit testing} *)

  val hit_test :
    ?radius:int -> ?policy:Hit.policy -> t -> px:int -> py:int -> Hit.t option
  (** [hit_test ?radius ?policy l ~px ~py] is the element nearest the cell
      [(px, py)] by [policy], [`Nearest_px] by default, among those within
      [radius] cells, [3] by default. Points outside the view are skipped. A bar
      or a candle is measured from its nearest cell, and a query inside a bar is
      at distance 0. Of equally near elements, the one of the later mark, or the
      later one in a mark, is found. It is [None] when [(px, py)] is outside the
      plot or nothing is within [radius], as with a negative [radius]. *)
end

(** {1:overlay Overlay} *)

module Overlay : sig
  (** Overlays, drawn over a chart after {!Matrix_charts.draw}.

      An overlay takes the chart's layout and the grid it was drawn on, and
      writes to the grid. Its positions are data coordinates, mapped by
      {!Layout.px_of_data}, so a point outside the view is drawn at the nearest
      cell of the plot's edge. *)

  val crosshair :
    ?style:Ansi.Style.t ->
    ?pattern:Charset.line_pattern ->
    Layout.t ->
    Matrix_grid.t ->
    x:float ->
    y:float ->
    unit
  (** [crosshair l grid ~x ~y] draws a vertical and a horizontal line across the
      plot through [(x, y)], with the glyphs of [pattern], [`Solid] by default,
      in [style], {!Theme.t.crosshair} by default. *)

  val marker :
    ?style:Ansi.Style.t ->
    ?glyph:string ->
    Layout.t ->
    Matrix_grid.t ->
    x:float ->
    y:float ->
    unit
  (** [marker l grid ~x ~y] draws [glyph], {!Charset.t.point_heavy} by default,
      at [(x, y)] in [style], {!Theme.t.marker} by default. *)

  type tooltip_anchor = [ `Auto | `Left | `Right | `Top | `Bottom ]
  (** The type for where a tooltip sits. [`Right] and [`Left] put it two cells
      beside its point, centered on the point's row, and [`Top] and [`Bottom]
      two cells above or below, centered on its column. [`Auto] takes, of
      [`Right], [`Left], [`Top] and [`Bottom] in this order, the first whose box
      the plot clips least among those whose box, once moved inside the plot,
      leaves the point uncovered, or among all four when each covers it. *)

  type tooltip_border = [ `Theme | `None | `Style of Ansi.Style.t ]
  (** The type for tooltip borders. [`Theme] is {!Theme.t.tooltip_border},
      [`None] draws no border and [`Style s] draws one in [s]. *)

  val tooltip :
    ?style:Ansi.Style.t ->
    ?border:tooltip_border ->
    ?padding:int ->
    ?anchor:tooltip_anchor ->
    Layout.t ->
    Matrix_grid.t ->
    x:float ->
    y:float ->
    string list ->
    unit
  (** [tooltip l grid ~x ~y lines] draws a box holding [lines], one per row,
      placed from [(x, y)] by [anchor], [`Auto] by default, and moved inside the
      plot. [style], {!Theme.t.tooltip} by default, is the style of the text and
      the box. [border] defaults to [`Theme], and [padding], [1] by default, is
      the number of cells between the border and the text. *)

  type h_anchor = [ `Left | `Center | `Right ]
  (** The type for the horizontal alignments of a text on its point. *)

  val text :
    ?style:Ansi.Style.t ->
    ?anchor:h_anchor ->
    Layout.t ->
    Matrix_grid.t ->
    x:float ->
    y:float ->
    string ->
    unit
  (** [text l grid ~x ~y s] draws [s] on the row of [(x, y)], starting at,
      centered on or ending at its column by [anchor], [`Left] by default, in
      [style], {!Theme.t.labels} by default. *)

  type arrow_head = [ `None | `Arrow | `Dot ]
  (** The type for arrow heads. [`None] draws no head, [`Arrow] an arrow glyph
      in the arrow's main direction, and [`Dot] {!Charset.t.point_heavy}. *)

  val arrow :
    ?style:Ansi.Style.t ->
    ?head:arrow_head ->
    Layout.t ->
    Matrix_grid.t ->
    x1:float ->
    y1:float ->
    x2:float ->
    y2:float ->
    unit
  (** [arrow l grid ~x1 ~y1 ~x2 ~y2] draws a line from [(x1, y1)] to [(x2, y2)]
      with [head], [`Arrow] by default, at [(x2, y2)], in [style],
      {!Theme.t.labels} by default. *)
end

(** {1:legend Legend} *)

module Legend : sig
  (** Legends, which name a chart's marks. *)

  type item = { label : string; style : Ansi.Style.t; marker : string }
  (** The type for legend items. An item draws [marker], then [label], both in
      [style]. *)

  val draw :
    ?direction:[ `Horizontal | `Vertical ] ->
    ?gap:int ->
    item list ->
    Matrix_grid.t ->
    width:int ->
    height:int ->
    unit
  (** [draw ?direction ?gap items grid ~width ~height] draws [items] from the
      cell [(0, 0)] of [grid]. [`Vertical], the default, puts one item per row,
      with [gap] empty rows between items, [0] by default. [`Horizontal] puts
      the items on row 0, with [gap] columns between them, where a [gap] of [0],
      the default, is taken as [2], and starts no item at or past column
      [width]. [height] is unused. *)

  val items_of_layout : Layout.t -> item list
  (** [items_of_layout l] is an item for each mark of [l] that has a label, in
      mark order and in the mark's style. Only lines, scatters, bars, areas,
      fills and histograms take a label. An item's marker is the mark's glyph,
      or else the charset's line, point, bar or middle shade glyph. *)
end

(** {1:sparkline Sparkline} *)

module Sparkline = Sparkline
(** Sparklines ({!Sparkline}). *)

(** {1:chart Charts}

    A chart made by {!empty} or {!make} has {!Scale.type-t.constructor-Auto}
    scales, {!Axis.default} x and y axes, no second y axis, {!Gridlines.hidden}
    gridlines, no title and an [Auto] frame. *)

type t
(** The type for charts. *)

type frame_config = { margins : int * int * int * int; inner_padding : int }
(** The type for frame settings. [margins] is [(top, right, bottom, left)] in
    cells around the chart's contents, and [inner_padding] is the number of
    cells kept on every side of the plot, inside the axes. *)

(** The type for frames. *)
type frame =
  | Auto  (** No margins and no padding. *)
  | Manual of frame_config  (** The given margins and padding. *)

type title = { text : string; style : Ansi.Style.t option }
(** The type for chart titles. A [style] of [None] is {!Theme.t.labels}. *)

val default_frame : frame
(** [default_frame] is [Auto]. *)

val manual_frame :
  ?margins:int * int * int * int -> ?inner_padding:int -> unit -> frame
(** [manual_frame ?margins ?inner_padding ()] is [Manual] with [margins],
    [(0, 0, 0, 0)] by default, and [inner_padding], [0] by default. *)

(** {2:constructors Constructors} *)

val empty : ?theme:Theme.t -> unit -> t
(** [empty ?theme ()] is a chart with no marks, in [theme], {!Theme.default} by
    default. *)

val make : ?theme:Theme.t -> ?title:string -> Mark.t list -> t
(** [make ?theme ?title marks] is a chart of [marks], drawn in list order, with
    [title], in [theme], {!Theme.default} by default. *)

(** {2:config Configuration}

    Each function below is the chart with one setting replaced. *)

val with_theme : Theme.t -> t -> t
(** [with_theme theme c] is [c] in [theme]. The styles its axes and gridlines
    left unset become [theme]'s. *)

val with_frame : frame -> t -> t
(** [with_frame f c] is [c] with frame [f]. *)

val with_title : ?style:Ansi.Style.t -> string -> t -> t
(** [with_title ?style text c] is [c] with the title [text], centered on the row
    above the plot, or on the chart's first row when the frame keeps no row
    above the plot. *)

val with_x_scale : Scale.t -> t -> t
(** [with_x_scale s c] is [c] with x scale [s]. *)

val with_y_scale : Scale.t -> t -> t
(** [with_y_scale s c] is [c] with y scale [s]. *)

val with_y2_scale : Scale.t -> t -> t
(** [with_y2_scale s c] is [c] with a second y axis of scale [s], against which
    [`Y2] marks are drawn. Its axis is drawn once {!with_y2_axis} gives one. *)

val with_axes : ?x:Axis.t -> ?y:Axis.t -> t -> t
(** [with_axes ?x ?y c] is [c] with x axis [x] and y axis [y]. An axis not given
    is kept. *)

val with_y2_axis : Axis.t -> t -> t
(** [with_y2_axis a c] is [c] with second y axis [a], drawn right of the plot
    when {!with_y2_scale} has given a scale. *)

val with_grid : Gridlines.t -> t -> t
(** [with_grid g c] is [c] with gridlines [g]. *)

val add : Mark.t -> t -> t
(** [add m c] is [c] with [m] drawn after its other marks, and so over them. *)

(** {2:mark_wrappers Adding marks}

    Each function below is {!add} of the {!Mark} constructor of the same name
    and arguments, so that a chart can be built in one pipeline. *)

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
(** [line ~x ~y data c] is [add (Mark.line ~x ~y data) c] ({!Mark.line}). *)

val line_gaps :
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
(** [line_gaps ~x ~y data c] is [add (Mark.line_gaps ~x ~y data) c]
    ({!Mark.line_gaps}). *)

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
(** [scatter ~x ~y data c] is [add (Mark.scatter ~x ~y data) c]
    ({!Mark.scatter}). *)

val bar :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?direction:Mark.direction ->
  ?mode:Mark.bar_mode ->
  category:('a -> string) ->
  value:('a -> float) ->
  'a array ->
  t ->
  t
(** [bar ~category ~value data c] is [add (Mark.bar ~category ~value data) c]
    ({!Mark.bar}). *)

val stacked_bar :
  ?id:Mark.id ->
  ?direction:Mark.direction ->
  ?gap:int ->
  ?size:int ->
  ?mode:Mark.bar_mode ->
  Mark.stacked_bar array ->
  t ->
  t
(** [stacked_bar data c] is [add (Mark.stacked_bar data) c]
    ({!Mark.val-stacked_bar}). *)

val rule :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  ?direction:Mark.direction ->
  ?pattern:Charset.line_pattern ->
  ?y_axis:Mark.y_axis_selector ->
  float ->
  t ->
  t
(** [rule v c] is [add (Mark.rule v) c] ({!Mark.rule}). *)

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
(** [heatmap ~x ~y ~value data c] is [add (Mark.heatmap ~x ~y ~value data) c]
    ({!Mark.heatmap}). *)

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
(** [candles data c] is [add (Mark.candles data) c] ({!Mark.candles}). *)

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
(** [circle ~cx ~cy ~r data c] is [add (Mark.circle ~cx ~cy ~r data) c]
    ({!Mark.circle}). *)

val shade :
  ?id:Mark.id -> ?style:Ansi.Style.t -> min:float -> max:float -> t -> t
(** [shade ~min ~max c] is [add (Mark.shade ~min ~max ()) c] ({!Mark.shade}). *)

val column_bg : ?id:Mark.id -> ?style:Ansi.Style.t -> float -> t -> t
(** [column_bg x c] is [add (Mark.column_bg x) c] ({!Mark.column_bg}). *)

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
(** [area ~x ~y data c] is [add (Mark.area ~x ~y data) c] ({!Mark.area}). *)

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
(** [fill_between ~x ~y_low ~y_high data c] is
    [add (Mark.fill_between ~x ~y_low ~y_high data) c] ({!Mark.fill_between}).
*)

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
(** [histogram ~x data c] is [add (Mark.histogram ~x data) c]
    ({!Mark.histogram}). *)

(** {1:rendering Drawing}

    A chart is drawn in a region of [width] by [height] cells, each at least 1,
    whose top-left cell is [(x, y)], [(0, 0)] by default. It shows the windows
    of [view], {!View.empty} by default. *)

val layout :
  ?view:View.t -> ?x:int -> ?y:int -> t -> width:int -> height:int -> Layout.t
(** [layout ?view ?x ?y c ~width ~height] is the layout {!draw} returns for the
    same arguments, computed without drawing.

    Raises [Invalid_argument] if a mark of [c] is drawn against [`Y2] and [c]
    has no second y scale ({!with_y2_scale}). *)

val draw :
  ?view:View.t ->
  ?x:int ->
  ?y:int ->
  t ->
  Matrix_grid.t ->
  width:int ->
  height:int ->
  Layout.t
(** [draw ?view ?x ?y c grid ~width ~height] draws [c] on [grid] and returns its
    layout. It fills the region with the theme's background, then draws the
    gridlines, the marks in order, the axes and the title.

    Raises [Invalid_argument] as {!val-layout} does. *)
