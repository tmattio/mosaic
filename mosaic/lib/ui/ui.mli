(** Mosaic UI * * A declarative, flexbox-based UI library for building terminal
    applications. *)

(** {1 Core Types} *)

type element
(** The abstract type for a UI element. *)

module Style : module type of Style
(** The type for defining a color style. See the [Style] module. *)

module Border : module type of Border
(** The type for defining element borders. See the [Border] module. *)

module Theme : module type of Theme
(** The type for theming and constants. See the [Theme] module. *)

module Spacing : module type of Spacing
(** Spacing utilities for padding and margin. *)

type spacing = Spacing.t
(** The type for defining spacing on the four sides of an element. *)

(** Defines how the element is positioned. *)
type position_type =
  | Relative  (** Positioned relative to its normal position. *)
  | Absolute  (** Positioned absolutely within its parent. *)

(** Defines how the element is displayed. *)
type display =
  | Block  (** Element is displayed as a block. *)
  | Flex  (** Element is displayed as a flex container. *)
  | Grid  (** Element is displayed as a grid container. *)
  | None  (** Element is not displayed (hidden). *)

(** Defines the layout direction for text and children. *)
type direction =
  | Inherit  (** Inherit direction from parent *)
  | Ltr  (** Left-to-right *)
  | Rtl  (** Right-to-left *)

(** Defines the main axis direction for flex containers. *)
type flex_direction =
  | Row  (** Horizontal from left to right *)
  | Column  (** Vertical from top to bottom *)
  | Row_reverse  (** Horizontal from right to left *)
  | Column_reverse  (** Vertical from bottom to top *)

(** Defines how content that exceeds bounds is handled. *)
type overflow =
  | Visible  (** Content is not clipped (Default) *)
  | Clip  (** Content is clipped at element bounds *)
  | Hidden  (** Content is clipped at element bounds *)
  | Scroll  (** Content is clipped but scrollable *)

(** Defines how to align items along the cross-axis of a container. *)
type align =
  | Start  (** Align items to the start of the container. *)
  | End  (** Align items to the end of the container. *)
  | Flex_start  (** Align items to the start of the container's cross axis. *)
  | Flex_end  (** Align items to the end of the container's cross axis. *)
  | Center  (** Align items to the center of the container's cross axis. *)
  | Baseline  (** Align items along their baseline *)
  | Stretch  (** Stretch items to fill the container's cross axis. *)

(** Defines how to distribute items along the main-axis of a container. *)
type justify =
  | Start  (** Pack items toward the start of the main axis. *)
  | End  (** Pack items toward the end of the main axis. *)
  | Flex_start  (** Pack items toward the start of the main axis. *)
  | Flex_end  (** Pack items toward the end of the main axis. *)
  | Center  (** Pack items toward the center of the main axis. *)
  | Stretch  (** Stretch items to fill the main axis. *)
  | Space_between  (** Distribute items evenly; first at start, last at end. *)
  | Space_evenly  (** Distribute items evenly with equal space around them. *)
  | Space_around
      (** Distribute items evenly with half-size spaces on either end. *)

(** Defines whether a container should wrap its children to new lines. *)
type wrap =
  | No_wrap  (** Items are laid out in a single line. (Default) *)
  | Wrap  (** Items wrap onto multiple lines if they overflow. *)
  | Wrap_reverse  (** Items wrap onto multiple lines in reverse order. *)

(** Defines the size of an element's dimension (width, height, etc.). * Sizes
    can be fixed, relative, or content-based. *)
type size =
  | Px of int  (** An exact size in terminal cells (pixels). *)
  | Percent of float
      (** A percentage of the parent's available size (0.0 to 100.0). *)
  | Auto  (** The size is determined by the layout engine. (Default) *)
  | Fit_content  (** The element's size is determined by its content. *)

(** {1 Layout Primitives} *)

val box :
  ?position_type:position_type ->
  ?display:display ->
  ?direction:direction ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?align_self:align ->
  ?width:size ->
  ?height:size ->
  ?min_width:size ->
  ?min_height:size ->
  ?max_width:size ->
  ?max_height:size ->
  ?padding:spacing ->
  ?margin:spacing ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  ?flex_direction:flex_direction ->
  ?align_items:align ->
  ?justify_content:justify ->
  ?flex_wrap:wrap ->
  ?gap:int ->
  ?row_gap:int ->
  ?col_gap:int ->
  ?overflow:overflow ->
  element list ->
  element
(** A foundational container element with extensive layout customization. It
    arranges a list of children according to the provided flexbox properties.

    @param children The list of elements to be placed inside this box.
    @param direction
      Sets the layout direction (Ltr or Rtl) for this container and its
      children.
    @param display Hides or shows the element and its children.

    --- Sizing ---
    @param width The width of the box.
    @param height The height of the box.
    @param min_width The minimum width.
    @param min_height The minimum height.
    @param max_width The maximum width.
    @param max_height The maximum height.

    --- Flex Container Properties (affect children) ---
    @param flex_direction
      The main axis for arranging children ([Row] or [Column]). Defaults to
      [Column].
    @param justify_content How to distribute children along the main axis.
    @param align_items How to align children along the cross axis.
    @param flex_wrap
      Whether children should wrap to the next line if they overflow.
    @param gap The space between each child, both horizontally and vertically.
    @param row_gap The vertical space between children in a wrapping layout.
    @param col_gap The horizontal space between children.

    --- Flex Item Properties (affect this box within its parent) ---
    @param flex_grow The factor by which this box should grow to fill space.
    @param flex_shrink
      The factor by which this box should shrink if there isn't enough space.
    @param align_self
      Overrides the parent's [align_items] for this specific box.

    --- Spacing ---
    @param padding
      The space between the box's border and its content (all sides).
    @param padding_x Horizontal padding (left and right).
    @param padding_y Vertical padding (top and bottom).
    @param margin The space outside the box's border (all sides).

    --- Positioning ---
    @param position_type
      Whether the element is positioned statically, relative to its normal flow,
      or absolutely.
    @param overflow What to do when content exceeds the box's bounds. *)

val vbox :
  ?width:size ->
  ?height:size ->
  ?min_width:size ->
  ?min_height:size ->
  ?max_width:size ->
  ?max_height:size ->
  ?padding:spacing ->
  ?margin:spacing ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?align_self:align ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  ?overflow:overflow ->
  ?align_items:align ->
  ?justify_content:justify ->
  ?gap:int ->
  element list ->
  element
(** Arranges elements vertically. Inherits all standard box-model parameters.

    @param align_items How to align children horizontally (on the cross-axis).
    @param justify_content
      How to distribute children vertically (on the main-axis).
    @param gap The vertical space in cells between each child element. *)

val hbox :
  ?width:size ->
  ?height:size ->
  ?min_width:size ->
  ?min_height:size ->
  ?max_width:size ->
  ?max_height:size ->
  ?padding:spacing ->
  ?margin:spacing ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?align_self:align ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  ?overflow:overflow ->
  ?align_items:align ->
  ?justify_content:justify ->
  ?gap:int ->
  element list ->
  element
(** Arranges elements horizontally. Inherits all standard box-model parameters.

    @param align_items How to align children vertically (on the cross-axis).
    @param justify_content
      How to distribute children horizontally (on the main-axis).
    @param gap The horizontal space in cells between each child element. *)

val zbox :
  ?width:size ->
  ?height:size ->
  ?min_width:size ->
  ?min_height:size ->
  ?max_width:size ->
  ?max_height:size ->
  ?padding:spacing ->
  ?margin:spacing ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?align_self:align ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  ?overflow:overflow ->
  element list ->
  element
(** Lays out elements on top of each other, from back to front. * Each child is
    stretched to fill the container by default. This function * inherits all
    standard box-model parameters but does not have child layout * parameters
    like [gap] or [align_items]. *)

val spacer :
  ?flex_grow:float -> ?min_width:size -> ?min_height:size -> unit -> element
(** Creates an invisible, flexible element that expands to fill available space.
    * Its primary customization is [flex_grow], but a minimum size can also be
    set. * It does not accept general styling, as it is intended to be
    invisible. *)

val divider :
  ?orientation:[ `Horizontal | `Vertical ] ->
  ?title:string ->
  ?char:string ->
  ?style:Style.t ->
  ?padding:spacing ->
  unit ->
  element
(** Renders a horizontal or vertical line to visually separate content.

    @param orientation The direction of the line. Defaults to [`Horizontal`].
    @param title
      An optional string to embed within the divider, e.g., "--- Title ---".
    @param style
      A style to apply to the divider's characters and title (e.g., color).
    @param padding
      Adds space around the divider, pushing it away from other content. *)

val text :
  ?style:Style.t ->
  ?align:[ `Left | `Center | `Right ] ->
  ?wrap:[ `Wrap | `Truncate | `Clip ] ->
  string ->
  element
(** Renders a block of text.

    @param style The color and text attributes (bold, italic, etc.).
    @param align The horizontal alignment of the text within its bounding box.
    @param wrap
      How to handle text that exceeds the element's width. Defaults to [`Wrap`].
    @param content The string to display. *)

val scroll_view :
  ?width:size ->
  ?height:size ->
  ?min_width:size ->
  ?min_height:size ->
  ?max_width:size ->
  ?max_height:size ->
  ?padding:spacing ->
  ?margin:spacing ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?align_self:align ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  ?show_scrollbars:bool ->
  h_offset:int ->
  v_offset:int ->
  element ->
  element
(** A container that provides a scrollable viewport for a single, larger child
    element.

    The application is responsible for storing the scroll offsets in its model
    and updating them via the [on_scroll] message. This keeps the UI fully
    state-driven.

    @param show_scrollbars
      If true (the default), visual scrollbars will be drawn inside the
      container's border/padding area.
    @param h_offset The current horizontal scroll position (column offset).
    @param v_offset The current vertical scroll position (row offset).
    @param on_scroll
      A function to create a message when the user scrolls (e.g., with arrow
      keys or a mouse wheel). The runtime provides the new offsets.
    @param child The single element to be displayed within the scroll view. *)

val center : element -> element
(** A container that centers its child both horizontally and vertically. *)

val styled : Style.t -> element -> element
(** Apply a style to an existing UI element. The style will be applied to a
    wrapper box around the element. *)

val flow : ?h_gap:int -> ?v_gap:int -> element list -> element
(** [flow ?h_gap ?v_gap children] creates a flow layout that wraps children
    horizontally to new lines when they don't fit in the available width.
    Similar to how inline elements work in HTML.
    @param h_gap Horizontal spacing between items (default: 0)
    @param v_gap Vertical spacing between lines (default: 0) *)

val block :
  ?width:size ->
  ?height:size ->
  ?min_width:size ->
  ?min_height:size ->
  ?max_width:size ->
  ?max_height:size ->
  ?padding:spacing ->
  ?margin:spacing ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  element list ->
  element
(** [block children] creates a block layout where each child takes the full
    width of its container and children are stacked vertically. This is the
    traditional block-level element behavior. *)

val grid :
  columns:size list ->
  rows:size list ->
  ?col_gap:int ->
  ?row_gap:int ->
  element list ->
  element
(** [grid ~columns ~rows children] creates a CSS Grid layout with specified
    column and row track sizes. Children are placed automatically in grid cells
    from left-to-right, top-to-bottom.

    @param columns
      List of column track sizes (e.g., [Px 100; Percent 50.; Auto])
    @param rows List of row track sizes
    @param col_gap Spacing between columns (default: 0)
    @param row_gap Spacing between rows (default: 0) *)

val checkbox : checked:bool -> label:string -> ?style:Style.t -> unit -> element
(** [checkbox ~checked ~label ?style ()] creates a checkbox UI element.

    @param checked Whether the checkbox is checked
    @param label The text label to display next to the checkbox
    @param style Optional style to apply to the checkbox *)

val radio : checked:bool -> label:string -> ?style:Style.t -> unit -> element
(** [radio ~checked ~label ?style ()] creates a radio button UI element.

    @param checked Whether the radio button is selected
    @param label The text label to display next to the radio button
    @param style Optional style to apply to the radio button *)

val image :
  lines:string list ->
  ?align:[< `Left | `Center | `Right > `Left ] ->
  unit ->
  element
(** [image ~lines ?align ()] creates an image element from ASCII art lines.

    @param lines List of strings representing each line of the ASCII art
    @param align Horizontal alignment of the image lines *)

val list :
  items:element list -> ?bullet:string -> ?numbering:bool -> unit -> element
(** [list ~items ?bullet ?numbering ()] creates a bulleted or numbered list.

    @param items List of UI elements to display as list items
    @param bullet Custom bullet character (default: "•")
    @param numbering If true, use numbers instead of bullets *)

val rich_text : (string * Style.t) list -> element
(** [rich_text segments] creates a single line of text with multiple styled
    segments. Each segment is a pair of text content and its associated style.
*)

(** {1 String utilities} *)

val measure_string : string -> int
(** Returns the display width of a string, accounting for UTF-8 characters. *)

val truncate_string_with_ellipsis : string -> int -> string -> string
(** [truncate_string_with_ellipsis str max_width suffix] truncates [str] to fit
    within [max_width] characters, appending [suffix] (typically "...") if
    truncation occurs. *)

val pad_string : string -> int -> string
(** [pad_string str width] pads [str] with spaces to reach [width] characters.
*)

val measure : ?width:int -> element -> int * int
(** [measure ?width element] computes the natural dimensions of an element.
    @param width Optional width constraint for the layout calculation
    @return A pair (width, height) representing the element's dimensions *)

(** {1 Extended UI Components} *)

(** {3 Canvas} *)

(** Canvas module for primitive drawing operations. *)
module Canvas : sig
  type t

  val plot : t -> x:int -> y:int -> ?style:Style.t -> string -> unit

  val draw_line :
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    ?style:Style.t ->
    ?kind:[ `Line | `Braille ] ->
    t ->
    unit

  val draw_box :
    x:int ->
    y:int ->
    width:int ->
    height:int ->
    ?style:Style.t ->
    ?border:Border.t ->
    t ->
    unit

  val create :
    ?width:size ->
    ?height:size ->
    ?min_width:size ->
    ?min_height:size ->
    ?max_width:size ->
    ?max_height:size ->
    ?padding:spacing ->
    ?margin:spacing ->
    ?flex_grow:float ->
    ?flex_shrink:float ->
    ?align_self:align ->
    ?style:Style.t ->
    ?border:Border.t ->
    ?border_style:Style.t ->
    (t -> unit) ->
    element
end

val canvas :
  ?width:size ->
  ?height:size ->
  ?min_width:size ->
  ?min_height:size ->
  ?max_width:size ->
  ?max_height:size ->
  ?padding:spacing ->
  ?margin:spacing ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?align_self:align ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  ((x:int -> y:int -> ?style:Style.t -> string -> unit) -> unit) ->
  element
(** A self-contained element for custom, imperative drawing.

    It combines the layout properties of a [box] with a low-level drawing
    function. This is the ideal escape hatch for custom visualizations like
    charts, sparklines, or game boards.

    While its size can be determined by flexbox (e.g., using [flex_grow]), it's
    often best to provide an explicit size like [width:(Px 40)] for predictable
    results.

    @param draw
      A function that receives the final, calculated [width] and [height] of the
      canvas's content area (inside any padding or border) and a [plot] function
      to draw with. This allows your drawing logic to be responsive to the
      canvas's actual size. *)

module Progress_bar : sig
  (** Progress bar rendering with animation and rich customization.

      This module provides a customizable progress bar similar to Python's Rich
      library, with support for determinate progress, indeterminate pulsing
      animations, and various styling options. *)

  type preset_def = {
    delimiters : (string * string) option;
    filled_char : string;
    empty_char : string;
    progress_stages : string list;
  }
  (** A record defining the characters used to render a progress bar. *)

  (** A collection of pre-defined visual styles for the progress bar. *)
  type preset =
    (* Classic Styles *)
    | ASCII  (** A standard ASCII bar: `[###---]` *)
    | UTF8  (** A high-resolution UTF-8 block bar: `│███▋ │` *)
    (* Line Styles *)
    | Line_double  (** Double-line characters: `╢═══───╟` *)
    | Line_single  (** Single-line characters: `├━━━───┤` *)
    | Line_arrow  (** Arrowhead progress: `>>--->` *)
    (* Block Styles *)
    | Block_shade_light  (** Light shading blocks: `▓▓▓░░░` *)
    | Block_shade_medium  (** Medium shading blocks: `███▒▒▒` *)
    | Block_shade_dark  (** Dark shading blocks: `███▓▓▓` *)
    | Block_dotted  (** Dotted blocks: `⣿⣿⣿⣀⣀⣀` *)
    (* Custom record for full control *)
    | Custom of preset_def

  val progress_bar :
    ?total:float ->
    ?completed:float ->
    ?width:int ->
    ?pulse:bool ->
    ?animation_time:float ->
    ?bar_style:Style.t ->
    ?complete_style:Style.t ->
    ?finished_style:Style.t ->
    ?pulse_style:Style.t ->
    ?preset:preset ->
    ?delimiters:string * string ->
    ?filled_char:string ->
    ?empty_char:string ->
    ?progress_stages:string list ->
    unit ->
    element
  (** [progress_bar ?total ... ()] creates a progress bar element.

      @param total
        The total number of steps. Defaults to `Some 100.0`. Set to `None` for a
        pulsing bar.
      @param completed The number of steps completed. Defaults to `0.0`.
      @param width The total width of the bar in characters. Defaults to 20.
      @param pulse
        If true, force a pulsing animation even if `total` is defined. Defaults
        to `false`.
      @param animation_time
        Time in seconds, used for the pulsing animation. Defaults to `0.0`.

      @param bar_style Style for the empty part of the bar.
      @param complete_style Style for the filled part of the bar.
      @param finished_style
        Style for the bar once it is complete. Overrides `complete_style`.
      @param pulse_style Style for the bright part of the pulsing animation.

      @param preset
        A style preset for the bar's characters. If not provided, a default
        heavy-line style (`━━━━━━━━╸───`) is used. Specific character arguments
        below will override the chosen preset or the default.
      @param delimiters
        A `(left, right)` tuple of strings for the bar's ends. E.g., `Some
        ("[", "]")`.
      @param filled_char
        The character for a fully filled segment. E.g., `"#"` or `"█"`.
      @param empty_char
        The character for an empty segment. E.g., `"-"` or `" "`.
      @param progress_stages
        A list of characters for sub-character progress, from least to most
        filled. E.g., `[">"]` or `["▏", "▎", "▍", "▌", "▋", "▊", "▉"]`. *)
end

module Panel : sig
  (** Panel element with Rich-like API for drawing borders around content *)

  type align = [ `Left | `Center | `Right ]
  (** Alignment options for title and subtitle *)

  val panel :
    ?box_style:Border.line_style ->
    ?title:string ->
    ?title_align:align ->
    ?subtitle:string ->
    ?subtitle_align:align ->
    ?expand:bool ->
    ?style:Style.t ->
    ?border_style:Style.t ->
    ?width:int ->
    ?height:int ->
    ?padding:Spacing.t ->
    ?highlight:bool ->
    element ->
    element
  (** Create a panel with a border around its contents.

      @param child The content to display inside the panel
      @param box_style The border style (default: Rounded)
      @param title Optional title displayed in panel header
      @param title_align Alignment of title (default: Center)
      @param subtitle Optional subtitle displayed in panel footer
      @param subtitle_align Alignment of subtitle (default: Center)
      @param expand
        If true, panel stretches to fill available width (default: true)
      @param style The style of the panel contents
      @param border_style The style of the border
      @param width Optional fixed width of panel
      @param height Optional fixed height of panel
      @param padding Padding around content (top, right, bottom, left)
      @param highlight Enable automatic highlighting of title/subtitle if string
  *)
end

(** Spinner animations for terminal UI.

    Spinners are from cli-spinners: MIT License Copyright (c) Sindre Sorhus
    <sindresorhus@gmail.com> (sindresorhus.com) *)
module Spinner : sig
  type spinner_kind =
    (* Braille patterns *)
    | Braille_dots  (** Classic rotating braille dots: ⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏ *)
    | Braille_dots2  (** Alternative braille dots: ⣾⣽⣻⢿⡿⣟⣯⣷ *)
    | Braille_dots3  (** Third braille variation: ⠋⠙⠚⠞⠖⠦⠴⠲⠳⠓ *)
    | Braille_dots4  (** Bouncing braille dots *)
    | Braille_dots5  (** Complex braille cycle *)
    | Braille_dots6  (** Extended braille animation *)
    | Braille_dots7  (** Another braille pattern *)
    | Braille_dots8  (** Long braille sequence *)
    | Braille_dots9  (** Vertical braille: ⢹⢺⢼⣸⣇⡧⡗⡏ *)
    | Braille_dots10  (** Small braille set: ⢄⢂⢁⡁⡈⡐⡠ *)
    | Braille_dots11  (** Simple braille cycle *)
    | Braille_dots12  (** Two-column braille animation *)
    | Braille_dots13  (** Dense braille: ⣼⣹⢻⠿⡟⣏⣧⣶ *)
    | Braille_dots14  (** Two-character braille patterns *)
    | Braille_8bit  (** Full 8-bit braille character set *)
    | Braille_circle  (** Circular braille motion *)
    | Braille_sand  (** Sand falling effect with braille *)
    (* Line and ASCII *)
    | Line_spin  (** Simple line rotation: - \ | / *)
    | Line_pulse  (** Pulsing line: ⠂-–—–- *)
    | Pipe_spin  (** Box drawing rotation: ┤┘┴└├┌┬┐ *)
    | Ascii_dots  (** Simple ASCII dots: . .. ... *)
    | Ascii_dots_scroll  (** Scrolling ASCII dots *)
    | Ascii_star  (** ASCII star rotation: + x * *)
    | Ascii_flip  (** Flipping characters: _ - ` ' ´ *)
    | Ascii_hamburger  (** Hamburger menu: ☱☲☴ *)
    | Ascii_binary  (** Binary numbers animation *)
    | Ascii_dqpb  (** Letters d q p b rotation *)
    (* Bars and blocks *)
    | Bar_vertical_grow  (** Growing vertical bar: ▁▃▄▅▆▇ *)
    | Bar_horizontal_grow  (** Growing horizontal bar: ▏▎▍▌▋▊▉ *)
    | Bar_bounce  (** Bouncing bar: [=   ] [==  ] *)
    | Block_bounce  (** Bouncing block corners: ▖▘▝▗ *)
    | Block_wave  (** Wave with blocks: ▌▀▐▄ *)
    | Block_square  (** Square toggle: □■ *)
    | Block_squish  (** Squishing blocks: ╫╪ *)
    (* Geometric shapes *)
    | Triangle_spin  (** Rotating triangle: ◢◣◤◥ *)
    | Square_corners  (** Rotating square corners: ◰◳◲◱ *)
    | Circle_quarters  (** Circle quarters: ◴◷◶◵ *)
    | Circle_halves  (** Circle halves: ◐◓◑◒ *)
    | Circle_simple  (** Simple circle: ◡⊙◠ *)
    | Arc_spin  (** Arc rotation: ◜◠◝◞◡◟ *)
    (* Progress indicators *)
    | Progress_bar  (** Material design progress bar *)
    | Progress_balloon  (** Balloon expansion: . o O @ * *)
    | Progress_balloon2  (** Alternative balloon: . o O ° *)
    | Progress_layer  (** Layered progress: - = ≡ *)
    | Progress_point  (** Moving point: ∙∙∙ ●∙∙ ∙●∙ *)
    | Progress_beta_wave  (** Beta wave: ρββββββ *)
    (* Animations *)
    | Anim_pong  (** Pong game animation *)
    | Anim_shark  (** Swimming shark *)
    | Anim_grenade  (** Exploding grenade *)
    | Anim_ball_bounce  (** Bouncing ball in parentheses *)
    | Anim_aesthetic  (** Aesthetic wave: ▰▱▱▱▱▱▱ *)
    | Anim_dwarf_fortress  (** Dwarf Fortress style animation *)
    (* Noise and effects *)
    | Noise_fade  (** Fading noise: ▓▒░ *)
    | Effect_dots_bounce  (** Simple bouncing dots: ⠁⠂⠄⠂ *)
    (* Toggle animations *)
    | Toggle_box  (** Box toggle: ▫▪ *)
    | Toggle_box2  (** Alternative box: ⊶⊷ *)
    | Toggle_square  (** Square toggle: □■ *)
    | Toggle_square2  (** Multi-state square: ■□▪▫ *)
    | Toggle_square3  (** Bold square: ▮▯ *)
    | Toggle_circle  (** Circle toggle: ဝ၀ *)
    | Toggle_circle2  (** Filled circles: ⦾⦿ *)
    | Toggle_circle3  (** Dotted circles: ◍◌ *)
    | Toggle_circle4  (** Bold circles: ◉◎ *)
    | Toggle_circle5  (** Numbered circles: ㊂㊀㊁ *)
    | Toggle_diamond  (** Diamond toggle: ⧇⧆ *)
    | Toggle_shogi  (** Shogi pieces: ☗☖ *)
    | Toggle_equals  (** Equals toggle: = * - *)
    (* Arrows *)
    | Arrow_rotate  (** Rotating arrows: ←↖↑↗→↘↓↙ *)
    | Arrow_rotate2  (** Emoji arrows: ⬆️↗️➡️↘️⬇️↙️⬅️↖️ *)
    | Arrow_progress  (** Arrow progress: ▹▹▹▹▹ ▸▹▹▹▹ *)
    (* Unicode and emoji *)
    | Unicode_star_pulse  (** Star pulse: ✶✸✹✺✹✷ *)
    | Unicode_moon_phases  (** Moon phases: 🌑🌒🌓🌔🌕🌖🌗🌘 *)
    | Unicode_earth_rotate  (** Earth rotation: 🌍🌎🌏 *)
    | Unicode_clock  (** Clock faces: 🕛🕐🕑... *)
    | Unicode_weather  (** Weather cycle: ☀️🌤⛅️🌥☁️🌧 *)
    (* Emoji animations *)
    | Emoji_hearts  (** Colored hearts: 💛💙💜💚❤️ *)
    | Emoji_monkey  (** See/hear/speak no evil: 🙈🙉🙊 *)
    | Emoji_faces  (** Face expressions: 😄😝 *)
    | Emoji_runner  (** Running figure: 🚶🏃 *)
    | Emoji_christmas  (** Christmas trees: 🌲🎄 *)
    | Emoji_finger_dance  (** Hand gestures: 🤘🤟🖖✋🤚👆 *)
    | Emoji_fist_bump  (** Fist bump animation *)
    | Emoji_soccer  (** Soccer header animation *)
    | Emoji_mindblown  (** Mind blown sequence *)
    | Emoji_speaker  (** Speaker volume: 🔈🔉🔊 *)
    (* Pulse animations *)
    | Pulse_orange  (** Orange pulse: 🔸🔶🟠 *)
    | Pulse_blue  (** Blue pulse: 🔹🔷🔵 *)
    | Pulse_orange_blue  (** Combined orange/blue pulse *)
    (* Special *)
    | Time_travel  (** Clock time travel effect *)
    (* Custom *)
    | Custom of { frames : string list; interval : int }
        (** Custom animation frames *)

  val spinner : ?speed:float -> ?time:float -> spinner_kind -> element
end

module Table : sig
  (** Table element with Rich-like API *)

  type justify = [ `Left | `Center | `Right | `Full ]
  (** Justification options for cell content *)

  type vertical_align = [ `Top | `Middle | `Bottom ]
  (** Vertical alignment options *)

  type overflow = [ `Ellipsis | `Crop | `Fold ]
  (** Overflow handling options *)

  type column = {
    header : string;
    footer : string option;
    header_style : Style.t;
    footer_style : Style.t;
    style : Style.t;
    justify : justify;
    vertical : vertical_align;
    overflow : overflow;
    width : int option;
    min_width : int option;
    max_width : int option;
    ratio : int option;
    no_wrap : bool;
  }
  (** Column configuration *)

  type padding = int * int * int * int
  (** Padding specification (top, right, bottom, left) *)

  (** Box drawing styles *)
  type box_style =
    | NoBox
    | Simple
    | Rounded
    | Heavy
    | HeavyHead
    | Double
    | DoubleEdge
    | Ascii
    | MinimalHeavyHead
    | MinimalDoubleHead
    | Minimal
    | Square
    | SquareDoubleHead

  val default_column : header:string -> column
  (** Create a default column configuration *)

  val table :
    ?title:string option ->
    ?caption:string option ->
    ?columns:column list ->
    ?rows:string list list ->
    ?box_style:box_style ->
    ?safe_box:bool ->
    ?padding:padding ->
    ?collapse_padding:bool ->
    ?pad_edge:bool ->
    ?expand:bool ->
    ?show_header:bool ->
    ?show_footer:bool ->
    ?show_edge:bool ->
    ?show_lines:bool ->
    ?leading:int ->
    ?style:Style.t ->
    ?row_styles:Style.t list ->
    ?header_style:Style.t ->
    ?footer_style:Style.t ->
    ?border_style:Style.t ->
    ?title_style:Style.t ->
    ?caption_style:Style.t ->
    ?title_justify:justify ->
    ?caption_justify:justify ->
    ?width:int option ->
    ?min_width:int option ->
    unit ->
    element
  (** Main table function with Rich-like API

      @param title Optional title displayed above the table
      @param caption Optional caption displayed below the table
      @param columns Column configurations
      @param rows Table data as list of string lists
      @param box_style Box drawing style (default: HeavyHead)
      @param safe_box Use safe box characters for compatibility
      @param padding Cell padding (default: 0,1,0,1)
      @param collapse_padding Collapse padding between cells
      @param pad_edge Add padding to edge cells
      @param expand Expand table to fill available width
      @param show_header Show header row (default: true)
      @param show_footer Show footer row (default: false)
      @param show_edge Show table border (default: true)
      @param show_lines Show lines between rows (default: false)
      @param leading Blank lines between rows (default: 0)
      @param style Default table style
      @param row_styles List of styles to alternate between rows
      @param header_style Header style (default: bold)
      @param footer_style Footer style
      @param border_style Border style
      @param title_style Title style
      @param caption_style Caption style
      @param title_justify Title justification (default: center)
      @param caption_justify Caption justification (default: center)
      @param width Fixed table width
      @param min_width Minimum table width *)

  val simple_table : headers:string list -> rows:string list list -> element
  (** Simple table with just headers and rows *)

  val grid_table : columns:column list -> rows:string list list -> element
  (** Grid-style table (no borders, minimal padding) *)
end

module Tree : sig
  type guide_style =
    | Normal  (** Standard lines: ├── └── │ *)
    | ASCII  (** ASCII-safe: +-- `-- | *)
    | Bold  (** Bold lines: ┣━━ ┗━━ ┃ *)
    | Double  (** Double lines: ╠══ ╚══ ║ *)

  type node = {
    label : element;
    expanded : bool;
    children : node list;
    guide_style : Style.t option;
        (** Optional style override for this node's guides *)
  }
  (** [node] represents a node in a tree structure.
    @field label The element to display for this node
    @field expanded Whether this node's children are shown (for display purposes only)
    @field children List of child nodes
    @field guide_style Optional style override for guide lines of this node *)

  val tree :
    ?style:Style.t ->
    ?guide_style:Style.t ->
    ?guides:guide_style ->
    ?hide_root:bool ->
    ?expanded:bool ->
    node ->
    element
  (** [tree ?style ?guide_style ?guides ?hide_root ?expanded node] creates a
      tree view display.
      @param style Style to apply to the tree labels
      @param guide_style Style to apply to the tree guide lines (default: gray)
      @param guides Which guide line style to use (default: Normal)
      @param hide_root Whether to hide the root node (default: false)
      @param expanded
        Override expanded state for all nodes (default: use node.expanded)

      Example rendering:
      {[
        ├─ Parent
        │  ├─ Child 1
        │  └─ Child 2
        └─ Parent 2
      ]} *)
end

val panel :
  ?box_style:Border.line_style ->
  ?title:string ->
  ?title_align:[ `Left | `Center | `Right ] ->
  ?subtitle:string ->
  ?subtitle_align:[ `Left | `Center | `Right ] ->
  ?expand:bool ->
  ?style:Style.t ->
  ?border_style:Style.t ->
  ?width:int ->
  ?height:int ->
  ?padding:spacing ->
  ?highlight:bool ->
  element ->
  element
(** Creates a panel with border and optional title/subtitle *)

val progress_bar :
  ?total:float ->
  ?completed:float ->
  ?width:int ->
  ?pulse:bool ->
  ?animation_time:float ->
  ?bar_style:Style.t ->
  ?complete_style:Style.t ->
  ?finished_style:Style.t ->
  ?pulse_style:Style.t ->
  ?preset:Progress_bar.preset ->
  ?delimiters:string * string ->
  ?filled_char:string ->
  ?empty_char:string ->
  ?progress_stages:string list ->
  unit ->
  element
(** Creates a progress bar with extensive customization options *)

val spinner : ?speed:float -> ?time:float -> Spinner.spinner_kind -> element
(** Creates an animated spinner with predefined animation styles *)

val table :
  ?title:string option ->
  ?caption:string option ->
  ?columns:Table.column list ->
  ?rows:string list list ->
  ?box_style:Table.box_style ->
  ?safe_box:bool ->
  ?padding:Table.padding ->
  ?collapse_padding:bool ->
  ?pad_edge:bool ->
  ?expand:bool ->
  ?show_header:bool ->
  ?show_footer:bool ->
  ?show_edge:bool ->
  ?show_lines:bool ->
  ?leading:int ->
  ?style:Style.t ->
  ?row_styles:Style.t list ->
  ?header_style:Style.t ->
  ?footer_style:Style.t ->
  ?border_style:Style.t ->
  ?title_style:Style.t ->
  ?caption_style:Style.t ->
  ?title_justify:Table.justify ->
  ?caption_justify:Table.justify ->
  ?width:int option ->
  ?min_width:int option ->
  unit ->
  element
(** Creates a table with customizable layout and styling *)

val tree :
  ?style:Style.t ->
  ?guide_style:Style.t ->
  ?guides:Tree.guide_style ->
  ?hide_root:bool ->
  ?expanded:bool ->
  Tree.node ->
  element
(** Creates a tree view with expandable nodes *)

(** {1 Rendering} *)

val render : ?dark:bool -> ?theme:Theme.t -> Screen.t -> element -> unit
(** Renders a UI tree to an in-memory [Screen.t] buffer.

    @param dark Toggles between light and dark mode for adaptive styles.
    @param theme An optional theme to apply to the UI.
    @param screen The target screen buffer to render to.
    @param ui The root UI element to render. *)

val render_string :
  ?width:int -> ?height:int -> ?dark:bool -> ?theme:Theme.t -> element -> string
(** Renders a UI element to a string, suitable for terminal output.

    @param width The width of the virtual screen to create.
    @param height
      The height of the virtual screen. If not provided, it will be auto-sized
      based on the content's measured height.
    @param dark Toggles between light and dark mode for adaptive styles.
    @param theme An optional theme to apply to the UI.
    @param element The UI element to render. *)

val print :
  ?width:int -> ?height:int -> ?dark:bool -> ?theme:Theme.t -> element -> unit
(** A convenience function that creates a screen, renders the UI, and prints the
    result to standard output.

    @param width The width of the virtual screen to create.
    @param height
      The height of the virtual screen. If not provided, it will be auto-sized
      based on the content's measured height. *)
