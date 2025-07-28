(** A High-Level Declarative Terminal UI Library.

    This module is the main entry point for the UI library. It provides all the
    necessary tools to build, lay out, and render a terminal user interface
    using a declarative, functional approach.

    Example Usage:
    {[
      let my_ui =
        Ui.vbox ~gap:1
          [
            Ui.text ~style:Style.(fg red ++ bold) "Hello, World!";
            Ui.divider ();
            Ui.hbox ~gap:2
              [ Ui.text "Item 1"; Ui.flex_spacer (); Ui.text "Item 2" ];
          ]

      let () =
        let buffer = Render.create 80 24 in
        Ui.render buffer my_ui;
        Render.to_string buffer |> print_endline
    ]} *)

module Style = Style
(** @inline *)

module Theme = Theme
(** @inline *)

type element
(** The abstract type for any UI element. Use the constructor functions below
    (e.g., [text], [hbox], [vbox]) to create values of this type. *)

(** {2 Padding} *)

type padding
(** [padding] specifies spacing between an element's border and its content.
    Values are non-negative integers representing character cells. *)

val padding :
  ?top:int -> ?right:int -> ?bottom:int -> ?left:int -> unit -> padding
(** [padding ?top ?right ?bottom ?left ()] creates padding with individual side
    control. All values default to 0. *)

val pad :
  ?all:int ->
  ?x:int ->
  ?y:int ->
  ?top:int ->
  ?right:int ->
  ?bottom:int ->
  ?left:int ->
  unit ->
  padding
(** [pad ?all ?x ?y ?top ?right ?bottom ?left ()] creates padding with shorthand
    controls. Parameters cascade from general ([all]) to specific ([top], etc.).
*)

val padding_all : int -> padding
(** [padding_all n] creates uniform padding of [n] cells on all sides. *)

val padding_xy : int -> int -> padding
(** [padding_xy x y] creates padding with [x] cells horizontally and [y] cells
    vertically. *)

(** {2 Border} *)

(** [border_style] determines the characters used for drawing borders. *)
type border_style =
  | Solid  (** Single-line box drawing characters (┌─┐). *)
  | Rounded  (** Single-line with rounded corners (╭─╮). *)
  | Double  (** Double-line box drawing characters (╔═╗). *)
  | Thick  (** Heavy-line box drawing characters (┏━┓). *)
  | ASCII  (** Portable characters for maximum compatibility (+-+|). *)

type border
(** [border] specifies visual border properties for elements, with per-side
    control. *)

val border :
  ?top:bool ->
  ?bottom:bool ->
  ?left:bool ->
  ?right:bool ->
  ?style:border_style ->
  ?color:Ansi.color ->
  unit ->
  border
(** [border ?top ?bottom ?left ?right ?style ?color ()] creates a border
    specification. All sides default to [true] and style to [Solid]. *)

val normal_border : border
(** Pre-defined solid border on all sides. *)

val rounded_border : border
(** Pre-defined rounded border on all sides. *)

val double_border : border
(** Pre-defined double-line border on all sides. *)

val thick_border : border
(** Pre-defined thick-line border on all sides. *)

val ascii_border : border
(** Pre-defined ASCII-compatible border on all sides. *)

(** {2 Alignement} *)

type align =
  [ `Start  (** Align to the beginning (left/top). *)
  | `Center  (** Position at the midpoint. *)
  | `End  (** Align to the end (right/bottom). *)
  | `Stretch  (** Expand to fill available space on the cross-axis. *) ]
(** [align] controls element positioning and sizing within available space. *)

(* ** {2 Size Definitions} *)

type size_def =
  [ `Fixed of int  (** A fixed size in character cells. *)
  | `Flex of int  (** A flexible weight for distributing remaining space. *) ]
(** [size_def] specifies how a grid column or row should size itself. *)

(** {2 Constructor Functions} *)

val text :
  ?style:Style.t ->
  ?align:align ->
  ?tab_width:int ->
  ?wrap:bool ->
  string ->
  element
(** A simple text element. *)

val hbox :
  ?gap:int ->
  ?width:int ->
  ?height:int ->
  ?min_width:int ->
  ?min_height:int ->
  ?max_width:int ->
  ?max_height:int ->
  ?margin:padding ->
  ?padding:padding ->
  ?border:border ->
  ?background:Style.t ->
  ?align_items:align ->
  ?justify_content:align ->
  ?flex_grow:int ->
  ?flex_shrink:int ->
  ?fill:bool ->
  ?wrap:bool ->
  element list ->
  element
(** A horizontal container for laying out child elements from left to right. *)

val vbox :
  ?gap:int ->
  ?width:int ->
  ?height:int ->
  ?min_width:int ->
  ?min_height:int ->
  ?max_width:int ->
  ?max_height:int ->
  ?margin:padding ->
  ?padding:padding ->
  ?border:border ->
  ?background:Style.t ->
  ?align_items:align ->
  ?justify_content:align ->
  ?flex_grow:int ->
  ?flex_shrink:int ->
  ?fill:bool ->
  element list ->
  element
(** A vertical container for laying out child elements from top to bottom. *)

val spacer : ?flex:int -> int -> element
(** An empty, transparent element used for spacing. *)

val rich_text : (string * Style.t) list -> element
(** A single line of text composed of multiple styled segments. *)

val z_stack : ?align:Element.Z_stack.z_align -> element list -> element
(** A container that layers its children on top of one another. *)

val flow : ?h_gap:int -> ?v_gap:int -> element list -> element
(** A container that lays out children horizontally, wrapping to new lines as
    needed. *)

val grid :
  ?col_spacing:int ->
  ?row_spacing:int ->
  columns:size_def list ->
  rows:size_def list ->
  element list ->
  element
(** A container that arranges children in a grid with fixed and flexible sizing.
*)

val scroll :
  ?width:int ->
  ?height:int ->
  ?h_offset:int ->
  ?v_offset:int ->
  element ->
  element
(** A container that provides a scrollable viewport for a single child element.
*)

(** {2 Helper Elements} *)

val flex_spacer : unit -> element
(** An expandable spacer that fills all available space along a container's main
    axis. *)

val divider : ?style:Style.t -> ?char:string -> unit -> element
(** A horizontal line that expands to fill the available width of its container.
*)

val center : element -> element
(** A convenience container that centers its child both horizontally and
    vertically. *)

val styled : Style.t -> element -> element
(** A convenience container that wraps a child to apply styling. *)

(** {2 Additional UI Primitives} *)

val checkbox : checked:bool -> label:string -> ?style:Style.t -> unit -> element
(** [checkbox ~checked ~label ?style ()] creates a static checkbox visual.
    Renders as "[x] label" when checked or "[ ] label" when unchecked. *)

val radio : checked:bool -> label:string -> ?style:Style.t -> unit -> element
(** [radio ~checked ~label ?style ()] creates a static radio button visual.
    Renders as "(o) label" when checked or "( ) label" when unchecked. *)

val image : lines:string list -> ?align:align -> unit -> element
(** [image ~lines ?align ()] creates an element for rendering multi-line ASCII
    art or logos. Each line is rendered as-is, with wrapping/clipping handled
    like text. *)

val separator :
  ?orientation:[ `Horizontal | `Vertical ] ->
  ?char:string ->
  ?style:Style.t ->
  unit ->
  element
(** [separator ?orientation ?char ?style ()] creates a separator line.
    @param orientation Direction of separator (default: `Horizontal)
    @param char
      Character to use for the line (default: "─" for horizontal, "│" for
      vertical)
    @param style Style to apply to the separator *)

val list :
  items:element list -> ?bullet:string -> ?numbering:bool -> unit -> element
(** [list ~items ?bullet ?numbering ()] creates a vertical list with optional
    bullets or numbers.
    @param items List of elements to display
    @param bullet Custom bullet character (default: "•")
    @param numbering If true, use numbers instead of bullets *)

(** {3 Spinner} *)

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
(** [spinner ?speed ?time kind] creates an animated spinner element.

    @param speed Animation speed multiplier. Defaults to 1.0.
    @param time Time in seconds for the animation frame. Defaults to 0.0. *)

(** {3 Progress Bar} *)

type progress_bar_preset_def = {
  delimiters : (string * string) option;
  filled_char : string;
  empty_char : string;
  progress_stages : string list;
}

type progress_bar_preset =
  | ASCII
  | UTF8
  | Line_double
  | Line_single
  | Line_arrow
  | Block_shade_light
  | Block_shade_medium
  | Block_shade_dark
  | Block_dotted
  | Custom of progress_bar_preset_def

val progress_bar :
  ?total:float option ->
  ?completed:float ->
  ?width:int ->
  ?pulse:bool ->
  ?animation_time:float ->
  ?bar_style:Style.t ->
  ?complete_style:Style.t ->
  ?finished_style:Style.t ->
  ?pulse_style:Style.t ->
  ?preset:progress_bar_preset ->
  ?delimiters:(string * string) option ->
  ?filled_char:string ->
  ?empty_char:string ->
  ?progress_stages:string list ->
  unit ->
  element
(** [progress_bar ?total ?completed ?width ?pulse ?bar_style ?complete_style
     ?finished_style ?pulse_style ?animation_time ()] creates a progress bar
    element.

    @param total
      Number of steps in the bar. Defaults to Some 100.0. Set to None to render
      a pulsing animation.
    @param completed Number of steps completed. Defaults to 0.0.
    @param width Width of the bar in characters. Defaults to 20.
    @param pulse
      Enable pulse effect. Defaults to false. Will pulse if a None total was
      passed.
    @param bar_style Style for the bar background. Defaults to empty style.
    @param complete_style Style for the completed bar. Defaults to green.
    @param finished_style Style for a finished bar. Defaults to bright green.
    @param pulse_style Style for pulsing bars. Defaults to cyan.
    @param animation_time
      Time in seconds to use for animation. Defaults to 0.0 for pulsing
      animations. *)

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
    ?border:border ->
    t ->
    unit

  val create : ?width:int -> ?height:int -> (t -> unit) -> element
end

val canvas : ?width:int -> ?height:int -> (Canvas.t -> unit) -> element
(** [canvas ?width ?height f] creates a canvas element with specified
    dimensions. The function [f] is called with the canvas to perform drawing
    operations. *)

(** {3 Tables} *)

(** Table module with Rich-like API for advanced table creation. Provides column
    configuration, styling, borders, and more. *)
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

val table :
  rows:string list list ->
  ?headers:string list ->
  ?col_styles:Style.t list ->
  ?border:border ->
  unit ->
  element
(** Alias for {!Table.table}. Creates a table with automatic column sizing. For
    simple tables with headers and basic styling. For advanced features like
    column configuration, padding, and rich borders, use the Table module
    directly.

    @param rows List of rows, where each row is a list of cell contents
    @param headers Optional header row
    @param col_styles Optional styles for each column
    @param border Optional border style for the table *)

(** {3 Panels} *)

val panel :
  ?box_style:border_style ->
  ?title:string ->
  ?title_align:[ `Left | `Center | `Right ] ->
  ?subtitle:string ->
  ?subtitle_align:[ `Left | `Center | `Right ] ->
  ?expand:bool ->
  ?style:Style.t ->
  ?border_style:Style.t ->
  ?width:int ->
  ?height:int ->
  ?padding:padding ->
  ?highlight:bool ->
  element ->
  element
(** Alias for {!Panel.panel}. Creates a panel with a border around content.
    @param child The content to display inside the panel
    @param box_style The border style (default: Rounded)
    @param title Optional title in panel header
    @param title_align Title alignment (default: Center)
    @param subtitle Optional subtitle in panel footer
    @param subtitle_align Subtitle alignment (default: Center)
    @param expand If true, stretches to fill width (default: true)
    @param style Style for panel contents
    @param border_style Style for the border
    @param width Fixed panel width
    @param height Fixed panel height
    @param padding Padding around content
    @param highlight Enable title/subtitle highlighting *)

(** {3 Tree View} *)

type tree_guide_style =
  | Normal  (** Standard lines: ├── └── │ *)
  | ASCII  (** ASCII-safe: +-- `-- | *)
  | Bold  (** Bold lines: ┣━━ ┗━━ ┃ *)
  | Double  (** Double lines: ╠══ ╚══ ║ *)

type tree_node = {
  label : element;
  expanded : bool;
  children : tree_node list;
  guide_style : Style.t option;
      (** Optional style override for this node's guides *)
}
(** [tree_node] represents a node in a tree structure.
    @field label The element to display for this node
    @field expanded Whether this node's children are shown (for display purposes only)
    @field children List of child nodes
    @field guide_style Optional style override for guide lines of this node *)

val tree :
  ?style:Style.t ->
  ?guide_style:Style.t ->
  ?guides:tree_guide_style ->
  ?hide_root:bool ->
  ?expanded:bool ->
  tree_node ->
  element
(** [tree ?style ?guide_style ?guides ?hide_root ?expanded node] creates a tree
    view display.
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

(** {1 Rendering} *)

val render : ?dark:bool -> ?theme:Theme.t -> Render.buffer -> element -> unit
(** [render ?dark ?theme buf elem] renders the complete element tree to the
    given buffer.

    @param dark
      Whether to use dark mode for adaptive colors. Defaults to [true].
    @param theme
      The theme to use for rendering. If provided, theme colors will be applied
      to elements. Defaults to no theme.

    It orchestrates the entire rendering process:

    - Calculates the complete layout tree for the given element.
    - Traverses the computed layout and draws the UI to the buffer.
    - Manages an internal layout cache for performance, which is valid only for
      the duration of this single call to [render]. *)

(** {1 Utilities} *)

val measure : ?width:int -> element -> int * int
(** [measure ?width elem] calculates the natural dimensions of an element.
    Returns [(width, height)] in character cells.

    @param width
      Optional width constraint for the element. If provided, the element will
      be measured as if it were being rendered within this width. This affects
      text wrapping and flow layouts.
    @return A tuple [(width, height)] representing the element's dimensions. *)

val pp_element : Format.formatter -> element -> unit
(** [pp_element fmt elem] pretty-prints the structure of an element tree, which
    is useful for debugging. *)

val dump_layout : element -> string
(** [dump_layout elem] returns a string representation of the element tree with
    computed layout bounds. This is useful for debugging layout issues. *)

val print :
  ?width:int -> ?height:int -> ?dark:bool -> ?theme:Theme.t -> element -> unit
(** [print ?width ?height ?dark ?theme elem] renders the element to stdout.
    @param width Terminal width (default: 80)
    @param height Terminal height (default: 24)
    @param dark Whether to use dark mode for adaptive colors (default: true)
    @param theme The theme to use for rendering *)
