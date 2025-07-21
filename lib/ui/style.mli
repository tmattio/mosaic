(** Text styling with colors, attributes, and hyperlinks. *)

(** {2 Color Specifications} *)

type adaptive_color = { light : Ansi.color; dark : Ansi.color }
(** [adaptive_color] represents a color that adapts to terminal background.

    Selects appropriate color based on whether terminal has light or dark
    background. Provides automatic contrast adjustment. *)

type gradient = {
  colors : Ansi.color list;
  direction : [ `Horizontal | `Vertical ];
}
(** [gradient] represents a linear gradient specification.

    Colors are interpolated evenly across the gradient. Requires at least one
    color. Single color acts as solid fill. Direction determines interpolation
    axis. *)

type color_spec =
  | Solid of Ansi.color
  | Adaptive of adaptive_color
  | Gradient of gradient
      (** [color_spec] specifies how color is applied.

          Solid applies uniform color. Adaptive selects color based on terminal
          background. Gradient interpolates across element bounds. *)

type t = private {
  fg : color_spec option;
  bg : color_spec option;
  bold : bool;
  dim : bool;
  italic : bool;
  underline : bool;
  double_underline : bool;
  blink : bool;
  reverse : bool;
  strikethrough : bool;
  overline : bool;
  conceal : bool;
  framed : bool;
  encircled : bool;
  uri : string option;  (** Hyperlink URI *)
}
(** [t] represents complete text styling information.

    All attributes can combine. Colors use None for terminal default. URI
    creates clickable hyperlinks on supporting terminals. Boolean attributes are
    additive. *)

val default : t
(** [default] is the terminal's default style.

    No colors set, all attributes false, no hyperlink. Equivalent to terminal
    reset state. *)

type attr =
  | Fg of Ansi.color
  | Bg of Ansi.color
  | Fg_gradient of gradient
  | Bg_gradient of gradient
  | Bold
  | Dim
  | Italic
  | Underline
  | Double_underline
  | Blink
  | Reverse
  | Strikethrough
  | Overline
  | Conceal
  | Framed
  | Encircled
  | Link of string
      (** [attr] provides individual style attributes for composition.

          Used with [of_list] to build styles declaratively. Fg/Bg set colors.
          Other variants enable specific formatting. Link creates hyperlinks. *)

val to_sgr : dark:bool -> t -> string
(** [to_sgr ~dark style] generates the ANSI SGR escape sequence for the style.

    Produces the escape string to apply the style's attributes and colors.
    Handles resolved colors (Solid only, as gradients/adaptive are pre-resolved
    in cell setting). Empty style returns empty string (no-op). *)

val resolve_color :
  color_spec option ->
  dark:bool ->
  x:int ->
  y:int ->
  width:int ->
  height:int ->
  Ansi.color option
(** Pure resolution of color_spec to concrete color at position (x,y) in bounds.
    Used during rendering. Raises Invalid_argument on invalid gradient. *)

(** {2 Style Constructors} *)

val empty : t
(** [empty] creates a style with no attributes set.

    Alias for [default]. Starting point for building custom styles. *)

val fg : Ansi.color -> t
(** [fg color] creates a style with only foreground color.

    All other attributes remain unset. Use [++] to combine with other styles. *)

val bg : Ansi.color -> t
(** [bg color] creates a style with only background color.

    All other attributes remain unset. Use [++] to combine with other styles. *)

(** {2 Gradient Constructors} *)

val gradient :
  colors:Ansi.color list -> direction:[ `Horizontal | `Vertical ] -> gradient
(** [gradient ~colors ~direction] creates a gradient specification.

    Colors are evenly distributed. Empty list raises Invalid_argument. Single
    color acts as solid fill.

    Example: Create a horizontal rainbow gradient.
    {[
      let rainbow =
        Style.gradient
          ~colors:[ Red; Yellow; Green; Cyan; Blue; Magenta ]
          ~direction:`Horizontal
    ]} *)

val gradient_fg :
  colors:Ansi.color list -> direction:[ `Horizontal | `Vertical ] -> t
(** [gradient_fg ~colors ~direction] creates a style with gradient foreground.

    The gradient is applied across the rendered element's bounds. Each character
    gets an interpolated color based on position.

    Example: Create text with horizontal gradient.
    {[
      let style =
        Style.gradient_fg ~colors:[ Blue; Cyan ] ~direction:`Horizontal
    ]} *)

val gradient_bg :
  colors:Ansi.color list -> direction:[ `Horizontal | `Vertical ] -> t
(** [gradient_bg ~colors ~direction] creates a style with gradient background.

    The gradient fills the element's background area. Works best with elements
    that have explicit dimensions. *)

val bold : t
(** [bold] creates a style with only bold attribute.

    Increases font weight. Terminal support universal. *)

val dim : t
(** [dim] creates a style with only dim attribute.

    Reduces text intensity. Less prominent than normal text. *)

val italic : t
(** [italic] creates a style with only italic attribute.

    Slants text. Terminal support varies, may show as different color. *)

val underline : t
(** [underline] creates a style with only underline attribute.

    Single line below text. Well-supported across terminals. *)

val blink : t
(** [blink] creates a style with only blink attribute.

    Makes text flash. Often disabled by users or terminals. *)

val reverse : t
(** [reverse] creates a style with only reverse attribute.

    Swaps foreground and background colors. Always visible effect. *)

val strikethrough : t
(** [strikethrough] creates a style with only strikethrough attribute.

    Line through middle of text. Modern terminal feature. *)

val double_underline : t
(** [double_underline] creates a style with only double underline attribute.

    Double line below text. Terminal support varies. *)

val overline : t
(** [overline] creates a style with only overline attribute.

    Line above text. Modern terminal feature. *)

val conceal : t
(** [conceal] creates a style with only conceal attribute.

    Hides text (SGR 8). Useful for passwords. Limited terminal support. *)

val framed : t
(** [framed] creates a style with only framed attribute.

    Adds frame around text (SGR 51). Very limited terminal support. *)

val encircled : t
(** [encircled] creates a style with only encircled attribute.

    Encircles text (SGR 52). Very limited terminal support. *)

val link : string -> t
(** [link uri] creates a style with only hyperlink.

    Makes text clickable to open URI. Support varies by terminal. *)

val of_list : attr list -> t
(** [of_list attrs] creates a style from multiple attributes.

    Combines all attributes into single style. Later attributes override earlier
    ones for colors. Boolean attributes are set to true if present.

    Example: Creates bold red text on blue background.
    {[
      let style = Style.of_list [ Bold; Fg Red; Bg Blue ]
    ]} *)

val merge : t -> t -> t
(** [merge s1 s2] combines two styles with right precedence.

    Colors from [s2] override those in [s1]. Boolean attributes are OR'd
    together. Hyperlink from [s2] takes precedence. Useful for layering styles.

    Example: Adds bold to existing colored style.
    {[
      let colored = Style.fg Red
      let bold_colored = Style.merge colored Style.bold
    ]} *)

val ( ++ ) : t -> t -> t
(** [s1 ++ s2] is a synonym for [merge s1 s2]. *)

(** [color] re-exports ANSI color type for convenience.

    Provides all color options without importing Ansi module. Basic colors work
    everywhere. Indexed and RGB colors need capable terminals. *)
type color = Ansi.color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default
  | Bright_black
  | Bright_red
  | Bright_green
  | Bright_yellow
  | Bright_blue
  | Bright_magenta
  | Bright_cyan
  | Bright_white
  | Index of int  (** 256-color palette (0-255) *)
  | RGB of int * int * int  (** 24-bit color (0-255 each) *)

val ansi256 : int -> color
(** [ansi256 n] creates a 256-color palette color.

    Values clamped to 0-255. Colors 0-15 are standard colors, 16-231 form 6×6×6
    RGB cube, 232-255 are grayscale. *)

val rgb : int -> int -> int -> color
(** [rgb r g b] creates a 24-bit RGB color.

    Each component clamped to 0-255. Requires true color terminal support.
    Automatically downgrades to nearest 256-color on limited terminals. *)

val gray : int -> color
(** [gray n] creates a grayscale color from 24 shades.

    Maps 0-23 to indices 232-255 in 256-color palette. 0 is black, 23 is white.
    Values outside range are clamped. *)

val rgb_hex : int -> color
(** [rgb_hex hex] creates an RGB color from hexadecimal.

    Format: 0xRRGGBB. Each component uses 8 bits.

    Example: Creates magenta from hex.
    {[
      let magenta = Style.rgb_hex 0xFF00FF
    ]} *)

(** {2 Adaptive Colors} *)

val adaptive : light:color -> dark:color -> adaptive_color
(** [adaptive ~light ~dark] creates an adaptive color specification.

    The [light] color is used on light backgrounds, [dark] on dark backgrounds.
    Background detection happens at render time.

    Example: Creates text that's readable on any background.
    {[
      let text_color = Style.adaptive ~light:Black ~dark:White
    ]} *)

val adaptive_fg : adaptive_color -> t
(** [adaptive_fg color] creates a style with adaptive foreground color.

    Color selection based on detected terminal background. Falls back to [dark]
    variant if detection fails (most terminals have dark backgrounds). *)

val adaptive_bg : adaptive_color -> t
(** [adaptive_bg color] creates a style with adaptive background color.

    Color selection based on detected terminal background. Falls back to [dark]
    variant if detection fails. *)

(** {2 Common Adaptive Colors} *)

val adaptive_primary : adaptive_color
(** Primary text color that adapts to background (Black on light, White on dark)
*)

val adaptive_secondary : adaptive_color
(** Secondary text color with reduced contrast *)

val adaptive_accent : adaptive_color
(** Accent color that remains visible on both backgrounds *)

val adaptive_error : adaptive_color
(** Error color that adapts while maintaining urgency *)

val adaptive_warning : adaptive_color
(** Warning color that adapts while maintaining visibility *)

val adaptive_success : adaptive_color
(** Success color that adapts while maintaining positive association *)

(** {2 Predefined Styles} *)

val error : t
(** [error] is a predefined error style (red + bold). *)

val warning : t
(** [warning] is a predefined warning style (yellow + underline). *)

val success : t
(** [success] is a predefined success style (green + italic). *)

val info : t
(** [info] is a predefined info style (blue). *)

val muted : t
(** [muted] is a predefined muted style (dim). *)

(** {2 Utility Functions} *)

val equal : t -> t -> bool
(** [equal s1 s2] tests structural equality of two styles.

    Useful for testing and caching. Compares all fields for exact match. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt style] pretty-prints a style for debugging.

    Shows all set attributes and colors in human-readable format. *)
