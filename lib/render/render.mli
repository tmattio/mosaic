(** Low-level rendering primitives for terminal output.

    This module provides cell-based rendering with style support, efficient
    diffing, and Unicode handling. Manages a 2D buffer of styled cells and
    generates optimal ANSI sequences for terminal updates.

    Buffers maintain fixed dimensions after creation. Cells support combining
    characters and wide characters. Coordinates are zero-based from top-left.
    Out-of-bounds access is safe but ignored. Rendering preserves Unicode
    normalization and respects terminal width constraints. *)

(** {1 Styles and Colors} *)

module Style : sig
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

            Solid applies uniform color. Adaptive selects color based on
            terminal background. Gradient interpolates across element bounds. *)

  type t = {
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
    uri : string option;  (** Hyperlink URI *)
  }
  (** [t] represents complete text styling information.

      All attributes can combine. Colors use None for terminal default. URI
      creates clickable hyperlinks on supporting terminals. Boolean attributes
      are additive. *)

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
    | Blink
    | Reverse
    | Strikethrough
    | Link of string
        (** [attr] provides individual style attributes for composition.

            Used with [of_list] to build styles declaratively. Fg/Bg set colors.
            Other variants enable specific formatting. Link creates hyperlinks.
        *)

  val to_sgr : t -> string
  (** [to_sgr style] generates the ANSI SGR escape sequence for the style.

      Produces the escape string to apply the style's attributes and colors.
      Handles resolved colors (Solid only, as gradients/adaptive are
      pre-resolved in cell setting). Empty style returns empty string (no-op).
  *)

  (** {2 Style Constructors} *)

  val empty : t
  (** [empty] creates a style with no attributes set.

      Alias for [default]. Starting point for building custom styles. *)

  val fg : Ansi.color -> t
  (** [fg color] creates a style with only foreground color.

      All other attributes remain unset. Use [++] to combine with other styles.
  *)

  val bg : Ansi.color -> t
  (** [bg color] creates a style with only background color.

      All other attributes remain unset. Use [++] to combine with other styles.
  *)

  (** {2 Gradient Constructors} *)

  val gradient :
    colors:Ansi.color list -> direction:[ `Horizontal | `Vertical ] -> gradient
  (** [gradient ~colors ~direction] creates a gradient specification.

      Colors are evenly distributed. Empty list defaults to transparent. Single
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

      The gradient is applied across the rendered element's bounds. Each
      character gets an interpolated color based on position.

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

  val link : string -> t
  (** [link uri] creates a style with only hyperlink.

      Makes text clickable to open URI. Support varies by terminal. *)

  val of_list : attr list -> t
  (** [of_list attrs] creates a style from multiple attributes.

      Combines all attributes into single style. Later attributes override
      earlier ones for colors. Boolean attributes are set to true if present.

      Example: Creates bold red text on blue background.
      {[
        let style = Style.of_list [ Bold; Fg Red; Bg Blue ]
      ]} *)

  val ( ++ ) : t -> t -> t
  (** [s1 ++ s2] combines two styles with right precedence.

      Colors from [s2] override those in [s1]. Boolean attributes are OR'd
      together. Hyperlink from [s2] takes precedence. Useful for layering
      styles.

      Example: Adds bold to existing colored style.
      {[
        let colored = Style.fg Red
        let bold_colored = colored ++ Style.bold
      ]} *)

  (** [color] re-exports ANSI color type for convenience.

      Provides all color options without importing Ansi module. Basic colors
      work everywhere. Indexed and RGB colors need capable terminals. *)
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

      Values clamped to 0-255. Colors 0-15 are standard colors, 16-231 form
      6×6×6 RGB cube, 232-255 are grayscale. *)

  val rgb : int -> int -> int -> color
  (** [rgb r g b] creates a 24-bit RGB color.

      Each component clamped to 0-255. Requires true color terminal support.
      Automatically downgrades to nearest 256-color on limited terminals. *)

  val gray : int -> color
  (** [gray n] creates a grayscale color from 24 shades.

      Maps 0-23 to indices 232-255 in 256-color palette. 0 is black, 23 is
      white. Values outside range are clamped. *)

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

      The [light] color is used on light backgrounds, [dark] on dark
      backgrounds. Background detection happens at render time.

      Example: Creates text that's readable on any background.
      {[
        let text_color = Style.adaptive ~light:Black ~dark:White
      ]} *)

  val adaptive_fg : adaptive_color -> t
  (** [adaptive_fg color] creates a style with adaptive foreground color.

      Color selection based on detected terminal background. Falls back to
      [dark] variant if detection fails (most terminals have dark backgrounds).
  *)

  val adaptive_bg : adaptive_color -> t
  (** [adaptive_bg color] creates a style with adaptive background color.

      Color selection based on detected terminal background. Falls back to
      [dark] variant if detection fails. *)

  (** {2 Common Adaptive Colors} *)

  val adaptive_primary : adaptive_color
  (** Primary text color that adapts to background (Black on light, White on
      dark) *)

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
end

(** {1 Cells} *)

type cell = {
  chars : Uchar.t list;  (** Base character followed by combining characters *)
  style : Style.t;
  width : int;  (** Display width (0 for continuation, 2 for wide chars) *)
}
(** [cell] represents a single terminal cell with content and style.

    Supports Unicode combining characters for accents and marks. Width indicates
    display columns: 1 for normal, 2 for wide (CJK), 0 for continuation cells.
    Empty cells have empty chars list and default style. *)

val empty_cell : cell
(** [empty_cell] is a blank cell with default style.

    Contains no characters, default style, width 1. Used for clearing and
    initialization. *)

(** {1 Terminal Background Detection} *)

val set_terminal_background : dark:bool -> unit
(** [set_terminal_background ~dark] updates the global terminal background
    state.

    This affects how adaptive colors are rendered. Should be called once during
    initialization after detecting the terminal's background color. The setting
    persists for the lifetime of the process.

    @param dark [true] for dark backgrounds, [false] for light backgrounds *)

(** {1 Buffer} *)

type buffer
(** [buffer] is a 2D grid of styled cells.

    Fixed-size after creation. Mutable for efficient updates. Coordinates are
    (x,y) with origin at top-left (0,0). *)

val create : int -> int -> buffer
(** [create width height] creates a new buffer filled with empty cells.

    Dimensions must be positive. All cells initialized to [empty_cell]. Memory
    allocated upfront for all cells.

    @raise Invalid_argument if width or height <= 0 *)

val clear : buffer -> unit
(** [clear buffer] resets all cells to empty state.

    Efficient batch operation. Preserves buffer dimensions. Equivalent to
    filling with [empty_cell]. *)

val dimensions : buffer -> int * int
(** [dimensions buffer] returns buffer size as [(width, height)].

    Dimensions are immutable after creation. Useful for boundary checking. *)

(** {2 Clipping Support} *)

module Clip : sig
  type t
  (** [t] represents a rectangular clipping region *)

  val make : int -> int -> int -> int -> t
  (** [make x y width height] creates a clipping rectangle.

      Coordinates are in buffer space. The rectangle is immutable. *)

  val x : t -> int
  (** [x rect] returns the x-coordinate of the rectangle's top-left corner *)

  val y : t -> int
  (** [y rect] returns the y-coordinate of the rectangle's top-left corner *)

  val width : t -> int
  (** [width rect] returns the width of the rectangle *)

  val height : t -> int
  (** [height rect] returns the height of the rectangle *)

  val intersect : t -> t -> t
  (** [intersect c1 c2] returns the intersection of two clipping rectangles. *)

  val intersect_opt : t option -> t option -> t option
  (** [intersect_opt c1 c2] returns the intersection of two optional clipping
      rectangles. Returns None if either is None. *)

  val contains : t -> int -> int -> bool
  (** [contains rect x y] checks if point [(x, y)] is inside the rectangle.
      Returns [true] if the point is inside, [false] otherwise. *)
end

val full_clip : buffer -> Clip.t
(** [full_clip buffer] returns a clip rect covering the entire buffer *)

val get : buffer -> int -> int -> cell
(** [get buffer x y] retrieves the cell at position [(x, y)].

    Returns [empty_cell] for out-of-bounds coordinates. Safe for any integer
    coordinates. O(1) access time. *)

val set : ?clip:Clip.t -> buffer -> int -> int -> cell -> unit
(** [set ?clip buffer x y cell] places [cell] at position [(x, y)].

    Out-of-bounds writes are ignored. If [clip] is provided, writes outside the
    clip rectangle are also ignored. Handles wide characters by setting
    continuation cells automatically. O(1) update time. *)

val set_char :
  ?clip:Clip.t -> buffer -> int -> int -> Uchar.t -> Style.t -> unit
(** [set_char ?clip buffer x y char style] places a styled character.

    Convenience for single character updates. Handles wide characters correctly.
    Sets continuation cells for 2-column characters. Out-of-bounds and
    out-of-clip writes are ignored. *)

val set_string :
  ?clip:Clip.t -> buffer -> int -> int -> string -> Style.t -> unit
(** [set_string ?clip buffer x y str style] writes styled UTF-8 string
    horizontally.

    Starts at [(x, y)] and continues rightward. Handles Unicode correctly.
    Clipped at buffer edge and clip rectangle if provided. Wide characters use 2
    columns. Newlines ignored.

    Example: Writes colored label.
    {[
      Render.set_string buffer 10 5 "Hello" (Style.fg Red)
    ]} *)

(** {1 Rendering} *)

type cursor_pos = [ `Hide | `Move of int * int  (** (x, y) position *) ]
(** [cursor_pos] controls cursor visibility and position after rendering.

    `Hide makes cursor invisible during updates to prevent flicker. `Move
    positions cursor at specific coordinates for input fields. *)

type patch = { row : int; col : int; old_cell : cell; new_cell : cell }
(** [patch] represents a single cell change between buffer states.

    Used by diff algorithm to minimize terminal updates. Contains position and
    both old and new cell states for optimal escape sequence generation. *)

val diff : buffer -> buffer -> patch list
(** [diff old new] computes minimal cell changes between buffers.

    Compares buffers cell-by-cell, returning only positions that changed.
    Optimizes terminal bandwidth by sending minimal updates. Order preserves
    left-to-right, top-to-bottom for efficient cursor movement.

    @raise Invalid_argument if buffer dimensions differ

    Example: Typical update cycle.
    {[
      let patches = Render.diff old_buffer new_buffer in
      let output = Render.render_patches patches in
      Terminal.write term output
    ]} *)

val render_patch : patch -> string
(** [render_patch patch] generates ANSI sequences for one cell change.

    Includes cursor positioning and style changes. Less efficient than
    [render_patches] for multiple changes due to repeated cursor moves. Useful
    for debugging or real-time single cell updates. *)

(** Rendering mode for different terminal contexts *)
type render_mode =
  | Absolute  (** Use absolute positioning (for alt-screen mode) *)
  | Relative
      (** Use relative positioning with aggressive style resets (for
          non-alt-screen mode) *)

val render_patches :
  ?cursor_pos:cursor_pos -> ?mode:render_mode -> patch list -> string
(** [render_patches ?cursor_pos ?mode patches] optimally renders multiple
    changes.

    Minimizes cursor movements by grouping nearby changes. Coalesces style
    changes for efficiency when safe. Handles wide characters and combining
    marks correctly. Final cursor position controlled by [cursor_pos] parameter.

    @param cursor_pos Final cursor state (default: `Hide)
    @param mode Rendering mode (default: `Absolute) *)

val render_full :
  ?cursor_pos:cursor_pos -> ?mode:render_mode -> buffer -> string
(** [render_full ?cursor_pos ?mode buffer] renders complete buffer contents.

    Generates escape sequences for entire screen. In Absolute mode, starts from
    home position (0,0). In Relative mode, starts from current cursor position.
    Optimizes by coalescing adjacent cells with same style when safe.

    @param cursor_pos Final cursor state (default: `Hide)
    @param mode Rendering mode (default: `Absolute) *)

(** {1 Utilities} *)

val measure_string : string -> int
(** [measure_string s] calculates the display width of UTF-8 string [s].

    Handles wide characters (2 columns), combining marks (0 columns), and
    control characters. Essential for accurate terminal layout. ANSI escapes
    should be stripped first.

    Example: Measures CJK text width.
    {[
      let width = Render.measure_string "Hello 世界" (* 10 columns *)
    ]} *)

val truncate_string : string -> int -> string
(** [truncate_string s width] cuts string [s] to fit in [width] columns.

    Preserves valid UTF-8. Avoids splitting wide characters or combining marks.
    Returns shorter string if needed. Never exceeds target width. *)

val pad_string : string -> int -> string
(** [pad_string s width] extends string [s] with spaces to [width] columns.

    No effect if string already meets or exceeds width. Padding added at end.
    Handles wide characters correctly when calculating padding needed. *)

val truncate_string_with_ellipsis : string -> int -> string -> string
(** [truncate_string_with_ellipsis s max_width ellipsis] truncates with custom
    ellipsis.

    Ensures total width including ellipsis fits in [max_width]. If string fits,
    returns unchanged. If truncation needed, removes enough characters for
    ellipsis. Handles wide character ellipsis.

    Example: Truncates long path with ellipsis.
    {[
      let short =
        Render.truncate_string_with_ellipsis "/very/long/path/to/file.txt" 20
          "..."
      (* Result: "/very/long/pa..." *)
    ]} *)

(** {1 Gradient Rendering} *)

val set_string_gradient :
  ?clip:Clip.t ->
  buffer ->
  int ->
  int ->
  string ->
  Style.t ->
  width:int ->
  height:int ->
  line_offset:int ->
  unit
(** [set_string_gradient buffer x y str style ~width ~height ~line_offset]
    writes string with gradient.

    Applies gradient colors character-by-character based on position within the
    specified width and height bounds. Horizontal gradients interpolate along
    the x-axis, vertical gradients along the y-axis. The line_offset parameter
    specifies the y offset within a multi-line gradient (0 for single line or
    first line of multi-line text).

    @param width Total width of the text area for gradient calculation
    @param height Total height of the text area (usually 1 for single line) *)

val fill_rect :
  ?clip:Clip.t -> buffer -> int -> int -> int -> int -> Style.t -> unit
(** [fill_rect buffer x y width height style] fills rectangle with solid style.

    Fills the specified rectangular area with space characters and the given
    style. More efficient than fill_rect_gradient for solid colors. *)

val fill_rect_gradient :
  ?clip:Clip.t -> buffer -> int -> int -> int -> int -> Style.t -> unit
(** [fill_rect_gradient buffer x y width height style] fills rectangle with
    gradient.

    Fills the specified rectangular area with space characters, applying
    gradient background colors based on position. Each cell gets an interpolated
    color based on its position in the rectangle. *)
