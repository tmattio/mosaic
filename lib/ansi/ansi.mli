(** ANSI escape sequences for terminal control.

    This module provides type-safe generation of ANSI escape sequences for
    terminal manipulation. Supports colors, text styling, cursor control, and
    screen management following standard terminal protocols.

    All functions produce valid ANSI escape sequences as strings. Color indices
    are clamped to 0-255 range. RGB values are clamped to 0-255 per channel.
    Sequences follow CSI (Control Sequence Introducer) format. *)

(** {1 Colors} *)

(** [color] represents terminal color values.

    Basic colors (Black through White) map to standard 16-color palette. Bright
    variants use high-intensity versions. Default resets to terminal's default
    color. Index allows 256-color palette access (0-255). RGB enables true color
    on supporting terminals with red, green, blue values (0-255 each). *)
type color =
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
  | Index of int  (** 256-color palette index *)
  | RGB of int * int * int  (** 24-bit RGB color *)

(** {1 Styles} *)

type style =
  [ `Bold  (** Increases font weight *)
  | `Dim  (** Reduces font intensity *)
  | `Italic  (** Slants text (terminal support varies) *)
  | `Underline  (** Adds single underline *)
  | `Double_underline  (** Adds double underline (limited support) *)
  | `Blink  (** Makes text flash (often disabled) *)
  | `Reverse  (** Reverses foreground/background colors *)
  | `Strikethrough  (** Crosses out text *)
  | `Overline  (** Adds line above text (limited support) *) ]
(** [style] represents text formatting attributes.*)

type attr =
  [ `Fg of color  (** Sets foreground (text) color *)
  | `Bg of color  (** Sets background color *)
  | `Reset  (** Resets all attributes to terminal defaults *)
  | style ]
(** [attr] combines colors and styles into display attributes. *)

(** {1 Cursor Control} *)

val cursor_up : int -> string
(** [cursor_up n] moves cursor up [n] lines.

    Cursor remains in same column. Stops at top of screen. Negative values
    treated as 0. *)

val cursor_down : int -> string
(** [cursor_down n] moves cursor down [n] lines.

    Cursor remains in same column. Stops at bottom of screen. Negative values
    treated as 0. *)

val cursor_forward : int -> string
(** [cursor_forward n] moves cursor forward [n] columns.

    Stays on current line. Stops at right edge. Negative values treated as 0. *)

val cursor_back : int -> string
(** [cursor_back n] moves cursor backward [n] columns.

    Stays on current line. Stops at left edge. Negative values treated as 0. *)

val cursor_position : int -> int -> string
(** [cursor_position row col] moves cursor to absolute position.

    Coordinates are 1-based (top-left is 1,1). Values clamped to screen
    dimensions. *)

val cursor_save : string
(** [cursor_save] saves current cursor position.

    Position can be restored with [cursor_restore]. Terminal typically supports
    one saved position. *)

val cursor_restore : string
(** [cursor_restore] restores previously saved cursor position.

    Returns to position saved by [cursor_save]. No effect if position never
    saved. *)

val cursor_show : string
(** [cursor_show] makes cursor visible.

    Reverses effect of [cursor_hide]. Cursor visibility is terminal state, not
    saved position. *)

val cursor_hide : string
(** [cursor_hide] makes cursor invisible.

    Cursor still moves but not displayed. Useful during rendering to prevent
    flicker. *)

(** {2 Cursor Shape} *)

val set_cursor_style_blinking_block : string
(** [set_cursor_style_blinking_block] sets cursor to blinking block shape.

    Full character cell cursor that blinks. Most visible cursor style. *)

val set_cursor_style_steady_block : string
(** [set_cursor_style_steady_block] sets cursor to steady block shape.

    Full character cell cursor without blinking. High visibility without
    distraction. *)

val set_cursor_style_blinking_underline : string
(** [set_cursor_style_blinking_underline] sets cursor to blinking underline
    shape.

    Thin line at bottom of character cell that blinks. Common for insert mode.
*)

val set_cursor_style_steady_underline : string
(** [set_cursor_style_steady_underline] sets cursor to steady underline shape.

    Thin line at bottom of character cell without blinking. Less intrusive than
    block. *)

val set_cursor_style_blinking_bar : string
(** [set_cursor_style_blinking_bar] sets cursor to blinking vertical bar shape.

    Thin vertical line between characters that blinks. Common for modern
    editors. *)

val set_cursor_style_steady_bar : string
(** [set_cursor_style_steady_bar] sets cursor to steady vertical bar shape.

    Thin vertical line between characters without blinking. Minimal and precise.
*)

(** {1 Screen Control} *)

val clear_screen : string
(** [clear_screen] clears entire screen and moves cursor to home position.

    Erases all content. Cursor moves to top-left (1,1). Scrollback buffer
    unaffected. *)

val clear_screen_above : string
(** [clear_screen_above] clears screen from cursor to top.

    Includes current line. Cursor position unchanged. Content below cursor
    preserved. *)

val clear_screen_below : string
(** [clear_screen_below] clears screen from cursor to bottom.

    Includes current line. Cursor position unchanged. Content above cursor
    preserved. *)

val clear_line : string
(** [clear_line] clears entire current line.

    Cursor position unchanged. Line content replaced with spaces. *)

val clear_line_left : string
(** [clear_line_left] clears current line from start to cursor.

    Includes character at cursor. Content right of cursor preserved. *)

val clear_line_right : string
(** [clear_line_right] clears current line from cursor to end.

    Includes character at cursor. Content left of cursor preserved. *)

val scroll_up : int -> string
(** [scroll_up n] scrolls screen content up [n] lines.

    New blank lines appear at bottom. Content scrolled off top is lost. Cursor
    stays at same screen position. *)

val scroll_down : int -> string
(** [scroll_down n] scrolls screen content down [n] lines.

    New blank lines appear at top. Content scrolled off bottom is lost. Cursor
    stays at same screen position. *)

(** {1 Terminal Modes} *)

val alternate_screen_on : string
(** [alternate_screen_on] switches to alternate screen buffer.

    Saves current screen content. Shows blank alternate screen. Common for
    full-screen applications. *)

val alternate_screen_off : string
(** [alternate_screen_off] returns to main screen buffer.

    Restores previously saved screen content. Alternate screen content is
    discarded. *)

val mouse_on : string
(** [mouse_on] enables mouse event reporting.

    Terminal sends escape sequences for mouse clicks, releases, and motion.
    Required for mouse support. *)

val mouse_off : string
(** [mouse_off] disables mouse event reporting.

    Terminal stops sending mouse events. Returns mouse to normal selection mode.
*)

val bracketed_paste_on : string
(** [bracketed_paste_on] enables bracketed paste mode.

    Pasted text is wrapped in escape sequences. Allows distinguishing typed
    input from pastes. *)

val bracketed_paste_off : string
(** [bracketed_paste_off] disables bracketed paste mode.

    Pasted text appears as normal input. Cannot distinguish pastes from typing.
*)

val set_window_title : string -> string
(** [set_window_title title] creates escape sequence to set terminal window
    title.

    Uses OSC (Operating System Command) sequence. Support varies by terminal
    emulator. Title typically appears in window title bar or tab. Special
    characters should be escaped. *)

(** {1 SGR (Select Graphic Rendition)} *)

val sgr : attr list -> string
(** [sgr attrs] generates SGR escape sequence for the given attributes.

    Combines multiple attributes into single escape sequence. Empty list
    produces no-op sequence. Attributes applied in order given. Later attributes
    may override earlier ones.

    Example: Creates bold red text on blue background.
    {[
      let style_seq = Ansi.sgr [ `Bold; `Fg Red; `Bg Blue ]
    ]} *)

val reset : string
(** [reset] clears all text attributes to terminal defaults.

    Removes all colors, styles, and formatting. Returns to normal text
    appearance. *)

val reset_bold_dim : string
(** [reset_bold_dim] removes bold and dim attributes only.

    Other attributes like colors and underline remain. Returns text to normal
    weight. *)

val reset_italic : string
(** [reset_italic] removes italic attribute only.

    Returns text to upright position. Other attributes preserved. *)

val reset_underline : string
(** [reset_underline] removes all underline attributes.

    Clears both single and double underlines. Other attributes preserved. *)

val reset_blink : string
(** [reset_blink] removes blink attribute only.

    Stops text flashing. Other attributes preserved. *)

val reset_reverse : string
(** [reset_reverse] removes reverse video attribute.

    Returns to normal foreground/background arrangement. Other attributes
    preserved. *)

val reset_strikethrough : string
(** [reset_strikethrough] removes strikethrough attribute only.

    Removes line through text. Other attributes preserved. *)

val reset_overline : string
(** [reset_overline] removes overline attribute only.

    Removes line above text. Other attributes preserved. *)

(** {1 Utilities} *)

val strip : string -> string
(** [strip s] removes all ANSI escape sequences from string [s].

    Returns plain text content only. Useful for calculating display width or
    logging. Handles all standard escape sequences including CSI, OSC, and
    others.

    Example: Gets plain text from styled string.
    {[
      let styled = Ansi.style [ `Bold; `Fg Red ] "Error!"
      let plain = Ansi.strip styled (* "Error!" *)
    ]} *)

val hyperlink : uri:string -> string -> string
(** [hyperlink ~uri text] creates a clickable terminal hyperlink.

    Displays [text] as clickable link to [uri]. Uses OSC 8 escape sequence.
    Terminal support varies - non-supporting terminals show plain text. URIs
    should be properly encoded. Common schemes: http, https, file.

    Example: Creates clickable documentation link.
    {[
      let link =
        Ansi.hyperlink ~uri:"https://example.com/docs" "View Documentation"
    ]} *)

val style : attr list -> string -> string
(** [style attrs s] applies attributes to string [s] with automatic reset.

    Wraps text with SGR sequences for attributes and appends reset. Ensures
    styles don't leak. Empty attribute list returns unchanged string. Handles
    multi-line strings correctly.

    Example: Creates error message with red bold text.
    {[
      let error_msg =
        Ansi.style
          [ `Bold; `Fg Red ]
          "Error: File not found" print_endline error_msg
    ]} *)
