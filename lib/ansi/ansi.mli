(** ANSI escape sequences for terminal control.

    This module provides type-safe generation of ANSI escape sequences for
    terminal manipulation. Supports colors, text styling, cursor control, and
    screen management following standard terminal protocols (ECMA-48/ANSI X3.64
    where possible; extensions noted).

    All functions produce valid ANSI escape sequences as strings. Color indices
    and RGB values are clamped to 0-255 range. Sequences follow CSI (Control
    Sequence Introducer) format. *)

(** {1 Colors} *)

(** [color] represents terminal color values.

    Basic colors (Black through White) map to standard 16-color palette. Bright
    variants use high-intensity versions. Default resets to terminal's default
    color. Index allows 256-color palette access (clamped to 0-255). RGB enables
    true color on supporting terminals with red, green, blue values (clamped to
    0-255 each). *)
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
  | `Double_underline
    (** Adds double underline (limited support; may conflict with bold reset) *)
  | `Blink  (** Makes text flash (often disabled) *)
  | `Reverse  (** Reverses foreground/background colors *)
  | `Conceal  (** Hides text (e.g., for passwords; limited support) *)
  | `Strikethrough  (** Crosses out text *)
  | `Overline  (** Adds line above text (limited support) *)
  | `Framed  (** Adds frame around text (SGR 51; very limited support) *)
  | `Encircled  (** Encircles text (SGR 52; very limited support) *) ]
(** [style] represents text formatting attributes. *)

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

val cursor_next_line : int -> string
(** [cursor_next_line n] moves cursor to beginning of line [n] lines down.

    Negative values treated as 0. *)

val cursor_previous_line : int -> string
(** [cursor_previous_line n] moves cursor to beginning of line [n] lines up.

    Negative values treated as 0. *)

val cursor_horizontal_absolute : int -> string
(** [cursor_horizontal_absolute col] moves cursor to column [col] on current
    row.

    1-based; values <=0 treated as 1. *)

val cursor_vertical_absolute : int -> string
(** [cursor_vertical_absolute row] moves cursor to row [row] on current column.

    1-based; values <=0 treated as 1. *)

val cursor_position : int -> int -> string
(** [cursor_position row col] moves cursor to absolute position.

    Coordinates are 1-based (top-left is 1,1). Values <=0 treated as 1. *)

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

val cursor_tab : int -> string
(** [cursor_tab n] advances cursor to next tab stop [n] times (CHT).

    Negative values treated as 0. *)

val cursor_back_tab : int -> string
(** [cursor_back_tab n] moves cursor to previous tab stop [n] times (CBT).

    Negative values treated as 0. *)

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

val set_cursor_style_default : string
(** [set_cursor_style_default] resets cursor to terminal's default style.

    Returns cursor to the style configured in terminal preferences. *)

(** {1 Screen Control} *)

val erase_in_display : int -> string
(** [erase_in_display param] erases part of the display.
    - 0: from cursor to end (below)
    - 1: from beginning to cursor (above)
    - 2: entire screen
    - 3: entire screen including scrollback (extension) Invalid params default
      to 2. Cursor position unchanged. *)

val clear_screen : string
(** [clear_screen] clears entire screen (alias for [erase_in_display 2]). *)

val clear_screen_above : string
(** [clear_screen_above] clears from top to cursor (alias for
    [erase_in_display 1]). *)

val clear_screen_below : string
(** [clear_screen_below] clears from cursor to bottom (alias for
    [erase_in_display 0]). *)

val clear_terminal : string
(** [clear_terminal] clears entire screen, scrollback buffer, and moves cursor
    to home (extension; not universal). *)

val erase_in_line : int -> string
(** [erase_in_line param] erases part of the current line.
    - 0: from cursor to end (right)
    - 1: from beginning to cursor (left)
    - 2: entire line Invalid params default to 2. Cursor position unchanged. *)

val clear_line : string
(** [clear_line] clears entire current line (alias for [erase_in_line 2]). *)

val clear_line_left : string
(** [clear_line_left] clears from start to cursor (alias for [erase_in_line 1]).
*)

val clear_line_right : string
(** [clear_line_right] clears from cursor to end (alias for [erase_in_line 0]).
*)

val scroll_up : int -> string
(** [scroll_up n] scrolls screen content up [n] lines.

    New blank lines appear at bottom. Content scrolled off top is lost. Cursor
    stays at same screen position. Negative values treated as 0. *)

val scroll_down : int -> string
(** [scroll_down n] scrolls screen content down [n] lines.

    New blank lines appear at top. Content scrolled off bottom is lost. Cursor
    stays at same screen position. Negative values treated as 0. *)

val set_scrolling_region : ?top:int -> ?bottom:int -> unit -> string
(** [set_scrolling_region ~top ~bottom ()] sets the scrolling region from row
    [top] to [bottom]. Both 1-based; [top] defaults to 1. If [bottom] is None,
    resets to full screen. Invalid regions (top < 1 or bottom <= top) raise
    Invalid_argument. *)

val insert_lines : int -> string
(** [insert_lines n] inserts [n] blank lines at cursor row, scrolling down.
    Negative values treated as 0. *)

val delete_lines : int -> string
(** [delete_lines n] deletes [n] lines at cursor row, scrolling up. Negative
    values treated as 0. *)

val insert_characters : int -> string
(** [insert_characters n] inserts [n] blank characters at cursor, shifting
    right. Negative values treated as 0. *)

val delete_characters : int -> string
(** [delete_characters n] deletes [n] characters at cursor, shifting left.
    Negative values treated as 0. *)

val erase_characters : int -> string
(** [erase_characters n] erases [n] characters at cursor with spaces. Negative
    values treated as 0. *)

val repeat_last_character : int -> string
(** [repeat_last_character n] repeats the last printed character [n] times.
    Negative values treated as 0. Note: Requires a character to have been
    printed previously. *)

val set_tab_stop : string
(** [set_tab_stop] sets a tab stop at current cursor column (HTS). *)

val clear_tab_stop_at_cursor : string
(** [clear_tab_stop_at_cursor] clears tab stop at current cursor column (TBC 0).
*)

val clear_tab_stop : int -> string
(** [clear_tab_stop param] clears tab stops (TBC).
    - 0: at current column
    - 3: all tab stops Invalid params default to 0. *)

val clear_all_tab_stops : string
(** [clear_all_tab_stops] clears all tab stops (TBC 3). *)

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

(** {2 Mouse Reporting Modes} *)

val mouse_x10_on : string
(** [mouse_x10_on] enables X10 mouse protocol. Only button press events. *)

val mouse_x10_off : string
(** [mouse_x10_off] disables X10 mouse protocol. *)

val mouse_normal_on : string
(** [mouse_normal_on] enables normal mouse tracking (press and release). *)

val mouse_normal_off : string
(** [mouse_normal_off] disables normal mouse tracking. *)

val mouse_button_tracking_on : string
(** [mouse_button_tracking_on] enables button-event tracking (includes motion
    with buttons). *)

val mouse_button_tracking_off : string
(** [mouse_button_tracking_off] disables button-event tracking. *)

val mouse_any_tracking_on : string
(** [mouse_any_tracking_on] enables any-event tracking (all motion events). *)

val mouse_any_tracking_off : string
(** [mouse_any_tracking_off] disables any-event tracking. *)

val mouse_utf8_on : string
(** [mouse_utf8_on] enables UTF-8 encoded mouse coordinates. *)

val mouse_utf8_off : string
(** [mouse_utf8_off] disables UTF-8 encoded mouse coordinates. *)

val mouse_sgr_on : string
(** [mouse_sgr_on] enables SGR mouse mode (extended coordinates). *)

val mouse_sgr_off : string
(** [mouse_sgr_off] disables SGR mouse mode. *)

val mouse_urxvt_on : string
(** [mouse_urxvt_on] enables URXVT mouse mode. *)

val mouse_urxvt_off : string
(** [mouse_urxvt_off] disables URXVT mouse mode. *)

val mouse_sgr_pixels_on : string
(** [mouse_sgr_pixels_on] enables SGR pixel-precise mouse mode. *)

val mouse_sgr_pixels_off : string
(** [mouse_sgr_pixels_off] disables SGR pixel-precise mouse mode. *)

val bracketed_paste_on : string
(** [bracketed_paste_on] enables bracketed paste mode.

    Pasted text is wrapped in escape sequences. Allows distinguishing typed
    input from pastes. *)

val bracketed_paste_off : string
(** [bracketed_paste_off] disables bracketed paste mode.

    Pasted text appears as normal input. Cannot distinguish pastes from typing.
*)

val kitty_keyboard_on : string
(** [kitty_keyboard_on] enables enhanced keyboard reporting (Kitty keyboard
    protocol).

    Allows terminals to send distinct escape sequences for key combinations like
    Shift+Enter that are normally indistinguishable. *)

val kitty_keyboard_off : string
(** [kitty_keyboard_off] disables enhanced keyboard reporting.

    Returns to standard keyboard input mode. *)

val push_kitty_keyboard_flags : int -> string
(** [push_kitty_keyboard_flags flags] enables Kitty keyboard protocol with
    specific flags.

    Pushes current keyboard state and enables protocol with given flags
    (bitmask). See [Kitty_keyboard_flags] module for flag values. *)

val pop_kitty_keyboard : int -> string
(** [pop_kitty_keyboard count] pops keyboard state from stack.

    Pops [count] entries from keyboard state stack. *)

val query_kitty_keyboard : string
(** [query_kitty_keyboard] queries current Kitty keyboard protocol flags.

    Terminal responds with current flag values. *)

(** {2 Kitty Keyboard Protocol Flags} *)

module Kitty_keyboard_flags : sig
  val disambiguate_escape_codes : int
  (** Flag to disambiguate escape codes (1) *)

  val report_event_types : int
  (** Flag to report key press/release/repeat events (2) *)

  val report_alternate_keys : int
  (** Flag to report alternate key values (4) *)

  val report_all_keys_as_escape_codes : int
  (** Flag to report all keys as escape codes (8) *)

  val report_associated_text : int
  (** Flag to report associated text with key events (16) *)
end

val focus_event_on : string
(** [focus_event_on] enables focus event reporting (xterm extension).

    Terminal sends sequences on focus in/out. *)

val focus_event_off : string
(** [focus_event_off] disables focus event reporting. *)

val synchronized_update_on : string
(** [synchronized_update_on] enables synchronized updates to reduce flicker (DEC
    extension). *)

val synchronized_update_off : string
(** [synchronized_update_off] disables synchronized updates. *)

val save_screen : string
(** [save_screen] saves current screen to buffer (extension; alternative to alt
    screen). *)

val restore_screen : string
(** [restore_screen] restores saved screen content (extension). *)

val set_window_title : string -> string
(** [set_window_title title] creates escape sequence to set terminal window
    title.

    Uses OSC (Operating System Command) sequence. Support varies by terminal
    emulator. Title typically appears in window title bar or tab. Special
    characters should be escaped. *)

(** {2 Window Manipulation (XTWINOPS)} *)

val window_deiconify : string
(** [window_deiconify] restores window from minimized/iconified state.

    Un-minimizes the terminal window. Part of xterm window operations (CSI 1 t).
    Support varies by terminal and window manager. *)

val window_iconify : string
(** [window_iconify] minimizes/iconifies the terminal window.

    Minimizes window to taskbar/dock. Part of xterm window operations (CSI 2 t).
    Support varies by terminal and window manager. *)

val window_move : int -> int -> string
(** [window_move x y] moves window to screen position [x], [y].

    Coordinates are in pixels from top-left of screen. Part of xterm window
    operations (CSI 3 ; x ; y t). Support varies by terminal and window manager.
    May be restricted by security policies. *)

val window_resize : int -> int -> string
(** [window_resize rows cols] resizes terminal text area to [rows] by [cols].

    Resizes the text area (not window) to specified character dimensions. Part
    of xterm window operations (CSI 8 ; rows ; cols t). Support varies by
    terminal. Clamped to reasonable limits by terminal. *)

val window_maximize : string
(** [window_maximize] maximizes the terminal window.

    Expands window to fill available screen space. Part of xterm window
    operations (CSI 9 ; 1 t). Support varies by terminal and window manager. *)

val window_unmaximize : string
(** [window_unmaximize] restores window from maximized state.

    Returns window to normal size. Part of xterm window operations (CSI 9 ; 0
    t). Support varies by terminal and window manager. *)

val window_fullscreen : string
(** [window_fullscreen] enters fullscreen mode.

    Expands window to cover entire screen including decorations. Part of xterm
    window operations (CSI 10 ; 1 t). Support varies by terminal. *)

val window_exit_fullscreen : string
(** [window_exit_fullscreen] exits fullscreen mode.

    Returns to windowed mode. Part of xterm window operations (CSI 10 ; 0 t).
    Support varies by terminal. *)

val window_push_title : string
(** [window_push_title] saves current window and icon titles to stack.

    Allows restoring titles later. Part of xterm window operations (CSI 22 ; 0
    t). Terminal maintains internal stack of titles. *)

val window_pop_title : string
(** [window_pop_title] restores window and icon titles from stack.

    Pops and sets previously saved titles. Part of xterm window operations (CSI
    23 ; 0 t). No effect if stack is empty. *)

(** {2 Keyboard Modes} *)

val application_keypad_on : string
(** [application_keypad_on] enables application keypad mode.

    Keypad sends escape sequences instead of numbers. *)

val application_keypad_off : string
(** [application_keypad_off] disables application keypad mode.

    Keypad sends normal numeric characters. *)

val application_cursor_keys_on : string
(** [application_cursor_keys_on] enables application cursor keys mode.

    Cursor keys send SS3 sequences instead of CSI. *)

val application_cursor_keys_off : string
(** [application_cursor_keys_off] disables application cursor keys mode.

    Cursor keys send normal CSI sequences. *)

val modify_other_keys_off : string
(** [modify_other_keys_off] disables modifyOtherKeys mode. *)

val modify_other_keys_1 : string
(** [modify_other_keys_1] enables modifyOtherKeys mode 1.

    Special keys with modifiers send escape sequences. *)

val modify_other_keys_2 : string
(** [modify_other_keys_2] enables modifyOtherKeys mode 2.

    All keys with modifiers send escape sequences. *)

(** {2 Device Status Requests} *)

val request_cursor_position : string
(** [request_cursor_position] requests current cursor position.

    Terminal responds with CSI row ; col R. *)

val request_device_attributes : string
(** [request_device_attributes] requests primary device attributes.

    Terminal reports device identification. *)

val request_secondary_device_attributes : string
(** [request_secondary_device_attributes] requests secondary device attributes.

    Terminal reports more detailed identification. *)

val request_terminal_size_chars : string
(** [request_terminal_size_chars] requests terminal text area size in characters
    (extension).

    Terminal reports CSI 8 ; height ; width t. *)

val request_terminal_size_pixels : string
(** [request_terminal_size_pixels] requests terminal size in pixels (extension).

    Terminal reports CSI 4 ; height ; width t. *)

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

val reset_intensity : string
(** [reset_intensity] alias for [reset_bold_dim]. *)

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

val reset_conceal : string
(** [reset_conceal] removes conceal attribute only.

    Makes hidden text visible again. Other attributes preserved. *)

val reset_strikethrough : string
(** [reset_strikethrough] removes strikethrough attribute only.

    Removes line through text. Other attributes preserved. *)

val reset_overline : string
(** [reset_overline] removes overline attribute only.

    Removes line above text. Other attributes preserved. *)

val reset_framed_encircled : string
(** [reset_framed_encircled] removes framed and encircled attributes.

    Removes frame or circle around text. Other attributes preserved. *)

(** {2 DEC Cursor Save/Restore} *)

val dec_cursor_save : string
(** [dec_cursor_save] saves cursor position using DEC sequence (DECSC).

    Uses ESC 7 instead of ESC [ s. Some applications specifically expect the DEC
    variant. Functionality is equivalent to [cursor_save]. *)

val dec_cursor_restore : string
(** [dec_cursor_restore] restores cursor position using DEC sequence (DECRC).

    Uses ESC 8 instead of ESC [ u. Some applications specifically expect the DEC
    variant. Functionality is equivalent to [cursor_restore]. *)

(** {2 SGR Stack (XTPUSHSGR/XTPOPSGR)} *)

val push_sgr : string
(** [push_sgr] pushes current SGR attributes onto stack.

    Saves all current text attributes (colors, styles) for later restoration.
    Part of xterm SGR stack extension (CSI # {). More powerful than single
    save/restore. *)

val pop_sgr : string
(** [pop_sgr] pops and restores SGR attributes from stack.

    Restores previously pushed text attributes. Part of xterm SGR stack extension
    (CSI # }). No effect if stack is empty. *)

(** {1 Low-level Constants and Functions} *)

val esc : string
(** [esc] is the ANSI CSI (Control Sequence Introducer) prefix "\x1b[".
    
    This is the standard prefix for most ANSI escape sequences. Useful when
    building custom escape sequences not covered by this module.
    
    Example:
    {[
      let custom_seq = Ansi.esc ^ "38;5;196m" (* 256-color red *)
    ]} *)

val csi_params : int list -> string -> string
(** [csi_params params cmd] creates a CSI sequence with the given parameters.
    
    Generates escape sequence in format "ESC[{params separated by ;}{cmd}".
    Empty params list produces "ESC[{cmd}". Used internally. *)

val csi : int -> string -> string
(** [csi n cmd] creates a CSI sequence with single numeric parameter (alias
    using [csi_params]). *)

val csi2 : int -> int -> string -> string
(** [csi2 n m cmd] creates a CSI sequence with two numeric parameters (alias
    using [csi_params]). *)

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

    Wraps text with SGR sequences for attributes and appends targeted reset.
    Ensures styles don't leak. Empty attribute list returns unchanged string.
    Handles multi-line strings correctly.

    Example: Creates error message with red bold text.
    {[
      let error_msg = Ansi.style [ `Bold; `Fg Red ] "Error: File not found"
    ]} *)
