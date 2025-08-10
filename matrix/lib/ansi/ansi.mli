(** ANSI escape sequences for terminal control.

    Ansi provides type-safe generation of ANSI escape sequences following
    ECMA-48/ANSI X3.64 standards. Functions produce escape sequences as strings
    for colors, styles, cursor control, and screen management.

    {1 Ansi in a Nutshell}

    Create styled text:
    {[
      let error = Ansi.style [ `Bold; `Fg Red ] "Error: file not found"
      let link = Ansi.hyperlink ~uri:"https://example.com" "View docs"
    ]}

    Control cursor and screen:
    {[
      print_string Ansi.clear_screen;
      print_string (Ansi.cursor_position 10 20);
      print_string Ansi.cursor_hide
    ]}

    {1 Key Concepts}

    {2 Terminal Compatibility}

    Most sequences follow ECMA-48 standard. Extensions are explicitly noted.
    Support varies:
    - Basic colors and cursor movement: universal
    - 256-color and RGB: most modern terminals
    - Advanced features (double underline, overline): limited support
    - Mouse and keyboard protocols: terminal-specific

    {2 Escape Sequence Structure}

    Sequences use CSI format: [ESC[{params}{command}]]. Parameters are
    semicolon-separated numbers. This module uses semicolon separators for RGB
    colors (e.g., [38;2;r;g;b]) for broader compatibility instead of colon
    separators from the standard.

    {2 Mental Model}

    Think of ANSI sequences as invisible commands embedded in text streams. When
    a terminal encounters these sequences, it performs actions (move cursor,
    change color) rather than displaying them. This module generates these
    command strings for you to print alongside your text.

    Common patterns:
    - Style text: [sgr] → text → reset
    - Position cursor: [cursor_position] → print content
    - Clear and redraw: [clear_screen] → [cursor_position] → render

    {2 Value Clamping}

    - Color indices: clamped to 0-255
    - RGB values: clamped to 0-255 per channel
    - Negative counts: treated as 0
    - Invalid positions: clamped to valid range

    All functions produce valid sequences. No function raises exceptions for
    out-of-range values except {!set_scrolling_region}.

    {2 Common Pitfalls}

    - {b Forgetting to reset attributes}: Always pair styling with appropriate
      resets
    - {b Assuming universal support}: Test advanced features on target terminals
    - {b Not handling mouse/keyboard cleanup}: Always disable modes on exit
    - {b Mixing coordinate systems}: Remember positions are 1-based, not 0-based

    Example of proper cleanup:
    {[
      try
        print_string Ansi.mouse_on;
        print_string Ansi.alternate_screen_on (* Application logic *)
      with exn ->
        print_string Ansi.mouse_off;
        print_string Ansi.alternate_screen_off;
        raise exn
    ]} *)

(** {1 Styles} *)

module Style = Style
(** @inline *)

(** Re-export common types for convenience *)
type color = Style.color =
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
  | RGBA of int * int * int * int
      (** 32-bit RGBA color with alpha channel for blending *)

(** {1 Cursor Control} *)

(** {2 Relative Movement} *)

val cursor_up : int -> string
(** [cursor_up n] moves cursor up [n] lines.

    Cursor remains in same column. Stops at screen edge. Negative values treated
    as 0. Uses CUU (Cursor Up) sequence.

    Example:
    {[
      print_string (Ansi.cursor_up 5) (* Move up 5 lines *)
    ]} *)

val cursor_down : int -> string
(** [cursor_down n] moves cursor down [n] lines.

    Cursor remains in same column. Stops at screen edge. Negative values treated
    as 0. Uses CUD (Cursor Down) sequence.

    Example:
    {[
      print_string (Ansi.cursor_down 3) (* Move down 3 lines *)
    ]} *)

val cursor_forward : int -> string
(** [cursor_forward n] moves cursor forward [n] columns.

    Stays on current line. Stops at screen edge. Negative values treated as 0.
    Uses CUF (Cursor Forward) sequence.

    Example:
    {[
      print_string (Ansi.cursor_forward 10) (* Move right 10 columns *)
    ]} *)

val cursor_back : int -> string
(** [cursor_back n] moves cursor backward [n] columns.

    Stays on current line. Stops at screen edge. Negative values treated as 0.
    Uses CUB (Cursor Back) sequence.

    Example:
    {[
      print_string (Ansi.cursor_back 5) (* Move left 5 columns *)
    ]} *)

val cursor_next_line : int -> string
(** [cursor_next_line n] moves cursor to beginning of line [n] lines down.

    Equivalent to moving down [n] lines then to column 1. Negative values
    treated as 0. Uses CNL (Cursor Next Line) sequence.

    Example:
    {[
      print_string (Ansi.cursor_next_line 2) (* Start of line 2 lines down *)
    ]} *)

val cursor_previous_line : int -> string
(** [cursor_previous_line n] moves cursor to beginning of line [n] lines up.

    Equivalent to moving up [n] lines then to column 1. Negative values treated
    as 0. Uses CPL (Cursor Previous Line) sequence.

    Example:
    {[
      print_string (Ansi.cursor_previous_line 1) (* Start of previous line *)
    ]} *)

(** {2 Absolute Positioning} *)

val cursor_horizontal_absolute : int -> string
(** [cursor_horizontal_absolute col] moves cursor to column [col] on current
    row.

    Column is 1-based (leftmost is 1). Values ≤0 treated as 1. Uses CHA (Cursor
    Horizontal Absolute) sequence.

    Example:
    {[
      print_string
        (Ansi.cursor_horizontal_absolute 40) (* Column 40 *)
        print_string
        (Ansi.cursor_horizontal_absolute 1)
      (* Start of line *)
    ]} *)

val cursor_vertical_absolute : int -> string
(** [cursor_vertical_absolute row] moves cursor to row [row] keeping current
    column.

    Row is 1-based (top is 1). Values ≤0 treated as 1. Uses VPA (Vertical
    Position Absolute) sequence.

    Example:
    {[
      print_string (Ansi.cursor_vertical_absolute 10) (* Row 10, same column *)
    ]} *)

val cursor_position : int -> int -> string
(** [cursor_position row col] moves cursor to absolute position.

    Both coordinates are 1-based (top-left is 1,1). Values ≤0 treated as 1. Uses
    CUP (Cursor Position) sequence.

    Example:
    {[
      print_string
        (Ansi.cursor_position 1 1) (* Home position *)
        print_string
        (Ansi.cursor_position 10 40)
      (* Row 10, column 40 *)
    ]} *)

(** {2 Save and Restore} *)

val cursor_save : string
(** [cursor_save] saves current cursor position.

    Saves position for later restoration with {!cursor_restore}. Most terminals
    support one saved position. Uses ANSI SCP sequence. See also
    {!dec_cursor_save} for DEC variant and {!push_sgr} for saving text
    attributes separately from position.

    Example:
    {[
      print_string Ansi.cursor_save;
      print_string (Ansi.cursor_position 10 10);
      print_string "Temporary text";
      print_string Ansi.cursor_restore (* Back to saved position *)
    ]} *)

val cursor_restore : string
(** [cursor_restore] restores previously saved cursor position.

    Returns to position saved by {!cursor_save}. No effect if position never
    saved. Uses ANSI RCP sequence. See also {!dec_cursor_restore} for DEC
    variant. *)

(** {2 Visibility} *)

val cursor_show : string
(** [cursor_show] makes cursor visible.

    Shows cursor after {!cursor_hide}. Visibility is terminal state, not saved
    with position. Uses DECTCEM sequence.

    Example:
    {[
      print_string Ansi.cursor_hide;
      (* Render without cursor flicker *)
      print_string Ansi.cursor_show
    ]} *)

val cursor_hide : string
(** [cursor_hide] makes cursor invisible.

    Cursor still moves but not displayed. Useful during rendering to prevent
    flicker. Remember to restore visibility. Uses DECTCEM sequence. *)

(** {2 Tab Movement} *)

val cursor_tab : int -> string
(** [cursor_tab n] advances cursor to next tab stop [n] times.

    Moves forward to nth next tab stop. Default tab stops every 8 columns.
    Negative values treated as 0. Uses CHT (Cursor Horizontal Tab) sequence.

    Example:
    {[
      print_string (Ansi.cursor_tab 2) (* Advance 2 tab stops *)
    ]} *)

val cursor_back_tab : int -> string
(** [cursor_back_tab n] moves cursor to previous tab stop [n] times.

    Moves backward to nth previous tab stop. Negative values treated as 0. Uses
    CBT (Cursor Backward Tab) sequence. *)

(** {2 Cursor Shape} *)

val set_cursor_style_blinking_block : string
(** [set_cursor_style_blinking_block] sets cursor to blinking block shape.

    Full character cell cursor that blinks. Most visible style. DECSCUSR 1.
    Support varies by terminal.

    Example:
    {[
      print_string Ansi.set_cursor_style_blinking_block
    ]} *)

val set_cursor_style_steady_block : string
(** [set_cursor_style_steady_block] sets cursor to steady block shape.

    Full character cell cursor without blinking. High visibility without
    distraction. DECSCUSR 2. *)

val set_cursor_style_blinking_underline : string
(** [set_cursor_style_blinking_underline] sets cursor to blinking underline
    shape.

    Thin line at character baseline that blinks. Common for insert mode.
    DECSCUSR 3. *)

val set_cursor_style_steady_underline : string
(** [set_cursor_style_steady_underline] sets cursor to steady underline shape.

    Thin line at character baseline without blinking. Less intrusive than block.
    DECSCUSR 4. *)

val set_cursor_style_blinking_bar : string
(** [set_cursor_style_blinking_bar] sets cursor to blinking vertical bar shape.

    Thin vertical line between characters that blinks. Modern editor style.
    DECSCUSR 5. *)

val set_cursor_style_steady_bar : string
(** [set_cursor_style_steady_bar] sets cursor to steady vertical bar shape.

    Thin vertical line between characters without blinking. Minimal and precise.
    DECSCUSR 6. *)

val set_cursor_style_default : string
(** [set_cursor_style_default] resets cursor to terminal's default style.

    Returns cursor to user's configured preference. DECSCUSR 0. *)

(** {1 Screen Control} *)

(** {2 Display Clearing} *)

val erase_in_display : int -> string
(** [erase_in_display param] erases part of the display.

    Parameters:
    - 0: from cursor to end of screen (below)
    - 1: from start of screen to cursor (above)
    - 2: entire visible screen
    - 3: entire screen including scrollback buffer (xterm extension)

    Invalid parameters default to 2. Cursor position unchanged. Uses ED (Erase
    in Display) sequence.

    Example:
    {[
      print_string
        (Ansi.erase_in_display 0) (* Clear below cursor *)
        print_string
        (Ansi.erase_in_display 3)
      (* Clear everything *)
    ]} *)

val clear_screen : string
(** [clear_screen] clears entire visible screen.

    Alias for [erase_in_display 2]. Cursor position unchanged. Most common clear
    operation.

    Example:
    {[
      print_string Ansi.clear_screen;
      print_string (Ansi.cursor_position 1 1)
      (* Often paired with home *)
    ]} *)

val clear_screen_above : string
(** [clear_screen_above] clears from top of screen to cursor.

    Alias for [erase_in_display 1]. Cursor position unchanged. *)

val clear_screen_below : string
(** [clear_screen_below] clears from cursor to bottom of screen.

    Alias for [erase_in_display 0]. Cursor position unchanged. *)

val clear_terminal : string
(** [clear_terminal] clears screen, scrollback, and homes cursor.

    Combines clear screen (ED 2), clear scrollback (ED 3), and home cursor. Not
    universally supported; terminals without ED 3 support will only clear
    visible screen. Common in terminal reset operations. *)

(** {2 Line Clearing} *)

val erase_in_line : int -> string
(** [erase_in_line param] erases part of the current line.

    Parameters:
    - 0: from cursor to end of line (right)
    - 1: from start of line to cursor (left)
    - 2: entire line

    Invalid parameters default to 2. Cursor position unchanged. Uses EL (Erase
    in Line) sequence.

    Example:
    {[
      print_string (Ansi.erase_in_line 0) (* Clear rest of line *)
    ]} *)

val clear_line : string
(** [clear_line] clears entire current line.

    Alias for [erase_in_line 2]. Cursor remains at current position. Useful for
    status line updates.

    Example:
    {[
      print_string "\r";
      (* Return to start *)
      print_string Ansi.clear_line;
      print_string "New status" (* Replace line content *)
    ]} *)

val clear_line_left : string
(** [clear_line_left] clears from start of line to cursor.

    Alias for [erase_in_line 1]. Cursor position unchanged. *)

val clear_line_right : string
(** [clear_line_right] clears from cursor to end of line.

    Alias for [erase_in_line 0]. Cursor position unchanged. *)

(** {2 Scrolling} *)

val scroll_up : int -> string
(** [scroll_up n] scrolls screen content up [n] lines.

    Content moves up; blank lines appear at bottom. Content scrolled off top is
    lost. Cursor stays at same screen position. Negative values treated as 0.
    Uses SU (Scroll Up) sequence.

    Example:
    {[
      print_string (Ansi.scroll_up 3) (* Scroll content up 3 lines *)
    ]} *)

val scroll_down : int -> string
(** [scroll_down n] scrolls screen content down [n] lines.

    Content moves down; blank lines appear at top. Content scrolled off bottom
    is lost. Cursor stays at same screen position. Negative values treated as 0.
    Uses SD (Scroll Down) sequence. *)

val set_scrolling_region : ?top:int -> ?bottom:int -> unit -> string
(** [set_scrolling_region ~top ~bottom ()] sets scrolling region.

    Restricts scrolling to rows [top] through [bottom] (inclusive). Both
    1-based. Default [top] is 1. Omitting [bottom] resets to full screen. Uses
    DECSTBM sequence.

    @raise Invalid_argument
      if [top < 1] or [bottom <= top]. This is the only function in the module
      that raises exceptions for invalid input; all others clamp values.

    Example:
    {[
      (* Valid usage *)
      print_string (Ansi.set_scrolling_region ~top:5 ~bottom:20 ());

      (* This raises Invalid_argument *)
      print_string
        (Ansi.set_scrolling_region ~top:20 ~bottom:5 ())
        (* Reset to full screen *)
        print_string
        (Ansi.set_scrolling_region ())
    ]} *)

(** {2 Line and Character Manipulation} *)

val insert_lines : int -> string
(** [insert_lines n] inserts [n] blank lines at cursor row.

    Lines at and below cursor move down. Bottom lines scroll off. Negative
    values treated as 0. Uses IL (Insert Line) sequence.

    Example:
    {[
      print_string (Ansi.insert_lines 2) (* Insert 2 blank lines *)
    ]} *)

val delete_lines : int -> string
(** [delete_lines n] deletes [n] lines starting at cursor row.

    Lines below move up. Blank lines appear at bottom. Negative values treated
    as 0. Uses DL (Delete Line) sequence. *)

val insert_characters : int -> string
(** [insert_characters n] inserts [n] spaces at cursor position.

    Characters at and after cursor shift right. Rightmost characters may be
    lost. Negative values treated as 0. Uses ICH (Insert Character) sequence. *)

val delete_characters : int -> string
(** [delete_characters n] deletes [n] characters at cursor position.

    Characters after deletion point shift left. Spaces appear at line end.
    Negative values treated as 0. Uses DCH (Delete Character) sequence. *)

val erase_characters : int -> string
(** [erase_characters n] replaces [n] characters with spaces.

    Characters are overwritten with spaces starting at cursor. No shifting
    occurs. Negative values treated as 0. Uses ECH (Erase Character) sequence.
*)

val repeat_last_character : int -> string
(** [repeat_last_character n] repeats last printed character [n] times.

    Terminal must have received a printable character to repeat. No effect if
    last output was control sequence. Negative values treated as 0. Uses REP
    (Repeat) sequence. Limited support.

    Example:
    {[
      print_char '-';
      print_string (Ansi.repeat_last_character 79)
      (* Line of dashes *)
    ]} *)

(** {2 Tab Stops} *)

val set_tab_stop : string
(** [set_tab_stop] sets tab stop at current cursor column.

    Creates custom tab stop. Default stops are every 8 columns. Uses HTS
    (Horizontal Tab Set) sequence.

    Example:
    {[
      print_string (Ansi.cursor_horizontal_absolute 20);
      print_string Ansi.set_tab_stop (* Tab stop at column 20 *)
    ]} *)

val clear_tab_stop_at_cursor : string
(** [clear_tab_stop_at_cursor] clears tab stop at current column.

    Removes custom tab stop if present. Uses TBC 0 (Tab Clear) sequence. *)

val clear_tab_stop : int -> string
(** [clear_tab_stop param] clears tab stops.

    Parameters:
    - 0: at current column only
    - 3: all tab stops

    Invalid parameters default to 0. Uses TBC (Tab Clear) sequence. *)

val clear_all_tab_stops : string
(** [clear_all_tab_stops] clears all tab stops.

    Removes all custom tab stops. Terminal reverts to default 8-column tabs.
    Alias for [clear_tab_stop 3]. *)

(** {1 Terminal Modes} *)

(** {2 Screen Buffers} *)

val alternate_screen_on : string
(** [alternate_screen_on] switches to alternate screen buffer.

    Saves main screen content and switches to blank alternate buffer. Used by
    full-screen applications (vim, less). Remember to restore before exit. Uses
    xterm extension (DECSET 1049).

    Example:
    {[
      print_string Ansi.alternate_screen_on;
      (* Full-screen app content *)
      print_string Ansi.alternate_screen_off (* Restore on exit *)
    ]} *)

val alternate_screen_off : string
(** [alternate_screen_off] returns to main screen buffer.

    Restores previously saved main screen. Alternate screen content is lost.
    Uses xterm extension (DECRST 1049). *)

(** {2 Mouse Support} *)

val mouse_on : string
(** [mouse_on] enables mouse event reporting.

    Enables normal tracking, button tracking, and SGR encoding. Terminal sends
    escape sequences for mouse events. Disables normal text selection. Remember
    to disable on exit.

    Example:
    {[
      print_string Ansi.mouse_on;
      (* Handle mouse events *)
      print_string Ansi.mouse_off (* Restore on exit *)
    ]} *)

val mouse_off : string
(** [mouse_off] disables mouse event reporting.

    Restores normal mouse selection. Disables all tracking modes enabled by
    {!mouse_on}. *)

(** {3 Mouse Protocols} *)

(** Mouse protocols control which events are reported and encoding format. Later
    protocols extend earlier ones. SGR protocol recommended for modern
    applications. *)

val mouse_x10_on : string
(** [mouse_x10_on] enables X10 mouse protocol.

    Reports button press only (no release/motion). Oldest protocol with
    coordinate limits. DECSET 9. *)

val mouse_x10_off : string
(** [mouse_x10_off] disables X10 mouse protocol. *)

val mouse_normal_on : string
(** [mouse_normal_on] enables normal tracking mode.

    Reports button press and release. No motion events. DECSET 1000. *)

val mouse_normal_off : string
(** [mouse_normal_off] disables normal mouse tracking. *)

val mouse_button_tracking_on : string
(** [mouse_button_tracking_on] enables button-event tracking.

    Reports press, release, and motion while buttons held. DECSET 1002. *)

val mouse_button_tracking_off : string
(** [mouse_button_tracking_off] disables button-event tracking. *)

val mouse_any_tracking_on : string
(** [mouse_any_tracking_on] enables any-event tracking.

    Reports all mouse motion even without buttons. High event volume. DECSET
    1003. *)

val mouse_any_tracking_off : string
(** [mouse_any_tracking_off] disables any-event tracking. *)

(** {3 Mouse Coordinate Encoding} *)

val mouse_utf8_off : string
(** [mouse_utf8_off] disables UTF-8 encoded mouse coordinates. *)

val mouse_utf8_on : string
(** [mouse_utf8_on] enables UTF-8 coordinate encoding.

    Extends coordinate range beyond 223. Conflicts with SGR mode. DECSET 1005.
*)

val mouse_sgr_on : string
(** [mouse_sgr_on] enables SGR mouse protocol.

    Modern protocol with unlimited coordinates. Text format. Recommended for new
    applications. DECSET 1006.

    Example:
    {[
      print_string Ansi.mouse_sgr_on
      (* Receive events like: ESC[<0;10;20M for press at (10,20) *)
    ]} *)

val mouse_sgr_off : string
(** [mouse_sgr_off] disables SGR mouse mode. *)

val mouse_urxvt_on : string
(** [mouse_urxvt_on] enables URXVT mouse protocol.

    Alternative extended protocol. Less common than SGR. DECSET 1015. *)

val mouse_urxvt_off : string
(** [mouse_urxvt_off] disables URXVT mouse mode. *)

val mouse_sgr_pixels_on : string
(** [mouse_sgr_pixels_on] enables pixel-precise SGR mode.

    Reports coordinates in pixels instead of characters. Requires SGR mode.
    DECSET 1016. *)

val mouse_sgr_pixels_off : string
(** [mouse_sgr_pixels_off] disables SGR pixel-precise mouse mode. *)

(** {2 Input Processing} *)

val bracketed_paste_on : string
(** [bracketed_paste_on] enables bracketed paste mode.

    Wraps pasted text in [ESC[200~] and [ESC[201~] markers. Allows safe handling of pasted content containing control sequences. Common in modern shells and editors. DECSET 2004.

    Example:
    {[
      print_string Ansi.bracketed_paste_on;
      (* Pasted text arrives as: ESC[200~<content>ESC[201~ *)
    ]} *)

val bracketed_paste_off : string
(** [bracketed_paste_off] disables bracketed paste mode.

    Pasted text appears as normal typed input. DECRST 2004. *)

(** {2 Keyboard Protocols} *)

val kitty_keyboard_on : string
(** [kitty_keyboard_on] enables Kitty keyboard protocol.

    Enhanced protocol distinguishing more key combinations (e.g., Shift+Enter vs
    Enter). Enables disambiguate mode. See {!Kitty_keyboard_flags} for options.

    Example:
    {[
      print_string Ansi.kitty_keyboard_on;
      (* Receive enhanced key events *)
      print_string Ansi.kitty_keyboard_off
    ]} *)

val kitty_keyboard_off : string
(** [kitty_keyboard_off] disables Kitty keyboard protocol.

    Returns to standard keyboard mode. Pops all pushed flags. *)

val push_kitty_keyboard_flags : int -> string
(** [push_kitty_keyboard_flags flags] pushes keyboard state and sets flags.

    Saves current state on stack. Flags are bitwise OR of
    {!Kitty_keyboard_flags} values. Stack allows nested protocol changes.

    Example:
    {[
      let flags =
        Kitty_keyboard_flags.(disambiguate_escape_codes lor report_event_types)
      in
      print_string (Ansi.push_kitty_keyboard_flags flags)
    ]} *)

val pop_kitty_keyboard : int -> string
(** [pop_kitty_keyboard count] pops [count] keyboard states.

    Restores previous keyboard configuration. Usually [count] is 1. *)

val query_kitty_keyboard : string
(** [query_kitty_keyboard] queries current keyboard flags.

    Terminal responds with [ESC[?{flags}u]. Parse response to determine active features. *)

(** {3 Kitty Keyboard Flags} *)

module Kitty_keyboard_flags : sig
  val disambiguate_escape_codes : int
  (** Disambiguate escape codes (value: 1).

      Distinguish Esc from Alt+key, Enter from Ctrl+M, etc. Most useful flag. *)

  val report_event_types : int
  (** Report event types (value: 2).

      Distinguish key press, release, and repeat events. *)

  val report_alternate_keys : int
  (** Report alternate key meanings (value: 4).

      Include shifted key values for non-ASCII keys. *)

  val report_all_keys_as_escape_codes : int
  (** Report all keys as escape codes (value: 8).

      Even printable ASCII sent as escape sequences. *)

  val report_associated_text : int
  (** Report associated text (value: 16).

      Include text that would be inserted by the key. *)
end

(** {2 Focus Tracking} *)

val focus_event_on : string
(** [focus_event_on] enables focus event reporting.

    Terminal sends [ESC[I] on focus gained, [ESC[O] on focus lost. Useful for pausing updates when unfocused. xterm extension (DECSET 1004).

    Example:
    {[
      print_string Ansi.focus_event_on;
      (* Handle ESC[I and ESC[O events *)
    ]} *)

val focus_event_off : string
(** [focus_event_off] disables focus event reporting.

    Stops focus change notifications. DECRST 1004. *)

(** {2 Synchronized Updates} *)

val synchronized_update_on : string
(** [synchronized_update_on] begins synchronized update mode.

    Terminal buffers output until {!synchronized_update_off}. Reduces flicker in
    complex updates. DEC extension (DECSET 2026). Support varies.

    Example:
    {[
      print_string Ansi.synchronized_update_on;
      (* Multiple drawing operations *)
      print_string Ansi.synchronized_update_off (* Atomic display update *)
    ]} *)

val synchronized_update_off : string
(** [synchronized_update_off] ends synchronized update mode.

    Flushes buffered output atomically. DECRST 2026. *)

(** {2 Screen Save/Restore} *)

val save_screen : string
(** [save_screen] saves current screen to buffer.

    Alternative to alternate screen. Saves content without switching buffers.
    Extension (DECSET 47). Limited support. *)

val restore_screen : string
(** [restore_screen] restores saved screen content.

    Restores content saved by {!save_screen}. Extension (DECRST 47). *)

(** {2 Window Control} *)

val set_window_title : string -> string
(** [set_window_title title] sets terminal window title.

    Uses OSC 0 sequence terminated with BEL (\x07). Title appears in window
    titlebar or tab. Some terminals restrict for security. Special characters in
    [title] are passed through unchanged; escape them if needed.

    Example:
    {[
      print_string (Ansi.set_window_title "My App - Processing...")
    ]} *)

(** {3 Window Manipulation} *)

(** xterm window operations (XTWINOPS). Support varies widely. Many terminals
    disable these for security. Test availability before relying on these
    features. *)

val window_deiconify : string
(** [window_deiconify] restores window from minimized state.

    Un-minimizes terminal window. Often disabled. Uses CSI 1 t. *)

val window_iconify : string
(** [window_iconify] minimizes terminal window.

    Minimizes to taskbar/dock. Often disabled. Uses CSI 2 t. *)

val window_move : int -> int -> string
(** [window_move x y] moves window to screen position.

    Coordinates in pixels from top-left. Often disabled for security. Uses CSI
    3;x;y t. *)

val window_resize : int -> int -> string
(** [window_resize rows cols] resizes terminal text area.

    Sets character grid size, not pixel size. Terminal may clamp values. Uses
    CSI 8;rows;cols t. *)

val window_maximize : string
(** [window_maximize] maximizes terminal window.

    Expands to available screen space. Uses CSI 9;1 t. *)

val window_unmaximize : string
(** [window_unmaximize] restores from maximized state.

    Returns to normal size. Uses CSI 9;0 t. *)

val window_fullscreen : string
(** [window_fullscreen] enters fullscreen mode.

    Covers entire screen. Uses CSI 10;1 t. *)

val window_exit_fullscreen : string
(** [window_exit_fullscreen] exits fullscreen mode.

    Returns to windowed mode. Uses CSI 10;0 t. *)

val window_push_title : string
(** [window_push_title] saves current titles to stack.

    Saves window and icon titles for later restoration. Uses CSI 22;0 t. *)

val window_pop_title : string
(** [window_pop_title] restores titles from stack.

    Pops and applies previously saved titles. No effect if stack empty. Uses CSI
    23;0 t. *)

(** {3 Keyboard Modes} *)

val application_keypad_on : string
(** [application_keypad_on] enables application keypad mode.

    Numeric keypad sends escape sequences instead of digits. Used by
    applications needing keypad navigation. Uses DECKPAM.

    Example:
    {[
      print_string Ansi.application_keypad_on
      (* Keypad arrows send ESC O A instead of ESC [ A *)
    ]} *)

val application_keypad_off : string
(** [application_keypad_off] disables application keypad mode.

    Keypad sends normal digits. Uses DECKPNM. *)

val application_cursor_keys_on : string
(** [application_cursor_keys_on] enables application cursor keys.

    Arrow keys send [ESC O] prefix instead of [ESC []. Used by some applications. DECSET 1. *)

val application_cursor_keys_off : string
(** [application_cursor_keys_off] disables application cursor keys.

    Arrow keys send normal [ESC [] sequences. DECRST 1. *)

(** {3 ModifyOtherKeys} *)

val modify_other_keys_off : string
(** [modify_other_keys_off] disables modifyOtherKeys mode.

    Returns to standard key reporting. xterm extension. *)

val modify_other_keys_1 : string
(** [modify_other_keys_1] enables modifyOtherKeys level 1.

    Reports modifiers for keys that don't naturally support them. *)

val modify_other_keys_2 : string
(** [modify_other_keys_2] enables modifyOtherKeys level 2.

    Reports modifiers for all keys including those that naturally vary. *)

(** {2 Terminal Queries} *)

val request_cursor_position : string
(** [request_cursor_position] requests cursor position report.

    Terminal responds with [ESC[{row};{col}R]. Parse response to get position. Uses DSR 6.

    Example:
    {[
      print_string Ansi.request_cursor_position;
      (* Read and parse response: ESC[10;25R means row 10, col 25 *)
    ]} *)

val request_device_attributes : string
(** [request_device_attributes] requests primary device attributes.

    Terminal identifies capabilities. Response varies by terminal. Uses DA1. *)

val request_secondary_device_attributes : string
(** [request_secondary_device_attributes] requests secondary attributes.

    More detailed terminal identification. Uses DA2. *)

val request_terminal_size_chars : string
(** [request_terminal_size_chars] requests size in characters.

    Terminal responds with [ESC[8;{height};{width}t]. xterm extension. *)

val request_terminal_size_pixels : string
(** [request_terminal_size_pixels] requests size in pixels.

    Terminal responds with [ESC[4;{height};{width}t]. xterm extension. *)

(** {1 Styling and Colors} *)

val sgr : Style.attr list -> string
(** [sgr attrs] creates SGR sequence for attributes.

    Combines multiple attributes efficiently. Empty list produces empty string.
    Later attributes override earlier ones for same property (e.g., color).

    Examples:
    {[
      let red_bold = Ansi.sgr [ `Bold; `Fg Red ]
      let rainbow = Ansi.sgr [ `Fg (RGB (255, 0, 255)); `Bg (Index 226) ]
      let reset_all = Ansi.sgr [ `Reset ]
    ]} *)

(** {2 Attribute Reset} *)

val reset : string
(** [reset] clears all attributes to defaults.

    Removes all colors, styles, and formatting. SGR 0. Most comprehensive reset.

    Example:
    {[
      print_string Ansi.reset (* Ensure clean state *)
    ]} *)

val reset_bold_dim : string
(** [reset_bold_dim] removes bold and dim attributes.

    Returns text to normal intensity. Colors and other styles preserved. SGR 22.
    Note: Also affects double underline on some terminals. *)

val reset_intensity : string
(** [reset_intensity] alias for {!reset_bold_dim}. *)

val reset_italic : string
(** [reset_italic] removes italic attribute.

    Returns text to upright. Other attributes preserved. SGR 23. *)

val reset_underline : string
(** [reset_underline] removes underline attributes.

    Clears single and double underlines. Other attributes preserved. SGR 24. *)

val reset_blink : string
(** [reset_blink] removes blink attribute.

    Stops text flashing. Other attributes preserved. SGR 25. *)

val reset_reverse : string
(** [reset_reverse] removes reverse video.

    Restores normal foreground/background. Other attributes preserved. SGR 27.
*)

val reset_conceal : string
(** [reset_conceal] removes conceal attribute.

    Makes hidden text visible. Other attributes preserved. SGR 28. *)

val reset_strikethrough : string
(** [reset_strikethrough] removes strikethrough.

    Removes line through text. Other attributes preserved. SGR 29. *)

val reset_overline : string
(** [reset_overline] removes overline attribute.

    Removes line above text. Other attributes preserved. SGR 55. *)

val reset_framed_encircled : string
(** [reset_framed_encircled] removes framed and encircled.

    Removes borders around text. Other attributes preserved. SGR 54. *)

(** {2 DEC Private Sequences} *)

val dec_cursor_save : string
(** [dec_cursor_save] saves cursor position (DEC variant).

    Uses [ESC 7] instead of CSI format. Some applications require this variant.
    Functionally equivalent to {!cursor_save}. DECSC. *)

val dec_cursor_restore : string
(** [dec_cursor_restore] restores cursor position (DEC variant).

    Uses [ESC 8] instead of CSI format. Paired with {!dec_cursor_save}. DECRC.
*)

(** {2 Attribute Stack} *)

val push_sgr : string
(** [push_sgr] pushes current SGR attributes to stack.

    Saves all text attributes for nested style changes. More flexible than
    single save/restore. xterm extension (XTPUSHSGR).

    Example:
    {[
      print_string (Ansi.sgr [ `Bold; `Fg Red ]);
      print_string Ansi.push_sgr;
      print_string (Ansi.sgr [ `Italic; `Fg Blue ]);
      print_string "Nested style";
      print_string Ansi.pop_sgr (* Back to bold red *)
    ]} *)

val pop_sgr : string
(** [pop_sgr] pops and restores SGR attributes from stack.

    Restores previously pushed attributes. No effect if stack empty. xterm
    extension (XTPOPSGR). *)

(** {1 Low-level Building Blocks} *)

val esc : string
(** [esc] is the CSI (Control Sequence Introducer) "\x1b\[".

    This is the ESC character followed by '\[', forming the CSI used in most
    ANSI sequences. Use when building custom sequences not provided by this
    module.

    Example:
    {[
      let custom = Ansi.esc ^ "38;5;196m" (* 256-color palette red *)
      let home = Ansi.esc ^ "H" (* Cursor home *)
    ]} *)

val csi_params : int list -> string -> string
(** [csi_params params cmd] builds CSI sequence with parameters.
    
    Creates "ESC[{params}{cmd}" where params are semicolon-separated. Empty params produces "ESC[{cmd}". Useful for custom sequences.
    
    Example:
    {[
      let sgr_seq = Ansi.csi_params [1; 31] "m"  (* Bold red *)
      let cup_seq = Ansi.csi_params [10; 20] "H" (* Cursor to 10,20 *)
    ]} *)

val csi : int -> string -> string
(** [csi n cmd] builds CSI sequence with single parameter.

    Shorthand for [csi_params [n] cmd]. *)

val csi2 : int -> int -> string -> string
(** [csi2 n m cmd] builds CSI sequence with two parameters.

    Shorthand for [csi_params [n; m] cmd]. *)

(** {1 High-level Utilities} *)

val strip : string -> string
(** [strip s] removes all ANSI escape sequences from [s].

    Returns plain text only. Handles CSI, OSC, and other escape types. Useful
    for display width calculation, logging, or text processing.
    
    @param s The string to strip. O(n) where n is string length.
    
    Implementation handles all standard ANSI escape types including:
    - CSI sequences (ESC[...)
    - OSC sequences (ESC]...BEL or ESC]...ST)
    - Single-character escapes (ESC + letter)
    - Other escape types (ESC P/X/^/_)
    
    Edge cases:
    - Incomplete sequences at string end are removed
    - Malformed sequences are partially stripped
    - Preserves all non-ANSI content including Unicode
    
    Examples:
    {[
      let plain = Ansi.strip "\x1b[31mRed\x1b[0m" (* "Red" *)
      let width = String.length (Ansi.strip styled_text)
      Ansi.strip "\x1b[31mRed\x1b[" = "Red"  (* Incomplete sequence removed *)
      Ansi.strip "Hello \x1b world" = "Hello  world"  (* Preserves lone ESC *)
    ]} *)

val hyperlink : uri:string -> string -> string
(** [hyperlink ~uri text] creates clickable hyperlink.

    Shows [text] as clickable link to [uri]. Uses OSC 8. Terminals without
    support show plain text. Encode URIs properly.

    Examples:
    {[
      let link = Ansi.hyperlink ~uri:"https://example.com" "Visit site"
      let file = Ansi.hyperlink ~uri:"file:///path/to/file" "Open file"
      let styled = Ansi.style [ `Bold ] (Ansi.hyperlink ~uri:"#" "Click here")
    ]} *)

val style : Style.attr list -> string -> string
(** [style attrs s] applies attributes to string with smart reset.

    Wraps text with SGR sequences and targeted reset codes. Only resets
    attributes that were set, preserving others. Empty attribute list returns
    unchanged string.

    Later attributes in the list override earlier ones for the same property.
    For example:
    {[
      (* Green overrides red *)
      Ansi.style [ `Fg Red; `Fg Green ] "text" (* Results in green text *)
    ]}

    Examples:
    {[
      let error = Ansi.style [ `Bold; `Fg Red ] "Error: file not found"

      let success =
        (* Preserves existing blue when adding bold *)
        Ansi.style [ `Fg Green ] "✓ Done" print_string (Ansi.sgr [ `Fg Blue ]);
        (* Still blue *)
        print_string (Ansi.style [ `Bold ] "Important")
    ]} *)

val rgb_of_color : color -> int * int * int
(** Best‑effort conversion of an [Ansi.color] ( [`Index n] or [`RGB …] ) into an
    [r,g,b] triple in the 0‑255 range. Other variants return (0,0,0). *)

(** {1 Pretty-printing} *)

val pp_attr : Format.formatter -> Style.attr -> unit
(** [pp_attr fmt attr] pretty-prints an attribute for debugging. *)

val pp_attrs : Format.formatter -> Style.attr list -> unit
(** [pp_attrs fmt attrs] pretty-prints a list of attributes for debugging. *)

(** {1 Equality} *)

val equal_color : color -> color -> bool
(** [equal_color c1 c2] returns true if the two colors are equal. *)

val equal_attr : Style.attr -> Style.attr -> bool
(** [equal_attr a1 a2] returns true if the two attributes are equal. *)

(** {1 Streaming Parser} *)

module Parser = Parser
(** @inline *)
