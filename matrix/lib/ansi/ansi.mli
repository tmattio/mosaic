(** High-level ANSI terminal styling and control.

    Unified interface for terminal styling, cursor control, and ANSI parsing.
    Build rich terminal UIs with colors, attributes, and screen manipulation.

    Use this module for CLI tools, terminal user interfaces (TUIs), progress
    bars, colored logging, or any application that needs terminal output
    control.

    {1 Quick Start}

    Style text with inline functions:
    {[
      print_endline (Ansi.styled ~fg:Color.red ~bold:true "Error!");
      print_endline
        (Ansi.hyperlink ~url:"https://ocaml.org" ~text:"Visit OCaml")
    ]}

    Build complex outputs with segments:
    {[
      let output =
        Ansi.render
          [
            (Style.make ~fg:Color.cyan ~bold:true (), "INFO:");
            (Style.default, " ");
            (Style.make ~fg:Color.white (), "Server started on port 8080");
          ]
    ]}

    Strip ANSI codes for logging or width calculation:
    {[
      let plain = Ansi.strip styled_output in
      let width = String.length plain
    ]}

    {1 Performance}

    This module provides two API layers with different performance
    characteristics:

    {2 High-Level API (this module)}

    Functions like {!styled}, {!render}, {!cursor_position}, etc. return strings
    and are designed for {b convenience over performance}. Each call allocates:
    - A closure for the escape sequence builder
    - A string for the result

    {b Use for}: CLI tools, one-off styling, logging, infrequent operations.

    {2 Low-Level API ({!Escape} + {!Sgr_state})}

    For performance-critical code (render loops, TUI frames), use the low-level
    modules directly:

    - {!Escape}: Buffer-based sequence builders with zero-allocation primitives
    - {!Sgr_state}: Terminal state tracker with delta encoding

    {b Example}: Zero-allocation render loop:
    {[
      let buf = Bytes.create 65536 in
      let writer = Escape.make buf in
      let state = Sgr_state.create () in

      (* In your render loop: *)
      for row = 0 to height - 1 do
        for col = 0 to width - 1 do
          (* Update style - only emits codes when style changes *)
          Sgr_state.update state writer ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r ~bg_g
            ~bg_b ~bg_a ~attrs ~link:None;
          (* Emit character *)
          Escape.emit (Escape.char c) writer
        done;
        Sgr_state.reset state
      done
    ]}

    {1 Contracts and Compatibility}

    - Cursor movement helpers clamp negative distances to zero and return an
      empty string when the movement would be a no-op.
    - Absolute coordinates are 1-based; values ≤ 0 are coerced to 1 to match
      terminal semantics.
    - Screen/line erase modes outside their documented ranges fall back to the
      most commonly supported mode.
    - All string-returning helpers are allocation-free for the no-op cases, so
      callers can emit them unconditionally.
    - The only API that raises is {!set_scrolling_region}, which follows DECSTBM
      by rejecting invalid bounds with [Invalid_argument].

    Functions use widely-supported ANSI escape sequences. Modern features such
    as truecolor, hyperlinks (OSC 8), or explicit width (OSC 66) degrade
    gracefully on terminals that ignore or partially implement the extensions.
    When targeting a specific capability level, run {!Color.downgrade} or query
    the terminal with the {!Escape} builders exposed below. *)

(** {1 Low-Level Modules}

    These modules provide fine-grained control over ANSI features. Use them when
    the high-level API is insufficient or when building custom abstractions.

    For performance-critical code, {!Escape} and {!Sgr_state} provide
    zero-allocation APIs suitable for render loops. *)

module Color = Color
(** Color representations and conversions.

    Supports 16 basic colors, 256-color palette, and truecolor RGB/RGBA. See
    {!Color} for details. *)

module Attr = Attr
(** Text attribute flags (bold, italic, underline, etc.).

    Efficient bit-flag representation with set operations. See {!Attr} for
    details. *)

module Style = Style
(** Complete styling with colors, attributes, and hyperlinks.

    Immutable style values with composition support. See {!Style} for details.
*)

module Parser = Parser
(** Streaming ANSI escape sequence parser.

    Tokenizes ANSI output into high-level tokens. Returns a list, so allocates
    proportionally to input size. See {!Parser} for details. *)

module Escape = Escape
(** Low-level escape sequence builders.

    Composable, buffer-based emitters for efficient sequence construction.
    Provides zero-allocation primitives ({!Escape.sgr_open}, {!Escape.sgr_code},
    etc.) for hot render loops. See {!Escape} for details. *)

module Sgr_state = Sgr_state
(** Terminal SGR state tracker.

    Tracks the terminal's current style state to minimize emitted SGR codes.
    Zero-allocation: {!Sgr_state.update} writes directly to an {!Escape.writer}
    without intermediate allocations. See {!Sgr_state} for details. *)

(** {1 High-Level Styling} *)

val styled :
  ?reset:bool ->
  ?fg:Color.t ->
  ?bg:Color.t ->
  ?bold:bool ->
  ?dim:bool ->
  ?italic:bool ->
  ?underline:bool ->
  ?blink:bool ->
  ?inverse:bool ->
  ?hidden:bool ->
  ?strikethrough:bool ->
  ?overline:bool ->
  ?double_underline:bool ->
  ?framed:bool ->
  ?encircled:bool ->
  ?link:string ->
  string ->
  string
(** [styled ?reset ?fg ?bg ... str] applies styles to [str].

    Builds a {!Style.t} under the hood and emits only the necessary SGR
    transitions. When [link] is provided, wraps the output in OSC 8 hyperlink
    sequences (open before text, close after).

    @param reset
      Controls style persistence after [str]. Defaults to [~reset:false] (style
      persists). Pass [~reset:true] to append reset sequence [\[ESC[0m]].
    @param link
      Optional URL for OSC 8 hyperlink. When provided, emits hyperlink open
      before the styled text and hyperlink close after.

    {[
      print_string (Ansi.styled ~fg:Color.red "Error: ");
      print_endline (Ansi.styled ~reset:true ~fg:Color.white "details");
      (* With hyperlink *)
      print_endline
        (Ansi.styled ~fg:Color.blue ~underline:true ~link:"https://ocaml.org"
           "OCaml")
    ]} *)

val hyperlink_start : ?params:string -> url:string -> unit -> string
(** [hyperlink_start ?params ~url ()] emits the OSC 8 start sequence.

    Useful for manually constructing hyperlinks spanning complex layouts. Must
    be paired with {!hyperlink_end} to close the hyperlink.

    @param params
      Optional key=value pairs (e.g., "id=link1") for terminal link
      identification. Defaults to empty string.
    @param url The URL to link to. *)

val hyperlink_end : string
(** [hyperlink_end] emits the OSC 8 terminator sequence.

    Closes the most recent hyperlink opened by {!hyperlink_start}. Must be
    paired with {!hyperlink_start}. *)

val hyperlink : url:string -> text:string -> string
(** [hyperlink ~url ~text] wraps [text] in an OSC 8 hyperlink.

    Thin wrapper over {!Escape.hyperlink} that produces a ready-to-print string.
    Hyperlinks degrade gracefully on terminals that ignore OSC 8.

    {[
      print_endline
        (Ansi.hyperlink ~url:"https://ocaml.org" ~text:"Visit OCaml")
    ]} *)

val render : ?hyperlinks_enabled:bool -> (Style.t * string) list -> string
(** [render ?hyperlinks_enabled segments] renders styled segments to a string.

    Emits minimal SGR transitions by tracking terminal state; identical
    consecutive styles generate no output. Automatically closes all open
    hyperlinks at the end. Always appends a final reset [\[ESC[0m]].

    @param hyperlinks_enabled
      When [false], suppresses OSC 8 sequences. Defaults to [true]. Useful for
      terminals without hyperlink support.
    @param segments List of [(style, text)] pairs.

    Example:
    {[
      let output =
        Ansi.render
          [
            (Style.make ~fg:Color.yellow (), "Warning:");
            (Style.default, " Low disk space");
          ]
    ]}

    {b Streaming}: For streaming rendering (e.g., in render loops), use
    {!Sgr_state} directly. It tracks terminal state including hyperlinks and
    emits minimal escape sequences. *)

(** {1 Parsing} *)

val parse : string -> Parser.token list
(** [parse s] tokenizes ANSI-encoded string [s].

    Parses escape sequences, control codes, and plain text into high-level
    tokens. See {!Parser.token} for token types. Parsing never raises; malformed
    sequences are emitted as {!Parser.Unknown} controls so callers can log or
    ignore them.

    {[
      let tokens = Ansi.parse "\027[1;31mError\027[0m"
      (* Returns: [SGR [`Bold; `Fg Red]; Text "Error"; SGR [`Reset]] *)
    ]} *)

val strip : string -> string
(** [strip s] removes all ANSI escape sequences from [s].

    Returns only plain text content. Useful for display width calculation,
    logging to files, or text processing. ESC bytes followed by unrecognized
    characters are consumed along with the character to avoid leaving partial
    escape sequences.

    Handles CSI ([ESC \[ ... ]), OSC ([ESC \] ... BEL/ST]), and other standard
    ANSI escape types. Incomplete sequences at string end are removed. Preserves
    Unicode.

    Time complexity: O(n).

    {[
      Ansi.strip "\x1b[31mRed\x1b[0m" = "Red";;

      (* Calculate display width *)
      let width = String.length (Ansi.strip styled_text)
    ]} *)

(** {1 Cursor Control} *)

(** {2 Movement} *)

val cursor_up : n:int -> string
(** [cursor_up ~n] moves cursor up.

    Cursor stays in same column. Stops at top edge. Negative or zero values
    return empty string. *)

val cursor_down : n:int -> string
(** [cursor_down ~n] moves cursor down.

    Cursor stays in same column. Stops at bottom edge. Negative or zero values
    return empty string. *)

val cursor_forward : n:int -> string
(** [cursor_forward ~n] moves cursor right.

    Cursor stays on same line. Stops at right edge. Negative or zero values
    return empty string. *)

val cursor_back : n:int -> string
(** [cursor_back ~n] moves cursor left.

    Cursor stays on same line. Stops at left edge. Negative or zero values
    return empty string. *)

(** {2 Positioning} *)

val cursor_position : row:int -> col:int -> string
(** [cursor_position ~row ~col] moves to absolute position.

    Uses 1-based coordinates where (1,1) is top-left. Values ≤0 are treated as
    1.

    Example:
    {[
      print_string (Ansi.cursor_position ~row:10 ~col:40);
      print_string "Text at position (10, 40)"
    ]} *)

val move_cursor_and_clear : row:int -> col:int -> string
(** [move_cursor_and_clear ~row ~col] moves the cursor then clears to the end of
    the screen.

    Atomic operation that repositions cursor then erases to screen end, avoiding
    flicker between separate calls. Equivalent to {!cursor_position} followed by
    {!erase_display} with mode 0. *)

(** {2 State} *)

val cursor_save : string
(** [cursor_save] saves cursor position (CSI s).

    Uses the ANSI.SYS/SCO sequence, widely supported by modern terminals. Must
    be paired with {!cursor_restore}. *)

val cursor_restore : string
(** [cursor_restore] restores cursor position saved by {!cursor_save} (CSI u).

    Behavior undefined if called without prior {!cursor_save}. *)

val cursor_next_line : n:int -> string
(** [cursor_next_line ~n] moves cursor down by [n] lines and to column 1.

    Negative or zero values return empty string. Equivalent to CSI E. *)

val cursor_previous_line : n:int -> string
(** [cursor_previous_line ~n] moves cursor up by [n] lines and to column 1.

    Negative or zero values return empty string. Equivalent to CSI F. *)

val cursor_horizontal_absolute : col:int -> string
(** [cursor_horizontal_absolute ~col] moves to absolute column on current row.

    1-based column, values ≤0 treated as 1. Equivalent to CHA. *)

val cursor_vertical_absolute : row:int -> string
(** [cursor_vertical_absolute ~row] moves to absolute row keeping the column.

    1-based row, values ≤0 treated as 1. Equivalent to VPA. *)

(** {2 Appearance} *)

val show_cursor : string
(** [show_cursor] makes the cursor visible. *)

val hide_cursor : string
(** [hide_cursor] makes the cursor invisible.

    Useful for cleaner output during animations or progress indicators. Always
    pair with {!show_cursor} to restore visibility before program exit to avoid
    leaving terminal cursor hidden. *)

val cursor_style : shape:int -> string
(** [cursor_style ~shape] sets the cursor shape (DECSCUSR).
    - 0: User default
    - 1: Blinking block
    - 2: Steady block
    - 3: Blinking underline
    - 4: Steady underline
    - 5: Blinking bar
    - 6: Steady bar *)

val default_cursor_style : string
(** [default_cursor_style] restores the terminal's default cursor appearance.

    Shape and blink behavior depend on terminal configuration. *)

val cursor_block : string
(** [cursor_block] selects a steady block cursor. *)

val cursor_block_blink : string
(** [cursor_block_blink] selects a blinking block cursor. *)

val cursor_line : string
(** [cursor_line] selects a steady bar cursor. *)

val cursor_line_blink : string
(** [cursor_line_blink] selects a blinking bar cursor. *)

val cursor_underline : string
(** [cursor_underline] selects a steady underline cursor. *)

val cursor_underline_blink : string
(** [cursor_underline_blink] selects a blinking underline cursor. *)

val cursor_color : r:int -> g:int -> b:int -> string
(** [cursor_color ~r ~g ~b] sets the cursor colour via OSC 12.

    Components outside \[0,255\] are clamped. The request is ignored by
    terminals that do not expose cursor colour control. Always pair with either
    {!reset_cursor_color} (for terminals supporting OSC 112) or
    {!reset_cursor_color_fallback} (for compatibility with terminals that only
    support OSC 12) during teardown. *)

val reset_cursor_color : string
(** [reset_cursor_color] resets cursor colour (OSC 112). *)

val reset_cursor_color_fallback : string
(** [reset_cursor_color_fallback] resets cursor colour using OSC 12 default. *)

(** {1 Screen Control} *)

val erase_display : mode:int -> string
(** [erase_display ~mode] erases part of the display.

    @param mode
      Erase mode:
      - 0: Cursor to end of screen
      - 1: Start of screen to cursor
      - 2: Entire screen (most common)
      - 3: Entire screen including scrollback buffer

    Mode clamped to [0-3]; invalid values use 2. Cursor position is unchanged.
*)

val scroll_up : n:int -> string
(** [scroll_up ~n] scrolls the viewport up by [n] lines (CSI S).

    Negative or zero values return empty string. *)

val scroll_down : n:int -> string
(** [scroll_down ~n] scrolls the viewport down by [n] lines (CSI T).

    Negative or zero values return empty string. *)

val erase_line : mode:int -> string
(** [erase_line ~mode] erases part of the current line.

    @param mode
      Erase mode:
      - 0: Cursor to end of line
      - 1: Start of line to cursor
      - 2: Entire line

    Invalid modes default to 2. Cursor position is unchanged. *)

val clear_and_home : string
(** [clear_and_home] clears the screen and moves cursor to top-left.

    Moves cursor to home (1, 1) then erases entire display. Equivalent to
    {!home} followed by {!erase_display} with mode 2. *)

val clear : string
(** [clear] clears the visible screen without moving cursor.

    Erases entire display (mode 2) but leaves cursor position unchanged. Use
    {!clear_and_home} to also move cursor to home. *)

val home : string
(** [home] moves the cursor to the home position. *)

val erase_below_cursor : string
(** [erase_below_cursor] erases from cursor to end of screen.

    Equivalent to [erase_display ~mode:0]. *)

val insert_lines : n:int -> string
(** [insert_lines ~n] inserts [n] blank lines at the cursor row (IL).

    Negative or zero values return empty string. *)

val delete_lines : n:int -> string
(** [delete_lines ~n] deletes [n] lines from the cursor row (DL).

    Negative or zero values return empty string. *)

val set_scrolling_region : top:int -> bottom:int -> string
(** [set_scrolling_region ~top ~bottom] constrains scrolling to [top..bottom].

    Both are 1-based. Raises [Invalid_argument] if bounds are invalid. *)

(** {1 Colors and Attributes} *)

val reset : string
(** [reset] resets all styling to terminal defaults.

    Emits [\[ESC [ 0 m]]. Clears colors and attributes, but not cursor position
    or screen content. *)

val set_foreground : r:int -> g:int -> b:int -> string
(** [set_foreground ~r ~g ~b] sets the foreground color using truecolor (SGR
    38;2). Components are clamped to [0, 255]. *)

val set_background : r:int -> g:int -> b:int -> string
(** [set_background ~r ~g ~b] sets the background color using truecolor (SGR
    48;2). Components outside \[0,255\] are clamped. *)

val reset_background : string
(** [reset_background] restores the terminal's default background (SGR 49). *)

val reset_foreground : string
(** [reset_foreground] restores the terminal's default foreground (SGR 39). *)

(** {1 Screen Buffers} *)

val enter_alternate_screen : string
(** [enter_alternate_screen] switches to the alternate screen buffer.

    Commonly used by full-screen applications (editors, pagers). The main screen
    content is preserved and restored with {!exit_alternate_screen}. *)

val exit_alternate_screen : string
(** [exit_alternate_screen] returns to the main screen buffer.

    Restores the screen state before {!enter_alternate_screen}. *)

(** {1 Terminal Properties} *)

val set_title : title:string -> string
(** [set_title ~title] sets the terminal window title (OSC 0). *)

val explicit_width : width:int -> text:string -> string
(** [explicit_width ~width ~text] emits OSC 66 explicit-width text.

    OSC 66 extension for explicit width (WezTerm, foot). Unsupported terminals
    ignore the sequence. [width] must be strictly positive; terminals treat zero
    or negative widths as implementation-defined behaviour. The text payload is
    emitted verbatim (no escaping); ensure [text] does not contain unescaped ST
    sequences (ESC \\) which would terminate the OSC prematurely. *)

(** {1 Terminal Modes} *)

val mouse_pixel_mode_on : string
(** [mouse_pixel_mode_on] enables pixel-precise mouse reporting.

    Enables button events (1002), motion (1003), focus (1004), and SGR pixel
    encoding (1016). Kitty extension. *)

val mouse_pixel_mode_off : string
(** [mouse_pixel_mode_off] disables pixel-precise mouse reporting.

    Disables button events (1002), motion (1003), focus (1004), and SGR pixel
    encoding (1016). *)

val bracketed_paste_on : string
(** [bracketed_paste_on] enables bracketed paste mode (DECSET 2004). *)

val bracketed_paste_off : string
(** [bracketed_paste_off] disables bracketed paste mode (DECRST 2004). *)

val focus_tracking_on : string
(** [focus_tracking_on] enables focus reporting (DECSET 1004). *)

val focus_tracking_off : string
(** [focus_tracking_off] disables focus reporting (DECRST 1004). *)

val sync_output_on : string
(** [sync_output_on] enables synchronized output (DECSET 2026). *)

val sync_output_off : string
(** [sync_output_off] disables synchronized output (DECRST 2026). *)

val unicode_mode_on : string
(** [unicode_mode_on] enables Unicode mode (DECSET 2027). *)

val unicode_mode_off : string
(** [unicode_mode_off] disables Unicode mode (DECRST 2027). *)

val mouse_tracking_on : string
(** [mouse_tracking_on] enables basic mouse tracking (DECSET 1000). *)

val mouse_tracking_off : string
(** [mouse_tracking_off] disables basic mouse tracking (DECRST 1000). *)

val mouse_button_tracking_on : string
(** [mouse_button_tracking_on] enables button-event tracking (DECSET 1002). *)

val mouse_button_tracking_off : string
(** [mouse_button_tracking_off] disables button-event tracking (DECRST 1002). *)

val mouse_motion_on : string
(** [mouse_motion_on] enables any-event mouse tracking (DECSET 1003). *)

val mouse_motion_off : string
(** [mouse_motion_off] disables any-event mouse tracking (DECRST 1003). *)

val mouse_sgr_mode_on : string
(** [mouse_sgr_mode_on] enables SGR mouse encoding (DECSET 1006). *)

val mouse_sgr_mode_off : string
(** [mouse_sgr_mode_off] disables SGR mouse encoding (DECRST 1006). *)

val mouse_x10_on : string
(** [mouse_x10_on] enables legacy X10 mouse tracking (DECSET 9). *)

val mouse_x10_off : string
(** [mouse_x10_off] disables legacy X10 mouse tracking (DECRST 9). *)

val urxvt_mouse_on : string
(** [urxvt_mouse_on] enables urxvt mouse reporting (DECSET 1015). *)

val urxvt_mouse_off : string
(** [urxvt_mouse_off] disables urxvt mouse reporting (DECRST 1015). *)

val color_scheme_set : string
(** [color_scheme_set] enables colour-scheme reporting. *)

val color_scheme_reset : string
(** [color_scheme_reset] disables colour-scheme reporting. *)

(** {1 Key Encoding} *)

val csi_u_on : string
(** [csi_u_on] enables CSI-u keyboard encoding (ESC \[ > 1 u). *)

val csi_u_off : string
(** [csi_u_off] disables CSI-u keyboard encoding (ESC \[ < 1 u). *)

val csi_u_push : flags:int -> string
(** [csi_u_push ~flags] enables kitty keyboard protocol with [flags] (ESC \[ >
    [flags] u). *)

val csi_u_pop : string
(** [csi_u_pop] disables kitty keyboard protocol (ESC \[ < u). *)

val modify_other_keys_on : string
(** [modify_other_keys_on] enables xterm modifyOtherKeys mode (CSI > 4 ; 1 m).
*)

val modify_other_keys_off : string
(** [modify_other_keys_off] disables xterm modifyOtherKeys mode (CSI > 4 ; 0 m).
*)

(** {1 Device and Capability Queries} *)

(** {2 Terminal and Device Information} *)

val request_cursor_position : string
(** [request_cursor_position] queries current cursor position (DSR 6). *)

val request_pixel_size : string
(** [request_pixel_size] queries terminal pixel dimensions.

    Emits CSI 14 t query. Terminal responds with [CSI 4 ; height ; width t].
    Parse response via {!Parser} or raw input. *)

val request_device_attributes : string
(** [request_device_attributes] queries device attributes (DA1). *)

val request_tertiary_device_attributes : string
(** [request_tertiary_device_attributes] queries tertiary device attributes
    (DA3). *)

val request_terminal_identity : string
(** [request_terminal_identity] queries terminal identity/version (XTVERSION).
*)

val request_device_status : string
(** [request_device_status] requests a generic status report (DSR 5). *)

(** {2 Feature and Protocol Support} *)

val request_csi_u_support : string
(** [request_csi_u_support] queries CSI-u keyboard encoding support. *)

val request_kitty_graphics_support : string
(** [request_kitty_graphics_support] probes kitty graphics protocol support. *)

val request_sixel_geometry : string
(** [request_sixel_geometry] queries for Sixel graphics geometry limits. *)

val request_explicit_width_support : string
(** [request_explicit_width_support] queries explicit-width support.

    OSC 66 extension (WezTerm, foot). Unsupported terminals ignore sequence. *)

val request_scaled_text_support : string
(** [request_scaled_text_support] queries scaled-text support.

    OSC 66 extension (WezTerm, foot). Unsupported terminals ignore sequence. *)

val request_color_scheme : string
(** [request_color_scheme] queries terminal colour-scheme support. *)

(** {2 Mode State Queries} *)

val request_focus_mode : string
(** [request_focus_mode] queries focus tracking mode (DECRQM 1004). *)

val request_sgr_pixels_mode : string
(** [request_sgr_pixels_mode] queries SGR pixel mouse mode (DECRQM 1016). *)

val request_bracketed_paste_mode : string
(** [request_bracketed_paste_mode] queries bracketed paste mode (DECRQM 2004).
*)

val request_sync_mode : string
(** [request_sync_mode] queries synchronized output mode (DECRQM 2026). *)

val request_unicode_mode : string
(** [request_unicode_mode] queries Unicode mode (DECRQM 2027). *)

val request_color_scheme_mode : string
(** [request_color_scheme_mode] queries color scheme mode (DECRQM 2031). *)

(** {2 Response Markers} *)

val bracketed_paste_start : string
(** [bracketed_paste_start] is the marker sequence emitted by the terminal at
    paste start.

    Applications read this sequence (ESC \[ 200 ~), not emit it. Used to detect
    paste boundaries when bracketed paste mode is enabled. *)

val bracketed_paste_end : string
(** [bracketed_paste_end] is the marker sequence emitted by the terminal at
    paste end.

    Applications read this sequence (ESC \[ 201 ~), not emit it. Marks the end
    of pasted content. *)
