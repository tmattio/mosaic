(** Low-level ANSI escape sequence builders.

    This module provides primitives for constructing ANSI escape sequences
    directly into a buffer using functional combinators. It is the
    high-performance backend for the library, optimized to minimize allocations
    and copying.

    Use this module when you need maximum performance or fine-grained control.
    For most terminal styling, prefer the high-level {!Ansi} module.

    {1 Overview}

    Sequences are represented as functions ([writer -> unit]) that append data
    to a pre-allocated buffer. This design provides:

    - {b Zero-allocation composition}: Combining sequences creates a new
      function, not a new string.
    - {b Safety}: Numeric parameters are automatically clamped to valid ranges.
    - {b Efficiency}: Output is written directly to the target buffer in a
      single pass.

    {1 Usage}

    A typical workflow: allocate a buffer, create a writer, compose sequences,
    then extract the result:

    {[
      let buf = Bytes.create 1024 in
      let writer = Escape.make buf in
      let seq = Escape.seq [
        Escape.cursor_position ~row:1 ~col:1;
        Escape.literal "Loading...";
      ] in
      Escape.emit seq writer;
      let result = Escape.slice writer
    ]}

    {1 Zero-Allocation Primitives}

    For hot render loops where even closure allocation matters, use the
    low-level SGR and hyperlink primitives that write directly to the buffer:

    {[
      (* Zero-allocation SGR: \027[0;38;2;255;0;0;1m *)
      Escape.sgr_open w;
      Escape.sgr_code w 0;
      (* reset *)
      Escape.sgr_sep w;
      Escape.sgr_code w 38;
      (* fg color *)
      Escape.sgr_sep w;
      Escape.sgr_code w 2;
      Escape.sgr_sep w;
      Escape.sgr_code w 255;
      (* red *)
      Escape.sgr_sep w;
      Escape.sgr_code w 0;
      Escape.sgr_sep w;
      Escape.sgr_code w 0;
      Escape.sgr_sep w;
      Escape.sgr_code w 1;
      (* bold *)
      Escape.sgr_close w;

      (* Zero-allocation hyperlinks *)
      Escape.hyperlink_open w "https://example.com";
      (* ... emit link text ... *)
      Escape.hyperlink_close w
    ]}

    See also {!Sgr_state} which wraps these primitives with state tracking. *)

(** {1 The Writer} *)

type writer
(** An abstract buffer writer.

    It wraps a [Bytes.t] buffer and manages a write position. It is mutable and
    not thread-safe. *)

val make : bytes -> writer
(** [make buf] creates a writer targeting [buf].

    {b Precondition}: [buf] must be large enough to contain the generated
    sequences. Operations raise [Invalid_argument] if the buffer capacity is
    exceeded. *)

val len : writer -> int
(** [len w] returns the number of bytes currently written to [w]. *)

val reset_pos : writer -> unit
(** [reset_pos w] resets the write position to zero.

    Allows reusing a writer without allocation. The underlying buffer is not
    modified; subsequent writes simply overwrite previous content. *)

val slice : writer -> bytes
(** [slice w] returns a fresh copy of the written data.

    Equivalent to [Bytes.sub buf 0 (len w)]. *)

(** {1 Sequence Combinators} *)

type t = writer -> unit
(** A sequence builder function. *)

val empty : t
(** [empty] is the identity sequence. It emits nothing. *)

val literal : string -> t
(** [literal s] emits the string [s] verbatim. *)

val char : char -> t
(** [char c] emits the character [c]. *)

val concat : t -> t -> t
(** [concat a b] emits sequence [a] followed by [b]. *)

val seq : t list -> t
(** [seq xs] emits the list of sequences [xs] in order. *)

val bytes : bytes -> off:int -> len:int -> t
(** [bytes b ~off ~len] writes a slice of bytes directly to the output.

    {b Zero-allocation}: Copies data immediately into the writer's buffer.
    @raise Invalid_argument if the slice is out of bounds. *)

val utf8 : int -> t
(** [utf8 codepoint] encodes a Unicode scalar value into UTF-8 and writes it.

    Handles 1-4 byte sequences. Invalid codepoints (negative, > 0x10FFFF, or
    surrogates in the range 0xD800-0xDFFF) are replaced with U+FFFD. *)

val emit : t -> writer -> unit
(** [emit seq w] applies the sequence [seq] to the writer [w]. *)

val to_string : t -> string
(** [to_string seq] converts the sequence to a string.

    Allocates a temporary buffer. Use [emit] for high-performance scenarios. *)

val to_buffer : t -> Buffer.t -> unit
(** [to_buffer seq buf] appends the sequence to an existing [Buffer.t].

    Convenience wrapper that allocates an intermediate string via {!to_string}.
    For high-performance scenarios, prefer {!emit} with a pre-allocated buffer.
*)

(** {1 Primitives} *)

val esc : string -> t
(** [esc body] emits a CSI sequence [ESC \[ body].

    Example: [esc "2J"] becomes ["\027\[2J"]. *)

val csi : params:string -> command:char -> t
(** [csi ~params ~command] emits [ESC \[ params command]. *)

val sgr : int list -> t
(** [sgr codes] emits an SGR sequence [ESC \[ codes m].

    Example: [sgr [1; 31]] becomes ["\027\[1;31m"].

    Note: [sgr []] emits nothing. Use [reset] or [sgr [0]] to clear attributes.
*)

val sgr_direct : ((int -> unit) -> unit) -> writer -> unit
(** [sgr_direct f w] emits SGR codes generated by callback [f].

    {b Optimization}: Avoids list allocation for dynamic styles. The callback
    [f] receives a [push] function; every call to [push code] appends that
    integer to the sequence parameter list. *)

(** {2 Low-level SGR Building}

    These primitives allow zero-allocation SGR sequence construction by writing
    directly to the buffer. Use when the closure overhead of {!sgr_direct} is
    unacceptable (e.g., in hot render loops).

    {b Usage}:
    {[
      (* Emit: \027[0;38;2;255;0;0m (reset + red foreground) *)
      Escape.sgr_open w;
      Escape.sgr_code w 0;
      Escape.sgr_sep w;
      Escape.sgr_code w 38;
      Escape.sgr_sep w;
      Escape.sgr_code w 2;
      Escape.sgr_sep w;
      Escape.sgr_code w 255;
      Escape.sgr_sep w;
      Escape.sgr_code w 0;
      Escape.sgr_sep w;
      Escape.sgr_code w 0;
      Escape.sgr_close w
    ]} *)

val sgr_open : writer -> unit
(** [sgr_open w] writes the SGR sequence opener [ESC \[]. *)

val sgr_code : writer -> int -> unit
(** [sgr_code w n] writes an integer parameter. *)

val sgr_sep : writer -> unit
(** [sgr_sep w] writes the parameter separator [;]. *)

val sgr_close : writer -> unit
(** [sgr_close w] writes the SGR terminator [m]. *)

val reset : t
(** [reset] emits the SGR reset sequence [ESC \[ 0 m]. *)

(** {1 Cursor Control} *)

(** {2 Relative Movement} *)

val cursor_up : n:int -> t
(** [cursor_up ~n] moves the cursor up [n] lines. Clamps negative values to 0.
*)

val cursor_down : n:int -> t
(** [cursor_down ~n] moves the cursor down [n] lines. *)

val cursor_forward : n:int -> t
(** [cursor_forward ~n] moves the cursor right [n] columns. *)

val cursor_back : n:int -> t
(** [cursor_back ~n] moves the cursor left [n] columns. *)

val cursor_next_line : n:int -> t
(** [cursor_next_line ~n] moves the cursor down [n] lines and to the first
    column (CNL). *)

val cursor_previous_line : n:int -> t
(** [cursor_previous_line ~n] moves the cursor up [n] lines and to the first
    column (CPL). *)

(** {2 Absolute Positioning} *)

val cursor_horizontal_absolute : int -> t
(** [cursor_horizontal_absolute col] moves to 1-based column [col]. Clamps
    values < 1 to 1. *)

val cursor_vertical_absolute : int -> t
(** [cursor_vertical_absolute row] moves to 1-based row [row]. Clamps values < 1
    to 1. *)

val cursor_position : row:int -> col:int -> t
(** [cursor_position ~row ~col] moves to 1-based coordinates [(row, col)]. *)

(** {2 State} *)

val cursor_save : t
(** [cursor_save] saves cursor position (CSI s).

    Uses the ANSI.SYS/SCO sequence, widely supported by modern terminals. Must
    be paired with {!cursor_restore}. *)

val cursor_restore : t
(** [cursor_restore] restores cursor position saved by {!cursor_save} (CSI u).

    Behavior undefined if called without prior {!cursor_save}. *)

val move_cursor_and_clear : row:int -> col:int -> t
(** [move_cursor_and_clear row col] moves to [(row, col)] and clears to the end
    of the screen. *)

(** {2 Appearance} *)

val show_cursor : t
(** [show_cursor] shows the cursor (DECTCEM). *)

val hide_cursor : t
(** [hide_cursor] hides the cursor (DECTCEM). *)

val cursor_style : shape:int -> t
(** [cursor_style ~shape] sets the cursor shape (DECSCUSR).
    - 0: User default.
    - 1: Blinking block.
    - 2: Steady block.
    - 3: Blinking underline.
    - 4: Steady underline.
    - 5: Blinking bar.
    - 6: Steady bar. *)

val default_cursor_style : t
(** [default_cursor_style] sets the cursor to the user default (Style 0). *)

val cursor_block : t
(** [cursor_block] sets the cursor to a steady block (Style 2). *)

val cursor_block_blink : t
(** [cursor_block_blink] sets the cursor to a blinking block (Style 1). *)

val cursor_line : t
(** [cursor_line] sets the cursor to a steady vertical bar (Style 6). *)

val cursor_line_blink : t
(** [cursor_line_blink] sets the cursor to a blinking vertical bar (Style 5). *)

val cursor_underline : t
(** [cursor_underline] sets the cursor to a steady underline (Style 4). *)

val cursor_underline_blink : t
(** [cursor_underline_blink] sets the cursor to a blinking underline (Style 3).
*)

val cursor_color : r:int -> g:int -> b:int -> t
(** [cursor_color ~r ~g ~b] sets the cursor color (OSC 12).

    Always pair with either {!reset_cursor_color} (for terminals supporting OSC
    112) or {!reset_cursor_color_fallback} (for compatibility with terminals
    that only support OSC 12) during teardown. *)

val reset_cursor_color : t
(** [reset_cursor_color] resets the cursor color to terminal default (OSC 112).

    Preferred method for terminals supporting OSC 112. *)

val reset_cursor_color_fallback : t
(** [reset_cursor_color_fallback] resets cursor color using OSC 12 "default".

    Fallback for terminals that only support OSC 12. *)

(** {1 Screen Control} *)

val clear : t
(** [clear] clears the visible screen without moving cursor (ED 2).

    Erases entire display but leaves cursor position unchanged. Use
    {!clear_and_home} to also move cursor to home. *)

val home : t
(** [home] moves the cursor to (1, 1). *)

val clear_and_home : t
(** [clear_and_home] moves to home, then clears the screen.

    Moves cursor to (1, 1) then erases entire display. Atomic operation to avoid
    flicker. *)

val erase_display : mode:int -> t
(** [erase_display mode] erases parts of the screen (ED).
    - 0: Cursor to end.
    - 1: Start to cursor.
    - 2: Entire screen.
    - 3: Entire screen + scrollback. *)

val erase_below_cursor : t
(** [erase_below_cursor] clears from the cursor to the end of the screen (ED 0).
*)

val erase_line : mode:int -> t
(** [erase_line mode] erases parts of the current line (EL).
    - 0: Cursor to end.
    - 1: Start to cursor.
    - 2: Entire line. *)

val insert_lines : n:int -> t
(** [insert_lines ~n] inserts [n] blank lines at the cursor (IL). *)

val delete_lines : n:int -> t
(** [delete_lines ~n] deletes [n] lines at the cursor (DL). *)

val scroll_up : n:int -> t
(** [scroll_up ~n] scrolls the viewport up by [n] lines (SU). *)

val scroll_down : n:int -> t
(** [scroll_down ~n] scrolls the viewport down by [n] lines (SD). *)

val set_scrolling_region : top:int -> bottom:int -> t
(** [set_scrolling_region ~top ~bottom] restricts scrolling to the specified
    line range.

    {b Invariant}: [top] must be >= 1 and [bottom] > [top].
    @raise Invalid_argument if bounds are invalid. *)

(** {1 Colors and Attributes} *)

val set_foreground : r:int -> g:int -> b:int -> t
(** [set_foreground ~r ~g ~b] sets the foreground color (Truecolor). Clamps
    values to [0-255]. *)

val set_background : r:int -> g:int -> b:int -> t
(** [set_background ~r ~g ~b] sets the background color (Truecolor). *)

val reset_background : t
(** [reset_background] resets the background color to default (SGR 49). *)

val reset_foreground : t
(** [reset_foreground] resets the foreground color to terminal default (SGR 39).
*)

(** {1 Screen Buffers} *)

val enter_alternate_screen : t
(** [enter_alternate_screen] switches to the alternate screen buffer. *)

val exit_alternate_screen : t
(** [exit_alternate_screen] restores the main screen buffer. *)

(** {1 Terminal Properties} *)

val set_title : title:string -> t
(** [set_title ~title] sets the window title (OSC 0). *)

val explicit_width : width:int -> text:string -> t
(** [explicit_width ~width ~text] renders [text] while forcing the terminal to
    treat it as occupying [width] cells.

    Emits OSC 66 sequence. The text payload is emitted verbatim (no escaping);
    ensure [text] does not contain unescaped ST sequences (ESC \\) which would
    terminate the OSC prematurely. *)

val explicit_width_bytes : width:int -> bytes:bytes -> off:int -> len:int -> t
(** [explicit_width_bytes ~width ~bytes ~off ~len] writes directly from a byte
    buffer without creating intermediate strings.

    Same sanitization requirements apply: ensure the byte slice does not contain
    unescaped ST sequences. *)

(** {1 Operating System Commands (OSC)} *)

type terminator = [ `Bel | `St ]
(** OSC terminator type. *)

val osc : ?terminator:terminator -> payload:string -> t
(** [osc ?terminator ~payload] emits an OSC sequence. *)

(** {2 Hyperlinks (OSC 8)} *)

val hyperlink_start : ?params:string -> url:string -> t
(** [hyperlink_start ?params ~url] opens a hyperlink.

    Must be paired with {!hyperlink_end}.

    @param params
      Optional key=value pairs (e.g., "id=link1") for terminal link
      identification. Defaults to empty string. *)

val hyperlink_end : t
(** [hyperlink_end] closes the current hyperlink.

    Must be paired with {!hyperlink_start}. *)

val hyperlink : ?params:string -> url:string -> text:string -> t
(** [hyperlink ...] emits a complete linked text segment. *)

(** {2 Direct Hyperlink Emission}

    Zero-allocation versions for use in hot render loops. *)

val hyperlink_open : writer -> string -> unit
(** [hyperlink_open w url] opens a hyperlink directly without allocation.

    Equivalent to [emit (hyperlink_start ~url) w] but avoids closure creation.
*)

val hyperlink_close : writer -> unit
(** [hyperlink_close w] closes the current hyperlink directly. *)

(** {1 Terminal Modes} *)

(** {2 Mouse} *)

val mouse_tracking_on : t
(** [mouse_tracking_on] enables basic X10 mouse tracking. *)

val mouse_tracking_off : t
(** [mouse_tracking_off] disables mouse tracking. *)

val mouse_button_tracking_on : t
(** [mouse_button_tracking_on] enables click tracking (DECSET 1002). *)

val mouse_button_tracking_off : t
(** [mouse_button_tracking_off] disables click tracking. *)

val mouse_motion_on : t
(** [mouse_motion_on] enables drag tracking (DECSET 1003). *)

val mouse_motion_off : t
(** [mouse_motion_off] disables drag tracking. *)

val mouse_sgr_mode_on : t
(** [mouse_sgr_mode_on] enables SGR-encoded coordinates (DECSET 1006). *)

val mouse_sgr_mode_off : t
(** [mouse_sgr_mode_off] disables SGR encoding. *)

val mouse_pixel_mode_on : t
(** [mouse_pixel_mode_on] enables pixel-precise mouse reporting (Kitty). *)

val mouse_pixel_mode_off : t
(** [mouse_pixel_mode_off] disables pixel-precise mouse reporting. *)

val mouse_x10_on : t
(** [mouse_x10_on] enables legacy X10 compatibility. *)

val mouse_x10_off : t
(** [mouse_x10_off] disables X10 compatibility. *)

val urxvt_mouse_on : t
(** [urxvt_mouse_on] enables urxvt-style extended coordinates. *)

val urxvt_mouse_off : t
(** [urxvt_mouse_off] disables urxvt-style coordinates. *)

(** {2 Keyboard} *)

val csi_u_on : t
(** [csi_u_on] enables CSI-u extended keyboard encoding. *)

val csi_u_off : t
(** [csi_u_off] disables CSI-u encoding. *)

val csi_u_push : flags:int -> t
(** [csi_u_push ~flags] pushes Kitty keyboard protocol flags. *)

val csi_u_pop : t
(** [csi_u_pop] pops the Kitty keyboard protocol state. *)

val modify_other_keys_on : t
(** [modify_other_keys_on] enables Xterm modifyOtherKeys (level 1). *)

val modify_other_keys_off : t
(** [modify_other_keys_off] disables modifyOtherKeys. *)

(** {2 Other Modes} *)

val bracketed_paste_on : t
(** [bracketed_paste_on] enables bracketed paste. *)

val bracketed_paste_off : t
(** [bracketed_paste_off] disables bracketed paste. *)

val focus_tracking_on : t
(** [focus_tracking_on] enables focus reporting. *)

val focus_tracking_off : t
(** [focus_tracking_off] disables focus reporting. *)

val sync_output_on : t
(** [sync_output_on] begins a synchronized update block (DECSET 2026). *)

val sync_output_off : t
(** [sync_output_off] ends a synchronized update block. *)

val unicode_mode_on : t
(** [unicode_mode_on] enables Unicode mode. *)

val unicode_mode_off : t
(** [unicode_mode_off] disables Unicode mode. *)

val color_scheme_set : t
(** [color_scheme_set] enables color scheme updates. *)

val color_scheme_reset : t
(** [color_scheme_reset] disables color scheme updates. *)

(** {1 Device and Capability Queries} *)

(** {2 Terminal and Device Information} *)

val request_cursor_position : t
(** [request_cursor_position] queries the cursor position (DSR 6). *)

val request_pixel_size : t
(** [request_pixel_size] requests the terminal size in pixels (DECRQSS 14t). *)

val request_device_attributes : t
(** [request_device_attributes] queries primary device attributes (DA1). *)

val request_tertiary_device_attributes : t
(** [request_tertiary_device_attributes] queries tertiary device attributes
    (DA3). *)

val request_terminal_identity : t
(** [request_terminal_identity] queries terminal identity (XTVERSION). *)

val request_device_status : t
(** [request_device_status] requests a generic status report (DSR 5). *)

(** {2 Feature and Protocol Support} *)

val request_csi_u_support : t
(** [request_csi_u_support] queries if CSI-u keyboard encoding is enabled. *)

val request_kitty_graphics_support : t
(** [request_kitty_graphics_support] probes for Kitty graphics protocol support.
*)

val request_sixel_geometry : t
(** [request_sixel_geometry] queries for Sixel graphics geometry limits. *)

val request_explicit_width_support : t
(** [request_explicit_width_support] probes for OSC 66 support. *)

val request_scaled_text_support : t
(** [request_scaled_text_support] probes for text scaling support. *)

val request_color_scheme : t
(** [request_color_scheme] queries the terminal color scheme. *)

(** {2 Mode State Queries} *)

val request_focus_mode : t
(** [request_focus_mode] queries if focus tracking is enabled (DECRQM 1004). *)

val request_sgr_pixels_mode : t
(** [request_sgr_pixels_mode] queries if SGR pixel mouse mode is enabled (DECRQM
    1016). *)

val request_unicode_mode : t
(** [request_unicode_mode] queries if Unicode mode is enabled (DECRQM 2027). *)

val request_color_scheme_mode : t
(** [request_color_scheme_mode] queries if color scheme reporting is enabled
    (DECRQM 2031). *)

val request_bracketed_paste_mode : t
(** [request_bracketed_paste_mode] queries if bracketed paste is enabled (DECRQM
    2004). *)

val request_sync_mode : t
(** [request_sync_mode] queries if synchronized output is enabled (DECRQM 2026).
*)

(** {2 Response Markers} *)

val bracketed_paste_start : t
(** [bracketed_paste_start] is the start marker emitted by the terminal on
    paste.

    Applications read this sequence, not emit it. Used to detect paste
    boundaries when bracketed paste mode is enabled. *)

val bracketed_paste_end : t
(** [bracketed_paste_end] is the end marker emitted by the terminal on paste.

    Applications read this sequence, not emit it. Marks the end of pasted
    content. *)
