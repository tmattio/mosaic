(** Terminal input event parsing and representation.

    Input provides structured parsing of terminal input sequences into
    high-level events. The module handles keyboard input with modifiers, mouse
    events, window events, and paste detection through an incremental parser
    that correctly handles partial escape sequences split across reads.

    {1 Overview}

    Terminal input arrives as byte sequences that encode various events. This
    module parses these sequences into structured {!event} values:

    {[
      let parser = Input.create () in
      let events = Input.feed parser bytes 0 (Bytes.length bytes) in
      List.iter
        (function
          | Input.Key { key = Enter; _ } -> print_endline "Enter pressed"
          | Input.Mouse (Button_press (x, y, Left, _)) ->
              Printf.printf "Click at (%d, %d)\n" x y
          | _ -> ())
        events
    ]}

    {1 Key Concepts}

    {2 Incremental Parsing}

    The parser maintains state across {!feed} calls to handle escape sequences
    that may be split across multiple reads:

    {[
      (* First read gets partial escape sequence *)
      let events1 = Input.feed parser "\027[" 0 2 in  (* [] *)
      (* Second read completes it *)
      let events2 = Input.feed parser "A" 0 1 in      (* [Key Up] *)
    ]}

    {2 Coordinate System}

    All coordinates are zero-based from the top-left corner:
    - [(0, 0)] is the top-left cell
    - [(width-1, height-1)] is the bottom-right cell

    Exception: {!Cursor_position} uses 1-based coordinates following terminal
    conventions.

    {2 Protocol Support}

    The module supports multiple terminal protocols:
    - {b Keyboard}: Legacy ANSI, Kitty keyboard protocol
    - {b Mouse}: X10, SGR, URXVT protocols with motion tracking
    - {b Paste}: Bracketed paste mode, Windows heuristic detection
    - {b OSC}: Operating System Commands for clipboard and custom sequences

    {1 Common Patterns}

    {2 Basic Event Loop}

    {[
      let rec process_input parser input_channel =
        let buffer = Bytes.create 1024 in
        let n = input input_channel buffer 0 1024 in
        let events = Input.feed parser buffer 0 n in
        List.iter handle_event events;
        if n > 0 then process_input parser input_channel
    ]}

    {2 Handling Modified Keys}

    {[
      match event with
      | Input.Key { key = Char c; modifier; _ } when modifier.ctrl ->
          (* Ctrl+key pressed *)
          handle_ctrl_key c
      | Input.Key { key = Enter; modifier = { shift = true; _ }; _ } ->
          (* Shift+Enter *)
          insert_newline ()
      | _ -> ()
    ]}

    {1 Features}

    - Unicode character support with full modifier key combinations
    - Advanced keyboard protocol (Kitty) with event types, associated text,
      alternate keys
    - Mouse protocols: SGR (extended), X10/Normal (legacy with UTF-8 support),
      URXVT, with motion tracking and horizontal wheel
    - Window events: resize, focus, blur
    - Bracketed paste mode for safe multi-line input
    - OSC sequences (e.g., clipboard)
    - Platform-aware paste detection (ANSI on Unix, heuristic on Windows) *)

(** {1 Types}

    {2 Key Events} *)

(** [key] represents keyboard keys that can be pressed. *)
type key =
  | Char of Uchar.t
      (** Unicode character. Includes control characters (e.g.,
          [Uchar.of_int 0x03] for Ctrl+C) *)
  | Enter  (** Enter key, typically newline or carriage return *)
  | Tab  (** Tab key, often used for indentation or navigation *)
  | Backspace  (** Backspace key, deletes previous character *)
  | Delete  (** Delete key, removes character under cursor *)
  | Escape  (** Escape key, often used to cancel or exit modes *)
  | Up  (** Up arrow key for navigation *)
  | Down  (** Down arrow key for navigation *)
  | Left  (** Left arrow key for navigation *)
  | Right  (** Right arrow key for navigation *)
  | Home  (** Home key, moves cursor to start of line *)
  | End  (** End key, moves cursor to end of line *)
  | Page_up  (** Page up key, scrolls up one page *)
  | Page_down  (** Page down key, scrolls down one page *)
  | Insert  (** Insert key, toggles between insert and overwrite modes *)
  | F of int
      (** Function keys F1-F20. Values outside 1-20 are treated as unknown *)
  | Print_screen  (** Print screen key *)
  | Pause  (** Pause/Break key *)
  | Menu  (** Application/Context menu key *)
  | Scroll_lock  (** Scroll lock key *)
  | Media_play  (** Media control: play *)
  | Media_pause  (** Media control: pause *)
  | Media_play_pause  (** Media control: play/pause toggle *)
  | Media_stop  (** Media control: stop *)
  | Media_reverse  (** Media control: reverse/rewind *)
  | Media_fast_forward  (** Media control: fast forward *)
  | Media_rewind  (** Media control: rewind *)
  | Media_next  (** Media control: next track *)
  | Media_prev  (** Media control: previous track *)
  | Media_record  (** Media control: record *)
  | Volume_up  (** Volume control: increase *)
  | Volume_down  (** Volume control: decrease *)
  | Volume_mute  (** Volume control: mute toggle *)
  | Shift_left  (** Left shift modifier key *)
  | Shift_right  (** Right shift modifier key *)
  | Ctrl_left  (** Left control modifier key *)
  | Ctrl_right  (** Right control modifier key *)
  | Alt_left  (** Left alt/option modifier key *)
  | Alt_right  (** Right alt/option modifier key *)
  | Super_left  (** Left super/windows/command key *)
  | Super_right  (** Right super/windows/command key *)
  | Hyper_left  (** Left hyper modifier key (rare) *)
  | Hyper_right  (** Right hyper modifier key (rare) *)
  | Meta_left  (** Left meta modifier key *)
  | Meta_right  (** Right meta modifier key *)
  | Iso_level3_shift  (** ISO Level 3 Shift (e.g., AltGr) *)
  | Iso_level5_shift  (** ISO Level 5 Shift *)
  | Caps_lock  (** Caps lock key *)
  | Num_lock  (** Number lock key *)
  | KP_0  (** Keypad digit 0 *)
  | KP_1  (** Keypad digit 1 *)
  | KP_2  (** Keypad digit 2 *)
  | KP_3  (** Keypad digit 3 *)
  | KP_4  (** Keypad digit 4 *)
  | KP_5  (** Keypad digit 5 *)
  | KP_6  (** Keypad digit 6 *)
  | KP_7  (** Keypad digit 7 *)
  | KP_8  (** Keypad digit 8 *)
  | KP_9  (** Keypad digit 9 *)
  | KP_decimal  (** Keypad decimal point *)
  | KP_divide  (** Keypad division *)
  | KP_multiply  (** Keypad multiplication *)
  | KP_subtract  (** Keypad subtraction *)
  | KP_add  (** Keypad addition *)
  | KP_enter  (** Keypad enter *)
  | KP_equal  (** Keypad equals *)
  | KP_separator  (** Keypad separator *)
  | KP_begin  (** Keypad begin/5 key *)
  | KP_left  (** Keypad left arrow *)
  | KP_right  (** Keypad right arrow *)
  | KP_up  (** Keypad up arrow *)
  | KP_down  (** Keypad down arrow *)
  | KP_page_up  (** Keypad page up *)
  | KP_page_down  (** Keypad page down *)
  | KP_home  (** Keypad home *)
  | KP_end  (** Keypad end *)
  | KP_insert  (** Keypad insert *)
  | KP_delete  (** Keypad delete *)
  | Unknown of int
      (** Unknown key code from Private Use Area (PUA) or unmapped sequences *)
(** [key] represents keyboard keys that can be pressed.

    Most keys map directly to their expected values. Special keys like arrows
    and function keys have dedicated constructors. The [Char] variant handles
    all Unicode characters including control codes. *)

(** [event_type] indicates the type of key event when using advanced keyboard
    protocols. *)
type event_type =
  | Press  (** Key was pressed down *)
  | Repeat  (** Key is being held down and repeating *)
  | Release  (** Key was released *)

type modifier = {
  ctrl : bool;  (** Control key held *)
  alt : bool;  (** Alt/Option key held *)
  shift : bool;  (** Shift key held *)
  super : bool;  (** Super/Windows/Command key held *)
  hyper : bool;  (** Hyper modifier key held (rare) *)
  meta : bool;  (** Meta modifier key held *)
  caps_lock : bool;  (** Caps lock is active *)
  num_lock : bool;  (** Num lock is active *)
}
(** [modifier] tracks which modifier keys are held during an event.

    All modifiers can combine freely. Some combinations may be intercepted by
    the terminal or OS before reaching the application (e.g., Ctrl+C for
    SIGINT). The [caps_lock] and [num_lock] fields indicate lock state, not
    whether the lock key itself is pressed. *)

val no_modifier : modifier
(** [no_modifier] represents no modifier keys pressed or active.

    All fields are [false]. Use as a base for creating modifier states:

    {[
      let ctrl_only = { no_modifier with ctrl = true }
      let alt_shift = { no_modifier with alt = true; shift = true }
    ]} *)

type key_event = {
  key : key;  (** The key that was pressed *)
  modifier : modifier;  (** Active modifiers *)
  event_type : event_type;  (** Type of key event *)
  associated_text : string;  (** Text to insert (Kitty protocol) *)
  shifted_key : Uchar.t option;  (** Key with shift applied *)
  base_key : Uchar.t option;  (** Key without modifiers *)
}
(** [key_event] combines a key press with modifiers and additional metadata.

    The [event_type] field indicates press/repeat/release when supported by the
    terminal (Kitty protocol). The [associated_text] field contains the text
    that would be inserted by this key press. The [shifted_key] and [base_key]
    fields provide alternate representations for more accurate handling.

    {4 Examples}

    Pattern matching on key events:
    {[
      match event with
      | Key { key = Char c; modifier; event_type = Press; _ }
        when modifier.ctrl && not modifier.shift ->
          handle_ctrl_key c
      | Key { key = Enter; event_type = Release; _ } ->
          (* Key release events (Kitty protocol) *)
          handle_key_up ()
      | _ -> ()
    ]} *)

val key :
  ?modifier:modifier ->
  ?event_type:event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t option ->
  ?base_key:Uchar.t option ->
  key ->
  key_event
(** [key ?modifier ?event_type ?associated_text ?shifted_key ?base_key k]
    constructs a key event for pattern matching.

    @param modifier defaults to {!no_modifier}
    @param event_type defaults to [Press]
    @param associated_text defaults to [""]
    @param shifted_key defaults to [None]
    @param base_key defaults to [None]

    {4 Examples}

    {[
      (* Match Ctrl+C *)
      let ctrl_c =
        key
          ~modifier:{ no_modifier with ctrl = true }
          (Char (Uchar.of_char 'c'))

      (* Match function key F1 *)
      let f1 = key (F 1)

      (* Match key release (Kitty protocol) *)
      let space_up = key ~event_type:Release (Char (Uchar.of_char ' '))
    ]} *)

val char :
  ?modifier:modifier ->
  ?event_type:event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t option ->
  ?base_key:Uchar.t option ->
  char ->
  key_event
(** [char ?modifier ?event_type ?associated_text ?shifted_key ?base_key c]
    constructs a key event from ASCII character.

    Convenience function that converts ASCII character to {!Uchar.t}.

    @param modifier defaults to {!no_modifier}
    @param event_type defaults to [Press]
    @param associated_text defaults to [""]
    @param shifted_key defaults to [None]
    @param base_key defaults to [None]

    {4 Examples}

    {[
      (* Simple character *)
      let a = char 'a'

      (* Alt+x *)
      let alt_x = char ~modifier:{ no_modifier with alt = true } 'x'
    ]} *)

(** {2 Mouse Events} *)

(** [mouse_button] identifies which mouse button triggered an event. *)
type mouse_button =
  | Left  (** Left mouse button *)
  | Middle  (** Middle mouse button/wheel click *)
  | Right  (** Right mouse button *)
  | Wheel_up  (** Mouse wheel scrolled up *)
  | Wheel_down  (** Mouse wheel scrolled down *)
  | Wheel_left  (** Mouse wheel scrolled left (horizontal) *)
  | Wheel_right  (** Mouse wheel scrolled right (horizontal) *)
  | Button of int  (** Extended buttons (4+) *)

type mouse_button_state = {
  left : bool;  (** Left button is pressed *)
  middle : bool;  (** Middle button is pressed *)
  right : bool;  (** Right button is pressed *)
}
(** [mouse_button_state] tracks which buttons are held during mouse motion.

    Used with motion events to support drag operations. All three primary
    buttons can be held simultaneously. *)

(** [mouse_event] represents mouse interactions with position and modifiers.

    Coordinates [(x, y)] are zero-based from top-left corner: [(0, 0)] is the
    top-left cell. Position is clamped to terminal bounds by most protocols.

    {4 Examples}

    {[
      (* Click and drag *)
      let start_drag = function
        | Mouse (Button_press (x, y, Left, _)) -> Some (x, y)
        | _ -> None

      let handle_drag start_pos = function
        | Mouse (Motion (x, y, { left = true; _ }, _)) ->
            update_selection start_pos (x, y)
        | Mouse (Button_release (_, _, Left, _)) -> finalize_selection ()
        | _ -> ()
    ]} *)
type mouse_event =
  | Button_press of int * int * mouse_button * modifier
      (** Button pressed at (x, y) with modifiers *)
  | Button_release of int * int * mouse_button * modifier
      (** Button released at (x, y) with modifiers *)
  | Motion of int * int * mouse_button_state * modifier
      (** Mouse moved to (x, y) with button states and modifiers *)

(** {2 Events} *)

(** [event] represents any input event from the terminal. *)
type event =
  | Key of key_event  (** Keyboard input with modifiers *)
  | Mouse of mouse_event  (** Mouse button or motion *)
  | Resize of int * int  (** Terminal resized to (width, height) *)
  | Focus  (** Terminal window gained focus *)
  | Blur  (** Terminal window lost focus *)
  | Paste_start  (** Start of bracketed paste *)
  | Paste_end  (** End of bracketed paste *)
  | Paste of string
      (** Pasted content. On Unix systems, requires bracketed paste mode. On
          Windows, uses timing heuristics to detect rapid key sequences. *)
  | Clipboard of string * string  (** OSC 52: (selection, data) *)
  | Osc of int * string  (** Other OSC sequences: (number, payload) *)
  | Cursor_position of int * int
      (** Cursor position report: [(row, col)] with 1-based coordinates. Row 1
          is the top line, column 1 is the leftmost column. *)
  | Device_attributes of int list
      (** Device attributes response - terminal capabilities *)
(** [event] represents any input event from the terminal.

    Events arrive in the order they were generated. Mouse coordinates are
    zero-based, while cursor position reports use 1-based coordinates following
    terminal conventions. *)

(** {1 Parser Interface} *)

type parser
(** [parser] maintains state for incremental input parsing.

    The parser accumulates partial escape sequences across {!feed} calls,
    emitting complete events as they are recognized. State includes incomplete
    sequences, parsing mode, and protocol detection. A single parser instance is
    not thread-safe - use one parser per input source.

    Invalid or unrecognized sequences are silently discarded to maintain
    robustness against corrupted input. *)

val create : unit -> parser
(** [create ()] creates a new parser with empty state.

    The parser starts with no pending data and default protocol assumptions. It
    will auto-detect advanced protocols as it processes input.

    {[
      let parser = Input.create ()
    ]} *)

val feed : parser -> bytes -> int -> int -> event list
(** [feed parser bytes offset length] parses input bytes into events.

    Processes bytes from [offset] to [offset+length-1]. Returns list of parsed
    events in order received. Incomplete sequences are stored internally and
    combined with future input. Returns empty list if input contains only
    partial sequences.

    The same parser instance should be used for all input from a given source to
    correctly handle split sequences.

    @raise Invalid_argument if offset or length are out of bounds

    {4 Examples}

    {[
      let buffer = Bytes.create 1024 in
      let n = input stdin buffer 0 1024 in
      let events = Input.feed parser buffer 0 n in
      List.iter handle_event events
    ]}

    Handling split escape sequences:
    {[
      (* First chunk has incomplete sequence *)
      let events1 = Input.feed parser "\027[1;" 0 5 in   (* [] *)
      (* Second chunk completes it *)
      let events2 = Input.feed parser "5H" 0 2 in       (* [Key Home] *)
    ]} *)

val pending : parser -> bytes
(** [pending parser] returns accumulated incomplete sequences.

    Useful for debugging or detecting stuck parser state. Normal operation has
    0-10 bytes pending (partial escape sequence). Large pending buffer (>20
    bytes) may indicate corrupted input or unsupported sequences.

    The returned bytes should not be modified or fed back to the parser.

    {[
      if Bytes.length (Input.pending parser) > 20 then
        (* Possible corrupted input *)
        Input.reset parser
    ]} *)

val reset : parser -> unit
(** [reset parser] clears all parser state and pending data.

    Use when input stream is corrupted or after terminal mode changes. Discards
    any incomplete sequences. After reset, the parser behaves as if newly
    created.

    Common scenarios for reset:
    - Switching between raw and cooked terminal modes
    - Recovering from binary data accidentally sent to terminal
    - Clear input buffer after extended idle period

    {[
      (* Reset after mode change *)
      set_raw_mode true;
      Input.reset parser (* Clear any buffered cooked mode input *)
    ]} *)

(** {1 Pretty Printing}

    Format-based printers for debugging and logging. All printers follow
    standard conventions for use with [%a] in format strings. *)

val pp_key : Format.formatter -> key -> unit
(** [pp_key fmt k] pretty-prints key [k].

    {[
      Format.printf "Key pressed: %a\n" Input.pp_key Up
      (* Output: Key pressed: Up *)
    ]} *)

val pp_event_type : Format.formatter -> event_type -> unit
(** [pp_event_type fmt et] pretty-prints event type [et]. *)

val pp_modifier : Format.formatter -> modifier -> unit
(** [pp_modifier fmt m] pretty-prints modifier state [m].

    Shows active modifiers in a compact format:
    {[
      let m = { no_modifier with ctrl = true; shift = true } in
      Format.printf "Modifiers: %a\n" Input.pp_modifier m
      (* Output: Modifiers: Ctrl+Shift *)
    ]} *)

val pp_key_event : Format.formatter -> key_event -> unit
(** [pp_key_event fmt ke] pretty-prints complete key event [ke]. *)

val pp_mouse_button : Format.formatter -> mouse_button -> unit
(** [pp_mouse_button fmt mb] pretty-prints mouse button [mb]. *)

val pp_mouse_button_state : Format.formatter -> mouse_button_state -> unit
(** [pp_mouse_button_state fmt mbs] pretty-prints button states [mbs]. *)

val pp_mouse_event : Format.formatter -> mouse_event -> unit
(** [pp_mouse_event fmt me] pretty-prints complete mouse event [me]. *)

val pp_event : Format.formatter -> event -> unit
(** [pp_event fmt e] pretty-prints any event [e].

    Useful for debugging input handling:
    {[
      List.iter (Format.printf "Event: %a\n" Input.pp_event) events
    ]} *)

(** {1 Convenience Functions}

    Helper functions for common use cases and testing. *)

val parse_single : string -> event list
(** [parse_single s] parses a complete input string into events.

    Creates a temporary parser, feeds the entire string, and returns resulting
    events. Useful for testing or parsing known complete sequences. Does not
    handle partial sequences - use a persistent parser with {!feed} for stream
    processing.

    {4 Examples}

    {[
      (* Parse arrow key *)
      let events = Input.parse_single "\027[A" in
      assert (events = [Key (key Up)])

      (* Parse mouse click *)
      let events = Input.parse_single "\027[<0;5;10M" in
      (* Mouse click at (5, 10) *)
    ]} *)

(** {2 Event Creation Helpers} *)

val key_event : ?modifier:modifier -> ?event_type:event_type -> key -> event
(** [key_event ?modifier ?event_type key] creates a {!Key} event.

    @param modifier defaults to {!no_modifier}
    @param event_type defaults to [Press]

    {[
      let enter = key_event Enter

      let ctrl_a =
        key_event
          ~modifier:{ no_modifier with ctrl = true }
          (Char (Uchar.of_char 'a'))
    ]} *)

val char_event : ?modifier:modifier -> ?event_type:event_type -> char -> event
(** [char_event ?modifier ?event_type c] creates a {!Key} event for character
    [c].

    Convenience wrapper that converts [char] to {!key}.

    @param modifier defaults to {!no_modifier}
    @param event_type defaults to [Press]

    {[
      let a = char_event 'a'
      let alt_q = char_event ~modifier:{ no_modifier with alt = true } 'q'
    ]} *)

val mouse_press : ?modifier:modifier -> int -> int -> mouse_button -> event
(** [mouse_press ?modifier x y button] creates a mouse button press event.

    @param modifier defaults to {!no_modifier}
    @param x column position (0-based)
    @param y row position (0-based)

    {[
      let click = mouse_press 10 5 Left

      let ctrl_click =
        mouse_press ~modifier:{ no_modifier with ctrl = true } 10 5 Left
    ]} *)

val mouse_release : ?modifier:modifier -> int -> int -> mouse_button -> event
(** [mouse_release ?modifier x y button] creates a mouse button release event.

    @param modifier defaults to {!no_modifier}
    @param x column position (0-based)
    @param y row position (0-based)

    {[
      let release = mouse_release 10 5 Left
    ]} *)

val mouse_motion :
  ?modifier:modifier -> int -> int -> mouse_button_state -> event
(** [mouse_motion ?modifier x y state] creates a mouse motion event.

    @param modifier defaults to {!no_modifier}
    @param x column position (0-based)
    @param y row position (0-based)
    @param state which buttons are currently held

    {[
      (* Mouse drag with left button *)
      let drag =
        mouse_motion 15 8 { left = true; middle = false; right = false }
    ]} *)
