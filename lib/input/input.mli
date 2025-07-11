(** Terminal input event parsing and representation.

    This module handles parsing of terminal input sequences into structured
    events. Supports keyboard input with modifiers, mouse events, window events,
    and paste detection. Works incrementally to handle partial escape sequences
    across reads.

    Parser state persists across feeds for handling split escape sequences.
    Events are emitted in order received. Coordinates are zero-based from
    top-left. Function keys are numbered 1-20. Invalid sequences are discarded.

    {1 Features}

    - Unicode character support with full modifier key combinations
    - Mouse protocols: SGR (extended) and X10 (legacy) with motion tracking
    - Window events: resize, focus, blur
    - Bracketed paste mode for safe multi-line input
    - Platform-aware paste detection (ANSI on Unix, timing-based on Windows) *)

(** {1 Key Events} *)

(** [key] represents keyboard keys that can be pressed. *)
type key =
  | Char of Uchar.t
      (** Holds any Unicode character including control characters *)
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
  | F of int  (** Function keys F1-F20 *)

type modifier = { ctrl : bool; alt : bool; shift : bool }
(** [modifier] tracks which modifier keys are held during a key press.

    All three modifiers can combine. Shift with letters produces capitals
    automatically. Some combinations may be intercepted by terminal or OS (e.g.,
    Ctrl+C for SIGINT). *)

val no_modifier : modifier
(** [no_modifier] represents no modifier keys pressed.

    Equivalent to [{ ctrl = false; alt = false; shift = false }]. Common for
    plain key presses. *)

type key_event = { key : key; modifier : modifier }
(** [key_event] combines a key press with active modifier keys.

    Complete representation of keyboard input. Used in pattern matching for key
    bindings. *)

val key : ?ctrl:bool -> ?alt:bool -> ?shift:bool -> key -> key_event
(** [key ?ctrl ?alt ?shift k] constructs a key event for pattern matching.

    All modifiers default to false. Useful for matching specific key
    combinations in event handlers.

    Example: Matches Ctrl+S for save.
    {[
      match event with
      | Key ke when ke = Input.key ~ctrl:true (Char (Uchar.of_char 's')) ->
          save ()
      | _ -> ()
    ]} *)

val char : ?ctrl:bool -> ?alt:bool -> ?shift:bool -> char -> key_event
(** [char ?ctrl ?alt ?shift c] constructs a key event from ASCII character.

    Convenience function converting char to Uchar. Modifiers default to false.

    Example: Matches plain 'q' for quit.
    {[
      match event with Key ke when ke = Input.char 'q' -> quit () | _ -> ()
    ]} *)

(** {1 Mouse Events} *)

(** [mouse_button] identifies which mouse button triggered an event.

    Standard three-button mouse plus wheel. Extended buttons for mice with more
    than 3 buttons. Wheel events typically used for scrolling. Button numbers
    start at 4 for extended buttons. *)
type mouse_button =
  | Left
  | Middle
  | Right
  | Wheel_up
  | Wheel_down
  | Button of int  (** Extended buttons *)

type mouse_button_state = { left : bool; middle : bool; right : bool }
(** [mouse_button_state] tracks which buttons are held during mouse motion.

    Used in motion events to support dragging. True when button is pressed.
    Wheel buttons not tracked. *)

(** [mouse_event] represents mouse interactions with position and modifiers.

    Coordinates are zero-based from top-left corner. Press/Release track
    individual button events. Motion events sent during movement, button states
    indicate dragging. Modifiers work like keyboard events. *)
type mouse_event =
  | Press of int * int * mouse_button * modifier  (** x, y, button, modifiers *)
  | Release of int * int * mouse_button * modifier
      (** x, y, button, modifiers *)
  | Motion of int * int * mouse_button_state * modifier
      (** x, y, button states, modifiers *)

(** {1 Events} *)

(** [event] represents any input event from the terminal.

    Key and Mouse are most common. Resize fired on terminal size change with new
    dimensions. Focus/Blur track terminal window focus (support varies). Paste
    events enable safe multi-line input handling. On Unix, Paste_start/end wrap
    pasted content. On Windows, rapid key sequences detected as paste. *)
type event =
  | Key of key_event
  | Mouse of mouse_event
  | Resize of int * int  (** Terminal width and height *)
  | Focus  (** Terminal gained focus *)
  | Blur  (** Terminal lost focus *)
  | Paste_start  (** Start of bracketed paste *)
  | Paste_end  (** End of bracketed paste *)
  | Paste of string
      (** Pasted content. Note: On Windows, this is a heuristic based on the
          timing of key events, as bracketed paste is not a native console
          concept. *)

(** {1 Parsing} *)

type parser
(** [parser] maintains state for incremental input parsing.

    Handles partial escape sequences split across reads. Stateful to accumulate
    incomplete sequences. Thread-safe when used with single writer. *)

val create : unit -> parser
(** [create ()] creates a new parser with empty state.

    Ready to accept input bytes. No accumulated partial sequences. *)

val feed : parser -> bytes -> int -> int -> event list
(** [feed parser bytes offset length] parses input bytes into events.

    Processes bytes from [offset] for [length] bytes. Returns list of complete
    events parsed. Incomplete sequences retained for next feed. Empty list if no
    complete events.

    @param parser Parser instance maintaining state
    @param bytes Input byte buffer to parse
    @param offset Starting position in buffer
    @param length Number of bytes to process

    Example: Typical read loop pattern.
    {[
      let parser = Input.create () in
      let buf = Bytes.create 1024 in
      let n = read_input buf in
      let events = Input.feed parser buf 0 n in
      List.iter handle_event events
    ]} *)

val pending : parser -> bytes
(** [pending parser] returns accumulated incomplete sequences.

    Contains partial escape sequences awaiting more data. Empty when no
    incomplete parsing. Useful for debugging parsing issues or recovery after
    errors. *)

val reset : parser -> unit
(** [reset parser] clears all parser state and pending data.

    Discards any incomplete sequences. Returns parser to initial state. Use
    after errors or mode changes. *)

val pp_key : Format.formatter -> key -> unit
(** [pp_key fmt k] pretty-prints key [k] for debugging.

    Shows human-readable key names. Unicode characters display as "Char 'x'".
    Special keys show names. *)

val pp_modifier : Format.formatter -> modifier -> unit
(** [pp_modifier fmt m] pretty-prints modifier state [m].

    Shows active modifiers in readable format like "Ctrl+Alt". Empty string if
    no modifiers. *)

val pp_key_event : Format.formatter -> key_event -> unit
(** [pp_key_event fmt ke] pretty-prints complete key event [ke].

    Combines key and modifiers like "Ctrl+Shift+A" or "Alt+F4". Useful for
    displaying bindings. *)

val pp_mouse_button : Format.formatter -> mouse_button -> unit
(** [pp_mouse_button fmt mb] pretty-prints mouse button [mb].

    Shows button names: "Left", "Middle", "Right", "Wheel_up", "Wheel_down", or
    "Button N". *)

val pp_mouse_button_state : Format.formatter -> mouse_button_state -> unit
(** [pp_mouse_button_state fmt mbs] pretty-prints button states [mbs].

    Shows pressed buttons like "[Left,Right]" or "[]" if none pressed. *)

val pp_mouse_event : Format.formatter -> mouse_event -> unit
(** [pp_mouse_event fmt me] pretty-prints complete mouse event [me].

    Shows event type, coordinates, button, and modifiers. Format:
    "Press(10,20,Left,Ctrl)". *)

val pp_event : Format.formatter -> event -> unit
(** [pp_event fmt e] pretty-prints any event [e] for debugging.

    Dispatches to appropriate printer. Shows event type and details. Essential
    for event handler debugging. *)
