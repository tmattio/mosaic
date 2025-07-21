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
    - Advanced keyboard protocol (Kitty) with event types, associated text,
      alternate keys
    - Mouse protocols: SGR (extended), X10/Normal (legacy with UTF-8 support),
      URXVT, with motion tracking and horizontal wheel
    - Window events: resize, focus, blur
    - Bracketed paste mode for safe multi-line input
    - OSC sequences (e.g., clipboard)
    - Platform-aware paste detection (ANSI on Unix, heuristic on Windows) *)

(** {1 Key Events} *)

type key =
  | Char of Uchar.t
  | Enter
  | Tab
  | Backspace
  | Delete
  | Escape
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Page_up
  | Page_down
  | Insert
  | F of int
  | Print_screen
  | Pause
  | Menu
  | Scroll_lock
  | Media_play
  | Media_pause
  | Media_play_pause
  | Media_stop
  | Media_reverse
  | Media_fast_forward
  | Media_rewind
  | Media_next
  | Media_prev
  | Media_record
  | Volume_up
  | Volume_down
  | Volume_mute
  | Shift_left
  | Shift_right
  | Ctrl_left
  | Ctrl_right
  | Alt_left
  | Alt_right
  | Super_left
  | Super_right
  | Hyper_left
  | Hyper_right
  | Meta_left
  | Meta_right
  | Iso_level3_shift
  | Iso_level5_shift
  | Caps_lock
  | Num_lock
  | KP_0
  | KP_1
  | KP_2
  | KP_3
  | KP_4
  | KP_5
  | KP_6
  | KP_7
  | KP_8
  | KP_9
  | KP_decimal
  | KP_divide
  | KP_multiply
  | KP_subtract
  | KP_add
  | KP_enter
  | KP_equal
  | KP_separator
  | KP_begin
  | KP_left
  | KP_right
  | KP_up
  | KP_down
  | KP_page_up
  | KP_page_down
  | KP_home
  | KP_end
  | KP_insert
  | KP_delete
  | Unknown of int
      (** Extended key types including media, modifier keys, keypad, and unknown
          for unmapped PUA codes. *)

type event_type =
  | Press
  | Repeat
  | Release  (** Key press, repeat, or release event type. *)

type modifier = {
  ctrl : bool;
  alt : bool;
  shift : bool;
  super : bool;
  hyper : bool;
  meta : bool;
  caps_lock : bool;
  num_lock : bool;
}
(** Extended modifiers including super, hyper, meta, caps, num lock. *)

val no_modifier : modifier

type key_event = {
  key : key;
  modifier : modifier;
  event_type : event_type;
  associated_text : string;
  shifted_key : Uchar.t option;
  base_key : Uchar.t option;
}
(** Extended with event_type, associated_text (for Kitty), shifted and base
    keys. *)

val key :
  ?modifier:modifier ->
  ?event_type:event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t option ->
  ?base_key:Uchar.t option ->
  key ->
  key_event

val char :
  ?modifier:modifier ->
  ?event_type:event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t option ->
  ?base_key:Uchar.t option ->
  char ->
  key_event

(** {1 Mouse Events} *)

type mouse_button =
  | Left
  | Middle
  | Right
  | Wheel_up
  | Wheel_down
  | Wheel_left
  | Wheel_right
  | Button of int

type mouse_button_state = { left : bool; middle : bool; right : bool }

type mouse_event =
  | Button_press of int * int * mouse_button * modifier
  | Button_release of int * int * mouse_button * modifier
  | Motion of int * int * mouse_button_state * modifier

(** {1 Events} *)

type event =
  | Key of key_event
  | Mouse of mouse_event
  | Resize of int * int
  | Focus
  | Blur
  | Paste_start
  | Paste_end
  | Paste of string
  | Clipboard of string * string  (** Clipboard (selection, data) - OSC 52 **)
  | Osc of int * string
  | Cursor_position of int * int
      (** Cursor position report (row, col) - CSI row ; col R **)
  | Device_attributes of int list
      (** Device attributes response - CSI ? ... c **)

type parser

val create : unit -> parser
val feed : parser -> bytes -> int -> int -> event list
val pending : parser -> bytes
val reset : parser -> unit
val pp_key : Format.formatter -> key -> unit
val pp_event_type : Format.formatter -> event_type -> unit
val pp_modifier : Format.formatter -> modifier -> unit
val pp_key_event : Format.formatter -> key_event -> unit
val pp_mouse_button : Format.formatter -> mouse_button -> unit
val pp_mouse_button_state : Format.formatter -> mouse_button_state -> unit
val pp_mouse_event : Format.formatter -> mouse_event -> unit
val pp_event : Format.formatter -> event -> unit

(** {1 Convenience Functions} *)

val parse_single : string -> event list
(** [parse_single s] creates a temporary parser and attempts to parse the
    complete string [s]. Returns a list of parsed events. Useful for testing
    individual sequences without state management. *)

(** {2 Event Creation Helpers} *)

val key_event : ?modifier:modifier -> ?event_type:event_type -> key -> event
(** [key_event ?modifier ?event_type key] creates a Key event with defaults.
    [modifier] defaults to [no_modifier], [event_type] defaults to [Press]. *)

val char_event : ?modifier:modifier -> ?event_type:event_type -> char -> event
(** [char_event ?modifier ?event_type c] creates a Key event for character [c].
    [modifier] defaults to [no_modifier], [event_type] defaults to [Press]. *)

val mouse_press : ?modifier:modifier -> int -> int -> mouse_button -> event
(** [mouse_press ?modifier x y button] creates a mouse button press event. *)

val mouse_release : ?modifier:modifier -> int -> int -> mouse_button -> event
(** [mouse_release ?modifier x y button] creates a mouse button release event.
*)

val mouse_motion :
  ?modifier:modifier -> int -> int -> mouse_button_state -> event
(** [mouse_motion ?modifier x y state] creates a mouse motion event. *)
