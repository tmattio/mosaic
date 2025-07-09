(** Input event parsing

    This module provides parsing for terminal input events including keyboard
    and mouse input. It supports modern terminal features like:
    - Unicode character input
    - Modifier keys (Ctrl, Alt, Shift) with any key combination
    - SGR and X10 mouse protocols
    - Focus/blur events
    - Bracketed paste mode

    The parser works incrementally, accepting byte sequences and returning
    parsed events as they become available. *)

(** {1 Key Events} *)

type key =
  | Char of Uchar.t  (** Unicode character *)
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
  | F of int  (** Function keys F1-F20 *)

type modifier = { ctrl : bool; alt : bool; shift : bool }
(** Modifier keys that can be combined with any key *)

val no_modifier : modifier

type key_event = { key : key; modifier : modifier }
(** A key press event with possible modifiers *)

(** {1 Mouse Events} *)

type mouse_button =
  | Left
  | Middle
  | Right
  | Wheel_up
  | Wheel_down
  | Button of int  (** Extended buttons *)

type mouse_button_state = { left : bool; middle : bool; right : bool }
(** Current state of mouse buttons (for motion events) *)

type mouse_event =
  | Press of int * int * mouse_button * modifier  (** x, y, button, modifiers *)
  | Release of int * int * mouse_button * modifier
      (** x, y, button, modifiers *)
  | Motion of int * int * mouse_button_state * modifier
      (** x, y, button states, modifiers *)

(** {1 Events} *)

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
(** Parser state for incremental parsing *)

val create : unit -> parser
(** Create a new parser *)

val feed : parser -> bytes -> int -> int -> event list
(** [feed parser bytes offset length] feeds bytes to the parser and returns
    parsed events. The parser maintains internal state for incomplete sequences.
*)

val pending : parser -> bytes
(** Get any pending unparsed bytes. Useful for debugging or recovering from
    errors. *)

val reset : parser -> unit
(** Reset parser state, clearing any incomplete sequences. *)

val pp_key : Format.formatter -> key -> unit
(** Pretty-printing *)

val pp_modifier : Format.formatter -> modifier -> unit
val pp_key_event : Format.formatter -> key_event -> unit
val pp_mouse_button : Format.formatter -> mouse_button -> unit
val pp_mouse_button_state : Format.formatter -> mouse_button_state -> unit
val pp_mouse_event : Format.formatter -> mouse_event -> unit
val pp_event : Format.formatter -> event -> unit
