(** Keyboard shortcuts for subscriptions and event handlers.

    A shortcut matches a key press or repeat ({!Matrix_input.Keystroke.t}) by
    its key and its held modifiers, exactly. A key's value is the character
    typed, with Shift consumed: Shift+r is [Char 'R'], so
    {!char}[ ~shift:true 'r'] is the shortcut [Char 'R'], and [char 'r'] does
    not match Shift+r. A lock never changes a chord's key, and Caps Lock and Num
    Lock are ignored in matching; a letter typed with Caps Lock on is the
    capital, as typed. The keypad is ignored: keypad Enter matches {!enter}, and
    the keypad's 5 without Num Lock is [Begin]. When a terminal reports the
    base-layout key, matching then tries that key with the same modifiers, so
    that Ctrl+c keeps working on a layout whose key in that place types another
    letter. Releases never match. *)

type t
(** The type for keyboard shortcuts. *)

val key :
  ?ctrl:bool ->
  ?alt:bool ->
  ?shift:bool ->
  ?super:bool ->
  ?hyper:bool ->
  ?meta:bool ->
  Matrix_input.Keystroke.key ->
  t
(** [key k] matches [k] with no modifiers. Optional modifier arguments default
    to [false]. With [shift], an ASCII letter is its capital and Shift is
    consumed, as in the keystrokes Matrix reports: [key ~shift:true (Char 'r')]
    is [key (Char 'R')]. [meta] is a Meta key distinct from Alt, which only the
    kitty keyboard protocol reports. *)

val char :
  ?ctrl:bool ->
  ?alt:bool ->
  ?shift:bool ->
  ?super:bool ->
  ?hyper:bool ->
  ?meta:bool ->
  char ->
  t
(** [char c] is [key (Char c)], with [c] as a Unicode code point. Optional
    modifier arguments default to [false]. *)

val ctrl : char -> t
(** [ctrl c] matches Ctrl+[c]. *)

val alt : char -> t
(** [alt c] matches Alt+[c]. *)

val shift : char -> t
(** [shift c] matches Shift+[c]: the capital, for a letter. *)

val escape : t
(** [escape] matches Escape. *)

val enter : t
(** [enter] matches Enter. *)

val tab : t
(** [tab] matches Tab. *)

val backspace : t
(** [backspace] matches Backspace. *)

val delete : t
(** [delete] matches Delete. *)

val f : int -> t
(** [f n] matches function key F[n]. *)

val up : t
(** [up] matches the Up arrow key. *)

val down : t
(** [down] matches the Down arrow key. *)

val left : t
(** [left] matches the Left arrow key. *)

val right : t
(** [right] matches the Right arrow key. *)

val space : t
(** [space] matches Space. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] denote the same shortcut. *)

val matches : t -> Event.key -> bool
(** [matches shortcut ev] is [true] iff [ev] matches [shortcut]. *)

(**/**)

module Private : sig
  val resolve :
    find_char:('a -> int -> int -> 'b option) ->
    find_key:('a -> int -> Matrix_input.Keystroke.key -> 'b option) ->
    'a ->
    Matrix_input.Keystroke.t ->
    'b option

  val find : (t * 'a) list -> Event.key -> 'a option
  val key : t -> Matrix_input.Keystroke.key
  val modifier : t -> int
end
