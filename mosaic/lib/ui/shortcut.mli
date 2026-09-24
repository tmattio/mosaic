(** Keyboard shortcuts for subscriptions and event handlers.

    A shortcut matches a key press or repeat with exact non-lock modifiers. Caps
    Lock and Num Lock state are ignored. When a terminal reports a base-layout
    key, matching also tries that base key so physical shortcuts such as Ctrl+C
    keep working across keyboard layouts. Lowercase ASCII shortcuts also match
    uppercase event keys; uppercase shortcuts remain exact. Keypad digits,
    arithmetic, Enter and navigation match their main-keyboard equivalents.
    Keypad Begin remains distinct. *)

type t
(** The type for keyboard shortcuts. *)

val key :
  ?ctrl:bool ->
  ?alt:bool ->
  ?shift:bool ->
  ?super:bool ->
  ?hyper:bool ->
  ?meta:bool ->
  Matrix_input.Key.t ->
  t
(** [key k] matches [k] with no modifiers. Optional modifier arguments default
    to [false]. [alt] also implies [meta], matching terminal Alt input as
    reported by Matrix. *)

val char :
  ?ctrl:bool ->
  ?alt:bool ->
  ?shift:bool ->
  ?super:bool ->
  ?hyper:bool ->
  ?meta:bool ->
  char ->
  t
(** [char c] is [key (Char c)]. Optional modifier arguments default to [false].
*)

val ctrl : char -> t
(** [ctrl c] matches Ctrl+[c]. *)

val alt : char -> t
(** [alt c] matches Alt+[c]. *)

val shift : char -> t
(** [shift c] matches Shift+[c]. *)

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
(** [equal a b] is [true] iff [a] and [b] denote the same shortcut, including
    keypad equivalences. *)

val matches : t -> Event.key -> bool
(** [matches shortcut ev] is [true] iff [ev] matches [shortcut]. *)

(**/**)

module Private : sig
  val key : t -> Matrix_input.Key.t
  val modifier : t -> int
  val modifiers : Matrix_input.Modifier.t -> int
  val normalize_key : Matrix_input.Key.t -> Matrix_input.Key.t
  val lowercase : int -> int
end
