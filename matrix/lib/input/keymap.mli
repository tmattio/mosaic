(** Minimal keymap for binding key presses to values.

    Bindings match on key identity and optional modifier constraints. First
    matching binding wins. Unspecified modifiers match any state, enabling
    prefix-free patterns like [Ctrl+C] without constraining Shift/Alt. *)

type 'a t
(** Immutable keymap. *)

val empty : 'a t
(** [empty] is a keymap with no bindings. *)

val add :
  ?ctrl:bool -> ?alt:bool -> ?shift:bool -> 'a t -> Event.Key.t -> 'a -> 'a t
(** [add ?ctrl ?alt ?shift map key data] adds a binding for [key] with optional
    modifier constraints. Unspecified modifiers accept any state. *)

val add_char :
  ?ctrl:bool -> ?alt:bool -> ?shift:bool -> 'a t -> char -> 'a -> 'a t
(** [add_char ?ctrl ?alt ?shift map c data] adds a binding for character [c].
    Convenience wrapper over {!add}. *)

val add_key :
  ?ctrl:bool -> ?alt:bool -> ?shift:bool -> 'a t -> Event.Key.t -> 'a -> 'a t
(** [add_key] is an alias for {!add}.

    Provided for naming symmetry with {!add_char}, though it offers no
    functional benefit over using {!add} directly. Consider using {!add} in new
    code. *)

val find : 'a t -> Event.t -> 'a option
(** [find map event] returns the first binding matching a {!Event.Key} event
    with compatible modifiers. Non-key events yield [None]. *)
