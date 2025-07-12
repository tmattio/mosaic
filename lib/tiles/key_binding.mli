(** Common key binding configuration for text input components.

    This module provides a unified way to configure how keyboard events are
    handled across all text input components in Mosaic. *)

(** Key binding actions for text input components *)
type 'msg action =
  | Handle of 'msg  (** Handle and produce this message *)
  | Pass_through  (** Let parent handle it *)
  | Insert  (** Insert as text (for text inputs) *)

type 'msg config = {
  bindings : (Mosaic.Input.key_event * 'msg action) list;
  default : 'msg action;
}
(** Key binding configuration.

    - [bindings] is a list of specific key events and their handling
    - [default] is the behavior for keys not in the bindings list *)

val default_config : 'msg config
(** [default_config] inserts all keys as text with no special handling. *)

val pass_through_ctrl_keys : 'msg config -> char list -> 'msg config
(** [pass_through_ctrl_keys config keys] adds pass-through bindings for the
    specified control key characters.

    Example:
    {[
      let config = default_config |> pass_through_ctrl_keys [ 'c'; 'x'; 'v' ]
    ]} *)

val key_event_equal : Mosaic.Input.key_event -> Mosaic.Input.key_event -> bool
(** [key_event_equal e1 e2] returns true if two key events are equal. *)

val find_binding :
  Mosaic.Input.key_event ->
  'msg config ->
  (Mosaic.Input.key_event * 'msg action) option
(** [find_binding event config] finds the action for a key event if it exists.
*)
