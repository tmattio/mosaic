(** Attributes for UI elements *)

type key = private string
(** Stable identity key for elements *)

val key : string -> key
(** Create a key from a string. Keys should be unique within their parent
    context. *)

val key_to_string : key -> string
(** Convert a key back to its string representation *)
