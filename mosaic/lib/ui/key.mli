(** Key generation and management for UI elements

    Keys are used to uniquely identify UI elements for event handling, focus
    management, and other runtime operations. *)

val create : prefix:string -> Attr.key
(** [create ~prefix] generates a new unique key for a UI element. Note: This
    creates a new key each time it's called.

    For stable keys across re-renders, use [of_string] with a stable identifier,
    or use the Hook-based [Mosaic.use_key] function. *)

val of_string : string -> Attr.key
(** [of_string s] creates a key directly from a string. Useful when you have a
    stable identifier like an ID from your data model. *)

val of_int : int -> Attr.key
(** [of_int i] creates a key from an integer. Useful for list items with numeric
    IDs. *)
