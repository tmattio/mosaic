(** Type-safe dependency tracking for React hooks *)

type t
(** Abstract type representing dependencies for effects *)

type key
(** Abstract type representing a single typed dependency *)

(** Dependency specification *)
val always : t
(** Run effect on every render *)

val once : t
(** Run effect only on first mount *)

val keys : key list -> t
(** Run effect when any of the given keys change *)

(** Typed key constructors *)
val int : int -> key
(** Integer key with equality comparison *)

val bool : bool -> key
(** Boolean key with equality comparison *)

val float : ?epsilon:float -> float -> key
(** Float key with optional epsilon for approximate equality *)

val string : string -> key
(** String key with equality comparison *)

val phys : 'a -> key
(** Physical identity comparison (useful for immutable data structures) *)

val custom : id:'a Type.Id.t -> equal:('a -> 'a -> bool) -> 'a -> key
(** Custom typed key with user-provided equality *)

val ui_key : Ui.Attr.key -> key
(** UI element key with equality comparison *)

(** Internal functions for the framework *)

val changed : t -> t -> bool
(** Check if dependencies have changed between renders *)
