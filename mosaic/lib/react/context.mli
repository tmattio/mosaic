(** React-style context. *)

type 'a t
(** A handle created by [create]. *)

val create : ?default:'a -> unit -> 'a t

val provide : 'a t -> 'a -> (unit -> 'b) -> 'b
(** [provide ctx v thunk] runs [thunk ()] with [v] pushed on [ctx]’s stack. *)

val use : 'a t -> 'a
(** Returns the nearest value or the default, or raises if none. *)

val unsafe_use : 'a t -> 'a
(** Internal: same as use but exposed for Hook module *)
