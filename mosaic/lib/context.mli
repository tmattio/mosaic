(** React-style context. *)

type 'a t
(** A handle created by [create]. *)

val create : ?default:'a -> ?name:string -> unit -> 'a t
(** [create ?default ?name ()] creates a new context with optional default value
    and name for debugging. The name will be shown in error messages when no
    provider is found. *)

val provide : 'a t -> 'a -> (unit -> 'b) -> 'b
(** [provide ctx v thunk] runs [thunk ()] with [v] pushed on [ctx]’s stack. *)

val use : 'a t -> 'a
(** Returns the nearest value or the default, or raises if none. *)
