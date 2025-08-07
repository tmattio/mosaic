(** Public hook API (effect-based). *)

type _ Effect.t +=
  | Use_state : 'a -> ('a * ('a -> unit)) Effect.t
  | Use_effect :
      (unit -> (unit -> unit) option) * Obj.t array option
      -> unit Effect.t
  | Use_context : 'a Context.t -> 'a Effect.t
  | Use_subscription : 'msg Engine.Sub.t -> unit Effect.t
  | Dispatch_cmd : 'msg Engine.Cmd.t -> unit Effect.t

val use_state : 'a -> 'a * ('a -> unit)
(** Returns current value and a *setter* (snapshot-style). *)

val use_effect : ?deps:Obj.t array -> (unit -> (unit -> unit) option) -> unit
(** Registers an effect; optional dependency array. *)

val use_context : 'a Context.t -> 'a
val use_subscription : 'msg Engine.Sub.t -> unit
val dispatch_cmd : 'msg Engine.Cmd.t -> unit
