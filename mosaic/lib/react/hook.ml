(* custom effects *)
type _ Effect.t +=
  | Use_state : 'a -> ('a * ('a -> unit)) Effect.t
  | Use_effect :
      (unit -> (unit -> unit) option) * Obj.t array option
      -> unit Effect.t
  | Use_context : 'a Context.t -> 'a Effect.t
  | Use_subscription : 'msg Engine.Sub.t -> unit Effect.t
  | Dispatch_cmd : 'msg Engine.Cmd.t -> unit Effect.t

(* public wrappers *)
let use_state v = Effect.perform (Use_state v)
let use_effect ?deps f = Effect.perform (Use_effect (f, deps))
let use_context ctx = Effect.perform (Use_context ctx)

let use_subscription sub = Effect.perform (Use_subscription sub)
let dispatch_cmd cmd = Effect.perform (Dispatch_cmd cmd)
