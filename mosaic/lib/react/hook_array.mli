type 'a id = 'a Type.Id.t

module Sub = Engine.Sub

type _ kind =
  | State : 'a ref -> 'a kind
  | Effect : {
      cleanup : (unit -> unit) option;
      deps : Obj.t array option;
    }
      -> unit kind
  | Sub : 'msg Sub.t -> unit kind
  | Hole : unit kind

type slot

val make_slot : unit kind -> slot
val get : slot -> unit kind
val set : slot -> unit kind -> unit

type t

val create : unit -> t
val length : t -> int
val ensure_capacity : t -> int -> unit
val get_slot : t -> int -> slot
