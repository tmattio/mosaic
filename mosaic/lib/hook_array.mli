type 'a id = 'a Type.Id.t

module Sub = Engine.Sub

type _ kind =
  | State : 'a ref -> 'a kind
  | Effect : {
      cleanup : (unit -> unit) option;
      deps : Deps.t option;
    }
      -> unit kind
  | Sub : 'msg Sub.t -> unit kind
  | Reducer : {
      sid : 's id; (* NOTE: carries 's *)
      aid : 'a id; (* NOTE: carries 'a *)
      state : 's ref;
      reducer : ('s -> 'a -> 's) ref;
      dynamic : bool; (* whether to update reducer each render *)
    }
      -> 's kind
  | Memo : 'a * Deps.t option -> 'a kind
  | Ref : 'a ref -> 'a kind
  | Hole : unit kind

(* NEW: existential wrapper *)
type packed = P : 'a kind -> packed
type slot

val make_slot : 'a kind -> slot
val get : slot -> packed
val set : slot -> 'a kind -> unit

type t

val create : unit -> t
val length : t -> int
val get_slot : t -> int -> slot
