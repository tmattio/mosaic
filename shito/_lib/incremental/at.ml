open Import
open Types.Kind
module Node = Types.Node

type t = Types.At.t = {
  main : Before_or_after.t Node.t;
  at : Time_ns.t;
  mutable alarm : Types.Alarm.t;
  clock : Types.Clock.t;
}

let invariant t =
  (match t.main.kind with
  | Invalid -> ()
  | Const After -> () (* happens once the current time passes [t.at]. *)
  | At t' -> assert (t == t')
  | _ -> assert false);
  (* Alarm.invariant t.alarm *) ()
