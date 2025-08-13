open Import
module Node = Types.Node

type ('a, 'acc) t = ('a, 'acc) Types.Array_fold.t = {
  init : 'acc;
  f : 'acc -> 'a -> 'acc;
  children : 'a Node.t array;
}

let invariant invariant_a invariant_acc t =
  invariant_acc t.init;
  Array.iter
    (fun (child : _ Node.t) ->
      match child.value_opt with None -> () | Some v -> invariant_a v)
    t.children

let compute { init; f; children } =
  let result = ref init in
  for i = 0 to Array.length children - 1 do
    result :=
      f !result (Uopt.unsafe_value (Array.unsafe_get children i).value_opt)
  done;
  !result
