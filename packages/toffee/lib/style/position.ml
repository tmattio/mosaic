type t = Relative | Absolute

let default = Relative
let to_string = function Relative -> "relative" | Absolute -> "absolute"

let equal a b =
  match (a, b) with
  | Relative, Relative -> true
  | Absolute, Absolute -> true
  | _ -> false

let compare a b =
  let to_int = function Relative -> 0 | Absolute -> 1 in
  Int.compare (to_int a) (to_int b)

let pp fmt t = Format.pp_print_string fmt (to_string t)
