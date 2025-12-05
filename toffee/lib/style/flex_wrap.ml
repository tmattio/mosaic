type t = No_wrap | Wrap | Wrap_reverse

let default = No_wrap

let to_string = function
  | No_wrap -> "nowrap"
  | Wrap -> "wrap"
  | Wrap_reverse -> "wrap-reverse"

let equal a b =
  match (a, b) with
  | No_wrap, No_wrap -> true
  | Wrap, Wrap -> true
  | Wrap_reverse, Wrap_reverse -> true
  | _ -> false

let compare a b =
  let to_int = function No_wrap -> 0 | Wrap -> 1 | Wrap_reverse -> 2 in
  Int.compare (to_int a) (to_int b)

let pp fmt t = Format.pp_print_string fmt (to_string t)
