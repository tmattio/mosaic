type t = Content_size | Inherent_size

let to_string = function
  | Content_size -> "ContentSize"
  | Inherent_size -> "InherentSize"

let compare a b =
  match (a, b) with
  | Content_size, Content_size -> 0
  | Content_size, _ -> -1
  | Inherent_size, Inherent_size -> 0
  | Inherent_size, _ -> 1

let equal a b = compare a b = 0
let pp fmt t = Format.pp_print_string fmt (to_string t)
