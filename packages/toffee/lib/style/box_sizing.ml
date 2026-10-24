type t = Border_box | Content_box

let default = Border_box

let to_string = function
  | Border_box -> "border-box"
  | Content_box -> "content-box"

let equal a b =
  match (a, b) with
  | Border_box, Border_box -> true
  | Content_box, Content_box -> true
  | _ -> false

let compare a b =
  let to_int = function Border_box -> 0 | Content_box -> 1 in
  Int.compare (to_int a) (to_int b)

let pp fmt t = Format.pp_print_string fmt (to_string t)
