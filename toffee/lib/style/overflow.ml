type t = Visible | Clip | Hidden | Scroll

let default = Visible

let to_string = function
  | Visible -> "visible"
  | Clip -> "clip"
  | Hidden -> "hidden"
  | Scroll -> "scroll"

let is_container = function Hidden | Scroll -> true | Visible | Clip -> false

let to_automatic_min_size = function
  | Visible | Clip -> Dimension.auto
  | Hidden | Scroll -> Dimension.zero

let equal a b =
  match (a, b) with
  | Visible, Visible -> true
  | Clip, Clip -> true
  | Hidden, Hidden -> true
  | Scroll, Scroll -> true
  | _ -> false

let compare a b =
  let to_int = function
    | Visible -> 0
    | Clip -> 1
    | Hidden -> 2
    | Scroll -> 3
  in
  Int.compare (to_int a) (to_int b)

let pp fmt t = Format.pp_print_string fmt (to_string t)
