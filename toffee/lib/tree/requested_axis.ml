type t = Horizontal | Vertical | Both

let of_absolute_axis = function
  | Geometry.Absolute_axis.Horizontal -> Horizontal
  | Geometry.Absolute_axis.Vertical -> Vertical

let to_absolute_axis = function
  | Horizontal -> Some Geometry.Absolute_axis.Horizontal
  | Vertical -> Some Geometry.Absolute_axis.Vertical
  | Both -> None

let to_string = function
  | Horizontal -> "Horizontal"
  | Vertical -> "Vertical"
  | Both -> "Both"

let compare a b =
  match (a, b) with
  | Horizontal, Horizontal -> 0
  | Horizontal, _ -> -1
  | _, Horizontal -> 1
  | Vertical, Vertical -> 0
  | Vertical, _ -> -1
  | _, Vertical -> 1
  | Both, Both -> 0

let equal a b = compare a b = 0
let pp fmt t = Format.pp_print_string fmt (to_string t)
