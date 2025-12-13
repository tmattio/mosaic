type t = Auto | Legacy_left | Legacy_right | Legacy_center

let default = Auto

let to_string = function
  | Auto -> "auto"
  | Legacy_left -> "legacy-left"
  | Legacy_right -> "legacy-right"
  | Legacy_center -> "legacy-center"

let equal a b =
  match (a, b) with
  | Auto, Auto -> true
  | Legacy_left, Legacy_left -> true
  | Legacy_right, Legacy_right -> true
  | Legacy_center, Legacy_center -> true
  | _ -> false

let compare a b =
  let to_int = function
    | Auto -> 0
    | Legacy_left -> 1
    | Legacy_right -> 2
    | Legacy_center -> 3
  in
  Int.compare (to_int a) (to_int b)

let pp fmt t = Format.pp_print_string fmt (to_string t)
