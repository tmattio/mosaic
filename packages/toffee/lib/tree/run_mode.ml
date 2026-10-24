type t = Perform_layout | Compute_size | Perform_hidden_layout

let to_string = function
  | Perform_layout -> "PerformLayout"
  | Compute_size -> "ComputeSize"
  | Perform_hidden_layout -> "PerformHiddenLayout"

let compare a b =
  match (a, b) with
  | Perform_layout, Perform_layout -> 0
  | Perform_layout, _ -> -1
  | _, Perform_layout -> 1
  | Compute_size, Compute_size -> 0
  | Compute_size, _ -> -1
  | _, Compute_size -> 1
  | Perform_hidden_layout, Perform_hidden_layout -> 0

let equal a b = compare a b = 0
let pp fmt t = Format.pp_print_string fmt (to_string t)
