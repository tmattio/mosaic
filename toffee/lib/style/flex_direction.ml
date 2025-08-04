(** The direction of the flexbox layout main axis *)

type t =
  | Row
      (** Defines +x as the main axis. Items will be added from left to right *)
  | Column
      (** Defines +y as the main axis. Items will be added from top to bottom *)
  | Row_reverse
      (** Defines -x as the main axis. Items will be added from right to left *)
  | Column_reverse
      (** Defines -y as the main axis. Items will be added from bottom to top *)

let default = Row

let is_row = function
  | Row | Row_reverse -> true
  | Column | Column_reverse -> false

let is_column t = not (is_row t)

let is_reverse = function
  | Row_reverse | Column_reverse -> true
  | Row | Column -> false

let main_axis = function
  | Row | Row_reverse -> Geometry.Absolute_axis.Horizontal
  | Column | Column_reverse -> Geometry.Absolute_axis.Vertical

let cross_axis = function
  | Row | Row_reverse -> Geometry.Absolute_axis.Vertical
  | Column | Column_reverse -> Geometry.Absolute_axis.Horizontal

let to_string = function
  | Row -> "row"
  | Column -> "column"
  | Row_reverse -> "row-reverse"
  | Column_reverse -> "column-reverse"

let equal a b =
  match (a, b) with
  | Row, Row -> true
  | Column, Column -> true
  | Row_reverse, Row_reverse -> true
  | Column_reverse, Column_reverse -> true
  | _ -> false

let compare a b =
  let to_int = function
    | Row -> 0
    | Column -> 1
    | Row_reverse -> 2
    | Column_reverse -> 3
  in
  Int.compare (to_int a) (to_int b)

let pp fmt t = Format.pp_print_string fmt (to_string t)
