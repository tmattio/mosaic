type t =
  | Row (* Fill rows first *)
  | Column (* Fill columns first *)
  | Row_dense (* Fill rows first, packing items *)
  | Column_dense (* Fill columns first, packing items *)

let default = Row

let to_string = function
  | Row -> "row"
  | Column -> "column"
  | Row_dense -> "row dense"
  | Column_dense -> "column dense"

let equal a b =
  match (a, b) with
  | Row, Row -> true
  | Column, Column -> true
  | Row_dense, Row_dense -> true
  | Column_dense, Column_dense -> true
  | _ -> false

let compare a b =
  let to_int = function
    | Row -> 0
    | Column -> 1
    | Row_dense -> 2
    | Column_dense -> 3
  in
  Int.compare (to_int a) (to_int b)

let pp fmt t = Format.pp_print_string fmt (to_string t)

let is_dense = function
  | Row | Column -> false
  | Row_dense | Column_dense -> true

let primary_axis = function
  | Row | Row_dense -> Geometry.Abstract_axis.Inline
  | Column | Column_dense -> Geometry.Abstract_axis.Block
