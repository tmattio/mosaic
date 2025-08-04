(** Controls whether a node's position is determined by the normal flow of
    layout or not *)

type t =
  | Relative
      (** The offset is computed relative to the final position given by the
          layout algorithm. Offsets do not affect the position of any other
          items. *)
  | Absolute
      (** The node is taken out of the normal flow of layout and positioned
          relative to its parent. *)

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
