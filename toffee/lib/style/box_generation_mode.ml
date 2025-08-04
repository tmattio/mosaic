(** Defines whether a box should be generated or not *)

type t =
  | Normal  (** The node generates a box in the regular way *)
  | None
      (** The node and its descendants generate no boxes (they are hidden) *)

let default = Normal
let to_string = function Normal -> "normal" | None -> "none"

let equal a b =
  match (a, b) with Normal, Normal -> true | None, None -> true | _ -> false

let compare a b =
  let to_int = function Normal -> 0 | None -> 1 in
  Int.compare (to_int a) (to_int b)

let pp fmt t = Format.pp_print_string fmt (to_string t)
