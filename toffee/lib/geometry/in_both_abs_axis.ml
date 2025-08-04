(* Container that holds an item in each absolute axis *)

type 'a t = { horizontal : 'a; vertical : 'a }

let make ~horizontal ~vertical = { horizontal; vertical }

let get t axis =
  match axis with
  | Absolute_axis.Horizontal -> t.horizontal
  | Absolute_axis.Vertical -> t.vertical

let map f t = { horizontal = f t.horizontal; vertical = f t.vertical }

let map2 f t1 t2 =
  {
    horizontal = f t1.horizontal t2.horizontal;
    vertical = f t1.vertical t2.vertical;
  }

let equal eq t1 t2 =
  eq t1.horizontal t2.horizontal && eq t1.vertical t2.vertical

let compare cmp t1 t2 =
  match cmp t1.horizontal t2.horizontal with
  | 0 -> cmp t1.vertical t2.vertical
  | c -> c

let to_string f t =
  Printf.sprintf "{ horizontal = %s; vertical = %s }" (f t.horizontal)
    (f t.vertical)

let pp f fmt t =
  Format.fprintf fmt "{ horizontal = %a; vertical = %a }" f t.horizontal f
    t.vertical
