type t = { idx : int; gen : int }

let make idx = { idx; gen = 0 }
let make_with_generation idx gen = { idx; gen }
let to_int t = t.idx
let index t = t.idx
let generation t = t.gen
let equal lhs rhs = lhs.idx = rhs.idx && lhs.gen = rhs.gen

let compare lhs rhs =
  match Int.compare lhs.idx rhs.idx with
  | 0 -> Int.compare lhs.gen rhs.gen
  | non_zero -> non_zero

let pp fmt t = Format.fprintf fmt "NodeId(%d,%d)" t.idx t.gen

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)
