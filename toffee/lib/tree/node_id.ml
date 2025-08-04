type t = int

let make n = n
let to_int t = t
let equal = Int.equal
let compare = Int.compare
let pp fmt t = Format.fprintf fmt "NodeId(%d)" t

module Map = Map.Make (Int)
module Set = Set.Make (Int)
