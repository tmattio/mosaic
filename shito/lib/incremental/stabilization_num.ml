type t = int

let compare = Int.compare
let equal = Int.equal
let to_int t = t
let zero = 0

let invariant t = assert (t >= -1)
let none = -1
let is_none t = t = none
let is_some t = t >= 0
let add1 t = t + 1
