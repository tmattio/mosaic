type t = int

let compare = Int.compare
let equal = Int.equal
let hash = Hashtbl.hash
let to_string = string_of_int

let invariant t = assert (t >= 1)

let next =
  let r = Atomic.make 0 in
  fun () -> Atomic.fetch_and_add r 1 + 1
