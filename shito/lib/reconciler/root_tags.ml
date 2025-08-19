type t = 
  | Legacy_root
  | Concurrent_root

let to_int = function
  | Legacy_root -> 0
  | Concurrent_root -> 1

let of_int = function
  | 0 -> Some Legacy_root
  | 1 -> Some Concurrent_root
  | _ -> None