type t = 
  | Legacy_root
  | Concurrent_root

val to_int : t -> int
val of_int : int -> t option