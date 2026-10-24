type t = Block | Flex | Grid | None

let default = Flex

let to_string = function
  | Block -> "block"
  | Flex -> "flex"
  | Grid -> "grid"
  | None -> "none"

let is_none = function None -> true | Block | Flex | Grid -> false

let equal a b =
  match (a, b) with
  | Block, Block -> true
  | Flex, Flex -> true
  | Grid, Grid -> true
  | None, None -> true
  | _ -> false

let compare a b =
  let to_int = function Block -> 0 | Flex -> 1 | Grid -> 2 | None -> 3 in
  Int.compare (to_int a) (to_int b)

let pp fmt t = Format.pp_print_string fmt (to_string t)
