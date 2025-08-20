type t = string option

let none = None
let of_string s = Some s
let is_some = function Some _ -> true | None -> false
let is_none = function Some _ -> false | None -> true

let equal (a : t) (b : t) =
  match (a, b) with
  | None, None -> true
  | Some x, Some y -> String.equal x y
  | _ -> false

let compare (a : t) (b : t) =
  match (a, b) with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x, Some y -> Stdlib.compare x y

let to_string = function None -> "" | Some s -> s

let pp fmt = function
  | None -> Format.fprintf fmt "None"
  | Some s -> Format.fprintf fmt "Some(%S)" s
