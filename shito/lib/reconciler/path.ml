type t = int list

let root = []
let is_root = function [] -> true | _ -> false
let push idx p = p @ [ idx ]
let append a b = a @ b

let rec pp fmt = function
  | [] -> Format.fprintf fmt "/"
  | [ i ] -> Format.fprintf fmt "/%d" i
  | i :: rest -> Format.fprintf fmt "/%d%a" i pp rest

let to_string p =
  let b = Buffer.create 16 in
  let rec aux = function
    | [] -> Buffer.add_char b '/'
    | [ i ] -> Buffer.add_string b ("/" ^ string_of_int i)
    | i :: rest ->
        Buffer.add_string b ("/" ^ string_of_int i);
        aux rest
  in
  aux p;
  Buffer.contents b

let rec compare a b =
  match (a, b) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
      let c = Stdlib.compare x y in
      if c <> 0 then c else compare xs ys
