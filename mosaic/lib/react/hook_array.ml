type 'a id = 'a Type.Id.t

module Sub = Engine.Sub

type _ kind =
  | State : 'a ref -> 'a kind
  | Effect : {
      cleanup : (unit -> unit) option;
      deps : Obj.t array option;
    }
      -> unit kind
  | Sub : 'msg Sub.t -> unit kind
  | Hole : unit kind

type slot = { mutable cell : unit kind }

let make_slot k = { cell = (k :> unit kind) }
let get s = s.cell
let set s k = s.cell <- (k :> unit kind)

type t = { mutable arr : slot array; mutable len : int }

let create () = { arr = [||]; len = 0 }
let length t = t.len

let ensure_capacity t n =
  if n <= Array.length t.arr then ()
  else
    let new_cap = max (Array.length t.arr * 2) (max 4 n) in
    let new_arr =
      Array.init new_cap (fun i ->
          if i < Array.length t.arr then t.arr.(i) else make_slot Hole)
    in
    t.arr <- new_arr

let get_slot t idx =
  if idx < t.len then t.arr.(idx)
  else (
    ensure_capacity t (idx + 1);
    for i = t.len to idx do
      t.arr.(i) <- make_slot Hole
    done;
    t.len <- idx + 1;
    t.arr.(idx))
