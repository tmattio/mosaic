(** Type-safe dependency tracking for React hooks *)

type key = K : 'a Type.Id.t * 'a * ('a -> 'a -> bool) -> key
type t = Always | Once | Keys of key array

let always = Always
let once = Once
let keys ks = Keys (Array.of_list ks)

(* Stable type IDs for common primitives *)
let int_id : int Type.Id.t = Type.Id.make ()
let bool_id : bool Type.Id.t = Type.Id.make ()
let float_id : float Type.Id.t = Type.Id.make ()
let string_id : string Type.Id.t = Type.Id.make ()
let ui_key_id : Ui.Attr.key Type.Id.t = Type.Id.make ()
let int x = K (int_id, x, Int.equal)
let bool x = K (bool_id, x, Bool.equal)
let string x = K (string_id, x, String.equal)

let float ?(epsilon = 0.) x =
  let eq a b =
    if epsilon = 0. then Float.equal a b else Float.abs (a -. b) <= epsilon
  in
  K (float_id, x, eq)

let phys (type a) (x : a) =
  (* Create a fresh type id for this specific use *)
  let id : a Type.Id.t = Type.Id.make () in
  K (id, x, ( == ))

let custom ~id ~equal x = K (id, x, equal)
let ui_key x = K (ui_key_id, x, ( = ))

(* Compare two dependency arrays for changes *)
let changed old_deps new_deps =
  match (old_deps, new_deps) with
  | Always, _ | _, Always -> true
  | Once, Once -> false
  | Once, _ | _, Once -> true
  | Keys xs, Keys ys ->
      let n = Array.length xs in
      n <> Array.length ys
      ||
      let rec loop i =
        if i = n then false
        else
          let (K (id1, v1, eq1)) = xs.(i) in
          let (K (id2, v2, _)) = ys.(i) in
          match Type.Id.provably_equal id1 id2 with
          | None -> true (* Different types = changed *)
          | Some Type.Equal -> if eq1 v1 v2 then loop (i + 1) else true
      in
      loop 0
