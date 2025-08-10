(** Key generation and management for UI elements *)

(* Counter for generating unique IDs *)
let next_id = ref 0

(* Generate a new unique key - note: this generates a new key each time *)
let create ~prefix =
  let id = !next_id in
  next_id := !next_id + 1;
  Attr.key (Printf.sprintf "%s_%d" prefix id)

(* Create a key directly from a string *)
let of_string s = Attr.key s

(* Create a key from an int *)
let of_int i = Attr.key (string_of_int i)
