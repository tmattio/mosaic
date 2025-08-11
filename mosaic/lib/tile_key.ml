(** Key generation implementation *)

(* Global counter for generating unique IDs *)
let next_id : int ref = ref 0

(* Generate a unique key with stable identity per hook call site *)
let use_key ~prefix =
  (* Use a ref to store the key, making it stable across re-renders *)
  let key_ref = Hook.use_ref None in
  match !key_ref with
  | Some key -> key
  | None ->
      let id = !next_id in
      next_id := id + 1;
      let key = Ui.Attr.key (Printf.sprintf "%s_%d" prefix id) in
      key_ref := Some key;
      key
