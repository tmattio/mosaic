(** Key generation implementation *)

(* Domain-local counter for generating unique IDs *)
let next_id_key : int ref Domain.DLS.key = Domain.DLS.new_key (fun () -> ref 0)

(* Generate a unique key with stable identity per hook call site *)
let use_key ~prefix =
  (* Use a ref to store the key, making it stable across re-renders *)
  let key_ref = Hook.use_ref None in
  match !key_ref with
  | Some key -> key
  | None ->
      let r = Domain.DLS.get next_id_key in
      let id = !r in
      r := id + 1;
      let key = Ui.Attr.key (Printf.sprintf "%s_%d" prefix id) in
      key_ref := Some key;
      key
