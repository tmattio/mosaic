(** Key management for Tile components *)

type key = Ui.Attr.key

(** Key generation implementation *)

(* Global counter for generating unique IDs *)
let next_key_id : int ref = ref 0

(* Logging for tile_key module *)
let src = Logs.Src.create "mosaic.tile_key" ~doc:"Tile key generation"
module Log = (val Logs.src_log src : Logs.LOG)

(* Generate a unique key with stable identity per hook call site *)
let use_key ~prefix =
  (* Sanitize and validate prefix *)
  let sanitize_prefix p =
    let p = 
      if String.length p = 0 then begin
        Log.warn (fun m -> m "use_key: empty prefix provided, using 'default'");
        "default"
      end else if String.length p > 100 then begin
        let truncated = String.sub p 0 100 in
        Log.warn (fun m -> m "use_key: prefix too long (%d chars), truncating to 100: %s..." 
          (String.length p) (String.sub p 0 50));
        truncated
      end else p
    in
    (* Replace any null characters or other problematic chars *)
    let cleaned = String.map (fun c ->
      if c = '\000' || c < ' ' then begin
        Log.warn (fun m -> m "use_key: replacing invalid character (code %d) in prefix" (Char.code c));
        '_'
      end else c
    ) p in
    cleaned
  in
  
  (* Use a ref to store the key, making it stable across re-renders *)
  let key_ref = Hook.use_ref None in
  match !key_ref with
  | Some key -> key
  | None ->
      let safe_prefix = sanitize_prefix prefix in
      let id = !next_key_id in
      next_key_id := id + 1;
      let key_str = Printf.sprintf "%s_%d" safe_prefix id in
      (* Final safety check *)
      let final_key_str = 
        if String.length key_str > 200 then begin
          let truncated = String.sub key_str 0 200 in
          Log.warn (fun m -> m "use_key: generated key too long, truncating: %s..." 
            (String.sub key_str 0 100));
          truncated
        end else key_str
      in
      let key = Ui.Attr.key final_key_str in
      key_ref := Some key;
      key

let with_key = Ui.with_key
