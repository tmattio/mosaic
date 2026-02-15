type entry = { sample : Glyph.t; mutable count : int }

type t = {
  (* Map from Grapheme payload (ID + generation) -> local reference count *)
  counts : (int, entry) Hashtbl.t;
  pool : Glyph.pool;
  mutable unique : int;
}

(* Payload mask covers index (bits 0-17) + generation (bits 18-24) = 25 bits.
   Must match Cell_code.mask_payload exactly to avoid aliasing bugs. *)
let payload_mask = 0x01FFFFFF

let mask_index = 0x3FFFF

let[@inline] payload_key id =
  if Glyph.is_simple id then None
  else
    let key = id land payload_mask in
    (* Pool index 0 is never allocated (slots start at 1). A complex cell with
       index 0 is a continuation of a simple wide glyph — no pool entry. *)
    if key land mask_index = 0 then None else Some key

let create pool = { counts = Hashtbl.create 128; pool; unique = 0 }

let add t id =
  match payload_key id with
  | None -> ()
  | Some key -> (
      match Hashtbl.find_opt t.counts key with
      | Some entry -> entry.count <- entry.count + 1
      | None ->
          (* First sighting of this grapheme in the grid: grab a pool ref
             once *)
          Glyph.incref t.pool id;
          Hashtbl.add t.counts key { sample = id; count = 1 };
          t.unique <- t.unique + 1)

let remove t id =
  match payload_key id with
  | None -> ()
  | Some key -> (
      match Hashtbl.find_opt t.counts key with
      | Some entry when entry.count = 1 ->
          Glyph.decref t.pool entry.sample;
          Hashtbl.remove t.counts key;
          t.unique <- t.unique - 1
      | Some entry -> entry.count <- entry.count - 1
      | None -> ())

let replace t ~old_id ~new_id =
  if old_id <> new_id then (
    add t new_id;
    remove t old_id)

let clear t =
  Hashtbl.iter (fun _ entry -> Glyph.decref t.pool entry.sample) t.counts;
  Hashtbl.clear t.counts;
  t.unique <- 0

let unique_count t = t.unique
