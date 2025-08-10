(** Layout snapshot implementation *)

type rect = { x : int; y : int; w : int; h : int }
type entry = { rect : rect; z_index : int; clipping : rect option }

(* Use a hashtable for O(1) lookups by key *)
type t = {
  entries : (Attr.key, entry) Hashtbl.t; (* Hashtbl itself is mutable *)
  mutable z_counter : int; (* Auto-increment z-index for insertion order *)
}

let create () = { entries = Hashtbl.create 64; z_counter = 0 }

let record t (key : Attr.key) entry =
  (* Auto-assign z-index if not set (use negative to indicate auto) *)
  let final_entry =
    if entry.z_index < 0 then (
      let z = t.z_counter in
      t.z_counter <- t.z_counter + 1;
      { entry with z_index = z })
    else entry
  in
  Hashtbl.replace t.entries key final_entry

let get t key = Hashtbl.find_opt t.entries key

let clear t =
  Hashtbl.clear t.entries;
  t.z_counter <- 0

let iter t f = Hashtbl.iter f t.entries

let fold t ~init ~f =
  Hashtbl.fold (fun key entry acc -> f acc key entry) t.entries init

let keys t = Hashtbl.fold (fun key _ acc -> key :: acc) t.entries []

let point_in_rect ~x ~y rect =
  x >= rect.x && x < rect.x + rect.w && y >= rect.y && y < rect.y + rect.h

let hit_test_all t ~x ~y =
  (* Collect all elements that contain the point *)
  let hits =
    Hashtbl.fold
      (fun key entry acc ->
        (* Check if point is in the element's rect *)
        if point_in_rect ~x ~y entry.rect then
          (* Also check clipping if present *)
          match entry.clipping with
          | Some clip_rect ->
              if point_in_rect ~x ~y clip_rect then (key, entry) :: acc else acc
          | None -> (key, entry) :: acc
        else acc)
      t.entries []
  in
  (* Sort by z-index, highest first *)
  List.sort (fun (_, e1) (_, e2) -> compare e2.z_index e1.z_index) hits

let hit_test t ~x ~y =
  match hit_test_all t ~x ~y with [] -> None | (key, _) :: _ -> Some key

let size t = Hashtbl.length t.entries

(* Thread-local storage for current snapshot *)
let current_snapshot = ref None
let set_current snapshot = current_snapshot := snapshot
let get_current () = !current_snapshot

let with_recording snapshot f =
  let old = !current_snapshot in
  current_snapshot := Some snapshot;
  try
    let result = f () in
    current_snapshot := old;
    result
  with e ->
    current_snapshot := old;
    raise e
