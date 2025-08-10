(** Layout snapshot implementation with spatial indexing **)

type rect = { x : int; y : int; w : int; h : int }
type entry = { rect : rect; z_index : int; clipping : rect option }

(* Simple spatial index using grid cells for fast hit testing *)
module Spatial_index = struct
  type cell = (Attr.key * entry) list ref
  type t = { cells : (int * int, cell) Hashtbl.t; cell_size : int }

  let create ?(cell_size = 10) () = { cells = Hashtbl.create 64; cell_size }
  let get_cell_coords t x y = (x / t.cell_size, y / t.cell_size)

  let get_cells_for_rect t rect =
    let x1, y1 = get_cell_coords t rect.x rect.y in
    let x2, y2 =
      get_cell_coords t (rect.x + rect.w - 1) (rect.y + rect.h - 1)
    in
    let cells = ref [] in
    for x = x1 to x2 do
      for y = y1 to y2 do
        cells := (x, y) :: !cells
      done
    done;
    !cells

  let add t key entry =
    let cells = get_cells_for_rect t entry.rect in
    List.iter
      (fun coord ->
        let cell =
          match Hashtbl.find_opt t.cells coord with
          | Some c -> c
          | None ->
              let c = ref [] in
              Hashtbl.add t.cells coord c;
              c
        in
        cell := (key, entry) :: !cell)
      cells

  let query t x y =
    let coord = get_cell_coords t x y in
    match Hashtbl.find_opt t.cells coord with Some cell -> !cell | None -> []

  let clear t = Hashtbl.clear t.cells
end

(* Use a hashtable for O(1) lookups by key and spatial index for hit testing *)
type t = {
  entries : (Attr.key, entry) Hashtbl.t; (* Hashtbl itself is mutable *)
  mutable z_counter : int; (* Auto-increment z-index for insertion order *)
  spatial_index : Spatial_index.t; (* Spatial index for fast hit testing *)
  mutable last_hit_cache : (int * int * Attr.key option) option;
      (* Cache last hit test result *)
}

let create () =
  {
    entries = Hashtbl.create 64;
    z_counter = 0;
    spatial_index = Spatial_index.create ~cell_size:10 ();
    last_hit_cache = None;
  }

let record t (key : Attr.key) entry =
  (* Auto-assign z-index if not set (use negative to indicate auto) *)
  let final_entry =
    if entry.z_index < 0 then (
      let z = t.z_counter in
      t.z_counter <- t.z_counter + 1;
      { entry with z_index = z })
    else entry
  in
  Hashtbl.replace t.entries key final_entry;
  (* Also add to spatial index *)
  Spatial_index.add t.spatial_index key final_entry;
  (* Invalidate hit cache *)
  t.last_hit_cache <- None

let get t key = Hashtbl.find_opt t.entries key

let clear t =
  Hashtbl.clear t.entries;
  t.z_counter <- 0;
  Spatial_index.clear t.spatial_index;
  t.last_hit_cache <- None

let iter t f = Hashtbl.iter f t.entries

let fold t ~init ~f =
  Hashtbl.fold (fun key entry acc -> f acc key entry) t.entries init

let keys t = Hashtbl.fold (fun key _ acc -> key :: acc) t.entries []

let point_in_rect ~x ~y rect =
  x >= rect.x && x < rect.x + rect.w && y >= rect.y && y < rect.y + rect.h

let hit_test_all t ~x ~y =
  (* Use spatial index to get candidates - much faster than checking all entries *)
  let candidates = Spatial_index.query t.spatial_index x y in
  (* Filter candidates that actually contain the point *)
  let hits =
    List.fold_left
      (fun acc (key, entry) ->
        (* Check if point is in the element's rect *)
        if point_in_rect ~x ~y entry.rect then
          (* Also check clipping if present *)
          match entry.clipping with
          | Some clip_rect ->
              if point_in_rect ~x ~y clip_rect then (key, entry) :: acc else acc
          | None -> (key, entry) :: acc
        else acc)
      [] candidates
  in
  (* Sort by z-index, highest first *)
  List.sort (fun (_, e1) (_, e2) -> compare e2.z_index e1.z_index) hits

let hit_test t ~x ~y =
  (* Check cache first *)
  match t.last_hit_cache with
  | Some (cx, cy, result) when cx = x && cy = y -> result
  | _ ->
      let result =
        match hit_test_all t ~x ~y with [] -> None | (key, _) :: _ -> Some key
      in
      t.last_hit_cache <- Some (x, y, result);
      result

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
