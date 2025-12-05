(** Optimized focus manager implementation with O(1) lookups **)

type focusable = {
  key : Ui.Attr.key;
  tab_index : int option;
  auto_focus : bool;
  order : int; (* insertion order *)
}

(* Use priority queue for maintaining sorted tab order *)
module Tab_order = struct
  type t = focusable array ref * int ref (* array and size *)

  let create () = (ref [||], ref 0)

  let compare_focusable a b =
    (* Sort by (has_index, tab_index, insertion_order) for stable ordering *)
    let rank = function Some i -> (0, i) | None -> (1, max_int) in
    match compare (rank a.tab_index) (rank b.tab_index) with
    | 0 -> compare a.order b.order
    | c -> c

  let rebuild (arr_ref, size_ref) focusables =
    let sorted = List.sort compare_focusable focusables in
    arr_ref := Array.of_list sorted;
    size_ref := Array.length !arr_ref

  let find_index (arr_ref, size_ref) key =
    (* Linear search since we need to find by key, not by sort order *)
    let rec linear_search i =
      if i >= !size_ref then None
      else if !arr_ref.(i).key = key then Some i
      else linear_search (i + 1)
    in
    linear_search 0
end

type t = {
  focusables_map : (Ui.Attr.key, focusable) Hashtbl.t; (* O(1) lookups *)
  mutable tab_order : Tab_order.t; (* Pre-sorted array for fast navigation *)
  mutable focused : Ui.Attr.key option;
  mutable router : Input_router.t option;
  mutable next_order : int; (* counter for insertion order *)
  mutable dirty : bool; (* Flag to rebuild tab_order when needed *)
}

let create () =
  {
    focusables_map = Hashtbl.create 64;
    tab_order = Tab_order.create ();
    focused = None;
    router = None;
    next_order = 0;
    dirty = false;
  }

let set_router t router = t.router <- Some router

let rebuild_tab_order t =
  if t.dirty then (
    let focusables =
      Hashtbl.fold (fun _ f acc -> f :: acc) t.focusables_map []
    in
    Tab_order.rebuild t.tab_order focusables;
    t.dirty <- false)

let register t focusable =
  (* Preserve previous order if re-registering; else assign new order *)
  let prev = Hashtbl.find_opt t.focusables_map focusable.key in
  let order =
    match prev with
    | Some f -> f.order
    | None ->
        let o = t.next_order in
        t.next_order <- o + 1;
        o
  in
  let focusable = { focusable with order } in
  (* Add/replace in hashtable - O(1) *)
  Hashtbl.replace t.focusables_map focusable.key focusable;
  t.dirty <- true;

  (* Mark for rebuild *)

  (* Auto-focus if requested and nothing focused yet *)
  if focusable.auto_focus && t.focused = None then
    t.focused <- Some focusable.key

let unregister t key =
  if Hashtbl.mem t.focusables_map key then (
    Hashtbl.remove t.focusables_map key;
    t.dirty <- true;
    if t.focused = Some key then t.focused <- None)

let focus t key =
  (* Check if the key is registered as focusable - O(1) *)
  if Hashtbl.mem t.focusables_map key then (
    t.focused <- Some key;
    (* Notify router if connected *)
    Option.iter (fun r -> Input_router.set_focused r (Some key)) t.router)

let blur t =
  t.focused <- None;
  (* Notify router if connected *)
  Option.iter (fun r -> Input_router.set_focused r None) t.router

let get_focused t = t.focused

let focus_first t =
  rebuild_tab_order t;
  let arr_ref, size_ref = t.tab_order in
  if !size_ref > 0 then focus t !arr_ref.(0).key

let focus_last t =
  rebuild_tab_order t;
  let arr_ref, size_ref = t.tab_order in
  if !size_ref > 0 then focus t !arr_ref.(!size_ref - 1).key

let focus_next t =
  rebuild_tab_order t;
  let arr_ref, size_ref = t.tab_order in
  if !size_ref = 0 then ()
  else
    match t.focused with
    | None -> focus t !arr_ref.(0).key
    | Some current -> (
        match Tab_order.find_index t.tab_order current with
        | None -> focus t !arr_ref.(0).key (* Not found, focus first *)
        | Some idx ->
            let next_idx = (idx + 1) mod !size_ref in
            focus t !arr_ref.(next_idx).key)

let focus_prev t =
  rebuild_tab_order t;
  let arr_ref, size_ref = t.tab_order in
  if !size_ref = 0 then ()
  else
    match t.focused with
    | None -> focus t !arr_ref.(!size_ref - 1).key
    | Some current -> (
        match Tab_order.find_index t.tab_order current with
        | None ->
            focus t !arr_ref.(!size_ref - 1).key (* Not found, focus last *)
        | Some idx ->
            let prev_idx = if idx = 0 then !size_ref - 1 else idx - 1 in
            focus t !arr_ref.(prev_idx).key)

let handle_tab t ~forward =
  if forward then focus_next t else focus_prev t;
  true

let clear t =
  Hashtbl.clear t.focusables_map;
  t.tab_order <- Tab_order.create ();
  t.focused <- None;
  t.dirty <- false
