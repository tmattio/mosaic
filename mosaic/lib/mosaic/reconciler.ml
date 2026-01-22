module Renderer = Mosaic_ui.Renderer
module Renderable = Mosaic_ui.Renderable

(* Re-export Fiber's child type for convenience *)
type child = Fiber.child = C_fiber of Fiber.t | C_raw of Renderable.t

(* Reconciler State - tracks ALL root children in order *)

type t = {
  renderer : Renderer.t;
  container : Renderable.t;
  mutable root_children : child list;
}

let create renderer ~container = { renderer; container; root_children = [] }
let renderer t = t.renderer
let container t = t.container

(* Flattened child - either an element to reconcile or a raw node to attach *)
type flattened_child =
  | Flattened_element of unit Vnode.element
  | Flattened_raw of Renderable.t

(* Vnode Flattening *)

let rec flatten_children (vnodes : unit Vnode.t list) : flattened_child list =
  List.concat_map flatten_vnode vnodes

and flatten_vnode (vnode : unit Vnode.t) : flattened_child list =
  match vnode with
  | Vnode.Null -> []
  | Vnode.Fragment children -> flatten_children children
  | Vnode.Element elem -> [ Flattened_element elem ]
  | Vnode.Raw node -> [ Flattened_raw node ]

(* Helper functions *)

let node_of_child = function C_fiber f -> Fiber.node f | C_raw n -> n

(* Extract fibers from a child list *)
let fibers_of_children (children : child list) : Fiber.t list =
  List.filter_map (function C_fiber f -> Some f | C_raw _ -> None) children

(* Child Reconciliation *)

(* Check if a fiber can be reused for an element (same tag and key). *)
let can_reuse (fiber : Fiber.t) (elem : unit Vnode.element) : bool =
  Vnode.tag_equal (Fiber.tag fiber) elem.Vnode.tag
  && Fiber.key fiber = elem.Vnode.key

(* Build maps for efficient fiber lookup during reconciliation. Stores both the
   fiber and its original index for O(1) keyed lookup. *)
type fiber_maps = {
  by_key : (string, Fiber.t * int) Hashtbl.t;
  by_index : Fiber.t array;
  used : bool array;
}

let build_fiber_maps (fibers : Fiber.t list) : fiber_maps =
  let arr = Array.of_list fibers in
  let len = Array.length arr in
  let by_key = Hashtbl.create len in
  Array.iteri
    (fun i fiber ->
      match Fiber.key fiber with
      | Some k -> Hashtbl.replace by_key k (fiber, i)
      | None -> ())
    arr;
  { by_key; by_index = arr; used = Array.make len false }

(* Try to find a matching fiber, marking it as used if found *)
let find_matching_fiber maps new_idx (elem : unit Vnode.element) :
    Fiber.t option =
  let old_len = Array.length maps.by_index in
  match elem.key with
  | Some k -> (
      match Hashtbl.find_opt maps.by_key k with
      | Some (fiber, idx) when can_reuse fiber elem && not maps.used.(idx) ->
          maps.used.(idx) <- true;
          Some fiber
      | _ -> None)
  | None ->
      if new_idx < old_len && not maps.used.(new_idx) then
        let fiber = maps.by_index.(new_idx) in
        if can_reuse fiber elem then (
          maps.used.(new_idx) <- true;
          Some fiber)
        else None
      else None

(* Commit placement: ensure children are in correct order under parent. Now
   handles unified child list preserving relative order of fibers and raw
   nodes. *)
let commit_placement (parent_node : Renderable.t) (children : child list) : unit
    =
  let parent_node = Host_config.reconcile_parent parent_node in
  (* Build a set of nodes that should remain *)
  let target_nodes =
    let set = Hashtbl.create (List.length children) in
    List.iter (fun c -> Hashtbl.add set (node_of_child c) ()) children;
    set
  in
  (* First, detach any current children not in our target set *)
  List.iter
    (fun child ->
      if not (Hashtbl.mem target_nodes child) then
        Host_config.detach_if_attached child)
    (Renderable.children parent_node);
  (* Now place each child at its correct index *)
  List.iteri
    (fun i child ->
      let child_node = node_of_child child in
      let current = Renderable.children parent_node in
      let current_len = List.length current in
      let already_correct =
        i < current_len && List.nth current i == child_node
      in
      if not already_correct then (
        Host_config.detach_if_attached child_node;
        if i >= current_len then
          match child with
          | C_fiber f ->
              Host_config.append_child ~parent:parent_node
                ~child:(Fiber.instance f)
          | C_raw n -> (
              match Renderable.append_child ~parent:parent_node ~child:n with
              | Ok () -> ()
              | Error _ -> failwith "Failed to append raw node")
        else
          match child with
          | C_fiber f ->
              Host_config.insert_at ~parent:parent_node
                ~child:(Fiber.instance f) ~index:i
          | C_raw n -> (
              match
                Renderable.insert_child ~parent:parent_node ~index:i ~child:n
              with
              | Ok () -> ()
              | Error _ -> failwith "Failed to insert raw node")))
    children

(* Reconcile a single element, returning the fiber *)
let rec reconcile_element (t : t) maps elem_idx (elem : unit Vnode.element) :
    Fiber.t =
  match find_matching_fiber maps elem_idx elem with
  | Some fiber ->
      (* Reuse existing fiber *)
      let old_props = Fiber.props fiber in
      let new_props = elem.props in
      (* Update props if changed (handlers update via refs in
         Fiber.update_props) *)
      if old_props != new_props then begin
        let changed =
          Host_config.update_props (Fiber.instance fiber) ~old_props ~new_props
        in
        Fiber.update_props fiber new_props;
        (* Only commit update if something actually changed *)
        if changed then Host_config.commit_update (Fiber.instance fiber)
      end;
      (* Recursively reconcile children *)
      let old_children = Fiber.child_list fiber in
      let flattened = flatten_children elem.children in
      let new_children =
        reconcile_flattened t ~parent_node:(Fiber.node fiber) old_children
          flattened
      in
      Fiber.set_child_list fiber new_children;
      fiber
  | None ->
      (* Create new fiber *)
      let instance =
        Host_config.create_instance t.renderer elem.tag elem.props
      in
      let fiber =
        Fiber.create ~tag:elem.tag ~key:elem.key ~props:elem.props ~instance
      in
      (* Invoke ref callback if present *)
      (match elem.props.ref with
      | Some ref_cb -> ref_cb (Host_config.node_of instance)
      | None -> ());
      (* Recursively create children *)
      let flattened = flatten_children elem.children in
      let new_children =
        reconcile_flattened t
          ~parent_node:(Host_config.node_of instance)
          [] flattened
      in
      Fiber.set_child_list fiber new_children;
      fiber

(* Reconcile a flattened child list, preserving order of elements and raw
   nodes *)
and reconcile_flattened (t : t) ~(parent_node : Renderable.t)
    (old_children : child list) (flattened : flattened_child list) : child list
    =
  (* Extract old fibers for matching *)
  let old_fibers = fibers_of_children old_children in
  let maps = build_fiber_maps old_fibers in
  let old_len = Array.length maps.by_index in

  (* Keep track of element index for positional matching *)
  let elem_idx = ref 0 in

  (* Phase 1: Reconcile each flattened child in order *)
  let new_children =
    List.map
      (fun fc ->
        match fc with
        | Flattened_element elem ->
            let idx = !elem_idx in
            incr elem_idx;
            C_fiber (reconcile_element t maps idx elem)
        | Flattened_raw n -> C_raw n)
      flattened
  in

  (* Phase 2: Destroy unused old fibers *)
  for i = 0 to old_len - 1 do
    if not maps.used.(i) then Fiber.destroy maps.by_index.(i)
  done;

  (* Phase 3: Commit placement - ensure correct order in parent *)
  commit_placement parent_node new_children;

  new_children

(* Root Reconciliation *)

let render (t : t) (vnode : unit Vnode.t) : unit =
  let flattened = flatten_vnode vnode in
  let old_children = t.root_children in
  let new_children =
    reconcile_flattened t ~parent_node:t.container old_children flattened
  in
  t.root_children <- new_children;
  Host_config.reset_after_commit t.container

(* Unmounting *)

let unmount (t : t) : unit =
  List.iter
    (function
      | C_fiber f -> Fiber.destroy f
      | C_raw n -> Host_config.detach_if_attached n)
    t.root_children;
  t.root_children <- [];
  Host_config.reset_after_commit t.container
