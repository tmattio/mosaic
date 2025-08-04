(* Toffee: An OCaml port of Taffy's tree module *)

open Tree
open Geometry
open Compute
module Geometry = Geometry
module Style = Style

(* Tree submodule exports *)
module Node_id = Node_id
module Layout = Layout
module Available_space = Available_space
module Layout_input = Layout_input
module Layout_output = Layout_output
module Run_mode = Run_mode
module Cache = Cache

module Error = struct
  (* Error types *)
  type t =
    | Child_index_out_of_bounds of {
        parent : Node_id.t;
        child_index : int;
        child_count : int;
      }
    | Invalid_parent_node of Node_id.t
    | Invalid_child_node of Node_id.t
    | Invalid_input_node of Node_id.t

  let to_string = function
    | Child_index_out_of_bounds { parent; child_index; child_count } ->
        Printf.sprintf
          "Index (is %d) should be < child_count (%d) for parent node %d"
          child_index child_count (Node_id.to_int parent)
    | Invalid_parent_node parent ->
        Printf.sprintf "Parent Node %d is not in the TaffyTree instance"
          (Node_id.to_int parent)
    | Invalid_child_node child ->
        Printf.sprintf "Child Node %d is not in the TaffyTree instance"
          (Node_id.to_int child)
    | Invalid_input_node node ->
        Printf.sprintf "Supplied Node %d is not in the TaffyTree instance"
          (Node_id.to_int node)
end

open Error

type nonrec 'a result = ('a, Error.t) result
type config = { use_rounding : bool }
(* Configuration for TaffyTree *)

let default_config = { use_rounding = true }

type node_data = {
  style : Style.t;
  unrounded_layout : Layout.t;
  final_layout : Layout.t;
  has_context : bool;
  cache : Cache.t;
}
(* Node data stored for each node *)

let new_node_data style =
  {
    style;
    cache = Cache.make ();
    unrounded_layout = Layout.default;
    final_layout = Layout.default;
    has_context = false;
  }

let mark_dirty node_data = Cache.clear node_data.cache

type 'context tree = {
  mutable nodes : node_data Node_id.Map.t;
  mutable node_context_data : 'context Node_id.Map.t;
  mutable children : Node_id.t list Node_id.Map.t;
  mutable parents : Node_id.t option Node_id.Map.t;
  mutable next_id : int;
  config : config;
}
(* The main TaffyTree type *)

(* Create a new TaffyTree *)
let new_tree () =
  {
    nodes = Node_id.Map.empty;
    node_context_data = Node_id.Map.empty;
    children = Node_id.Map.empty;
    parents = Node_id.Map.empty;
    next_id = 0;
    config = default_config;
  }

let with_capacity _capacity = new_tree ()

(* Enable/disable rounding *)
let enable_rounding tree = { tree with config = { use_rounding = true } }
let disable_rounding tree = { tree with config = { use_rounding = false } }

(* Node creation *)
let new_leaf tree style =
  let id = Node_id.make tree.next_id in
  tree.next_id <- tree.next_id + 1;
  tree.nodes <- Node_id.Map.add id (new_node_data style) tree.nodes;
  tree.children <- Node_id.Map.add id [] tree.children;
  tree.parents <- Node_id.Map.add id None tree.parents;
  Ok id

let new_leaf_with_context tree style context =
  let id = Node_id.make tree.next_id in
  tree.next_id <- tree.next_id + 1;
  let node_data = { (new_node_data style) with has_context = true } in
  tree.nodes <- Node_id.Map.add id node_data tree.nodes;
  tree.node_context_data <- Node_id.Map.add id context tree.node_context_data;
  tree.children <- Node_id.Map.add id [] tree.children;
  tree.parents <- Node_id.Map.add id None tree.parents;
  Ok id

let new_with_children tree style children =
  let id = Node_id.make tree.next_id in
  tree.next_id <- tree.next_id + 1;
  tree.nodes <- Node_id.Map.add id (new_node_data style) tree.nodes;
  tree.children <- Node_id.Map.add id (Array.to_list children) tree.children;
  tree.parents <- Node_id.Map.add id None tree.parents;

  (* Set parent for all children *)
  Array.iter
    (fun child -> tree.parents <- Node_id.Map.add child (Some id) tree.parents)
    children;

  Ok id

(* Tree operations *)
let clear tree =
  tree.nodes <- Node_id.Map.empty;
  tree.node_context_data <- Node_id.Map.empty;
  tree.children <- Node_id.Map.empty;
  tree.parents <- Node_id.Map.empty;
  tree.next_id <- 0

let remove tree node =
  match Node_id.Map.find_opt node tree.parents with
  | None -> Error (Invalid_input_node node)
  | Some parent_opt ->
      (* Remove from parent's children list *)
      (match parent_opt with
      | Some parent ->
          let parent_children = Node_id.Map.find parent tree.children in
          let new_children =
            List.filter (fun n -> not (Node_id.equal n node)) parent_children
          in
          tree.children <- Node_id.Map.add parent new_children tree.children
      | None -> ());

      (* Remove parent references from this node's children *)
      (match Node_id.Map.find_opt node tree.children with
      | Some children ->
          List.iter
            (fun child ->
              tree.parents <- Node_id.Map.add child None tree.parents)
            children
      | None -> ());

      (* Remove the node *)
      tree.nodes <- Node_id.Map.remove node tree.nodes;
      tree.node_context_data <- Node_id.Map.remove node tree.node_context_data;
      tree.children <- Node_id.Map.remove node tree.children;
      tree.parents <- Node_id.Map.remove node tree.parents;

      Ok node

(* Context operations *)
let rec mark_dirty_recursive tree node =
  match Node_id.Map.find_opt node tree.nodes with
  | None -> ()
  | Some node_data -> (
      match mark_dirty node_data with
      | Cache.Already_empty -> ()
      | Cache.Cleared -> (
          tree.nodes <-
            Node_id.Map.add node
              { node_data with cache = node_data.cache }
              tree.nodes;
          match Node_id.Map.find_opt node tree.parents with
          | Some (Some parent) -> mark_dirty_recursive tree parent
          | _ -> ()))

let set_node_context tree node context_opt =
  match Node_id.Map.find_opt node tree.nodes with
  | None -> Error (Invalid_input_node node)
  | Some node_data ->
      (match context_opt with
      | Some context ->
          tree.nodes <-
            Node_id.Map.add node
              { node_data with has_context = true }
              tree.nodes;
          tree.node_context_data <-
            Node_id.Map.add node context tree.node_context_data
      | None ->
          tree.nodes <-
            Node_id.Map.add node
              { node_data with has_context = false }
              tree.nodes;
          tree.node_context_data <-
            Node_id.Map.remove node tree.node_context_data);
      mark_dirty_recursive tree node;
      Ok ()

let get_node_context tree node =
  Node_id.Map.find_opt node tree.node_context_data

let get_node_context_mut tree node =
  Node_id.Map.find_opt node tree.node_context_data

(* Child management *)
let add_child tree parent child =
  match Node_id.Map.find_opt parent tree.children with
  | None -> Error (Invalid_parent_node parent)
  | Some children ->
      tree.parents <- Node_id.Map.add child (Some parent) tree.parents;
      tree.children <-
        Node_id.Map.add parent (children @ [ child ]) tree.children;
      mark_dirty_recursive tree parent;
      Ok ()

let insert_child_at_index tree parent child_index child =
  match Node_id.Map.find_opt parent tree.children with
  | None -> Error (Invalid_parent_node parent)
  | Some children ->
      let child_count = List.length children in
      if child_index > child_count then
        Error (Child_index_out_of_bounds { parent; child_index; child_count })
      else
        let rec insert_at idx lst =
          match (idx, lst) with
          | 0, _ -> child :: lst
          | n, h :: t -> h :: insert_at (n - 1) t
          | _, [] -> [ child ]
        in
        tree.parents <- Node_id.Map.add child (Some parent) tree.parents;
        tree.children <-
          Node_id.Map.add parent (insert_at child_index children) tree.children;
        mark_dirty_recursive tree parent;
        Ok ()

let set_children tree parent new_children =
  match Node_id.Map.find_opt parent tree.children with
  | None -> Error (Invalid_parent_node parent)
  | Some old_children ->
      (* Remove parent from old children *)
      List.iter
        (fun child -> tree.parents <- Node_id.Map.add child None tree.parents)
        old_children;

      (* Remove children from their previous parents and set new parent *)
      Array.iter
        (fun child ->
          (match Node_id.Map.find_opt child tree.parents with
          | Some (Some old_parent) when not (Node_id.equal old_parent parent) ->
              let old_parent_children =
                Node_id.Map.find old_parent tree.children
              in
              let new_children =
                List.filter
                  (fun n -> not (Node_id.equal n child))
                  old_parent_children
              in
              tree.children <-
                Node_id.Map.add old_parent new_children tree.children
          | _ -> ());
          tree.parents <- Node_id.Map.add child (Some parent) tree.parents)
        new_children;

      tree.children <-
        Node_id.Map.add parent (Array.to_list new_children) tree.children;
      mark_dirty_recursive tree parent;
      Ok ()

let rec remove_child tree parent child =
  match Node_id.Map.find_opt parent tree.children with
  | None -> Error (Invalid_parent_node parent)
  | Some children -> (
      match List.find_opt (fun n -> Node_id.equal n child) children with
      | None -> Error (Invalid_child_node child)
      | Some _ -> (
          match
            List.find_index
              (fun (_, n) -> Node_id.equal n child)
              (List.mapi (fun i x -> (i, x)) children)
          with
          | None -> Error (Invalid_child_node child)
          | Some idx -> remove_child_at_index tree parent idx))

and remove_child_at_index tree parent child_index =
  match Node_id.Map.find_opt parent tree.children with
  | None -> Error (Invalid_parent_node parent)
  | Some children ->
      let child_count = List.length children in
      if child_index >= child_count then
        Error (Child_index_out_of_bounds { parent; child_index; child_count })
      else
        let rec remove_at idx lst =
          match (idx, lst) with
          | 0, h :: t -> (h, t)
          | n, h :: t ->
              let removed, rest = remove_at (n - 1) t in
              (removed, h :: rest)
          | _, [] -> failwith "Index out of bounds"
        in
        let removed_child, new_children = remove_at child_index children in
        tree.parents <- Node_id.Map.add removed_child None tree.parents;
        tree.children <- Node_id.Map.add parent new_children tree.children;
        mark_dirty_recursive tree parent;
        Ok removed_child

let remove_children_range tree parent range =
  match Node_id.Map.find_opt parent tree.children with
  | None -> Error (Invalid_parent_node parent)
  | Some children ->
      let start_idx, end_idx = range in
      let rec remove_range idx lst acc =
        match lst with
        | [] -> List.rev acc
        | h :: t ->
            if idx >= start_idx && idx <= end_idx then (
              tree.parents <- Node_id.Map.add h None tree.parents;
              remove_range (idx + 1) t acc)
            else remove_range (idx + 1) t (h :: acc)
      in
      let new_children = remove_range 0 children [] in
      tree.children <- Node_id.Map.add parent new_children tree.children;
      mark_dirty_recursive tree parent;
      Ok ()

let replace_child_at_index tree parent child_index new_child =
  match Node_id.Map.find_opt parent tree.children with
  | None -> Error (Invalid_parent_node parent)
  | Some children ->
      let child_count = List.length children in
      if child_index >= child_count then
        Error (Child_index_out_of_bounds { parent; child_index; child_count })
      else
        let rec replace_at idx lst =
          match (idx, lst) with
          | 0, h :: t ->
              tree.parents <- Node_id.Map.add h None tree.parents;
              (h, new_child :: t)
          | n, h :: t ->
              let old, rest = replace_at (n - 1) t in
              (old, h :: rest)
          | _, [] -> failwith "Index out of bounds"
        in
        let old_child, new_children = replace_at child_index children in
        tree.parents <- Node_id.Map.add new_child (Some parent) tree.parents;
        tree.children <- Node_id.Map.add parent new_children tree.children;
        mark_dirty_recursive tree parent;
        Ok old_child

(* Query operations *)
let child_at_index tree parent child_index =
  match Node_id.Map.find_opt parent tree.children with
  | None -> Error (Invalid_parent_node parent)
  | Some children ->
      let child_count = List.length children in
      if child_index >= child_count then
        Error (Child_index_out_of_bounds { parent; child_index; child_count })
      else Ok (List.nth children child_index)

let total_node_count tree = Node_id.Map.cardinal tree.nodes

let parent tree child =
  match Node_id.Map.find_opt child tree.parents with
  | None -> None
  | Some parent_opt -> parent_opt

let children tree parent =
  match Node_id.Map.find_opt parent tree.children with
  | None -> Error (Invalid_parent_node parent)
  | Some children -> Ok children

(* Style operations *)
let set_style tree node style =
  match Node_id.Map.find_opt node tree.nodes with
  | None -> Error (Invalid_input_node node)
  | Some node_data ->
      tree.nodes <- Node_id.Map.add node { node_data with style } tree.nodes;
      mark_dirty_recursive tree node;
      Ok ()

let style tree node =
  match Node_id.Map.find_opt node tree.nodes with
  | None -> Error (Invalid_input_node node)
  | Some node_data -> Ok node_data.style

(* Layout operations *)
let layout tree node =
  match Node_id.Map.find_opt node tree.nodes with
  | None -> Error (Invalid_input_node node)
  | Some node_data ->
      if tree.config.use_rounding then Ok node_data.final_layout
      else Ok node_data.unrounded_layout

let unrounded_layout tree node =
  match Node_id.Map.find_opt node tree.nodes with
  | None -> Layout.default
  | Some node_data -> node_data.unrounded_layout

let mark_dirty tree node =
  match Node_id.Map.find_opt node tree.nodes with
  | None -> Error (Invalid_input_node node)
  | Some _ ->
      mark_dirty_recursive tree node;
      Ok ()

let dirty tree node =
  match Node_id.Map.find_opt node tree.nodes with
  | None -> Error (Invalid_input_node node)
  | Some node_data -> Ok (Cache.is_empty node_data.cache)

type 'context measure_function =
  float option size ->
  Available_space.t size ->
  Node_id.t ->
  'context option ->
  Style.t ->
  float size
(* Type of measure functions *)

(* View - a view over the tree for layout computation *)
module View = struct
  type 'context t = {
    taffy : 'context tree;
    measure_function : 'context measure_function;
  }

  (* Traverse_partial_tree implementation *)
  let child_ids tree parent_node_id =
    match Node_id.Map.find_opt parent_node_id tree.taffy.children with
    | None -> Seq.empty
    | Some children -> List.to_seq children

  let child_count tree parent_node_id =
    match Node_id.Map.find_opt parent_node_id tree.taffy.children with
    | None -> 0
    | Some children -> List.length children

  let get_child_id tree parent_node_id index =
    match Node_id.Map.find_opt parent_node_id tree.taffy.children with
    | None -> failwith "Invalid parent node"
    | Some children -> List.nth children index

  (* LayoutPartialTree implementation *)
  let get_core_container_style tree node_id =
    match Node_id.Map.find_opt node_id tree.taffy.nodes with
    | None -> Style.default
    | Some node_data -> node_data.style

  let set_unrounded_layout tree node_id layout =
    match Node_id.Map.find_opt node_id tree.taffy.nodes with
    | None -> ()
    | Some node_data ->
        tree.taffy.nodes <-
          Node_id.Map.add node_id
            { node_data with unrounded_layout = layout }
            tree.taffy.nodes

  let resolve_calc_value _tree _val _basis = 0.0

  (* Cache operations *)
  let cache_get tree node_id ~known_dimensions ~available_space ~run_mode =
    match Node_id.Map.find_opt node_id tree.taffy.nodes with
    | None -> None
    | Some node_data ->
        Cache.get node_data.cache ~known_dimensions ~available_space ~run_mode

  let cache_store tree node_id ~known_dimensions ~available_space ~run_mode
      layout_output =
    match Node_id.Map.find_opt node_id tree.taffy.nodes with
    | None -> ()
    | Some node_data ->
        Cache.store node_data.cache ~known_dimensions ~available_space ~run_mode
          layout_output

  let cache_clear tree node_id =
    match Node_id.Map.find_opt node_id tree.taffy.nodes with
    | None -> ()
    | Some node_data -> ignore (Cache.clear node_data.cache)

  let rec compute_child_layout : type ctx.
      ctx t -> Node_id.t -> Layout_input.t -> Layout_output.t =
   fun tree node inputs ->
    (* Check if we should use hidden layout *)
    if Layout_input.run_mode inputs = Run_mode.Perform_hidden_layout then
      let module M = struct
        type nonrec t = ctx t

        let child_ids = child_ids
        let child_count = child_count
        let get_child_id = get_child_id
        let get_core_container_style = get_core_container_style
        let set_unrounded_layout = set_unrounded_layout
        let compute_child_layout = compute_child_layout
        let resolve_calc_value = resolve_calc_value
        let cache_get = cache_get
        let cache_store = cache_store
        let cache_clear = cache_clear
      end in
      compute_hidden_layout
        (module M : CACHE_LAYOUT_PARTIAL_TREE with type t = ctx t)
        tree node
    else
      (* Use cached layout computation *)
      let module CacheM = struct
        type nonrec t = ctx t

        let cache_get = cache_get
        let cache_store = cache_store
        let cache_clear = cache_clear
      end in
      compute_cached_layout
        (module CacheM : CACHE_TREE with type t = ctx t)
        tree node inputs
        (fun tree node _inputs ->
          match Node_id.Map.find_opt node tree.taffy.nodes with
          | None -> Layout_output.hidden
          | Some node_data -> (
              let display_mode = Style.display node_data.style in
              let has_children = child_count tree node > 0 in

              match (display_mode, has_children) with
              | None, _ ->
                  compute_hidden_layout
                    (module struct
                      type nonrec t = ctx t

                      let child_ids = child_ids
                      let child_count = child_count
                      let get_child_id = get_child_id
                      let get_core_container_style = get_core_container_style
                      let set_unrounded_layout = set_unrounded_layout
                      let compute_child_layout = compute_child_layout
                      let resolve_calc_value = resolve_calc_value
                      let cache_get = cache_get
                      let cache_store = cache_store
                      let cache_clear = cache_clear
                    end : CACHE_LAYOUT_PARTIAL_TREE
                      with type t = ctx t)
                    tree node
              | Block, true ->
                  compute_block_layout
                    (module struct
                      type nonrec t = ctx t

                      let child_ids = child_ids
                      let child_count = child_count
                      let get_child_id = get_child_id
                      let get_core_container_style = get_core_container_style
                      let set_unrounded_layout = set_unrounded_layout
                      let compute_child_layout = compute_child_layout
                      let resolve_calc_value = resolve_calc_value
                    end : LAYOUT_PARTIAL_TREE
                      with type t = ctx t)
                    tree node inputs
              | Flex, true ->
                  compute_flexbox_layout
                    (module struct
                      type nonrec t = ctx t

                      let child_ids = child_ids
                      let child_count = child_count
                      let get_child_id = get_child_id
                      let get_core_container_style = get_core_container_style
                      let set_unrounded_layout = set_unrounded_layout
                      let compute_child_layout = compute_child_layout
                      let resolve_calc_value = resolve_calc_value
                    end : LAYOUT_PARTIAL_TREE
                      with type t = ctx t)
                    tree node inputs
              | Grid, true ->
                  compute_grid_layout
                    (module struct
                      type nonrec t = ctx t

                      let child_ids = child_ids
                      let child_count = child_count
                      let get_child_id = get_child_id
                      let get_core_container_style = get_core_container_style
                      let set_unrounded_layout = set_unrounded_layout
                      let compute_child_layout = compute_child_layout
                      let resolve_calc_value = resolve_calc_value
                    end : LAYOUT_PARTIAL_TREE
                      with type t = ctx t)
                    ~tree ~node ~inputs
              | _, false ->
                  (* Leaf node *)
                  let style = node_data.style in
                  let node_context =
                    if node_data.has_context then
                      Node_id.Map.find_opt node tree.taffy.node_context_data
                    else None
                  in
                  let measure_function known_dimensions available_space =
                    tree.measure_function known_dimensions available_space node
                      node_context style
                  in
                  compute_leaf_layout ~inputs ~style
                    ~resolve_calc_value:(resolve_calc_value tree)
                    ~measure_function))

  (* RoundTree implementation *)
  let get_unrounded_layout tree node =
    match Node_id.Map.find_opt node tree.taffy.nodes with
    | None -> Layout.default
    | Some node_data -> node_data.unrounded_layout

  let set_final_layout tree node_id layout =
    match Node_id.Map.find_opt node_id tree.taffy.nodes with
    | None -> ()
    | Some node_data ->
        tree.taffy.nodes <-
          Node_id.Map.add node_id
            { node_data with final_layout = layout }
            tree.taffy.nodes
end

(* Main layout computation *)
let compute_layout_with_measure (type ctx) (tree : ctx tree) node_id
    available_space measure_function =
  let view = { View.taffy = tree; measure_function } in

  compute_root_layout
    (module struct
      type t = ctx View.t

      let child_ids = View.child_ids
      let child_count = View.child_count
      let get_child_id = View.get_child_id
      let get_core_container_style = View.get_core_container_style
      let set_unrounded_layout = View.set_unrounded_layout
      let compute_child_layout = View.compute_child_layout
      let resolve_calc_value = View.resolve_calc_value
    end)
    view node_id available_space;

  if tree.config.use_rounding then
    round_layout
      (module struct
        type t = ctx View.t

        let child_ids = View.child_ids
        let child_count = View.child_count
        let get_child_id = View.get_child_id
        let get_unrounded_layout = View.get_unrounded_layout
        let set_final_layout = View.set_final_layout
      end)
      view node_id;

  Ok ()

let compute_layout tree node available_space =
  compute_layout_with_measure tree node available_space (fun _ _ _ _ _ ->
      Size.zero)

(* Modules for tree traversal - expose for external use *)
module Traverse_partial_tree = struct
  type 'context t = 'context tree

  let child_ids tree parent_node_id =
    match Node_id.Map.find_opt parent_node_id tree.children with
    | None -> Seq.empty
    | Some children -> List.to_seq children

  let child_count tree parent_node_id =
    match Node_id.Map.find_opt parent_node_id tree.children with
    | None -> 0
    | Some children -> List.length children

  let get_child_id tree parent_node_id index =
    match Node_id.Map.find_opt parent_node_id tree.children with
    | None -> failwith "Invalid parent node"
    | Some children -> List.nth children index
end

module Cache_tree = struct
  type 'context t = 'context tree

  let cache_get tree node_id ~known_dimensions ~available_space ~run_mode =
    match Node_id.Map.find_opt node_id tree.nodes with
    | None -> None
    | Some node_data ->
        Cache.get node_data.cache ~known_dimensions ~available_space ~run_mode

  let cache_store tree node_id ~known_dimensions ~available_space ~run_mode
      layout_output =
    match Node_id.Map.find_opt node_id tree.nodes with
    | None -> ()
    | Some node_data ->
        Cache.store node_data.cache ~known_dimensions ~available_space ~run_mode
          layout_output

  let cache_clear tree node_id =
    match Node_id.Map.find_opt node_id tree.nodes with
    | None -> ()
    | Some node_data -> ignore (Cache.clear node_data.cache)
end

module Print_tree = struct
  type 'context t = 'context tree

  let child_ids = Traverse_partial_tree.child_ids
  let child_count = Traverse_partial_tree.child_count
  let get_child_id = Traverse_partial_tree.get_child_id

  let get_debug_label tree node_id =
    match Node_id.Map.find_opt node_id tree.nodes with
    | None -> "UNKNOWN"
    | Some node_data -> (
        let display = Style.display node_data.style in
        let num_children = Traverse_partial_tree.child_count tree node_id in
        match (display, num_children) with
        | None, _ -> "NONE"
        | _, 0 -> "LEAF"
        | Block, _ -> "BLOCK"
        | Flex, _ -> (
            match Style.flex_direction node_data.style with
            | Row | Row_reverse -> "FLEX ROW"
            | Column | Column_reverse -> "FLEX COL")
        | Grid, _ -> "GRID")

  let get_final_layout tree node_id =
    match Node_id.Map.find_opt node_id tree.nodes with
    | None -> Layout.default
    | Some node_data ->
        if tree.config.use_rounding then node_data.final_layout
        else node_data.unrounded_layout
end

(* Print tree for debugging *)
let print_tree (type ctx) (tree : ctx tree) root_id =
  Tree.print_tree
    (module struct
      type t = ctx tree

      let child_ids = Print_tree.child_ids
      let child_count = Print_tree.child_count
      let get_child_id = Print_tree.get_child_id
      let get_debug_label = Print_tree.get_debug_label
      let get_final_layout = Print_tree.get_final_layout
    end : Tree.PRINT_TREE
      with type t = ctx tree)
    tree root_id
