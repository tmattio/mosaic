(** Toffee - A high-performance flexbox/grid layout engine for OCaml

    Toffee is an OCaml port of the Taffy layout library, providing a flexible
    and efficient implementation of CSS layout algorithms including flexbox, CSS
    grid, and block layout.

    {1 Overview}

    The main type is {!tree}, which represents a tree of UI nodes. Each node has
    an associated style (from the {!Style} module) and can have children. The
    library computes layouts based on these styles and the available space.

    {1 Example}

    {[
      open Toffee

      (* Create a new tree *)
      let tree = new_tree () in

      (* Create some nodes *)
      let root = new_leaf tree Style.default |> Result.get_ok in
      let child1 = new_leaf tree Style.(default |> with_size ~width:(px 100.) ~height:(px 50.)) |> Result.get_ok in
      let child2 = new_leaf tree Style.(default |> with_flex_grow 1.0) |> Result.get_ok in

      (* Build the tree structure *)
      let _ = add_child tree root child1 in
      let _ = add_child tree root child2 in

      (* Compute layout *)
      let available_space = Size.{ width = Available_space.from 500.; height = Available_space.from 300. } in
      let _ = compute_layout tree root available_space in

      (* Get the computed layout *)
      match layout tree root with
      | Ok layout -> Printf.printf "Root size: %fx%f\n" layout.size.width layout.size.height
      | Error _ -> ()
    ]} *)

module Geometry = Geometry
(** Geometry module - provides geometric types and utilities *)

module Style = Style
(** Style module - provides CSS style types and utilities *)

open Tree
open Geometry

(** {1 Tree Submodules}

    These submodules from Tree are exposed for use with the API. *)

module Node_id = Node_id
(** Node identifier module *)

module Layout = Layout
(** Layout module containing the layout type and operations *)

module Available_space = Available_space
(** Available space module *)

module Layout_input = Layout_input
(** Layout input type *)

module Layout_output = Layout_output
(** Layout output type *)

module Run_mode = Run_mode
(** Run mode type *)

module Cache = Cache
(** Cache module for layout caching *)

(** {1 Error Handling} *)

module Error : sig
  (** The type of errors that can occur when manipulating the tree *)
  type t =
    | Child_index_out_of_bounds of {
        parent : Node_id.t;
        child_index : int;
        child_count : int;
      }  (** Raised when attempting to access a child at an invalid index *)
    | Invalid_parent_node of Node_id.t
        (** Raised when a parent node is not found in the tree *)
    | Invalid_child_node of Node_id.t
        (** Raised when a child node is not found in the tree *)
    | Invalid_input_node of Node_id.t
        (** Raised when an input node is not found in the tree *)

  val to_string : t -> string
  (** Convert an error to a human-readable string *)
end

type nonrec 'a result = ('a, Error.t) result
(** The result type for operations that can fail *)

(** {1 Tree Type and Creation} *)

type 'context tree
(** The main tree type, parameterized by the type of context data that can be
    associated with nodes. Use [unit] if you don't need context data. *)

val new_tree : unit -> 'context tree
(** Create a new empty tree with default configuration *)

val with_capacity : int -> 'context tree
(** Create a new tree with the specified initial capacity. This can be more
    efficient if you know approximately how many nodes you'll need. *)

(** {1 Configuration} *)

type config = { use_rounding : bool }
(** Configuration options for layout computation *)

val default_config : config
(** Default configuration with rounding enabled *)

val enable_rounding : 'context tree -> 'context tree
(** Enable rounding of layout values (default) *)

val disable_rounding : 'context tree -> 'context tree
(** Disable rounding of layout values *)

(** {1 Node Creation} *)

val new_leaf : 'context tree -> Style.t -> Node_id.t result
(** Create a new leaf node (no children) with the given style *)

val new_leaf_with_context :
  'context tree -> Style.t -> 'context -> Node_id.t result
(** Create a new leaf node with associated context data *)

val new_with_children :
  'context tree -> Style.t -> Node_id.t array -> Node_id.t result
(** Create a new node with the given style and children *)

(** {1 Tree Operations} *)

val clear : 'context tree -> unit
(** Remove all nodes from the tree *)

val remove : 'context tree -> Node_id.t -> Node_id.t result
(** Remove a node and all its descendants from the tree *)

val total_node_count : 'context tree -> int
(** Get the total number of nodes in the tree *)

(** {1 Node Context} *)

val set_node_context :
  'context tree -> Node_id.t -> 'context option -> unit result
(** Set or update the context data associated with a node. Pass [None] to remove
    the context. *)

val get_node_context : 'context tree -> Node_id.t -> 'context option
(** Get the context data associated with a node, if any *)

val get_node_context_mut : 'context tree -> Node_id.t -> 'context option
(** Get a mutable reference to the context data. Note: This returns the same
    immutable option type in the functional interface. *)

(** {1 Child Management} *)

val add_child : 'context tree -> Node_id.t -> Node_id.t -> unit result
(** Add a child to the end of a parent's child list *)

val insert_child_at_index :
  'context tree -> Node_id.t -> int -> Node_id.t -> unit result
(** Insert a child at a specific index in the parent's child list *)

val set_children : 'context tree -> Node_id.t -> Node_id.t array -> unit result
(** Replace all children of a parent node *)

val remove_child : 'context tree -> Node_id.t -> Node_id.t -> Node_id.t result
(** Remove a specific child from a parent *)

val remove_child_at_index :
  'context tree -> Node_id.t -> int -> Node_id.t result
(** Remove the child at a specific index *)

val remove_children_range :
  'context tree -> Node_id.t -> int * int -> unit result
(** Remove children in the given range (inclusive) *)

val replace_child_at_index :
  'context tree -> Node_id.t -> int -> Node_id.t -> Node_id.t result
(** Replace the child at a specific index with a new child *)

(** {1 Tree Queries} *)

val child_at_index : 'context tree -> Node_id.t -> int -> Node_id.t result
(** Get the child at a specific index *)

val parent : 'context tree -> Node_id.t -> Node_id.t option
(** Get the parent of a node, if any *)

val children : 'context tree -> Node_id.t -> Node_id.t list result
(** Get all children of a node *)

(** {1 Style Management} *)

val set_style : 'context tree -> Node_id.t -> Style.t -> unit result
(** Set the style of a node *)

val style : 'context tree -> Node_id.t -> Style.t result
(** Get the style of a node *)

(** {1 Layout} *)

val layout : 'context tree -> Node_id.t -> Layout.t result
(** Get the computed layout of a node. Returns the rounded layout if rounding is
    enabled, otherwise the unrounded layout. *)

val unrounded_layout : 'context tree -> Node_id.t -> Layout.t
(** Get the unrounded layout of a node *)

val mark_dirty : 'context tree -> Node_id.t -> unit result
(** Mark a node as needing layout recomputation. This will also mark all
    ancestors as dirty. *)

val dirty : 'context tree -> Node_id.t -> bool result
(** Check if a node needs layout recomputation *)

(** {1 Layout Computation} *)

type 'context measure_function =
  float option size ->
  Available_space.t size ->
  Node_id.t ->
  'context option ->
  Style.t ->
  float size
(** Type of measure functions for leaf nodes. The function receives:
    - known_dimensions: Already computed dimensions (if any)
    - available_space: Space constraints from the parent
    - node_id: The node being measured
    - context: The node's associated context data (if any)
    - style: The node's style

    It should return the measured size of the content. *)

val compute_layout_with_measure :
  'context tree ->
  Node_id.t ->
  Available_space.t size ->
  'context measure_function ->
  unit result
(** Compute the layout of a node and its descendants with a custom measure
    function *)

val compute_layout :
  'context tree -> Node_id.t -> Available_space.t size -> unit result
(** Compute the layout of a node and its descendants. Uses a default measure
    function that returns zero size. *)

(** {1 Tree Traversal Modules}

    These modules provide implementations of tree traversal traits that can be
    used with generic layout algorithms. *)

(** Partial tree traversal implementation *)
module Traverse_partial_tree : sig
  type 'context t = 'context tree

  val child_ids : 'context t -> Node_id.t -> Node_id.t Seq.t
  val child_count : 'context t -> Node_id.t -> int
  val get_child_id : 'context t -> Node_id.t -> int -> Node_id.t
end

(** Cache tree implementation for layout caching *)
module Cache_tree : sig
  type 'context t = 'context tree

  val cache_get :
    'context t ->
    Node_id.t ->
    known_dimensions:float option size ->
    available_space:Available_space.t size ->
    run_mode:Run_mode.t ->
    Layout_output.t option

  val cache_store :
    'context t ->
    Node_id.t ->
    known_dimensions:float option size ->
    available_space:Available_space.t size ->
    run_mode:Run_mode.t ->
    Layout_output.t ->
    unit

  val cache_clear : 'context t -> Node_id.t -> unit
end

(** Debug printing support *)
module Print_tree : sig
  type 'context t = 'context tree

  val child_ids : 'context t -> Node_id.t -> Node_id.t Seq.t
  val child_count : 'context t -> Node_id.t -> int
  val get_child_id : 'context t -> Node_id.t -> int -> Node_id.t
  val get_debug_label : 'context t -> Node_id.t -> string
  val get_final_layout : 'context t -> Node_id.t -> Layout.t
end

(** {1 Debugging} *)

val print_tree : 'context tree -> Node_id.t -> unit
(** Print a debug representation of the tree to stdout. This requires the
    Print_tree module implementation. *)
