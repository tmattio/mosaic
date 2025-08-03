(* toffee.mli *)

(** Toffee: A flexible, high-performance library for UI layout.

    This is the main, high-level API for Toffee. It provides a concrete,
    easy-to-use, imperative-style tree implementation for managing your UI
    nodes, styles, and layouts.

    For advanced use cases where you need to integrate Toffee's layout
    algorithms with your own existing tree data structure, see the
    {!module-Toffee.Low_level}.

    @see <https://github.com/DioxusLabs/taffy> for the original Rust library. *)

(** {1:main_types Main Types} *)

type 'a t
(** An opaque handle to the toffee layout tree. The type parameter ['a]
    represents the user-defined context data that can be associated with each
    node. *)

type node_id = Node.Node_id.t
(** A unique identifier for a node within a toffee tree. *)

type layout = Layout.Layout.t = {
  order : int;
  location : float Geometry.point;
  size : float Geometry.size;
  content_size : float Geometry.size;
  scrollbar_size : float Geometry.size;
  border : float Geometry.rect;
  padding : float Geometry.rect;
  margin : float Geometry.rect;
}
(** The computed layout of a single node after running a layout computation. *)

(** Errors that can occur during tree manipulation or layout computation. *)
type toffee_error =
  | Child_index_out_of_bounds of {
      parent : node_id;
      child_index : int;
      child_count : int;
    }
  | Invalid_parent_node of node_id
  | Invalid_child_node of node_id
  | Invalid_input_node of node_id

type 'a result = ('a, toffee_error) Result.t
(** The result of an operation that may fail. *)

type 'a measure_function =
  known_dimensions:float option Geometry.size ->
  available_space:Style.Available_space.t Geometry.size ->
  node_id ->
  'a option ->
  Style.style ->
  float Geometry.size
(** A function provided by the user to measure the intrinsic size of a leaf
    node. This is necessary for nodes whose size is determined by their content,
    such as text or images. *)

(** {1:lifecycle Tree Lifecycle} *)

val create : unit -> 'a t
(** Creates a new, empty toffee tree. *)

val new_leaf : 'a t -> Style.style -> node_id
(** Creates a new leaf node (with no children), applies the given style, and
    adds it to the tree.
    @return The [node_id] of the newly created node. *)

val new_with_children : 'a t -> Style.style -> node_id list -> node_id
(** Creates a new node with the given children, applies the given style, and
    adds it to the tree.
    @return The [node_id] of the newly created node. *)

val remove : 'a t -> node_id -> unit result
(** Removes a node and its entire subtree from the tree. This operation
    invalidates the [node_id] of the removed node and all of its descendants. *)

(** {1:layout Layout Computation} *)

val compute_layout :
  'a t -> node_id -> Style.Available_space.t Geometry.size -> unit result
(** Computes the layout of the tree starting from the given [node_id]. The
    [available_space] parameter defines the containing block for the root of the
    layout computation. *)

val compute_layout_with_measure :
  'a t ->
  node_id ->
  Style.Available_space.t Geometry.size ->
  'a measure_function ->
  unit result
(** Computes layout with a custom [measure_function] for leaf nodes. This is
    useful for integrating with text-layout engines, image libraries, etc. The
    measure function is called for each leaf node that needs to be measured. *)

(** {1:node_inspection Node Inspection & Manipulation} *)

val style : 'a t -> node_id -> Style.style result
(** Returns the [Style.style] of the given node. *)

val layout : 'a t -> node_id -> layout result
(** Returns the computed [layout] of the given node. You must call
    [compute_layout] before this will return a meaningful value. *)

val set_style : 'a t -> node_id -> Style.style -> unit result
(** Sets the [Style.style] of the given node and marks it as "dirty". *)

val mark_dirty : 'a t -> node_id -> unit result
(** Marks a node as "dirty", forcing its layout and the layout of its ancestors
    to be recomputed on the next call to [compute_layout]. *)

val dirty : 'a t -> node_id -> bool result
(** Returns [true] if the node is considered "dirty" (its layout cache is
    empty), [false] otherwise. *)

val set_node_context : 'a t -> node_id -> 'a -> unit result
(** Sets the user-defined context data for a given node. *)

val get_node_context : 'a t -> node_id -> 'a option result
(** Gets the user-defined context data for a given node. *)

val children : 'a t -> node_id -> node_id list result
(** Returns a list of the node's children. *)

val child_count : 'a t -> node_id -> int result
(** Returns the number of children of a node. *)

val parent : 'a t -> node_id -> node_id option result
(** Returns the parent of a node, if it has one. *)

val child_at_index : 'a t -> node_id -> int -> node_id result
(** Returns the child of a node at a specific index. *)

val add_child : 'a t -> node_id -> node_id -> unit result
(** Appends a child node to a parent node's list of children. *)

val insert_child_at_index : 'a t -> node_id -> int -> node_id -> unit result
(** Inserts a child node into a parent node's list of children at a specific
    index. *)

val set_children : 'a t -> node_id -> node_id list -> unit result
(** Sets the children of a node, replacing any existing children. *)

val remove_child : 'a t -> node_id -> node_id -> unit result
(** Removes a specific child from a parent node's list of children. *)

val remove_child_at_index : 'a t -> node_id -> int -> node_id result
(** Removes a child at a specific index from a parent's list of children.
    @return The [node_id] of the removed child. *)

val replace_child_at_index : 'a t -> node_id -> int -> node_id -> node_id result
(** Replaces a child at a specific index in a parent's list of children with a
    new child.
    @return The [node_id] of the replaced child. *)

(** {1:configuration Configuration} *)

val set_rounding_enabled : 'a t -> bool -> unit
(** Enable or disable pixel-rounding of the final layout. Enabled by default.
    Disabling rounding may be useful for tests or for environments where
    sub-pixel layout is desired. *)

(** {1:debugging Debugging} *)

val print_tree : 'a t -> node_id -> unit
(** Prints a debug representation of the layout tree starting from the given
    node. *)

(** {1:modules Sub-modules} *)

module Geometry : module type of Geometry
(** Core geometric primitives. *)

module Style : module type of Style
(** Data types for all CSS properties supported by Toffee. *)

module Node : module type of Node
(** Node identifier types. *)

module Layout : module type of Layout
(** Layout-related types, such as input and output structures. *)

module Tree_intf : module type of Tree_intf
(** Interfaces for integrating Toffee's algorithms with a custom tree structure.
*)

(** For advanced users who wish to use Toffee's layout algorithms with their own
    tree data structure. *)
module Low_level : sig
  type layout_input = Layout.Layout_input.t
  (** The input to a layout computation. *)

  type layout_output = Layout.Layout_output.t
  (** The output of a layout computation. *)

  val compute_block_layout :
    (module Tree_intf.LayoutBlockContainer
       with type t = 'tree
        and type block_container_style = Style.style
        and type block_item_style = Style.style) ->
    'tree ->
    node_id ->
    layout_input ->
    layout_output
  (** Computes the layout of a single node with `display: block`. *)

  val compute_flexbox_layout :
    (module Tree_intf.LayoutFlexboxContainer
       with type t = 'tree
        and type flexbox_container_style = Style.style
        and type flexbox_item_style = Style.style) ->
    'tree ->
    node_id ->
    layout_input ->
    layout_output
  (** Computes the layout of a single node with `display: flex`. *)

  val compute_grid_layout :
    (module Tree_intf.LayoutGridContainer
       with type t = 'tree
        and type grid_container_style = Style.style
        and type grid_item_style = Style.style) ->
    'tree ->
    node_id ->
    layout_input ->
    layout_output
  (** Computes the layout of a single node with `display: grid`. *)

  val compute_leaf_layout :
    inputs:layout_input ->
    style:Style.style ->
    resolve_calc_value:(unit -> float -> float) ->
    measure_function:
      (float option Geometry.size ->
      Style.Available_space.t Geometry.size ->
      float Geometry.size) ->
    layout_output
  (** Computes the layout for a leaf node (e.g., text, image). *)

  val compute_root_layout :
    (module Tree_intf.LayoutPartialTree
       with type t = 'tree
        and type core_container_style = Style.style) ->
    'tree ->
    node_id ->
    Style.Available_space.t Geometry.size ->
    unit
  (** Computes the layout for the root of a tree. *)

  val compute_cached_layout :
    (module Tree_intf.CacheTree with type t = 'tree) ->
    'tree ->
    node_id ->
    layout_input ->
    ('tree -> node_id -> layout_input -> layout_output) ->
    layout_output
  (** Wraps a layout computation with caching. *)

  val compute_hidden_layout :
    (module Tree_intf.LayoutPartialTree
       with type t = 'tree
        and type core_container_style = Style.style) ->
    (module Tree_intf.CacheTree with type t = 'tree) ->
    'tree ->
    node_id ->
    layout_output
  (** Computes a zero-sized layout for a node with `display: none`. *)

  val round_layout :
    (module Tree_intf.RoundTree with type t = 'tree) -> 'tree -> node_id -> unit
  (** Recursively rounds the layout of a node and its descendants to align with
      the pixel grid. *)
end
