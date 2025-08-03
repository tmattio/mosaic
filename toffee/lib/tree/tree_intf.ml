(** The abstractions that make up the core of Taffy's low-level API

    ## Overview

    ### Trait dependency tree

    The tree below illustrates which traits depend on which other traits.

    {[
      TraversePartialTree     - Access a node's children
      ├──  LayoutPartialTree  - Run layout algorithms on a node and it's direct children
      └──  TraverseTree       - Recursively access a node's descendants
          ├──  RoundTree      - Round a float-valued layout to integer pixels
          └──  PrintTree      - Print a debug representation of a node tree
    ]} *)

open Node
open Layout
open Geometry
open Style

(** Taffy's abstraction for downward tree traversal.

    However, this trait does *not* require access to any node's other than a
    single container node's immediate children unless you also intend to
    implement `TraverseTree`. *)
module type TraversePartialTree = sig
  type t
  (** The tree type *)

  type child_iter
  (** Type representing an iterator of the children of a node *)

  val child_ids : t -> Node_id.t -> child_iter
  (** Get the list of children IDs for the given node *)

  val child_count : t -> Node_id.t -> int
  (** Get the number of children for the given node *)

  val get_child_id : t -> Node_id.t -> int -> Node_id.t
  (** Get a specific child of a node, where the index represents the nth child
  *)
end

(** A marker trait which extends `TraversePartialTree`

    Implementing this trait implies the additional guarantee that the
    child/children methods can be used to recurse infinitely down the tree. Is
    required by the `RoundTree` and the `PrintTree` traits. *)
module type TraverseTree = sig
  include TraversePartialTree
end

(** Any type that implements [`LayoutPartialTree`] can be laid out using Taffy's
    algorithms

    Note that this trait extends [`TraversePartialTree`] (not [`TraverseTree`]).
    Taffy's algorithm implementations have been designed such that they can be
    used for laying out a single node that only has access to it's immediate
    children. *)
module type LayoutPartialTree = sig
  include TraversePartialTree

  type core_container_style
  (** The style type representing the core container styles that all containers
      should have Used when laying out the root node of a tree *)

  val get_core_container_style : t -> Node_id.t -> core_container_style
  (** Get core style *)

  val resolve_calc_value : t -> ptr:unit -> basis:float -> float
  (** Resolve calc value *)

  val set_unrounded_layout : t -> Node_id.t -> Layout.t -> unit
  (** Set the node's unrounded layout *)

  val compute_child_layout : t -> Node_id.t -> Layout_input.t -> Layout_output.t
  (** Compute the specified node's size or full layout given the specified
      constraints *)
end

(** Trait used by the `compute_cached_layout` method which allows cached layout
    results to be stored and retrieved.

    The `Cache` struct implements a per-node cache that is compatible with this
    trait. *)
module type CacheTree = sig
  type t

  val cache_get :
    t ->
    Node_id.t ->
    float option size ->
    Available_space.t size ->
    Run_mode.t ->
    Layout_output.t option
  (** Try to retrieve a cached result from the cache *)

  val cache_store :
    t ->
    Node_id.t ->
    float option size ->
    Available_space.t size ->
    Run_mode.t ->
    Layout_output.t ->
    unit
  (** Store a computed size in the cache *)

  val cache_clear : t -> Node_id.t -> unit
  (** Clear all cache entries for the node *)
end

(** Trait used by the `round_layout` method which takes a tree of unrounded
    float-valued layouts and performs rounding to snap the values to the pixel
    grid.

    As indicated by it's dependence on `TraverseTree`, it required full
    recursive access to the tree. *)
module type RoundTree = sig
  include TraverseTree

  val get_unrounded_layout : t -> Node_id.t -> Layout.t
  (** Get the node's unrounded layout *)

  val set_final_layout : t -> Node_id.t -> Layout.t -> unit
  (** Set the node's final layout *)
end

(** Trait used by the `print_tree` method which prints a debug representation

    As indicated by it's dependence on `TraverseTree`, it required full
    recursive access to the tree. *)
module type PrintTree = sig
  include TraverseTree

  val get_debug_label : t -> Node_id.t -> string
  (** Get a debug label for the node (typically the type of node: flexbox, grid,
      text, image, etc) *)

  val get_final_layout : t -> Node_id.t -> Layout.t
  (** Get a reference to the node's final layout *)
end

(** Extends [`LayoutPartialTree`] with getters for the styles required for
    Flexbox layout *)
module type LayoutFlexboxContainer = sig
  include LayoutPartialTree

  type flexbox_container_style
  (** The style type representing the Flexbox container's styles *)

  type flexbox_item_style
  (** The style type representing each Flexbox item's styles *)

  val get_flexbox_container_style : t -> Node_id.t -> flexbox_container_style
  (** Get the container's styles *)

  val get_flexbox_child_style : t -> Node_id.t -> flexbox_item_style
  (** Get the child's styles *)
end

(** Extends [`LayoutPartialTree`] with getters for the styles required for CSS
    Grid layout *)
module type LayoutGridContainer = sig
  include LayoutPartialTree

  type grid_container_style
  (** The style type representing the CSS Grid container's styles *)

  type grid_item_style
  (** The style type representing each CSS Grid item's styles *)

  val get_grid_container_style : t -> Node_id.t -> grid_container_style
  (** Get the container's styles *)

  val get_grid_child_style : t -> Node_id.t -> grid_item_style
  (** Get the child's styles *)

  (** Set the node's detailed grid information

      Implementing this method is optional. Doing so allows you to access
      details about the the grid such as the computed size of each grid track
      and the computed placement of each grid item. *)
  module DetailedGridInfo : sig
    type t
  end

  val set_detailed_grid_info : t -> Node_id.t -> DetailedGridInfo.t -> unit
end

(** Extends [`LayoutPartialTree`] with getters for the styles required for CSS
    Block layout *)
module type LayoutBlockContainer = sig
  include LayoutPartialTree

  type block_container_style
  (** The style type representing the CSS Block container's styles *)

  type block_item_style
  (** The style type representing each CSS Block item's styles *)

  val get_block_container_style : t -> Node_id.t -> block_container_style
  (** Get the container's styles *)

  val get_block_child_style : t -> Node_id.t -> block_item_style
  (** Get the child's styles *)
end

(** A private module type which allows us to add extra convenience methods to
    types which implement LayoutPartialTree without making those methods public.
*)
module type LayoutPartialTreeExt = sig
  include LayoutPartialTree

  val measure_child_size :
    t ->
    Node_id.t ->
    known_dimensions:float option size ->
    parent_size:float option size ->
    available_space:Available_space.t size ->
    sizing_mode:Sizing_mode.t ->
    axis:absolute_axis ->
    vertical_margins_are_collapsible:bool line ->
    float
  (** Compute the size of the node given the specified constraints *)

  val perform_child_layout :
    t ->
    Node_id.t ->
    known_dimensions:float option size ->
    parent_size:float option size ->
    available_space:Available_space.t size ->
    sizing_mode:Sizing_mode.t ->
    vertical_margins_are_collapsible:bool line ->
    Layout_output.t
  (** Perform a full layout on the node given the specified constraints *)

  val calc : t -> ptr:unit -> basis:float -> float
  (** Alias to `resolve_calc_value` with a shorter function name *)
end

(** Functor to add extension methods to any LayoutPartialTree implementation *)
module LayoutPartialTreeExt (T : LayoutPartialTree) :
  LayoutPartialTreeExt with type t = T.t = struct
  include T

  let measure_child_size t node_id ~known_dimensions ~parent_size
      ~available_space ~sizing_mode ~axis ~vertical_margins_are_collapsible =
    let axis_requested =
      match axis with
      | Horizontal -> Requested_axis.Horizontal
      | Vertical -> Requested_axis.Vertical
    in
    let layout_input =
      {
        Layout_input.run_mode = Run_mode.Compute_size;
        sizing_mode;
        axis = axis_requested;
        known_dimensions;
        parent_size;
        available_space;
        vertical_margins_are_collapsible;
      }
    in
    let output = compute_child_layout t node_id layout_input in
    size_get_abs output.size axis

  let perform_child_layout t node_id ~known_dimensions ~parent_size
      ~available_space ~sizing_mode ~vertical_margins_are_collapsible =
    let layout_input =
      {
        Layout_input.run_mode = Run_mode.Perform_layout;
        sizing_mode;
        axis = Requested_axis.Both;
        known_dimensions;
        parent_size;
        available_space;
        vertical_margins_are_collapsible;
      }
    in
    compute_child_layout t node_id layout_input

  let calc t ~ptr ~basis = resolve_calc_value t ~ptr ~basis
end
