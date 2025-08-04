(** An OCaml port of Taffy's tree module. This library provides the core data
    structures and interfaces for layout computation. It defines the inputs and
    outputs for layout algorithms and the module types for creating custom
    layout trees. *)

(** {1 Core Types} *)

(** A type representing the id of a single node in a tree of nodes. It is
    abstract to ensure type safety. *)
module Node_id : sig
  type t
  (** The abstract type for a node ID. *)

  val make : int -> t
  (** Create a new NodeId from an integer value. *)

  val to_int : t -> int
  (** Convert a NodeId to its underlying integer representation. *)

  val equal : t -> t -> bool
  (** Equality test for node IDs. *)

  val compare : t -> t -> int
  (** Comparison function for node IDs, suitable for use in maps and sets. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer for node IDs. *)

  module Map : Map.S with type key = t
  (** A map with node IDs as keys. *)

  module Set : Set.S with type elt = t
  (** A set of node IDs. *)
end

(** Whether we are performing a full layout, or we merely need to size the node.
*)
module Run_mode : sig
  type t =
    | Perform_layout
        (** A full layout for this node and all children should be computed. *)
    | Compute_size
        (** The layout algorithm should be run only to determine the node's
            container size. Steps not necessary for this can be skipped. *)
    | Perform_hidden_layout
        (** The node should have a null layout as it is hidden (e.g., `display:
            none`). *)
end

(** Whether styles should be taken into account when computing size. *)
module Sizing_mode : sig
  type t =
    | Content_size  (** Only content contributions should be considered. *)
    | Inherent_size
        (** Inherent size styles (like `width`, `height`) should be considered
            in addition to content. *)
end

(** A set of margins for margin collapsing in block layout. *)
module Collapsible_margin_set : sig
  type t

  val zero : t
  (** A default margin set with no collapsible margins. *)

  val from_margin : float -> t
  (** Create a set from a single margin value. *)

  val collapse_with_margin : t -> float -> t
  (** Collapse a single margin with this set. *)

  val collapse_with_set : t -> t -> t
  (** Collapse another margin set with this set. *)

  val resolve : t -> float
  (** Resolve the resultant margin from this set. *)
end

(** An axis that layout algorithms can be requested to compute a size for. *)
module Requested_axis : sig
  type t = Horizontal | Vertical | Both

  val of_absolute_axis : Geometry.absolute_axis -> t
  val to_absolute_axis : t -> Geometry.absolute_axis option
end

(** The amount of space available to a node in a given axis *)
module Available_space : sig
  type t =
    | Definite of float  (** A specific number of pixels available *)
    | Min_content  (** Use minimum content size *)
    | Max_content  (** Use maximum content size *)

  val zero : t
  val min_content : t
  val max_content : t
  val of_float : float -> t
  val of_option : float option -> t
  val is_definite : t -> bool
  val to_option : t -> float option
  val unwrap_or : t -> float -> float
  val unwrap : t -> float
  val map_definite_value : t -> (float -> float) -> t
  val compute_free_space : t -> float -> float
  val is_roughly_equal : t -> t -> bool
  val to_string : t -> string

  val equal : t -> t -> bool
  (** Equality function *)

  val compare : t -> t -> int
  (** Comparison function for use in sets/maps *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer *)

  (** {2 Operations with concrete values} *)

  val min : t -> float -> t
  (** Returns the minimum. MinContent/MaxContent become Definite(rhs). *)

  val max : t -> float -> t
  (** Returns the maximum. MinContent/MaxContent are preserved. *)

  val clamp : t -> float -> float -> t
  (** Clamps between min and max. MinContent/MaxContent are preserved. *)

  val add : t -> float -> t
  (** Adds rhs. MinContent/MaxContent are preserved. *)

  val sub : t -> float -> t
  (** Subtracts rhs. MinContent/MaxContent are preserved. *)

  (** {2 Operations with optional values} *)

  val min_or_self : t -> float option -> t
  (** Returns minimum if Some, otherwise self. *)

  val max_or_self : t -> float option -> t
  (** Returns maximum if Some, otherwise self. *)

  val clamp_or_self : t -> float option -> float option -> t
  (** Clamps between optional bounds. None means no constraint. *)

  val add_or_zero : t -> float option -> t
  (** Adds if Some, otherwise treats as zero. *)

  val sub_or_zero : t -> float option -> t
  (** Subtracts if Some, otherwise treats as zero. *)

  val set_or_self : t -> float option -> t
  (** Sets to Definite(value) if Some, otherwise preserves self. *)
end

(** Input constraints and hints for laying out a node, passed from its parent.
*)
module Layout_input : sig
  type t
  (** The abstract type for layout input. *)

  val make :
    run_mode:Run_mode.t ->
    sizing_mode:Sizing_mode.t ->
    axis:Requested_axis.t ->
    known_dimensions:float option Geometry.size ->
    parent_size:float option Geometry.size ->
    available_space:Available_space.t Geometry.size ->
    vertical_margins_are_collapsible:bool Geometry.line ->
    t
  (** Create a new layout input. *)

  val run_mode : t -> Run_mode.t
  val sizing_mode : t -> Sizing_mode.t
  val axis : t -> Requested_axis.t
  val known_dimensions : t -> float option Geometry.size
  val parent_size : t -> float option Geometry.size
  val available_space : t -> Available_space.t Geometry.size
  val vertical_margins_are_collapsible : t -> bool Geometry.line

  val hidden : t
  (** A pre-defined layout input for hidden nodes. *)
end

(** The result of laying out a single node, returned up to the parent. *)
module Layout_output : sig
  type t
  (** The abstract type for layout output. *)

  val make :
    size:float Geometry.size ->
    content_size:float Geometry.size ->
    first_baselines:float option Geometry.point ->
    top_margin:Collapsible_margin_set.t ->
    bottom_margin:Collapsible_margin_set.t ->
    margins_can_collapse_through:bool ->
    t
  (** Create a new layout output. *)

  val size : t -> float Geometry.size
  val content_size : t -> float Geometry.size
  val first_baselines : t -> float option Geometry.point
  val top_margin : t -> Collapsible_margin_set.t
  val bottom_margin : t -> Collapsible_margin_set.t
  val margins_can_collapse_through : t -> bool

  val hidden : t
  (** An all-zero layout output for hidden nodes. *)

  val default : t
  (** A blank layout output. *)

  val from_outer_size : float Geometry.size -> t
  (** Construct a layout output from just the container's size. *)

  val from_sizes_and_baselines :
    float Geometry.size ->
    float Geometry.size ->
    float option Geometry.point ->
    t
  (** Construct a layout output from size and baselines. *)
end

(** The final result of a layout algorithm for a single node. *)
module Layout : sig
  type t
  (** The abstract type for layout. *)

  val make :
    order:int ->
    location:float Geometry.point ->
    size:float Geometry.size ->
    content_size:float Geometry.size ->
    scrollbar_size:float Geometry.size ->
    border:float Geometry.rect ->
    padding:float Geometry.rect ->
    margin:float Geometry.rect ->
    t
  (** Create a new layout. *)

  val order : t -> int
  val location : t -> float Geometry.point
  val size : t -> float Geometry.size
  val content_size : t -> float Geometry.size
  val scrollbar_size : t -> float Geometry.size
  val border : t -> float Geometry.rect
  val padding : t -> float Geometry.rect
  val margin : t -> float Geometry.rect

  val default : t
  (** A new zero-layout. *)

  val with_order : int -> t
  (** A new zero-layout with a specified order. *)

  val content_box_width : t -> float
  (** Get the width of the node's content box. *)

  val content_box_height : t -> float
  (** Get the height of the node's content box. *)

  val content_box_size : t -> float Geometry.size
  (** Get the size of the node's content box. *)

  val content_box_x : t -> float
  (** Get the x-offset of the node's content box relative to its parent. *)

  val content_box_y : t -> float
  (** Get the y-offset of the node's content box relative to its parent. *)

  val scroll_width : t -> float
  (** The scroll width of the node. *)

  val scroll_height : t -> float
  (** The scroll height of the node. *)
end

(** A cache for storing the results of layout computations. *)
module Cache : sig
  type t

  val make : unit -> t
  (** Create a new empty cache. *)

  type clear_state =
    | Cleared
    | Already_empty  (** The result of a clear operation. *)

  val get :
    t ->
    known_dimensions:float option Geometry.size ->
    available_space:Available_space.t Geometry.size ->
    run_mode:Run_mode.t ->
    Layout_output.t option
  (** Try to retrieve a cached result. *)

  val store :
    t ->
    known_dimensions:float option Geometry.size ->
    available_space:Available_space.t Geometry.size ->
    run_mode:Run_mode.t ->
    Layout_output.t ->
    unit
  (** Store a computed result in the cache. *)

  val clear : t -> clear_state
  (** Clear all cache entries. *)

  val is_empty : t -> bool
  (** Check if the cache is empty. *)
end

(** {1 Tree Types} *)

(** Taffy's abstraction for downward tree traversal of immediate children. *)
module type TRAVERSE_PARTIAL_TREE = sig
  type t
  (** The type of the tree structure. *)

  val child_ids : t -> Node_id.t -> Node_id.t Seq.t
  val child_count : t -> Node_id.t -> int
  val get_child_id : t -> Node_id.t -> int -> Node_id.t
end

(** A marker signature which implies `TRAVERSE_PARTIAL_TREE` can be used to
    recurse infinitely down the tree. *)
module type TRAVERSE_TREE = sig
  include TRAVERSE_PARTIAL_TREE
end

(** A signature for a tree that can be laid out using Taffy's algorithms. *)
module type LAYOUT_PARTIAL_TREE = sig
  include TRAVERSE_PARTIAL_TREE

  val get_core_container_style : t -> Node_id.t -> Style.t
  val set_unrounded_layout : t -> Node_id.t -> Layout.t -> unit
  val compute_child_layout : t -> Node_id.t -> Layout_input.t -> Layout_output.t

  val resolve_calc_value : t -> int -> float -> float
  (** Resolve CSS calc() expressions. The first parameter is an identifier for
      the calc expression, and the second is the basis value (e.g., parent width
      for percentage calculations). Returns the resolved pixel value. A default
      implementation that returns 0.0 can be used if calc() support is not
      needed. *)
end

(** A signature for a tree that can store and retrieve cached layout results. *)
module type CACHE_TREE = sig
  type t

  val cache_get :
    t ->
    Node_id.t ->
    known_dimensions:float option Geometry.size ->
    available_space:Available_space.t Geometry.size ->
    run_mode:Run_mode.t ->
    Layout_output.t option

  val cache_store :
    t ->
    Node_id.t ->
    known_dimensions:float option Geometry.size ->
    available_space:Available_space.t Geometry.size ->
    run_mode:Run_mode.t ->
    Layout_output.t ->
    unit

  val cache_clear : t -> Node_id.t -> unit
end

(** A signature for a tree whose float-valued layouts can be rounded to integer
    pixels. *)
module type ROUND_TREE = sig
  include TRAVERSE_TREE

  val get_unrounded_layout : t -> Node_id.t -> Layout.t
  val set_final_layout : t -> Node_id.t -> Layout.t -> unit
end

(** A signature for a tree that can be printed for debugging purposes. *)
module type PRINT_TREE = sig
  include TRAVERSE_TREE

  val get_debug_label : t -> Node_id.t -> string
  val get_final_layout : t -> Node_id.t -> Layout.t
end

(** {1 Tree Functions} *)

val print_tree : (module PRINT_TREE with type t = 'a) -> 'a -> Node_id.t -> unit
(** Print a debug representation of the computed layout for a tree of nodes,
    starting with the passed root node. The tree must implement the PRINT_TREE
    signature which provides tree traversal and layout access capabilities. *)
