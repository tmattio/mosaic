(* toffee.ml *)

module Geometry = Geometry
module Style = Style
module Node = Node
module Layout = Layout
module Tree_intf = Tree_intf
include Toffee_tree.Tree

type node_id = Node.Node_id.t

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

type toffee_error = Toffee_tree.Error.t =
  | Child_index_out_of_bounds of {
      parent : Node.Node_id.t;
      child_index : int;
      child_count : int;
    }
  | Invalid_parent_node of Node.Node_id.t
  | Invalid_child_node of Node.Node_id.t
  | Invalid_input_node of Node.Node_id.t

type 'a result = ('a, toffee_error) Result.t

(* Measure function type *)
type 'a measure_function =
  known_dimensions:float option Geometry.size ->
  available_space:Style.Available_space.t Geometry.size ->
  node_id ->
  'a option ->
  Style.style ->
  float Geometry.size

(* Low-level module *)
module Low_level = struct
  type layout_input = Layout.Layout_input.t
  type layout_output = Layout.Layout_output.t

  let compute_block_layout = Compute.compute_block_layout
  let compute_flexbox_layout = Compute.compute_flexbox_layout
  let compute_grid_layout = Compute.compute_grid_layout
  let compute_leaf_layout = Compute.compute_leaf_layout
  let compute_root_layout = Compute.compute_root_layout
  let compute_cached_layout = Compute.compute_cached_layout
  let compute_hidden_layout = Compute.compute_hidden_layout
  let round_layout = Compute.round_layout
end
