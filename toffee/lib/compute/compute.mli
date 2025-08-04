val compute_leaf_layout :
  inputs:Tree.Layout_input.t ->
  style:Style.t ->
  resolve_calc_value:Style.calc_resolver ->
  measure_function:
    (float option Geometry.size ->
    Tree.Available_space.t Geometry.size ->
    float Geometry.size) ->
  Tree.Layout_output.t

val compute_block_layout :
  (module Tree.LAYOUT_PARTIAL_TREE with type t = 'a) ->
  'a ->
  Tree.Node_id.t ->
  Tree.Layout_input.t ->
  Tree.Layout_output.t

val compute_flexbox_layout :
  (module Tree.LAYOUT_PARTIAL_TREE with type t = 'a) ->
  'a ->
  Tree.Node_id.t ->
  Tree.Layout_input.t ->
  Tree.Layout_output.t

val compute_grid_layout :
  (module Tree.LAYOUT_PARTIAL_TREE with type t = 'a) ->
  tree:'a ->
  node:Tree.Node_id.t ->
  inputs:Tree.Layout_input.t ->
  Tree.Layout_output.t

val compute_root_layout :
  (module Tree.LAYOUT_PARTIAL_TREE with type t = 't) ->
  't ->
  Tree.Node_id.t ->
  Tree.Available_space.t Geometry.size ->
  unit

val compute_cached_layout :
  (module Tree.CACHE_TREE with type t = 't) ->
  't ->
  Tree.Node_id.t ->
  Tree.Layout_input.t ->
  ('t -> Tree.Node_id.t -> Tree.Layout_input.t -> Tree.Layout_output.t) ->
  Tree.Layout_output.t

val round_layout :
  (module Tree.ROUND_TREE with type t = 't) -> 't -> Tree.Node_id.t -> unit

module type CACHE_LAYOUT_PARTIAL_TREE = sig
  include Tree.LAYOUT_PARTIAL_TREE
  include Tree.CACHE_TREE with type t := t
end

val compute_hidden_layout :
  (module CACHE_LAYOUT_PARTIAL_TREE with type t = 't) ->
  't ->
  Tree.Node_id.t ->
  Tree.Layout_output.t
