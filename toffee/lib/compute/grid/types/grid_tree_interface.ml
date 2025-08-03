(** tree_interface.ml
    ---------------------------------------------------------------------------
    Simple interface for tree operations needed by grid layout
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

open Geometry
open Style

(** Simplified tree interface for grid computations *)
class type tree = object
  method calc : Length_percentage.t -> float -> float

  method measure_child_size :
    Node.Node_id.t ->
    known_dimensions:float option size ->
    parent_size:float option size ->
    available_space:Available_space.t size ->
    sizing_mode:Layout.Sizing_mode.t ->
    axis:absolute_axis ->
    vertical_margins_are_collapsible:bool line ->
    float
end
