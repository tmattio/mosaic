(* Flexbox layout computation algorithm *)

open Tree

(* Main entry point for flexbox layout computation *)
val compute_flexbox_layout :
  (module LAYOUT_PARTIAL_TREE with type t = 't) ->
  't ->
  Node_id.t ->
  Layout_input.t ->
  Layout_output.t
