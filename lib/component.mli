(** Composable component system for Mosaic applications.

    This module provides tools for building reusable UI components that can be
    easily composed into larger applications following The Elm Architecture. *)

(** Component signature matching the structure of a Mosaic app. *)
module type T = sig
  type model
  type msg

  val init : unit -> model * msg Cmd.t
  val update : msg -> model -> model * msg Cmd.t
  val view : model -> Ui.element
  val subscriptions : model -> msg Sub.t
end

(** Component instance that has been connected to a parent application. *)
module Instance : sig
  type ('child_msg, 'parent_model, 'parent_msg) t = {
    update : 'child_msg -> 'parent_model -> 'parent_model * 'parent_msg Cmd.t;
    view : 'parent_model -> Ui.element;
    subscriptions : 'parent_model -> 'parent_msg Sub.t;
  }
end

val make :
  (module T with type model = 'child_model and type msg = 'child_msg) ->
  get:('parent_model -> 'child_model) ->
  set:('child_model -> 'parent_model -> 'parent_model) ->
  wrap:('child_msg -> 'parent_msg) ->
  ('child_msg, 'parent_model, 'parent_msg) Instance.t
(** [make (module Child) ~get ~set ~wrap] creates a component instance.

    - [get] extracts the child's model from the parent's model
    - [set] updates the parent's model with a new child model
    - [wrap] converts child messages to parent messages

    This function handles all the boilerplate of mapping between child and
    parent types, allowing clean composition of components without manual
    plumbing. *)
