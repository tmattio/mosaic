(** Composable component system for Mosaic applications.

    This module provides tools for building reusable UI components that can be
    easily composed into larger applications following The Elm Architecture. *)

open Engine

(** The [S] module type defines the public interface for a composable component.
    Any module that implements this interface can be connected to a parent. *)
module type S = sig
  type model
  (** The internal state of the component. *)

  type msg
  (** The internal messages that the component's [update] and [view] functions
      use. *)

  type outgoing
  (** Messages the component can send to its parent for special handling. If a
      component has no outgoing messages, use an empty variant:
      [type outgoing = |] *)

  val update : msg -> model -> model * msg Cmd.t * outgoing option
  (** The [update] function processes an internal message. It can return an
      [outgoing] message for the parent to handle. *)

  val view : model -> Ui.element
  (** The [view] function renders the component's UI based on its model. *)

  val subscriptions : model -> msg Sub.t
  (** The [subscriptions] function allows the component to listen to global
      events like key presses or window resizes. *)
end

type ('child_msg, 'child_outgoing, 'parent_model, 'parent_msg) connected = {
  update :
    'child_msg ->
    'parent_model ->
    'parent_model * 'parent_msg Cmd.t * 'child_outgoing option;
  view : 'parent_model -> Ui.element;
  subscriptions : 'parent_model -> 'parent_msg Sub.t;
}
(** A [connected] value is the result of using [Component.connect]. It is a
    record of functions that are pre-configured to work within a parent
    component, handling all necessary model and message mapping automatically.
*)

val connect :
  (module S
     with type model = 'child_model
      and type msg = 'child_msg
      and type outgoing = 'child_outgoing) ->
  get:('parent_model -> 'child_model) ->
  set:('child_model -> 'parent_model -> 'parent_model) ->
  wrap:('child_msg -> 'parent_msg) ->
  ('child_msg, 'child_outgoing, 'parent_model, 'parent_msg) connected
(** [connect (module Child) ~get ~set ~wrap] is the primary tool for component
    composition.

    It takes a first-class module implementing a child component's logic and a
    "recipe" for how to integrate it into a parent. It returns a [connected]
    record of ready-to-use functions.

    - [get] extracts the child's model from the parent's model
    - [set] updates the parent's model with a new child model
    - [wrap] converts child messages to parent messages

    The resulting functions handle all the boilerplate of mapping between child
    and parent types, allowing clean composition without manual plumbing. *)
