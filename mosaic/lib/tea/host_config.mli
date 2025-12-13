(** Host configuration for the reconciler.

    This module provides the low-level operations for creating, updating, and
    manipulating widget instances. It mirrors React's HostConfig interface used
    in custom renderers.

    This module is internal to the reconciler and should not be used directly by
    application code. *)

(** {1 Types} *)

type instance =
  | Box_instance of Mosaic_ui.Box.t
  | Text_instance of Mosaic_ui.Text.t
  | Canvas_instance of Mosaic_ui.Canvas.t
  | Table_instance of Mosaic_ui.Table.t
  | Slider_instance of Mosaic_ui.Slider.t
  | Select_instance of Mosaic_ui.Select.t
  | Spinner_instance of Mosaic_ui.Spinner.t
  | Tab_select_instance of Mosaic_ui.Tab_select.t
  | Scroll_bar_instance of Mosaic_ui.Scroll_bar.t
  | Scroll_box_instance of Mosaic_ui.Scroll_box.t
  | Text_input_instance of Mosaic_ui.Text_input.t
  | Code_instance of Mosaic_ui.Code.t
  | Markdown_instance of Mosaic_markdown.t  (** A mounted widget instance. *)

type container = Mosaic_ui.Renderable.t
(** A container that holds child instances. *)

val node_of : instance -> Mosaic_ui.Renderable.t
(** [node_of instance] returns the underlying Renderable node. *)

(** {1 Instance Creation} *)

val create_instance :
  Mosaic_ui.Renderer.t -> Vnode.tag -> unit Vnode.props -> instance
(** [create_instance renderer tag props] creates a new instance for the given
    tag and props. *)

(** {1 Property Updates} *)

val update_props :
  instance -> old_props:unit Vnode.props -> new_props:unit Vnode.props -> bool
(** [update_props instance ~old_props ~new_props] updates the instance
    properties by diffing old and new props. Returns [true] if any property was
    actually changed, [false] otherwise. Does not update handlers - those are
    managed via refs in the Fiber. *)

(** {1 Tree Mutations} *)

val append_child : parent:Mosaic_ui.Renderable.t -> child:instance -> unit
(** [append_child ~parent ~child] appends child to the end of parent's children.
*)

val insert_at :
  parent:Mosaic_ui.Renderable.t -> child:instance -> index:int -> unit
(** [insert_at ~parent ~child ~index] inserts child at the given index. *)

val remove_child : Mosaic_ui.Renderable.t -> unit
(** [remove_child node] removes and destroys the node from its parent. *)

val detach_if_attached : Mosaic_ui.Renderable.t -> unit
(** [detach_if_attached node] detaches the node from its parent only if it has
    one. Safe to call on nodes that aren't in the tree. *)

val reconcile_parent : Mosaic_ui.Renderable.t -> Mosaic_ui.Renderable.t
(** [reconcile_parent node] returns the logical parent to use when reconciling
    its children (defaults to [node]). *)

(** {1 Commit Phase} *)

val commit_update : instance -> unit
(** [commit_update instance] marks the instance as needing a re-render. *)

val reset_after_commit : container -> unit
(** [reset_after_commit container] triggers a render after reconciliation. *)
