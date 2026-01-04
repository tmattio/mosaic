(** Fiber tree for tracking reconciliation state.

    Each fiber corresponds to a vnode element and holds the associated widget
    instance. The fiber tree mirrors the structure of the rendered component
    tree.

    Fibers are internal to the reconciler and should not be used directly by
    application code. *)

(** {1 Types} *)

type child =
  | C_fiber of t
  | C_raw of Mosaic_ui.Renderable.t
      (** A child in the fiber tree - either a fiber or a raw renderable node.
          This unified type preserves the original order of elements and raw
          nodes. *)

and t
(** A fiber node in the reconciliation tree. *)

(** {1 Creation} *)

val create :
  tag:Vnode.tag ->
  key:string option ->
  props:unit Vnode.props ->
  instance:Host_config.instance ->
  t
(** [create ~tag ~key ~props ~instance] creates a new fiber. Handlers are
    registered exactly once during creation and dispatch through refs that are
    updated on prop changes. *)

(** {1 Accessors} *)

val instance : t -> Host_config.instance
(** [instance fiber] returns the widget instance for this fiber. *)

val node : t -> Mosaic_ui.Renderable.t
(** [node fiber] returns the underlying Renderable node. *)

val tag : t -> Vnode.tag
(** [tag fiber] returns the vnode tag for this fiber. *)

val key : t -> string option
(** [key fiber] returns the key for this fiber. *)

val props : t -> unit Vnode.props
(** [props fiber] returns the current props for this fiber. *)

val child_list : t -> child list
(** [child_list fiber] returns all children (fibers and raw nodes) in order. *)

val children : t -> t list
(** [children fiber] returns only the fiber children, filtering out raw nodes.
*)

(** {1 Mutations} *)

val set_child_list : t -> child list -> unit
(** [set_child_list fiber children] sets the children of the fiber. Children can
    be either fibers or raw renderable nodes. *)

val update_props : t -> unit Vnode.props -> unit
(** [update_props fiber props] updates the props and handler refs. Does not
    re-register handlers - they dispatch through refs updated here. *)

(** {1 Destruction} *)

val destroy : t -> unit
(** [destroy fiber] removes the fiber and its children from the tree. Uses
    [remove] (not [detach]) to properly destroy nodes. *)
