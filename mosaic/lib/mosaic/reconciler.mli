(** React-style reconciler for Mosaic.

    The reconciler manages the mapping between virtual nodes (vnodes) and actual
    Renderable instances. It diffs vnode trees and applies minimal mutations to
    the underlying Renderable tree. *)

type t
(** The reconciler state, tracking the current fiber tree. *)

val create : Mosaic_ui.Renderer.t -> container:Mosaic_ui.Renderable.t -> t
(** [create renderer ~container] creates a reconciler that will render into the
    given container node. *)

val renderer : t -> Mosaic_ui.Renderer.t
(** [renderer t] returns the underlying renderer. *)

val container : t -> Mosaic_ui.Renderable.t
(** [container t] returns the root container node. *)

val render : t -> unit Vnode.t -> unit
(** [render t vnode] reconciles the vnode tree against the current state and
    applies minimal mutations to update the Renderable tree. *)

val unmount : t -> unit
(** [unmount t] removes all rendered content and cleans up fibers. *)
