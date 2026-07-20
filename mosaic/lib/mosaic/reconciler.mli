(** Reconciler for diffing and patching {!Vnode.t} trees.

    The reconciler bridges the declarative {!Vnode.t} tree produced by a [view]
    function and the imperative {!Renderable.t} tree. On each {!val-render} call
    it first resolves {!Vnode.Viewport_switch} nodes from the explicit current
    viewport width, then flattens the selected tree, matches fibers by key
    (preferred) then by position, reuses or creates renderable nodes, destroys
    unmatched fibers, and commits child placement. {!Vnode.Embed} nodes are
    attached but bypass the reconciler lifecycle.

    Fiber tracking and instance management are internal; this module exposes
    only the container, render, and unmount operations. *)

open Mosaic_ui

(** {1:types Types} *)

type t
(** The type for a mutable reconciler bound to a container node. Tracks the
    current fiber tree between {!val-render} calls. *)

(** {1:constructors Constructors} *)

val create : container:Renderable.t -> t
(** [create ~container] is a reconciler that manages children of [container].
    The reconciler starts with an empty fiber tree; existing children of
    [container] are not adopted. *)

(** {1:accessors Accessors} *)

val container : t -> Renderable.t
(** [container r] is the container node of [r]. *)

(** {1:reconciliation Reconciliation} *)

val render : t -> viewport_width:int -> unit Vnode.t -> unit
(** [render r ~viewport_width vnode] reconciles [vnode] against the previous
    fiber tree, applying minimal mutations to the renderable tree rooted at
    [container r]. [viewport_width] is the current terminal width in cells and
    resolves {!Vnode.Viewport_switch} nodes before their selected branch is
    reconciled or laid out.

    Fibers unmatched in the new tree are destroyed and their renderable nodes
    removed. Ref callbacks ({!Vnode.attrs.ref}) fire for newly created elements.
    Requests a render on [container r] after reconciliation completes.

    Raises [Invalid_argument] if [viewport_width] is not positive. *)

val unmount : t -> unit
(** [unmount r] destroys all fibers and detaches all embedded nodes, leaving
    [container r] empty. Requests a render on [container r] after cleanup. [r]
    may be reused after unmounting. *)
