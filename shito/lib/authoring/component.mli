(** Authoring layer: components and hooks, parameterized by an Incremental
    instance. IMPORTANT: instantiate with the *same* Incremental instance used
    by your reconciler, so state changes trigger re-render.

    This layer only produces renderable Element trees (no Component nodes). *)

module Make (Incr : Incremental.S) : sig
  type 'a t = 'a Incr.t

  type ('primitive, 'host_props, 'props) component =
    'props -> ('primitive, 'host_props) Reconciler.Element.t t
  (** A component transforms its props into a renderable Element incremental. *)

  val component :
    ('props -> ('primitive, 'host_props) Reconciler.Element.t t) ->
    ('primitive, 'host_props, 'props) component
  (** Helpers to define components. *)

  module Hooks : sig
    type 'a state = 'a t * ('a -> unit)
    (** State *)

    val use_state : 'a -> 'a state

    val use_reducer :
      ('state -> 'action -> 'state) ->
      init:'state ->
      'state t * ('action -> unit)
    (** Reducer *)

    val use_memo : ('deps -> 'a) -> deps:'deps t -> 'a t
    (** Memoization: recompute only when deps change (physical equality). *)

    val use_callback : ('deps -> 'arg -> 'r) -> deps:'deps t -> ('arg -> 'r) t
    (** Callback: a function stable wrt deps. *)

    type 'a ref_ = { mutable current : 'a }
    (** Ref: stable mutable cell. *)

    val use_ref : 'a -> 'a ref_

    (** Context: simple global-ish value propagated via a Var. *)
    module Context : sig
      type 'a t

      val create : 'a -> 'a t
      val provide : 'a t -> 'a -> unit
      val use_context : 'a t -> 'a Incr.t
    end

    (** Passive/layout effects (minimal version). Runs [create ()] after
        stabilization of [deps]; if it returns [Some cleanup], cleanup runs on
        the next change or invalidation. Returns a [cancel] that stops observing
        and runs any outstanding cleanup.

        NOTE: Without fiber integration, effects aren't tied to unmount
        automatically. If you create short-lived subgraphs, keep the returned
        cancel and invoke it when appropriate (or keep long-lived effects at
        app/root scope). *)
    val use_effect :
      (unit -> (unit -> unit) option) -> deps:'d t -> unit -> unit
    (** cancel *)

    val use_layout_effect :
      (unit -> (unit -> unit) option) -> deps:'d t -> unit -> unit
    (** cancel; same timing as [use_effect] for now *)
  end
end
