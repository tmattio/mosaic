(* Incremental bridge that drives the core reconciler *)

module Make (Host : Reconciler.Host_config.S) (Incr : Incremental.S) : sig
  type element = (Host.primitive, Host.props) Reconciler.Element.t

  val render : Host.container -> (unit -> element Incr.t) -> unit
  (** Observe a reactive root component and keep the host in sync.
      - On first stabilization: mount.
      - On changes: update.
      - On invalidation: unmount. *)

  val create_root : Host.container -> (unit -> element Incr.t) -> unit
  (** Convenience: freeze a container and return a function that can render a
      component into it. *)
end
