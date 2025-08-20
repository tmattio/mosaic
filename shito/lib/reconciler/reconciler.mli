(* Generic reconciler (mutation host) *)

module Host_config = Host_config
module Element = Element
module Either = Either

module Make (Host : Host_config.S) : sig
  type element = (Host.primitive, Host.props) Element.t
  (** Renderable element for this host *)

  type root
  (** An opaque handle to a mounted tree rooted in a specific container. *)

  val mount : container:Host.container -> element -> root
  (** Mount a fresh element tree into [container]. Returns a handle to
      update/unmount later. *)

  val update : root -> element -> unit
  (** Update the mounted tree to match the new element. Mutates the host
      accordingly. *)

  val unmount : root -> unit
  (** Unmount the current tree and clear the container’s contents. Safe to call
      multiple times. *)

  val container : root -> Host.container
  (** Access the container associated with this root. *)
end
