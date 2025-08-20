(** Host configuration — contract for the reconciler. *)

module type S = sig
  type primitive
  (** The type of primitive element identifiers the host understands (e.g. "div"
      for web, "box"/"text" for a TUI). *)

  type props
  (** Props are completely host-defined. *)

  type instance
  (** Opaque host objects managed by the renderer. *)

  type text_instance
  type container

  type public_instance
  (** What gets exposed to userland refs. Often the same as [instance], but
      hosts may choose a different public view. *)

  type update
  (** Host-specific, precomputed update payload produced during the render phase
      and consumed during commit. Keep this opaque to the reconciler. *)

  val equal_primitive : primitive -> primitive -> bool
  (** Equality for primitives. The reconciler uses this instead of polymorphic
      equality to decide whether two primitive nodes are the “same type”. *)

  (* Instance creation *)

  val create_instance :
    primitive:primitive -> props:props -> container:container -> instance
  (** Create a new host instance (non-text). The [container] is provided for
      hosts that need container context during creation. *)

  val create_text_instance : text:string -> container:container -> text_instance
  (** Create a new text instance. *)

  val append_initial_child :
    parent:instance -> child:(instance, text_instance) Either.t -> unit
  (** During the initial mount of an instance, append its child instances. (You
      may simply delegate to [append_child].) *)

  val finalize_initial_children : instance:instance -> props:props -> unit
  (** Called once after all initial children have been appended. Use this to
      finalize attributes that depend on children (e.g., auto-focus). *)

  (* Commit batching *)

  val prepare_for_commit : container:container -> unit
  (** Called before a batch of host mutations. Typical uses: pause I/O, grab a
      render lock, snapshot terminal buffers, etc. *)

  val reset_after_commit : container:container -> unit
  (** Called after a batch of host mutations. Typical uses: resume I/O, flush a
      frame to the screen, release locks, etc. *)

  (* Updates *)

  val prepare_update :
    instance:instance -> old_props:props -> new_props:props -> update option
  (** Compute a minimal, host-specific diff between [old_props] and [new_props].
      Return [None] if nothing needs to change. This runs in the render phase,
      so it must be pure and side-effect free. *)

  val commit_update : instance:instance -> update:update -> unit
  (** Apply a previously prepared [update] to an existing instance. *)

  val commit_text_update :
    text_instance:text_instance -> new_text:string -> unit
  (** Update text content for an existing text instance. The reconciler will
      call this only when the text actually changed. *)

  val commit_mount : instance:instance -> props:props -> unit
  (** Called right after an instance is inserted into the host tree (mount
      complete). Useful for imperative hooks like focusing. *)

  (* Tree mutations: non-root parents *)

  val append_child :
    parent:instance -> child:(instance, text_instance) Either.t -> unit
  (** Append a child under a non-root parent. *)

  val insert_before :
    parent:instance ->
    child:(instance, text_instance) Either.t ->
    before_child:(instance, text_instance) Either.t ->
    unit
  (** Insert [child] before [before_child] under the same parent. *)

  val remove_child :
    parent:instance -> child:(instance, text_instance) Either.t -> unit
  (** Remove a direct child from a non-root parent. *)

  (* Tree mutations: root container *)

  val append_child_to_container :
    container:container -> child:(instance, text_instance) Either.t -> unit
  (** Append a child directly to the root container. *)

  val insert_before_in_container :
    container:container ->
    child:(instance, text_instance) Either.t ->
    before_child:(instance, text_instance) Either.t ->
    unit
  (** Insert [child] into the root container before [before_child]. *)

  val remove_child_from_container :
    container:container -> child:(instance, text_instance) Either.t -> unit
  (** Remove a direct child from the root container. *)

  val clear_container : container:container -> unit
  (** Efficiently clear all contents of the container (optional but recommended
      for hosts where a bulk clear is cheaper than removing each child). *)

  (* Refs / public exposure *)

  val get_public_instance : instance:instance -> public_instance
  (** Map an internal instance to what userland refs should receive. *)

  val get_public_text_instance : text_instance:text_instance -> public_instance
  (** Public exposure for text nodes, if desired. You can return the same type
      as [get_public_instance] or a specialized text handle. *)

  (* Deletion hooks *)

  val detach_deleted_instance : child:(instance, text_instance) Either.t -> unit
  (** Called when a subtree is being deleted, for per-node cleanup that must
      happen before the host object is detached. The reconciler will already
      have run effect cleanups; use this to release host-side resources. *)
end
