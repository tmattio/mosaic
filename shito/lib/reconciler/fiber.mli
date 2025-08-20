module Make (Host : Host_config.S) : sig
  (** Node kind *)
  type tag =
    | HostNode of Host.primitive
    | TextNode
    | FragmentNode
    | SuspenseNode

  (** Bitset flags for commit-time behavior *)
  module Flag : sig
    type t

    val none : t

    val placement : t
    (** needs insert/move *)

    val update : t
    (** props/text changed *)

    val child_deletion : t
    (** has children to delete *)

    val layout : t
    (** has layout effects *)

    val passive : t
    (** has passive effects *)

    val ref_ : t
    (** ref changed *)

    val ( + ) : t -> t -> t
    (** union *)

    val mem : t -> t -> bool
    (** [mem flag set] *)

    val pp : Format.formatter -> t -> unit
  end

  type effect_ = {
    mutable create : unit -> (unit -> unit) option;
    mutable destroy : (unit -> unit) option;
  }
  (** Effect payload is intentionally generic: [create] returns an optional
      cleanup to run on unmount/dep change. *)

  type t = {
    mutable tag : tag;
    mutable key : Key.t;
    mutable element_type : Host.primitive option;  (** HostNode only *)
    mutable pending_props : Host.props option;  (** next props *)
    mutable memo_props : Host.props option;  (** last committed *)
    mutable state_node : (Host.instance, Host.text_instance) Either.t option;
    mutable parent : t option;
    mutable child : t option;
    mutable sibling : t option;
    mutable alternate : t option;  (** current/WIP pair *)
    mutable flags : Flag.t;
    mutable subtree_flags : Flag.t;
    mutable layout_effects : effect_ list;
    mutable passive_effects : effect_ list;
  }
  (** The fiber record *)

  (** Constructors *)

  val make_host :
    key:Key.t -> element_type:Host.primitive -> props:Host.props -> t

  val make_text : key:Key.t -> t
  val make_fragment : key:Key.t -> t
  val make_suspense : key:Key.t -> t

  (** Linking helpers *)

  val set_child : t -> t option -> unit
  val set_sibling : t -> t option -> unit
  val set_parent : t -> t option -> unit
  val set_alternate_pair : current:t -> wip:t -> unit

  (** Flags *)

  val reset_flags : t -> unit
  val add_flag : t -> Flag.t -> unit
  val bubble_subtree_flags : t -> unit

  (** Host instance helpers *)

  val set_state_instance :
    t -> (Host.instance, Host.text_instance) Either.t -> unit

  val get_state_instance :
    t -> (Host.instance, Host.text_instance) Either.t option

  (** Effect helpers *)

  val push_layout_effect : t -> effect_ -> unit
  val push_passive_effect : t -> effect_ -> unit
end
