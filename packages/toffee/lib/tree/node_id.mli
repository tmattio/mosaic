(** Unique identifier for nodes in a layout tree.

    Node identifiers use a generational index scheme to prevent use-after-free
    errors. Each identifier contains an index (position in the tree's internal
    storage) and a generation number (incremented when the slot is reused). This
    detects stale references to removed nodes.

    Invariants:
    - Two identifiers are equal only if both index and generation match.
    - Generation numbers monotonically increase for each index slot.
    - A node identifier becomes invalid when its node is removed from the tree.
*)

type t

val make : int -> t
(** [make index] creates a node identifier with generation 0.

    Used by the tree when allocating new nodes. *)

val make_with_generation : int -> int -> t
(** [make_with_generation index generation] creates a node identifier with the
    specified index and generation.

    Used by the tree when reusing freed node slots with incremented generation.
*)

val to_int : t -> int
(** [to_int node_id] returns the index component.

    Discards the generation. Use only for indexing into internal storage after
    validating the full identifier. *)

val index : t -> int
(** [index node_id] returns the index component.

    Alias for {!to_int}. *)

val generation : t -> int
(** [generation node_id] returns the generation component.

    Used by the tree to detect stale references when validating identifiers. *)

val equal : t -> t -> bool
(** [equal lhs rhs] tests if two identifiers reference the same node.

    Returns [true] only if both index and generation match. *)

val compare : t -> t -> int
(** [compare lhs rhs] orders identifiers lexicographically by index, then
    generation.

    Enables use in ordered data structures like [Map] and [Set]. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt node_id] prints the identifier as [NodeId(index,generation)]. *)

module Map : Map.S with type key = t
(** Map keyed by node identifiers. *)

module Set : Set.S with type elt = t
(** Set of node identifiers. *)
