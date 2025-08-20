(** Keyed children reconciliation (pure, host-agnostic). Computes reuse (match)
    mapping between "old" and "new" arrays of nodes, which new positions require
    a placement (insert or move), and which old positions are deletions.

    The algorithm:
    - Reuse by key when present (string key).
    - For unkeyed nodes, reuse by "kind bucket" in order (via [kind_id]).
    - A reused node requires a move when its old index is < the last placed
      index.
    - Non-reused new nodes are inserts; unmatched old nodes are deletes.

    This is intentionally unaware of props diffs or host instances. It only
    answers structural questions needed by the reconciler. *)

module type NODE = sig
  type t

  val key : t -> Key.t
  (** Optional sibling key. [Some k] must be stable across renders if provided.
  *)

  val same_kind : t -> t -> bool
  (** True if the two nodes are the same "host kind" (e.g., same primitive tag,
      or both Text, or both Fragment, etc.). *)

  val kind_id : t -> int
  (** A stable integer bucket representing the "kind" used for unkeyed matching.
      For example:
      - Text -> 0
      - Fragment -> 1
      - Suspense -> 2
      - Primitive p -> hash of [p]'s tag If two nodes return the same [kind_id],
        [same_kind] should be true. *)
end

module Make (N : NODE) : sig
  type node = N.t

  (** For each new index, either reuses an old index or not. *)
  type reuse = int option array
  (** [reuse.(i) = Some j] means new child at index [i] reuses old child at
      index [j]. [None] means it's a fresh insert. *)

  (** For each new index, whether a placement (insert or move) is required. *)
  type needs_placement = bool array
  (** [needs_placement.(i) = true] means reconciler should place/move this
      child. *)

  type deletes = int list
  (** Old indices that must be deleted (no reuse). *)

  type result = {
    reuse : reuse;
    needs_placement : needs_placement;
    deletes : deletes;
  }

  val diff : old_nodes:node array -> new_nodes:node array -> result
  (** Compute reconciliation result between old and new arrays. *)

  val diff_list : old_nodes:node list -> new_nodes:node list -> result
  (** Convenience wrapper for lists. *)
end
