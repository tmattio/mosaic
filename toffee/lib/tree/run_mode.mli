(** Layout computation mode.

    Run mode determines how layout algorithms execute: computing full layouts
    for all nodes, computing only container sizes, or handling hidden nodes.
    This affects which layout steps are performed and whether child layouts are
    computed.

    {1 Performance}

    [Compute_size] enables early exits in layout algorithms when only the
    container's dimensions are needed. Layout implementations check run mode and
    return immediately once container size is determined, skipping child
    positioning and final layout steps. This optimization is critical for
    intrinsic size queries (e.g., computing min-content or max-content widths)
    where only dimensions matter, not child positions. *)

type t =
  | Perform_layout
      (** Compute full layout for this node and all children.

          All layout steps are performed, including child layout computation,
          positioning, and size determination. This is the standard mode for
          visible nodes requiring complete layout information. *)
  | Compute_size
      (** Compute only the container size for this node.

          Layout steps unnecessary for determining the node's container size may
          be skipped. This mode optimizes layout computation when only the
          node's dimensions are needed, not the full layout tree. *)
  | Perform_hidden_layout
      (** Set null layout for this hidden node.

          Applied to nodes with [Display.None]. The node receives a zero-sized
          layout and its children are not computed. *)

val to_string : t -> string
(** [to_string mode] returns the string representation of [mode].

    Returns ["PerformLayout"], ["ComputeSize"], or ["PerformHiddenLayout"]. *)

val compare : t -> t -> int
(** [compare a b] compares run modes [a] and [b].

    Ordering: [Perform_layout < Compute_size < Perform_hidden_layout]. *)

val equal : t -> t -> bool
(** [equal a b] tests whether [a] and [b] are the same run mode. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt mode] prints [mode] to formatter [fmt] using {!to_string}. *)
