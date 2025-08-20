(** In-memory host adapter for tests.

    Implements Host_config.S and exposes a small testing surface:
    - create containers,
    - snapshot the current tree,
    - drain a structured mutation log.

    Primitives are plain strings (e.g. "box", "row", "text"). Props are string
    attributes as an association list [(key, value)], last write wins.

    This module is perfect for verifying the reconciler's behavior
    (insert/move/delete/update ordering) without any real UI. *)

module Host : Reconciler.Host_config.S with type primitive = string

(** Extra testing surface *)

type id = int
(** Opaque identifiers for instances/text/containers exposed to tests. *)

(** A serializable snapshot of the mounted host tree. *)
type snapshot =
  | Node of {
      id : id;
      tag : string;
      attrs : (string * string) list;  (** sorted by key *)
      children : snapshot list;
    }
  | Text of { id : id; text : string }

(** Mutation log for assertions. *)
type op =
  | Prepare_for_commit of { container : id }
  | Reset_after_commit of { container : id }
  | Clear_container of { container : id }
  | Create_instance of { id : id; tag : string; attrs : (string * string) list }
  | Create_text of { id : id; text : string }
  | Append_initial_child of { parent : id; child : id }
  | Finalize_initial_children of { id : id }
  | Append_child of { parent : id; child : id }
  | Insert_before of { parent : id; child : id; before : id }
  | Remove_child of { parent : id; child : id }
  | Append_child_to_container of { container : id; child : id }
  | Insert_before_in_container of { container : id; child : id; before : id }
  | Remove_child_from_container of { container : id; child : id }
  | Commit_update of {
      id : id;
      diff : (string * [ `Set of string | `Remove ]) list;
    }
  | Commit_text_update of { id : id; text : string }
  | Commit_mount of { id : id }
  | Detach_deleted of { id : id }

val create_container : ?name:string -> unit -> Host.container
(** Create a fresh container. Optionally name it for easier snapshots. *)

val snapshot_container : Host.container -> snapshot list
(** Snapshot just the children mounted at the container root. *)

val pp_snapshot : Format.formatter -> snapshot list -> unit
(** Pretty-print a container snapshot (for debug). *)

val drain_log : unit -> op list
(** Drain the mutation log since last drain (or since start). *)
