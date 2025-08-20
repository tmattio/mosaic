(** Element - renderer-agnostic, renderable UI tree (no components).

    This module defines the canonical tree the reconciler operates on: only host
    primitives, text, fragments, suspense, and empties. Authoring-time
    "Component" nodes must be expanded before reaching this API.

    Design choices:
    - Children are stored as arrays for fast indexed access (O(1)). Builders
      accept lists and convert internally.
    - Keys are optional strings, meaningful only among siblings.
    - Utilities provided for traversal, mapping, folds, and key validation. *)

type key = string option

type 'primitive kind =
  [ `Primitive of 'primitive | `Text | `Fragment | `Suspense | `Empty ]
(** The node kind (useful for shallow comparisons). *)

(** Renderable element type. *)
type ('primitive, 'props) t =
  | Primitive of {
      element_type : 'primitive;
      key : key;
      props : 'props;
      children : ('primitive, 'props) t array;
    }
  | Text of string
  | Fragment of { key : key; children : ('primitive, 'props) t array }
  | Suspense of {
      key : key;
      children : ('primitive, 'props) t array;
      fallback : ('primitive, 'props) t option;
    }
  | Empty

(** {1 Constructors} *)

val primitive :
  ?key:key ->
  element_type:'primitive ->
  props:'props ->
  ('primitive, 'props) t list ->
  ('primitive, 'props) t

val text : string -> ('primitive, 'props) t
val fragment : ?key:key -> ('primitive, 'props) t list -> ('primitive, 'props) t

val suspense :
  ?key:key ->
  ?fallback:('primitive, 'props) t ->
  ('primitive, 'props) t list ->
  ('primitive, 'props) t

val empty : ('primitive, 'props) t

(** {1 Observers / accessors} *)

val kind : ('primitive, 'props) t -> 'primitive kind
val key : ('primitive, 'props) t -> key

val children : ('primitive, 'props) t -> ('primitive, 'props) t array
(** Direct children of a node (returns an array). *)

val children_list : ('primitive, 'props) t -> ('primitive, 'props) t list
(** Same as [children] but returns a list for convenience. *)

val is_empty : ('primitive, 'props) t -> bool
(** Whether the node is [Empty]. *)

val has_children : ('primitive, 'props) t -> bool
(** True if a node has one or more children (Text/Empty -> false). *)

(** {1 Structural operations} *)

val map :
  f:(('primitive, 'props) t -> ('primitive, 'props) t) ->
  ('primitive, 'props) t ->
  ('primitive, 'props) t
(** [map ~f node] applies [f] top-down and then recurses into resulting
    children. If [f] replaces the node, recursion uses the replaced node. *)

val fold :
  init:'acc ->
  f:('acc -> ('primitive, 'props) t -> 'acc) ->
  ('primitive, 'props) t ->
  'acc
(** [fold ~init ~f node] traverses the tree (pre-order), folding values. *)

val iter : f:(('primitive, 'props) t -> unit) -> ('primitive, 'props) t -> unit
(** [iter ~f node] traverses the tree (pre-order). *)

val with_children :
  ('primitive, 'props) t ->
  ('primitive, 'props) t list ->
  ('primitive, 'props) t
(** Replace the children of a node (no-op for Text/Empty). *)

(** {1 Comparison helpers} *)

val equal_shallow_kind :
  prim_eq:('primitive -> 'primitive -> bool) ->
  ('primitive, 'props) t ->
  ('primitive, 'props) t ->
  bool
(** Shallow node-kind equality (ignores key/props/children for primitives). For
    primitive nodes, requires an equality function for primitives. *)

(** {1 Key validation (dev tooling)} *)

val duplicate_child_keys : ('primitive, 'props) t -> string list
(** Return duplicate keys among the direct children of a node (ignores [None]).
*)

val duplicate_keys_deep :
  ('primitive, 'props) t -> (int list * string list) list
(** Recursively check the whole tree for sibling duplicate keys. Returns an
    association list of problematic sibling groups with the path (as a list of
    indices from the root) and the duplicate keys present. *)

(** {1 Pretty printing (debug)} *)

val pp :
  (Format.formatter -> 'primitive -> unit) ->
  (Format.formatter -> 'props -> unit) ->
  Format.formatter ->
  ('primitive, 'props) t ->
  unit
