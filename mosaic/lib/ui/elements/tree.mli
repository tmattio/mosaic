type guide_style =
  | Normal  (** Standard lines: ├── └── │ *)
  | ASCII  (** ASCII-safe: +-- `-- | *)
  | Bold  (** Bold lines: ┣━━ ┗━━ ┃ *)
  | Double  (** Double lines: ╠══ ╚══ ║ *)

type node = {
  label : Element.t;
  expanded : bool;
  children : node list;
  guide_style : Style.t option;
      (** Optional style override for this node's guides *)
}
(** [node] represents a node in a tree structure.
    @field label The element to display for this node
    @field expanded Whether this node's children are shown (for display purposes only)
    @field children List of child nodes
    @field guide_style Optional style override for guide lines of this node *)

val tree :
  ?style:Style.t ->
  ?guide_style:Style.t ->
  ?guides:guide_style ->
  ?hide_root:bool ->
  ?expanded:bool ->
  node ->
  Element.t
(** [tree ?style ?guide_style ?guides ?hide_root ?expanded node] creates a tree
    view display.
    @param style Style to apply to the tree labels
    @param guide_style Style to apply to the tree guide lines (default: gray)
    @param guides Which guide line style to use (default: Normal)
    @param hide_root Whether to hide the root node (default: false)
    @param expanded
      Override expanded state for all nodes (default: use node.expanded)

    Example rendering:
    {[
      ├─ Parent
      │  ├─ Child 1
      │  └─ Child 2
      └─ Parent 2
    ]} *)
