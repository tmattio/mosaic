(** Hierarchical tree with expand/collapse navigation.

    A focusable, keyboard-navigable tree widget that displays items in a
    hierarchy with optional guide lines. Each node can be expanded or collapsed
    to show or hide its children. The widget automatically scrolls to keep the
    selected node visible.

    Input gestures propose a complete {!State.t} through {!set_on_change}.
    Rejected proposals leave the tree unchanged. Explicit setters and
    {!apply_props} install accepted state without emitting proposals.

    {1:navigation Navigation}

    Keyboard bindings:
    - Up/Down arrows move the selection. Shift moves by [fast_scroll_step]
      items.
    - [j]/[k] move the selection by one item.
    - Right expands the focused node or moves to its first child.
    - Left collapses the focused node or moves to its parent.
    - Space toggles expand/collapse of the focused node.
    - Enter/KP_enter activates the focused node (fires [on_activate]).

    Mouse bindings:
    - Left click on the icon area toggles expand/collapse.
    - Left click on the label area selects the node.
    - Scroll wheel navigates through the list.

    {1:guides Guide lines}

    When [show_guides] is [true], Unicode box-drawing characters render the tree
    structure. The character set is controlled by [guide_style], which accepts
    any {!Matrix_grid.Border.t} preset (single, rounded, heavy, etc.). When
    [show_guides] is [false] (the default), only indentation is used. *)

type t
(** The type for tree widgets. *)

(** {1:item Item} *)

type item = { id : string; label : string; children : item list }
(** The type for tree nodes. [label] is the display string; [children] is the
    (possibly empty) list of child nodes. *)

val item : id:string -> ?children:item list -> string -> item
(** [item ~id label] is a tree node with [label] and no children.

    [children] defaults to [[]]. *)

module State : sig
  type t
  (** Persistent selection and expansion, using IDs unique within the tree. *)

  val empty : t
  val make : ?selected:string -> ?expanded:string list -> unit -> t
  val selected : t -> string option
  val expanded : t -> string list
  val select : string option -> t -> t
  val set_expanded : string -> bool -> t -> t
  val is_expanded : string -> t -> bool
  val equal : t -> t -> bool
end

type action =
  | Move_up
  | Move_down
  | Move_up_fast
  | Move_down_fast
  | Expand
  | Collapse
  | Toggle
  | Activate  (** Keyboard navigation and activation actions. *)

type key_binding = Shortcut.t * action
(** A shortcut and its action. Custom bindings override defaults; the last
    binding for a shortcut wins. Releases are ignored. *)

(** {1:construction Construction} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?items:item list ->
  ?state:State.t ->
  ?indent_size:int ->
  ?show_guides:bool ->
  ?guide_style:Matrix_grid.Border.t ->
  ?expand_icon:string ->
  ?collapse_icon:string ->
  ?leaf_icon:string ->
  ?background:Ansi.Color.t ->
  ?text_color:Ansi.Color.t ->
  ?selected_background:Ansi.Color.t ->
  ?selected_text_color:Ansi.Color.t ->
  ?focused_selected_background:Ansi.Color.t ->
  ?focused_selected_text_color:Ansi.Color.t ->
  ?guide_color:Ansi.Color.t ->
  ?icon_color:Ansi.Color.t ->
  ?wrap_selection:bool ->
  ?key_bindings:key_binding list ->
  ?fast_scroll_step:int ->
  unit ->
  t
(** [create ~parent ()] is a tree widget attached to [parent].

    The widget is focusable and uses buffered rendering. Tree-specific
    parameters are forwarded to {!Props.make}; refer to {!Props.make} for their
    defaults. *)

val node : t -> Renderable.t
(** [node t] is the underlying {!Renderable.t} for [t]. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for declarative property bundles used for reconciler diffing. *)

  val make :
    ?items:item list ->
    ?state:State.t ->
    ?indent_size:int ->
    ?show_guides:bool ->
    ?guide_style:Matrix_grid.Border.t ->
    ?expand_icon:string ->
    ?collapse_icon:string ->
    ?leaf_icon:string ->
    ?background:Ansi.Color.t ->
    ?text_color:Ansi.Color.t ->
    ?selected_background:Ansi.Color.t ->
    ?selected_text_color:Ansi.Color.t ->
    ?focused_selected_background:Ansi.Color.t ->
    ?focused_selected_text_color:Ansi.Color.t ->
    ?guide_color:Ansi.Color.t ->
    ?icon_color:Ansi.Color.t ->
    ?wrap_selection:bool ->
    ?key_bindings:key_binding list ->
    ?fast_scroll_step:int ->
    unit ->
    t
  (** [make ()] is a property bundle with defaults:
      - [items]: [[]].
      - [state]: accepted selection and expansion; defaults to [State.empty].
      - [indent_size]: columns per depth level; defaults to [2].
      - [show_guides]: draw box-drawing guide lines; defaults to [false].
      - [guide_style]: border character set for guide lines; defaults to
        {!Matrix_grid.Border.single}.
      - [expand_icon]: icon for collapsed expandable nodes; defaults to
        ["\xe2\x96\xb6"] (U+25B6 BLACK RIGHT-POINTING TRIANGLE).
      - [collapse_icon]: icon for expanded nodes; defaults to ["\xe2\x96\xbc"]
        (U+25BC BLACK DOWN-POINTING TRIANGLE).
      - [leaf_icon]: icon for leaf nodes; defaults to [" "].
      - [background]: background color; defaults to transparent.
      - [text_color]: text color; defaults to white.
      - [selected_background]: background color of the selected node; defaults
        to dark blue.
      - [selected_text_color]: text color of the selected node; defaults to
        yellow.
      - [focused_selected_background]: background color of the selected node
        when the widget is focused; when unset, falls back to
        [selected_background].
      - [focused_selected_text_color]: text color of the selected node when the
        widget is focused; when unset, falls back to [selected_text_color].
      - [guide_color]: guide line color; defaults to gray.
      - [icon_color]: expand/collapse icon color; defaults to gray.
      - [wrap_selection]: wrap selection at list boundaries; defaults to
        [false].
      - [fast_scroll_step]: rows to skip with Shift+Up/Down; defaults to [5]. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] applies [props] to [t], triggering the minimum
    necessary layout and render updates. Never fires the change callback;
    callbacks are reserved for user gestures. *)

(** {1:data Data} *)

val items : t -> item list
(** [items t] is the current item list of [t]. *)

val set_items : t -> item list -> unit
(** [set_items t items] replaces the item tree while preserving accepted IDs. *)

(** {1:selection Selection} *)

val selected_index : t -> int
(** [selected_index t] is the 0-based index of the selected entry in the visible
    (flattened) list of [t], or [-1] when its selected ID is not visible. *)

val set_selected_index : t -> int -> unit
(** [set_selected_index t i] selects the visible entry at index [i], clamped to
    the valid range. Does nothing for an empty tree and does not emit a change
    callback. *)

val selected_item : t -> item option
(** [selected_item t] is the currently selected item of [t], or [None] if the
    selected ID is absent or hidden. *)

val visible_count : t -> int
(** [visible_count t] is the number of currently visible entries in [t]. *)

val depth_of : t -> int -> int
(** [depth_of t i] is the nesting depth of visible entry [i] in [t].
    [depth_of t i] is [0] for out-of-bounds indices. *)

(** {1:expansion Expansion} *)

val expand : t -> int -> unit
(** [expand t i] expands the visible entry at index [i]. No-op if the entry is
    already expanded or is a leaf. *)

val collapse : t -> int -> unit
(** [collapse t i] collapses the visible entry at index [i]. No-op if the entry
    is already collapsed or is a leaf. If a descendant is selected, selection
    moves to the collapsed entry. *)

val toggle_expand : t -> int -> unit
(** [toggle_expand t i] toggles the expansion state of visible entry [i] in [t].
*)

val expand_all : t -> unit
(** [expand_all t] expands every expandable node in [t]. *)

val collapse_all : t -> unit
(** [collapse_all t] collapses every expandable node in [t]. *)

val is_expanded : t -> int -> bool
(** [is_expanded t i] is [true] iff visible entry [i] is expanded. *)

(** {1:display Display} *)

val set_indent_size : t -> int -> unit
(** [set_indent_size t n] sets the number of columns per depth level to [n]. *)

val set_show_guides : t -> bool -> unit
(** [set_show_guides t v] shows guide lines when [v] is [true] and hides them
    when [v] is [false]. *)

val set_guide_style : t -> Matrix_grid.Border.t -> unit
(** [set_guide_style t style] sets the guide line character set to [style]. *)

val set_expand_icon : t -> string -> unit
(** [set_expand_icon t s] sets the icon for collapsed expandable nodes to [s].
*)

val set_collapse_icon : t -> string -> unit
(** [set_collapse_icon t s] sets the icon for expanded nodes to [s]. *)

val set_leaf_icon : t -> string -> unit
(** [set_leaf_icon t s] sets the icon for leaf nodes to [s]. *)

(** {1:colors Colors} *)

val set_background : t -> Ansi.Color.t -> unit
(** [set_background t c] sets the background color of [t] to [c]. *)

val set_text_color : t -> Ansi.Color.t -> unit
(** [set_text_color t c] sets the text color of [t] to [c]. *)

val set_selected_background : t -> Ansi.Color.t -> unit
(** [set_selected_background t c] sets the background color of the selected node
    to [c]. *)

val set_selected_text_color : t -> Ansi.Color.t -> unit
(** [set_selected_text_color t c] sets the text color of the selected node to
    [c]. *)

val set_guide_color : t -> Ansi.Color.t -> unit
(** [set_guide_color t c] sets the guide line color of [t] to [c]. *)

val set_icon_color : t -> Ansi.Color.t -> unit
(** [set_icon_color t c] sets the expand/collapse icon color of [t] to [c]. *)

(** {1:behavior Behavior} *)

val set_wrap_selection : t -> bool -> unit
(** [set_wrap_selection t v] enables selection wrapping at list boundaries when
    [v] is [true] and disables it when [v] is [false]. *)

val set_fast_scroll_step : t -> int -> unit
(** [set_fast_scroll_step t n] sets the number of rows to skip with
    Shift+Up/Down to [n]. [n] is clamped to a minimum of [1]. *)

(** {1:callbacks Callbacks} *)

val set_on_change : t -> (State.t -> unit) option -> unit
(** [set_on_change t cb] receives complete states proposed by input gestures. No
    proposal is applied until accepted through {!apply_props}. Explicit setters
    do not invoke [cb]. *)

val set_on_activate : t -> (item -> unit) option -> unit
(** [set_on_activate t cb] receives the activated logical item. *)

(** {1:layout Layout} *)

val set_style : t -> Toffee.Style.t -> unit
(** [set_style t style] sets the layout style of [t] to [style]. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf] for debugging. *)
