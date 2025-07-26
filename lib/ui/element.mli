(** High-level declarative UI construction.

    This module provides the core data types and constructor functions for
    building a user interface. It defines an abstract type [t] for all UI
    elements, ensuring that they can only be created and manipulated through the
    functions provided in this interface. *)

(** {2 Padding} *)

module Padding : sig
  type t
  (** An abstract type representing padding around an element's content. *)

  val make : ?top:int -> ?right:int -> ?bottom:int -> ?left:int -> unit -> t
  (** [make ?top ?right ?bottom ?left ()] creates a new padding object with
      individual side control. All values default to 0. *)

  val no_padding : t
  (** [no_padding] is a padding object with all sides set to 0. *)

  val all : int -> t
  (** [all n] creates uniform padding of [n] cells on all sides. *)

  val xy : int -> int -> t
  (** [xy x y] creates padding with [x] cells horizontally and [y] cells
      vertically. *)

  val top : t -> int
  (** [top p] returns the top padding value. *)

  val bottom : t -> int
  (** [bottom p] returns the bottom padding value. *)

  val left : t -> int
  (** [left p] returns the left padding value. *)

  val right : t -> int
  (** [right p] returns the right padding value. *)

  val pad :
    ?all:int ->
    ?x:int ->
    ?y:int ->
    ?top:int ->
    ?right:int ->
    ?bottom:int ->
    ?left:int ->
    unit ->
    t
  (** [pad ?all ?x ?y ?top ?right ?bottom ?left ()] creates padding with
      shorthand controls. Parameters cascade from general ([all]) to specific
      ([top], etc.). *)
end

(** {2 Alignement and Size} *)

type align =
  [ `Start  (** Align to the beginning (left/top). *)
  | `Center  (** Position at the midpoint. *)
  | `End  (** Align to the end (right/bottom). *)
  | `Stretch  (** Expand to fill available space on the cross-axis. *) ]
(** [align] controls element positioning and sizing within available space. *)

type size_def =
  [ `Fixed of int  (** A fixed size in character cells. *)
  | `Flex of int  (** A flexible weight for distributing remaining space. *) ]
(** Specifies sizing for grid columns and rows. *)

(** {1 Element Kinds} *)

module rec Text : sig
  type t

  val make :
    ?style:Style.t ->
    ?align:align ->
    ?tab_width:int ->
    ?wrap:bool ->
    string ->
    t

  val content : t -> string
  val style : t -> Style.t
  val alignment : t -> align
  val tab_width : t -> int
  val is_wrapping : t -> bool
end

and Rich_text : sig
  type t

  val make : (string * Style.t) list -> t
  (** Creates a single line of text with multiple styled segments. *)

  val segments : t -> (string * Style.t) list
end

and Spacer : sig
  type t

  val make : ?flex:int -> int -> t
  (** [make ?flex size] creates an empty space element. If [flex > 0], it
      expands to fill available space. *)

  val size : t -> int
  (** [size t] returns the fixed size of the spacer in character cells. *)

  val flex : t -> int
  (** [flex t] returns the flex-grow factor of the spacer. If [flex > 0], it
      expands to fill available space. *)
end

and Box : sig
  type t
  type direction = [ `Horizontal | `Vertical ]

  type layout_options = {
    direction : direction;
    gap : int;
    width : int option;
    height : int option;
    min_width : int option;
    min_height : int option;
    max_width : int option;
    max_height : int option;
    margin : Padding.t;
    padding : Padding.t;
    border : Border.t option;
    background : Style.t option;
    align : align;
    justify : align;
    flex_grow : int;
    flex_shrink : int;
    fill : bool;
  }

  val make : options:layout_options -> T.t list -> t
  (** Creates a box element (e.g., hbox, vbox). *)

  val children : t -> T.t list
  (** [children box] returns the list of child elements contained in the box. *)

  (** {3 Accessors} *)

  val options : t -> layout_options
  (** [options box] returns the layout options used for this box. *)
end

and Z_stack : sig
  type t

  type z_align =
    | Top_left
    | Top
    | Top_right
    | Left
    | Center
    | Right
    | Bottom_left
    | Bottom
    | Bottom_right

  val make : ?align:z_align -> T.t list -> t
  (** Creates a Z-stack element where children are layered on top of each other.
  *)

  val children : t -> T.t list
  (** [children box] returns the list of child elements contained in the box. *)

  val alignment : t -> z_align
  (** [alignment box] returns the alignment used for stacking children. *)
end

and Flow : sig
  type t

  val make : ?h_gap:int -> ?v_gap:int -> T.t list -> t
  (** Creates a flow element that wraps children to fit available width. *)

  val children : t -> T.t list
  (** [children box] returns the list of child elements contained in the box. *)

  val h_gap : t -> int
  (** [h_gap box] returns the horizontal gap between children in the flow. *)

  val v_gap : t -> int
  (** [v_gap box] returns the vertical gap between lines in the flow. *)
end

and Grid : sig
  type t

  val make :
    ?col_spacing:int ->
    ?row_spacing:int ->
    columns:size_def list ->
    rows:size_def list ->
    T.t list ->
    t
  (** Creates a grid element with fixed and flexible sizing. *)

  val children : t -> T.t list
  (** [children box] returns the list of child elements contained in the box. *)

  val columns : t -> size_def list
  val rows : t -> size_def list
  val col_spacing : t -> int
  val row_spacing : t -> int
end

and Scroll : sig
  type t

  val make :
    ?width:int -> ?height:int -> ?h_offset:int -> ?v_offset:int -> T.t -> t
  (** Creates a scrollable viewport for a single child element. *)

  val child : t -> T.t
  (** [child scroll] returns the single child element contained in the scroll.
  *)

  val width : t -> int option
  val height : t -> int option
  val h_offset : t -> int
  val v_offset : t -> int
end

and Canvas : sig
  type t

  val width : t -> int option
  val height : t -> int option

  val draw :
    t ->
    buffer:Render.buffer ->
    x:int ->
    y:int ->
    w:int ->
    h:int ->
    dark:bool ->
    unit
  (** [draw canvas ~buffer ~x ~y ~h ~w ~dark] performs custom drawing on the
      canvas using the provided buffer and dimensions. *)
end

and T : sig
  type t =
    | Text of Text.t
    | Rich_text of Rich_text.t
    | Spacer of Spacer.t
    | Box of Box.t
    | Z_stack of Z_stack.t
    | Flow of Flow.t
    | Grid of Grid.t
    | Scroll of Scroll.t
    | Canvas of Canvas.t
end

type t = T.t =
  | Text of Text.t
  | Rich_text of Rich_text.t
  | Spacer of Spacer.t
  | Box of Box.t
  | Z_stack of Z_stack.t
  | Flow of Flow.t
  | Grid of Grid.t
  | Scroll of Scroll.t
  | Canvas of Canvas.t

(** {2 Element Constructors} *)

val text :
  ?style:Style.t -> ?align:align -> ?tab_width:int -> ?wrap:bool -> string -> t
(** [text ?style ?align ?tab_width ?wrap s] creates a text
    @param wrap If true, text wraps to fit the container's width. *)

val hbox :
  ?gap:int ->
  ?width:int ->
  ?height:int ->
  ?min_width:int ->
  ?min_height:int ->
  ?max_width:int ->
  ?max_height:int ->
  ?margin:Padding.t ->
  ?padding:Padding.t ->
  ?border:Border.t ->
  ?background:Style.t ->
  ?align_items:align ->
  ?justify_content:align ->
  ?flex_grow:int ->
  ?flex_shrink:int ->
  ?fill:bool ->
  ?wrap:bool ->
  t list ->
  t
(** [hbox children] creates a horizontal layout container.
    @param align_items
      Controls vertical alignment of children (default: Stretch).
    @param justify_content Controls horizontal distribution (default: Start). *)

val vbox :
  ?gap:int ->
  ?width:int ->
  ?height:int ->
  ?min_width:int ->
  ?min_height:int ->
  ?max_width:int ->
  ?max_height:int ->
  ?margin:Padding.t ->
  ?padding:Padding.t ->
  ?border:Border.t ->
  ?background:Style.t ->
  ?align_items:align ->
  ?justify_content:align ->
  ?flex_grow:int ->
  ?flex_shrink:int ->
  ?fill:bool ->
  t list ->
  t
(** [vbox children] creates a vertical layout container.
    @param align_items
      Controls horizontal alignment of children (default: Stretch).
    @param justify_content Controls vertical distribution (default: Start).
    @param fill
      If true, inserts a flexible spacer to push content (default: true). *)

val spacer : ?flex:int -> int -> t
(** [spacer ?flex size] creates empty space. If [flex > 0], it expands. *)

val rich_text : (string * Style.t) list -> t
(** [rich_text segments] creates a single line of text with multiple styles. *)

val z_stack : ?align:Z_stack.z_align -> t list -> t
(** [z_stack ?align children] overlays children, rendering later children on
    top. *)

val flow : ?h_gap:int -> ?v_gap:int -> t list -> t
(** [flow ?h_gap ?v_gap children] lays out children horizontally, wrapping to
    new lines as needed. *)

val grid :
  ?col_spacing:int ->
  ?row_spacing:int ->
  columns:size_def list ->
  rows:size_def list ->
  t list ->
  t
(** [grid ~columns ~rows children] creates a grid with fixed and flexible
    sizing. *)

val scroll :
  ?width:int -> ?height:int -> ?h_offset:int -> ?v_offset:int -> t -> t
(** [scroll ?h_offset ?v_offset child] creates a scrollable viewport for child
    content. *)

val canvas :
  ?width:int ->
  ?height:int ->
  (buffer:Render.buffer ->
  x:int ->
  y:int ->
  w:int ->
  h:int ->
  dark:bool ->
  unit) ->
  t
(** [canvas ?width ?height draw] creates a canvas element that allows custom
    drawing commands.
    @param width Optional fixed width for the canvas.
    @param height Optional fixed height for the canvas.
    @param draw The function that performs the drawing on the canvas. *)

(** {2 Helper Functions} *)

val flex_spacer : unit -> t
(** [flex_spacer ()] creates an expandable spacer to push content apart.
    Equivalent to [spacer ~flex:1 0]. *)

val divider : ?style:Style.t -> ?char:string -> unit -> t
(** [divider ()] creates a horizontal line that expands to fill available width.
*)

val center : t -> t
(** [center child] centers an element both horizontally and vertically. *)

val styled : Style.t -> t -> t
(** [styled style child] wraps an element in a box to apply styling. *)

(** {2 Measure Elements} *)

val measure : ?width:int -> t -> int * int
(** [measure ?width element] returns the (width, height) that the element would
    naturally occupy.
    @param width
      An optional width constraint for width-aware elements like [vbox] or
      wrapping text. *)

val min_width : t -> int
(** [min_width element] returns the minimum possible width the element can be
    shrunk to. For wrapping text, this is the width of the longest word. *)

val min_height : t -> int
(** [min_height element] returns the minimum possible height the element can be
    shrunk to. For boxes, this is the sum/max of children's min heights. *)

val grow_fact : t -> int
(** [grow_fact element] returns the flex-grow factor of an *)

val shrink_fact : t -> int
(** [shrink_fact element] returns the flex-shrink factor of an *)

(** {2 Utilities} *)

val pp : Format.formatter -> t -> unit
(** [pp_element fmt elem] pretty-prints the structure of an element for
    debugging. *)

type expanded_child = { elem : t; is_new_item : bool; is_hard_break : bool }

val expand_children : t list -> expanded_child list
(** [expand_children children] expands a list of elements into a list of
    expanded_child records, which include information about whether each child
    is a new item or a hard break. This is useful for layout calculations. *)

(** {2 Additional UI Primitives} *)

val checkbox : checked:bool -> label:string -> ?style:Style.t -> unit -> t
(** [checkbox ~checked ~label ?style ()] creates a static checkbox visual.
    Renders as "[x] label" when checked or "[ ] label" when unchecked. *)

val radio : checked:bool -> label:string -> ?style:Style.t -> unit -> t
(** [radio ~checked ~label ?style ()] creates a static radio button visual.
    Renders as "(o) label" when checked or "( ) label" when unchecked. *)

val image : lines:string list -> ?align:align -> unit -> t
(** [image ~lines ?align ()] creates an element for rendering multi-line ASCII
    art or logos. Each line is rendered as-is, with wrapping/clipping handled
    like text. *)

val separator :
  ?orientation:[ `Horizontal | `Vertical ] ->
  ?char:string ->
  ?style:Style.t ->
  unit ->
  t
(** [separator ?orientation ?char ?style ()] creates a separator line.
    @param orientation Direction of separator (default: `Horizontal)
    @param char
      Character to use for the line (default: "─" for horizontal, "│" for
      vertical)
    @param style Style to apply to the separator *)

val list : items:t list -> ?bullet:string -> ?numbering:bool -> unit -> t
(** [list ~items ?bullet ?numbering ()] creates a vertical list with optional
    bullets or numbers.
    @param items List of elements to display
    @param bullet Custom bullet character (default: "•")
    @param numbering If true, use numbers instead of bullets *)
