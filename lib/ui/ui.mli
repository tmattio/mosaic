(** A High-Level Declarative Terminal UI Library.

    This module is the main entry point for the UI library. It provides all the
    necessary tools to build, lay out, and render a terminal user interface
    using a declarative, functional approach.

    Example Usage:
    {[
      let my_ui =
        Ui.vbox ~gap:1
          [
            Ui.text ~style:Render.Style.(fg red ++ bold) "Hello, World!";
            Ui.divider ();
            Ui.hbox ~gap:2
              [ Ui.text "Item 1"; Ui.flex_spacer (); Ui.text "Item 2" ];
          ]

      let () =
        let buffer = Render.create 80 24 in
        Ui.render buffer my_ui;
        Render.to_string buffer |> print_endline
    ]} *)

type element
(** The abstract type for any UI element. Use the constructor functions below
    (e.g., [text], [hbox], [vbox]) to create values of this type. *)

(** {2 Padding} *)

type padding
(** [padding] specifies spacing between an element's border and its content.
    Values are non-negative integers representing character cells. *)

val padding :
  ?top:int -> ?right:int -> ?bottom:int -> ?left:int -> unit -> padding
(** [padding ?top ?right ?bottom ?left ()] creates padding with individual side
    control. All values default to 0. *)

val pad :
  ?all:int ->
  ?x:int ->
  ?y:int ->
  ?top:int ->
  ?right:int ->
  ?bottom:int ->
  ?left:int ->
  unit ->
  padding
(** [pad ?all ?x ?y ?top ?right ?bottom ?left ()] creates padding with shorthand
    controls. Parameters cascade from general ([all]) to specific ([top], etc.).
*)

val padding_all : int -> padding
(** [padding_all n] creates uniform padding of [n] cells on all sides. *)

val padding_xy : int -> int -> padding
(** [padding_xy x y] creates padding with [x] cells horizontally and [y] cells
    vertically. *)

(** {2 Border} *)

(** [border_style] determines the characters used for drawing borders. *)
type border_style =
  | Solid  (** Single-line box drawing characters (┌─┐). *)
  | Rounded  (** Single-line with rounded corners (╭─╮). *)
  | Double  (** Double-line box drawing characters (╔═╗). *)
  | Thick  (** Heavy-line box drawing characters (┏━┓). *)
  | ASCII  (** Portable characters for maximum compatibility (+-+|). *)

type border
(** [border] specifies visual border properties for elements, with per-side
    control. *)

val border :
  ?top:bool ->
  ?bottom:bool ->
  ?left:bool ->
  ?right:bool ->
  ?style:border_style ->
  ?color:Ansi.color ->
  unit ->
  border
(** [border ?top ?bottom ?left ?right ?style ?color ()] creates a border
    specification. All sides default to [true] and style to [Solid]. *)

val normal_border : border
(** Pre-defined solid border on all sides. *)

val rounded_border : border
(** Pre-defined rounded border on all sides. *)

val double_border : border
(** Pre-defined double-line border on all sides. *)

val thick_border : border
(** Pre-defined thick-line border on all sides. *)

val ascii_border : border
(** Pre-defined ASCII-compatible border on all sides. *)

(** {2 Alignement} *)

type align =
  [ `Start  (** Align to the beginning (left/top). *)
  | `Center  (** Position at the midpoint. *)
  | `End  (** Align to the end (right/bottom). *)
  | `Stretch  (** Expand to fill available space on the cross-axis. *) ]
(** [align] controls element positioning and sizing within available space. *)

(* ** {2 Size Definitions} *)

type size_def =
  [ `Fixed of int  (** A fixed size in character cells. *)
  | `Flex of int  (** A flexible weight for distributing remaining space. *) ]
(** [size_def] specifies how a grid column or row should size itself. *)

(** {2 Constructor Functions} *)

val text :
  ?style:Render.Style.t ->
  ?align:align ->
  ?tab_width:int ->
  ?wrap:bool ->
  string ->
  element
(** A simple text element. *)

val hbox :
  ?gap:int ->
  ?width:int ->
  ?height:int ->
  ?min_width:int ->
  ?min_height:int ->
  ?max_width:int ->
  ?max_height:int ->
  ?margin:padding ->
  ?padding:padding ->
  ?border:border ->
  ?background:Render.Style.t ->
  ?align_items:align ->
  ?justify_content:align ->
  ?flex_grow:int ->
  ?flex_shrink:int ->
  ?fill:bool ->
  ?wrap:bool ->
  element list ->
  element
(** A horizontal container for laying out child elements from left to right. *)

val vbox :
  ?gap:int ->
  ?width:int ->
  ?height:int ->
  ?min_width:int ->
  ?min_height:int ->
  ?max_width:int ->
  ?max_height:int ->
  ?margin:padding ->
  ?padding:padding ->
  ?border:border ->
  ?background:Render.Style.t ->
  ?align_items:align ->
  ?justify_content:align ->
  ?flex_grow:int ->
  ?flex_shrink:int ->
  ?fill:bool ->
  element list ->
  element
(** A vertical container for laying out child elements from top to bottom. *)

val spacer : ?flex:int -> int -> element
(** An empty, transparent element used for spacing. *)

val rich_text : (string * Render.Style.t) list -> element
(** A single line of text composed of multiple styled segments. *)

val zstack : ?align:Element.Z_stack.z_align -> element list -> element
(** A container that layers its children on top of one another. *)

val flow : ?h_gap:int -> ?v_gap:int -> element list -> element
(** A container that lays out children horizontally, wrapping to new lines as
    needed. *)

val grid :
  ?col_spacing:int ->
  ?row_spacing:int ->
  columns:size_def list ->
  rows:size_def list ->
  element list ->
  element
(** A container that arranges children in a grid with fixed and flexible sizing.
*)

val scroll :
  ?width:int ->
  ?height:int ->
  ?h_offset:int ->
  ?v_offset:int ->
  element ->
  element
(** A container that provides a scrollable viewport for a single child element.
*)

(** {2 Helper Elements} *)

val flex_spacer : unit -> element
(** An expandable spacer that fills all available space along a container's main
    axis. *)

val divider : ?style:Render.Style.t -> ?char:string -> unit -> element
(** A horizontal line that expands to fill the available width of its container.
*)

val center : element -> element
(** A convenience container that centers its child both horizontally and
    vertically. *)

val styled : ?fg:Ansi.color -> ?bg:Ansi.color -> element -> element
(** A convenience container that wraps a child to apply a background color. *)

(** {1 Rendering} *)

val render : Render.buffer -> element -> unit
(** [render buf elem] renders the complete element tree to the given buffer.

    It orchestrates the entire rendering process:

    - Calculates the complete layout tree for the given element.
    - Traverses the computed layout and draws the UI to the buffer.
    - Manages an internal layout cache for performance, which is valid only for
      the duration of this single call to [render]. *)

(** {1 Utilities} *)

val measure : ?width:int -> element -> int * int
(** [measure ?width elem] calculates the natural dimensions of an element.
    Returns [(width, height)] in character cells.

    @param width
      Optional width constraint for the element. If provided, the element will
      be measured as if it were being rendered within this width. This affects
      text wrapping and flow layouts.
    @return A tuple [(width, height)] representing the element's dimensions. *)

val pp_element : Format.formatter -> element -> unit
(** [pp_element fmt elem] pretty-prints the structure of an element tree, which
    is useful for debugging. *)

(** {1 Low-level Graphics Module} *)

module Graphics : module type of Graphics
(** Direct access to the low-level graphics primitives module. *)
