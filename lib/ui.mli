(** High-level declarative UI construction and layout.

    This module provides composable UI elements with automatic layout
    calculation using a flexbox-inspired model. Elements are immutable values
    that describe UI structure. The rendering engine handles efficient drawing
    and caching.

    All layout calculations respect parent constraints. Padding reduces
    available content area without affecting outer dimensions. Borders are drawn
    within element bounds. Child elements cannot exceed parent dimensions unless
    explicitly sized. *)

(** {2 Layout Types} *)

type padding = { top : int; right : int; bottom : int; left : int }
(** [padding] specifies spacing between element borders and content.

    Padding values are non-negative integers representing terminal character
    cells. Padding reduces the available area for child content without
    affecting the element's outer dimensions. *)

val padding :
  ?top:int -> ?right:int -> ?bottom:int -> ?left:int -> unit -> padding
(** [padding ?top ?right ?bottom ?left ()] creates padding with individual side
    control.

    All values default to 0. Negative values are treated as 0.

    Example: Creates padding with 2 cells on top, 1 on sides.
    {[
      let p = Ui.padding ~top:2 ~left:1 ~right:1 ()
    ]} *)

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
    controls.

    Parameters cascade from general to specific: [all] sets all four sides, [x]
    overrides horizontal sides (left/right), [y] overrides vertical sides
    (top/bottom), and individual side parameters override all others.

    Example: Creates 2-cell padding everywhere except 4 cells on top.
    {[
      let p = Ui.pad ~all:2 ~top:4 ()
    ]} *)

val padding_all : int -> padding
(** [padding_all n] creates uniform padding of [n] cells on all sides.

    Equivalent to [pad ~all:n ()]. Negative values are treated as 0. *)

val padding_xy : int -> int -> padding
(** [padding_xy x y] creates padding with [x] cells horizontally and [y] cells
    vertically.

    Equivalent to [pad ~x ~y ()]. Sets left/right to [x] and top/bottom to [y].
*)

type border_style =
  | Solid
  | Rounded
  | Double
  | Thick
  | ASCII
      (** [border_style] determines the characters used for drawing borders.

          Solid uses single-line box drawing (┌─┐│└┘). Rounded uses curved
          corners (╭─╮│╰╯). Double uses double-line characters (╔═╗║╚╝). Thick
          uses heavy lines (┏━┓┃┗┛). ASCII uses portable characters (+-+|) for
          environments without Unicode support. *)

type border_spec = {
  top : bool;
  bottom : bool;
  left : bool;
  right : bool;
  style : border_style;
  color : Ansi.color option;
}
(** [border_spec] specifies visual border properties for elements with per-side control.

    Each side can be individually enabled/disabled. The border occupies one 
    character cell on each enabled side within the element's dimensions. 
    Style and color apply to all border characters. Borders reduce available
    content area by the number of enabled sides. *)

type border = border_spec
(** Alias for backward compatibility *)

val border : ?style:border_style -> ?color:Ansi.color -> unit -> border
(** [border ?style ?color ()] creates a border specification with all sides enabled.

    The [style] defaults to [Solid]. The [color] defaults to terminal's default
    foreground color.

    Example: Creates a blue rounded border.
    {[
      let b = Ui.border ~style:Rounded ~color:Ansi.Blue ()
    ]} *)

val border_spec : 
  ?top:bool -> 
  ?bottom:bool -> 
  ?left:bool -> 
  ?right:bool -> 
  ?style:border_style -> 
  ?color:Ansi.color -> 
  unit -> 
  border_spec
(** [border_spec ?top ?bottom ?left ?right ?style ?color ()] creates a border
    specification with per-side control.

    All sides default to [true]. The [style] defaults to [Solid].

    Example: Creates a border with only top and bottom.
    {[
      let b = Ui.border_spec ~left:false ~right:false ()
    ]} *)

val normal_border : border_spec
(** Pre-defined normal border with single lines (┌─┐│└┘) on all sides *)

val rounded_border : border_spec  
(** Pre-defined rounded border with curved corners (╭─╮│╰╯) on all sides *)

val double_border : border_spec
(** Pre-defined double border with double lines (╔═╗║╚╝) on all sides *)

val thick_border : border_spec
(** Pre-defined thick border with heavy lines (┏━┓┃┗┛) on all sides *)

val ascii_border : border_spec
(** Pre-defined ASCII border with portable characters (+-+|) on all sides *)

type align =
  | Start
  | Center
  | End
  | Stretch
      (** [align] controls element positioning within available space.

          Start aligns to the beginning (left/top). Center positions at the
          midpoint. End aligns to the end (right/bottom). Stretch expands to
          fill available space. *)

type element
(** [element] represents an abstract UI component.

    Elements are immutable descriptions of UI structure. They support automatic
    layout calculation and efficient caching. The rendering engine converts
    elements to terminal output while preserving layout constraints. *)

val text : ?style:Render.Style.t -> string -> element
(** [text ?style s] creates a text element displaying [s].

    Text elements have natural dimensions based on string width and line count.
    Newlines create multiple lines. The optional [style] applies color,
    attributes, and formatting. Multi-byte Unicode is handled correctly.

    Example: Creates bold red error text.
    {[
      let error = Ui.text ~style:Style.(fg red ++ bold) "Error: File not found"
    ]} *)

val hbox :
  ?gap:int ->
  ?width:int ->
  ?height:int ->
  ?margin:padding ->
  ?padding:padding ->
  ?border:border ->
  ?align_items:align ->
  ?justify_content:align ->
  element list ->
  element
(** [hbox ?gap ?width ?height ?margin ?padding ?border ?align_items ?justify_content
     children] creates a horizontal layout container.

    Children are arranged left-to-right with optional spacing. The box respects
    size constraints while calculating child positions. Expandable children
    (created with [expand]) share remaining space equally after fixed-size
    children are placed.

    @param gap Space between children in character cells (default: 0)
    @param width Fixed width in cells (default: sum of children plus gaps)
    @param height Fixed height in cells (default: tallest child)
    @param margin External spacing around the element
    @param padding Internal spacing reducing content area
    @param border Visual border drawn within bounds
    @param align_items
      Controls vertical alignment of children (default: Stretch)
    @param justify_content Controls horizontal distribution (default: Start)

    Example: Creates a horizontal menu bar with spacing.
    {[
      let menu =
        Ui.hbox ~gap:2 ~border:(Ui.border ())
          [
            Ui.text "File";
            Ui.text "Edit";
            Ui.expand (Ui.text "");
            (* Pushes remaining items right *)
            Ui.text "Help";
          ]
    ]} *)

val vbox :
  ?gap:int ->
  ?width:int ->
  ?height:int ->
  ?margin:padding ->
  ?padding:padding ->
  ?border:border ->
  ?align_items:align ->
  ?justify_content:align ->
  element list ->
  element
(** [vbox ?gap ?width ?height ?padding ?border ?align_items ?justify_content
     children] creates a vertical layout container.

    Children are arranged top-to-bottom with optional spacing. The box respects
    size constraints while calculating child positions. Expandable children
    share remaining vertical space equally after fixed-size children are placed.

    @param gap Space between children in character cells (default: 0)
    @param width Fixed width in cells (default: widest child)
    @param height Fixed height in cells (default: sum of children plus gaps)
    @param padding Internal spacing reducing content area
    @param border Visual border drawn within bounds
    @param align_items
      Controls horizontal alignment of children (default: Stretch)
    @param justify_content Controls vertical distribution (default: Start)

    Example: Creates a form with labeled fields.
    {[
      let form =
        Ui.vbox ~gap:1 ~padding:(Ui.pad ~all:2 ())
          [
            Ui.text "Username:";
            Ui.hbox [ Ui.text "[____________________]" ];
            Ui.text "Password:";
            Ui.hbox [ Ui.text "[____________________]" ];
          ]
    ]} *)

val spacer : int -> element
(** [spacer n] creates n units of empty space *)

val space : int -> element
(** [space n] creates n units of empty space. Alias for spacer. *)

val expand : element -> element
(** [expand elem] makes elem fill available space *)

val render : Render.buffer -> element -> unit
(** [render buf elem] renders elem to buf with layout support *)

val pp_element : Format.formatter -> element -> unit
(** [pp_element fmt elem] pretty-prints element elem.

    Useful for debugging or logging UI structures. *)

val clear_cache : element -> unit
(** [clear_cache elem] invalidates layout caches.

    Call before each frame to ensure fresh layout calculations. *)
