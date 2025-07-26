(** Table element with Rich-like API *)

type justify = [ `Left | `Center | `Right | `Full ]
(** Justification options for cell content *)

type vertical_align = [ `Top | `Middle | `Bottom ]
(** Vertical alignment options *)

type overflow = [ `Ellipsis | `Crop | `Fold ]
(** Overflow handling options *)

type column = {
  header : string;
  footer : string option;
  header_style : Style.t;
  footer_style : Style.t;
  style : Style.t;
  justify : justify;
  vertical : vertical_align;
  overflow : overflow;
  width : int option;
  min_width : int option;
  max_width : int option;
  ratio : int option;
  no_wrap : bool;
}
(** Column configuration *)

type padding = int * int * int * int
(** Padding specification (top, right, bottom, left) *)

(** Box drawing styles *)
type box_style =
  | NoBox
  | Simple
  | Rounded
  | Heavy
  | HeavyHead
  | Double
  | DoubleEdge
  | Ascii
  | MinimalHeavyHead
  | MinimalDoubleHead
  | Minimal
  | Square
  | SquareDoubleHead

val default_column : header:string -> column
(** Create a default column configuration *)

val table :
  ?title:string option ->
  ?caption:string option ->
  ?columns:column list ->
  ?rows:string list list ->
  ?box_style:box_style ->
  ?safe_box:bool ->
  ?padding:padding ->
  ?collapse_padding:bool ->
  ?pad_edge:bool ->
  ?expand:bool ->
  ?show_header:bool ->
  ?show_footer:bool ->
  ?show_edge:bool ->
  ?show_lines:bool ->
  ?leading:int ->
  ?style:Style.t ->
  ?row_styles:Style.t list ->
  ?header_style:Style.t ->
  ?footer_style:Style.t ->
  ?border_style:Style.t ->
  ?title_style:Style.t ->
  ?caption_style:Style.t ->
  ?title_justify:justify ->
  ?caption_justify:justify ->
  ?width:int option ->
  ?min_width:int option ->
  unit ->
  Element.t
(** Main table function with Rich-like API

    @param title Optional title displayed above the table
    @param caption Optional caption displayed below the table
    @param columns Column configurations
    @param rows Table data as list of string lists
    @param box_style Box drawing style (default: HeavyHead)
    @param safe_box Use safe box characters for compatibility
    @param padding Cell padding (default: 0,1,0,1)
    @param collapse_padding Collapse padding between cells
    @param pad_edge Add padding to edge cells
    @param expand Expand table to fill available width
    @param show_header Show header row (default: true)
    @param show_footer Show footer row (default: false)
    @param show_edge Show table border (default: true)
    @param show_lines Show lines between rows (default: false)
    @param leading Blank lines between rows (default: 0)
    @param style Default table style
    @param row_styles List of styles to alternate between rows
    @param header_style Header style (default: bold)
    @param footer_style Footer style
    @param border_style Border style
    @param title_style Title style
    @param caption_style Caption style
    @param title_justify Title justification (default: center)
    @param caption_justify Caption justification (default: center)
    @param width Fixed table width
    @param min_width Minimum table width *)

val simple_table : headers:string list -> rows:string list list -> Element.t
(** Simple table with just headers and rows *)

val grid_table : columns:column list -> rows:string list list -> Element.t
(** Grid-style table (no borders, minimal padding) *)
