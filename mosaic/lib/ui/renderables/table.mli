(** Rich data table rendering with flexible layout and styling.

    Table provides a feature-complete table widget with headers, footers,
    borders, column sizing, text wrapping, padding, and hierarchical styling. It
    handles complex layout calculations including word wrapping, ratio-based
    column distribution, and content measurement.

    {1 Overview}

    Table renders structured data with customizable columns and rows. Each
    column defines width constraints, alignment, overflow behavior, and optional
    header/footer cells. The table calculates optimal column widths based on
    content, constraints, and available space.

    {1 Column Width}

    Columns support flexible sizing:
    - [width]: Fixed width in cells or auto-calculated
    - [min_width]/[max_width]: Constraints for auto width
    - [ratio]: Proportional distribution of extra space when [expand] is true

    Width algorithm: measures content, applies min/max constraints, distributes
    extra space via ratios when [expand] is true, and collapses wrappable
    columns when exceeding available width.

    {1 Row Styling}

    Row styling supports alternating patterns via [row_styles] list, cycling
    with modulo indexing.

    {1 Text Overflow}

    Three overflow modes control cell content:
    - [`Ellipsis]: Truncate with "..." suffix
    - [`Crop]: Hard truncate at width
    - [`Fold]: Word wrap or character wrap based on [no_wrap] *)

type justify = [ `Left | `Center | `Right | `Full ]
(** Horizontal text alignment within cells. *)

type vertical_align = [ `Top | `Middle | `Bottom ]
(** Vertical alignment of cell content. *)

type overflow = [ `Ellipsis | `Crop | `Fold ]
(** Cell content overflow handling strategy. *)

type padding = int * int * int * int
(** Cell padding as [(top, right, bottom, left)] in cells. *)

type box_style =
  | No_box
  | Simple
  | Rounded
  | Heavy
  | Heavy_head
  | Double
  | Double_edge
  | Ascii
  | Minimal_heavy_head
  | Minimal_double_head
  | Minimal
  | Square
  | Square_double_head
      (** Box drawing styles with Unicode or ASCII characters. *)

type cell
(** Table cell with text content and optional styling. *)

type column
(** Column definition with sizing, alignment, and styling. *)

type row
(** Table row containing cells with optional row-level styling. *)

module Props : sig
  type t

  val make :
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
    ?cell_style:Ansi.Style.t ->
    ?row_styles:Ansi.Style.t list ->
    ?header_style:Ansi.Style.t ->
    ?footer_style:Ansi.Style.t ->
    ?border_style:Ansi.Style.t ->
    ?title:cell ->
    ?title_style:Ansi.Style.t ->
    ?title_justify:justify ->
    ?caption:cell ->
    ?caption_style:Ansi.Style.t ->
    ?caption_justify:justify ->
    ?width:int ->
    ?min_width:int ->
    columns:column list ->
    rows:row list ->
    unit ->
    t

  val equal : t -> t -> bool
end

type t

val apply_props : t -> Props.t -> unit
(** [apply_props table props] applies [props] to a mounted table by updating its
    columns, rows, and styling options and rebuilding the render plan. *)

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render a table. Builds a complete
    styled text representation via {!Text_surface} and internal render plan
    generation. Handles width distribution, text wrapping, and border rendering.
*)

val cell : ?style:Ansi.Style.t -> string -> cell
(** [cell text] creates a cell with optional style override. *)

val column :
  ?header:cell ->
  ?footer:cell ->
  ?style:Ansi.Style.t ->
  ?header_style:Ansi.Style.t ->
  ?footer_style:Ansi.Style.t ->
  ?justify:justify ->
  ?vertical:vertical_align ->
  ?overflow:overflow ->
  ?width:[ `Auto | `Fixed of int ] ->
  ?min_width:int ->
  ?max_width:int ->
  ?ratio:int ->
  ?no_wrap:bool ->
  string ->
  column
(** [column name] defines a table column with layout and styling options.

    @param header Optional header cell.
    @param footer Optional footer cell.
    @param justify Horizontal alignment. Default is [`Left].
    @param vertical Vertical alignment. Default is [`Top].
    @param overflow Content overflow strategy. Default is [`Ellipsis].
    @param width Column width: [`Auto] or [`Fixed n]. Default is [`Auto].
    @param min_width Minimum width constraint for auto columns.
    @param max_width Maximum width constraint for auto columns.
    @param ratio Proportional weight for extra space distribution.
    @param no_wrap
      When true, uses character-level chopping instead of word wrapping in
      [`Fold] mode. Default is [false]. *)

val row : ?style:Ansi.Style.t -> cell list -> row
(** [row cells] creates a table row with optional row-level styling. *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable node. *)

val columns : t -> column list
(** [columns t] returns current column definitions. *)

val rows : t -> row list
(** [rows t] returns current data rows. *)

val set_columns : t -> column list -> unit
(** [set_columns t columns] replaces column definitions. Triggers full rebuild.
*)

val set_rows : t -> row list -> unit
(** [set_rows t rows] replaces data rows. Triggers full rebuild. *)
