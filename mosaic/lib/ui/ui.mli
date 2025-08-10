(** Mosaic UI * * A declarative, flexbox-based UI library for building terminal
    applications. *)

(** {1 Core Types} *)

type element
(** The abstract type for a UI element. *)

(** Attributes for UI elements *)
module Attr : sig
  type key = private string
  (** Stable identity key for elements *)

  val key : string -> key
  (** Create a key from a string. Keys should be unique within their parent
      context. *)

  val key_to_string : key -> string
  (** Convert a key back to its string representation *)
end

module Key : sig
  val create : prefix:string -> Attr.key
  val of_string : string -> Attr.key
  val of_int : int -> Attr.key
end

module Layout_snapshot : sig
  (** Layout snapshot for element geometry observation

      This module provides a way to record and query the final rendered
      positions and dimensions of UI elements that have been tagged with keys.
      The snapshot is populated during rendering and can be queried by the
      runtime for hit-testing, focus management, and other position-aware
      operations. *)

  type rect = {
    x : int;  (** Absolute column position *)
    y : int;  (** Absolute row position *)
    w : int;  (** Width in columns *)
    h : int;  (** Height in rows *)
  }
  (** Rectangle representing absolute screen coordinates *)

  type entry = {
    rect : rect;  (** Absolute bounding box *)
    z_index : int;  (** Stacking order (higher = on top) *)
    clipping : rect option;  (** Clipping rectangle if element is clipped *)
  }
  (** Entry for a keyed element *)

  type t
  (** The snapshot type *)

  val create : unit -> t
  (** Create a new empty snapshot *)

  val record : t -> Attr.key -> entry -> unit
  (** Record an element's layout information *)

  val get : t -> Attr.key -> entry option
  (** Get the layout information for a keyed element *)

  val clear : t -> unit
  (** Clear all recorded entries *)

  val iter : t -> (Attr.key -> entry -> unit) -> unit
  (** Iterate over all recorded entries *)

  val fold : t -> init:'a -> f:('a -> Attr.key -> entry -> 'a) -> 'a
  (** Fold over all recorded entries *)

  val keys : t -> Attr.key list
  (** Get all keys in the snapshot *)

  val hit_test : t -> x:int -> y:int -> Attr.key option
  (** Find the topmost element at the given position *)

  val hit_test_all : t -> x:int -> y:int -> (Attr.key * entry) list
  (** Find all elements at the given position, sorted by z-index (topmost first)
  *)

  val point_in_rect : x:int -> y:int -> rect -> bool
  (** Check if a point is within a rectangle *)

  val size : t -> int
  (** Get the number of entries in the snapshot *)
end

module Style : module type of Style
(** The type for defining a color style. See the [Style] module. *)

module Border : module type of Border
(** The type for defining element borders. See the [Border] module. *)

module Theme : module type of Theme
(** The type for theming and constants. See the [Theme] module. *)

(** {2 Geometric Types} *)

type 'a sides = { top : 'a; right : 'a; bottom : 'a; left : 'a }
(** Four-sided values for padding, margin, border, inset *)

type 'a span = { start : 'a; end_ : 'a }
(** Start/end span for grid placement *)

(** {2 Dimension Types} *)

type length_percentage = [ `Cells of int | `Pct of float ]
(** Length in terminal cells or percentage (for padding, borders, gaps).
    Percentage is 0.0-1.0, not 0-100. *)

type length_percentage_auto = [ length_percentage | `Auto ]
(** Length, percentage, or auto (for margins, insets). Percentage is 0.0-1.0,
    not 0-100. *)

type calc_resolver = int -> float -> float
(** Calc resolver function: index -> context_size -> resolved_value *)

type dimension = [ length_percentage_auto | `Calc of int ]
(** Dimension type for width, height, flex-basis, etc. Calc takes an integer
    index that gets resolved via calc_resolver. *)

(** {2 Layout Control Types} *)

type display = [ `Block | `Flex | `Grid | `Hidden ]
(** Display mode: Block, Flex, Grid, or Hidden (generates no boxes) *)

type position = [ `Relative | `Absolute ]
(** Positioning: Relative to normal flow or Absolute within parent *)

type overflow = [ `Visible | `Clip | `Hidden | `Scroll ]
(** Overflow handling: Visible, Clip at bounds, Hidden, or Scrollable *)

type box_sizing = [ `Border_box | `Content_box ]
(** Box sizing: Border_box includes padding/border, Content_box excludes them *)

type text_align = [ `Auto | `Legacy_left | `Legacy_right | `Legacy_center ]
(** Text alignment for block layout *)

(** {2 Flexbox Types} *)

type flex_direction = [ `Row | `Column | `Row_reverse | `Column_reverse ]
(** Flex direction: Row, Column, or their reverse variants *)

type flex_wrap = [ `No_wrap | `Wrap | `Wrap_reverse ]
(** Flex wrapping: No_wrap (single line), Wrap, or Wrap_reverse *)

(** {2 Alignment Types} *)

type align_items =
  [ `Start | `End | `Flex_start | `Flex_end | `Center | `Baseline | `Stretch ]
(** Alignment on cross axis (Flexbox) or block axis (Grid) *)

type align_content =
  [ `Start
  | `End
  | `Flex_start
  | `Flex_end
  | `Center
  | `Stretch
  | `Space_between
  | `Space_evenly
  | `Space_around ]
(** Content alignment for wrapped lines (Flexbox) or tracks (Grid) *)

type align_self = align_items
(** Self alignment, overrides parent's align_items *)

type justify_items = align_items
(** Item justification on inline axis (Grid only, ignored in Flexbox) *)

type justify_self = align_items
(** Self justification, overrides parent's justify_items (Grid only, ignored in
    Flexbox) *)

type justify_content = align_content
(** Content distribution on main axis (Flexbox) or inline axis (Grid) *)

(** {2 Grid Types} *)

type grid_auto_flow = [ `Row | `Column | `Row_dense | `Column_dense ]
(** Grid auto-placement flow: Row, Column, or dense packing variants *)

type grid_placement =
  [ `Auto
  | `Line of int  (** CSS line number (can be negative) *)
  | `Span of int
  | `Named_line of string * int
  | `Named_span of string * int ]
(** Grid placement specification for a single axis *)

type track_sizing_function
(** Grid track sizing function - use constructor functions below *)

type grid_repetition
(** Grid repetition pattern - use constructor functions below *)

type repetition_count = [ `Count of int | `Auto_fill | `Auto_fit ]

type grid_template_component =
  | Single of track_sizing_function
  | Repeat of grid_repetition

type grid_template_area
(** Named grid area - use constructor functions below *)

(** {2 Dimension Constructors} *)

val cells : int -> [ `Cells of int ]
(** Create a length in terminal cells (e.g., cells 10) *)

val pct : float -> [ `Pct of float ]
(** Create a percentage (0.0 to 1.0, not 0-100) *)

val auto : [ `Auto ]
(** Auto value for dimensions *)

val calc : int -> [ `Calc of int ]
(** Create a calc dimension with given index *)

(** {2 Sides Constructors} *)

val all : int -> [> `Cells of int ] sides
(** Create sides with all edges set to the same value in cells *)

val xy : int -> int -> [> `Cells of int ] sides
(** Create sides with horizontal (left/right) and vertical (top/bottom) values
    in cells *)

val sides :
  ?top:int ->
  ?bottom:int ->
  ?left:int ->
  ?right:int ->
  unit ->
  [> `Cells of int ] sides
(** Create sides with optional overrides, all in cells *)

(** {2 Grid Track Constructors} *)

val fr : float -> track_sizing_function
(** Create a fractional unit track (e.g., 1fr, 2.5fr) *)

val minmax : length_percentage -> length_percentage -> track_sizing_function
(** Create a minmax track sizing function *)

val fit_content_track : length_percentage -> track_sizing_function
(** Create a fit-content track sizing function *)

val min_content : track_sizing_function
(** Min-content track sizing *)

val max_content : track_sizing_function
(** Max-content track sizing *)

val track_cells : int -> track_sizing_function
(** Fixed cell size track *)

val track_pct : float -> track_sizing_function
(** Percentage size track *)

val track_auto : track_sizing_function
(** Auto-sized track *)

val repeat :
  ?line_names:string list list ->
  repetition_count ->
  track_sizing_function list ->
  grid_repetition
(** Create a repeat pattern for grid tracks with optional line names *)

val grid_area :
  name:string ->
  row_start:int ->
  row_end:int ->
  column_start:int ->
  column_end:int ->
  grid_template_area
(** Create a named grid area *)

(** {1 Layout Primitives} *)

val box :
  ?display:display ->
  ?position:position ->
  ?box_sizing:box_sizing ->
  ?text_align:text_align ->
  ?flex_direction:flex_direction ->
  ?flex_wrap:flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?align_items:align_items ->
  ?align_self:align_self ->
  ?align_content:align_content ->
  ?justify_content:justify_content ->
  ?justify_items:justify_items ->
  ?justify_self:justify_self ->
  ?overflow_x:overflow ->
  ?overflow_y:overflow ->
  ?aspect_ratio:float ->
  ?scrollbar_width:float ->
  ?inset:length_percentage_auto sides ->
  ?width:dimension ->
  ?height:dimension ->
  ?min_width:dimension ->
  ?min_height:dimension ->
  ?max_width:dimension ->
  ?max_height:dimension ->
  ?padding:length_percentage sides ->
  ?margin:length_percentage_auto sides ->
  ?border_width:length_percentage sides ->
  ?gap:length_percentage ->
  ?row_gap:length_percentage ->
  ?col_gap:length_percentage ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  element list ->
  element
(** A foundational container element with comprehensive layout control.

    @param children The list of elements to be placed inside this box.

    --- Layout Mode ---
    @param display The layout mode: Block, Flex, Grid, or None.
    @param box_generation_mode Whether to generate boxes (Normal) or not (None).
    @param box_sizing
      Border_box includes padding/border in size, Content_box doesn't.
    @param text_align Text alignment for block layout.

    --- Sizing ---
    @param width The width of the box.
    @param height The height of the box.
    @param min_width The minimum width.
    @param min_height The minimum height.
    @param max_width The maximum width.
    @param max_height The maximum height.
    @param aspect_ratio Aspect ratio constraint (width/height).

    --- Flex Container Properties (affect children) ---
    @param flex_direction
      The main axis: Row, Column, Row_reverse, Column_reverse.
    @param flex_wrap Whether items wrap: No_wrap, Wrap, Wrap_reverse.
    @param align_items How to align children on cross axis.
    @param align_content How wrapped lines are aligned.
    @param justify_content How to distribute children along main axis.
    @param justify_items Default justify-self for children (Grid).
    @param gap Space between children (both axes).
    @param row_gap Vertical space between children.
    @param col_gap Horizontal space between children.

    --- Flex Item Properties (affect this box within its parent) ---
    @param flex_grow Growth factor to fill available space.
    @param flex_shrink Shrink factor when space is insufficient.
    @param flex_basis Initial main size before grow/shrink.
    @param align_self Override parent's align_items.
    @param justify_self Override parent's justify_items (Grid).

    --- Spacing ---
    @param padding Space between border and content.
    @param margin Space outside the border.
    @param border_width Width of the border for layout calculation.

    --- Positioning ---
    @param position Relative or Absolute positioning.
    @param inset Offsets for positioned elements (top/right/bottom/left).

    --- Overflow ---
    @param overflow_x Horizontal overflow behavior.
    @param overflow_y Vertical overflow behavior.
    @param scrollbar_width Width reserved for scrollbars.

    --- Visual ---
    @param style Text/color styling.
    @param border Visual border decoration.
    @param border_style Style for the visual border. *)

val vbox :
  ?width:dimension ->
  ?height:dimension ->
  ?min_width:dimension ->
  ?min_height:dimension ->
  ?max_width:dimension ->
  ?max_height:dimension ->
  ?padding:length_percentage sides ->
  ?margin:length_percentage_auto sides ->
  ?border_width:length_percentage sides ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?align_self:align_self ->
  ?align_items:align_items ->
  ?justify_content:justify_content ->
  ?gap:length_percentage ->
  ?overflow_x:overflow ->
  ?overflow_y:overflow ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  element list ->
  element
(** Arranges elements vertically in a flex column.

    This is a convenience function that creates a flex container with
    flex_direction set to Column.

    @param width The width of the container.
    @param height The height of the container.
    @param min_width The minimum width.
    @param min_height The minimum height.
    @param max_width The maximum width.
    @param max_height The maximum height.
    @param padding Space between border and content.
    @param margin Space outside the border.
    @param border_width Width of the border for layout.
    @param flex_grow Growth factor for this container.
    @param flex_shrink Shrink factor for this container.
    @param flex_basis Initial size before grow/shrink.
    @param align_self Override parent's align_items.
    @param align_items How to align children horizontally (cross-axis).
    @param justify_content How to distribute children vertically (main-axis).
    @param gap The space between each child element.
    @param overflow_x Horizontal overflow behavior.
    @param overflow_y Vertical overflow behavior.
    @param style Text/color styling.
    @param border Visual border decoration.
    @param border_style Style for the visual border. *)

val hbox :
  ?width:dimension ->
  ?height:dimension ->
  ?min_width:dimension ->
  ?min_height:dimension ->
  ?max_width:dimension ->
  ?max_height:dimension ->
  ?padding:length_percentage sides ->
  ?margin:length_percentage_auto sides ->
  ?border_width:length_percentage sides ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?align_self:align_self ->
  ?align_items:align_items ->
  ?justify_content:justify_content ->
  ?gap:length_percentage ->
  ?overflow_x:overflow ->
  ?overflow_y:overflow ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  element list ->
  element
(** Arranges elements horizontally in a flex row.

    This is a convenience function that creates a flex container with
    flex_direction set to Row.

    @param width The width of the container.
    @param height The height of the container.
    @param min_width The minimum width.
    @param min_height The minimum height.
    @param max_width The maximum width.
    @param max_height The maximum height.
    @param padding Space between border and content.
    @param margin Space outside the border.
    @param border_width Width of the border for layout.
    @param flex_grow Growth factor for this container.
    @param flex_shrink Shrink factor for this container.
    @param flex_basis Initial size before grow/shrink.
    @param align_self Override parent's align_items.
    @param align_items How to align children vertically (cross-axis).
    @param justify_content How to distribute children horizontally (main-axis).
    @param gap The space between each child element.
    @param overflow_x Horizontal overflow behavior.
    @param overflow_y Vertical overflow behavior.
    @param style Text/color styling.
    @param border Visual border decoration.
    @param border_style Style for the visual border. *)

val zbox :
  ?width:dimension ->
  ?height:dimension ->
  ?min_width:dimension ->
  ?min_height:dimension ->
  ?max_width:dimension ->
  ?max_height:dimension ->
  ?padding:length_percentage sides ->
  ?margin:length_percentage_auto sides ->
  ?border_width:length_percentage sides ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?align_self:align_self ->
  ?overflow_x:overflow ->
  ?overflow_y:overflow ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  element list ->
  element
(** Lays out elements on top of each other, from back to front.

    Each child is stretched to fill the container by default. This function
    inherits all standard box-model parameters but does not have child layout
    parameters like [gap] or [align_items] as children overlap. *)

val spacer :
  ?flex_grow:float ->
  ?min_width:dimension ->
  ?min_height:dimension ->
  unit ->
  element
(** Creates an invisible, flexible element that expands to fill available space.
    * Its primary customization is [flex_grow], but a minimum size can also be
    set. * It does not accept general styling, as it is intended to be
    invisible. *)

val divider :
  ?orientation:[ `Horizontal | `Vertical ] ->
  ?title:string ->
  ?char:string ->
  ?style:Style.t ->
  ?padding:length_percentage sides ->
  unit ->
  element
(** Renders a horizontal or vertical line to visually separate content.

    @param orientation The direction of the line. Defaults to [`Horizontal`].
    @param title
      An optional string to embed within the divider, e.g., "--- Title ---".
    @param style
      A style to apply to the divider's characters and title (e.g., color).
    @param padding
      Adds space around the divider, pushing it away from other content. *)

val text :
  ?style:Style.t ->
  ?align:[ `Left | `Center | `Right ] ->
  ?wrap:[ `Wrap | `Truncate | `Clip ] ->
  ?overflow_x:overflow ->
  ?overflow_y:overflow ->
  string ->
  element
(** Renders a block of text.

    When [wrap] is [`Wrap] (the default), the text automatically uses
    [overflow_x:`Hidden] to ensure it can shrink and wrap properly within its
    container. This prevents the CSS flexbox automatic minimum size from
    blocking text wrapping.

    @param style The color and text attributes (bold, italic, etc.).
    @param align The horizontal alignment of the text within its bounding box.
    @param wrap
      How to handle text that exceeds the element's width. Defaults to [`Wrap`].
      When [`Wrap`], automatically sets overflow_x to [`Hidden`] for proper
      wrapping.
    @param overflow_x
      Controls horizontal overflow behavior. Defaults to [`Hidden`] when wrap is
      [`Wrap`], [`Visible`] otherwise. Set explicitly to override smart
      defaults.
    @param overflow_y
      Controls vertical overflow behavior. Defaults to [`Visible`].
    @param content The string to display. *)

val scroll_view :
  ?width:dimension ->
  ?height:dimension ->
  ?min_width:dimension ->
  ?min_height:dimension ->
  ?max_width:dimension ->
  ?max_height:dimension ->
  ?padding:length_percentage sides ->
  ?margin:length_percentage_auto sides ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?align_self:align_self ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  ?show_scrollbars:bool ->
  h_offset:int ->
  v_offset:int ->
  element ->
  element
(** A container that provides a scrollable viewport for a single, larger child
    element.

    The application is responsible for storing the scroll offsets in its model.
    This keeps the UI fully state-driven.

    @param show_scrollbars
      If true (the default), visual scrollbars will be drawn inside the
      container's border/padding area.
    @param h_offset The current horizontal scroll position (column offset).
    @param v_offset The current vertical scroll position (row offset).
    @param child The single element to be displayed within the scroll view. *)

val center : element -> element
(** A container that centers its child both horizontally and vertically. *)

val styled : Style.t -> element -> element
(** Apply a style to an existing UI element. The style will be applied to a
    wrapper box around the element. *)

val with_key : Attr.key -> element -> element
(** Attach a stable identity key to an element. This key is used by the runtime
    for event routing, focus management, and other identity-based operations.
    Keys should be unique within their parent context. *)

val flow :
  ?h_gap:length_percentage ->
  ?v_gap:length_percentage ->
  ?overflow_x:overflow ->
  ?overflow_y:overflow ->
  element list ->
  element
(** [flow ?h_gap ?v_gap ?overflow_x ?overflow_y children] creates a flow layout
    that wraps children horizontally to new lines when they don't fit in the
    available width. Similar to how inline elements work in HTML.

    Defaults to [overflow_x:`Hidden] and [overflow_y:`Hidden] to ensure the flow
    container can shrink to fit within its parent, allowing proper wrapping
    behavior.

    @param h_gap Horizontal spacing between items (default: 0)
    @param v_gap Vertical spacing between lines (default: 0)
    @param overflow_x
      Horizontal overflow (default: [`Hidden] for proper shrinking)
    @param overflow_y
      Vertical overflow (default: [`Hidden] for proper shrinking) *)

val block :
  ?width:dimension ->
  ?height:dimension ->
  ?min_width:dimension ->
  ?min_height:dimension ->
  ?max_width:dimension ->
  ?max_height:dimension ->
  ?padding:length_percentage sides ->
  ?margin:length_percentage_auto sides ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  element list ->
  element
(** [block children] creates a block layout where each child takes the full
    width of its container and children are stacked vertically. This is the
    traditional block-level element behavior. *)

val grid :
  ?template_columns:grid_template_component list ->
  ?template_rows:grid_template_component list ->
  ?auto_columns:track_sizing_function list ->
  ?auto_rows:track_sizing_function list ->
  ?auto_flow:grid_auto_flow ->
  ?template_areas:grid_template_area list ->
  ?column_names:string list list ->
  ?row_names:string list list ->
  ?col_gap:length_percentage ->
  ?row_gap:length_percentage ->
  element list ->
  element
(** Creates a CSS Grid layout with full control over track sizing and placement.

    @param template_columns Column track definitions (use Single or Repeat).
    @param template_rows Row track definitions.
    @param auto_columns Sizing for implicit columns.
    @param auto_rows Sizing for implicit rows.
    @param auto_flow
      How auto-placed items flow (Row, Column, with Dense option).
    @param template_areas Named grid areas for semantic placement.
    @param column_names Names for column lines.
    @param row_names Names for row lines.
    @param col_gap Spacing between columns.
    @param row_gap Spacing between rows.
    @param children Elements to place in the grid. *)

val grid_item :
  ?row:grid_placement span -> ?column:grid_placement span -> element -> element
(** Wraps an element with grid placement information.

    @param row Row placement (start and end lines).
    @param column Column placement (start and end lines).
    @param child The element to place in the grid. *)

val checkbox : checked:bool -> label:string -> ?style:Style.t -> unit -> element
(** [checkbox ~checked ~label ?style ()] creates a checkbox UI element.

    @param checked Whether the checkbox is checked
    @param label The text label to display next to the checkbox
    @param style Optional style to apply to the checkbox *)

val radio : checked:bool -> label:string -> ?style:Style.t -> unit -> element
(** [radio ~checked ~label ?style ()] creates a radio button UI element.

    @param checked Whether the radio button is selected
    @param label The text label to display next to the radio button
    @param style Optional style to apply to the radio button *)

val image :
  lines:string list -> ?align:[ `Left | `Center | `Right ] -> unit -> element
(** [image ~lines ?align ()] creates an image element from ASCII art lines.

    @param lines List of strings representing each line of the ASCII art
    @param align Horizontal alignment of the image lines *)

val list :
  items:element list -> ?bullet:string -> ?numbering:bool -> unit -> element
(** [list ~items ?bullet ?numbering ()] creates a bulleted or numbered list.

    @param items List of UI elements to display as list items
    @param bullet Custom bullet character (default: "•")
    @param numbering If true, use numbers instead of bullets *)

val rich_text : (string * Style.t) list -> element
(** [rich_text segments] creates a single line of text with multiple styled
    segments. Each segment is a pair of text content and its associated style.
*)

val panel :
  ?box_style:Border.line_style ->
  ?title:string ->
  ?title_align:[ `Left | `Center | `Right ] ->
  ?subtitle:string ->
  ?subtitle_align:[ `Left | `Center | `Right ] ->
  ?expand:bool ->
  ?style:Style.t ->
  ?border_style:Style.t ->
  ?width:int ->
  ?height:int ->
  ?padding:length_percentage sides ->
  ?highlight:bool ->
  element ->
  element
(** Create a panel with a border around its contents.

    @param child The content to display inside the panel
    @param box_style The border style (default: Rounded)
    @param title Optional title displayed in panel header
    @param title_align Alignment of title (default: Center)
    @param subtitle Optional subtitle displayed in panel footer
    @param subtitle_align Alignment of subtitle (default: Center)
    @param expand
      If true, panel stretches to fill available width (default: false)
    @param style The style of the panel contents
    @param border_style The style of the border
    @param width Optional fixed width of panel
    @param height Optional fixed height of panel
    @param padding Padding around content (top, right, bottom, left)
    @param highlight Enable automatic highlighting of title/subtitle if string
*)

(** {1 String utilities} *)

val measure_string : string -> int
(** Returns the display width of a string, accounting for UTF-8 characters. *)

val truncate_string_with_ellipsis : string -> int -> string -> string
(** [truncate_string_with_ellipsis str max_width suffix] truncates [str] to fit
    within [max_width] characters, appending [suffix] (typically "...") if
    truncation occurs. *)

val pad_string : string -> int -> string
(** [pad_string str width] pads [str] with spaces to reach [width] characters.
*)

val measure : ?width:int -> element -> int * int
(** [measure ?width element] computes the natural dimension of an element.
    @param width Optional width constraint for the layout calculation
    @return A pair (width, height) representing the element's dimension *)

(** {1 Extended UI Components} *)

(** {3 Canvas} *)

(** Canvas module for primitive drawing operations. *)
module Canvas : sig
  type t

  val plot : t -> x:int -> y:int -> ?style:Style.t -> string -> unit

  val draw_line :
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    ?style:Style.t ->
    ?kind:[ `Line | `Braille ] ->
    t ->
    unit

  val draw_box :
    x:int ->
    y:int ->
    width:int ->
    height:int ->
    ?style:Style.t ->
    ?border:Border.t ->
    t ->
    unit

  val create :
    ?width:dimension ->
    ?height:dimension ->
    ?min_width:dimension ->
    ?min_height:dimension ->
    ?max_width:dimension ->
    ?max_height:dimension ->
    ?padding:length_percentage sides ->
    ?margin:length_percentage_auto sides ->
    ?flex_grow:float ->
    ?flex_shrink:float ->
    ?align_self:align_self ->
    ?style:Style.t ->
    ?border:Border.t ->
    ?border_style:Style.t ->
    (width:int -> height:int -> t -> unit) ->
    element
end

val canvas :
  ?width:dimension ->
  ?height:dimension ->
  ?min_width:dimension ->
  ?min_height:dimension ->
  ?max_width:dimension ->
  ?max_height:dimension ->
  ?padding:length_percentage sides ->
  ?margin:length_percentage_auto sides ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?align_self:align_self ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  (width:int ->
  height:int ->
  (x:int -> y:int -> ?style:Style.t -> string -> unit) ->
  unit) ->
  element
(** A self-contained element for custom, imperative drawing.

    It combines the layout properties of a [box] with a low-level drawing
    function. This is the ideal escape hatch for custom visualizations like
    charts, sparklines, or game boards.

    While its size can be determined by flexbox (e.g., using [flex_grow]), it's
    often best to provide an explicit size like [width:(`Cells 40)] for
    predictable results.

    @param draw
      A function that receives the final, calculated [width] and [height] of the
      canvas's content area (inside any padding or border) and a [plot] function
      to draw with. This allows your drawing logic to be responsive to the
      canvas's actual size. *)

module Progress_bar : sig
  (** Progress bar rendering with animation and rich customization.

      This module provides a customizable progress bar similar to Python's Rich
      library, with support for determinate progress, indeterminate pulsing
      animations, and various styling options. *)

  type preset_def = {
    delimiters : (string * string) option;
    filled_char : string;
    empty_char : string;
    progress_stages : string list;
  }
  (** A record defining the characters used to render a progress bar. *)

  (** A collection of pre-defined visual styles for the progress bar. *)
  type preset =
    (* Classic Styles *)
    | ASCII  (** A standard ASCII bar: `[###---]` *)
    | UTF8  (** A high-resolution UTF-8 block bar: `│███▋ │` *)
    (* Line Styles *)
    | Line_double  (** Double-line characters: `╢═══───╟` *)
    | Line_single  (** Single-line characters: `├━━━───┤` *)
    | Line_arrow  (** Arrowhead progress: `>>--->` *)
    (* Block Styles *)
    | Block_shade_light  (** Light shading blocks: `▓▓▓░░░` *)
    | Block_shade_medium  (** Medium shading blocks: `███▒▒▒` *)
    | Block_shade_dark  (** Dark shading blocks: `███▓▓▓` *)
    | Block_dotted  (** Dotted blocks: `⣿⣿⣿⣀⣀⣀` *)
    (* Custom record for full control *)
    | Custom of preset_def

  val progress_bar :
    ?total:float ->
    ?completed:float ->
    ?width:int ->
    ?pulse:bool ->
    ?animation_time:float ->
    ?bar_style:Style.t ->
    ?complete_style:Style.t ->
    ?finished_style:Style.t ->
    ?pulse_style:Style.t ->
    ?preset:preset ->
    ?delimiters:string * string ->
    ?filled_char:string ->
    ?empty_char:string ->
    ?progress_stages:string list ->
    unit ->
    element
  (** [progress_bar ?total ... ()] creates a progress bar element.

      @param total
        The total number of steps. Defaults to `Some 100.0`. Set to `None` for a
        pulsing bar.
      @param completed The number of steps completed. Defaults to `0.0`.
      @param width The total width of the bar in characters. Defaults to 20.
      @param pulse
        If true, force a pulsing animation even if `total` is defined. Defaults
        to `false`.
      @param animation_time
        Time in seconds, used for the pulsing animation. Defaults to `0.0`.

      @param bar_style Style for the empty part of the bar.
      @param complete_style Style for the filled part of the bar.
      @param finished_style
        Style for the bar once it is complete. Overrides `complete_style`.
      @param pulse_style Style for the bright part of the pulsing animation.

      @param preset
        A style preset for the bar's characters. If not provided, a default
        heavy-line style (`━━━━━━━━╸───`) is used. Specific character arguments
        below will override the chosen preset or the default.
      @param delimiters
        A `(left, right)` tuple of strings for the bar's ends. E.g., `Some
        ("[", "]")`.
      @param filled_char
        The character for a fully filled segment. E.g., `"#"` or `"█"`.
      @param empty_char
        The character for an empty segment. E.g., `"-"` or `" "`.
      @param progress_stages
        A list of characters for sub-character progress, from least to most
        filled. E.g., `[">"]` or `["▏", "▎", "▍", "▌", "▋", "▊", "▉"]`. *)
end

(** Spinner animations for terminal UI.

    Spinners are from cli-spinners: MIT License Copyright (c) Sindre Sorhus
    <sindresorhus@gmail.com> (sindresorhus.com) *)
module Spinner : sig
  type spinner_kind =
    (* Braille patterns *)
    | Braille_dots  (** Classic rotating braille dots: ⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏ *)
    | Braille_dots2  (** Alternative braille dots: ⣾⣽⣻⢿⡿⣟⣯⣷ *)
    | Braille_dots3  (** Third braille variation: ⠋⠙⠚⠞⠖⠦⠴⠲⠳⠓ *)
    | Braille_dots4  (** Bouncing braille dots *)
    | Braille_dots5  (** Complex braille cycle *)
    | Braille_dots6  (** Extended braille animation *)
    | Braille_dots7  (** Another braille pattern *)
    | Braille_dots8  (** Long braille sequence *)
    | Braille_dots9  (** Vertical braille: ⢹⢺⢼⣸⣇⡧⡗⡏ *)
    | Braille_dots10  (** Small braille set: ⢄⢂⢁⡁⡈⡐⡠ *)
    | Braille_dots11  (** Simple braille cycle *)
    | Braille_dots12  (** Two-column braille animation *)
    | Braille_dots13  (** Dense braille: ⣼⣹⢻⠿⡟⣏⣧⣶ *)
    | Braille_dots14  (** Two-character braille patterns *)
    | Braille_8bit  (** Full 8-bit braille character set *)
    | Braille_circle  (** Circular braille motion *)
    | Braille_sand  (** Sand falling effect with braille *)
    (* Line and ASCII *)
    | Line_spin  (** Simple line rotation: - \ | / *)
    | Line_pulse  (** Pulsing line: ⠂-–—–- *)
    | Pipe_spin  (** Box drawing rotation: ┤┘┴└├┌┬┐ *)
    | Ascii_dots  (** Simple ASCII dots: . .. ... *)
    | Ascii_dots_scroll  (** Scrolling ASCII dots *)
    | Ascii_star  (** ASCII star rotation: + x * *)
    | Ascii_flip  (** Flipping characters: _ - ` ' ´ *)
    | Ascii_hamburger  (** Hamburger menu: ☱☲☴ *)
    | Ascii_binary  (** Binary numbers animation *)
    | Ascii_dqpb  (** Letters d q p b rotation *)
    (* Bars and blocks *)
    | Bar_vertical_grow  (** Growing vertical bar: ▁▃▄▅▆▇ *)
    | Bar_horizontal_grow  (** Growing horizontal bar: ▏▎▍▌▋▊▉ *)
    | Bar_bounce  (** Bouncing bar: [=   ] [==  ] *)
    | Block_bounce  (** Bouncing block corners: ▖▘▝▗ *)
    | Block_wave  (** Wave with blocks: ▌▀▐▄ *)
    | Block_square  (** Square toggle: □■ *)
    | Block_squish  (** Squishing blocks: ╫╪ *)
    (* Geometric shapes *)
    | Triangle_spin  (** Rotating triangle: ◢◣◤◥ *)
    | Square_corners  (** Rotating square corners: ◰◳◲◱ *)
    | Circle_quarters  (** Circle quarters: ◴◷◶◵ *)
    | Circle_halves  (** Circle halves: ◐◓◑◒ *)
    | Circle_simple  (** Simple circle: ◡⊙◠ *)
    | Arc_spin  (** Arc rotation: ◜◠◝◞◡◟ *)
    (* Progress indicators *)
    | Progress_bar  (** Material design progress bar *)
    | Progress_balloon  (** Balloon expansion: . o O @ * *)
    | Progress_balloon2  (** Alternative balloon: . o O ° *)
    | Progress_layer  (** Layered progress: - = ≡ *)
    | Progress_point  (** Moving point: ∙∙∙ ●∙∙ ∙●∙ *)
    | Progress_beta_wave  (** Beta wave: ρββββββ *)
    (* Animations *)
    | Anim_pong  (** Pong game animation *)
    | Anim_shark  (** Swimming shark *)
    | Anim_grenade  (** Exploding grenade *)
    | Anim_ball_bounce  (** Bouncing ball in parentheses *)
    | Anim_aesthetic  (** Aesthetic wave: ▰▱▱▱▱▱▱ *)
    | Anim_dwarf_fortress  (** Dwarf Fortress style animation *)
    (* Noise and effects *)
    | Noise_fade  (** Fading noise: ▓▒░ *)
    | Effect_dots_bounce  (** Simple bouncing dots: ⠁⠂⠄⠂ *)
    (* Toggle animations *)
    | Toggle_box  (** Box toggle: ▫▪ *)
    | Toggle_box2  (** Alternative box: ⊶⊷ *)
    | Toggle_square  (** Square toggle: □■ *)
    | Toggle_square2  (** Multi-state square: ■□▪▫ *)
    | Toggle_square3  (** Bold square: ▮▯ *)
    | Toggle_circle  (** Circle toggle: ဝ၀ *)
    | Toggle_circle2  (** Filled circles: ⦾⦿ *)
    | Toggle_circle3  (** Dotted circles: ◍◌ *)
    | Toggle_circle4  (** Bold circles: ◉◎ *)
    | Toggle_circle5  (** Numbered circles: ㊂㊀㊁ *)
    | Toggle_diamond  (** Diamond toggle: ⧇⧆ *)
    | Toggle_shogi  (** Shogi pieces: ☗☖ *)
    | Toggle_equals  (** Equals toggle: = * - *)
    (* Arrows *)
    | Arrow_rotate  (** Rotating arrows: ←↖↑↗→↘↓↙ *)
    | Arrow_rotate2  (** Emoji arrows: ⬆️↗️➡️↘️⬇️↙️⬅️↖️ *)
    | Arrow_progress  (** Arrow progress: ▹▹▹▹▹ ▸▹▹▹▹ *)
    (* Unicode and emoji *)
    | Unicode_star_pulse  (** Star pulse: ✶✸✹✺✹✷ *)
    | Unicode_moon_phases  (** Moon phases: 🌑🌒🌓🌔🌕🌖🌗🌘 *)
    | Unicode_earth_rotate  (** Earth rotation: 🌍🌎🌏 *)
    | Unicode_clock  (** Clock faces: 🕛🕐🕑... *)
    | Unicode_weather  (** Weather cycle: ☀️🌤⛅️🌥☁️🌧 *)
    (* Emoji animations *)
    | Emoji_hearts  (** Colored hearts: 💛💙💜💚❤️ *)
    | Emoji_monkey  (** See/hear/speak no evil: 🙈🙉🙊 *)
    | Emoji_faces  (** Face expressions: 😄😝 *)
    | Emoji_runner  (** Running figure: 🚶🏃 *)
    | Emoji_christmas  (** Christmas trees: 🌲🎄 *)
    | Emoji_finger_dance  (** Hand gestures: 🤘🤟🖖✋🤚👆 *)
    | Emoji_fist_bump  (** Fist bump animation *)
    | Emoji_soccer  (** Soccer header animation *)
    | Emoji_mindblown  (** Mind blown sequence *)
    | Emoji_speaker  (** Speaker volume: 🔈🔉🔊 *)
    (* Pulse animations *)
    | Pulse_orange  (** Orange pulse: 🔸🔶🟠 *)
    | Pulse_blue  (** Blue pulse: 🔹🔷🔵 *)
    | Pulse_orange_blue  (** Combined orange/blue pulse *)
    (* Special *)
    | Time_travel  (** Clock time travel effect *)
    (* Custom *)
    | Custom of { frames : string list; interval : int }
        (** Custom animation frames *)

  val spinner : ?speed:float -> ?time:float -> spinner_kind -> element
end

module Table : sig
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
    element
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

  val simple_table : headers:string list -> rows:string list list -> element
  (** Simple table with just headers and rows *)

  val grid_table : columns:column list -> rows:string list list -> element
  (** Grid-style table (no borders, minimal padding) *)
end

module Tree : sig
  type guide_style =
    | Normal  (** Standard lines: ├── └── │ *)
    | ASCII  (** ASCII-safe: +-- `-- | *)
    | Bold  (** Bold lines: ┣━━ ┗━━ ┃ *)
    | Double  (** Double lines: ╠══ ╚══ ║ *)

  type node = {
    label : element;
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
    element
  (** [tree ?style ?guide_style ?guides ?hide_root ?expanded node] creates a
      tree view display.
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
end

val progress_bar :
  ?total:float ->
  ?completed:float ->
  ?width:int ->
  ?pulse:bool ->
  ?animation_time:float ->
  ?bar_style:Style.t ->
  ?complete_style:Style.t ->
  ?finished_style:Style.t ->
  ?pulse_style:Style.t ->
  ?preset:Progress_bar.preset ->
  ?delimiters:string * string ->
  ?filled_char:string ->
  ?empty_char:string ->
  ?progress_stages:string list ->
  unit ->
  element
(** Creates a progress bar with extensive customization options *)

val spinner : ?speed:float -> ?time:float -> Spinner.spinner_kind -> element
(** Creates an animated spinner with predefined animation styles *)

val table :
  ?title:string option ->
  ?caption:string option ->
  ?columns:Table.column list ->
  ?rows:string list list ->
  ?box_style:Table.box_style ->
  ?safe_box:bool ->
  ?padding:Table.padding ->
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
  ?title_justify:Table.justify ->
  ?caption_justify:Table.justify ->
  ?width:int option ->
  ?min_width:int option ->
  unit ->
  element
(** Creates a table with customizable layout and styling *)

val tree :
  ?style:Style.t ->
  ?guide_style:Style.t ->
  ?guides:Tree.guide_style ->
  ?hide_root:bool ->
  ?expanded:bool ->
  Tree.node ->
  element
(** Creates a tree view with expandable nodes *)

(** {1 Rendering} *)

val render :
  ?dark:bool ->
  ?theme:Theme.t ->
  ?calc:calc_resolver ->
  ?snapshot:Layout_snapshot.t ->
  Screen.t ->
  element ->
  unit
(** Renders a UI tree to an in-memory [Screen.t] buffer.

    @param dark Toggles between light and dark mode for adaptive styles.
    @param theme An optional theme to apply to the UI.
    @param calc Optional calc resolver for calc() expressions.
    @param snapshot Optional layout snapshot to populate with element positions.
    @param screen The target screen buffer to render to.
    @param ui The root UI element to render. *)

val render_string :
  ?width:int ->
  ?height:int ->
  ?dark:bool ->
  ?theme:Theme.t ->
  ?calc:calc_resolver ->
  element ->
  string
(** Renders a UI element to a string, suitable for terminal output.

    @param width The width of the virtual screen to create.
    @param height
      The height of the virtual screen. If not provided, it will be auto-sized
      based on the content's measured height.
    @param dark Toggles between light and dark mode for adaptive styles.
    @param theme An optional theme to apply to the UI.
    @param calc Optional calc resolver for calc() expressions.
    @param element The UI element to render. *)

val print :
  ?width:int ->
  ?height:int ->
  ?dark:bool ->
  ?theme:Theme.t ->
  ?calc:calc_resolver ->
  element ->
  unit
(** A convenience function that creates a screen, renders the UI, and prints the
    result to standard output.

    @param width The width of the virtual screen to create.
    @param height
      The height of the virtual screen. If not provided, it will be auto-sized
      based on the content's measured height.
    @param dark Toggles between light and dark mode for adaptive styles.
    @param theme An optional theme to apply to the UI.
    @param calc Optional calc resolver for calc() expressions. *)
