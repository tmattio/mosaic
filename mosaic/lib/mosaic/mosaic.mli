(** TEA (The Elm Architecture) runtime for terminal UIs.

    Mosaic implements the Model-View-Update loop: the application maintains an
    immutable {e model}, produces a {!type-t} describing the UI from that model,
    and reacts to events by running an {e update} function that returns a new
    model together with optional {!Cmd} side-effects. Subscriptions ({!Sub}) let
    the application express ongoing event interests such as timers, key presses,
    and terminal resize events.

    - {{!geometry}Geometry types} and {{!dim_helpers}dimension helpers}
    - {{!layout_enums}Layout enums} and the {{!grid}Grid module}
    - {{!widget_types}Widget types}
    - {{!elements}UI elements}
    - {{!views}Views}, {{!commands}Commands}, and
      {{!subscriptions}Subscriptions}
    - {{!application}Application} and {{!running}Running}

    The entry point is {!val-run}. *)

open Mosaic_ui

(** {1:re_exports Re-exports} *)

module Ansi = Matrix.Ansi
(** Terminal ANSI escape sequences: colors ({!Ansi.Color}), styles
    ({!Ansi.Style}), and attributes ({!Ansi.Attr}). *)

module Border = Matrix.Grid.Border
(** Border character sets for box-drawing. Provides {!Border.single},
    {!Border.rounded}, {!Border.heavy}, {!Border.double}, and {!Border.ascii}.
*)

module Editor = Mosaic_ui.Editor
(** Persistent text, caret, selection, undo and viewport state. *)

module Scroll = Mosaic_ui.Scroll
(** Persistent viewport position and following intent. *)

module Event = Mosaic_ui.Event
(** Input event types for keyboard, mouse, and paste events. *)

module Focus : sig
  (** Pure traversal of application-owned focus identifiers. *)

  val next : equal:('a -> 'a -> bool) -> 'a list -> 'a option -> 'a option
  (** [next ~equal order current] is the identifier after [current] in [order],
      wrapping to the first identifier. An absent or unknown [current] selects
      the first identifier; an empty [order] returns [None]. Identifiers should
      be distinct under [equal]. No widget focus is changed. *)

  val prev : equal:('a -> 'a -> bool) -> 'a list -> 'a option -> 'a option
  (** [prev ~equal order current] traverses in reverse, wrapping to the last
      identifier. An absent or unknown [current] selects the last identifier; an
      empty [order] returns [None]. *)
end

module Shortcut = Mosaic_ui.Shortcut
(** Typed keyboard shortcuts shared by widgets and subscriptions. *)

module Canvas : sig
  (** Mutable cell-level drawing surface.

      A canvas is passed to the [~on_draw] callback of the {!val-canvas}
      element. Use the drawing functions to render content into the canvas grid.
  *)

  type t = Mosaic_ui.Canvas.t
  (** The type for canvases. *)

  (** {1:canvas_dimensions Dimensions} *)

  val width : t -> int
  (** [width t] is the grid width of [t] in cells. *)

  val height : t -> int
  (** [height t] is the grid height of [t] in cells. *)

  (** {1:drawing Drawing} *)

  val draw_text :
    ?style:Ansi.Style.t ->
    ?tab_width:int ->
    t ->
    x:int ->
    y:int ->
    text:string ->
    unit
  (** [draw_text t ~x ~y ~text] draws [text] as a single line at column [x], row
      [y]. Defaults are those of {!Matrix.Grid.draw_text}. *)

  val fill_rect :
    t -> x:int -> y:int -> width:int -> height:int -> color:Ansi.Color.t -> unit
  (** [fill_rect t ~x ~y ~width ~height ~color] fills the rectangle with the
      given background [color]. *)

  val draw_box :
    t ->
    x:int ->
    y:int ->
    width:int ->
    height:int ->
    ?border:Border.t ->
    ?sides:Border.side list ->
    ?style:Ansi.Style.t ->
    ?fill:Ansi.Color.t ->
    ?title:string ->
    ?title_alignment:[ `Left | `Center | `Right ] ->
    ?title_style:Ansi.Style.t ->
    unit ->
    unit
  (** [draw_box t ~x ~y ~width ~height ()] draws a Unicode box at column [x],
      row [y]. Defaults are those of {!Matrix.Grid.draw_box}. *)

  val draw_line :
    t ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    ?style:Ansi.Style.t ->
    ?symbols:Matrix.Grid.line_symbols ->
    ?kind:[ `Line | `Braille ] ->
    unit ->
    unit
  (** [draw_line t ~x1 ~y1 ~x2 ~y2 ()] draws a line from [(x1, y1)] to
      [(x2, y2)].
      - [style] defaults to {!Ansi.Style.default}.
      - [symbols] defaults to standard box-drawing characters.
      - [kind] defaults to [`Line]. Use [`Braille] for Braille dot patterns. *)

  val set_uchar :
    t ->
    x:int ->
    y:int ->
    uchar:Uchar.t ->
    fg:Ansi.Color.t ->
    bg:Ansi.Color.t ->
    attrs:Ansi.Attr.t ->
    ?link:string ->
    ?blend:bool ->
    unit ->
    unit
  (** [set_uchar t ~x ~y ~uchar:cell ~fg ~bg ~attrs ()] writes a single cell.

      [link] defaults to no hyperlink. [blend] defaults to [false]. *)

  val clear : ?color:Ansi.Color.t -> t -> unit
  (** [clear t] clears the canvas grid and schedules a re-render.

      [color] defaults to the terminal default background. *)

  (** {1:grid Low-level grid access} *)

  val grid : t -> Matrix.Grid.t
  (** [grid t] is the underlying grid for [t]. Use this for operations not
      covered by the canvas drawing functions. *)

  (** {1:callbacks Callbacks} *)

  val set_on_draw : t -> (t -> delta:float -> unit) option -> unit
  (** [set_on_draw t cb] registers [cb] as the render-time drawing callback for
      [t]. [cb] fires each render pass after the grid has been auto-resized;
      [~delta] is the elapsed time in milliseconds since the last frame. Pass
      [None] to clear the callback. *)

  val set_on_resize : t -> (t -> unit) option -> unit
  (** [set_on_resize t cb] registers [cb] as the resize callback for [t]. [cb]
      fires when the canvas grid dimensions change. Pass [None] to clear the
      callback. *)

  (** {1:render_control Render control} *)

  val request_render : t -> unit
  (** [request_render t] schedules a re-render of [t]. Call this after drawing
      outside of a draw callback. *)
end

(** {1:geometry Geometry types} *)

type 'a size = 'a Toffee.Geometry.Size.t = { width : 'a; height : 'a }
(** The type for 2-dimensional sizes. See {!val-size} and {!val-size_wh}. *)

type 'a rect = 'a Toffee.Geometry.Rect.t = {
  left : 'a;
  right : 'a;
  top : 'a;
  bottom : 'a;
}
(** The type for axis-aligned rectangles with four edges. See {!val-padding},
    {!val-margin}, and {!val-inset}. *)

type 'a point = 'a Toffee.Geometry.Point.t = { x : 'a; y : 'a }
(** The type for 2-dimensional points. *)

type 'a line = 'a Toffee.Geometry.Line.t = { start : 'a; end_ : 'a }
(** The type for line segments with start and end values. Used by
    {!Grid.line_range} and {!Grid.span_range} for grid placement. *)

type dimension = Toffee.Style.Dimension.t
(** The type for CSS dimensions: a fixed length, a percentage, or auto. Create
    values with {!val-px}, {!val-pct}, or {!val-auto}. *)

type length_percentage = Toffee.Style.Length_percentage.t
(** The type for CSS length-or-percentage values. Produced by helpers such as
    {!val-padding}, {!val-gap}, and {!val-gap_xy}. *)

type length_percentage_auto = Toffee.Style.Length_percentage_auto.t
(** The type for CSS length-or-percentage-or-auto values. Produced by helpers
    such as {!val-margin}, {!val-margin_xy}, and {!val-inset}. *)

type span = Mosaic_ui.Text_buffer.span = { text : string; style : Ansi.Style.t }
(** A contiguous run of text with a single visual style. Used by editable text
    widgets that accept pre-styled content. *)

(** {1:layout_enums Layout enum modules} *)

(** CSS display property controlling box generation and child layout algorithm.
*)
module Display : sig
  type t = Toffee.Style.Display.t =
    | Block  (** Children follow the block layout algorithm. *)
    | Flex  (** Children follow the flexbox layout algorithm. *)
    | Grid  (** Children follow the CSS grid layout algorithm. *)
    | None  (** Node is hidden and generates no boxes. *)
end

(** CSS position property controlling normal flow participation. *)
module Position : sig
  type t = Toffee.Style.Position.t =
    | Relative
        (** The offset is computed relative to the final position given by the
            layout algorithm. *)
    | Absolute
        (** The node is taken out of the normal flow and positioned relative to
            its parent. *)
end

(** CSS box-sizing property. *)
module Box_sizing : sig
  type t = Toffee.Style.Box_sizing.t =
    | Border_box  (** Size styles specify the box's border box. *)
    | Content_box  (** Size styles specify the box's content box. *)
end

(** CSS overflow property. *)
module Overflow : sig
  type t = Toffee.Style.Overflow.t =
    | Visible  (** Overflowing content is displayed beyond the element. *)
    | Clip  (** Overflowing content is clipped at the element boundary. *)
    | Hidden  (** Overflowing content is hidden; no scrolling. *)
    | Scroll  (** Overflowing content is scrollable. *)
end

(** CSS text-align property for block layout. *)
module Text_align : sig
  type t = Toffee.Style.Text_align.t =
    | Auto  (** No special text alignment behavior. *)
    | Legacy_left  (** Left alignment. *)
    | Legacy_right  (** Right alignment. *)
    | Legacy_center  (** Center alignment. *)
end

(** CSS flex-direction property. *)
module Flex_direction : sig
  type t = Toffee.Style.Flex_direction.t =
    | Row  (** Main axis is horizontal, left to right. *)
    | Column  (** Main axis is vertical, top to bottom. *)
    | Row_reverse  (** Main axis is horizontal, right to left. *)
    | Column_reverse  (** Main axis is vertical, bottom to top. *)
end

(** CSS flex-wrap property. *)
module Flex_wrap : sig
  type t = Toffee.Style.Flex_wrap.t =
    | No_wrap  (** Items stay on a single line. *)
    | Wrap  (** Items wrap to multiple lines as needed. *)
    | Wrap_reverse  (** Items wrap to multiple lines in reverse direction. *)
end

(** Alignment along the cross/block axis (align-items, align-self,
    justify-items, justify-self). *)
module Align : sig
  type t = Toffee.Style.Align_items.t =
    | Start  (** Pack items toward the start of the axis. *)
    | End  (** Pack items toward the end of the axis. *)
    | Flex_start  (** Pack items toward the flex-relative start. *)
    | Flex_end  (** Pack items toward the flex-relative end. *)
    | Center  (** Pack items along the center of the axis. *)
    | Baseline  (** Align items such that their baselines align. *)
    | Stretch  (** Stretch items to fill the container. *)
end

(** Distribution of space between and around content items (align-content,
    justify-content). *)
module Justify : sig
  type t = Toffee.Style.Align_content.t =
    | Start  (** Pack items toward the start of the axis. *)
    | End  (** Pack items toward the end of the axis. *)
    | Flex_start  (** Pack items toward the flex-relative start. *)
    | Flex_end  (** Pack items toward the flex-relative end. *)
    | Center  (** Center items around the middle of the axis. *)
    | Stretch  (** Stretch items to fill the container. *)
    | Space_between  (** Distribute items evenly, flush with edges. *)
    | Space_evenly  (** Distribute items evenly with equal edge spacing. *)
    | Space_around  (** Distribute items evenly with half-size edge gaps. *)
end

(** CSS grid-auto-flow property. *)
module Grid_auto_flow : sig
  type t = Toffee.Style.Grid_auto_flow.t =
    | Row  (** Place items by filling each row in turn. *)
    | Column  (** Place items by filling each column in turn. *)
    | Row_dense  (** Fill rows first using the dense packing algorithm. *)
    | Column_dense  (** Fill columns first using the dense packing algorithm. *)
end

(** {1:grid Grid module} *)

(** Grid layout constructors for defining tracks and placing items.

    {[
      box ~display:Display.Grid
        ~grid_template_columns:[ Grid.length 20.; Grid.fr 1.; Grid.fr 1. ]
        ~grid_template_rows:[ Grid.length 3.; Grid.fr 1.; Grid.length 3. ]
        ~grid_row:(Grid.line_range 1 2) ~grid_column:(Grid.line_range 1 4)
        [...]
    ]} *)
module Grid : sig
  (** {2 Grid template components}

      Values for [~grid_template_columns] and [~grid_template_rows]. *)

  type template = Toffee.Style.grid_template_component
  (** A grid template component (single track or repeat clause). *)

  val fr : float -> template
  (** [fr n] is a flexible track taking [n] fractional units of remaining space.
  *)

  val length : float -> template
  (** [length n] is a fixed-width track of [n] cells. *)

  val percent : float -> template
  (** [percent n] is a percentage-width track (0.0 to 1.0 range). *)

  val auto : template
  (** [auto] is an auto-sized track. *)

  val min_content : template
  (** [min_content] is a track sized to the minimum content width. *)

  val max_content : template
  (** [max_content] is a track sized to the maximum content width. *)

  val fit_content : Toffee.Style.Compact_length.t -> template
  (** [fit_content limit] is a track clamped between min-content and [limit]. *)

  val minmax :
    min:Toffee.Style.Compact_length.t ->
    max:Toffee.Style.Compact_length.t ->
    template
  (** [minmax ~min ~max] is a track that sizes between [min] and [max]. *)

  (** {2 Grid placement}

      Values for [~grid_row] and [~grid_column]. *)

  type placement = Toffee.Style.grid_placement
  (** A grid placement specification for a single axis endpoint. *)

  val line : int -> placement
  (** [line n] places at grid line [n] (1-indexed, negative from end). *)

  val span : int -> placement
  (** [span n] spans [n] tracks from the opposite endpoint. *)

  val auto_placement : placement
  (** [auto_placement] uses the auto-placement algorithm. *)

  val line_range : int -> int -> placement line
  (** [line_range s e] is a placement from line [s] to line [e]. Shorthand for
      [{ start = line s; end_ = line e }]. *)

  val span_range : int -> int -> placement line
  (** [span_range s n] places starting at line [s] and spanning [n] tracks.
      Shorthand for [{ start = line s; end_ = span n }]. *)

  (** {2 Track sizing functions}

      Values for [~grid_auto_rows] and [~grid_auto_columns]. *)

  type track = Toffee.Style.track_sizing_function
  (** A track sizing function for auto rows/columns. *)

  val track_fr : float -> track
  (** [track_fr n] is a flexible track of [n] fractional units. *)

  val track_length : float -> track
  (** [track_length n] is a fixed-length track of [n] cells. *)

  val track_percent : float -> track
  (** [track_percent n] is a percentage-width track (0.0 to 1.0 range). *)

  val track_auto : track
  (** [track_auto] is an auto-sized track. *)

  val track_min_content : track
  (** [track_min_content] is a track sized to the minimum content width. *)

  val track_max_content : track
  (** [track_max_content] is a track sized to the maximum content width. *)

  (** {2 Grid template areas} *)

  type area = Toffee.Style.grid_template_area
  (** A named grid template area. *)
end

(** {1:widget_types Widget types} *)

(** Companion types for the {!val-select} widget. *)
module Select : sig
  type action = Mosaic_ui.Select.action =
    | Move_up
    | Move_down
    | Move_up_fast
    | Move_down_fast
    | Activate  (** Keyboard navigation and activation actions. *)

  type 'a item
  (** A display label and optional description paired with an application value.
      Values must be distinct under the selection widget's [equal] function. *)

  val item : 'a -> ?description:string -> string -> 'a item
  (** [item value ?description label] displays [label] and reports [value] when
      selected or activated. [description] is absent by default. *)
end

(** Companion types for the {!val-tab_select} widget. *)
module Tab_select : sig
  type action = Mosaic_ui.Tab_select.action =
    | Move_left
    | Move_right
    | Activate  (** Keyboard navigation and activation actions. *)

  type 'a item
  (** A tab label and description paired with an application value. Values must
      be distinct under the selection widget's [equal] function. *)

  val item : 'a -> ?description:string -> string -> 'a item
  (** [item value ?description label] displays [label] and reports [value] when
      selected or activated. [description] defaults to the empty string. *)
end

(** Companion types for the {!val-table} widget. *)
module Table : sig
  type action = Mosaic_ui.Table.action =
    | Move_up
    | Move_down
    | Move_up_fast
    | Move_down_fast
    | Page_up
    | Page_down
    | Activate  (** Keyboard navigation and activation actions. *)

  type alignment = Mosaic_ui.Table.alignment
  (** Horizontal text alignment: [`Left], [`Right], or [`Center]. Defaults to
      [`Left]. *)

  type width = Mosaic_ui.Table.width
  (** Column width strategy: [`Auto], [`Fixed of int], or [`Fraction of float].
      Defaults to [`Auto]. *)

  type overflow = Mosaic_ui.Table.overflow
  (** Cell content overflow: [`Ellipsis] or [`Wrap]. Defaults to [`Ellipsis]. *)

  type column = Mosaic_ui.Table.column = {
    header : string;  (** Column header text. *)
    width : width;  (** Sizing strategy. *)
    alignment : alignment;  (** Text alignment. *)
    overflow : overflow;  (** Content overflow strategy. *)
    min_width : int option;  (** Minimum column width in cells. *)
    max_width : int option;  (** Maximum column width in cells. *)
  }
  (** The type for column definitions. Use {!val-column} to create values with
      sensible defaults. *)

  val column :
    ?width:width ->
    ?alignment:alignment ->
    ?overflow:overflow ->
    ?min_width:int ->
    ?max_width:int ->
    string ->
    column
  (** [column header] is a column definition with the given [header].

      [width] defaults to [`Auto]. [alignment] defaults to [`Left]. [overflow]
      defaults to [`Ellipsis]. *)

  type cell = Mosaic_ui.Table.cell
  (** The type for table cell content. *)

  type 'a row
  (** Table cells paired with an application value. Values must be distinct
      under the table's [equal] function. *)

  val cell : ?style:Ansi.Style.t -> string -> cell
  (** [cell s] is a plain-text cell containing [s].

      [style] defaults to {!Ansi.Style.default}. *)

  val rich : Mosaic_ui.Text.fragment list -> cell
  (** [rich fragments] is a styled cell built from a list of styled text
      fragments. *)

  val row : 'a -> cell array -> 'a row
  (** [row value cells] displays [cells] and reports [value] when selected or
      activated. [cells] must not be mutated while the row is mounted. *)
end

(** Companion types for the {!val-tree} widget. *)
module Tree : sig
  type action = Mosaic_ui.Tree.action =
    | Move_up
    | Move_down
    | Move_up_fast
    | Move_down_fast
    | Expand
    | Collapse
    | Toggle
    | Activate  (** Keyboard navigation, expansion and activation actions. *)

  type item = Mosaic_ui.Tree.item = {
    id : string;
    label : string;
    children : item list;
  }
  (** A node with a stable ID unique in the tree. *)

  val item : id:string -> ?children:item list -> string -> item
  (** [item ~id label] is a node whose children default to [[]]. *)

  module State = Mosaic_ui.Tree.State
end

(** Companion types for the {!val-spinner} widget. *)
module Spinner : sig
  type frame_set = Mosaic_ui.Spinner.frame_set = {
    frames : string array;  (** Characters cycled as animation frames. *)
    interval : float;  (** Time in milliseconds between frame advances. *)
  }
  (** The type for animation frame sets. *)

  val dots : frame_set
  (** Braille dot pattern (10 frames, 80 ms). *)

  val dots2 : frame_set
  (** Braille block pattern (8 frames, 80 ms). *)

  val line : frame_set
  (** ASCII line rotation (4 frames, 130 ms). *)

  val arc : frame_set
  (** Quarter-circle arc (6 frames, 100 ms). *)

  val bounce : frame_set
  (** Braille bounce (4 frames, 120 ms). *)

  val circle : frame_set
  (** Circle animation (3 frames, 120 ms). *)

  val default_frame_set : frame_set
  (** [default_frame_set] is {!dots}. *)
end

(** Companion types for the {!val-slider} widget. *)
module Slider : sig
  type orientation = Mosaic_ui.Slider.orientation
  (** The type for track direction: [`Horizontal] or [`Vertical]. *)
end

(** Companion types for the {!val-scroll_bar} widget. *)
module Scroll_bar : sig
  type orientation = Mosaic_ui.Scroll_bar.orientation
  (** The type for scroll bar orientation: [`Vertical] or [`Horizontal]. *)

  type style = Mosaic_ui.Scroll_bar.style
  (** Scroll bar appearance, independent of scroll position and dimensions. *)

  val style :
    ?show_arrows:bool ->
    ?track_color:Ansi.Color.t ->
    ?thumb_color:Ansi.Color.t ->
    ?arrow_fg:Ansi.Color.t ->
    ?arrow_bg:Ansi.Color.t ->
    unit ->
    style
  (** [style ()] is the default scroll bar appearance. Optional fields have the
      same defaults as {!Mosaic_ui.Scroll_bar.style}. *)
end

(** Structured content for {!val-rich_text}. *)
module Text : sig
  type fragment = Mosaic_ui.Text.fragment
  (** An immutable text fragment with optional nested styles. *)

  module Fragment = Mosaic_ui.Text.Fragment
end

(** Companion types for the {!val-text} and {!val-code} widgets. *)
module Text_surface : sig
  type wrap = Mosaic_ui.Text_surface.wrap
  (** The type for wrapping mode: [`None] (no wrapping), [`Char] (break at any
      character), or [`Word] (break at word boundaries). *)
end

(** Companion types for the {!val-line_number} widget. *)
module Line_number : sig
  type line_color = Mosaic_ui.Line_number.line_color = {
    gutter : Ansi.Color.t;  (** Background color of the gutter. *)
    content : Ansi.Color.t option;
        (** Background color of the content area, if any. *)
  }
  (** The type for per-line background color overrides. *)

  type line_sign = Mosaic_ui.Line_number.line_sign = {
    before : string option;  (** Icon rendered before the number. *)
    after : string option;  (** Icon rendered after the number. *)
    before_color : Ansi.Color.t option;  (** Color of [before]. *)
    after_color : Ansi.Color.t option;  (** Color of [after]. *)
  }
  (** The type for gutter sign decorations. *)
end

module Diff = Mosaic_ui.Diff
(** Companion types for the {!val-diff} widget. *)

(** Companion types for the {!val-markdown} widget. *)
module Markdown : sig
  type style_key = Mosaic_ui.Markdown.style_key =
    | Default
    | Heading of int
    | Emphasis
    | Strong
    | Code_span
    | Code_block
    | Link
    | Image
    | Blockquote
    | Thematic_break
    | List_marker
    | Strikethrough
    | Task_marker
    | Table_border
    | Conceal_punctuation
        (** Style keys identifying markdown element classes. *)

  module Style = Mosaic_ui.Markdown.Style
  (** Element styles: the terminal style of every {!style_key}, compared by
      value. *)
end

module Code : sig
  type syntax = Mosaic_ui.Code.syntax
  (** The type for source-code syntax settings. *)

  module Highlighter : sig
    type request = Mosaic_ui.Code.Highlighter.request = {
      content : string;
      language : string;
    }

    type result = (Mosaic_ui.Syntax_highlight.t, exn) Stdlib.result
    type job = Mosaic_ui.Code.Highlighter.job
    type t = Mosaic_ui.Code.Highlighter.t

    val job : poll:(unit -> result option) -> cancel:(unit -> unit) -> job

    val sync :
      (language:string -> content:string -> Mosaic_ui.Syntax_highlight.t) -> t
    (** [sync f] runs [f] on the rendering thread; errors fall back to plain
        text. *)

    val threaded :
      (language:string -> content:string -> Mosaic_ui.Syntax_highlight.t) -> t
    (** [threaded f] serializes requests on one on-demand worker systhread per
        highlighter. Cancellation removes queued requests and discards running
        results. Reuse the highlighter across renders. See
        {!Mosaic_ui.Code.Highlighter.threaded} for the threading contract. *)

    val async : (request -> notify:(unit -> unit) -> job) -> t
  end

  val syntax :
    ?language:string ->
    ?style:Mosaic_ui.Syntax_style.t ->
    ?conceal:bool ->
    Mosaic_ui.Syntax_highlight.t ->
    syntax
  (** [syntax highlights] is a source-code syntax configuration. *)

  val with_highlighter :
    language:string ->
    ?style:Mosaic_ui.Syntax_style.t ->
    ?conceal:bool ->
    Highlighter.t ->
    syntax
  (** [with_highlighter ~language highlighter] is a source-code syntax
      configuration produced by [highlighter]. See
      {!Mosaic_ui.Code.with_highlighter} for what is shown while a result is
      pending. *)
end

(** Syntax styles. The normative scope vocabulary and fallback rules are
    documented in {!Mosaic_ui.Syntax_style}. *)
module Syntax_style : sig
  type t = Mosaic_ui.Syntax_style.t
  (** The type for syntax styles. *)

  val make : base:Ansi.Style.t -> (string * Ansi.Style.t) list -> t
  (** [make ~base mappings] is a syntax style with [base] as the default style.
  *)

  val default : t
  (** The built-in dark syntax style. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have equal base styles and equal
      mappings. Styles are values; see {!Mosaic_ui.Syntax_style.equal}. *)

  val base : t -> Ansi.Style.t
  (** [base style] is the default style for text outside highlighted ranges. *)

  val resolve_overlay : t -> string -> Ansi.Style.t
  (** [resolve_overlay style group] is the raw overlay style for [group]. *)

  val resolve : t -> string -> Ansi.Style.t
  (** [resolve style group] is the complete style for [group]: overlay merged on
      top of the base style. *)
end

module Syntax_highlight = Mosaic_ui.Syntax_highlight
(** Source-bound highlight ranges and their resolved byte annotations. *)

(** {1:views Views} *)

type 'msg t = 'msg Vnode.t
(** The type for view nodes parameterized by message type ['msg]. Semantic
    callbacks produce messages directly. Raw event handlers return
    ['msg option]: [Some msg] dispatches the message; [None] ignores the event.

    Build values of this type with the element constructors ({!val-box},
    {!val-text}, {!val-input}, etc.) and compose them with {!val-fragment}. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f view] is [view] with every message transformed by [f].

    Use [map] to embed a child component whose message type differs from the
    parent's. *)

(** {1:commands Commands} *)

(** Side-effects issued by the application.

    Commands are values returned alongside a new model from {!app.init} and
    {!app.update}. The runtime executes them after each update cycle. Compose
    multiple commands with {!Cmd.val-batch}. *)
module Cmd : sig
  (** The type for commands. *)
  type 'msg t =
    | None  (** No command. Equivalent to [batch []]. *)
    | Batch of 'msg t list
        (** Start the commands in list order. See {!val-batch}. *)
    | Perform of (('msg -> unit) -> unit)
        (** Execute an arbitrary side-effecting function. The function receives
            a {e dispatch} callback and may call it zero or more times, from any
            thread. See {!val-perform}. *)
    | Exec of (unit -> 'msg)
        (** Give the terminal to a function and dispatch its result. See
            {!val-exec}. *)
    | Suspend  (** Stop the job. See {!val-suspend}. *)
    | Quit  (** End the run. See {!val-quit}. *)
    | Set_title of string
        (** Set the terminal window title to the given string. *)
    | Copy_to_clipboard of string
        (** Copy text to the terminal clipboard when supported. *)
    | Copy_selection
        (** Copy the renderer's current text selection to the clipboard via OSC
            52. No-op when there is no active selection. *)
    | Query_color_scheme
        (** Ask the terminal for its light/dark colour scheme by emitting
            [CSI ? 996 n]. The reply, when supported, arrives through
            {!Sub.on_color_scheme}. *)
    | Bell  (** Ring the terminal bell (BEL), a best-effort attention cue. *)
    | Notify of { title : string; body : string }
        (** Post a desktop notification via OSC 9 (iTerm2, kitty) and OSC 777
            (urxvt and others); a terminal ignores the sequence it does not
            understand. Wrapped for tmux passthrough when inside tmux. *)
    | Clear_selection  (** Clear the active text selection, if any. *)
    | Static_commit of 'msg Vnode.t
        (** Render a vnode snapshot and write it to the static area with ANSI
            styling preserved. The row count is computed automatically from the
            rendered grid. *)
    | Static_clear  (** Clear all previously written static content. *)
    | Abort_paste
        (** End the bracketed paste in progress, if any, and drop it; the bytes
            after it are input again. See {!val-abort_paste}. *)

  val none : 'msg t
  (** [none] is the empty command. Produces no side-effects. *)

  val batch : 'msg t list -> 'msg t
  (** [batch cmds] starts the commands of [cmds] in list order, depth first.

      A command starts after the update that returned it, before the next
      message, event or frame. The commands that act on the terminal (a title,
      the clipboard, static output, a child given the terminal) therefore take
      effect in list order: they write to one stream, in order. Work done off
      the loop ({!val-perform}) starts in that order too, but its messages
      arrive in the order it finishes, which nothing promises. *)

  val perform : (('msg -> unit) -> unit) -> 'msg t
  (** [perform f] runs [f dispatch] off the loop, by {!val-run}'s
      [process_perform] (by default a new thread), so that long work never holds
      the loop.

      [f] may call [dispatch msg] any number of times, from any thread; each
      call queues [msg] for the loop, which applies it with [update] when it
      takes it, a step of a turn as an event is. Each thread's messages come in
      the order it dispatched them.

      An exception [f] raises ends the run: {!val-run} gives the terminal back
      and raises it, with the backtrace of where [f] raised it (when backtraces
      are recorded, {!Printexc.record_backtrace}). After the run, [dispatch]
      does nothing, and an exception from [f] has no run left to end: it is
      dropped. *)

  val exec : (unit -> 'msg) -> 'msg t
  (** [exec f] gives the terminal to [f] and dispatches [f ()]'s result once the
      terminal is taken again ({!Matrix.release}): [f] can run another program
      in the terminal, an editor or a pager, as a shell runs a command. The
      program waits for [f] as POSIX [system()]'s caller does: nothing is drawn
      or read, and timers, workers' messages and other commands wait. [f] finds
      the terminal as a shell leaves it to a command: cooked, on the primary
      screen, with the cursor shown and the keys typed for it, and the
      terminal's suspend character stopping its program. The next frame is drawn
      whole.

      An exception [f] raises ends the run, once the terminal is taken again. *)

  val suspend : 'msg t
  (** [suspend] stops the program's job as the terminal's suspend character does
      in a shell ({!Matrix.stop_job}): the terminal is given back, the job
      stops, and when the shell continues it the terminal is taken again and the
      next frame drawn whole. Raw mode turns the suspend character, Ctrl+Z, into
      a key, so a program that wants it to suspend binds it to this command.
      Nothing happens when the job cannot be stopped (SIGTSTP was ignored when
      the run began). *)

  val quit : 'msg t
  (** [quit] ends the run once the commands of the update that returned it have
      started, so [batch [static_commit v; quit]] commits [v]. Nothing after
      that update is applied: events read and not yet handled, typed for a
      program that has ended, are dropped, and no frame is drawn. *)

  val set_title : string -> 'msg t
  (** [set_title s] sets the terminal window title to [s]. *)

  val copy_to_clipboard : string -> 'msg t
  (** [copy_to_clipboard s] copies [s] to the terminal clipboard when supported.
  *)

  val copy_selection : 'msg t
  (** [copy_selection] copies the renderer's current text selection to the
      clipboard via OSC 52. Does nothing when no selection is active. *)

  val query_color_scheme : 'msg t
  (** [query_color_scheme] asks the terminal whether it is using a light or dark
      colour scheme by emitting [CSI ? 996 n]. Terminals that support the query
      reply, and the reply is delivered through {!Sub.on_color_scheme}. *)

  val bell : 'msg t
  (** [bell] rings the terminal bell (BEL). *)

  val notify : title:string -> body:string -> 'msg t
  (** [notify ~title ~body] posts a desktop notification through the terminal.
  *)

  val clear_selection : 'msg t
  (** [clear_selection] clears the active text selection, if any. *)

  val static_commit : 'msg Vnode.t -> 'msg t
  (** [static_commit view] renders [view] offscreen at the current terminal
      width and appends the styled result to static output. *)

  val static_clear : 'msg t
  (** [static_clear] removes all previously emitted static content. *)

  val abort_paste : 'msg t
  (** [abort_paste] ends the bracketed paste in progress, if any, and drops what
      it received ({!Matrix.abort_paste}): the bytes after it are input again. A
      paste ends only at its end marker, so one whose terminal stops before the
      marker takes every later byte as content, keys typed included, until the
      application gives up on it; {!Sub.on_paste_stalled} says when it has gone
      silent. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f cmd] is [cmd] with every dispatched message transformed by [f]. Use
      this to embed child-component commands in a parent command. *)
end

(** {1:subscriptions Subscriptions} *)

(** Ongoing event interests declared by the application.

    The {!app.subscriptions} function is called after every update and returns a
    subscription value describing which events the application wants to receive.
    Subscriptions are re-evaluated on each cycle, so they may depend on the
    current model. Compose multiple subscriptions with {!Sub.val-batch}. *)
module Sub : sig
  (** {1:phases Phases}

      A key, a paste or a mouse event goes from the application down to its
      target element and back up, as in the DOM: the application's [Capture]
      listeners, then the target's handlers, then its behavior unless prevented
      (the character a key types into a focused input, the item a key selects in
      a focused list), then each ancestor's handlers, then the application's
      [Bubble] listeners. A key or a paste targets the focused element; a mouse
      event the element under the pointer. {!Event} says what stopping
      propagation and preventing the default do along the way. A behavior that
      acts on an event consumes it: it prevents and stops it. The keys a
      widget's behavior acts on are part of the widget's interface, listed in
      its module (for example {!Mosaic_ui.Select}).

      The application is one node: its listeners of a phase all see what reaches
      the phase, in declaration order, whatever the others did with it. A
      [Capture] listener sees every event before any element, and can keep it
      from them. A [Bubble] listener sees every event no element stopped,
      prevented or not, and reads {!Event.Key.default_prevented} to know whether
      something used it. A behavior that acts also stops the event, so a
      [Bubble] listener never sees a key typed into a focused input: a shortcut
      on [q] declared there does not fire while [q] is typed. A key the
      application takes whatever is focused, such as Tab moving focus, belongs
      in [Capture], with {!Event.Key.prevent_default} so the focused element
      does not act on it too. One [Capture] listener cannot hide an event from
      another; while a dialog owns the keyboard, stop declaring the listeners it
      overrides.

      A listener's message goes through [update] at once. The path is taken when
      the event is dispatched, so a focus change that message makes moves the
      next event, not this one, with one exception: the behavior runs on the
      element focused when it runs, so a [Capture] listener that focuses an
      input on a printable key gets that character inserted into the input. *)

  (** The type for the phase a listener runs in. *)
  type phase =
    | Capture  (** Before the elements, for every event. *)
    | Bubble  (** After the elements, for the events no element stopped. *)

  (** {1:workers Workers}

      A worker is work that runs off the loop for as long as the program
      declares it: following a file, reading a socket, watching a process. Its
      lifetime is its subscription's, as a timer's is: it starts in the update
      whose subscriptions first declare its id, and its stop is requested in the
      update whose subscriptions no longer do, or when the run ends. OCaml
      cannot interrupt a thread, so a worker stops itself: it reads
      {!Stop.requested} between steps, and a step that blocks (a read, a wait)
      is unblocked from {!Stop.on_request}, for example by closing what it waits
      on. *)

  (** Stop requests. *)
  module Stop : sig
    type t
    (** The type for a worker's stop request, as the DOM's [AbortSignal]. *)

    val requested : t -> bool
    (** [requested t] is [true] once the worker's stop was requested. Any thread
        may call it. *)

    val on_request : t -> (unit -> unit) -> unit
    (** [on_request t f] calls [f ()] once, when the stop is requested, on the
        loop's thread; at once, on the calling thread, if it was already. [f]
        runs on the loop, so it should only unblock the worker. An exception it
        raises ends the run, or is dropped when the run is ending. *)
  end

  type 'msg mailbox = {
    dispatch : 'msg -> unit;
        (** [dispatch msg] queues [msg] for the loop and returns: it never
            blocks. The queue has no bound, so a worker that dispatches faster
            than the loop applies its messages grows memory, and collecting it
            slows the loop. *)
    send : 'msg -> unit;
        (** [send msg] queues [msg] as [dispatch] does and returns once the loop
            has taken it and [update] applied it, or the run has ended: a worker
            that sends keeps pace with the loop. A worker that produces values
            faster than they are shown, such as progress, keeps the latest and
            sends it when the previous [send] returns.

            [send] waits for the loop, so it deadlocks the loop's thread and
            anything the loop waits for: it raises [Invalid_argument] on the
            loop's thread, and must not be called while holding a lock that
            [update], [view] or a command takes, or from a fiber or a coroutine
            that shares the loop's thread. *)
  }
  (** The type for the way a worker gives the program messages. Messages of one
      worker come in the order it gave them. Once the worker's stop is requested
      both do nothing, and a message given before but not yet taken by the loop
      is dropped: no message of a worker reaches [update] after the update that
      removed it. *)

  (** The type for subscriptions. *)
  type 'msg t =
    | None  (** No subscription. Equivalent to [batch []]. *)
    | Batch of 'msg t list  (** Activate all subscriptions in the list. *)
    | Every of string * float * (int -> 'msg)
        (** [Every (id, period, f)] fires [f k] every [period] seconds; see
            {!val-every}. *)
    | After of string * float * 'msg
        (** [After (id, delay, msg)] dispatches [msg] once, [delay] seconds
            after [id] appears; see {!val-after}. *)
    | On_tick of string * (dt:float -> 'msg)
        (** [On_tick (id, f)] fires [f ~dt] once per frame; see {!val-on_tick}.
        *)
    | On_key of phase * (Event.key -> 'msg option)
        (** [On_key (phase, f)] listens to key presses; see {!val-on_key}. *)
    | On_key_release of (Matrix.Input.Keystroke.release -> 'msg option)
        (** [On_key_release f] delivers key releases. *)
    | On_mouse of phase * (Event.mouse -> 'msg option)
        (** [On_mouse (phase, f)] listens to mouse events; see {!val-on_mouse}.
        *)
    | On_paste of phase * (Event.paste -> 'msg option)
        (** [On_paste (phase, f)] listens to pastes; see {!val-on_paste}. *)
    | On_paste_stalled of float * 'msg
        (** [On_paste_stalled (seconds, msg)] dispatches [msg] when a paste in
            progress has received nothing for [seconds]. *)
    | On_resize of (width:int -> height:int -> 'msg)
        (** [On_resize f] fires [f ~width ~height] whenever the terminal is
            resized. *)
    | On_focus of 'msg
        (** [On_focus msg] dispatches [msg] when the terminal window gains
            focus. *)
    | On_blur of 'msg
        (** [On_blur msg] dispatches [msg] when the terminal window loses focus.
        *)
    | On_color_scheme of ([ `Dark | `Light ] -> 'msg option)
        (** [On_color_scheme f] delivers the terminal's light/dark colour
            scheme, both as the reply to {!Cmd.query_color_scheme} and as
            unsolicited DEC 2031 notifications. [f] returns [None] to ignore a
            report. *)
    | Worker of string * (Stop.t -> 'msg mailbox -> unit)
        (** [Worker (id, f)] runs [f] off the loop while [id] is declared; see
            {!val-worker}. *)

  val none : 'msg t
  (** [none] is the empty subscription. Produces no events. *)

  val batch : 'msg t list -> 'msg t
  (** [batch subs] is a subscription that activates every subscription in
      [subs]. *)

  val every : id:string -> float -> (int -> 'msg) -> 'msg t
  (** [every ~id period f] fires on the grid [origin + n * period], [origin] the
      time [id] first appeared in the subscriptions, and dispatches [f k].
      [k >= 1] is the number of periods that ended since its previous call: [1]
      on time, more when the loop could not run for longer than a period (a long
      [update], a stopped process). The grid does not drift: a late call does
      not move the next one, and a timer never fires twice in a turn. Timers
      that come due in the same turn fire in the order of their grid points.

      The subscription is the timer: keeping [id] keeps its grid, declaring it
      with another [period] starts a new grid at that time, and dropping [id]
      stops it. A pending timer costs nothing until it fires. Not for animation:
      use {!val-on_tick}.

      Raises [Invalid_argument] if [id] is empty or [period] is not positive and
      finite. The run raises it when two subscriptions declare the same id,
      among those that take one ({!val-every}, {!val-after}, {!val-on_tick},
      {!val-worker}). *)

  val after : id:string -> float -> 'msg -> 'msg t
  (** [after ~id delay msg] dispatches [msg] once, [delay] seconds after [id]
      first appears in the subscriptions: in the first turn at or past that
      time, as {!val-every} fires. Dropping [id] before then cancels it, which
      makes a debounce: declare [after] while the model waits, and each update
      that should restart the wait declares it under a new id. Once fired, it
      stays inert while [id] stays declared, and fires again only if [id] leaves
      and returns. Declaring [id] with another [delay] starts it again from that
      time.

      Raises [Invalid_argument] if [id] is empty or [delay] is negative or not
      finite. *)

  val on_tick : id:string -> (dt:float -> 'msg) -> 'msg t
  (** [on_tick ~id f] dispatches [f ~dt] once per frame while declared, before
      the frame is laid out, and asks for the next frame: frames then come at
      the frame rate ({!Matrix.config}'s [fps]), and stop when no tick or live
      element asks. [dt] is the time in seconds since this subscription's
      previous tick, [0.] on its first: an animation started after a pause does
      not jump by the pause. Keeping [id] keeps the count of time; dropping it
      and declaring it again starts at [0.]. Use this for animations; for work
      at a fixed rate, {!val-every}.

      Raises [Invalid_argument] if [id] is empty. *)

  val on_key : ?phase:phase -> (Event.key -> 'msg option) -> 'msg t
  (** [on_key f] calls [f] on each key press that reaches [phase] (defaults to
      [Bubble]; see {!section-phases}) and dispatches the message it returns. *)

  val on_keys : ?phase:phase -> (Shortcut.t * 'msg) list -> 'msg t
  (** [on_keys bindings] dispatches the message bound to a key that reaches
      [phase] (defaults to [Bubble]; see {!section-phases}) with its default not
      prevented, and prevents it: a binding is a default action of its key, so a
      key something prevented fires none. Later equivalent shortcuts override
      earlier ones. Resolution tries the key, then the base-layout key, as
      widget bindings do. See {!Shortcut} for how keys and modifiers match. *)

  val on_key_release : (Matrix.Input.Keystroke.release -> 'msg option) -> 'msg t
  (** [on_key_release f] delivers key releases to [f], which no widget sees. A
      terminal reports releases only under the kitty keyboard protocol, when the
      runtime asks for them ({!Matrix.config}'s [key_releases], given through
      {!run}'s [config]). *)

  val on_mouse : ?phase:phase -> (Event.mouse -> 'msg option) -> 'msg t
  (** [on_mouse f] calls [f] on each mouse event that reaches [phase] (defaults
      to [Bubble]; see {!section-phases}) and dispatches the message it returns.
  *)

  val on_paste : ?phase:phase -> (Event.paste -> 'msg option) -> 'msg t
  (** [on_paste f] calls [f] on each paste that reaches [phase] (defaults to
      [Bubble]; see {!section-phases}) and dispatches the message it returns.
      The focused editor's behavior inserts a paste and stops it, so a [Bubble]
      listener sees a paste no editor took; a [Capture] listener can prevent the
      insertion with {!Event.Paste.prevent_default}. A bracketed paste is
      delivered whole once its end marker arrives, and dropped when cut short
      ({!Cmd.abort_paste}, or the end of the input); see {!val-on_paste_stalled}
      for a paste whose end marker never comes. Text a terminal reports without
      a key, such as an input method's, is delivered as a paste too. *)

  val on_paste_stalled : float -> 'msg -> 'msg t
  (** [on_paste_stalled seconds msg] dispatches [msg] once each time a bracketed
      paste in progress has received nothing for [seconds].

      A paste ends only at its end marker, however long the terminal takes to
      send it: time never ends a paste. A paste whose marker never comes, from a
      terminal or link that failed mid-paste, takes every later byte as its
      content, Ctrl+C included, and nothing the user types can end it. The
      application decides when to give up, with {!Cmd.abort_paste}, from how
      long the paste has been silent: any byte counts, keys typed into the paste
      too, since they are its content. Raises [Invalid_argument] if [seconds] is
      not positive and finite. *)

  val on_resize : (width:int -> height:int -> 'msg) -> 'msg t
  (** [on_resize f] fires [f ~width ~height] whenever the terminal is resized,
      reporting the new dimensions in columns and rows. *)

  val on_focus : 'msg -> 'msg t
  (** [on_focus msg] dispatches [msg] when the terminal window gains focus. *)

  val on_blur : 'msg -> 'msg t
  (** [on_blur msg] dispatches [msg] when the terminal window loses focus. *)

  val on_color_scheme : ([ `Dark | `Light ] -> 'msg option) -> 'msg t
  (** [on_color_scheme f] delivers the terminal's light/dark colour scheme to
      [f], both as the reply to {!Cmd.query_color_scheme} and as unsolicited DEC
      2031 notifications. [f] returns [None] to ignore a report. *)

  val worker : id:string -> (Stop.t -> 'msg mailbox -> unit) -> 'msg t
  (** [worker ~id f] runs [f stop mailbox] off the loop, on {!val-run}'s
      [process_perform] executor, from the update whose subscriptions first
      declare [id] (see {{!workers}Workers}). Declaring [id] again, with any
      function, keeps the worker that runs. [f] returning ends the worker; it
      starts again only if [id] leaves the subscriptions and returns.

      An exception [f] raises ends the run, as one from {!Cmd.val-perform} does,
      unless its stop was requested before: the program no longer waits for
      anything from it.

      Raises [Invalid_argument] if [id] is empty. The run raises it when two
      subscriptions declare the same id, among those that take one. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f sub] is [sub] with every produced message transformed by [f]. Use
      this to embed child-component subscriptions in a parent subscription. *)
end

(** {1:application Application} *)

type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
      (** [init ()] is the initial model and any startup commands. Called once
          before the first render. *)
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
      (** [update msg model] is the new model and any side-effects produced in
          response to [msg]. Called once per dispatched message. *)
  view : 'model -> 'msg t;
      (** [view model] is the UI description for [model]. Refreshed after
          accepted model changes before subsequent input processing. Several
          views may be evaluated before one paint. *)
  subscriptions : 'model -> 'msg Sub.t;
      (** [subscriptions model] is the set of events the application wants to
          receive given [model]. Re-evaluated after every update. *)
}
(** The type for a Mosaic application. Provide a value of this type to
    {!val-run}. *)

(** {1:probe Runtime probe} *)

module Probe : sig
  (** Runtime quiescence introspection for test harnesses.

      A probe collects named checks for outstanding work. {!val-run} supplies
      checks for undispatched messages, in-flight {!Cmd.val-perform} callbacks,
      and asynchronous rendering, named [messages], [performs], and [render]
      respectively. Applications may add checks for work they own outside Mosaic
      with {!with_pending}.

      A settled application's frame is stable until new input arrives, the
      backend clock advances, or an external producer adds work. Live timers
      ({!Sub.val-every}, {!Sub.val-on_tick}) do not prevent settlement; they
      only fire as time moves. Obtain a probe through {!val-run}'s [probe]
      argument. *)

  type t
  (** The type for runtime probes. *)

  val is_settled : t -> bool
  (** [is_settled t] is [true] iff every pending-work check in [t] returns
      [false]. *)

  val pending : t -> string list
  (** [pending t] is the names of the checks in [t] that currently return
      [true], in the order they were added. This allocates a list and is
      intended for diagnostics. *)

  val with_pending : name:string -> (unit -> bool) -> t -> t
  (** [with_pending ~name is_pending t] is [t] extended with the named
      [is_pending] check. The check must be fast, non-blocking, and free of side
      effects.

      Adding a check does not arrange a runtime wake. When external work
      completes, its owner must dispatch a message or {!Matrix.post} to the loop
      so a driver blocked on backend idleness can re-evaluate the probe. *)
end

(** {1:running Running} *)

val run :
  ?backend:Matrix.Backend.t ->
  ?config:Matrix.config ->
  ?process_perform:((unit -> unit) -> unit) ->
  ?probe:(Probe.t -> unit) ->
  ('model, 'msg) app ->
  unit
(** [run ?backend ?config ?process_perform ?probe app] calls [app]'s [init],
    then runs [app] in the terminal ({!Matrix.run}) until {!Cmd.val-quit}, a
    Ctrl+C press no handler prevented ({!Event.Key.prevent_default}), the end of
    the input, or a signal that ends the process. In raw mode Ctrl+C is a key:
    quitting is its default, which an element or a listener that uses the key
    prevents. The terminal is given back on every way out, an exception
    included. [init] runs before the terminal is taken; its commands and the
    first subscriptions run after.

    - [backend] -- the terminal to run in. Defaults to {!Matrix.Backend.unix}.
    - [config] -- what the run sets up. Defaults to {!Matrix.config} with a
      hidden cursor.
    - [process_perform] -- controls how {!Cmd.Perform} callbacks are executed.
      Receives a thunk that wraps the user callback with the dispatch function
      already wired in. The default spawns a native [Thread] per callback so
      that long-running operations (e.g. HTTP requests) never block the UI loop.
      The dispatch function may be called from any thread or fiber: it posts the
      message to the loop ({!Matrix.post}), which applies it in a later turn.
    - [probe] -- receives a {!Probe.t} for this runtime before the loop starts.
      Test harnesses use it to settle deterministically (see the [matrix.test]
      library). When a probe is requested, every perform completion also posts
      to the loop, so a blocked loop re-evaluates its quiescence.

    {b Eio integration.} Pass a [backend] built with the [matrix_eio] library
    and a [process_perform] that forks fibers:

    {[
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let backend =
      Matrix_eio.backend
        ~clock:(Eio.Stdenv.mono_clock env)
        ~stdin:env#stdin ~stdout:env#stdout ()
    in
    let process_perform thunk =
      Eio.Fiber.fork_daemon ~sw (fun () ->
          thunk ();
          `Stop_daemon)
    in
    Mosaic.run ~backend ~process_perform { init; update; view; subscriptions }
    ]}

    Daemon fibers are cancelled when the switch completes, so long-running
    perform operations do not block application shutdown. *)

(** {1:dim_helpers Dimension helpers} *)

val px : int -> dimension
(** [px n] is a dimension of exactly [n] terminal columns or rows. *)

val pct : int -> dimension
(** [pct n] is a dimension of [n] percent of the parent's available space. *)

val auto : dimension
(** [auto] is the automatic dimension, letting the layout engine determine the
    size. *)

val size : width:int -> height:int -> dimension size
(** [size ~width ~height] is a fixed size with the given [width] and [height]
    measured in terminal cells. *)

val size_wh : dimension -> dimension -> dimension size
(** [size_wh w h] is a size with the given [w] width and [h] height dimensions.
*)

val gap : int -> length_percentage size
(** [gap n] is a uniform gap of [n] cells applied to both axes of a flex or grid
    container. *)

val gap_xy : int -> int -> length_percentage size
(** [gap_xy x y] is a gap of [x] cells on the horizontal axis and [y] cells on
    the vertical axis. *)

val padding : int -> length_percentage rect
(** [padding n] is a uniform padding of [n] cells on all four sides. *)

val padding_xy : int -> int -> length_percentage rect
(** [padding_xy x y] is padding of [x] cells on the left and right sides and [y]
    cells on the top and bottom sides. *)

val padding_lrtb : int -> int -> int -> int -> length_percentage rect
(** [padding_lrtb l r t b] is padding of [l] cells on the left, [r] on the
    right, [t] on the top, and [b] on the bottom. *)

val margin : int -> length_percentage_auto rect
(** [margin n] is a uniform margin of [n] cells on all four sides. *)

val margin_xy : int -> int -> length_percentage_auto rect
(** [margin_xy x y] is margin of [x] cells on the left and right sides and [y]
    cells on the top and bottom sides. *)

val margin_lrtb : int -> int -> int -> int -> length_percentage_auto rect
(** [margin_lrtb l r t b] is margin of [l] cells on the left, [r] on the right,
    [t] on the top, and [b] on the bottom. *)

val inset : int -> length_percentage_auto rect
(** [inset n] is a uniform inset of [n] cells on all four sides, used with
    [position = `Absolute] or [position = `Fixed]. *)

val inset_lrtb : int -> int -> int -> int -> length_percentage_auto rect
(** [inset_lrtb l r t b] is inset of [l] cells on the left, [r] on the right,
    [t] on the top, and [b] on the bottom. *)

(** {1:elements UI elements}

    Elements describe the current model. Layout and paint use labelled
    arguments. Layout follows {!Toffee.Style.default}; [box] uses its default
    row direction, and minimum dimensions remain automatic unless explicitly
    supplied.

    Common layout arguments include [display], [position], [size], [min_size],
    [max_size], [padding], [margin], [gap], the [flex_*] properties, alignment,
    and the [grid_*] properties. They map directly to Toffee layout
    declarations. [visible] defaults to [true], [opacity] to [1.], [z_index] to
    [0], and [buffered] and [live] to [false] unless the widget supplies a
    different default.

    [key] is an optional reconciliation hint. [focused] declares keyboard focus.
    With no focused element, keys reach subscriptions only. Declaring more than
    one visible focused element raises [Invalid_argument]. Hidden subtrees do
    not participate in focus. Traversal is application-owned; {!Focus} provides
    pure next/previous selection over application identifiers.

    Focus is the model's, so nothing moves it but a declaration. Removing or
    hiding the focused element leaves nothing focused, and no event says so:
    keys reach subscriptions only until the model declares another element
    focused. The update that removes the focused element is where to choose the
    next one, such as the row now at its position. Some toolkits (AWT) move
    focus forward by themselves when the focused component goes; here that would
    give the model's focus a second owner.

    [on_focus_request] supplies the message for an ordinary left-button press on
    an unfocused element. The request runs before caret and selection handling.
    The application accepts it by declaring the element [focused]. Rejecting the
    request, hiding or removing the element cancels that press's default
    interaction. Shift-click does not request focus.

    Generic event callbacks return a message or [None]. [on_click] runs after a
    primary-button press and release on the element, without dragging.
    Preventing the mouse event default cancels activation. Neither callback
    changes application state by itself. Interactive [state] values belong to
    the model. Semantic callbacks such as [on_change], [on_scroll], [on_submit]
    and [on_activate] return messages directly; acceptance or rejection belongs
    in [update]. Storing an [on_change] proposal accepts it. Subsequent events
    derive from the accepted model.

    [on_size] reports an element's allocated dimensions in cells when they
    differ from the last report. Reports are delivered during layout, and their
    messages are applied before the frame is painted, so a view that depends on
    a size settles within the frame. Within one frame, once a layout pass has
    delivered reports, later passes deliver only the reports of elements nested
    deeper than the shallowest element already reported: a response that resizes
    descendants settles in the same frame. A response that resizes the reported
    element itself, a sibling or an ancestor waits: the frame is painted from
    the model as it stands, and the paint delivers the waiting report, whose
    message is applied before the next frame. This is the depth rule of CSS
    Resize Observer
    ({{:https://www.w3.org/TR/resize-observer/#gather-active-observations-h}
      §3.4.1}). A response that keeps resizing its own element therefore
    alternates on every frame, presenting each one, instead of blocking the
    loop. *)

val empty : 'msg t
(** [empty] occupies no space. *)

val fragment : 'msg t list -> 'msg t
(** [fragment children] groups children without a layout wrapper. *)

val embed : Mosaic_ui.Renderable.t -> 'msg t
(** [embed node] integrates an existing renderable into the view tree. Mosaic
    attaches the node but does not manage its properties or interaction state;
    the integration is responsible for their ownership. *)

val memo : key:'key -> ('key -> 'msg t) -> 'msg t
(** [memo ~key view] defers [view key]. When the previous render mounted an
    equal memo at the same position, Mosaic reuses that subtree without calling
    [view].

    Two memos are equal when they have the same [view] function, compared
    physically, equal keys, and the same message mappers ({!val-map}). Keys are
    equal when they have the same representation: the same constructors and
    integers, the same characters, and the same floating-point bits. Since a
    [view] receives only its key, equal memos build equal subtrees whatever the
    key's type.

    Pass a [view] defined once, for example at top level, and put every value
    the subtree depends on in [key]:
    {[
      let message_view (id, revision, theme) = ...
      let transcript model =
        List.map
          (fun m -> memo ~key:(m.id, m.revision, model.theme) message_view)
          model.messages
    ]}
    A closure allocated by each view call is a new function: it never matches,
    so its subtree is rebuilt on every render. Keys must be immutable and
    acyclic.

    Raises [Invalid_argument] if [key] contains a function, an object or an
    abstract value, which have no representation to compare. *)

val viewport_switch :
  at_least_width:int -> wide:'msg t -> narrow:'msg t -> 'msg t
(** [viewport_switch ~at_least_width ~wide ~narrow] selects [wide] when the
    current terminal viewport is at least [at_least_width] cells wide and
    [narrow] otherwise.

    Mosaic resolves the condition before reconciliation and layout. Only the
    selected branch is mounted, so the other branch cannot take space, receive
    focus, install handlers, or keep live rendering active. Crossing the
    threshold reconciles the new branch normally against the old one; compatible
    keyed elements reuse their renderables and caches; unmatched elements are
    unmounted.

    This is a viewport query, not a query on the containing element's allocated
    width. Raises [Invalid_argument] if [at_least_width] is not positive. *)

val box :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?border:bool ->
  ?border_style:Border.t ->
  ?border_sides:Border.side list ->
  ?border_color:Ansi.Color.t ->
  ?focused_border_color:Ansi.Color.t ->
  ?background:Ansi.Color.t ->
  ?fill:bool ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  'msg t list ->
  'msg t
(** [box children] is a container for [children]. Widget-specific visual options
    follow {!Mosaic_ui.Box}; layout and paint use the labelled arguments above.
*)

val button :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?border:bool ->
  ?border_style:Border.t ->
  ?border_sides:Border.side list ->
  ?border_color:Ansi.Color.t ->
  ?focused_border_color:Ansi.Color.t ->
  ?background:Ansi.Color.t ->
  ?fill:bool ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  ?text_style:Ansi.Style.t ->
  ?disabled:bool ->
  on_activate:'msg ->
  string ->
  'msg t
(** [button ~on_activate label] displays [label] and emits [on_activate] on a
    primary-button click or an unmodified Enter, keypad Enter or Space press. A
    click requires a press and release inside the button without dragging. Key
    repeat and release events do not activate it; lock modifiers are ignored.
    When focused, the button acts on those keys and consumes them: they reach
    neither its ancestors nor a [Bubble] listener ({!Sub.section-phases}); every
    other key passes on.

    [focused] defaults to [false] and is owned by the application. Use
    [on_focus_request] to accept pointer focus before activation. [disabled],
    default [false], suppresses activation and focus requests and makes the
    button unfocused. Layout and box paint defaults follow {!box}; [text_style]
    styles the label. Focus adds inverse video; disabled adds dim instead.
    Caller-supplied colors and other attributes are preserved.

    A [Capture] listener ({!Sub.section-phases}) can prevent activation with
    {!Event.Key.prevent_default}. The button composes a box and text; it has no
    separate widget state. *)

val text :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?style:Ansi.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?tab_width:int ->
  ?truncate:bool ->
  string ->
  'msg t
(** [text content] is a text element. Widget-specific visual options follow
    {!Mosaic_ui.Text}; layout and paint use the labelled arguments above. *)

val rich_text :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?style:Ansi.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?tab_width:int ->
  ?truncate:bool ->
  Text.fragment list ->
  'msg t
(** [rich_text fragments] renders structured text as one selectable text widget.
    Fragment styles inherit from [text_style]; nested colours override inherited
    colours. Wrapping and selection cross fragment boundaries. All labelled
    options have the same meaning as {!text}. Switching between {!text} and
    [rich_text] retains the text widget. *)

val slider :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?orientation:Slider.orientation ->
  ?min:float ->
  ?max:float ->
  ?viewport_size:float ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  state:float ->
  on_change:(float -> 'msg) ->
  unit ->
  'msg t
(** [slider ()] is a slider. Widget-specific visual options follow
    {!Mosaic_ui.Slider}; layout and paint use the labelled arguments above. *)

val input :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?placeholder:string ->
  ?text_color:Ansi.Color.t ->
  ?background_color:Ansi.Color.t ->
  ?focused_text_color:Ansi.Color.t ->
  ?focused_background_color:Ansi.Color.t ->
  ?placeholder_color:Ansi.Color.t ->
  ?selection_color:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?cursor_style:[ `Block | `Line | `Underline ] ->
  ?cursor_color:Ansi.Color.t ->
  ?cursor_blinking:bool ->
  ?selectable:bool ->
  ?show_cursor:bool ->
  ?key_bindings:Mosaic_ui.Text_input.key_binding list ->
  on_change:(Editor.t -> 'msg) ->
  ?on_submit:(string -> 'msg) ->
  state:Editor.t ->
  unit ->
  'msg t
(** [input ~state ~on_change ()] is a single-line editor. [on_change] proposes
    editing state, including caret and selection changes. Accept the state on
    every edit, and compare {!Editor.text} before running text-dependent work.
    [on_submit] receives the accepted text on Enter.

    Raises [Invalid_argument] if [state] contains CR or LF. Typed and pasted
    newlines are stripped. Undo and redo that would restore multiline text are
    ignored. Widget-specific visual options follow {!Mosaic_ui.Text_input},
    which lists the keys the input acts on when focused; layout and paint use
    the labelled arguments above. *)

val select :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?background:Ansi.Color.t ->
  ?text_color:Ansi.Color.t ->
  ?focused_background:Ansi.Color.t ->
  ?focused_text_color:Ansi.Color.t ->
  ?selected_background:Ansi.Color.t ->
  ?selected_text_color:Ansi.Color.t ->
  ?description_color:Ansi.Color.t ->
  ?selected_description_color:Ansi.Color.t ->
  ?show_description:bool ->
  ?show_scroll_indicator:bool ->
  ?wrap_selection:bool ->
  ?key_bindings:(Shortcut.t * Select.action) list ->
  ?item_spacing:int ->
  ?fast_scroll_step:int ->
  on_change:('a -> 'msg) ->
  ?on_activate:('a -> 'msg) ->
  equal:('a -> 'a -> bool) ->
  selected:'a option ->
  'a Select.item list ->
  'msg t
(** [select ~equal ~selected ~on_change options] displays [options], keeping
    selection attached to the application value under [equal] when options move.
    [None] or a value absent from [options] displays no selection; it does not
    select another item or call [on_change]. Navigation then proposes an item
    value; activation without a selection does nothing. Values must be distinct
    under [equal]. Callbacks receive the value from the current options.

    When focused, the list acts on the keys {!Mosaic_ui.Select} lists. Each
    declaration performs a linear selection lookup and presentation conversion.
    Event-to-value lookup is constant-time. *)

val tab_select :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?tab_width:int ->
  ?background:Ansi.Color.t ->
  ?text_color:Ansi.Color.t ->
  ?focused_background:Ansi.Color.t ->
  ?focused_text_color:Ansi.Color.t ->
  ?selected_background:Ansi.Color.t ->
  ?selected_text_color:Ansi.Color.t ->
  ?description_color:Ansi.Color.t ->
  ?selected_description_color:Ansi.Color.t ->
  ?show_underline:bool ->
  ?show_description:bool ->
  ?show_scroll_arrows:bool ->
  ?wrap_selection:bool ->
  ?key_bindings:(Shortcut.t * Tab_select.action) list ->
  on_change:('a -> 'msg) ->
  ?on_activate:('a -> 'msg) ->
  equal:('a -> 'a -> bool) ->
  selected:'a option ->
  'a Tab_select.item list ->
  'msg t
(** [tab_select ~equal ~selected ~on_change options] displays horizontal tabs
    with the value-selection and callback semantics of {!select}. [None] or a
    value absent from [options] displays no selection. Values must be distinct
    under [equal]. When focused, the tabs act on the keys
    {!Mosaic_ui.Tab_select} lists. *)

val canvas :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  (Canvas.t -> delta:float -> unit) ->
  'msg t
(** [canvas draw] is a custom drawing surface. Widget-specific visual options
    follow {!Mosaic_ui.Canvas}; layout and paint use the labelled arguments
    above. *)

val spinner :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?frame_set:Spinner.frame_set ->
  ?color:Ansi.Color.t ->
  unit ->
  'msg t
(** [spinner ()] is an animated activity indicator. Widget-specific visual
    options follow {!Mosaic_ui.Spinner}; layout and paint use the labelled
    arguments above. *)

val progress_bar :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?value:float ->
  ?min:float ->
  ?max:float ->
  ?orientation:[ `Horizontal | `Vertical ] ->
  ?filled_color:Ansi.Color.t ->
  ?empty_color:Ansi.Color.t ->
  unit ->
  'msg t
(** [progress_bar ()] is a progress indicator. Widget-specific visual options
    follow {!Mosaic_ui.Progress_bar}; layout and paint use the labelled
    arguments above. *)

val scroll_bar :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?orientation:Scroll_bar.orientation ->
  ?show_arrows:bool ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  ?arrow_fg:Ansi.Color.t ->
  ?arrow_bg:Ansi.Color.t ->
  on_change:(int -> 'msg) ->
  state:int ->
  scroll_size:int ->
  viewport_size:int ->
  unit ->
  'msg t
(** [scroll_bar ()] is a scroll indicator. Widget-specific visual options follow
    {!Mosaic_ui.Scroll_bar}, whose {!Mosaic_ui.Scroll_bar.handle_key} lists the
    keys the bar acts on when focused; layout and paint use the labelled
    arguments above. *)

val scroll_box :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?scroll_x:bool ->
  ?scroll_y:bool ->
  ?background:Ansi.Color.t ->
  ?show_scrollbars:bool ->
  ?scrollbar_style:Scroll_bar.style ->
  ?vertical_bar_style:Scroll_bar.style ->
  ?horizontal_bar_style:Scroll_bar.style ->
  on_scroll:(Scroll.t -> 'msg) ->
  scroll:Scroll.t ->
  'msg t list ->
  'msg t
(** [scroll_box children] is a scrollable container. Widget-specific visual
    options follow {!Mosaic_ui.Scroll_box}; layout and paint use the labelled
    arguments above. [scrollbar_style] sets shared bar appearance;
    [vertical_bar_style] and [horizontal_bar_style] replace it for the
    corresponding axis. When focused, the box acts on the keys
    {!Mosaic_ui.Scroll_box} lists. *)

val textarea :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?annotations:Mosaic_ui.Text_buffer.Annotations.t ->
  ?ghost_text:string ->
  ?ghost_text_color:Ansi.Color.t ->
  ?placeholder:string ->
  ?wrap:Text_surface.wrap ->
  ?text_color:Ansi.Color.t ->
  ?background_color:Ansi.Color.t ->
  ?focused_text_color:Ansi.Color.t ->
  ?focused_background_color:Ansi.Color.t ->
  ?placeholder_color:Ansi.Color.t ->
  ?selection_color:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?cursor_style:[ `Block | `Line | `Underline ] ->
  ?cursor_color:Ansi.Color.t ->
  ?cursor_blinking:bool ->
  ?selectable:bool ->
  ?show_cursor:bool ->
  ?key_bindings:Mosaic_ui.Textarea.key_binding list ->
  on_change:(Editor.t -> 'msg) ->
  ?on_submit:(string -> 'msg) ->
  state:Editor.t ->
  unit ->
  'msg t
(** [textarea ()] is a multiline editor. Widget-specific visual options follow
    {!Mosaic_ui.Textarea}, which lists the keys the editor acts on when focused;
    layout and paint use the labelled arguments above. *)

val code :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?syntax:Code.syntax ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?tab_width:int ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?on_selection:((int * int) option -> 'msg option) ->
  string ->
  'msg t
(** [code content] is syntax-highlighted source. Widget-specific visual options
    follow {!Mosaic_ui.Code}; layout and paint use the labelled arguments above.
*)

val line_number :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?fg:Ansi.Color.t ->
  ?bg:Ansi.Color.t ->
  ?min_width:int ->
  ?padding_right:int ->
  ?show_line_numbers:bool ->
  ?line_number_offset:int ->
  ?line_colors:(int * Line_number.line_color) list ->
  ?line_signs:(int * Line_number.line_sign) list ->
  ?hidden_line_numbers:int list ->
  'msg t ->
  'msg t
(** [line_number child] is a line-number gutter for [child]. Widget-specific
    visual options follow {!Mosaic_ui.Line_number}; layout and paint use the
    labelled arguments above. *)

val diff :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?layout:Diff.layout ->
  ?theme:Diff.theme ->
  ?highlight:Diff.highlight ->
  ?line_highlights:Diff.line_highlight list ->
  ?line_spans:Diff.line_span list ->
  ?line_signs:Diff.line_sign list ->
  ?show_line_numbers:bool ->
  ?wrap:Text_surface.wrap ->
  ?selectable:bool ->
  ?text_style:Ansi.Style.t ->
  ?on_line_click:(Diff.line_hit -> 'msg option) ->
  Diff.Patch.t ->
  'msg t
(** [diff patch] is a rendered patch. Widget-specific visual options follow
    {!Mosaic_ui.Diff}; layout and paint use the labelled arguments above. *)

val markdown :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?md_style:Markdown.Style.t ->
  ?conceal:bool ->
  ?streaming:bool ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?highlighter:Code.Highlighter.t ->
  ?syntax_style:Syntax_style.t ->
  ?on_selection:(string option -> 'msg option) ->
  string ->
  'msg t
(** [markdown content] is rendered Markdown. Widget-specific visual options
    follow {!Mosaic_ui.Markdown}; layout and paint use the labelled arguments
    above. *)

val table :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?columns:Table.column list ->
  ?rows:'a Table.row list ->
  ?border:bool ->
  ?border_style:Border.t ->
  ?show_header:bool ->
  ?show_column_separator:bool ->
  ?show_row_separator:bool ->
  ?cell_padding:int ->
  ?header_color:Ansi.Color.t ->
  ?header_background:Ansi.Color.t ->
  ?text_color:Ansi.Color.t ->
  ?background:Ansi.Color.t ->
  ?selected_text_color:Ansi.Color.t ->
  ?selected_background:Ansi.Color.t ->
  ?focused_selected_text_color:Ansi.Color.t ->
  ?focused_selected_background:Ansi.Color.t ->
  ?selection_visible:bool ->
  ?show_scroll_indicator:bool ->
  ?activate_on_click:bool ->
  ?wheel_navigation:bool ->
  ?row_styles:Ansi.Style.t list ->
  ?wrap_selection:bool ->
  ?key_bindings:(Shortcut.t * Table.action) list ->
  ?fast_scroll_step:int ->
  on_change:('a -> 'msg) ->
  ?on_activate:('a -> 'msg) ->
  ?on_hover:('a option -> 'msg option) ->
  equal:('a -> 'a -> bool) ->
  selected:'a option ->
  unit ->
  'msg t
(** [table ~equal ~selected ~on_change ?rows ()] displays rows whose selection
    follows their application values under [equal]. [rows] defaults to [[]].
    [None] or a value absent from [rows] displays no selection; activation
    without selection does nothing. Values must be distinct under [equal].
    Change and activation callbacks receive the row value. A left press captures
    that value before its hover callback: filtering or reordering rows during
    the press preserves the clicked value for selection and activation. Removing
    the table cancels the remaining callbacks. [on_hover] receives [None] when
    the pointer leaves all rows.

    When focused, the table acts on the keys {!Mosaic_ui.Table} lists. Each
    declaration performs a linear selection lookup and row conversion.
    Event-to-value lookup is constant-time. *)

val tree :
  ?key:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?items:Tree.item list ->
  ?indent_size:int ->
  ?show_guides:bool ->
  ?guide_style:Border.t ->
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
  ?key_bindings:(Shortcut.t * Tree.action) list ->
  ?fast_scroll_step:int ->
  on_change:(Tree.State.t -> 'msg) ->
  ?on_activate:(Tree.item -> 'msg) ->
  state:Tree.State.t ->
  unit ->
  'msg t
(** [tree ()] is a tree view. Widget-specific visual options follow
    {!Mosaic_ui.Tree}, which lists the keys the tree acts on when focused;
    layout and paint use the labelled arguments above. *)

module Reconciler = Reconciler
