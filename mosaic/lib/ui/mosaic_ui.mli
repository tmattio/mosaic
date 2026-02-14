(** Terminal UI toolkit for building efficient rendering systems.

    Mosaic_ui provides the core building blocks for terminal user interfaces:
    renderables, a renderer, and UI components. The primary API centers on
    {!module-Renderable} and {!module-Renderer}, which give full control over
    node creation, tree manipulation, layout, and rendering for building
    efficient reconcilers and rendering loops.

    {1 Typical Usage}

    Create a renderer, build a tree of renderables, and run a render loop:

    {[
      let renderer = Renderer.create () in
      match Renderer.create_node renderer ~id:"root" () with
      | Error _ -> failwith "Failed to create root"
      | Ok root ->
          let _ = Box.mount ~props:(Box.Props.make ~border:true ()) root in
          ignore (Renderer.set_root renderer root)
      (* Run your rendering loop using Renderer.render_frame *)
    ]}

    {1 Element API}

    For testing and simple UIs, an element API provides a declarative layer over
    the renderable system. Elements describe UI trees that get instantiated into
    renderables via {!instantiate}:

    {[
      let ui = box ~id:"root" ~border:true [ text ~id:"greeting" "Hello" ] in
      match instantiate renderer ui with
      | Ok node -> ignore (Renderer.set_root renderer node)
      | Error _ -> failwith "Failed to instantiate"
    ]}

    This API is not intended for production reconcilers, which should use the
    {!module-Renderable} and {!module-Renderer} APIs directly for optimal
    performance. *)

module Renderable = Renderable
module Renderer = Renderer
module Border = Grid.Border
module Event = Event

(** {1 Renderables} *)

module Box = Box
module Text = Text
module Canvas = Canvas
module Table = Table
module Slider = Slider
module Select = Select
module Spinner = Spinner
module Tab_select = Tab_select
module Scroll_bar = Scroll_bar
module Scroll_box = Scroll_box
module Text_input = Text_input
module Code = Code
module Text_surface = Text_surface

(** {1 Dimension Helpers} *)

val px : int -> Toffee.Style.dimension
(** [px n] creates a pixel dimension from an integer. *)

val pct : int -> Toffee.Style.dimension
(** [pct n] creates a percentage dimension from an integer (0-100). *)

val size :
  width:int -> height:int -> Toffee.Style.dimension Toffee.Geometry.size
(** [size ~width ~height] creates a size in pixels. *)

val gap : int -> Toffee.Style.length_percentage Toffee.Geometry.size
(** [gap n] creates a uniform gap size in pixels. *)

val auto : Toffee.Style.dimension
(** [auto] is the auto dimension value for flexible sizing. *)

val padding : int -> Toffee.Style.length_percentage Toffee.Geometry.rect
(** [padding n] creates uniform padding in pixels on all sides. *)

val margin : int -> Toffee.Style.length_percentage_auto Toffee.Geometry.rect
(** [margin n] creates uniform margin in pixels on all sides. *)

val inset : int -> Toffee.Style.length_percentage_auto Toffee.Geometry.rect
(** [inset n] creates uniform inset in pixels on all sides. *)

(** {1 Core Types} *)

type renderer = Renderer.t
(** Alias for {!Renderer.t}. *)

type renderable = Renderable.t
(** Alias for {!Renderable.t}. *)

type 'props renderable_component =
  renderer -> 'props -> (renderable, Renderer.error) result
(** [renderable_component] is a function that creates a renderable node.

    It receives a renderer and props, returning either a configured renderable
    or an error. Renderable components mount directly onto renderer nodes and
    handle their own node creation and setup. *)

type 'props functional_component = 'props -> child list -> element
(** [functional_component] is a function that returns child elements.

    Functional components enable composition without direct renderer
    interaction. They accept props and children, returning an element tree for
    further processing. *)

(** [component] discriminates between renderable and functional components. *)
and 'props component =
  | Renderable of 'props renderable_component
      (** Component that creates a renderable node *)
  | Functional of 'props functional_component
      (** Component that returns child elements *)

and element =
  | Element : {
      component : 'props component;
      props : 'props;
      children : child list;
      on_mount : (renderable -> unit) list;
    }
      -> element
      (** [element] is an opaque UI description carrying a component, props,
          children, and lifecycle hooks.

          Elements are created via constructors like {!make}, {!box}, and
          {!text}. The [on_mount] field stores callbacks invoked after
          renderable instantiation. *)

and child =
  | Child of element
  | Node of renderable
  | List of child list
  | Null
      (** [child] represents a child in the UI tree.

          - [Child element]: Nested element requiring instantiation
          - [Node node]: Pre-instantiated renderable node
          - [List children]: Flattened during processing
          - [Null]: Ignored during instantiation *)

(** {1 Error Conversion} *)

val renderer_error_of : Renderable.error -> Renderer.error
(** [renderer_error_of err] converts a renderable error to a renderer error. *)

(** {1 Tree Construction} *)

val flatten : child list -> child list
(** [flatten children] flattens nested lists and removes [Null] nodes.

    Recursively expands [List] constructors and filters out [Null] values,
    producing a normalized child list for tree instantiation. The result
    contains only [Child] and [Node] variants.

    {[
      flatten [ Child e1; List [ Child e2; Null ]; Child e3 ]
      (* Returns: [ Child e1; Child e2; Child e3 ] *)
    ]} *)

val make : 'a component -> 'a -> child list -> element
(** [make component props children] constructs an element.

    This is the low-level constructor for building elements. Prefer high-level
    constructors like {!box} and {!text} for typical use. *)

val on_mount : (renderable -> unit) -> element -> element
(** [on_mount fn element] registers a callback invoked after instantiation.

    The callback receives the instantiated renderable node. Multiple [on_mount]
    calls accumulate handlers, which run in reverse order (newest first) during
    instantiation. *)

(** {1 Instantiation} *)

val instantiate : renderer -> element -> (renderable, Renderer.error) result
(** [instantiate renderer element] converts an element tree to renderable nodes.

    Recursively instantiates the element and its children, appending child nodes
    to parents. For functional components, expands them first. For renderable
    components, creates the node, invokes [on_mount] hooks, then processes
    children.

    Invariant: All children are attached to their parent after successful
    instantiation.

    Raises [Layout_error] if layout operations fail.

    Raises [Tree_mismatch] if tree structure is invalid. *)

val instantiate_child :
  renderer -> child -> (renderable option, Renderer.error) result
(** [instantiate_child renderer child] instantiates a single child node.

    Returns [Ok None] for [Null] and [List] children, [Ok (Some node)] for
    [Child] and [Node] variants. *)

val add_child :
  parent:renderable -> renderer -> child -> (unit, Renderer.error) result
(** [add_child ~parent renderer child] attaches a child to a parent node.

    Handles all [child] variants: instantiates [Child] elements, appends [Node]
    directly, recursively processes [List], and ignores [Null]. *)

(** {1 Node Creation} *)

val create_node :
  renderer ->
  ?id:string ->
  ?host_props:Renderable.Props.t ->
  ?style:Toffee.Style.t ->
  unit ->
  (renderable, Renderer.error) result
(** [create_node renderer ?id ?host_props ?style ()] creates a renderable node.

    Creates a detached node via [Renderer.create_node] and optionally applies a
    layout style. This is the foundation for renderable component constructors.
*)

(** {1 Built-in Components} *)

val box :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?background:Ansi.Color.t ->
  ?border:bool ->
  ?border_sides:Grid.Border.side list ->
  ?border_style:Grid.Border.t ->
  ?border_color:Ansi.Color.t ->
  ?focused_border_color:Ansi.Color.t ->
  ?should_fill:bool ->
  ?custom_border_chars:Grid.Border.t ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  ?on_mount:(Box.t -> unit) ->
  element list ->
  element
(** [box ...children] creates a box container element.

    Boxes provide borders, backgrounds, and layout containers. All style and
    props parameters are inlined for convenience.

    {2 Identity and Host Props}
    - [id]: Node identifier (auto-generated if not provided)
    - [visible]: Whether the node is visible (default true)
    - [z_index]: Z-order for overlapping nodes (default 0)
    - [buffer]: Buffer mode for rendering (default [`None])
    - [live]: Whether node updates trigger re-renders (default false)

    {2 Layout Style}
    - [display], [box_sizing], [position], [overflow], [scrollbar_width]
    - [inset], [size], [min_size], [max_size], [aspect_ratio]
    - [margin], [padding], [gap]

    {2 Alignment}
    - [align_items], [align_self], [align_content]
    - [justify_items], [justify_self], [justify_content]

    {2 Flexbox}
    - [flex_direction], [flex_wrap], [flex_grow], [flex_shrink], [flex_basis]

    {2 Grid}
    - [grid_template_rows], [grid_template_columns]
    - [grid_auto_rows], [grid_auto_columns], [grid_auto_flow]
    - [grid_template_areas], [grid_row], [grid_column]

    {2 Box Props}
    - [background]: Background color
    - [border]: Enable border rendering (default false)
    - [border_sides]: Which sides to render (default all four)
    - [border_style]: Border character style (default single)
    - [border_color]: Border foreground color
    - [focused_border_color]: Border color when focused
    - [should_fill]: Fill background (default true)
    - [custom_border_chars]: Custom border characters
    - [title]: Title text displayed in border
    - [title_alignment]: Title position (default [`Left]) *)

val text :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?style:Ansi.Style.t ->
  ?wrap_mode:[ `None | `Char | `Word ] ->
  ?tab_indicator:int ->
  ?tab_indicator_color:Ansi.Color.t ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?selectable:bool ->
  ?on_mount:(Text.t -> unit) ->
  string ->
  element
(** [text ...] creates a text element.

    Text elements render styled text content. All style and props parameters are
    inlined for convenience.

    {2 Identity and Host Props}
    - [id]: Node identifier (auto-generated if not provided)
    - [visible]: Whether the node is visible (default true)
    - [z_index]: Z-order for overlapping nodes (default 0)
    - [buffer]: Buffer mode for rendering (default [`None])
    - [live]: Whether node updates trigger re-renders (default false)

    {2 Layout Style}
    Same as {!box}.

    {2 Text Props}
    - [style]: Default ANSI style for text
    - [content]: Text content string
    - [wrap_mode]: Line wrapping behavior (default [`Word])
    - [tab_indicator]: Character code for tab indicator
    - [tab_indicator_color]: Color for tab indicators
    - [selection_bg]: Background color for selected text
    - [selection_fg]: Foreground color for selected text
    - [selectable]: Whether text can be selected (default true) *)

val canvas :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?respect_alpha:bool ->
  ?width_method:Glyph.width_method ->
  ?initial_width:int ->
  ?initial_height:int ->
  ?draw:(Grid.t -> width:int -> height:int -> unit) ->
  ?on_mount:(Canvas.t -> unit) ->
  unit ->
  element
(** [canvas ...] creates a canvas element for direct drawing.

    Canvas elements provide a surface for arbitrary pixel-based drawing.

    {2 Identity and Host Props}
    - [id]: Node identifier (auto-generated if not provided)

    {2 Canvas Props}
    - [respect_alpha]: Whether to respect alpha channel (default false)
    - [width_method]: Glyph width calculation method
    - [initial_width]: Initial buffer width (default 1)
    - [initial_height]: Initial buffer height (default 1)
    - [draw]: Draw callback invoked on each render with canvas dimensions *)

val table :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?box_style:Table.box_style ->
  ?safe_box:bool ->
  ?table_padding:Table.padding ->
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
  ?title:Table.cell ->
  ?title_style:Ansi.Style.t ->
  ?title_justify:Table.justify ->
  ?caption:Table.cell ->
  ?caption_style:Ansi.Style.t ->
  ?caption_justify:Table.justify ->
  ?table_width:int ->
  ?table_min_width:int ->
  ?on_mount:(Table.t -> unit) ->
  columns:Table.column list ->
  rows:Table.row list ->
  unit ->
  element
(** [table ~columns ~rows ...] creates a table element.

    Tables render structured data with headers, footers, and various box styles.

    {2 Identity and Host Props}
    - [id]: Node identifier (auto-generated if not provided)

    {2 Table Props}
    - [columns]: Column definitions
    - [rows]: Row data
    - [box_style]: Border style (default Heavy_head)
    - [title]: Table title
    - [caption]: Table caption
    - [show_header]: Show header row (default true)
    - [show_footer]: Show footer row (default false)
    - [show_lines]: Show lines between rows (default false)
    - [expand]: Expand table to fill available width (default false) *)

val slider :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  orientation:Slider.orientation ->
  ?min:float ->
  ?max:float ->
  ?value:float ->
  ?viewport_size:float ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  ?on_change:(float -> unit) ->
  ?on_mount:(Slider.t -> unit) ->
  unit ->
  element
(** [slider ~orientation ...] creates a slider element.

    Sliders provide draggable value selectors with sub-cell precision.

    {2 Slider Props}
    - [orientation]: Horizontal or vertical layout (required)
    - [min]: Minimum value (default 0)
    - [max]: Maximum value (default 100)
    - [value]: Current value (default 0)
    - [viewport_size]: Thumb size relative to range
    - [track_color]: Background track color
    - [thumb_color]: Draggable thumb color
    - [on_change]: Callback fired on value change *)

val select :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?background:Ansi.Color.t ->
  ?text_color:Ansi.Color.t ->
  ?focused_background:Ansi.Color.t ->
  ?focused_text_color:Ansi.Color.t ->
  ?selected_background:Ansi.Color.t ->
  ?selected_text_color:Ansi.Color.t ->
  ?description_color:Ansi.Color.t ->
  ?selected_description_color:Ansi.Color.t ->
  ?show_scroll_indicator:bool ->
  ?wrap_selection:bool ->
  ?show_description:bool ->
  ?item_spacing:int ->
  ?fast_scroll_step:int ->
  ?selected_index:int ->
  ?autofocus:bool ->
  ?on_mount:(Select.t -> unit) ->
  Select.item list ->
  element
(** [select options] creates a vertical list selector element.

    Select provides a focusable, keyboard-navigable list.

    {2 Select Props}
    - [wrap_selection]: Wrap around at edges (default false)
    - [show_description]: Show item descriptions (default true)
    - [show_scroll_indicator]: Show scroll position (default false) *)

val spinner :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?preset:Spinner.preset ->
  ?frames:string array ->
  ?interval:float ->
  ?autoplay:bool ->
  ?color:Ansi.Color.t ->
  ?spinner_background:Ansi.Color.t ->
  ?on_mount:(Spinner.t -> unit) ->
  unit ->
  element
(** [spinner ...] creates an animated spinner element.

    Spinners display cycling animations for loading states.

    {2 Spinner Props}
    - [preset]: Built-in animation style (default Dots)
    - [frames]: Custom frame sequence
    - [interval]: Seconds between frames (default 0.08)
    - [autoplay]: Start animation on mount (default true)
    - [color]: Foreground color
    - [spinner_background]: Background color *)

val tab_select :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?wrap_selection:bool ->
  ?show_description:bool ->
  ?show_underline:bool ->
  ?show_scroll_arrows:bool ->
  ?mouse_navigation:bool ->
  ?autofocus:bool ->
  ?tab_width:int ->
  ?background:Ansi.Color.t ->
  ?text_color:Ansi.Color.t ->
  ?focused_background:Ansi.Color.t ->
  ?focused_text:Ansi.Color.t ->
  ?selected_background:Ansi.Color.t ->
  ?selected_text:Ansi.Color.t ->
  ?selected_description:Ansi.Color.t ->
  ?on_mount:(Tab_select.t -> unit) ->
  (string * string option) list ->
  element
(** [tab_select options] creates a horizontal tab selector element.

    Tab selectors provide keyboard and mouse navigable tab bars.

    {2 Tab_select Props}
    - [wrap_selection]: Wrap around at edges (default false)
    - [show_description]: Show tab descriptions (default false)
    - [show_underline]: Show selection underline (default true)
    - [tab_width]: Fixed tab width in cells *)

val scroll_bar :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  orientation:Scroll_bar.orientation ->
  ?show_arrows:bool ->
  ?arrow_style:Scroll_bar.arrow_style ->
  ?track_style:Scroll_bar.track_style ->
  ?track_viewport_size:int ->
  ?on_change:(int -> unit) ->
  ?autofocus:bool ->
  ?on_mount:(Scroll_bar.t -> unit) ->
  unit ->
  element
(** [scroll_bar ~orientation ...] creates a scroll bar element.

    Scroll bars provide programmatic scrolling with optional arrow buttons.

    {2 Scroll_bar Props}
    - [orientation]: Vertical or horizontal (required)
    - [show_arrows]: Show arrow buttons (default true)
    - [arrow_style]: Arrow button styling
    - [track_style]: Track and thumb colors
    - [on_change]: Callback fired on scroll position change
    - [autofocus]: Request focus on mount (default false) *)

val scroll_box :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?background:Ansi.Color.t ->
  ?scroll_x:bool ->
  ?scroll_y:bool ->
  ?scroll_acceleration:[ `Linear | `MacOS ] ->
  ?sticky_scroll:bool ->
  ?sticky_start:[ `Top | `Bottom | `Left | `Right ] ->
  ?viewport_culling:bool ->
  ?autofocus:bool ->
  ?on_mount:(Scroll_box.t -> unit) ->
  element list ->
  element
(** [scroll_box ...children] creates a scrollable container element.

    Scroll boxes provide viewport clipping and scroll management.

    {2 Scroll_box Props}
    - [scroll_x]: Enable horizontal scrolling (default false)
    - [scroll_y]: Enable vertical scrolling (default true)
    - [scroll_acceleration]: Scroll speed mode (default Linear)
    - [sticky_scroll]: Enable sticky scrolling (default false)
    - [sticky_start]: Initial sticky edge
    - [viewport_culling]: Only render visible children (default true)
    - [autofocus]: Request focus on mount (default false) *)

val input :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?background:Ansi.Color.t ->
  ?text_color:Ansi.Color.t ->
  ?focused_background:Ansi.Color.t ->
  ?focused_text_color:Ansi.Color.t ->
  ?placeholder:string ->
  ?placeholder_color:Ansi.Color.t ->
  ?cursor_color:Ansi.Color.t ->
  ?cursor_style:Text_input.cursor_style ->
  ?cursor_blinking:bool ->
  ?max_length:int ->
  ?value:string ->
  ?autofocus:bool ->
  ?on_mount:(Text_input.t -> unit) ->
  unit ->
  element
(** [text_input ...] creates a single-line text input element.

    Text inputs provide focusable text fields with cursor navigation.

    {2 Text_input Props}
    - [placeholder]: Placeholder text when empty
    - [value]: Initial text content
    - [max_length]: Maximum character count
    - [cursor_style]: Block, line, or underline cursor
    - [cursor_blinking]: Enable cursor blinking (default true)
    - [autofocus]: Request focus on mount (default false) *)

val code :
  ?id:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?buffer:Renderable.Props.buffer_mode ->
  ?live:bool ->
  ?display:Toffee.Style.display ->
  ?box_sizing:Toffee.Style.box_sizing ->
  ?position:Toffee.Style.position ->
  ?overflow:Toffee.Style.overflow Toffee.Geometry.point ->
  ?scrollbar_width:float ->
  ?inset:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?min_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?max_size:Toffee.Style.dimension Toffee.Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:Toffee.Style.length_percentage_auto Toffee.Geometry.rect ->
  ?padding:Toffee.Style.length_percentage Toffee.Geometry.rect ->
  ?gap:Toffee.Style.length_percentage Toffee.Geometry.size ->
  ?align_items:Toffee.Style.align_items ->
  ?align_self:Toffee.Style.align_self ->
  ?align_content:Toffee.Style.align_content ->
  ?justify_items:Toffee.Style.justify_items ->
  ?justify_self:Toffee.Style.justify_self ->
  ?justify_content:Toffee.Style.justify_content ->
  ?flex_direction:Toffee.Style.flex_direction ->
  ?flex_wrap:Toffee.Style.flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:Toffee.Style.dimension ->
  ?grid_template_rows:Toffee.Style.grid_template_component list ->
  ?grid_template_columns:Toffee.Style.grid_template_component list ->
  ?grid_auto_rows:Toffee.Style.track_sizing_function list ->
  ?grid_auto_columns:Toffee.Style.track_sizing_function list ->
  ?grid_auto_flow:Toffee.Style.grid_auto_flow ->
  ?grid_template_areas:Toffee.Style.grid_template_area list ->
  ?grid_row:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?grid_column:Toffee.Style.grid_placement Toffee.Geometry.line ->
  ?filetype:Mosaic_syntax.filetype ->
  ?languages:Mosaic_syntax.Set.t ->
  ?theme:Code.Theme.t ->
  ?conceal:bool ->
  ?draw_unstyled_text:bool ->
  ?wrap_mode:Code.Props.wrap_mode ->
  ?tab_width:int ->
  ?tab_indicator:int ->
  ?tab_indicator_color:Ansi.Color.t ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?selectable:bool ->
  ?on_mount:(Code.t -> unit) ->
  string ->
  element
(** [code content] creates a syntax-highlighted code element.

    Code elements render text with Tree-sitter syntax highlighting via
    [Mosaic_syntax].

    {2 Code Props}
    - [content]: Source code text
    - [filetype]: Language identifier for highlighting
    - [languages]: Set of available languages (default: built-ins)
    - [theme]: Color theme for syntax highlighting
    - [wrap_mode]: Line wrapping behavior (default Word)
    - [selectable]: Enable text selection (default true) *)

(** {1 Rendering} *)

val render : ?width:int -> ?height:int -> ?colors:bool -> element -> string
(** [render ?width ?height ?colors element] renders an element to a string.

    Creates a temporary renderer, instantiates the element, performs layout, and
    returns the rendered output as a string.

    - [width]: Layout width (default 80, or element's computed width if unset)
    - [height]: Layout height (default 40, or element's computed height if
      unset)
    - [colors]: Include ANSI color codes (default true). When false and no
      explicit dimensions, the output is trimmed via [trim_snapshot].

    Raises [Failure] if instantiation or layout fails. *)

val print : ?width:int -> ?height:int -> ?colors:bool -> element -> unit
(** [print ?width ?height ?colors element] renders an element to stdout.

    Equivalent to [render] followed by printing to stdout with a trailing
    newline. Useful for expect tests and debugging. *)
