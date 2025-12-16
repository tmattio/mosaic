(** Virtual node representation for declarative UI descriptions.

    Vnodes describe what the UI should look like. The reconciler diffs vnode
    trees and applies minimal mutations to the underlying Renderable tree. *)

(** {1 Internal Types}

    These types are exposed for the reconciler implementation. Application code
    should use the constructors below instead of constructing these directly. *)

type tag =
  | Box
  | Text
  | Canvas
  | Table
  | Slider
  | Select
  | Spinner
  | Tab_select
  | Scroll_bar
  | Scroll_box
  | Text_input
  | Code
  | Markdown  (** The type of widget this vnode represents. *)

type 'a handlers = {
  on_mouse : (Mosaic_ui.Event.mouse -> 'a) option;
  on_key : (Mosaic_ui.Event.key -> 'a) option;
  on_paste : (Mosaic_ui.Event.paste -> 'a) option;
}
(** Event handlers for a vnode, parameterized by the handler return type. *)

type 'a slider_spec = {
  slider_props : Mosaic_ui.Slider.Props.t;
  slider_on_change : (float -> 'a) option;
}

type 'a select_spec = {
  select_props : Mosaic_ui.Select.Props.t;
  select_on_change : (int -> 'a) option;
  select_on_activate : (int -> 'a) option;
}

type 'a scroll_bar_spec = {
  scroll_bar_props : Mosaic_ui.Scroll_bar.Props.t;
  scroll_bar_on_change : (int -> 'a) option;
}

type 'a tab_select_spec = {
  tab_select_props : Mosaic_ui.Tab_select.Props.t;
  tab_select_on_change : (int -> 'a) option;
  tab_select_on_activate : (int -> 'a) option;
  tab_select_on_change_full : (int * (string * string option) -> 'a) option;
  tab_select_on_activate_full : (int * (string * string option) -> 'a) option;
}

type 'a text_input_spec = {
  text_input_props : Mosaic_ui.Text_input.Props.t;
  text_input_on_input : (string -> 'a) option;
  text_input_on_change : (string -> 'a) option;
  text_input_on_submit : (string -> 'a) option;
}

type 'a scroll_box_spec = {
  scroll_box_props : Mosaic_ui.Scroll_box.Props.t;
  scroll_box_on_scroll : (x:int -> y:int -> 'a) option;
}

type box_spec = Mosaic_ui.Box.Props.t
(** Box-specific properties, using [Box.Props.t]. *)

type text_spec = Mosaic_ui.Text.Props.t
(** Text-specific properties, using [Text.Props.t]. *)

type canvas_spec = {
  props : Mosaic_ui.Canvas.Props.t;
  draw : (Mosaic_ui.Canvas.t -> width:int -> height:int -> unit) option;
}
(** Canvas-specific properties: renderable props plus optional draw callback. *)

type 'a spec =
  | Box_spec of box_spec
  | Text_spec of text_spec
  | Canvas_spec of canvas_spec
  | Table_spec of Mosaic_ui.Table.Props.t
  | Slider_spec of 'a slider_spec
  | Select_spec of 'a select_spec
  | Spinner_spec of Mosaic_ui.Spinner.Props.t
  | Tab_select_spec of 'a tab_select_spec
  | Scroll_bar_spec of 'a scroll_bar_spec
  | Scroll_box_spec of 'a scroll_box_spec
  | Text_input_spec of 'a text_input_spec
  | Code_spec of Mosaic_ui.Code.Props.t
  | Markdown_spec of Mosaic_markdown.Props.t
      (** Widget-specific configuration. *)

type 'a props = {
  id : string option;
  style : Toffee.Style.t;
  visible : bool;
  z_index : int;
  live : bool;
  buffer : Mosaic_ui.Renderable.Props.buffer_mode;
  handlers : 'a handlers;
  ref : (Mosaic_ui.Renderable.t -> unit) option;
  spec : 'a spec;
}
(** Common properties shared by all vnodes, parameterized by handler return
    type. The [id] is optional - if not provided, a stable ID will be generated
    when the fiber is created. *)

(** {1 Core Types} *)

type 'a t =
  | Element of 'a element
  | Fragment of 'a t list
  | Raw of Mosaic_ui.Renderable.t
  | Null
      (** A virtual node in the UI tree, parameterized by handler return type.

          - [Element]: A widget element with tag, props, and children
          - [Fragment]: Groups multiple vnodes without a wrapper
          - [Raw]: Embeds a pre-existing Renderable node (escape hatch)
          - [Null]: Renders nothing *)

and 'a element = {
  tag : tag;
  key : string option;
  props : 'a props;
  children : 'a t list;
}
(** An element vnode with tag, optional key, props, and children. *)

(** {1 Constructors} *)

val null : 'a t
(** [null] is a vnode that renders nothing. Useful for conditional rendering. *)

val fragment : 'a t list -> 'a t
(** [fragment children] groups multiple vnodes without a wrapper element. *)

val raw : Mosaic_ui.Renderable.t -> 'a t
(** [raw node] embeds an existing Renderable node into the vnode tree.

    This is an escape hatch for mixing imperative and declarative code. The node
    will be attached to the tree but not managed by the reconciler (no diffing,
    no updates). *)

val box :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  'a t list ->
  'a t
(** [box children] creates a box container vnode.

    @param key Reconciliation key for efficient diffing
    @param visible Whether the node is visible (default true)
    @param z_index Z-order for overlapping nodes (default 0)
    @param live Whether updates trigger re-renders (default false)
    @param buffer Buffer mode for rendering
    @param ref Callback invoked with the underlying Renderable after mount
    @param id Node identifier (auto-generated if not provided) *)

val text :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  ?text_style:Ansi.Style.t ->
  ?wrap_mode:[ `None | `Char | `Word ] ->
  ?tab_indicator:int ->
  ?tab_indicator_color:Ansi.Color.t ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?selectable:bool ->
  string ->
  'a t
(** [text content] creates a text element vnode.

    @param text_style ANSI style for text
    @param wrap_mode Line wrapping behavior *)

val canvas :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  ?width_method:[ `Unicode | `Wcwidth | `No_zwj ] ->
  ?initial_width:int ->
  ?initial_height:int ->
  ?draw:(Mosaic_ui.Canvas.t -> width:int -> height:int -> unit) ->
  unit ->
  'a t
(** [canvas ()] creates a canvas vnode for custom drawing.

    @param draw Callback invoked on each render with canvas and dimensions *)

val table :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  ?box_style:Mosaic_ui.Table.box_style ->
  ?safe_box:bool ->
  ?table_padding:Mosaic_ui.Table.padding ->
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
  ?title:Mosaic_ui.Table.cell ->
  ?title_style:Ansi.Style.t ->
  ?title_justify:Mosaic_ui.Table.justify ->
  ?caption:Mosaic_ui.Table.cell ->
  ?caption_style:Ansi.Style.t ->
  ?caption_justify:Mosaic_ui.Table.justify ->
  ?table_width:int ->
  ?table_min_width:int ->
  columns:Mosaic_ui.Table.column list ->
  rows:Mosaic_ui.Table.row list ->
  unit ->
  'a t
(** [table ~columns ~rows ()] creates a table vnode.

    @param columns Column definitions (required)
    @param rows Row data (required) *)

val slider :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  orientation:Mosaic_ui.Slider.orientation ->
  ?min:float ->
  ?max:float ->
  ?value:float ->
  ?viewport_size:float ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  ?on_change:(float -> 'a) ->
  unit ->
  'a t
(** [slider ~orientation ()] creates a slider vnode.

    @param orientation Horizontal or vertical (required) *)

val select :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  ?on_change:(int -> 'a) ->
  ?on_activate:(int -> 'a) ->
  Mosaic_ui.Select.item list ->
  'a t
(** [select options] creates a select list vnode. *)

val spinner :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  ?preset:Mosaic_ui.Spinner.preset ->
  ?frames:string array ->
  ?interval:float ->
  ?autoplay:bool ->
  ?color:Ansi.Color.t ->
  ?spinner_background:Ansi.Color.t ->
  unit ->
  'a t
(** [spinner ()] creates an animated spinner vnode. *)

val tab_select :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  ?on_change:(int -> 'a) ->
  ?on_activate:(int -> 'a) ->
  ?on_change_full:(int * (string * string option) -> 'a) ->
  ?on_activate_full:(int * (string * string option) -> 'a) ->
  (string * string option) list ->
  'a t
(** [tab_select options] creates a tab selector vnode. *)

val scroll_bar :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  orientation:Mosaic_ui.Scroll_bar.orientation ->
  ?show_arrows:bool ->
  ?arrow_style:Mosaic_ui.Scroll_bar.arrow_style ->
  ?track_style:Mosaic_ui.Scroll_bar.track_style ->
  ?track_viewport_size:int ->
  ?on_change:(int -> 'a) ->
  ?autofocus:bool ->
  unit ->
  'a t
(** [scroll_bar ~orientation ()] creates a scroll bar vnode.

    @param orientation Vertical or horizontal (required) *)

val scroll_box :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  ?on_scroll:(x:int -> y:int -> 'a) ->
  'a t list ->
  'a t
(** [scroll_box children] creates a scrollable container vnode. *)

val input :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  ?cursor_style:Mosaic_ui.Text_input.cursor_style ->
  ?cursor_blinking:bool ->
  ?max_length:int ->
  ?value:string ->
  ?autofocus:bool ->
  ?on_input:(string -> 'a) ->
  ?on_change:(string -> 'a) ->
  ?on_submit:(string -> 'a) ->
  unit ->
  'a t
(** [text_input ()] creates a text input vnode. *)

val code :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  ?theme:Mosaic_ui.Code.Theme.t ->
  ?conceal:bool ->
  ?draw_unstyled_text:bool ->
  ?wrap_mode:Mosaic_ui.Code.Props.wrap_mode ->
  ?tab_width:int ->
  ?tab_indicator:int ->
  ?tab_indicator_color:Ansi.Color.t ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?selectable:bool ->
  string ->
  'a t
(** [code content] creates a syntax-highlighted code vnode. *)

val markdown :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Mosaic_ui.Event.mouse -> 'a) ->
  ?on_key:(Mosaic_ui.Event.key -> 'a) ->
  ?on_paste:(Mosaic_ui.Event.paste -> 'a) ->
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
  ?style:Mosaic_markdown.Style.t ->
  ?wrap_width:Mosaic_markdown.Props.wrap_width ->
  ?paragraph_wrap:Mosaic_markdown.Props.wrap_mode ->
  ?block_quote_wrap:Mosaic_markdown.Props.wrap_mode ->
  ?headings:Mosaic_markdown.Props.headings ->
  ?code_blocks:Mosaic_markdown.Props.code_blocks ->
  ?raw_html:Mosaic_markdown.Props.raw_html ->
  ?links:Mosaic_markdown.Props.link ->
  ?images:Mosaic_markdown.Props.image ->
  ?unknown_inline:Mosaic_markdown.Props.unknown ->
  ?unknown_block:Mosaic_markdown.Props.unknown ->
  ?languages:Mosaic_syntax.Set.t ->
  string ->
  'a t
(** [markdown content] creates a markdown rendering vnode. *)

(** {1 Handler Transformation} *)

val map_handlers : ('a -> 'b) -> 'a t -> 'b t
(** [map_handlers f vnode] transforms handler return values using [f].

    This is useful for converting between different handler types, e.g., from
    [unit t] to ['msg option t] for TEA integration. *)

(** {1 Introspection} *)

val tag_equal : tag -> tag -> bool
(** [tag_equal a b] returns true if tags are the same variant. *)

val tag_of : 'a t -> tag option
(** [tag_of vnode] returns the tag if vnode is an Element. *)

val key_of : 'a t -> string option
(** [key_of vnode] returns the key if vnode is an Element with a key. *)

val children_of : 'a t -> 'a t list
(** [children_of vnode] returns the children of a vnode. *)

(** {1 Instantiation} *)

val instantiate :
  Mosaic_ui.renderer ->
  _ t ->
  (Mosaic_ui.renderable option, Mosaic_ui.Renderable.error) result
(** [instantiate renderer element] converts an element tree to renderable nodes.

    Recursively instantiates the element and its children, appending child nodes
    to parents. For functional components, expands them first. For renderable
    components, creates the node, invokes [on_mount] hooks, then processes
    children.

    Invariant: All children are attached to their parent after successful
    instantiation.

    @raise Layout_error if layout operations fail.
    @raise Tree_mismatch if tree structure is invalid. *)

(** {1 Rendering} *)

val render : ?width:int -> ?height:int -> ?colors:bool -> _ t -> string
(** [render ?width ?height ?colors vnode] renders a vnode to a string.

    Creates a temporary renderer, instantiates the vnode tree, performs layout,
    and returns the rendered output as a string.

    - [width]: Layout width (default 80, or element's computed width if unset)
    - [height]: Layout height (default 40, or element's computed height if
      unset)
    - [colors]: Include ANSI color codes (default true). When false and no
      explicit dimensions, the output is trimmed.

    @raise Failure if instantiation or layout fails. *)

val print : ?width:int -> ?height:int -> ?colors:bool -> _ t -> unit
(** [print ?width ?height ?colors vnode] renders a vnode to stdout.

    Equivalent to [render] followed by printing to stdout with a trailing
    newline. Useful for expect tests and debugging. *)

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
