(** TEA (The Elm Architecture) runtime for Mosaic.

    This module provides a declarative, functional API for building terminal
    applications using the Model-View-Update pattern. *)

module Event = Mosaic_ui.Event

(** {1 Views}

    TEA views are parameterized by a message type. Event handlers return
    [Some msg] to dispatch a message, or [None] to ignore the event. This keeps
    view functions pure - the runtime handles dispatch. *)

type 'msg t = 'msg option Vnode.t
(** A view node parameterized by message type ['msg]. Handlers return
    ['msg option] - [Some msg] to dispatch, [None] to ignore. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f view] transforms messages in a view using function [f]. Useful for
    composing views with different message types. *)

(** {1 Commands}

    Commands represent side effects to be performed by the runtime. *)

module Cmd : sig
  type 'msg t =
    | None
    | Batch of 'msg t list
    | Perform of (('msg -> unit) -> unit)
    | Quit
    | Set_title of string
    | Focus of string
    | Static_write of string
    | Static_print of string
    | Static_clear  (** A command that may produce messages of type ['msg]. *)

  val none : 'msg t
  (** [none] is a command that does nothing. *)

  val batch : 'msg t list -> 'msg t
  (** [batch cmds] combines multiple commands into one. *)

  val perform : (('msg -> unit) -> unit) -> 'msg t
  (** [perform f] creates a command that calls [f] with a dispatch function. Use
      this for async operations that produce messages. *)

  val quit : 'msg t
  (** [quit] exits the application. *)

  val set_title : string -> 'msg t
  (** [set_title title] sets the terminal window title. *)

  val focus : string -> 'msg t
  (** [focus id] focuses the node with the given ID. Does nothing if not found.
  *)

  val static_write : string -> 'msg t
  (** [static_write text] writes text to the primary screen above the renderer.
      Ignored in alternate screen mode. *)

  val static_print : string -> 'msg t
  (** [static_print text] writes text with a trailing newline to the primary
      screen above the renderer. Ignored in alternate screen mode. *)

  val static_clear : 'msg t
  (** [static_clear] clears previously written static content. *)
end

(** {1 Subscriptions}

    Subscriptions allow your application to listen to external events. *)

module Sub : sig
  type 'msg t =
    | None
    | Batch of 'msg t list
    | Every of float * (unit -> 'msg)
    | On_tick of (dt:float -> 'msg)
    | On_key of (Event.key -> 'msg option)
    | On_mouse of (Event.mouse -> 'msg option)
    | On_paste of (Event.paste -> 'msg option)
    | On_resize of (width:int -> height:int -> 'msg)
    | On_focus of 'msg
    | On_blur of 'msg
        (** A subscription that may produce messages of type ['msg]. *)

  val none : 'msg t
  (** [none] is a subscription that does nothing. *)

  val batch : 'msg t list -> 'msg t
  (** [batch subs] combines multiple subscriptions into one. *)

  val every : float -> (unit -> 'msg) -> 'msg t
  (** [every interval f] fires [f ()] every [interval] seconds. Useful for
      periodic tasks like polling or auto-save. *)

  val on_tick : (dt:float -> 'msg) -> 'msg t
  (** [on_tick f] subscribes to frame ticks. [dt] is seconds since last frame.
      Use for animations that need smooth timing. *)

  val on_key : (Event.key -> 'msg option) -> 'msg t
  (** [on_key f] subscribes to keyboard events. Return [Some msg] to dispatch.
  *)

  val on_mouse : (Event.mouse -> 'msg option) -> 'msg t
  (** [on_mouse f] subscribes to mouse events. Return [Some msg] to dispatch. *)

  val on_paste : (Event.paste -> 'msg option) -> 'msg t
  (** [on_paste f] subscribes to paste events. Return [Some msg] to dispatch. *)

  val on_resize : (width:int -> height:int -> 'msg) -> 'msg t
  (** [on_resize f] subscribes to terminal resize events. *)

  val on_focus : 'msg -> 'msg t
  (** [on_focus msg] dispatches [msg] when the terminal window gains focus. *)

  val on_blur : 'msg -> 'msg t
  (** [on_blur msg] dispatches [msg] when the terminal window loses focus. *)
end

(** {1 Application}

    The application record defines your TEA program. *)

type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
      (** [init ()] returns the initial model and command. *)
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
      (** [update msg model] handles a message and returns new model and
          command. *)
  view : 'model -> 'msg t;
      (** [view model] returns the view tree for the current model. *)
  subscriptions : 'model -> 'msg Sub.t;
      (** [subscriptions model] returns active subscriptions for current model.
      *)
}

(** {1 Running} *)

val run :
  ?mode:Matrix.mode ->
  ?raw_mode:bool ->
  ?target_fps:float option ->
  ?respect_alpha:bool ->
  ?mouse_enabled:bool ->
  ?mouse:Matrix.Terminal.mouse_mode option ->
  ?bracketed_paste:bool ->
  ?focus_reporting:bool ->
  ?kitty_keyboard:Matrix.kitty_keyboard ->
  ?exit_on_ctrl_c:bool ->
  ?debug_overlay:bool ->
  ?debug_overlay_corner:Matrix.debug_overlay_corner ->
  ?debug_overlay_capacity:int ->
  ?frame_dump_every:int ->
  ?frame_dump_dir:string ->
  ?frame_dump_pattern:string ->
  ?frame_dump_hits:bool ->
  ?cursor_visible:bool ->
  ?explicit_width:bool ->
  ?render_thread:bool ->
  ?input_timeout:float option ->
  ?resize_debounce:float option ->
  ?initial_caps:Matrix.Terminal.capabilities ->
  ?output:[ `Fd of Unix.file_descr | `Stdout ] ->
  ('model, 'msg) app ->
  unit
(** [run ?mode ?raw_mode ?target_fps ?respect_alpha ?mouse_enabled ?mouse
     ?bracketed_paste ?focus_reporting ?kitty_keyboard ?exit_on_ctrl_c
     ?debug_overlay ?debug_overlay_corner ?debug_overlay_capacity
     ?frame_dump_every ?frame_dump_dir ?frame_dump_pattern ?frame_dump_hits
     ?cursor_visible ?explicit_width ?render_thread ?input_timeout
     ?resize_debounce ?initial_caps ?output app] starts the application and
    blocks until it exits.

    All parameters are forwarded to {!Matrix.create}. Defaults match the
    previous behaviour: alternate screen, raw mode, 60 FPS cap, mouse and
    bracketed paste enabled, and Ctrl+C left to the application
    ([exit_on_ctrl_c] defaults to [false]). *)

(** {1 Internal Modules}

    These modules are exposed for testing and advanced use cases. *)

module Vnode = Vnode
(** Virtual node types for the view tree. *)

module Reconciler = Reconciler
(** The reconciler manages vnode-to-renderable mapping. *)

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

(** {1 UI Elements}

    Constructors for building the view tree. These return message-parameterized
    views where event handlers produce [Some msg] to dispatch or [None] to
    ignore. *)

val null : 'msg t
(** [null] renders nothing. Useful for conditional rendering. *)

val fragment : 'msg t list -> 'msg t
(** [fragment children] groups multiple elements without a wrapper. *)

val raw : Mosaic_ui.Renderable.t -> 'msg t
(** [raw node] embeds an existing Renderable node into the view tree. *)

val box :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  'msg t list ->
  'msg t
(** [box children] creates a box container with optional border and background.
*)

val text :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  ?content:string ->
  ?wrap_mode:[ `None | `Char | `Word ] ->
  ?tab_indicator:int ->
  ?tab_indicator_color:Ansi.Color.t ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?selectable:bool ->
  unit ->
  'msg t
(** [text ()] creates a text element. *)

val canvas :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  'msg t
(** [canvas ()] creates a canvas for custom drawing. *)

(** {2 Table} *)

module Table : sig
  type cell
  (** A table cell. *)

  val cell : ?style:Ansi.Style.t -> string -> cell
  (** [cell ?style text] creates a cell with the given text. *)

  type row
  (** A table row. *)

  val row : ?style:Ansi.Style.t -> cell list -> row
  (** [row ?style cells] creates a row from cells. *)

  type justify = [ `Left | `Center | `Right | `Full ]
  (** Column justification. *)

  type vertical_align = [ `Top | `Middle | `Bottom ]
  (** Vertical alignment within cells. *)

  type overflow = [ `Ellipsis | `Crop | `Fold ]
  (** How to handle overflow text. *)

  type padding = int * int * int * int
  (** Table padding (top, right, bottom, left). *)

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
    | Square_double_head  (** Table border styles. *)

  type column
  (** A column definition. *)

  val column :
    ?header:cell ->
    ?footer:cell ->
    ?style:Ansi.Style.t ->
    ?header_style:Ansi.Style.t ->
    ?footer_style:Ansi.Style.t ->
    ?justify:justify ->
    ?vertical:vertical_align ->
    ?overflow:overflow ->
    ?width:[ `Fixed of int | `Auto ] ->
    ?min_width:int ->
    ?max_width:int ->
    ?ratio:int ->
    ?no_wrap:bool ->
    string ->
    column
  (** [column name] creates a column definition. *)
end

val table :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  columns:Table.column list ->
  rows:Table.row list ->
  unit ->
  'msg t
(** [table ~columns ~rows ()] creates a table element. *)

(** {2 Slider} *)

module Slider : sig
  type orientation = [ `Horizontal | `Vertical ]
  (** Slider orientation. *)
end

val slider :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  ?on_change:(float -> 'msg option) ->
  unit ->
  'msg t
(** [slider ~orientation ()] creates a slider element. *)

(** {2 Select} *)

module Select : sig
  type item = { name : string; description : string option }
  (** A select list item with name and optional description. *)
end

val select :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  ?options:Select.item list ->
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
  ?on_change:(int -> 'msg option) ->
  ?on_activate:(int -> 'msg option) ->
  unit ->
  'msg t
(** [select ()] creates a select list element. *)

(** {2 Spinner} *)

module Spinner : sig
  type preset =
    | Dots
    | Line
    | Circle
    | Bounce
    | Bar
    | Arrow  (** Built-in spinner animation presets. *)
end

val spinner :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  unit ->
  'msg t
(** [spinner ()] creates an animated spinner element. *)

(** {2 Tab Select} *)

module Tab_select : sig
  type tab = { label : string; description : string option }
  (** A tab with label and optional description. *)
end

val tab_select :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  ?options:Tab_select.tab list ->
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
  ?on_change:(int -> 'msg option) ->
  ?on_activate:(int -> 'msg option) ->
  ?on_change_full:(int * Mosaic_ui.Tab_select.tab -> 'msg option) ->
  ?on_activate_full:(int * Mosaic_ui.Tab_select.tab -> 'msg option) ->
  unit ->
  'msg t
(** [tab_select ()] creates a tab selector element. *)

(** {2 Scroll Bar} *)

module Scroll_bar : sig
  type arrow_chars = {
    up : string;
    down : string;
    left : string;
    right : string;
  }
  (** Custom arrow characters. *)

  type arrow_style = {
    foreground : Ansi.Color.t option;
    background : Ansi.Color.t option;
    attributes : Ansi.Attr.flag list option;
    chars : arrow_chars option;
  }
  (** Arrow button styling. *)

  type track_style = {
    track_color : Ansi.Color.t option;
    thumb_color : Ansi.Color.t option;
  }
  (** Track and thumb styling. *)
end

val scroll_bar :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  orientation:[ `Vertical | `Horizontal ] ->
  ?show_arrows:bool ->
  ?arrow_style:Scroll_bar.arrow_style ->
  ?track_style:Scroll_bar.track_style ->
  ?track_viewport_size:int ->
  ?on_change:(int -> 'msg option) ->
  ?autofocus:bool ->
  unit ->
  'msg t
(** [scroll_bar ~orientation ()] creates a scroll bar element. *)

val scroll_box :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  ?on_scroll:(x:int -> y:int -> 'msg option) ->
  'msg t list ->
  'msg t
(** [scroll_box children] creates a scrollable container element. *)

(** {2 Text Input} *)

val text_input :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  ?cursor_style:[ `Block | `Line | `Underline ] ->
  ?cursor_blinking:bool ->
  ?max_length:int ->
  ?value:string ->
  ?autofocus:bool ->
  ?on_input:(string -> 'msg option) ->
  ?on_change:(string -> 'msg option) ->
  ?on_submit:(string -> 'msg option) ->
  unit ->
  'msg t
(** [text_input ()] creates a text input element. *)

(** {2 Code} *)

module Code : sig
  type grammar = Mosaic_ui.Code.grammar
  (** Tree-sitter grammar for syntax highlighting. *)

  module Syntax_style : sig
    type t
    (** Syntax highlighting style configuration. *)

    val create : default:Ansi.Style.t -> (string * Ansi.Style.t) list -> t
    (** [create ~default rules] creates a syntax style from capture rules. *)

    val of_default_theme : ?base:Ansi.Style.t -> unit -> t
    (** [of_default_theme ()] returns the default syntax highlighting theme. *)
  end
end

val code :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  ?content:string ->
  ?filetype:string ->
  ?grammar:Code.grammar ->
  ?grammar_resolvers:(string -> Code.grammar option) list ->
  ?conceal:bool ->
  ?draw_unstyled_text:bool ->
  ?wrap_mode:[ `None | `Char | `Word ] ->
  ?tab_indicator:int ->
  ?tab_indicator_color:Ansi.Color.t ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?default_style:Ansi.Style.t ->
  ?selectable:bool ->
  ?tree_syntax:Mosaic_syntax.t ->
  ?syntax_style:Code.Syntax_style.t ->
  unit ->
  'msg t
(** [code ()] creates a syntax-highlighted code element. *)

val markdown :
  ?id:string ->
  ?key:string ->
  ?visible:bool ->
  ?z_index:int ->
  ?live:bool ->
  ?buffer:Mosaic_ui.Renderable.Props.buffer_mode ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
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
  ?width:int ->
  ?strict:bool ->
  ?code_grammar_resolvers:(string -> Code.grammar option) list ->
  ?content:string ->
  unit ->
  'msg t
(** [markdown ()] creates a markdown rendering element. *)
