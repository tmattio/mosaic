module Renderable = Mosaic_ui.Renderable
module Event = Mosaic_ui.Event
module Box = Mosaic_ui.Box
module Text = Mosaic_ui.Text
module Canvas = Mosaic_ui.Canvas
module Table = Mosaic_ui.Table
module Slider = Mosaic_ui.Slider
module Select = Mosaic_ui.Select
module Spinner = Mosaic_ui.Spinner
module Tab_select = Mosaic_ui.Tab_select
module Scroll_bar = Mosaic_ui.Scroll_bar
module Scroll_box = Mosaic_ui.Scroll_box
module Text_input = Mosaic_ui.Text_input
module Code = Mosaic_ui.Code
module Markdown = Mosaic_markdown

(* Dimension Helpers *)

let px n = Toffee.Style.Dimension.length (Float.of_int n)
let pct n = Toffee.Style.Dimension.pct (Float.of_int n)
let size ~width ~height = Toffee.Geometry.Size.make (px width) (px height)

let gap n =
  let lp = Toffee.Style.Length_percentage.length (Float.of_int n) in
  Toffee.Geometry.Size.make lp lp

let auto = Toffee.Style.Dimension.auto

let padding n =
  let lp = Toffee.Style.Length_percentage.length (Float.of_int n) in
  Toffee.Geometry.Rect.all lp

let margin n =
  let lpa = Toffee.Style.Length_percentage_auto.length (Float.of_int n) in
  Toffee.Geometry.Rect.all lpa

let inset n =
  let lpa = Toffee.Style.Length_percentage_auto.length (Float.of_int n) in
  Toffee.Geometry.Rect.all lpa

(* Core Types *)

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
  | Markdown

type 'a handlers = {
  on_mouse : (Event.mouse -> 'a) option;
  on_key : (Event.key -> 'a) option;
  on_paste : (Event.paste -> 'a) option;
}

type 'a slider_spec = {
  slider_props : Slider.Props.t;
  slider_on_change : (float -> 'a) option;
}

type 'a select_spec = {
  select_props : Select.Props.t;
  select_on_change : (int -> 'a) option;
  select_on_activate : (int -> 'a) option;
}

type 'a scroll_bar_spec = {
  scroll_bar_props : Scroll_bar.Props.t;
  scroll_bar_on_change : (int -> 'a) option;
}

type 'a tab_select_spec = {
  tab_select_props : Tab_select.Props.t;
  tab_select_on_change : (int -> 'a) option;
  tab_select_on_activate : (int -> 'a) option;
  tab_select_on_change_full : (int * (string * string option) -> 'a) option;
  tab_select_on_activate_full : (int * (string * string option) -> 'a) option;
}

type 'a text_input_spec = {
  text_input_props : Text_input.Props.t;
  text_input_on_input : (string -> 'a) option;
  text_input_on_change : (string -> 'a) option;
  text_input_on_submit : (string -> 'a) option;
}

type 'a scroll_box_spec = {
  scroll_box_props : Scroll_box.Props.t;
  scroll_box_on_scroll : (x:int -> y:int -> 'a) option;
}

type box_spec = Box.Props.t
type text_spec = Text.Props.t

type canvas_spec = {
  props : Canvas.Props.t;
  draw : (Canvas.t -> width:int -> height:int -> unit) option;
}

type 'a spec =
  | Box_spec of box_spec
  | Text_spec of text_spec
  | Canvas_spec of canvas_spec
  | Table_spec of Table.Props.t
  | Slider_spec of 'a slider_spec
  | Select_spec of 'a select_spec
  | Spinner_spec of Spinner.Props.t
  | Tab_select_spec of 'a tab_select_spec
  | Scroll_bar_spec of 'a scroll_bar_spec
  | Scroll_box_spec of 'a scroll_box_spec
  | Text_input_spec of 'a text_input_spec
  | Code_spec of Code.Props.t
  | Markdown_spec of Markdown.Props.t

type 'a props = {
  id : string option;
  style : Toffee.Style.t;
  visible : bool;
  z_index : int;
  live : bool;
  buffer : Renderable.Props.buffer_mode;
  handlers : 'a handlers;
  ref : (Renderable.t -> unit) option;
  spec : 'a spec;
}

type 'a t =
  | Element of 'a element
  | Fragment of 'a t list
  | Raw of Renderable.t
  | Null

and 'a element = {
  tag : tag;
  key : string option;
  props : 'a props;
  children : 'a t list;
}

(* Constructors *)

let null = Null
let fragment children = Fragment children
let raw node = Raw node

let box ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Box props *)
    ?background ?(border = false) ?border_sides ?border_style ?border_color
    ?focused_border_color ?should_fill ?custom_border_chars ?title
    ?title_alignment
    (* Children *)
      children =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let box_props =
    Box.Props.make ?background ~border ?border_sides ?border_style ?border_color
      ?focused_border_color ?should_fill ?custom_border_chars ?title
      ?title_alignment ()
  in
  let spec = Box_spec box_props in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Box; key; props; children }

let text ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Text props *)
    ?text_style ?wrap_mode ?tab_indicator ?tab_indicator_color ?selection_bg
    ?selection_fg ?selectable content =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let text_props =
    Text.Props.make ?text_style ~content ?wrap_mode ?tab_indicator
      ?tab_indicator_color ?selection_bg ?selection_fg ?selectable ()
  in
  let spec = Text_spec text_props in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Text; key; props; children = [] }

let canvas ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Canvas props *)
    ?respect_alpha ?width_method ?initial_width ?initial_height ?draw () =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let canvas_props =
    Canvas.Props.make ?respect_alpha ?width_method ?initial_width
      ?initial_height ()
  in
  let spec = Canvas_spec { props = canvas_props; draw } in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Canvas; key; props; children = [] }

let table ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Table props *)
    ?box_style ?safe_box ?table_padding ?collapse_padding ?pad_edge ?expand
    ?show_header ?show_footer ?show_edge ?show_lines ?leading ?cell_style
    ?row_styles ?header_style ?footer_style ?border_style:table_border_style
    ?title:table_title ?title_style ?title_justify ?caption ?caption_style
    ?caption_justify ?table_width ?table_min_width
    (* Required table props *)
    ~columns ~rows () =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let table_props =
    Table.Props.make ?box_style ?safe_box ?padding:table_padding
      ?collapse_padding ?pad_edge ?expand ?show_header ?show_footer ?show_edge
      ?show_lines ?leading ?cell_style ?row_styles ?header_style ?footer_style
      ?border_style:table_border_style ?title:table_title ?title_style
      ?title_justify ?caption ?caption_style ?caption_justify ?width:table_width
      ?min_width:table_min_width ~columns ~rows ()
  in
  let spec = Table_spec table_props in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Table; key; props; children = [] }

let slider ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Slider props *)
    ~orientation ?min ?max ?value ?viewport_size ?track_color ?thumb_color
    ?on_change () =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let slider_props =
    Slider.Props.make ~orientation ?min ?max ?value ?viewport_size ?track_color
      ?thumb_color ()
  in
  let spec = Slider_spec { slider_props; slider_on_change = on_change } in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Slider; key; props; children = [] }

let select ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Select props *)
    ?background ?text_color ?focused_background ?focused_text_color
    ?selected_background ?selected_text_color ?description_color
    ?selected_description_color ?show_scroll_indicator ?wrap_selection
    ?show_description ?item_spacing ?fast_scroll_step ?selected_index ?autofocus
    ?on_change ?on_activate options =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let select_props =
    Select.Props.make ~options ?background ?text_color ?focused_background
      ?focused_text_color ?selected_background ?selected_text_color
      ?description_color ?selected_description_color ?show_scroll_indicator
      ?wrap_selection ?show_description ?item_spacing ?fast_scroll_step
      ?selected_index ?autofocus ()
  in
  let spec =
    Select_spec
      {
        select_props;
        select_on_change = on_change;
        select_on_activate = on_activate;
      }
  in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Select; key; props; children = [] }

let spinner ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Spinner props *)
    ?preset ?frames ?interval ?autoplay ?color ?spinner_background () =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let spec =
    Spinner_spec
      (Spinner.Props.make ?preset ?frames ?interval ?autoplay ?color
         ?background:spinner_background ())
  in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Spinner; key; props; children = [] }

let tab_select ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Tab_select props *)
    ?wrap_selection ?show_description ?show_underline ?show_scroll_arrows
    ?mouse_navigation ?autofocus ?tab_width ?background ?text_color
    ?focused_background ?focused_text ?selected_background ?selected_text
    ?selected_description ?on_change ?on_activate ?on_change_full
    ?on_activate_full options =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let tab_select_props =
    Tab_select.Props.make ~options ?wrap_selection ?show_description
      ?show_underline ?show_scroll_arrows ?mouse_navigation ?autofocus
      ?tab_width ?background ?text_color ?focused_background ?focused_text
      ?selected_background ?selected_text ?selected_description ()
  in
  let spec =
    Tab_select_spec
      {
        tab_select_props;
        tab_select_on_change = on_change;
        tab_select_on_activate = on_activate;
        tab_select_on_change_full = on_change_full;
        tab_select_on_activate_full = on_activate_full;
      }
  in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Tab_select; key; props; children = [] }

let scroll_bar ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Scroll_bar props *)
    ~orientation ?show_arrows ?arrow_style ?track_style ?track_viewport_size
    ?on_change ?autofocus () =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let scroll_bar_props =
    Scroll_bar.Props.make ~orientation ?show_arrows ?arrow_style ?track_style
      ?track_viewport_size ?autofocus ()
  in
  let spec =
    Scroll_bar_spec { scroll_bar_props; scroll_bar_on_change = on_change }
  in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Scroll_bar; key; props; children = [] }

let scroll_box ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Scroll_box props *)
    ?background ?scroll_x ?scroll_y ?scroll_acceleration ?sticky_scroll
    ?sticky_start ?viewport_culling ?autofocus ?on_scroll children =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let scroll_box_props =
    Scroll_box.Props.make ?background ?scroll_x ?scroll_y ?scroll_acceleration
      ?sticky_scroll ?sticky_start ?viewport_culling ?autofocus ()
  in
  let spec =
    Scroll_box_spec { scroll_box_props; scroll_box_on_scroll = on_scroll }
  in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Scroll_box; key; props; children }

let input ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Text_input props *)
    ?background ?text_color ?focused_background ?focused_text_color ?placeholder
    ?placeholder_color ?cursor_color ?cursor_style ?cursor_blinking ?max_length
    ?value ?autofocus ?on_input ?on_change ?on_submit () =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let text_input_props =
    Text_input.Props.make ?background ?text_color ?focused_background
      ?focused_text_color ?placeholder ?placeholder_color ?cursor_color
      ?cursor_style ?cursor_blinking ?max_length ?value ?autofocus ()
  in
  let spec =
    Text_input_spec
      {
        text_input_props;
        text_input_on_input = on_input;
        text_input_on_change = on_change;
        text_input_on_submit = on_submit;
      }
  in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Text_input; key; props; children = [] }

let code ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Code props *)
    ?filetype ?grammar ?syntax_client ?conceal ?draw_unstyled_text ?wrap_mode
    ?tab_indicator ?tab_indicator_color ?selection_bg ?selection_fg
    ?default_style ?selectable ?syntax_style content =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let code_props =
    Code.Props.make ~content ?filetype ?grammar ?syntax_client ?conceal
      ?draw_unstyled_text ?wrap_mode ?tab_indicator ?tab_indicator_color
      ?selection_bg ?selection_fg ?default_style ?selectable ?syntax_style ()
  in
  let spec = Code_spec code_props in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Code; key; props; children = [] }

let markdown ?id ?key
    (* Host props *)
    ?(visible = true) ?(z_index = 0) ?(live = false) ?(buffer = `None)
    (* Ref *)
    ?ref
    (* Handlers *)
    ?on_mouse ?on_key ?on_paste
    (* Style properties *)
    ?display ?box_sizing ?position ?overflow ?scrollbar_width ?inset ?size
    ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
    (* Alignment *)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content
    (* Flexbox *)
    ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
    (* Grid *)
    ?grid_template_rows ?grid_template_columns ?grid_auto_rows
    ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
    ?grid_column
    (* Markdown props *)
    ?style:markdown_style ?width:markdown_width ?strict ?syntax_client content =
  let handlers = { on_mouse; on_key; on_paste } in
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let markdown_props =
    Markdown.Props.make ?style:markdown_style ?width:markdown_width ?strict
      ?syntax_client ~content ()
  in
  let spec = Markdown_spec markdown_props in
  let props =
    { id; style; visible; z_index; live; buffer; handlers; ref; spec }
  in
  Element { tag = Markdown; key; props; children = [] }

(* Handler transformation *)

let map_slider_spec f spec =
  {
    spec with
    slider_on_change = Option.map (fun cb v -> f (cb v)) spec.slider_on_change;
  }

let map_select_spec f spec =
  {
    spec with
    select_on_change =
      Option.map (fun cb idx -> f (cb idx)) spec.select_on_change;
    select_on_activate =
      Option.map (fun cb idx -> f (cb idx)) spec.select_on_activate;
  }

let map_scroll_bar_spec f spec =
  {
    spec with
    scroll_bar_on_change =
      Option.map (fun cb pos -> f (cb pos)) spec.scroll_bar_on_change;
  }

let map_tab_select_spec f spec =
  {
    spec with
    tab_select_on_change =
      Option.map (fun cb idx -> f (cb idx)) spec.tab_select_on_change;
    tab_select_on_activate =
      Option.map (fun cb idx -> f (cb idx)) spec.tab_select_on_activate;
    tab_select_on_change_full =
      Option.map (fun cb data -> f (cb data)) spec.tab_select_on_change_full;
    tab_select_on_activate_full =
      Option.map (fun cb data -> f (cb data)) spec.tab_select_on_activate_full;
  }

let map_text_input_spec f spec =
  {
    spec with
    text_input_on_input =
      Option.map (fun cb s -> f (cb s)) spec.text_input_on_input;
    text_input_on_change =
      Option.map (fun cb s -> f (cb s)) spec.text_input_on_change;
    text_input_on_submit =
      Option.map (fun cb s -> f (cb s)) spec.text_input_on_submit;
  }

let map_scroll_box_spec f spec =
  {
    spec with
    scroll_box_on_scroll =
      Option.map (fun cb ~x ~y -> f (cb ~x ~y)) spec.scroll_box_on_scroll;
  }

let map_spec (f : 'a -> 'b) : 'a spec -> 'b spec = function
  | Box_spec s -> Box_spec s
  | Text_spec s -> Text_spec s
  | Canvas_spec s -> Canvas_spec s
  | Table_spec s -> Table_spec s
  | Slider_spec s -> Slider_spec (map_slider_spec f s)
  | Select_spec s -> Select_spec (map_select_spec f s)
  | Spinner_spec s -> Spinner_spec s
  | Tab_select_spec s -> Tab_select_spec (map_tab_select_spec f s)
  | Scroll_bar_spec s -> Scroll_bar_spec (map_scroll_bar_spec f s)
  | Scroll_box_spec s -> Scroll_box_spec (map_scroll_box_spec f s)
  | Text_input_spec s -> Text_input_spec (map_text_input_spec f s)
  | Code_spec s -> Code_spec s
  | Markdown_spec s -> Markdown_spec s

let map_handlers_record (f : 'a -> 'b) (h : 'a handlers) : 'b handlers =
  {
    on_mouse = Option.map (fun g ev -> f (g ev)) h.on_mouse;
    on_key = Option.map (fun g ev -> f (g ev)) h.on_key;
    on_paste = Option.map (fun g ev -> f (g ev)) h.on_paste;
  }

let rec map_handlers (f : 'a -> 'b) (vnode : 'a t) : 'b t =
  match vnode with
  | Null -> Null
  | Raw node -> Raw node
  | Fragment children -> Fragment (List.map (map_handlers f) children)
  | Element elem ->
      let handlers = map_handlers_record f elem.props.handlers in
      let spec = map_spec f elem.props.spec in
      let props = { elem.props with handlers; spec } in
      let children = List.map (map_handlers f) elem.children in
      Element { elem with props; children }

(* Introspection *)

let tag_equal a b =
  match (a, b) with
  | Box, Box -> true
  | Text, Text -> true
  | Canvas, Canvas -> true
  | Table, Table -> true
  | Slider, Slider -> true
  | Select, Select -> true
  | Spinner, Spinner -> true
  | Tab_select, Tab_select -> true
  | Scroll_bar, Scroll_bar -> true
  | Scroll_box, Scroll_box -> true
  | Text_input, Text_input -> true
  | Code, Code -> true
  | Markdown, Markdown -> true
  | _ -> false

let key_of = function
  | Element { key; _ } -> key
  | Fragment _ | Raw _ | Null -> None

let tag_of = function
  | Element { tag; _ } -> Some tag
  | Fragment _ | Raw _ | Null -> None

let children_of = function
  | Element { children; _ } -> children
  | Fragment children -> children
  | Raw _ | Null -> []

(* Instantiation - convert vnode to Renderable tree *)

module Renderer = Mosaic_ui.Renderer

let instantiate_element (renderer : Renderer.t) (props : 'a props) :
    (Renderable.t, Renderable.error) result =
  let id = Option.value props.id ~default:(Renderer.gen_id renderer) in
  let host_props =
    Renderable.Props.make ~id ~visible:props.visible ~z_index:props.z_index
      ~live:props.live ~buffer:props.buffer ()
  in
  match Renderer.create_node renderer ~id ~props:host_props () with
  | Error e -> Error e
  | Ok node -> (
      match Renderable.set_style node props.style with
      | Error e -> Error e
      | Ok () -> (
          match props.spec with
          | Box_spec spec ->
              ignore (Box.mount ~props:spec node);
              Ok node
          | Text_spec spec ->
              ignore (Text.mount ~props:spec node);
              Ok node
          | Canvas_spec { props = canvas_props; draw } ->
              let c = Canvas.mount ~props:canvas_props node in
              Option.iter (fun f -> Canvas.set_draw c (Some f)) draw;
              Ok node
          | Table_spec spec ->
              ignore (Table.mount ~props:spec node);
              Ok node
          | Slider_spec spec ->
              let slider = Slider.mount ~props:spec.slider_props node in
              Option.iter
                (fun cb ->
                  Slider.set_on_change slider (Some (fun v -> ignore (cb v))))
                spec.slider_on_change;
              Ok node
          | Select_spec spec ->
              let select = Select.mount ~props:spec.select_props node in
              Option.iter
                (fun cb ->
                  Select.set_on_change select (Some (fun v -> ignore (cb v))))
                spec.select_on_change;
              Option.iter
                (fun cb ->
                  Select.set_on_activate select (Some (fun v -> ignore (cb v))))
                spec.select_on_activate;
              Ok node
          | Spinner_spec spinner_props ->
              ignore (Spinner.mount ~props:spinner_props node);
              Ok node
          | Tab_select_spec spec ->
              let tabs = Tab_select.mount ~props:spec.tab_select_props node in
              Option.iter
                (fun cb ->
                  Tab_select.set_on_change tabs (Some (fun v -> ignore (cb v))))
                spec.tab_select_on_change;
              Option.iter
                (fun cb ->
                  Tab_select.set_on_activate tabs
                    (Some (fun v -> ignore (cb v))))
                spec.tab_select_on_activate;
              Option.iter
                (fun cb ->
                  Tab_select.set_on_change_full tabs
                    (Some (fun v -> ignore (cb v))))
                spec.tab_select_on_change_full;
              Option.iter
                (fun cb ->
                  Tab_select.set_on_activate_full tabs
                    (Some (fun v -> ignore (cb v))))
                spec.tab_select_on_activate_full;
              Ok node
          | Scroll_bar_spec spec ->
              let bar = Scroll_bar.mount ~props:spec.scroll_bar_props node in
              Option.iter
                (fun cb ->
                  Scroll_bar.set_on_change bar (Some (fun v -> ignore (cb v))))
                spec.scroll_bar_on_change;
              Ok node
          | Scroll_box_spec spec ->
              let sb = Scroll_box.mount ~props:spec.scroll_box_props node in
              (match spec.scroll_box_on_scroll with
              | None -> Scroll_box.set_on_scroll sb None
              | Some cb ->
                  Scroll_box.set_on_scroll sb
                    (Some (fun ~x ~y -> ignore (cb ~x ~y))));
              Ok node
          | Text_input_spec spec ->
              let ti = Text_input.mount ~props:spec.text_input_props node in
              let map_cb opt =
                Option.map (fun cb -> fun s -> ignore (cb s)) opt
              in
              Text_input.set_callbacks ti
                ?on_input:(map_cb spec.text_input_on_input)
                ?on_change:(map_cb spec.text_input_on_change)
                ?on_submit:(map_cb spec.text_input_on_submit)
                ();
              Ok node
          | Code_spec spec ->
              ignore (Code.mount ~props:spec node);
              Ok node
          | Markdown_spec spec ->
              ignore (Markdown.mount ~props:spec node);
              Ok node))

let rec instantiate : type a.
    Renderer.t -> a t -> (Renderable.t option, Renderable.error) result =
 fun renderer vnode ->
  match vnode with
  | Null -> Ok None
  | Raw node -> Ok (Some node)
  | Fragment children -> instantiate_children renderer None children
  | Element { props; children; _ } -> (
      match instantiate_element renderer props with
      | Error e -> Error e
      | Ok node -> (
          Option.iter (fun f -> f node) props.ref;
          match instantiate_children renderer (Some node) children with
          | Error e -> Error e
          | Ok _ -> Ok (Some node)))

and instantiate_children : type a.
    Renderer.t ->
    Renderable.t option ->
    a t list ->
    (Renderable.t option, Renderable.error) result =
 fun renderer parent children ->
  let rec loop first_child = function
    | [] -> Ok first_child
    | child :: rest -> (
        match instantiate renderer child with
        | Error e -> Error e
        | Ok None -> loop first_child rest
        | Ok (Some child_node) -> (
            let first_child =
              match first_child with None -> Some child_node | some -> some
            in
            match parent with
            | None -> loop first_child rest
            | Some p -> (
                match Renderable.append_child ~parent:p ~child:child_node with
                | Error e -> Error e
                | Ok () -> loop first_child rest)))
  in
  loop None children

(* Rendering *)

let renderable_error_to_string = function
  | Renderable.Layout_error e -> Toffee.Error.to_string e
  | Renderable.Tree_mismatch -> "tree mismatch"

let renderer_error_to_string = function
  | Renderer.Layout_error e -> Toffee.Error.to_string e
  | Renderer.Tree_mismatch -> "tree mismatch"
  | Renderer.Root_already_assigned -> "root already assigned"

let trim_snapshot text =
  let lines_list =
    let raw = String.split_on_char '\n' text in
    match List.rev raw with "" :: rest -> List.rev rest | _ -> raw
  in
  let lines = Array.of_list lines_list in
  let left = ref max_int in
  let right = ref (-1) in
  let top = ref max_int in
  let bottom = ref (-1) in
  Array.iteri
    (fun row line ->
      let len = String.length line in
      let rec scan idx =
        if idx >= len then ()
        else (
          if line.[idx] <> ' ' then (
            left := min !left idx;
            right := max !right idx;
            top := min !top row;
            bottom := max !bottom row);
          scan (idx + 1))
      in
      scan 0)
    lines;
  if !right < 0 then text
  else
    let slice_row line =
      let len = String.length line in
      let r = min (len - 1) !right in
      let l = min !left r in
      String.sub line l (r - l + 1)
    in
    let rows = ref [] in
    for r = !bottom downto !top do
      rows := slice_row lines.(r) :: !rows
    done;
    String.concat "\n" !rows

let render ?width ?height ?(colors = true) (vnode : _ t) : string =
  let renderer = Renderer.create () in
  match instantiate renderer vnode with
  | Error e -> failwith (renderable_error_to_string e)
  | Ok None -> ""
  | Ok (Some root) -> (
      let layout_width = Option.value width ~default:80 in
      let layout_height = Option.value height ~default:40 in
      match Renderer.set_root renderer root with
      | Error e -> failwith (renderer_error_to_string e)
      | Ok () -> (
          match
            Renderer.resize renderer ~width:layout_width ~height:layout_height
          with
          | Error e -> failwith (renderer_error_to_string e)
          | Ok () -> (
              let tmp_grid =
                Grid.create ~width:layout_width ~height:layout_height ()
              in
              let tmp_hits =
                Screen.Hit_grid.create ~width:layout_width ~height:layout_height
              in
              ignore (Renderer.render_into renderer tmp_grid tmp_hits ~delta:0.);
              let rw = Renderable.width root in
              let rh = Renderable.height root in
              let render_width = Option.value width ~default:(max 1 rw) in
              let render_height = Option.value height ~default:(max 1 rh) in
              (match
                 Renderer.resize renderer ~width:render_width
                   ~height:render_height
               with
              | Error e -> failwith (renderer_error_to_string e)
              | Ok () -> ());
              let raw = Renderer.snapshot_frame renderer ~delta:0. in
              let output = if colors then raw else Ansi.strip raw in
              match (width, height, colors) with
              | None, None, false -> trim_snapshot output
              | _ -> output)))

let print ?width ?height ?(colors = true) (vnode : _ t) : unit =
  let output = render ?width ?height ~colors vnode in
  output_string stdout output;
  if not (String.ends_with ~suffix:"\n" output) then output_char stdout '\n';
  flush stdout
