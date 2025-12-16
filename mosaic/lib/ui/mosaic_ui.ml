module Renderable = Renderable
module Renderer = Renderer
module Border = Grid.Border
module Event = Event

(* Renderables *)

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

(* Dimension helpers *)

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

(* Element API *)

type renderer = Renderer.t
type renderable = Renderable.t

type 'props renderable_component =
  renderer -> 'props -> (renderable, Renderer.error) result

type 'props functional_component = 'props -> child list -> element

and 'props component =
  | Renderable of 'props renderable_component
  | Functional of 'props functional_component

and element =
  | Element : {
      component : 'props component;
      props : 'props;
      children : child list;
      on_mount : (renderable -> unit) list;
    }
      -> element

and child = Child of element | Node of renderable | List of child list | Null

let renderer_error_of = function
  | Renderable.Layout_error e -> Renderer.Layout_error e
  | Renderable.Tree_mismatch -> Renderer.Tree_mismatch

let flatten (children : child list) : child list =
  let rec loop acc = function
    | [] -> List.rev acc
    | Null :: rest -> loop acc rest
    | (Child _ as c) :: rest | (Node _ as c) :: rest -> loop (c :: acc) rest
    | List ys :: rest -> loop acc (ys @ rest)
  in
  loop [] children

let make component props children =
  Element { component; props; children; on_mount = [] }

let on_mount fn = function
  | Element ({ on_mount; _ } as e) ->
      Element { e with on_mount = fn :: on_mount }

let rec instantiate : renderer -> element -> (renderable, Renderer.error) result
    =
 fun renderer element ->
  match element with
  | Element { component; props; children; on_mount } -> (
      match component with
      | Functional fn -> instantiate renderer (fn props children)
      | Renderable ctor -> (
          match ctor renderer props with
          | Error _ as e -> e
          | Ok node ->
              List.iter (fun f -> f node) (List.rev on_mount);
              let children = flatten children in
              let rec loop = function
                | [] -> Ok node
                | ch :: rest -> (
                    match add_child ~parent:node renderer ch with
                    | Ok () -> loop rest
                    | Error _ as e -> e)
              in
              loop children))

and instantiate_child (renderer : renderer) (ch : child) :
    (renderable option, Renderer.error) result =
  match ch with
  | Null -> Ok None
  | Node n -> Ok (Some n)
  | List _ -> Ok None
  | Child element -> (
      match instantiate renderer element with
      | Ok n -> Ok (Some n)
      | Error e -> Error e)

and add_child ~parent (renderer : renderer) (ch : child) :
    (unit, Renderer.error) result =
  match ch with
  | Null -> Ok ()
  | List ys ->
      let rec loop = function
        | [] -> Ok ()
        | z :: zs -> (
            match add_child ~parent renderer z with
            | Ok () -> loop zs
            | Error _ as e -> e)
      in
      loop ys
  | Node child -> (
      match Renderable.append_child ~parent ~child with
      | Ok () -> Ok ()
      | Error e -> Error (renderer_error_of e))
  | Child element -> (
      match instantiate renderer element with
      | Error e -> Error e
      | Ok child -> (
          match Renderable.append_child ~parent ~child with
          | Ok () -> Ok ()
          | Error e -> Error (renderer_error_of e)))

let create_node (renderer : renderer) ?id ?host_props ?style () :
    (renderable, Renderer.error) result =
  match Renderer.create_node renderer ?id ?props:host_props () with
  | Error e -> Error (renderer_error_of e)
  | Ok n -> (
      match style with
      | None -> Ok n
      | Some s -> (
          match Renderable.set_style n s with
          | Ok () -> Ok n
          | Error e -> Error (renderer_error_of e)))

let box
    (* Node identity *)
    ?id
    (* Host props *)
    ?visible ?z_index ?buffer ?live
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
    ?background ?border ?border_sides ?border_style ?border_color
    ?focused_border_color ?should_fill ?custom_border_chars ?title
    ?title_alignment
    (* Callback *)
    ?on_mount
    (* Children *)
      (children : element list) : element =
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
    Box.Props.make ?background ?border ?border_sides ?border_style ?border_color
      ?focused_border_color ?should_fill ?custom_border_chars ?title
      ?title_alignment ()
  in
  let ctor (renderer : renderer) (box_props : Box.Props.t) =
    let id = Option.value id ~default:(Renderer.gen_id renderer) in
    let host_props =
      Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match create_node renderer ~id ~host_props ~style () with
    | Error _ as e -> e
    | Ok n ->
        let b = Box.mount ~props:box_props n in
        Option.iter (fun f -> f b) on_mount;
        Ok n
  in
  make (Renderable ctor) box_props (List.map (fun e -> Child e) children)

let text
    (* Node identity *)
    ?id
    (* Host props *)
    ?visible ?z_index ?buffer ?live
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
    ?selection_fg ?selectable
    (* Callback *)
    ?on_mount content : element =
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
  let ctor (renderer : renderer) (text_props : Text.Props.t) =
    let id = Option.value id ~default:(Renderer.gen_id renderer) in
    let host_props =
      Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match create_node renderer ~id ~host_props ~style () with
    | Error _ as e -> e
    | Ok n ->
        let t = Text.mount ~props:text_props n in
        Option.iter (fun f -> f t) on_mount;
        Ok (Text.node t)
  in
  make (Renderable ctor) text_props []

let canvas
    (* Node identity *)
    ?id
    (* Host props *)
    ?visible ?z_index ?buffer ?live
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
    ?respect_alpha ?width_method ?initial_width ?initial_height
    (* Canvas draw callback *)
    ?draw
    (* Callback *)
    ?on_mount () : element =
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
  let ctor (renderer : renderer) (canvas_props : Canvas.Props.t) =
    let id = Option.value id ~default:(Renderer.gen_id renderer) in
    let host_props =
      Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match create_node renderer ~id ~host_props ~style () with
    | Error _ as e -> e
    | Ok n ->
        let c = Canvas.mount ~props:canvas_props n in
        Option.iter (fun f -> Canvas.set_draw c (Some f)) draw;
        Option.iter (fun f -> f c) on_mount;
        Ok (Canvas.node c)
  in
  make (Renderable ctor) canvas_props []

let table
    (* Node identity *)
    ?id
    (* Host props *)
    ?visible ?z_index ?buffer ?live
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
    ?row_styles ?header_style ?footer_style ?border_style ?title ?title_style
    ?title_justify ?caption ?caption_style ?caption_justify ?table_width
    ?table_min_width
    (* Callback *)
    ?on_mount ~columns ~rows () : element =
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
      ?border_style ?title ?title_style ?title_justify ?caption ?caption_style
      ?caption_justify ?width:table_width ?min_width:table_min_width ~columns
      ~rows ()
  in
  let ctor (renderer : renderer) (table_props : Table.Props.t) =
    let id = Option.value id ~default:(Renderer.gen_id renderer) in
    let host_props =
      Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match create_node renderer ~id ~host_props ~style () with
    | Error _ as e -> e
    | Ok n ->
        let t = Table.mount ~props:table_props n in
        Option.iter (fun f -> f t) on_mount;
        Ok (Table.node t)
  in
  make (Renderable ctor) table_props []

let slider
    (* Node identity *)
    ?id
    (* Host props *)
    ?visible ?z_index ?buffer ?live
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
    ?on_change
    (* Callback *)
    ?on_mount () : element =
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
      ?thumb_color ?on_change ()
  in
  let ctor (renderer : renderer) (slider_props : Slider.Props.t) =
    let id = Option.value id ~default:(Renderer.gen_id renderer) in
    let host_props =
      Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match create_node renderer ~id ~host_props ~style () with
    | Error _ as e -> e
    | Ok n ->
        let s = Slider.mount ~props:slider_props n in
        Option.iter (fun f -> f s) on_mount;
        Ok (Slider.node s)
  in
  make (Renderable ctor) slider_props []

let select
    (* Node identity *)
    ?id
    (* Host props *)
    ?visible ?z_index ?buffer ?live
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
    (* Callback *)
    ?on_mount options : element =
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
  let ctor (renderer : renderer) (select_props : Select.Props.t) =
    let id = Option.value id ~default:(Renderer.gen_id renderer) in
    let host_props =
      Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match create_node renderer ~id ~host_props ~style () with
    | Error _ as e -> e
    | Ok n ->
        let s = Select.mount ~props:select_props n in
        Option.iter (fun f -> f s) on_mount;
        Ok (Select.node s)
  in
  make (Renderable ctor) select_props []

let spinner
    (* Node identity *)
    ?id
    (* Host props *)
    ?visible ?z_index ?buffer ?live
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
    ?preset ?frames ?interval ?autoplay ?color ?spinner_background
    (* Callback *)
    ?on_mount () : element =
  let style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let spinner_props =
    Spinner.Props.make ?preset ?frames ?interval ?autoplay ?color
      ?background:spinner_background ()
  in
  let ctor (renderer : renderer) (spinner_props : Spinner.Props.t) =
    let id = Option.value id ~default:(Renderer.gen_id renderer) in
    let host_props =
      Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match create_node renderer ~id ~host_props ~style () with
    | Error _ as e -> e
    | Ok n ->
        let s = Spinner.mount ~props:spinner_props n in
        Option.iter (fun f -> f s) on_mount;
        Ok (Spinner.node s)
  in
  make (Renderable ctor) spinner_props []

let tab_select
    (* Node identity *)
    ?id
    (* Host props *)
    ?visible ?z_index ?buffer ?live
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
    ?selected_description
    (* Callback *)
    ?on_mount options : element =
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
  let ctor (renderer : renderer) (tab_select_props : Tab_select.Props.t) =
    let id = Option.value id ~default:(Renderer.gen_id renderer) in
    let host_props =
      Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match create_node renderer ~id ~host_props ~style () with
    | Error _ as e -> e
    | Ok n ->
        let s = Tab_select.mount ~props:tab_select_props n in
        Option.iter (fun f -> f s) on_mount;
        Ok (Tab_select.node s)
  in
  make (Renderable ctor) tab_select_props []

let scroll_bar
    (* Node identity *)
    ?id
    (* Host props *)
    ?visible ?z_index ?buffer ?live
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
    ?on_change ?autofocus
    (* Callback *)
    ?on_mount () : element =
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
      ?track_viewport_size ?on_change ?autofocus ()
  in
  let ctor (renderer : renderer) (scroll_bar_props : Scroll_bar.Props.t) =
    let id = Option.value id ~default:(Renderer.gen_id renderer) in
    let host_props =
      Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match create_node renderer ~id ~host_props ~style () with
    | Error _ as e -> e
    | Ok n ->
        let s = Scroll_bar.mount ~props:scroll_bar_props n in
        Option.iter (fun f -> f s) on_mount;
        Ok (Scroll_bar.node s)
  in
  make (Renderable ctor) scroll_bar_props []

let scroll_box
    (* Node identity *)
    ?id
    (* Host props *)
    ?visible ?z_index ?buffer ?live
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
    ?sticky_start ?viewport_culling ?autofocus
    (* Callback *)
    ?on_mount (children : element list) : element =
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
  let ctor (renderer : renderer) (scroll_box_props : Scroll_box.Props.t) =
    let id = Option.value id ~default:(Renderer.gen_id renderer) in
    let host_props =
      Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match create_node renderer ~id ~host_props ~style () with
    | Error _ as e -> e
    | Ok n ->
        let sb = Scroll_box.mount ~props:scroll_box_props n in
        Option.iter (fun f -> f sb) on_mount;
        Ok (Scroll_box.node sb)
  in
  make (Renderable ctor) scroll_box_props (List.map (fun e -> Child e) children)

let input
    (* Node identity *)
    ?id
    (* Host props *)
    ?visible ?z_index ?buffer ?live
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
    ?value ?autofocus
    (* Callback *)
    ?on_mount () : element =
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
  let ctor (renderer : renderer) (text_input_props : Text_input.Props.t) =
    let id = Option.value id ~default:(Renderer.gen_id renderer) in
    let host_props =
      Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match create_node renderer ~id ~host_props ~style () with
    | Error _ as e -> e
    | Ok n ->
        let ti = Text_input.mount ~props:text_input_props n in
        Option.iter (fun f -> f ti) on_mount;
        Ok (Text_input.node ti)
  in
  make (Renderable ctor) text_input_props []

let code
    (* Node identity *)
    ?id
    (* Host props *)
    ?visible ?z_index ?buffer ?live
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
    ?filetype ?languages ?theme ?conceal ?draw_unstyled_text ?wrap_mode
    ?tab_width ?tab_indicator ?tab_indicator_color ?selection_bg ?selection_fg
    ?selectable
    (* Callback *)
    ?on_mount content : element =
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
    Code.Props.make ~content ?filetype ?languages ?theme ?conceal
      ?draw_unstyled_text ?wrap_mode ?tab_width ?tab_indicator ?tab_indicator_color
      ?selection_bg ?selection_fg ?selectable ()
  in
  let ctor (renderer : renderer) (code_props : Code.Props.t) =
    let id = Option.value id ~default:(Renderer.gen_id renderer) in
    let host_props =
      Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match create_node renderer ~id ~host_props ~style () with
    | Error _ as e -> e
    | Ok n ->
        let c = Code.mount ~props:code_props n in
        Option.iter (fun f -> f c) on_mount;
        Ok (Code.node c)
  in
  make (Renderable ctor) code_props []

(* Rendering *)

let error_to_string = function
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

let render ?width ?height ?(colors = true) (element : element) : string =
  let r = Renderer.create () in
  match instantiate r element with
  | Error e -> failwith (error_to_string e)
  | Ok root -> (
      let layout_width = Option.value width ~default:80 in
      let layout_height = Option.value height ~default:40 in
      match Renderer.set_root r root with
      | Error e -> failwith (error_to_string e)
      | Ok () -> (
          match Renderer.resize r ~width:layout_width ~height:layout_height with
          | Error e -> failwith (error_to_string e)
          | Ok () -> (
              let tmp_grid =
                Grid.create ~width:layout_width ~height:layout_height ()
              in
              let tmp_hits =
                Screen.Hit_grid.create ~width:layout_width ~height:layout_height
              in
              ignore (Renderer.render_into r tmp_grid tmp_hits ~delta:0.);
              let rw = Renderable.width root in
              let rh = Renderable.height root in
              let render_width = Option.value width ~default:(max 1 rw) in
              let render_height = Option.value height ~default:(max 1 rh) in
              (match
                 Renderer.resize r ~width:render_width ~height:render_height
               with
              | Error e -> failwith (error_to_string e)
              | Ok () -> ());
              let raw = Renderer.snapshot_frame r ~delta:0. in
              let output = if colors then raw else Ansi.strip raw in
              match (width, height, colors) with
              | None, None, false -> trim_snapshot output
              | _ -> output)))

let print ?width ?height ?(colors = true) (element : element) : unit =
  let output = render ?width ?height ~colors element in
  output_string stdout output;
  if not (String.ends_with ~suffix:"\n" output) then output_char stdout '\n';
  flush stdout
