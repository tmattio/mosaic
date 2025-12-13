module Renderer = Mosaic_ui.Renderer
module Renderable = Mosaic_ui.Renderable
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

type container = Renderable.t

type instance =
  | Box_instance of Box.t
  | Text_instance of Text.t
  | Canvas_instance of Canvas.t
  | Table_instance of Table.t
  | Slider_instance of Slider.t
  | Select_instance of Select.t
  | Spinner_instance of Spinner.t
  | Tab_select_instance of Tab_select.t
  | Scroll_bar_instance of Scroll_bar.t
  | Scroll_box_instance of Scroll_box.t
  | Text_input_instance of Text_input.t
  | Code_instance of Code.t
  | Markdown_instance of Markdown.t

let node_of = function
  | Box_instance b -> Box.node b
  | Text_instance t -> Text.node t
  | Canvas_instance c -> Canvas.node c
  | Table_instance t -> Table.node t
  | Slider_instance s -> Slider.node s
  | Select_instance s -> Select.node s
  | Spinner_instance s -> Spinner.node s
  | Tab_select_instance t -> Tab_select.node t
  | Scroll_bar_instance s -> Scroll_bar.node s
  | Scroll_box_instance s -> Scroll_box.node s
  | Text_input_instance t -> Text_input.node t
  | Code_instance c -> Code.node c
  | Markdown_instance m -> Markdown.node m

(* Instance Creation *)

let create_instance (renderer : Renderer.t) (tag : Vnode.tag)
    (props : unit Vnode.props) : instance =
  (* Generate stable ID if not provided - this ID remains constant for the fiber's lifetime *)
  let id = Option.value props.id ~default:(Renderer.gen_id renderer) in
  let host_props =
    Renderable.Props.make ~id ~visible:props.visible ~z_index:props.z_index
      ~live:props.live ~buffer:props.buffer ()
  in
  let node =
    match Renderer.create_node renderer ~id ~props:host_props () with
    | Ok n -> n
    | Error e ->
        failwith
          (Printf.sprintf "Failed to create node: %s"
             (match e with
             | Renderable.Layout_error e -> Toffee.Error.to_string e
             | Renderable.Tree_mismatch -> "tree mismatch"))
  in
  (* Apply style *)
  ignore (Renderable.set_style node props.style);
  (* Mount the appropriate widget based on tag *)
  match (tag, props.spec) with
  | Vnode.Box, Vnode.Box_spec spec ->
      let box = Box.mount ~props:spec node in
      Box_instance box
  | Vnode.Text, Vnode.Text_spec spec ->
      let text = Text.mount ~props:spec node in
      Text_instance text
  | Vnode.Canvas, Vnode.Canvas_spec spec ->
      let canvas = Canvas.mount ~props:spec.props node in
      Canvas.set_draw canvas spec.draw;
      Canvas_instance canvas
  | Vnode.Table, Vnode.Table_spec spec ->
      let table = Table.mount ~props:spec node in
      Table_instance table
  | Vnode.Slider, Vnode.Slider_spec spec ->
      let slider = Slider.mount ~props:spec.slider_props node in
      Slider.set_on_change slider spec.slider_on_change;
      Slider_instance slider
  | Vnode.Select, Vnode.Select_spec spec ->
      let select = Select.mount ~props:spec.select_props node in
      Select.set_on_change select spec.select_on_change;
      Select.set_on_activate select spec.select_on_activate;
      Select_instance select
  | Vnode.Spinner, Vnode.Spinner_spec props ->
      let spinner = Spinner.mount ~props node in
      Spinner_instance spinner
  | Vnode.Tab_select, Vnode.Tab_select_spec spec ->
      let tab_select = Tab_select.mount ~props:spec.tab_select_props node in
      Tab_select.set_on_change tab_select spec.tab_select_on_change;
      Tab_select.set_on_activate tab_select spec.tab_select_on_activate;
      Tab_select.set_on_change_full tab_select spec.tab_select_on_change_full;
      Tab_select.set_on_activate_full tab_select
        spec.tab_select_on_activate_full;
      Tab_select_instance tab_select
  | Vnode.Scroll_bar, Vnode.Scroll_bar_spec spec ->
      let scroll_bar = Scroll_bar.mount ~props:spec.scroll_bar_props node in
      Scroll_bar.set_on_change scroll_bar spec.scroll_bar_on_change;
      Scroll_bar_instance scroll_bar
  | Vnode.Scroll_box, Vnode.Scroll_box_spec spec ->
      let scroll_box = Scroll_box.mount ~props:spec.scroll_box_props node in
      Option.iter (Scroll_box.on_scroll scroll_box) spec.scroll_box_on_scroll;
      Scroll_box_instance scroll_box
  | Vnode.Text_input, Vnode.Text_input_spec spec ->
      let input = Text_input.mount ~props:spec.text_input_props node in
      Text_input.set_callbacks input ?on_input:spec.text_input_on_input
        ?on_change:spec.text_input_on_change
        ?on_submit:spec.text_input_on_submit ();
      Text_input_instance input
  | Vnode.Code, Vnode.Code_spec spec ->
      let code = Code.mount ~props:spec node in
      Code_instance code
  | Vnode.Markdown, Vnode.Markdown_spec spec ->
      let markdown = Markdown.mount ~props:spec node in
      Markdown_instance markdown
  | _ -> failwith "Tag and spec mismatch"

(* Property Updates - does NOT update handlers, those are managed via refs.
   Returns true if any property was actually changed. *)

let update_common_props (node : Renderable.t) ~(old_props : unit Vnode.props)
    ~(new_props : unit Vnode.props) : bool =
  let changed = ref false in
  if old_props.visible <> new_props.visible then (
    Renderable.set_visible node new_props.visible;
    changed := true);
  if old_props.z_index <> new_props.z_index then (
    Renderable.set_z_index node new_props.z_index;
    changed := true);
  if old_props.live <> new_props.live then (
    Renderable.set_live node new_props.live;
    changed := true);
  if old_props.style <> new_props.style then (
    ignore (Renderable.set_style node new_props.style);
    changed := true);
  !changed

let update_props (instance : instance) ~(old_props : unit Vnode.props)
    ~(new_props : unit Vnode.props) : bool =
  let node = node_of instance in
  let common_changed = update_common_props node ~old_props ~new_props in
  let spec_changed =
    match (instance, old_props.spec, new_props.spec) with
    | Box_instance box, Vnode.Box_spec old_spec, Vnode.Box_spec new_spec ->
        if Box.Props.equal old_spec new_spec then false
        else (
          Box.apply_props box new_spec;
          true)
    | Text_instance text, Vnode.Text_spec old_spec, Vnode.Text_spec new_spec ->
        if Text.Props.equal old_spec new_spec then false
        else (
          Text.apply_props text new_spec;
          true)
    | ( Canvas_instance canvas,
        Vnode.Canvas_spec old_spec,
        Vnode.Canvas_spec new_spec ) ->
        let props_changed =
          not (Canvas.Props.equal old_spec.props new_spec.props)
        in
        if props_changed then Canvas.apply_props canvas new_spec.props;
        (* Update draw callback unconditionally to mirror previous semantics. *)
        Canvas.set_draw canvas new_spec.draw;
        props_changed || Option.is_some new_spec.draw
    | Table_instance table, Vnode.Table_spec old_spec, Vnode.Table_spec new_spec
      ->
        if Table.Props.equal old_spec new_spec then false
        else (
          Table.apply_props table new_spec;
          true)
    | ( Slider_instance slider,
        Vnode.Slider_spec old_spec,
        Vnode.Slider_spec new_spec ) ->
        let props_changed =
          not (Slider.Props.equal old_spec.slider_props new_spec.slider_props)
        in
        let cb_changed =
          not
            (Bool.equal
               (Option.is_some old_spec.slider_on_change)
               (Option.is_some new_spec.slider_on_change))
        in
        if props_changed then Slider.apply_props slider new_spec.slider_props;
        Slider.set_on_change slider new_spec.slider_on_change;
        props_changed || cb_changed
    | ( Select_instance select,
        Vnode.Select_spec old_spec,
        Vnode.Select_spec new_spec ) ->
        let props_changed =
          not (Select.Props.equal old_spec.select_props new_spec.select_props)
        in
        let change_cb_changed =
          not
            (Bool.equal
               (Option.is_some old_spec.select_on_change)
               (Option.is_some new_spec.select_on_change))
        in
        let activate_cb_changed =
          not
            (Bool.equal
               (Option.is_some old_spec.select_on_activate)
               (Option.is_some new_spec.select_on_activate))
        in
        if props_changed then Select.apply_props select new_spec.select_props;
        Select.set_on_change select new_spec.select_on_change;
        Select.set_on_activate select new_spec.select_on_activate;
        props_changed || change_cb_changed || activate_cb_changed
    | ( Spinner_instance spinner,
        Vnode.Spinner_spec old_spec,
        Vnode.Spinner_spec new_spec ) ->
        if Spinner.Props.equal old_spec new_spec then false
        else (
          Spinner.apply_props spinner new_spec;
          true)
    | ( Tab_select_instance tab,
        Vnode.Tab_select_spec old_spec,
        Vnode.Tab_select_spec new_spec ) ->
        let props_changed =
          not
            (Tab_select.Props.equal old_spec.tab_select_props
               new_spec.tab_select_props)
        in
        let cb_presence_changed =
          let presence spec =
            Option.is_some spec.Vnode.tab_select_on_change
            || Option.is_some spec.tab_select_on_activate
            || Option.is_some spec.tab_select_on_change_full
            || Option.is_some spec.tab_select_on_activate_full
          in
          not (Bool.equal (presence old_spec) (presence new_spec))
        in
        if props_changed then
          Tab_select.apply_props tab new_spec.tab_select_props;
        Tab_select.set_on_change tab new_spec.tab_select_on_change;
        Tab_select.set_on_activate tab new_spec.tab_select_on_activate;
        Tab_select.set_on_change_full tab new_spec.tab_select_on_change_full;
        Tab_select.set_on_activate_full tab new_spec.tab_select_on_activate_full;
        props_changed || cb_presence_changed
    | ( Scroll_bar_instance sb,
        Vnode.Scroll_bar_spec old_spec,
        Vnode.Scroll_bar_spec new_spec ) ->
        let props_changed =
          not
            (Scroll_bar.Props.equal old_spec.scroll_bar_props
               new_spec.scroll_bar_props)
        in
        let cb_changed =
          not
            (Bool.equal
               (Option.is_some old_spec.scroll_bar_on_change)
               (Option.is_some new_spec.scroll_bar_on_change))
        in
        if props_changed then
          Scroll_bar.apply_props sb new_spec.scroll_bar_props;
        Scroll_bar.set_on_change sb new_spec.scroll_bar_on_change;
        props_changed || cb_changed
    | ( Scroll_box_instance sb,
        Vnode.Scroll_box_spec old_spec,
        Vnode.Scroll_box_spec new_spec ) ->
        let props_changed =
          not
            (Scroll_box.Props.equal old_spec.scroll_box_props
               new_spec.scroll_box_props)
        in
        let cb_changed =
          not
            (Bool.equal
               (Option.is_some old_spec.scroll_box_on_scroll)
               (Option.is_some new_spec.scroll_box_on_scroll))
        in
        if props_changed then
          Scroll_box.apply_props sb new_spec.scroll_box_props;
        Scroll_box.set_on_scroll sb new_spec.scroll_box_on_scroll;
        props_changed || cb_changed
    | ( Text_input_instance ti,
        Vnode.Text_input_spec old_spec,
        Vnode.Text_input_spec new_spec ) ->
        let props_changed =
          not
            (Text_input.Props.equal old_spec.text_input_props
               new_spec.text_input_props)
        in
        let callbacks_changed =
          let presence spec =
            Option.is_some spec.Vnode.text_input_on_input
            || Option.is_some spec.text_input_on_change
            || Option.is_some spec.text_input_on_submit
          in
          not (Bool.equal (presence old_spec) (presence new_spec))
        in
        if props_changed then
          Text_input.apply_props ti new_spec.text_input_props;
        Text_input.set_callbacks ti ?on_input:new_spec.text_input_on_input
          ?on_change:new_spec.text_input_on_change
          ?on_submit:new_spec.text_input_on_submit ();
        props_changed || callbacks_changed
    | Code_instance code, Vnode.Code_spec old_spec, Vnode.Code_spec new_spec ->
        if Code.Props.equal old_spec new_spec then false
        else (
          Code.apply_props code new_spec;
          true)
    | ( Markdown_instance md,
        Vnode.Markdown_spec old_spec,
        Vnode.Markdown_spec new_spec ) ->
        if Markdown.Props.equal old_spec new_spec then false
        else (
          Markdown.apply_props md new_spec;
          true)
    | _ -> failwith "Widget and spec mismatch during update"
  in
  common_changed || spec_changed

(* Tree Mutations *)

let append_child ~(parent : Renderable.t) ~(child : instance) : unit =
  match Renderable.append_child ~parent ~child:(node_of child) with
  | Ok () -> ()
  | Error e ->
      failwith
        (Printf.sprintf "Failed to append child: %s"
           (match e with
           | Renderable.Layout_error e -> Toffee.Error.to_string e
           | Renderable.Tree_mismatch -> "tree mismatch"))

let insert_at ~(parent : Renderable.t) ~(child : instance) ~(index : int) : unit
    =
  match Renderable.insert_child ~parent ~index ~child:(node_of child) with
  | Ok () -> ()
  | Error e ->
      failwith
        (Printf.sprintf "Failed to insert child: %s"
           (match e with
           | Renderable.Layout_error e -> Toffee.Error.to_string e
           | Renderable.Tree_mismatch -> "tree mismatch"))

let remove_child (node : Renderable.t) : unit =
  match Renderable.remove node with
  | Ok () -> ()
  | Error e ->
      failwith
        (Printf.sprintf "Failed to remove child: %s"
           (match e with
           | Renderable.Layout_error e -> Toffee.Error.to_string e
           | Renderable.Tree_mismatch -> "tree mismatch"))

let detach_if_attached (node : Renderable.t) : unit =
  (* Only detach if the node has a parent *)
  match Renderable.parent node with
  | Some _ -> (
      match Renderable.detach node with
      | Ok () -> ()
      | Error e ->
          failwith
            (Printf.sprintf "Failed to detach child: %s"
               (match e with
               | Renderable.Layout_error e -> Toffee.Error.to_string e
               | Renderable.Tree_mismatch -> "tree mismatch")))
  | None -> () (* Not attached, nothing to do *)

let reconcile_parent = Renderable.reconcile_parent

(* Commit Phase *)

let commit_update (instance : instance) : unit =
  Renderable.request_render (node_of instance)

let reset_after_commit (container : container) : unit =
  Renderable.request_render container
