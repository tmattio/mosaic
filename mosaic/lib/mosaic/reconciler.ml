open Mosaic_ui

(* ───── Instance Operations ───── *)

type instance =
  | Box_instance of Box.t
  | Text_instance of Text.t
  | Slider_instance of Slider.t
  | Text_input_instance of Text_input.t
  | Select_instance of Select.t
  | Tab_select_instance of Tab_select.t
  | Canvas_instance of Canvas.t
  | Spinner_instance of Spinner.t
  | Progress_bar_instance of Progress_bar.t
  | Scroll_bar_instance of Scroll_bar.t
  | Scroll_box_instance of Scroll_box.t
  | Textarea_instance of Textarea.t
  | Table_instance of Table.t
  | Code_instance of Code.t
  | Line_number_instance of Line_number.t
  | Markdown_instance of Markdown.t
  | Diff_instance of Diff.t
  | Tree_instance of Tree.t

let node_of = function
  | Box_instance b -> Box.node b
  | Text_instance t -> Text.node t
  | Slider_instance s -> Slider.node s
  | Text_input_instance i -> Text_input.node i
  | Select_instance s -> Select.node s
  | Tab_select_instance ts -> Tab_select.node ts
  | Canvas_instance c -> Canvas.node c
  | Spinner_instance s -> Spinner.node s
  | Progress_bar_instance pb -> Progress_bar.node pb
  | Scroll_bar_instance sb -> Scroll_bar.node sb
  | Scroll_box_instance sb -> Scroll_box.node sb
  | Textarea_instance ta -> Textarea.node ta
  | Table_instance t -> Table.node t
  | Code_instance c -> Code.node c
  | Line_number_instance ln -> Line_number.node ln
  | Markdown_instance md -> Markdown.node md
  | Diff_instance d -> Diff.node d
  | Tree_instance tr -> Tree.node tr

let create_instance ~(parent : Renderable.t) (kind : Vnode.kind)
    (attrs : Vnode.attrs) : instance =
  let inst =
    match kind with
    | Vnode.Box spec ->
        let box =
          Box.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Box.apply_props box spec;
        Box_instance box
    | Vnode.Text spec ->
        let text =
          Text.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Text.apply_props text spec;
        Text_instance text
    | Vnode.Slider spec ->
        let slider =
          Slider.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Slider.apply_props slider spec;
        Slider_instance slider
    | Vnode.Text_input spec ->
        let input =
          Text_input.create ~state:Editor.empty ~on_change:(Fun.const ())
            ~parent ?id:attrs.id ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Text_input.apply_props input spec;
        Text_input_instance input
    | Vnode.Canvas ->
        let canvas =
          Canvas.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Canvas_instance canvas
    | Vnode.Select spec ->
        let select =
          Select.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Select.apply_props select spec;
        Select_instance select
    | Vnode.Tab_select spec ->
        let tab_select =
          Tab_select.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ~options:[] ()
        in
        Tab_select.apply_props tab_select spec;
        Tab_select_instance tab_select
    | Vnode.Spinner spec ->
        let spinner =
          Spinner.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Spinner.apply_props spinner spec;
        Spinner_instance spinner
    | Vnode.Progress_bar spec ->
        let progress_bar =
          Progress_bar.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Progress_bar.apply_props progress_bar spec;
        Progress_bar_instance progress_bar
    | Vnode.Scroll_bar spec ->
        let scroll_bar =
          Scroll_bar.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Scroll_bar.apply_props scroll_bar spec;
        Scroll_bar_instance scroll_bar
    | Vnode.Scroll_box spec ->
        let scroll_box =
          Scroll_box.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Scroll_box.apply_props scroll_box spec;
        Scroll_box_instance scroll_box
    | Vnode.Textarea spec ->
        let textarea =
          Textarea.create ~state:Editor.empty ~on_change:(Fun.const ()) ~parent
            ?id:attrs.id ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Textarea.apply_props textarea spec;
        Textarea_instance textarea
    | Vnode.Table spec ->
        let table =
          Table.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Table.apply_props table spec;
        Table_instance table
    | Vnode.Code spec ->
        let code =
          Code.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Code.apply_props code spec;
        Code_instance code
    | Vnode.Line_number spec ->
        let ln =
          Line_number.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Line_number.apply_props ln spec;
        Line_number_instance ln
    | Vnode.Markdown spec ->
        let md =
          Markdown.create ~parent ?id:attrs.id ~layout_style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Markdown.apply_props md spec;
        Markdown_instance md
    | Vnode.Diff spec ->
        let d =
          Diff.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            (Diff.Patch.make [])
        in
        Diff.apply_props d spec;
        Diff_instance d
    | Vnode.Tree spec ->
        let tree =
          Tree.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Tree.apply_props tree spec;
        Tree_instance tree
  in
  let node = node_of inst in
  Renderable.set_focusable node (attrs.focusable || attrs.focused);
  (* Autofocus is deferred to after commit_placement in reconcile_flattened, so
     the node is fully attached to the render tree when focus is applied. *)
  if attrs.buffered then Renderable.set_buffered node true;
  if attrs.live then Renderable.set_live node true;
  inst

let update_common (node : Renderable.t) ~(old_attrs : Vnode.attrs)
    ~(new_attrs : Vnode.attrs) : bool =
  let changed = ref false in
  (* Ids address nodes (Cmd.focus resolves by Renderable.id), so a reused
     fiber must track them: a stale id would make focus targeting miss an
     element that visibly renders the new id. Dropping the id reverts to the
     creation default. No render is needed — ids are not visual. *)
  if old_attrs.id <> new_attrs.id then
    Renderable.set_id node
      (match new_attrs.id with
      | Some id -> id
      | None -> Printf.sprintf "node-%d" (Renderable.Private.num node));
  if old_attrs.visible <> new_attrs.visible then (
    Renderable.set_visible node new_attrs.visible;
    changed := true);
  if old_attrs.z_index <> new_attrs.z_index then (
    Renderable.set_z_index node new_attrs.z_index;
    changed := true);
  if old_attrs.opacity <> new_attrs.opacity then (
    Renderable.set_opacity node new_attrs.opacity;
    changed := true);
  if
    old_attrs.focusable <> new_attrs.focusable
    || old_attrs.focused <> new_attrs.focused
  then (
    Renderable.set_focusable node (new_attrs.focusable || new_attrs.focused);
    changed := true);
  if old_attrs.buffered <> new_attrs.buffered then (
    Renderable.set_buffered node new_attrs.buffered;
    changed := true);
  if old_attrs.live <> new_attrs.live then (
    Renderable.set_live node new_attrs.live;
    changed := true);
  (* Style is handled in update_instance for widget-specific behavior.
     Box.set_style adds border insets and Scroll_box.set_style its root
     overrides; Renderable.set_style does not. *)
  !changed

let set_instance_style (inst : instance) (style : Toffee.Style.t) : unit =
  match inst with
  | Box_instance box -> Box.set_style box style
  | Scroll_box_instance scroll_box -> Scroll_box.set_style scroll_box style
  | Markdown_instance md -> Markdown.set_layout_style md style
  | _ -> Renderable.set_style (node_of inst) style

(* Accepted state only changes through declarations, so physical sharing is
   a valid fast path for every widget. *)
let update_instance (inst : instance) ~(old_attrs : Vnode.attrs)
    ~(new_attrs : Vnode.attrs) ~(old_kind : Vnode.kind) ~(new_kind : Vnode.kind)
    : bool =
  if old_attrs == new_attrs && old_kind == new_kind then false
  else
    let common_changed =
      if old_attrs == new_attrs then false
      else update_common (node_of inst) ~old_attrs ~new_attrs
    in
    let style_changed =
      if old_attrs == new_attrs then false
      else if old_attrs.style <> new_attrs.style then (
        set_instance_style inst new_attrs.style;
        true)
      else false
    in
    let kind_changed =
      match (inst, old_kind, new_kind) with
      | _ when old_kind == new_kind -> false
      | _ -> (
          match (inst, old_kind, new_kind) with
          | ( Text_input_instance input,
              Vnode.Text_input old_spec,
              Vnode.Text_input new_spec ) ->
              if Text_input.Props.equal old_spec new_spec then false
              else (
                Text_input.apply_props input new_spec;
                true)
          | ( Textarea_instance textarea,
              Vnode.Textarea old_spec,
              Vnode.Textarea new_spec ) ->
              if Textarea.Props.equal old_spec new_spec then false
              else (
                Textarea.apply_props textarea new_spec;
                true)
          | Box_instance box, Vnode.Box old_spec, Vnode.Box new_spec ->
              if Box.Props.equal old_spec new_spec then false
              else (
                Box.apply_props box new_spec;
                true)
          | Text_instance text, Vnode.Text old_spec, Vnode.Text new_spec ->
              if Text.Props.equal old_spec new_spec then false
              else (
                Text.apply_props text new_spec;
                true)
          | Slider_instance slider, Vnode.Slider old_spec, Vnode.Slider new_spec
            ->
              if Slider.Props.equal old_spec new_spec then false
              else (
                Slider.apply_props slider new_spec;
                true)
          | Select_instance select, Vnode.Select old_spec, Vnode.Select new_spec
            ->
              if Select.Props.equal old_spec new_spec then false
              else (
                Select.apply_props select new_spec;
                true)
          | ( Tab_select_instance tab_select,
              Vnode.Tab_select old_spec,
              Vnode.Tab_select new_spec ) ->
              if Tab_select.Props.equal old_spec new_spec then false
              else (
                Tab_select.apply_props tab_select new_spec;
                true)
          | Canvas_instance _, Vnode.Canvas, Vnode.Canvas -> false
          | ( Spinner_instance spinner,
              Vnode.Spinner old_spec,
              Vnode.Spinner new_spec ) ->
              if Spinner.Props.equal old_spec new_spec then false
              else (
                Spinner.apply_props spinner new_spec;
                true)
          | ( Progress_bar_instance pb,
              Vnode.Progress_bar old_spec,
              Vnode.Progress_bar new_spec ) ->
              if Progress_bar.Props.equal old_spec new_spec then false
              else (
                Progress_bar.apply_props pb new_spec;
                true)
          | ( Scroll_bar_instance scroll_bar,
              Vnode.Scroll_bar old_spec,
              Vnode.Scroll_bar new_spec ) ->
              if Scroll_bar.Props.equal old_spec new_spec then false
              else (
                Scroll_bar.apply_props scroll_bar new_spec;
                true)
          | ( Scroll_box_instance scroll_box,
              Vnode.Scroll_box old_spec,
              Vnode.Scroll_box new_spec ) ->
              if Scroll_box.Props.equal old_spec new_spec then false
              else (
                Scroll_box.apply_props scroll_box new_spec;
                true)
          | Table_instance table, Vnode.Table old_spec, Vnode.Table new_spec ->
              if Table.Props.equal old_spec new_spec then false
              else (
                Table.apply_props table new_spec;
                true)
          | Code_instance code, Vnode.Code old_spec, Vnode.Code new_spec ->
              if Code.Props.equal old_spec new_spec then false
              else (
                Code.apply_props code new_spec;
                true)
          | ( Line_number_instance ln,
              Vnode.Line_number old_spec,
              Vnode.Line_number new_spec ) ->
              if Line_number.Props.equal old_spec new_spec then false
              else (
                Line_number.apply_props ln new_spec;
                true)
          | ( Markdown_instance md,
              Vnode.Markdown old_spec,
              Vnode.Markdown new_spec ) ->
              if Markdown.Props.equal old_spec new_spec then false
              else (
                Markdown.apply_props md new_spec;
                true)
          | Diff_instance d, Vnode.Diff old_spec, Vnode.Diff new_spec ->
              if Diff.Props.equal old_spec new_spec then false
              else (
                Diff.apply_props d new_spec;
                true)
          | Tree_instance tree, Vnode.Tree old_spec, Vnode.Tree new_spec ->
              if Tree.Props.equal old_spec new_spec then false
              else (
                Tree.apply_props tree new_spec;
                true)
          | _ ->
              invalid_arg "Reconciler: instance and kind mismatch during update"
          )
    in
    common_changed || style_changed || kind_changed

let commit_update (inst : instance) : unit =
  Renderable.request_render (node_of inst)

(* ───── Fiber Tracking ───── *)

type callback_refs = {
  callbacks : unit Vnode.widget_callbacks ref;
  update_presence : unit Vnode.widget_callbacks -> unit;
}

type memo_cache = {
  previous : (int, unit Vnode.memo * unit Vnode.t) Hashtbl.t;
  current : (int, unit Vnode.memo * unit Vnode.t) Hashtbl.t;
  mutable next : int;
}

type declared_focus = Focus_none | Focus_one of Renderable.t | Focus_many

type child = Fiber of fiber | Embedded of Renderable.t

and fiber = {
  mutable kind : Vnode.kind;
  key : string option;
  mutable attrs : Vnode.attrs;
  instance : instance;
  mutable children : child list;
  mouse_ref : (Event.mouse -> unit option) option ref;
  key_ref : (Event.key -> unit option) option ref;
  paste_ref : (Event.paste -> unit option) option ref;
  click_ref : (Event.mouse -> unit option) option ref;
  size_ref : (w:int -> h:int -> unit option) option ref;
  mutable declared_children : unit Vnode.t list;
  mutable viewport_width : int;
  mutable memos : memo_cache option;
  mutable declared_focus : declared_focus;
  callback_refs : callback_refs;
}

let fiber_node f = node_of f.instance

(* Two kinds match when they are the same widget variant (Box/Text), regardless
   of their props. *)
let kind_matches (a : Vnode.kind) (b : Vnode.kind) : bool =
  match (a, b) with
  | Vnode.Box _, Vnode.Box _
  | Vnode.Text _, Vnode.Text _
  | Vnode.Slider _, Vnode.Slider _
  | Vnode.Text_input _, Vnode.Text_input _
  | Vnode.Select _, Vnode.Select _
  | Vnode.Tab_select _, Vnode.Tab_select _
  | Vnode.Canvas, Vnode.Canvas
  | Vnode.Spinner _, Vnode.Spinner _
  | Vnode.Progress_bar _, Vnode.Progress_bar _
  | Vnode.Scroll_bar _, Vnode.Scroll_bar _
  | Vnode.Scroll_box _, Vnode.Scroll_box _
  | Vnode.Textarea _, Vnode.Textarea _
  | Vnode.Table _, Vnode.Table _
  | Vnode.Code _, Vnode.Code _
  | Vnode.Line_number _, Vnode.Line_number _
  | Vnode.Markdown _, Vnode.Markdown _
  | Vnode.Diff _, Vnode.Diff _
  | Vnode.Tree _, Vnode.Tree _ ->
      true
  | _ -> false

(* Most widgets use stable callbacks dispatching through a mutable ref.
   Table instead installs callbacks directly so a press can retain its original
   row-to-value mapping across synchronous reconciliation. *)
let create_callback_refs (instance : instance)
    (initial : unit Vnode.widget_callbacks) : callback_refs =
  let callbacks = ref initial in
  let update_presence =
    match instance with
    | Slider_instance widget ->
        Slider.set_on_change widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Slider_callbacks c ->
                   Option.iter (fun f -> f value) c.on_value_change
               | _ -> ()));
        Fun.const ()
    | Text_input_instance widget ->
        Text_input.set_on_change widget (fun value ->
            match !callbacks with
            | Vnode.Input_callbacks c ->
                Option.iter (fun f -> f value) c.on_change
            | _ -> ());
        Text_input.set_on_submit widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Input_callbacks c ->
                   Option.iter (fun f -> f value) c.on_submit
               | _ -> ()));
        Fun.const ()
    | Textarea_instance widget ->
        Textarea.set_on_change widget (fun value ->
            match !callbacks with
            | Vnode.Textarea_callbacks c ->
                Option.iter (fun f -> f value) c.on_change
            | _ -> ());
        Textarea.set_on_submit widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Textarea_callbacks c ->
                   Option.iter (fun f -> f value) c.on_submit
               | _ -> ()));
        Fun.const ()
    | Select_instance widget ->
        Select.set_on_change widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Select_callbacks c ->
                   Option.iter (fun f -> f value) c.on_change
               | _ -> ()));
        Select.set_on_activate widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Select_callbacks c ->
                   Option.iter (fun f -> f value) c.on_activate
               | _ -> ()));
        Fun.const ()
    | Tab_select_instance widget ->
        Tab_select.set_on_change widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Tab_select_callbacks c ->
                   Option.iter (fun f -> f value) c.on_change
               | _ -> ()));
        Tab_select.set_on_activate widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Tab_select_callbacks c ->
                   Option.iter (fun f -> f value) c.on_activate
               | _ -> ()));
        Fun.const ()
    | Scroll_bar_instance widget ->
        Scroll_bar.set_on_change widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Scroll_bar_callbacks c ->
                   Option.iter (fun f -> f value) c.on_change
               | _ -> ()));
        Fun.const ()
    | Scroll_box_instance widget ->
        Scroll_box.set_on_scroll widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Scroll_box_callbacks c ->
                   Option.iter (fun f -> f value) c.on_scroll
               | _ -> ()));
        Fun.const ()
    | Code_instance widget ->
        Code.set_on_selection widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Code_callbacks c ->
                   Option.iter
                     (fun f -> ignore (f value : unit option))
                     c.on_selection
               | _ -> ()));
        Fun.const ()
    | Markdown_instance widget ->
        Markdown.set_on_selection widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Markdown_callbacks c ->
                   Option.iter
                     (fun f -> ignore (f value : unit option))
                     c.on_selection
               | _ -> ()));
        Fun.const ()
    | Table_instance widget -> (
        (* A press snapshots these callbacks before user code can reconcile a
           different domain-to-row mapping. *)
        function
        | Vnode.Table_callbacks c ->
            Table.set_on_change widget c.on_change;
            Table.set_on_activate widget c.on_activate;
            Table.set_on_hover widget
              (Option.map
                 (fun f value -> ignore (f value : unit option))
                 c.on_hover)
        | _ ->
            Table.set_on_change widget None;
            Table.set_on_activate widget None;
            Table.set_on_hover widget None)
    | Tree_instance widget ->
        Tree.set_on_change widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Tree_callbacks c ->
                   Option.iter (fun f -> f value) c.on_change
               | _ -> ()));
        Tree.set_on_activate widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Tree_callbacks c ->
                   Option.iter (fun f -> f value) c.on_activate
               | _ -> ()));
        Fun.const ()
    | Canvas_instance widget ->
        Canvas.set_on_draw widget
          (Some
             (fun canvas ~delta ->
               match !callbacks with
               | Vnode.Canvas_callbacks c ->
                   Option.iter (fun f -> f canvas ~delta) c.on_draw
               | _ -> ()));
        Fun.const ()
    | Diff_instance widget -> (
        let dispatch hit =
          match !callbacks with
          | Vnode.Diff_callbacks c ->
              Option.iter
                (fun f -> ignore (f hit : unit option))
                c.on_line_click
          | _ -> ()
        in
        fun current ->
          match current with
          | Vnode.Diff_callbacks c ->
              Diff.set_on_line_click widget
                (Option.map (Fun.const dispatch) c.on_line_click)
          | _ -> Diff.set_on_line_click widget None)
    | Box_instance _ | Text_instance _ | Spinner_instance _
    | Progress_bar_instance _ | Line_number_instance _ ->
        Fun.const ()
  in
  update_presence initial;
  { callbacks; update_presence }

let create_fiber ~(parent : Renderable.t) (elem : unit Vnode.element) : fiber =
  let instance = create_instance ~parent elem.kind elem.attrs in
  let node = node_of instance in
  Renderable.Private.set_focus_request node elem.handlers.on_focus_request;
  let mouse_ref = ref elem.handlers.on_mouse in
  let key_ref = ref elem.handlers.on_key in
  let paste_ref = ref elem.handlers.on_paste in
  let click_ref = ref elem.handlers.on_click in
  let size_ref = ref elem.handlers.on_size in
  let callback_refs = create_callback_refs instance elem.callbacks in
  Renderable.on_mouse node (fun ev ->
      Option.iter (fun f -> ignore (f ev : unit option)) !mouse_ref;
      if
        (not (Renderable.destroyed node))
        && not (Event.Mouse.propagation_stopped ev)
      then
        match Event.Mouse.kind ev with
        | Event.Mouse.Up { button = Left; is_dragging = false } ->
            Option.iter (fun f -> ignore (f ev : unit option)) !click_ref
        | _ -> ());
  Renderable.Private.set_size_observer node
    (Some
       (fun node ->
         Option.iter
           (fun f ->
             ignore
               (f ~w:(Renderable.width node) ~h:(Renderable.height node)
                 : unit option))
           !size_ref));
  Renderable.on_key node (fun ev ->
      match !key_ref with Some h -> ignore (h ev : unit option) | None -> ());
  Renderable.on_paste node (fun ev ->
      match !paste_ref with Some h -> ignore (h ev : unit option) | None -> ());
  {
    kind = elem.kind;
    key = elem.key;
    attrs = elem.attrs;
    instance;
    children = [];
    mouse_ref;
    key_ref;
    paste_ref;
    click_ref;
    size_ref;
    declared_children = [];
    viewport_width = 0;
    memos = None;
    declared_focus = Focus_none;
    callback_refs;
  }

let update_callback_refs refs callbacks =
  refs.callbacks := callbacks;
  refs.update_presence callbacks

let update_fiber (f : fiber) (elem : unit Vnode.element) : bool =
  Renderable.Private.set_focus_request (fiber_node f)
    elem.handlers.on_focus_request;
  f.mouse_ref := elem.handlers.on_mouse;
  f.key_ref := elem.handlers.on_key;
  f.paste_ref := elem.handlers.on_paste;
  f.click_ref := elem.handlers.on_click;
  f.size_ref := elem.handlers.on_size;
  update_callback_refs f.callback_refs elem.callbacks;
  let changed =
    update_instance f.instance ~old_attrs:f.attrs ~new_attrs:elem.attrs
      ~old_kind:f.kind ~new_kind:elem.kind
  in
  f.attrs <- elem.attrs;
  f.kind <- elem.kind;
  if changed then commit_update f.instance;
  changed

let rec destroy_fiber (f : fiber) : unit =
  List.iter
    (function
      | Fiber child -> destroy_fiber child | Embedded n -> Renderable.detach n)
    f.children;
  Renderable.destroy_recursively (fiber_node f)

(* ───── Vnode Flattening ───── *)

(* Fragments and Empty nodes are structural -- they group vnodes but don't
   correspond to real widgets. Flattening collapses the tree into a flat list of
   concrete children (elements and embeds) for positional matching. *)
type flattened =
  | Flat_element of unit Vnode.element
  | Flat_embed of Renderable.t

let rec flatten_vnode ~cache ~viewport_width (vnode : unit Vnode.t)
    (acc : flattened list) : flattened list =
  match vnode with
  | Vnode.Empty -> acc
  | Vnode.Memo memo ->
      let memo_cache =
        match !cache with
        | Some memo_cache -> memo_cache
        | None ->
            let memo_cache =
              {
                previous = Hashtbl.create 4;
                current = Hashtbl.create 4;
                next = 0;
              }
            in
            cache := Some memo_cache;
            memo_cache
      in
      let slot = memo_cache.next in
      memo_cache.next <- slot + 1;
      let view =
        match Hashtbl.find_opt memo_cache.previous slot with
        | Some (previous, view) when Vnode.memo_equal previous memo -> view
        | _ -> memo.view ()
      in
      Hashtbl.replace memo_cache.current slot (memo, view);
      flatten_vnode ~cache ~viewport_width view acc
  | Vnode.Embed node -> Flat_embed node :: acc
  | Vnode.Element elem -> Flat_element elem :: acc
  | Vnode.Fragment children ->
      List.fold_right
        (fun child acc -> flatten_vnode ~cache ~viewport_width child acc)
        children acc
  | Vnode.Viewport_switch { at_least_width; wide; narrow } ->
      let selected =
        if viewport_width >= at_least_width then wide else narrow
      in
      flatten_vnode ~cache ~viewport_width selected acc

let flatten_children ~memos ~viewport_width children =
  let cache =
    ref
      (Option.map
         (fun previous ->
           let cache =
             {
               previous = previous.current;
               current = previous.previous;
               next = 0;
             }
           in
           Hashtbl.clear cache.current;
           cache)
         memos)
  in
  let flattened =
    List.fold_right
      (fun child acc -> flatten_vnode ~cache ~viewport_width child acc)
      children []
  in
  Option.iter (fun cache -> Hashtbl.clear cache.previous) !cache;
  (!cache, flattened)

(* ───── Fiber Maps ───── *)

(* Keyed lookup + positional array for O(1) matching during reconciliation. *)
type fiber_maps = {
  by_key : (string, fiber * int) Hashtbl.t;
  by_index : fiber array;
  used : bool array;
}

let fibers_of_children (children : child list) : fiber list =
  List.filter_map (function Fiber f -> Some f | Embedded _ -> None) children

let build_maps (fibers : fiber list) : fiber_maps =
  let arr = Array.of_list fibers in
  let len = Array.length arr in
  let by_key = Hashtbl.create len in
  Array.iteri
    (fun i f ->
      match f.key with
      | Some k ->
          (* First occurrence wins on duplicate keys, so when a view
             accidentally emits duplicate-keyed siblings the first keeps its
             identity across renders and only the later duplicates remount. *)
          if not (Hashtbl.mem by_key k) then Hashtbl.add by_key k (f, i)
      | None -> ())
    arr;
  { by_key; by_index = arr; used = Array.make len false }

let can_reuse (f : fiber) (elem : unit Vnode.element) : bool =
  kind_matches f.kind elem.kind && f.key = elem.key

(* Keyed elements match by key, unkeyed elements match by position among their
   siblings. This mirrors React's reconciliation heuristic: keys give stable
   identity across reorders, positional matching is the fallback. *)
let find_match (maps : fiber_maps) (pos : int) (elem : unit Vnode.element) :
    fiber option =
  let old_len = Array.length maps.by_index in
  match elem.key with
  | Some k -> (
      match Hashtbl.find_opt maps.by_key k with
      | Some (f, idx) when can_reuse f elem && not maps.used.(idx) ->
          maps.used.(idx) <- true;
          Some f
      | _ -> None)
  | None ->
      if pos < old_len && not maps.used.(pos) then
        let f = maps.by_index.(pos) in
        if can_reuse f elem then (
          maps.used.(pos) <- true;
          Some f)
        else None
      else None

(* ───── Commit Placement ───── *)

let child_node = function Fiber f -> fiber_node f | Embedded n -> n

(* Two-pass placement: first detach stale managed children, then insert/reorder
   the new managed children. We intentionally avoid scanning all concrete
   Renderable children under [parent], because some widgets (e.g. Markdown)
   manage internal children outside the reconciler.

   This runs for every element fiber on every reconcile, so the steady state
   must stay cheap: position probes go through the O(1) [child_at] accessor
   rather than re-materializing the child list per index, and the stale-child
   set is keyed by [Renderable.Private.num] — hashing raw nodes would apply
   polymorphic hash and compare to cyclic records of closures. *)
let rec same_nodes (a : child list) (b : child list) : bool =
  match (a, b) with
  | [], [] -> true
  | x :: a, y :: b -> child_node x == child_node y && same_nodes a b
  | _ -> false

let commit_placement (parent_node : Renderable.t) ~(old_children : child list)
    ~(new_children : child list) : unit =
  let parent = Renderable.child_target parent_node in
  (* Pass 1: detach stale reconciler-managed children. Skipped when both
     lists hold the same nodes in order — nothing can be stale. *)
  if not (same_nodes old_children new_children) then begin
    let target_set = Hashtbl.create (List.length new_children) in
    List.iter
      (fun c ->
        Hashtbl.replace target_set (Renderable.Private.num (child_node c)) ())
      new_children;
    List.iter
      (fun c ->
        let node = child_node c in
        if not (Hashtbl.mem target_set (Renderable.Private.num node)) then
          Renderable.detach node)
      old_children
  end;
  (* Pass 2: place each child at its target index. The probe reads the live
     child array, so mutations from attach/detach are always visible. *)
  List.iteri
    (fun i c ->
      let node = child_node c in
      let already_correct =
        match Renderable.Private.child_at parent i with
        | Some n -> n == node
        | None -> false
      in
      if not already_correct then Renderable.attach ~parent ~index:i node)
    new_children

(* ───── Reconciliation ───── *)

type t = {
  container : Renderable.t;
  mutable root_children : child list;
  mutable pending_autofocus : Renderable.t list;
  mutable memos : memo_cache option;
}

let create ~container =
  { container; root_children = []; pending_autofocus = []; memos = None }

let container t = t.container

let apply_pending_autofocus t =
  match t.pending_autofocus with
  | [] -> ()
  | nodes ->
      List.iter (fun node -> ignore (Renderable.focus node : bool)) nodes;
      t.pending_autofocus <- []

let combine_focus left right =
  match (left, right) with
  | Focus_none, focus | focus, Focus_none -> focus
  | _ -> Focus_many

let children_focus children =
  List.fold_left
    (fun focus -> function
      | Embedded _ -> focus
      | Fiber fiber -> combine_focus focus fiber.declared_focus)
    Focus_none children

let update_declared_focus fiber =
  fiber.declared_focus <-
    (if not fiber.attrs.visible then Focus_none
     else
       combine_focus
         (if fiber.attrs.focused then Focus_one (fiber_node fiber)
          else Focus_none)
         (children_focus fiber.children))

let rec reconcile_element (t : t) ~(parent_node : Renderable.t)
    (maps : fiber_maps) (pos : int) ~viewport_width (elem : unit Vnode.element)
    : fiber =
  match find_match maps pos elem with
  | Some f ->
      let focus_changed =
        f.attrs.focused <> elem.attrs.focused
        || f.attrs.visible <> elem.attrs.visible
      in
      let children_changed =
        f.declared_children != elem.children
        || f.viewport_width <> viewport_width
      in
      ignore (update_fiber f elem : bool);
      if children_changed then begin
        let memos, flattened =
          flatten_children ~memos:f.memos ~viewport_width elem.children
        in
        f.memos <- memos;
        f.children <-
          reconcile_flattened t ~parent_node:(fiber_node f) ~viewport_width
            f.children flattened;
        f.declared_children <- elem.children;
        f.viewport_width <- viewport_width
      end;
      if focus_changed || children_changed then update_declared_focus f;
      f
  | None ->
      let f = create_fiber ~parent:parent_node elem in
      Option.iter (fun callback -> callback (fiber_node f)) elem.attrs.ref;
      if elem.attrs.autofocus && elem.attrs.focusable then
        t.pending_autofocus <- fiber_node f :: t.pending_autofocus;
      let memos, flattened =
        flatten_children ~memos:None ~viewport_width elem.children
      in
      f.memos <- memos;
      f.children <-
        reconcile_flattened t ~parent_node:(fiber_node f) ~viewport_width []
          flattened;
      f.declared_children <- elem.children;
      f.viewport_width <- viewport_width;
      update_declared_focus f;
      f

and reconcile_flattened (t : t) ~(parent_node : Renderable.t) ~viewport_width
    (old_children : child list) (flattened : flattened list) : child list =
  let old_fibers = fibers_of_children old_children in
  let maps = build_maps old_fibers in
  let old_len = Array.length maps.by_index in
  (* elem_idx tracks position among elements only -- embedded nodes are not
     managed by fiber maps, so they don't consume a positional slot. *)
  let elem_idx = ref 0 in
  let new_children =
    List.map
      (fun fc ->
        match fc with
        | Flat_element elem ->
            let idx = !elem_idx in
            incr elem_idx;
            Fiber
              (reconcile_element t ~parent_node maps idx ~viewport_width elem)
        | Flat_embed n -> Embedded n)
      flattened
  in
  (* Destroy unused old fibers *)
  for i = 0 to old_len - 1 do
    if not maps.used.(i) then destroy_fiber maps.by_index.(i)
  done;
  commit_placement parent_node ~old_children ~new_children;
  (* Apply deferred autofocus after placement, so the node is fully attached to
     the render tree when focus is applied. *)
  apply_pending_autofocus t;
  new_children

(* ───── Root Reconciliation ───── *)

let render (t : t) ~viewport_width (vnode : unit Vnode.t) : unit =
  if viewport_width < 1 then
    invalid_arg "Reconciler.render: viewport_width must be positive";
  let memos, flattened =
    flatten_children ~memos:t.memos ~viewport_width [ vnode ]
  in
  t.memos <- memos;
  let new_children =
    reconcile_flattened t ~parent_node:t.container ~viewport_width
      t.root_children flattened
  in
  t.root_children <- new_children;
  Renderable.request_render t.container

let unmount (t : t) : unit =
  List.iter
    (function Fiber f -> destroy_fiber f | Embedded n -> Renderable.detach n)
    t.root_children;
  t.root_children <- [];
  t.memos <- None;
  Renderable.request_render t.container

let focused t =
  match children_focus t.root_children with
  | Focus_none -> None
  | Focus_one node -> Some node
  | Focus_many -> invalid_arg "Mosaic: more than one focused element"
