open Windtrap
open Mosaic_ui
module Key = Matrix_input.Key
module Modifier = Matrix_input.Modifier

type bindings = Default | Override | Replace | Precedence

let bindings mode ~key ~forward ~backward =
  match mode with
  | Default -> []
  | Override ->
      [
        (Mosaic.Shortcut.key key, backward); (Mosaic.Shortcut.char 'n', forward);
      ]
  | Replace -> [ (Mosaic.Shortcut.char 'n', backward) ]
  | Precedence ->
      [
        (Shortcut.char 'q', backward);
        (Shortcut.char 'Q', backward);
        (Shortcut.char 'q', forward);
        (Shortcut.char 'x', backward);
      ]

let on_key prevent event =
  if prevent then Event.Key.prevent_default event;
  None

let change value = "change:" ^ value
let activate value = "activate:" ^ value
let values = [ "A"; "B"; "C" ]
let size = Mosaic.size ~width:30 ~height:6

let tabs ~mlx ~mode ~selected ~prevent =
  let create = if mlx then Mosaic_mlx.tab_select else Mosaic.tab_select in
  create ~size ~focused:true ~equal:String.equal ~selected:(Some selected)
    ~on_change:change ~on_activate:activate ~on_key:(on_key prevent)
    ~show_description:false ~tab_width:8
    ~key_bindings:
      (bindings mode ~key:Key.Right ~forward:Mosaic.Tab_select.Move_right
         ~backward:Mosaic.Tab_select.Move_left)
    (List.map (fun value -> Mosaic.Tab_select.item value value) values)

let table ~mlx ~mode ~selected ~prevent =
  let create = if mlx then Mosaic_mlx.table else Mosaic.table in
  create ~size ~focused:true ~equal:String.equal ~selected:(Some selected)
    ~on_change:change ~on_activate:activate ~on_key:(on_key prevent)
    ~show_header:false ~border:false
    ~key_bindings:
      (bindings mode ~key:Key.Down ~forward:Mosaic.Table.Move_down
         ~backward:Mosaic.Table.Move_up)
    ~columns:[ Mosaic.Table.column "Value" ]
    ~rows:
      (List.map
         (fun value -> Mosaic.Table.row value [| Mosaic.Table.cell value |])
         values)
    ()

let tree ~mlx ~mode ~selected ~prevent =
  let create = if mlx then Mosaic_mlx.tree else Mosaic.tree in
  create ~size ~focused:true
    ~state:(Mosaic.Tree.State.make ~selected ())
    ~on_change:(fun state ->
      change (Option.get (Mosaic.Tree.State.selected state)))
    ~on_activate:(fun item -> activate item.Mosaic.Tree.id)
    ~on_key:(on_key prevent)
    ~key_bindings:
      (bindings mode ~key:Key.Down ~forward:Mosaic.Tree.Move_down
         ~backward:Mosaic.Tree.Move_up)
    ~items:(List.map (fun id -> Mosaic.Tree.item ~id id) values)
    ()

let widgets =
  [
    ("tabs", tabs, Key.Right, Key.KP_right);
    ("table", table, Key.Down, Key.KP_down);
    ("tree", tree, Key.Down, Key.KP_down);
  ]

let fixture build ~mlx =
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer)
  in
  let received = ref [] in
  let render ?(mode = Default) ?(selected = "B") ?(prevent = false) () =
    Mosaic.Reconciler.render reconciler ~viewport_width:30
      (Vnode.map
         (fun message -> received := message :: !received)
         (build ~mlx ~mode ~selected ~prevent));
    (match Mosaic.Reconciler.focused reconciler with
    | Some node -> ignore (Renderer.focus renderer node : bool)
    | None -> failwith "navigation fixture has no focused widget");
    Renderer.render_frame renderer ~width:30 ~height:6 ~delta:0.
  in
  let send ?modifier ?event_type ?base_key key =
    ignore
      (Renderer.dispatch_key renderer
         (Key.make ?modifier ?event_type ?base_key key)
        : Event.key)
  in
  let take () =
    let result = List.rev !received in
    received := [];
    result
  in
  (render, send, take)

let exact_matching_and_accepted_state build forward keypad ~mlx =
  let render, send, take = fixture build ~mlx in
  render ();
  List.iter
    (fun modifier ->
      send ~modifier forward;
      send ~modifier (Key.Char (Uchar.of_char 'j'));
      equal ~msg:"modified navigation has no action" (list string) [] (take ()))
    [
      { Modifier.none with ctrl = true };
      { Modifier.none with alt = true; meta = true };
      { Modifier.none with hyper = true };
    ];
  send ~event_type:Release forward;
  equal ~msg:"release has no action" (list string) [] (take ());
  send ~event_type:Repeat forward;
  send ~event_type:Repeat forward;
  equal ~msg:"each repeat proposes from accepted selection" (list string)
    [ "change:C"; "change:C" ] (take ());
  send Key.KP_enter;
  equal ~msg:"unaccepted proposals do not alter activation" (list string)
    [ "activate:B" ] (take ());
  render ~selected:"C" ();
  send Key.Enter;
  equal ~msg:"accepted declaration changes activation" (list string)
    [ "activate:C" ] (take ());
  render ();
  send keypad;
  equal ~msg:"keypad navigation shares typed matching" (list string)
    [ "change:C" ] (take ());
  render ~prevent:true ();
  send forward;
  send Key.Enter;
  equal ~msg:"ordinary on_key prevention suppresses widget defaults"
    (list string) [] (take ())

let custom_binding_lifecycle build forward ~mlx =
  let render, send, take = fixture build ~mlx in
  render ~mode:Override ();
  send forward;
  equal ~msg:"custom binding overrides default" (list string) [ "change:A" ]
    (take ());
  send (Key.Char (Uchar.of_char 'n'));
  equal ~msg:"custom binding adds a key" (list string) [ "change:C" ] (take ());
  send ~base_key:(Uchar.of_char 'n') (Key.Char (Uchar.of_int 0x043d));
  equal ~msg:"custom binding uses base-layout fallback" (list string)
    [ "change:C" ] (take ());
  render ~mode:Replace ();
  send (Key.Char (Uchar.of_char 'n'));
  equal ~msg:"replacement changes the existing key action" (list string)
    [ "change:A" ] (take ());
  send forward;
  equal ~msg:"replacement restores omitted default override" (list string)
    [ "change:C" ] (take ());
  render ();
  send (Key.Char (Uchar.of_char 'n'));
  equal ~msg:"removing custom bindings removes the added key" (list string) []
    (take ());
  send forward;
  equal ~msg:"default navigation remains after removal" (list string)
    [ "change:C" ] (take ())

let subscriptions_and_widgets_agree build ~mlx =
  let render, send, take = fixture build ~mlx in
  render ~mode:Precedence ();
  let bindings =
    bindings Precedence ~key:Key.Down ~forward:"change:C" ~backward:"change:A"
  in
  let dispatch =
    match Mosaic.Sub.on_keys bindings with
    | Mosaic.Sub.On_key f -> f
    | _ -> fail "expected key subscription"
  in
  List.iter
    (fun (key, base_key, event_type) ->
      let data = Key.make ?base_key ~event_type key in
      let expected = dispatch (Event.Key.of_input data) in
      send ?base_key ~event_type key;
      equal ~msg:"subscription and compiled widget resolve the same binding"
        (list string) (Option.to_list expected) (take ()))
    [
      (Key.Char (Uchar.of_char 'q'), None, Press);
      (Key.Char (Uchar.of_char 'Q'), None, Repeat);
      (Key.Char (Uchar.of_char 'x'), Some (Uchar.of_char 'q'), Press);
      (Key.Char (Uchar.of_char 'X'), Some (Uchar.of_char 'q'), Press);
      (Key.Char (Uchar.of_int 0x0441), Some (Uchar.of_char 'Q'), Press);
      (Key.Char (Uchar.of_char 'q'), None, Release);
    ]

let () =
  run "mosaic.navigation-keys"
    (List.concat_map
       (fun (name, build, forward, keypad) ->
         List.map
           (fun mlx ->
             let api = if mlx then "MLX" else "Mosaic" in
             group
               (name ^ " " ^ api)
               [
                 test "matching, interception and model ownership" (fun () ->
                     exact_matching_and_accepted_state build forward keypad ~mlx);
                 test "custom binding lifecycle" (fun () ->
                     custom_binding_lifecycle build forward ~mlx);
                 test "subscription and widget resolution agree" (fun () ->
                     subscriptions_and_widgets_agree build ~mlx);
               ])
           [ false; true ])
       widgets)
