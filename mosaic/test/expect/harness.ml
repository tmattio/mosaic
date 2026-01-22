open Mosaic
module Renderer = Mosaic_ui.Renderer
module Renderable = Mosaic_ui.Renderable
module Event = Mosaic_ui.Event

type ('model, 'msg) t = {
  mutable model : 'model;
  mutable pending_msgs : 'msg list;
  app : ('model, 'msg) app;
  renderer : Renderer.t;
  reconciler : Reconciler.t;
  vte : Vte.t;
  grid : Grid.t;
  hits : Screen.Hit_grid.t;
  mutable width : int;
  mutable height : int;
  mutable key_subs : (Event.key -> 'msg option) list;
  mutable mouse_subs : (Event.mouse -> 'msg option) list;
  mutable paste_subs : (Event.paste -> 'msg option) list;
  mutable resize_sub : (width:int -> height:int -> 'msg) option;
  mutable tick_sub : (dt:float -> 'msg) option;
  mutable every_subs : (float * float * (unit -> 'msg)) list;
  mutable focus_sub : 'msg option;
  mutable blur_sub : 'msg option;
}

let rec process_cmd t (cmd : _ Cmd.t) =
  match cmd with
  | Cmd.None -> ()
  | Cmd.Batch cmds -> List.iter (process_cmd t) cmds
  | Cmd.Perform f -> f (fun msg -> t.pending_msgs <- msg :: t.pending_msgs)
  | Cmd.Quit -> () (* Test harness ignores quit *)
  | Cmd.Set_title _ -> () (* Test harness ignores set_title *)
  | Cmd.Focus _ -> () (* Test harness ignores focus *)
  | Cmd.Static_write _ -> () (* Test harness ignores static_write *)
  | Cmd.Static_print _ -> () (* Test harness ignores static_print *)
  | Cmd.Static_clear -> () (* Test harness ignores static_clear *)

let rec collect_subs t (sub : _ Sub.t) =
  match sub with
  | Sub.None -> ()
  | Sub.Batch subs -> List.iter (collect_subs t) subs
  | Sub.Every (interval, f) -> t.every_subs <- (interval, 0., f) :: t.every_subs
  | Sub.On_tick f -> t.tick_sub <- Some f
  | Sub.On_key f -> t.key_subs <- f :: t.key_subs
  | Sub.On_key_all f -> t.key_subs <- f :: t.key_subs
  | Sub.On_mouse f -> t.mouse_subs <- f :: t.mouse_subs
  | Sub.On_mouse_all f -> t.mouse_subs <- f :: t.mouse_subs
  | Sub.On_paste f -> t.paste_subs <- f :: t.paste_subs
  | Sub.On_paste_all f -> t.paste_subs <- f :: t.paste_subs
  | Sub.On_resize f -> t.resize_sub <- Some f
  | Sub.On_focus msg -> t.focus_sub <- Some msg
  | Sub.On_blur msg -> t.blur_sub <- Some msg

let update_subscriptions t =
  t.key_subs <- [];
  t.mouse_subs <- [];
  t.paste_subs <- [];
  t.resize_sub <- None;
  t.tick_sub <- None;
  t.every_subs <- [];
  t.focus_sub <- None;
  t.blur_sub <- None;
  collect_subs t (t.app.subscriptions t.model)

let dispatch t msg =
  let model', cmd = t.app.update msg t.model in
  t.model <- model';
  process_cmd t cmd;
  update_subscriptions t

let process_pending_msgs t =
  while t.pending_msgs <> [] do
    let msgs = List.rev t.pending_msgs in
    t.pending_msgs <- [];
    List.iter (dispatch t) msgs
  done

let compile ~dispatch (view : 'msg Mosaic.t) : unit Vnode.t =
  Vnode.map_handlers (function Some msg -> dispatch msg | None -> ()) view

let ensure_size t =
  if Vte.rows t.vte <> t.height || Vte.cols t.vte <> t.width then begin
    Vte.resize t.vte ~rows:t.height ~cols:t.width;
    Vte.reset t.vte
  end;
  if Grid.width t.grid <> t.width || Grid.height t.grid <> t.height then begin
    Grid.resize t.grid ~width:t.width ~height:t.height;
    Screen.Hit_grid.resize t.hits ~width:t.width ~height:t.height
  end;
  ignore (Renderer.resize t.renderer ~width:t.width ~height:t.height)

let create ?(subscriptions = fun _ -> Sub.none) ~width ~height ~init ~update
    ~view () =
  let model, init_cmd = init () in
  let renderer = Renderer.create () in
  ignore (Renderer.resize renderer ~width ~height);
  let container =
    match Renderer.create_node renderer ~id:"__harness_root__" () with
    | Ok node -> node
    | Error _ -> failwith "Failed to create harness root container"
  in
  ignore (Renderer.set_root renderer container);
  let reconciler = Reconciler.create renderer ~container in
  let vte = Vte.create ~rows:height ~cols:width () in
  let grid = Grid.create ~width ~height () in
  let hits = Screen.Hit_grid.create ~width ~height in
  let t =
    {
      model;
      pending_msgs = [];
      app = { init; update; view; subscriptions };
      renderer;
      reconciler;
      vte;
      grid;
      hits;
      width;
      height;
      key_subs = [];
      mouse_subs = [];
      paste_subs = [];
      resize_sub = None;
      tick_sub = None;
      every_subs = [];
      focus_sub = None;
      blur_sub = None;
    }
  in
  process_cmd t init_cmd;
  update_subscriptions t;
  t

let render t ~delta =
  ensure_size t;
  process_pending_msgs t;
  let view = t.app.view t.model in
  let dispatch msg = t.pending_msgs <- msg :: t.pending_msgs in
  let vnode = compile ~dispatch view in
  Reconciler.render t.reconciler vnode;
  Grid.clear t.grid;
  Screen.Hit_grid.clear t.hits;
  let _cursor = Renderer.render_into t.renderer t.grid t.hits ~delta in
  let ansi = Grid.snapshot ~reset:true t.grid in
  Vte.feed_string t.vte ansi

let snapshot t = Vte.to_string t.vte
let snapshot_ansi ?(reset = true) t = Grid.snapshot ~reset t.grid

let step t ~delta =
  render t ~delta;
  snapshot t

let step_ansi ?(reset = true) t ~delta =
  render t ~delta;
  snapshot_ansi ~reset t

let tick t ~delta =
  match t.tick_sub with Some f -> dispatch t (f ~dt:delta) | None -> ()

let resize t ~width ~height =
  t.width <- width;
  t.height <- height;
  ensure_size t;
  match t.resize_sub with Some f -> dispatch t (f ~width ~height) | None -> ()

let model t = t.model
let send t msg = dispatch t msg

let handle_key t (ev : Event.key) =
  List.iter
    (fun f -> match f ev with Some msg -> dispatch t msg | None -> ())
    t.key_subs;
  Renderer.handle_key t.renderer ev

let handle_mouse t (ev : Event.mouse) =
  List.iter
    (fun f -> match f ev with Some msg -> dispatch t msg | None -> ())
    t.mouse_subs;
  Renderer.handle_mouse t.renderer ev

let handle_paste t (ev : Event.paste) =
  List.iter
    (fun f -> match f ev with Some msg -> dispatch t msg | None -> ())
    t.paste_subs;
  Renderer.handle_paste t.renderer ev

let send_key t key_event =
  let ev = Event.Key.of_input key_event in
  handle_key t ev

let send_paste t text =
  let ev = Event.Paste.of_text text in
  handle_paste t ev

let key = Input.key_event
let char = Input.char_event
let press = Input.press
let repeat = Input.repeat
let release = Input.release
let focus t = match t.focus_sub with Some msg -> dispatch t msg | None -> ()
let blur t = match t.blur_sub with Some msg -> dispatch t msg | None -> ()
