(** TEA (The Elm Architecture) runtime for Mosaic.

    This module provides a declarative, functional API for building terminal
    applications using the Model-View-Update pattern. *)

module Ansi = Matrix.Ansi
module Renderer = Mosaic_ui.Renderer
module Renderable = Mosaic_ui.Renderable
module Event = Mosaic_ui.Event
module Vnode = Vnode
module Reconciler = Reconciler

(* TEA view is a Vnode with 'msg option handler return type *)

type 'msg t = 'msg option Vnode.t
(** A TEA view node. Handlers return ['msg option] - [Some msg] to dispatch a
    message, [None] to ignore the event. *)

let map (f : 'a -> 'b) (view : 'a t) : 'b t =
  Vnode.map_handlers (Option.map f) view

let compile ~(dispatch : 'msg -> unit) (view : 'msg t) : unit Vnode.t =
  Vnode.map_handlers (function Some msg -> dispatch msg | None -> ()) view

(* Cmd module *)

module Cmd = struct
  type 'msg t =
    | None
    | Batch of 'msg t list
    | Perform of (('msg -> unit) -> unit)
    | Quit
    | Set_title of string
    | Focus of string
    | Static_write of string
    | Static_print of string
    | Static_clear

  let none = None
  let batch cmds = Batch cmds
  let perform f = Perform f
  let quit = Quit
  let set_title title = Set_title title
  let focus id = Focus id
  let static_write text = Static_write text
  let static_print text = Static_print text
  let static_clear = Static_clear

  let rec map (f : 'a -> 'b) (cmd : 'a t) : 'b t =
    match cmd with
    | None -> None
    | Batch cmds -> Batch (List.map (map f) cmds)
    | Perform g -> Perform (fun dispatch -> g (fun msg -> dispatch (f msg)))
    | Quit -> Quit
    | Set_title title -> Set_title title
    | Focus id -> Focus id
    | Static_write text -> Static_write text
    | Static_print text -> Static_print text
    | Static_clear -> Static_clear
end

(* Sub module *)

module Sub = struct
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

  let none = None
  let batch subs = Batch subs
  let every interval f = Every (interval, f)
  let on_tick f = On_tick f
  let on_key f = On_key f
  let on_mouse f = On_mouse f
  let on_paste f = On_paste f
  let on_resize f = On_resize f
  let on_focus msg = On_focus msg
  let on_blur msg = On_blur msg

  let rec map (f : 'a -> 'b) (sub : 'a t) : 'b t =
    match sub with
    | None -> None
    | Batch subs -> Batch (List.map (map f) subs)
    | Every (interval, g) -> Every (interval, fun () -> f (g ()))
    | On_tick g -> On_tick (fun ~dt -> f (g ~dt))
    | On_key g -> On_key (fun ev -> Option.map f (g ev))
    | On_mouse g -> On_mouse (fun ev -> Option.map f (g ev))
    | On_paste g -> On_paste (fun ev -> Option.map f (g ev))
    | On_resize g -> On_resize (fun ~width ~height -> f (g ~width ~height))
    | On_focus msg -> On_focus (f msg)
    | On_blur msg -> On_blur (f msg)
end

(* Application configuration *)

type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> 'msg t;
  subscriptions : 'model -> 'msg Sub.t;
}

(* Runtime state *)

type ('model, 'msg) runtime = {
  mutable model : 'model;
  mutable pending_msgs : 'msg list;
  mutable pending_focus : string list;
  app : ('model, 'msg) app;
  matrix_app : Matrix.app;
  renderer : Renderer.t;
  reconciler : Reconciler.t;
  mutable key_subs : (Event.key -> 'msg option) list;
  mutable mouse_subs : (Event.mouse -> 'msg option) list;
  mutable paste_subs : (Event.paste -> 'msg option) list;
  mutable resize_sub : (width:int -> height:int -> 'msg) option;
  mutable tick_sub : (dt:float -> 'msg) option;
  mutable every_subs : (float * float * (unit -> 'msg)) list;
  mutable focus_sub : 'msg option;
  mutable blur_sub : 'msg option;
}

let try_focus runtime id =
  match Renderer.find_by_id runtime.renderer id with
  | Some node ->
      Renderer.focus runtime.renderer node;
      true
  | None -> false

let enqueue_focus runtime id =
  if
    not
      (List.exists
         (fun existing -> String.equal existing id)
         runtime.pending_focus)
  then runtime.pending_focus <- runtime.pending_focus @ [ id ]

let process_pending_focus runtime =
  let focused = ref false in
  let remaining =
    List.filter
      (fun id ->
        match Renderer.find_by_id runtime.renderer id with
        | Some node ->
            Renderer.focus runtime.renderer node;
            focused := true;
            false
        | None -> true)
      runtime.pending_focus
  in
  runtime.pending_focus <- remaining;
  if !focused then Matrix.request_redraw runtime.matrix_app

(* Process commands *)

let rec process_cmd runtime (cmd : _ Cmd.t) =
  match cmd with
  | Cmd.None -> ()
  | Cmd.Batch cmds -> List.iter (process_cmd runtime) cmds
  | Cmd.Perform f ->
      f (fun msg ->
          runtime.pending_msgs <- msg :: runtime.pending_msgs;
          Matrix.request_redraw runtime.matrix_app)
  | Cmd.Quit -> Matrix.stop runtime.matrix_app
  | Cmd.Set_title title ->
      let term = Matrix.terminal runtime.matrix_app in
      Matrix.Terminal.set_title term title
  | Cmd.Focus id ->
      if not (try_focus runtime id) then (
        enqueue_focus runtime id;
        Matrix.request_redraw runtime.matrix_app)
  | Cmd.Static_write text -> Matrix.static_write runtime.matrix_app text
  | Cmd.Static_print text -> Matrix.static_print runtime.matrix_app text
  | Cmd.Static_clear -> Matrix.static_clear runtime.matrix_app

(* Collect subscriptions *)

let rec collect_subs runtime (sub : _ Sub.t) =
  match sub with
  | Sub.None -> ()
  | Sub.Batch subs -> List.iter (collect_subs runtime) subs
  | Sub.Every (interval, f) ->
      runtime.every_subs <- (interval, 0., f) :: runtime.every_subs
  | Sub.On_tick f -> runtime.tick_sub <- Some f
  | Sub.On_key f -> runtime.key_subs <- f :: runtime.key_subs
  | Sub.On_mouse f -> runtime.mouse_subs <- f :: runtime.mouse_subs
  | Sub.On_paste f -> runtime.paste_subs <- f :: runtime.paste_subs
  | Sub.On_resize f -> runtime.resize_sub <- Some f
  | Sub.On_focus msg -> runtime.focus_sub <- Some msg
  | Sub.On_blur msg -> runtime.blur_sub <- Some msg

let update_subscriptions runtime =
  runtime.key_subs <- [];
  runtime.mouse_subs <- [];
  runtime.paste_subs <- [];
  runtime.resize_sub <- None;
  runtime.tick_sub <- None;
  runtime.every_subs <- [];
  runtime.focus_sub <- None;
  runtime.blur_sub <- None;
  collect_subs runtime (runtime.app.subscriptions runtime.model)

(* Update cycle *)

let dispatch runtime msg =
  let model', cmd = runtime.app.update msg runtime.model in
  runtime.model <- model';
  process_cmd runtime cmd;
  update_subscriptions runtime

let process_pending_msgs runtime =
  while runtime.pending_msgs <> [] do
    let msgs = List.rev runtime.pending_msgs in
    runtime.pending_msgs <- [];
    List.iter (dispatch runtime) msgs
  done

(* Event handling *)

let handle_key runtime (event : Event.key) =
  List.iter
    (fun f ->
      match f event with Some msg -> dispatch runtime msg | None -> ())
    runtime.key_subs

let handle_mouse runtime (event : Event.mouse) =
  List.iter
    (fun f ->
      match f event with Some msg -> dispatch runtime msg | None -> ())
    runtime.mouse_subs

let handle_paste runtime (event : Event.paste) =
  List.iter
    (fun f ->
      match f event with Some msg -> dispatch runtime msg | None -> ())
    runtime.paste_subs

let handle_resize runtime ~width ~height =
  match runtime.resize_sub with
  | Some f -> dispatch runtime (f ~width ~height)
  | None -> ()

let handle_tick runtime ~dt =
  match runtime.tick_sub with Some f -> dispatch runtime (f ~dt) | None -> ()

let handle_every_subs runtime ~dt =
  runtime.every_subs <-
    List.map
      (fun (interval, elapsed, f) ->
        let new_elapsed = elapsed +. dt in
        if new_elapsed >= interval then begin
          dispatch runtime (f ());
          (interval, new_elapsed -. interval, f)
        end
        else (interval, new_elapsed, f))
      runtime.every_subs

(* Input event conversion *)

let convert_key_event (input : Matrix.Input.t) : Event.key option =
  match input with
  | Matrix.Input.Key key_event -> Some (Event.Key.of_input key_event)
  | _ -> None

let convert_mouse_event (input : Matrix.Input.t) : Event.mouse option =
  match input with
  | Matrix.Input.Mouse mouse_event -> (
      match mouse_event with
      | Matrix.Input.Mouse.Button_press (x, y, btn, mods) ->
          Some (Event.Mouse.down ~x ~y ~button:btn ~modifiers:mods)
      | Matrix.Input.Mouse.Button_release (x, y, btn, mods) ->
          Some (Event.Mouse.up ~x ~y ~button:btn ~modifiers:mods)
      | Matrix.Input.Mouse.Motion (x, y, state, mods) -> (
          let button =
            if state.left then Some Matrix.Input.Mouse.Left
            else if state.middle then Some Matrix.Input.Mouse.Middle
            else if state.right then Some Matrix.Input.Mouse.Right
            else None
          in
          match button with
          | Some btn ->
              Some (Event.Mouse.drag ~x ~y ~button:btn ~modifiers:mods)
          | None -> Some (Event.Mouse.move ~x ~y ~modifiers:mods)))
  | Matrix.Input.Scroll (x, y, dir, delta, modifiers) ->
      let direction : Event.Mouse.scroll_direction =
        match dir with
        | Matrix.Input.Mouse.Scroll_up -> Scroll_up
        | Matrix.Input.Mouse.Scroll_down -> Scroll_down
        | Matrix.Input.Mouse.Scroll_left -> Scroll_left
        | Matrix.Input.Mouse.Scroll_right -> Scroll_right
      in
      Some (Event.Mouse.scroll ~x ~y ~direction ~delta ~modifiers)
  | _ -> None

let convert_paste_event (input : Matrix.Input.t) : Event.paste option =
  match input with
  | Matrix.Input.Paste text -> Some (Event.Paste.of_text text)
  | _ -> None

let apply_cursor runtime cursor_opt =
  match cursor_opt with
  | None -> Matrix.set_cursor ~visible:false runtime.matrix_app
  | Some Renderable.{ x; y; color; style; blinking } ->
      let r, g, b, a = Ansi.Color.to_rgba color in
      let to_float v = float_of_int v /. 255. in
      Matrix.set_cursor_color runtime.matrix_app ~r:(to_float r) ~g:(to_float g)
        ~b:(to_float b) ~a:(to_float a);
      Matrix.set_cursor_style runtime.matrix_app ~style ~blinking;
      Matrix.set_cursor_position runtime.matrix_app ~row:y ~col:x;
      Matrix.set_cursor ~visible:true runtime.matrix_app

(* Main input handler *)

let handle_input runtime (input : Matrix.Input.t) =
  match input with
  | Matrix.Input.Focus -> (
      match runtime.focus_sub with
      | Some msg -> dispatch runtime msg
      | None -> ())
  | Matrix.Input.Blur -> (
      match runtime.blur_sub with
      | Some msg -> dispatch runtime msg
      | None -> ())
  | _ -> (
      match convert_key_event input with
      | Some ev ->
          handle_key runtime ev;
          Renderer.handle_key runtime.renderer ev
      | None -> (
          match convert_mouse_event input with
          | Some ev ->
              handle_mouse runtime ev;
              Renderer.handle_mouse runtime.renderer ev
          | None -> (
              match convert_paste_event input with
              | Some ev ->
                  handle_paste runtime ev;
                  Renderer.handle_paste runtime.renderer ev
              | None -> ())))

(* Rendering *)

let render runtime =
  process_pending_msgs runtime;
  let view = runtime.app.view runtime.model in
  (* Compile the TEA view to Vnode, closing over dispatch *)
  let dispatch msg =
    runtime.pending_msgs <- msg :: runtime.pending_msgs;
    Matrix.request_redraw runtime.matrix_app
  in
  let vnode = compile ~dispatch view in
  Reconciler.render runtime.reconciler vnode

(* Run the application *)

let run ?mode ?raw_mode ?target_fps ?respect_alpha ?mouse_enabled ?mouse
    ?bracketed_paste ?focus_reporting ?kitty_keyboard ?exit_on_ctrl_c
    ?debug_overlay ?debug_overlay_corner ?debug_overlay_capacity
    ?frame_dump_every ?frame_dump_dir ?frame_dump_pattern ?frame_dump_hits
    ?cursor_visible ?explicit_width ?render_thread ?input_timeout
    ?resize_debounce ?initial_caps ?output app =
  let model, init_cmd = app.init () in
  let matrix_app =
    let target_fps = Option.value target_fps ~default:(Some 60.) in
    let exit_on_ctrl_c = Option.value exit_on_ctrl_c ~default:true in
    Matrix.create ?mode ?raw_mode ~target_fps ?respect_alpha ?mouse_enabled
      ?mouse ?bracketed_paste ?focus_reporting ?kitty_keyboard ~exit_on_ctrl_c
      ?debug_overlay ?debug_overlay_corner ?debug_overlay_capacity
      ?frame_dump_every ?frame_dump_dir ?frame_dump_pattern ?frame_dump_hits
      ?cursor_visible ?explicit_width ?render_thread ?input_timeout
      ?resize_debounce ?initial_caps ?output ()
  in
  let width, height = Matrix.size matrix_app in
  let base_grid = Matrix.grid matrix_app in
  let renderer =
    Renderer.create
      ~glyph_pool:(Matrix.Grid.glyph_pool base_grid)
      ~width_method:(Matrix.Grid.width_method base_grid)
      ~respect_alpha:(Matrix.Grid.respect_alpha base_grid)
      ()
  in
  ignore (Renderer.resize renderer ~width ~height);
  let container =
    match Renderer.create_node renderer ~id:"__tea_root__" () with
    | Ok node -> node
    | Error _ -> failwith "Failed to create root container"
  in
  ignore (Renderer.set_root renderer container);
  let reconciler = Reconciler.create renderer ~container in
  let runtime =
    {
      model;
      pending_msgs = [];
      pending_focus = [];
      app;
      matrix_app;
      renderer;
      reconciler;
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
  process_cmd runtime init_cmd;
  update_subscriptions runtime;

  (* Store the frame delta for use in rendering *)
  let frame_delta = ref 0. in

  Matrix.run runtime.matrix_app
    ~on_frame:(fun _app ~dt ->
      handle_tick runtime ~dt;
      handle_every_subs runtime ~dt;
      frame_delta := dt)
    ~on_input:(fun _app input -> handle_input runtime input)
    ~on_resize:(fun _app ~cols ~rows ->
      ignore (Renderer.resize runtime.renderer ~width:cols ~height:rows);
      handle_resize runtime ~width:cols ~height:rows)
    ~on_render:(fun _app ->
      render runtime;
      let grid = Matrix.grid runtime.matrix_app in
      let hits = Matrix.hits runtime.matrix_app in
      let cursor =
        Renderer.render_into runtime.renderer grid hits ~delta:!frame_delta
      in
      apply_cursor runtime cursor;
      process_pending_focus runtime)

(* Re-export Vnode constructors for TEA views *)

let null = Vnode.null
let fragment = Vnode.fragment
let raw = Vnode.raw
let box = Vnode.box
let text = Vnode.text
let canvas = Vnode.canvas
let table = Vnode.table
let slider = Vnode.slider
let select = Vnode.select
let spinner = Vnode.spinner
let tab_select = Vnode.tab_select
let scroll_bar = Vnode.scroll_bar
let scroll_box = Vnode.scroll_box
let input = Vnode.input
let code = Vnode.code
let markdown = Vnode.markdown

(* Re-export renderable types for TEA views *)

module Table = Mosaic_ui.Table
module Slider = Mosaic_ui.Slider
module Select = Mosaic_ui.Select
module Spinner = Mosaic_ui.Spinner
module Tab_select = Mosaic_ui.Tab_select
module Scroll_bar = Mosaic_ui.Scroll_bar
module Text_input = Mosaic_ui.Text_input
module Code = Mosaic_ui.Code

(* Re-export dimension helpers *)

let px = Vnode.px
let pct = Vnode.pct
let size = Vnode.size
let gap = Vnode.gap
let auto = Vnode.auto
let padding = Vnode.padding
let margin = Vnode.margin
let inset = Vnode.inset
