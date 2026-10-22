(** TEA (The Elm Architecture) runtime for mosaic. *)

open Mosaic_ui

(* External re-exports *)

module Ansi = Matrix.Ansi
module Border = Matrix.Grid.Border
module Event = Mosaic_ui.Event
module Canvas = Mosaic_ui.Canvas
module Scroll = Mosaic_ui.Scroll
module Editor = Mosaic_ui.Editor

module Focus = struct
  let next ~equal order current =
    match order with
    | [] -> None
    | first :: _ -> (
        match current with
        | None -> Some first
        | Some current ->
            let rec scan = function
              | [] | [ _ ] -> Some first
              | item :: (following :: _ as rest) ->
                  if equal item current then Some following else scan rest
            in
            scan order)

  let prev ~equal order current =
    match order with
    | [] -> None
    | first :: rest -> (
        let rec last item = function
          | [] -> Some item
          | item :: rest -> last item rest
        in
        match current with
        | None -> last first rest
        | Some current when equal first current -> last first rest
        | Some current ->
            let rec scan previous = function
              | [] -> Some previous
              | item :: rest ->
                  if equal item current then Some previous else scan item rest
            in
            scan first rest)
end

module Shortcut = Mosaic_ui.Shortcut

(* Geometry type aliases *)

type 'a size = 'a Toffee.Geometry.Size.t = { width : 'a; height : 'a }

type 'a rect = 'a Toffee.Geometry.Rect.t = {
  left : 'a;
  right : 'a;
  top : 'a;
  bottom : 'a;
}

type 'a point = 'a Toffee.Geometry.Point.t = { x : 'a; y : 'a }
type 'a line = 'a Toffee.Geometry.Line.t = { start : 'a; end_ : 'a }
type dimension = Toffee.Style.Dimension.t
type length_percentage = Toffee.Style.Length_percentage.t
type length_percentage_auto = Toffee.Style.Length_percentage_auto.t
type span = Mosaic_ui.Text_buffer.span = { text : string; style : Ansi.Style.t }

(* Layout enum modules *)

module Display = Toffee.Style.Display
module Position = Toffee.Style.Position
module Box_sizing = Toffee.Style.Box_sizing
module Overflow = Toffee.Style.Overflow
module Text_align = Toffee.Style.Text_align
module Flex_direction = Toffee.Style.Flex_direction
module Flex_wrap = Toffee.Style.Flex_wrap
module Align = Toffee.Style.Align_items
module Justify = Toffee.Style.Align_content
module Grid_auto_flow = Toffee.Style.Grid_auto_flow

(* Grid module *)

module Grid = struct
  type template = Toffee.Style.Grid_template_component.t

  let fr = Toffee.Style.Grid_template_component.fr
  let length = Toffee.Style.Grid_template_component.length
  let percent = Toffee.Style.Grid_template_component.percent
  let auto = Toffee.Style.Grid_template_component.auto
  let min_content = Toffee.Style.Grid_template_component.min_content
  let max_content = Toffee.Style.Grid_template_component.max_content
  let fit_content = Toffee.Style.Grid_template_component.fit_content
  let minmax = Toffee.Style.Grid_template_component.minmax

  type placement = Toffee.Style.Grid_placement.t

  let line = Toffee.Style.Grid_placement.line
  let span = Toffee.Style.Grid_placement.span
  let auto_placement = Toffee.Style.Grid_placement.auto

  let line_range s e : placement Toffee.Geometry.Line.t =
    {
      start = Toffee.Style.Grid_placement.line s;
      end_ = Toffee.Style.Grid_placement.line e;
    }

  let span_range s n : placement Toffee.Geometry.Line.t =
    {
      start = Toffee.Style.Grid_placement.line s;
      end_ = Toffee.Style.Grid_placement.span n;
    }

  type track = Toffee.Style.Track_sizing_function.t

  let track_fr = Toffee.Style.Track_sizing_function.fr
  let track_length = Toffee.Style.Track_sizing_function.length
  let track_percent = Toffee.Style.Track_sizing_function.percent
  let track_auto = Toffee.Style.Track_sizing_function.auto
  let track_min_content = Toffee.Style.Track_sizing_function.min_content
  let track_max_content = Toffee.Style.Track_sizing_function.max_content

  type area = Toffee.Style.Grid_template_area.t
end

(* Narrowed widget modules *)

module Select = struct
  type action = Mosaic_ui.Select.action =
    | Move_up
    | Move_down
    | Move_up_fast
    | Move_down_fast
    | Activate

  type 'a item = { value : 'a; presentation : Mosaic_ui.Select.item }

  let item value ?description label =
    { value; presentation = { Mosaic_ui.Select.label; description } }
end

module Tab_select = struct
  type action = Mosaic_ui.Tab_select.action =
    | Move_left
    | Move_right
    | Activate

  type 'a item = { value : 'a; presentation : Mosaic_ui.Tab_select.item }

  let item value ?(description = "") label =
    { value; presentation = { Mosaic_ui.Tab_select.label; description } }
end

module Table = struct
  include Mosaic_ui.Table

  type 'a row = { value : 'a; cells : cell array }

  let row value cells = { value; cells }
end

module Tree = Mosaic_ui.Tree
module Spinner = Mosaic_ui.Spinner
module Slider = Mosaic_ui.Slider
module Scroll_bar = Mosaic_ui.Scroll_bar
module Text = Mosaic_ui.Text
module Text_surface = Mosaic_ui.Text_surface
module Line_number = Mosaic_ui.Line_number
module Code = Mosaic_ui.Code
module Diff = Mosaic_ui.Diff
module Markdown = Mosaic_ui.Markdown
module Syntax_highlight = Mosaic_ui.Syntax_highlight
module Syntax_style = Mosaic_ui.Syntax_style

(* Internal modules for tests *)

module Reconciler = Reconciler

(* Semantic callbacks produce messages; raw event handlers may ignore events. *)
type 'msg t = 'msg Vnode.t

let map (f : 'a -> 'b) (view : 'a t) : 'b t = Vnode.map f view

let compile ~(dispatch : 'msg -> unit) (view : 'msg t) : unit Vnode.t =
  Vnode.map dispatch view

module Cmd = struct
  type 'msg t =
    | None
    | Batch of 'msg t list
    | Perform of (('msg -> unit) -> unit)
    | Quit
    | Set_title of string
    | Copy_to_clipboard of string
    | Copy_selection
    | Query_color_scheme
    | Bell
    | Notify of { title : string; body : string }
    | Clear_selection
    | Static_commit of 'msg Vnode.t
    | Static_clear

  let none = None
  let batch cmds = Batch cmds
  let perform f = Perform f
  let quit = Quit
  let set_title title = Set_title title
  let copy_to_clipboard text = Copy_to_clipboard text
  let copy_selection = Copy_selection
  let query_color_scheme = Query_color_scheme
  let bell = Bell
  let notify ~title ~body = Notify { title; body }
  let clear_selection = Clear_selection
  let static_commit view = Static_commit view
  let static_clear = Static_clear

  let rec map (f : 'a -> 'b) (cmd : 'a t) : 'b t =
    match cmd with
    | None -> None
    | Batch cmds -> Batch (List.map (map f) cmds)
    | Perform g -> Perform (fun dispatch -> g (fun msg -> dispatch (f msg)))
    | Quit -> Quit
    | Set_title title -> Set_title title
    | Copy_to_clipboard text -> Copy_to_clipboard text
    | Copy_selection -> Copy_selection
    | Query_color_scheme -> Query_color_scheme
    | Bell -> Bell
    | Notify { title; body } -> Notify { title; body }
    | Clear_selection -> Clear_selection
    | Static_commit view -> Static_commit (Vnode.map f view)
    | Static_clear -> Static_clear
end

module Sub = struct
  type 'msg t =
    | None
    | Batch of 'msg t list
    | Every of string * float * (unit -> 'msg)
    | On_tick of string * (dt:float -> 'msg)
    | On_key of (Event.key -> 'msg option)
    | On_key_all of (Event.key -> 'msg option)
    | On_mouse of (Event.mouse -> 'msg option)
    | On_mouse_all of (Event.mouse -> 'msg option)
    | On_paste of (Event.paste -> 'msg option)
    | On_paste_all of (Event.paste -> 'msg option)
    | On_resize of (width:int -> height:int -> 'msg)
    | On_focus of 'msg
    | On_blur of 'msg
    | On_color_scheme of ([ `Dark | `Light ] -> 'msg option)

  let none = None
  let batch subs = Batch subs

  let every ~id interval f =
    if id = "" || (not (Float.is_finite interval)) || interval <= 0. then
      invalid_arg
        "Mosaic.Sub.every: nonempty id and positive finite interval required";
    Every (id, interval, f)

  let on_tick ~id f =
    if id = "" then invalid_arg "Mosaic.Sub.on_tick: empty id";
    On_tick (id, f)

  let on_key f = On_key f

  let on_keys bindings =
    On_key
      (fun ev ->
        let message = Shortcut.Private.find bindings ev in
        if Option.is_some message then Event.Key.prevent_default ev;
        message)

  let on_key_all f = On_key_all f
  let on_mouse f = On_mouse f
  let on_mouse_all f = On_mouse_all f
  let on_paste f = On_paste f
  let on_paste_all f = On_paste_all f
  let on_resize f = On_resize f
  let on_focus msg = On_focus msg
  let on_blur msg = On_blur msg
  let on_color_scheme f = On_color_scheme f

  let rec map (f : 'a -> 'b) (sub : 'a t) : 'b t =
    match sub with
    | None -> None
    | Batch subs -> Batch (List.map (map f) subs)
    | Every (id, interval, g) -> Every (id, interval, fun () -> f (g ()))
    | On_tick (id, g) -> On_tick (id, fun ~dt -> f (g ~dt))
    | On_key g -> On_key (fun ev -> Option.map f (g ev))
    | On_key_all g -> On_key_all (fun ev -> Option.map f (g ev))
    | On_mouse g -> On_mouse (fun ev -> Option.map f (g ev))
    | On_mouse_all g -> On_mouse_all (fun ev -> Option.map f (g ev))
    | On_paste g -> On_paste (fun ev -> Option.map f (g ev))
    | On_paste_all g -> On_paste_all (fun ev -> Option.map f (g ev))
    | On_resize g -> On_resize (fun ~width ~height -> f (g ~width ~height))
    | On_focus msg -> On_focus (f msg)
    | On_blur msg -> On_blur (f msg)
    | On_color_scheme g ->
        On_color_scheme (fun scheme -> Option.map f (g scheme))
end

type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> 'msg t;
  subscriptions : 'model -> 'msg Sub.t;
}

(* Every-timer state. A timer's deadlines are the fixed-rate schedule
   [origin +. n *. interval] for [n = 1, 2, ...], computed from the origin and
   the fire count each time rather than carried forward by addition. The
   computed deadline rounds once; [due +. interval] rounds at every re-arm, and
   on a wall-clock origin near 1.7e9 — where a double's spacing is 2.4e-7 —
   adding 0.1 lands on 0.0999999046 every time, a fast drift of a microsecond
   per second. *)
type 'msg every_timer = {
  id : string;
  interval : float;
  origin : float;
  fired : int;
  callback : unit -> 'msg;
}

(* The schedule is computed, but the clock measured against it need not be: a
   virtual clock stepped by 0.1 eight times and the nominal [8 *. 0.1] differ
   by an ULP, in either direction. Exact comparison turns that noise into a
   stalled step (the clock lands short, so neither the wakeup nor the fire
   test sees the deadline until the next step) or a lost final fire (the
   clock stops at the nominal instant, the deadline sits beyond it). The
   slack is far above that noise and far below any sane interval. *)
let every_slack = 1e-6

(* The instant a timer's wakeup is armed for and its fire test compares
   against — the same float on both sides, so a frame the wakeup releases
   always fires the timer. *)
let every_deadline timer =
  timer.origin
  +. (float_of_int (timer.fired + 1) *. timer.interval)
  -. every_slack

type ('model, 'msg) runtime = {
  mutable model : 'model;
  (* [Cmd.perform] may dispatch from any thread (see mosaic.mli), so the
     pending-message queue is a lock-free stack: pushes are CAS loops and the
     loop thread drains with a single [Atomic.exchange]. A plain mutable list
     would lose messages pushed between the drain's read and its clear. *)
  pending_msgs : 'msg list Atomic.t;
  mutable view_dirty : bool;
  mutable synchronizing : bool;
  mutable delivering : bool;
  mutable map_message : 'msg -> unit;
  app : ('model, 'msg) app;
  matrix_app : Matrix.app;
  process_perform : (unit -> unit) -> unit;
  renderer : Renderer.t;
  reconciler : Reconciler.t;
  mutable key_subs : (bool * (Event.key -> 'msg option)) list;
  mutable mouse_subs : (bool * (Event.mouse -> 'msg option)) list;
  mutable paste_subs : (bool * (Event.paste -> 'msg option)) list;
  mutable resize_sub : (width:int -> height:int -> 'msg) list;
  mutable tick_sub : (string * (dt:float -> 'msg)) list;
  mutable every_subs : 'msg every_timer list;
  mutable focus_sub : 'msg list;
  mutable blur_sub : 'msg list;
  mutable color_scheme_sub : ([ `Dark | `Light ] -> 'msg option) list;
  mutable sub_live_active : bool;
}

(* Push [msg] and wake the loop. Safe from any thread: the CAS retries until
   the stack is extended atomically, and [Matrix.request_redraw] is a benign
   flag write plus a backend wake. *)
let push_msg runtime msg =
  let rec push () =
    let cur = Atomic.get runtime.pending_msgs in
    if not (Atomic.compare_and_set runtime.pending_msgs cur (msg :: cur)) then
      push ()
  in
  push ();
  Matrix.request_redraw runtime.matrix_app

(* Static output renders offscreen with an auto-height root, mirroring the
   primary-screen path: one pass at the terminal height measures the content's
   natural height (layout is independent of the grid), and only when the
   content is taller does a second pass paint it at that exact height. Width
   comes from [effective_size], the same source the live view lays out
   against. *)
let render_static_view runtime (view : _ t) =
  let width, _ = Matrix.effective_size runtime.matrix_app in
  let width = max 1 width in
  let _, full_height = Matrix.full_size runtime.matrix_app in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_display Toffee.Style.Display.Block
    |> Toffee.Style.set_width
         (Toffee.Style.Dimension.length (Float.of_int width))
    |> Toffee.Style.set_height Toffee.Style.Dimension.auto
  in
  let renderer = Renderer.create ~style () in
  let reconciler = Reconciler.create ~container:(Renderer.root renderer) in
  let vnode = compile ~dispatch:(fun _ -> ()) view in
  Reconciler.render reconciler ~viewport_width:width vnode;
  let measure_height = max 1 full_height in
  Renderer.render_frame renderer ~width ~height:measure_height ~delta:0.;
  let required = Renderable.height (Renderer.root renderer) in
  if required > measure_height then
    Renderer.render_frame renderer ~width ~height:required ~delta:0.;
  let grid = Matrix.Screen.next_grid (Renderer.screen renderer) in
  let used_rows = Matrix.Grid.active_height grid in
  (grid, used_rows)

let rec process_cmd runtime (cmd : _ Cmd.t) =
  match cmd with
  | Cmd.None -> ()
  | Cmd.Batch cmds -> List.iter (process_cmd runtime) cmds
  | Cmd.Perform f -> runtime.process_perform (fun () -> f (push_msg runtime))
  | Cmd.Quit -> Matrix.stop runtime.matrix_app
  | Cmd.Set_title title ->
      let term = Matrix.terminal runtime.matrix_app in
      Matrix.Terminal.set_title term title
  | Cmd.Copy_to_clipboard text ->
      Matrix.Terminal.copy_to_clipboard
        (Matrix.terminal runtime.matrix_app)
        text
  | Cmd.Copy_selection -> (
      match Renderer.selection_text runtime.renderer with
      | Some text when text <> "" ->
          Matrix.Terminal.copy_to_clipboard
            (Matrix.terminal runtime.matrix_app)
            text
      | Some _ | None -> ())
  | Cmd.Query_color_scheme ->
      Matrix.Terminal.query_color_scheme (Matrix.terminal runtime.matrix_app)
  | Cmd.Bell -> Matrix.Terminal.bell (Matrix.terminal runtime.matrix_app)
  | Cmd.Notify { title; body } ->
      Matrix.Terminal.notify (Matrix.terminal runtime.matrix_app) ~title ~body
  | Cmd.Clear_selection -> Renderer.clear_selection runtime.renderer
  | Cmd.Static_commit view ->
      let grid, rows = render_static_view runtime view in
      Matrix.static_write ~rows runtime.matrix_app grid
  | Cmd.Static_clear -> Matrix.static_clear runtime.matrix_app

let rec collect_subs runtime (sub : _ Sub.t) =
  match sub with
  | Sub.None -> ()
  | Sub.Batch subs -> List.iter (collect_subs runtime) subs
  | Sub.Every (id, interval, callback) ->
      (* The schedule is assigned in [update_subscriptions], where the current
         time and the previous timer generation are both at hand. *)
      runtime.every_subs <-
        { id; interval; origin = Float.nan; fired = 0; callback }
        :: runtime.every_subs
  | Sub.On_tick (id, f) -> runtime.tick_sub <- (id, f) :: runtime.tick_sub
  | Sub.On_key f -> runtime.key_subs <- (false, f) :: runtime.key_subs
  | Sub.On_key_all f -> runtime.key_subs <- (true, f) :: runtime.key_subs
  | Sub.On_mouse f -> runtime.mouse_subs <- (false, f) :: runtime.mouse_subs
  | Sub.On_mouse_all f -> runtime.mouse_subs <- (true, f) :: runtime.mouse_subs
  | Sub.On_paste f -> runtime.paste_subs <- (false, f) :: runtime.paste_subs
  | Sub.On_paste_all f -> runtime.paste_subs <- (true, f) :: runtime.paste_subs
  | Sub.On_resize f -> runtime.resize_sub <- f :: runtime.resize_sub
  | Sub.On_focus msg -> runtime.focus_sub <- msg :: runtime.focus_sub
  | Sub.On_blur msg -> runtime.blur_sub <- msg :: runtime.blur_sub
  | Sub.On_color_scheme f ->
      runtime.color_scheme_sub <- f :: runtime.color_scheme_sub

(* Arm the loop's one-shot wakeup at the earliest every-sub deadline, or clear
   it when no timer is pending. Runs after every subscription recollection and
   after every frame callback — both can move the earliest deadline. *)
let arm_every_wakeup runtime =
  let deadline =
    List.fold_left
      (fun acc timer ->
        let due = every_deadline timer in
        match acc with None -> Some due | Some acc -> Some (Float.min acc due))
      None runtime.every_subs
  in
  Matrix.schedule_wakeup runtime.matrix_app deadline

let update_subscriptions runtime =
  let prev_every = runtime.every_subs in
  runtime.key_subs <- [];
  runtime.mouse_subs <- [];
  runtime.paste_subs <- [];
  runtime.resize_sub <- [];
  runtime.tick_sub <- [];
  runtime.every_subs <- [];
  runtime.focus_sub <- [];
  runtime.blur_sub <- [];
  runtime.color_scheme_sub <- [];
  collect_subs runtime (runtime.app.subscriptions runtime.model);
  runtime.key_subs <- List.rev runtime.key_subs;
  runtime.mouse_subs <- List.rev runtime.mouse_subs;
  runtime.paste_subs <- List.rev runtime.paste_subs;
  runtime.resize_sub <- List.rev runtime.resize_sub;
  runtime.tick_sub <- List.rev runtime.tick_sub;
  runtime.focus_sub <- List.rev runtime.focus_sub;
  runtime.blur_sub <- List.rev runtime.blur_sub;
  runtime.color_scheme_sub <- List.rev runtime.color_scheme_sub;
  let ids = Hashtbl.create 8 in
  let check_id id =
    if id = "" || Hashtbl.mem ids id then
      invalid_arg ("Mosaic.Sub: empty or duplicate timer id: " ^ id);
    Hashtbl.add ids id ()
  in
  List.iter
    (fun (id, callback) ->
      check_id id;
      ignore callback)
    runtime.tick_sub;
  let now = Matrix.now runtime.matrix_app in
  runtime.every_subs <-
    List.rev_map
      (fun timer ->
        check_id timer.id;
        if (not (Float.is_finite timer.interval)) || timer.interval <= 0. then
          invalid_arg "Mosaic.Sub.every: positive finite interval required";
        match
          List.find_opt
            (fun prev ->
              String.equal prev.id timer.id
              && Float.equal prev.interval timer.interval)
            prev_every
        with
        | Some prev -> { timer with origin = prev.origin; fired = prev.fired }
        | None -> { timer with origin = now })
      runtime.every_subs;
  (* Track subscription-driven liveness: only tick subscriptions require the
     render cadence — a tick's [dt] is frame time by definition. Every-subs
     are timers, not animations: they wake the loop at their deadline
     ([arm_every_wakeup]) and the messages they dispatch request their own
     redraws, so a pending timer costs one wakeup, not a full render pipeline
     per frame over the whole mounted tree. *)
  let has_live_subs = runtime.tick_sub <> [] in
  if has_live_subs && not runtime.sub_live_active then (
    runtime.sub_live_active <- true;
    Matrix.request_live runtime.matrix_app)
  else if (not has_live_subs) && runtime.sub_live_active then (
    runtime.sub_live_active <- false;
    Matrix.drop_live runtime.matrix_app);
  arm_every_wakeup runtime

let dispatch runtime msg =
  let model', cmd = runtime.app.update msg runtime.model in
  runtime.view_dirty <- runtime.view_dirty || not (model' == runtime.model);
  runtime.model <- model';
  process_cmd runtime cmd;
  update_subscriptions runtime

let process_pending_msgs runtime =
  let rec drain () =
    match Atomic.exchange runtime.pending_msgs [] with
    | [] -> ()
    | msgs ->
        List.iter (dispatch runtime) (List.rev msgs);
        drain ()
  in
  drain ()

(* The space the tree is laid out in, for input and for frames alike. In
   [`Primary] mode it is the terminal's height, the most rows the live region
   can take; the region is as tall as the frame's content. *)
let layout_size runtime =
  match Matrix.mode runtime.matrix_app with
  | `Alt -> Matrix.effective_size runtime.matrix_app
  | `Primary -> Matrix.full_size runtime.matrix_app

(* Only the loop thread runs the application and reconciler. Widget callbacks
   complete their updates before default processing continues. During a view or
   geometry refresh, callbacks enqueue instead of re-entering reconciliation. *)
let synchronize runtime =
  if not runtime.synchronizing then begin
    runtime.synchronizing <- true;
    Fun.protect
      ~finally:(fun () -> runtime.synchronizing <- false)
      (fun () ->
        let rec settle () =
          process_pending_msgs runtime;
          if runtime.view_dirty then begin
            runtime.view_dirty <- false;
            let width, height = layout_size runtime in
            let vnode =
              Vnode.map runtime.map_message (runtime.app.view runtime.model)
            in
            Reconciler.render runtime.reconciler ~viewport_width:(max 1 width)
              vnode;
            (match Reconciler.focused runtime.reconciler with
            | Some node -> ignore (Renderer.focus runtime.renderer node : bool)
            | None -> Renderer.blur runtime.renderer);
            Renderer.prepare_layout runtime.renderer ~width ~height;
            settle ()
          end
        in
        settle ())
  end

let deliver runtime msg =
  push_msg runtime msg;
  if not runtime.delivering then synchronize runtime

let handle_key runtime event =
  let rec loop = function
    | [] -> ()
    | (all_events, f) :: rest ->
        if all_events || not (Event.Key.default_prevented event) then
          Option.iter (deliver runtime) (f event);
        if not (Event.Key.propagation_stopped event) then loop rest
  in
  loop runtime.key_subs

let handle_mouse runtime event =
  let rec loop = function
    | [] -> ()
    | (all_events, f) :: rest ->
        if all_events || not (Event.Mouse.default_prevented event) then
          Option.iter (deliver runtime) (f event);
        if not (Event.Mouse.propagation_stopped event) then loop rest
  in
  loop runtime.mouse_subs

let handle_paste runtime event =
  let rec loop = function
    | [] -> ()
    | (all_events, f) :: rest ->
        if all_events || not (Event.Paste.default_prevented event) then
          Option.iter (deliver runtime) (f event);
        if not (Event.Paste.propagation_stopped event) then loop rest
  in
  loop runtime.paste_subs

let handle_resize runtime ~width ~height =
  runtime.view_dirty <- true;
  List.iter (fun f -> push_msg runtime (f ~width ~height)) runtime.resize_sub;
  synchronize runtime

let handle_tick runtime ~dt =
  List.iter (fun pair -> push_msg runtime ((snd pair) ~dt)) runtime.tick_sub

let handle_every_subs runtime =
  let now = Matrix.now runtime.matrix_app in
  runtime.every_subs <-
    List.map
      (fun timer ->
        if now >= every_deadline timer then begin
          push_msg runtime (timer.callback ());
          { timer with fired = timer.fired + 1 }
        end
        else timer)
      runtime.every_subs

let handle_input runtime input =
  synchronize runtime;
  (match input with
  | Matrix.Input.Focus -> List.iter (deliver runtime) runtime.focus_sub
  | Matrix.Input.Blur -> List.iter (deliver runtime) runtime.blur_sub
  | Matrix.Input.Color_scheme scheme ->
      List.iter
        (fun f -> Option.iter (deliver runtime) (f scheme))
        runtime.color_scheme_sub
  | Matrix.Input.Key key_event ->
      ignore
        (Renderer.dispatch_key ~capture:(handle_key runtime) runtime.renderer
           key_event
          : Event.key)
  | Matrix.Input.Mouse mouse_event ->
      let event = Renderer.dispatch_mouse runtime.renderer mouse_event in
      handle_mouse runtime event
  | Matrix.Input.Paste text ->
      ignore
        (Renderer.dispatch_paste ~capture:(handle_paste runtime)
           runtime.renderer text
          : Event.paste)
  | _ -> ());
  synchronize runtime

let render runtime = synchronize runtime

module Probe = struct
  type check = { name : string; is_pending : unit -> bool }
  type t = check list

  let empty = []
  let with_pending ~name is_pending t = t @ [ { name; is_pending } ]

  let pending t =
    List.filter_map
      (fun check -> if check.is_pending () then Some check.name else None)
      t

  let is_settled t = not (List.exists (fun check -> check.is_pending ()) t)
end

let run ?matrix
    ?(process_perform = fun thunk -> ignore (Thread.create thunk () : Thread.t))
    ?probe app =
  let matrix_app =
    match matrix with
    | Some matrix -> matrix
    | None ->
        Matrix.create ~target_fps:(Some 60.) ~cursor_visible:false
          ~start_idle:true ()
  in
  (* When a probe is requested, count in-flight performs and nudge the loop on
     completion so a driver blocked on quiescence re-evaluates. *)
  let in_flight_performs = Atomic.make 0 in
  let process_perform =
    match probe with
    | None -> process_perform
    | Some _ ->
        fun thunk ->
          Atomic.incr in_flight_performs;
          process_perform (fun () ->
              Fun.protect thunk ~finally:(fun () ->
                  Atomic.decr in_flight_performs;
                  Matrix.request_redraw matrix_app))
  in
  let model, init_cmd = app.init () in
  let renderer_style =
    match Matrix.mode matrix_app with
    | `Primary ->
        Some
          (Toffee.Style.default
          |> Toffee.Style.set_width (Toffee.Style.Dimension.pct 100.)
          |> Toffee.Style.set_height Toffee.Style.Dimension.auto)
    | `Alt -> None
  in
  let rendering = Atomic.make false in
  let renderer =
    Renderer.create ?style:renderer_style ~screen:(Matrix.screen matrix_app)
      ~request_render:(fun () ->
        if not (Atomic.get rendering) then Matrix.request_redraw matrix_app)
      ()
  in
  let container = Renderer.root renderer in
  let reconciler = Reconciler.create ~container in
  let runtime =
    {
      model;
      pending_msgs = Atomic.make [];
      view_dirty = true;
      synchronizing = false;
      delivering = false;
      map_message = Fun.const ();
      app;
      matrix_app;
      process_perform;
      renderer;
      reconciler;
      key_subs = [];
      mouse_subs = [];
      paste_subs = [];
      resize_sub = [];
      tick_sub = [];
      every_subs = [];
      focus_sub = [];
      blur_sub = [];
      color_scheme_sub = [];
      sub_live_active = false;
    }
  in
  runtime.map_message <- deliver runtime;
  (match probe with
  | None -> ()
  | Some receive ->
      receive
        (Probe.empty
        |> Probe.with_pending ~name:"messages" (fun () ->
            Atomic.get runtime.pending_msgs <> [])
        |> Probe.with_pending ~name:"performs" (fun () ->
            Atomic.get in_flight_performs > 0)
        |> Probe.with_pending ~name:"render" (fun () ->
            not (Renderer.is_settled renderer))));
  process_cmd runtime init_cmd;
  update_subscriptions runtime;

  (* Drive Matrix loop activity from Renderable.live_count: start the
     render cadence when any node needs continuous rendering, stop when
     none do. *)
  let root = Renderer.root renderer in
  let live_was_positive = ref false in
  Renderable.Private.set_on_live_count_change root
    (Some
       (fun _node ->
         let count = Renderable.Private.live_count root in
         let is_positive = count > 0 in
         if is_positive && not !live_was_positive then (
           live_was_positive := true;
           Matrix.request_live matrix_app)
         else if (not is_positive) && !live_was_positive then (
           live_was_positive := false;
           Matrix.drop_live matrix_app)));

  let frame_delta = ref 0. in
  (* In [`Primary] mode the live region takes the rows of the frame before it
     is drawn, so the frame is laid out first, against the terminal's height,
     the most rows the region can have, and drawn once the region has its
     rows. Messages the layout delivers wait for the next frame, as those of a
     paint do. *)
  let primary_required_rows app =
    match Matrix.mode app with
    | `Primary ->
        render runtime;
        let width, height = layout_size runtime in
        runtime.delivering <- true;
        Fun.protect
          ~finally:(fun () -> runtime.delivering <- false)
          (fun () ->
            Renderer.layout_frame runtime.renderer ~width ~height
              ~delta:!frame_delta);
        Some (max 1 (Renderable.height (Renderer.root runtime.renderer)))
    | `Alt -> None
  in

  let finish_render () =
    (* Reconciliation invalidations are consumed by this frame. A worker or
       widget may still request a follow-up while it renders, and a message
       delivered during the frame waits for the next one. Clear the gate
       before checking so a concurrent notification cannot be lost. *)
    Atomic.set rendering false;
    if
      Renderer.needs_render runtime.renderer
      || Atomic.get runtime.pending_msgs <> []
    then Matrix.request_redraw runtime.matrix_app
  in
  let render_frame () =
    (match Matrix.mode runtime.matrix_app with
    | `Alt ->
        render runtime;
        let width, height = Matrix.size runtime.matrix_app in
        Renderer.render_frame runtime.renderer ~width ~height
          ~delta:!frame_delta
    | `Primary ->
        Renderer.paint_frame runtime.renderer
          ~height:(snd (Matrix.size runtime.matrix_app)));
    (* The renderer built directly into the runtime's screen; Matrix.submit
       presents it with a single diff. Forward the frame's hardware scroll
       hint so a scrolled viewport diffs only its revealed rows. *)
    (match Renderer.take_scroll_hint runtime.renderer with
    | Some hint -> Matrix.set_scroll_hint runtime.matrix_app hint
    | None -> ());
    ()
  in
  let on_render _app =
    Atomic.set rendering true;
    runtime.delivering <- true;
    Fun.protect
      ~finally:(fun () ->
        runtime.delivering <- false;
        finish_render ())
      render_frame
  in

  Fun.protect
    ~finally:(fun () -> Reconciler.unmount runtime.reconciler)
    (fun () ->
      Matrix.run runtime.matrix_app ~primary_required_rows
        ~on_frame:(fun _app ~dt ->
          handle_tick runtime ~dt;
          handle_every_subs runtime;
          synchronize runtime;
          (* A fired timer moved its deadline even when no message re-collected
         subscriptions, so the wakeup re-arms here. *)
          arm_every_wakeup runtime;
          (* Matrix reports [dt] in seconds; the render pipeline runs on
         milliseconds. Convert once at the boundary. *)
          frame_delta := dt *. 1000.)
        ~on_input:(fun _app input -> handle_input runtime input)
        ~on_resize:(fun _app ~cols ~rows ->
          handle_resize runtime ~width:cols ~height:rows)
        ~on_render)

let empty = Vnode.empty
let fragment = Vnode.fragment
let embed = Vnode.embed
let viewport_switch = Vnode.viewport_switch
let memo = Vnode.memo

let layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding
    ?border_width ?gap ?align_items ?align_self ?align_content ?justify_items
    ?justify_self ?justify_content ?flex_direction ?flex_wrap ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    () =
  Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding
    ?border:border_width ?gap ?align_items ?align_self ?align_content
    ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
    ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ()

let box ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?border ?border_style
    ?border_sides ?border_color ?focused_border_color ?background ?fill ?title
    ?title_alignment children =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.box ?key ~style ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?border
    ?border_style ?border_sides ?border_color ?focused_border_color ?background
    ?fill ?title ?title_alignment children

let text ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?style ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key
    ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?text_style ?wrap
    ?selectable ?selection_bg ?selection_fg ?tab_width ?truncate content =
  let layout_style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  let text_style =
    match text_style with Some _ -> text_style | None -> style
  in
  Vnode.text ?key ~style:layout_style ?visible ?z_index ?opacity ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused
    ?text_style ?wrap ?selectable ?selection_bg ?selection_fg ?tab_width
    ?truncate content

let rich_text ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?style ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key
    ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?text_style ?wrap
    ?selectable ?selection_bg ?selection_fg ?tab_width ?truncate content =
  let layout_style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  let text_style =
    match text_style with Some _ -> text_style | None -> style
  in
  Vnode.rich_text ?key ~style:layout_style ?visible ?z_index ?opacity ?buffered
    ?live ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ?focused ?text_style ?wrap ?selectable ?selection_bg ?selection_fg
    ?tab_width ?truncate content

let button ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_focus_request ?on_size
    ?(focused = false) ?border ?border_style ?border_sides ?border_color
    ?focused_border_color ?background ?fill ?title ?title_alignment ?text_style
    ?(disabled = false) ~on_activate label =
  let text_style = Option.value text_style ~default:Ansi.Style.default in
  let text_style =
    if disabled then Ansi.Style.with_dim true text_style
    else if focused then Ansi.Style.with_inverse true text_style
    else text_style
  in
  let on_focus_request = if disabled then None else on_focus_request in
  let on_key event =
    if
      (not disabled)
      && (not (Event.Key.default_prevented event))
      && (Event.Key.data event).event_type = Matrix.Input.Key.Press
      && (Shortcut.matches Shortcut.enter event
         || Shortcut.matches Shortcut.space event)
    then begin
      Event.Key.prevent_default event;
      Some on_activate
    end
    else None
  in
  let on_click event =
    if disabled || Event.Mouse.default_prevented event then None
    else begin
      Event.Mouse.prevent_default event;
      Some on_activate
    end
  in
  box ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width ?text_align
    ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items ?size
    ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_focus_request ?on_size
    ~focused:(focused && not disabled) ?border ?border_style ?border_sides
    ?border_color ?focused_border_color ?background ?fill ?title
    ?title_alignment ~on_key ~on_click
    [ text ~text_style ~selectable:false label ]

let slider ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?orientation ?min ?max
    ?viewport_size ?track_color ?thumb_color ~state ~on_change () =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.slider ~value:state ~on_value_change:on_change ?key ~style ?visible
    ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste ?on_click
    ?on_focus_request ?on_size ?focused ?orientation ?min ?max ?viewport_size
    ?track_color ?thumb_color ()

let input ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?placeholder ?text_color
    ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?key_bindings
    ~on_change ?on_submit ~state () =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.input ~state ?key ~style ?visible ?z_index ?opacity ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused
    ?placeholder ?text_color ?background_color ?focused_text_color
    ?focused_background_color ?placeholder_color ?selection_color ?selection_fg
    ?cursor_style ?cursor_color ?cursor_blinking ?selectable ?show_cursor
    ?key_bindings ~on_change ?on_submit ()

let selection_index ~equal ~value selected items =
  match selected with
  | None -> -1
  | Some selected ->
      let rec find index =
        if index = Array.length items then -1
        else if equal (value items.(index)) selected then index
        else find (index + 1)
      in
      find 0

let select ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?background ?text_color
    ?focused_background ?focused_text_color ?selected_background
    ?selected_text_color ?description_color ?selected_description_color
    ?show_description ?show_scroll_indicator ?wrap_selection ?key_bindings
    ?item_spacing ?fast_scroll_step ~on_change ?on_activate ~equal ~selected
    options =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  let items = Array.of_list options in
  let selected_index =
    selection_index ~equal ~value:(fun item -> item.Select.value) selected items
  in
  let options = List.map (fun item -> item.Select.presentation) options in
  let value index = items.(index).Select.value in
  let on_change index = on_change (value index) in
  let on_activate = Option.map (fun f index -> f (value index)) on_activate in
  Vnode.select ~selected_index ?key ~style ?visible ?z_index ?opacity ?buffered
    ?live ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ?focused ~options ?background ?text_color ?focused_background
    ?focused_text_color ?selected_background ?selected_text_color
    ?description_color ?selected_description_color ?show_description
    ?show_scroll_indicator ?wrap_selection ?key_bindings ?item_spacing
    ?fast_scroll_step ~on_change ?on_activate ()

let tab_select ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?tab_width ?background
    ?text_color ?focused_background ?focused_text_color ?selected_background
    ?selected_text_color ?description_color ?selected_description_color
    ?show_underline ?show_description ?show_scroll_arrows ?wrap_selection
    ?key_bindings ~on_change ?on_activate ~equal ~selected options =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  let items = Array.of_list options in
  let selected =
    selection_index ~equal
      ~value:(fun item -> item.Tab_select.value)
      selected items
  in
  let options = List.map (fun item -> item.Tab_select.presentation) options in
  let value index = items.(index).Tab_select.value in
  let on_change index = on_change (value index) in
  let on_activate = Option.map (fun f index -> f (value index)) on_activate in
  Vnode.tab_select ~selected ?key ~style ?visible ?z_index ?opacity ?buffered
    ?live ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ?focused ~options ?tab_width ?background ?text_color ?focused_background
    ?focused_text_color ?selected_background ?selected_text_color
    ?description_color ?selected_description_color ?show_underline
    ?show_description ?show_scroll_arrows ?wrap_selection ?key_bindings
    ~on_change ?on_activate ()

let canvas ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused draw =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.canvas ?key ~style ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused
    ~on_draw:draw ()

let spinner ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?frame_set ?color () =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.spinner ?key ~style ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?frame_set
    ?color ()

let progress_bar ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?value ?min ?max ?orientation
    ?filled_color ?empty_color () =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.progress_bar ?key ~style ?visible ?z_index ?opacity ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused
    ?value ?min ?max ?orientation ?filled_color ?empty_color ()

let scroll_bar ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?orientation ?show_arrows
    ?track_color ?thumb_color ?arrow_fg ?arrow_bg ~on_change ~state ~scroll_size
    ~viewport_size () =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.scroll_bar ~position:state ~scroll_size ~viewport_size ?key ~style
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?orientation ?show_arrows
    ?track_color ?thumb_color ?arrow_fg ?arrow_bg ~on_change ()

let scroll_box ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?scroll_x ?scroll_y
    ?background ?show_scrollbars ?scrollbar_style ?vertical_bar_style
    ?horizontal_bar_style ~on_scroll ~scroll children =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.scroll_box ~scroll ?key ~style ?visible ?z_index ?opacity ?buffered
    ?live ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ?focused ?scroll_x ?scroll_y ?background ?show_scrollbars ?scrollbar_style
    ?vertical_bar_style ?horizontal_bar_style ~on_scroll children

let textarea ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?annotations ?ghost_text
    ?ghost_text_color ?placeholder ?wrap ?text_color ?background_color
    ?focused_text_color ?focused_background_color ?placeholder_color
    ?selection_color ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking
    ?selectable ?show_cursor ?key_bindings ~on_change ?on_submit ~state () =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.textarea ~state ?key ~style ?visible ?z_index ?opacity ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused
    ?annotations ?ghost_text ?ghost_text_color ?placeholder ?wrap ?text_color
    ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor ~on_change
    ?key_bindings ?on_submit ()

let code ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?syntax ?text_style ?wrap
    ?tab_width ?selectable ?selection_bg ?selection_fg ?on_selection content =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.code ?key ~style ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?syntax
    ?text_style ?wrap ?tab_width ?selectable ?selection_bg ?selection_fg
    ?on_selection content

let line_number ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?fg ?bg ?min_width
    ?padding_right ?show_line_numbers ?line_number_offset ?line_colors
    ?line_signs ?hidden_line_numbers child =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.line_number ?key ~style ?visible ?z_index ?opacity ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused
    ?fg ?bg ?min_width ?padding_right ?show_line_numbers ?line_number_offset
    ?line_colors ?line_signs ?hidden_line_numbers child

let diff ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?layout ?theme ?highlight
    ?line_highlights ?line_spans ?line_signs ?show_line_numbers ?wrap
    ?selectable ?text_style ?on_line_click patch =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.diff ?key ~style ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?layout
    ?theme ?highlight ?line_highlights ?line_spans ?line_signs
    ?show_line_numbers ?wrap ?selectable ?text_style ?on_line_click patch

let markdown ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?md_style ?conceal ?streaming
    ?selectable ?selection_bg ?selection_fg ?highlighter ?syntax_style
    ?on_selection content =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.markdown ?key ~style ?visible ?z_index ?opacity ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused
    ?md_style ?conceal ?streaming ?selectable ?selection_bg ?selection_fg
    ?highlighter ?syntax_style ?on_selection content

let table ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?columns ?(rows = []) ?border
    ?border_style ?show_header ?show_column_separator ?show_row_separator
    ?cell_padding ?header_color ?header_background ?text_color ?background
    ?selected_text_color ?selected_background ?focused_selected_text_color
    ?focused_selected_background ?selection_visible ?show_scroll_indicator
    ?activate_on_click ?wheel_navigation ?row_styles ?wrap_selection
    ?key_bindings ?fast_scroll_step ~on_change ?on_activate ?on_hover ~equal
    ~selected () =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  let items = Array.of_list rows in
  let selected_row =
    selection_index ~equal ~value:(fun item -> item.Table.value) selected items
  in
  let rows = List.map (fun item -> item.Table.cells) rows in
  let value index = items.(index).Table.value in
  let on_change index = on_change (value index) in
  let on_activate = Option.map (fun f index -> f (value index)) on_activate in
  let on_hover =
    Option.map (fun f index -> f (Option.map value index)) on_hover
  in
  Vnode.table ~selected_row ?key ~style ?visible ?z_index ?opacity ?buffered
    ?live ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ?focused ?columns ~rows ?border ?border_style ?show_header
    ?show_column_separator ?show_row_separator ?cell_padding ?header_color
    ?header_background ?text_color ?background ?selected_text_color
    ?selected_background ?focused_selected_text_color
    ?focused_selected_background ?selection_visible ?show_scroll_indicator
    ?activate_on_click ?wheel_navigation ?row_styles ?wrap_selection
    ?key_bindings ?fast_scroll_step ~on_change ?on_activate ?on_hover ()

let tree ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?items ?indent_size
    ?show_guides ?guide_style ?expand_icon ?collapse_icon ?leaf_icon ?background
    ?text_color ?selected_background ?selected_text_color
    ?focused_selected_background ?focused_selected_text_color ?guide_color
    ?icon_color ?wrap_selection ?key_bindings ?fast_scroll_step ~on_change
    ?on_activate ~state () =
  let style =
    layout_style ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin
      ?padding ?border_width ?gap ?align_items ?align_self ?align_content
      ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
      ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
      ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
      ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
      ?grid_row ?grid_column ()
  in
  Vnode.tree ~state ?key ~style ?visible ?z_index ?opacity ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused
    ?items ?indent_size ?show_guides ?guide_style ?expand_icon ?collapse_icon
    ?leaf_icon ?background ?text_color ?selected_background ?selected_text_color
    ?focused_selected_background ?focused_selected_text_color ?guide_color
    ?icon_color ?wrap_selection ?key_bindings ?fast_scroll_step ~on_change
    ?on_activate ()

let px = Vnode.px
let pct = Vnode.pct
let size = Vnode.size
let gap = Vnode.gap
let auto = Vnode.auto
let padding = Vnode.padding
let margin = Vnode.margin
let inset = Vnode.inset
let size_wh w h : dimension Toffee.Geometry.Size.t = { width = w; height = h }

let gap_xy x y : length_percentage Toffee.Geometry.Size.t =
  let x = Toffee.Style.Length_percentage.length (Float.of_int x) in
  let y = Toffee.Style.Length_percentage.length (Float.of_int y) in
  { width = x; height = y }

let padding_xy x y : length_percentage Toffee.Geometry.Rect.t =
  let x = Toffee.Style.Length_percentage.length (Float.of_int x) in
  let y = Toffee.Style.Length_percentage.length (Float.of_int y) in
  { left = x; right = x; top = y; bottom = y }

let padding_lrtb l r t b : length_percentage Toffee.Geometry.Rect.t =
  let f n = Toffee.Style.Length_percentage.length (Float.of_int n) in
  { left = f l; right = f r; top = f t; bottom = f b }

let margin_xy x y : length_percentage_auto Toffee.Geometry.Rect.t =
  let x = Toffee.Style.Length_percentage_auto.length (Float.of_int x) in
  let y = Toffee.Style.Length_percentage_auto.length (Float.of_int y) in
  { left = x; right = x; top = y; bottom = y }

let margin_lrtb l r t b : length_percentage_auto Toffee.Geometry.Rect.t =
  let f n = Toffee.Style.Length_percentage_auto.length (Float.of_int n) in
  { left = f l; right = f r; top = f t; bottom = f b }

let inset_lrtb l r t b : length_percentage_auto Toffee.Geometry.Rect.t =
  let f n = Toffee.Style.Length_percentage_auto.length (Float.of_int n) in
  { left = f l; right = f r; top = f t; bottom = f b }
