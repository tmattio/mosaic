(** TEA (The Elm Architecture) runtime for mosaic. *)

open Mosaic_ui

(* External re-exports *)

module Ansi = Matrix.Ansi
module Border = Matrix.Grid.Border
module Event = Mosaic_ui.Event
module Canvas = Mosaic_ui.Canvas

module Shortcut = struct
  module Key = Matrix.Input.Key
  module Modifier = Matrix.Input.Modifier

  type t = { key : Key.t; modifier : Modifier.t }

  let modifier ?(ctrl = false) ?(alt = false) ?(shift = false) ?(super = false)
      ?(hyper = false) ?(meta = false) () =
    { Modifier.none with ctrl; alt; shift; super; hyper; meta = meta || alt }

  let key ?ctrl ?alt ?shift ?super ?hyper ?meta key =
    { key; modifier = modifier ?ctrl ?alt ?shift ?super ?hyper ?meta () }

  let char ?ctrl ?alt ?shift ?super ?hyper ?meta c =
    key ?ctrl ?alt ?shift ?super ?hyper ?meta (Key.Char (Uchar.of_char c))

  let ctrl c = char ~ctrl:true c
  let alt c = char ~alt:true c
  let shift c = char ~shift:true c
  let escape = key Key.Escape
  let enter = key Key.Enter
  let tab = key Key.Tab
  let backspace = key Key.Backspace
  let delete = key Key.Delete
  let f n = key (Key.F n)
  let up = key Key.Up
  let down = key Key.Down
  let left = key Key.Left
  let right = key Key.Right
  let space = char ' '

  let matches_modifier t (actual : Modifier.t) =
    Bool.equal t.modifier.ctrl actual.ctrl
    && Bool.equal t.modifier.alt actual.alt
    && Bool.equal t.modifier.shift actual.shift
    && Bool.equal t.modifier.super actual.super
    && Bool.equal t.modifier.hyper actual.hyper
    && Bool.equal t.modifier.meta actual.meta

  let normalized_char_key u =
    let code = Uchar.to_int u in
    if code >= Char.code 'A' && code <= Char.code 'Z' then
      Key.Char (Uchar.of_int (code + 32))
    else Key.Char u

  let add_key key keys =
    if List.exists (Key.equal key) keys then keys else key :: keys

  let matching_keys key base_key =
    let keys =
      match key with
      | Key.Char u -> add_key (normalized_char_key u) [ key ]
      | _ -> [ key ]
    in
    match base_key with
    | Some u -> add_key (normalized_char_key u) keys
    | None -> keys

  let matches t ev =
    let ev = Event.Key.data ev in
    match ev.event_type with
    | Key.Release -> false
    | Key.Press | Key.Repeat ->
        matches_modifier t ev.modifier
        && List.exists (Key.equal t.key) (matching_keys ev.key ev.base_key)
end

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

module Select = Mosaic_ui.Select
module Tab_select = Mosaic_ui.Tab_select
module Table = Mosaic_ui.Table
module Tree = Mosaic_ui.Tree
module Spinner = Mosaic_ui.Spinner
module Slider = Mosaic_ui.Slider
module Scroll_box = Mosaic_ui.Scroll_box
module Scroll_bar = Mosaic_ui.Scroll_bar
module Text_surface = Mosaic_ui.Text_surface
module Line_number = Mosaic_ui.Line_number
module Code = Mosaic_ui.Code
module Diff = Mosaic_ui.Diff
module Markdown = Mosaic_ui.Markdown
module Syntax_highlight = Mosaic_ui.Syntax_highlight
module Syntax_style = Mosaic_ui.Syntax_style

(* Internal modules for tests *)

module Reconciler = Reconciler

(* TEA view: handlers return ['msg option]. *)
type 'msg t = 'msg option Vnode.t

let map (f : 'a -> 'b) (view : 'a t) : 'b t = Vnode.map (Option.map f) view

let compile ~(dispatch : 'msg -> unit) (view : 'msg t) : unit Vnode.t =
  Vnode.map (function Some msg -> dispatch msg | None -> ()) view

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
    | Focus of string
    | Static_commit of 'msg option Vnode.t
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
  let focus id = Focus id
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
    | Focus id -> Focus id
    | Static_commit view -> Static_commit (Vnode.map (Option.map f) view)
    | Static_clear -> Static_clear
end

module Sub = struct
  type 'msg t =
    | None
    | Batch of 'msg t list
    | Every of float * (unit -> 'msg)
    | On_tick of (dt:float -> 'msg)
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
  let every interval f = Every (interval, f)
  let on_tick f = On_tick f
  let on_key f = On_key f

  let on_keys bindings =
    On_key
      (fun ev ->
        let rec loop : (Shortcut.t * 'msg) list -> 'msg option = function
          | [] -> None
          | (shortcut, msg) :: rest ->
              if Shortcut.matches shortcut ev then Some msg else loop rest
        in
        loop bindings)

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
    | Every (interval, g) -> Every (interval, fun () -> f (g ()))
    | On_tick g -> On_tick (fun ~dt -> f (g ~dt))
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

type ('model, 'msg) runtime = {
  mutable model : 'model;
  (* [Cmd.perform] may dispatch from any thread (see mosaic.mli), so the
     pending-message queue is a lock-free stack: pushes are CAS loops and the
     loop thread drains with a single [Atomic.exchange]. A plain mutable list
     would lose messages pushed between the drain's read and its clear. *)
  pending_msgs : 'msg list Atomic.t;
  mutable pending_focus : string list;
  app : ('model, 'msg) app;
  matrix_app : Matrix.app;
  process_perform : (unit -> unit) -> unit;
  renderer : Renderer.t;
  reconciler : Reconciler.t;
  mutable key_subs : (bool * (Event.key -> 'msg option)) list;
  mutable mouse_subs : (bool * (Event.mouse -> 'msg option)) list;
  mutable paste_subs : (bool * (Event.paste -> 'msg option)) list;
  mutable resize_sub : (width:int -> height:int -> 'msg) option;
  mutable tick_sub : (dt:float -> 'msg) option;
  (* Every-timer state: (interval, next_due, callback), where next_due is an
     absolute [Matrix.now] deadline. *)
  mutable every_subs : (float * float * (unit -> 'msg)) list;
  mutable focus_sub : 'msg option;
  mutable blur_sub : 'msg option;
  mutable color_scheme_sub : ([ `Dark | `Light ] -> 'msg option) option;
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

let rec find_by_id_in (node : Renderable.t) (id : string) : Renderable.t option
    =
  if String.equal (Renderable.id node) id then Some node
  else
    List.find_map
      (fun child -> find_by_id_in child id)
      (Renderable.children node)

let find_by_id runtime id = find_by_id_in (Renderer.root runtime.renderer) id

let try_focus runtime id =
  match find_by_id runtime id with
  | Some node -> Renderer.focus runtime.renderer node
  | None -> false

let enqueue_focus runtime id =
  if
    not
      (List.exists
         (fun existing -> String.equal existing id)
         runtime.pending_focus)
  then runtime.pending_focus <- runtime.pending_focus @ [ id ]

(* Each Cmd.focus gets one immediate attempt (process_cmd) and, when that
   misses, one deferred attempt after the next completed render — covering an
   element the update's own view is about to create. Requests that still miss
   are dropped: retrying forever would scan the whole tree every frame and
   retain the id, for a target that may never exist. *)
let process_pending_focus runtime =
  match runtime.pending_focus with
  | [] -> ()
  | pending ->
      runtime.pending_focus <- [];
      let focused = ref false in
      List.iter
        (fun id ->
          match find_by_id runtime id with
          | Some node ->
              if Renderer.focus runtime.renderer node then focused := true
          | None -> ())
        pending;
      if !focused then Matrix.request_redraw runtime.matrix_app

let serialize_grid_rows (grid : Matrix.Grid.t) ~rows =
  if rows <= 0 then ""
  else
    let width = Matrix.Grid.width grid in
    if rows = Matrix.Grid.height grid then Matrix.Grid.to_ansi ~reset:false grid
    else
      let cropped =
        Matrix.Grid.create ~width ~height:rows
          ~width_method:(Matrix.Grid.width_method grid)
          ()
      in
      Matrix.Grid.blit_region ~src:grid ~dst:cropped ~src_x:0 ~src_y:0 ~width
        ~height:rows ~dst_x:0 ~dst_y:0;
      Matrix.Grid.to_ansi ~reset:false cropped

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
  (serialize_grid_rows grid ~rows:used_rows, used_rows)

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
  | Cmd.Focus id ->
      if not (try_focus runtime id) then (
        enqueue_focus runtime id;
        Matrix.request_redraw runtime.matrix_app)
  | Cmd.Static_commit view ->
      let text, rows = render_static_view runtime view in
      Matrix.static_write runtime.matrix_app ~rows text
  | Cmd.Static_clear -> Matrix.static_clear runtime.matrix_app

let rec collect_subs runtime (sub : _ Sub.t) =
  match sub with
  | Sub.None -> ()
  | Sub.Batch subs -> List.iter (collect_subs runtime) subs
  | Sub.Every (interval, f) ->
      (* The deadline is assigned in [update_subscriptions], where the current
         time and the previous timer generation are both at hand. *)
      runtime.every_subs <- (interval, Float.nan, f) :: runtime.every_subs
  | Sub.On_tick f -> runtime.tick_sub <- Some f
  | Sub.On_key f -> runtime.key_subs <- (false, f) :: runtime.key_subs
  | Sub.On_key_all f -> runtime.key_subs <- (true, f) :: runtime.key_subs
  | Sub.On_mouse f -> runtime.mouse_subs <- (false, f) :: runtime.mouse_subs
  | Sub.On_mouse_all f -> runtime.mouse_subs <- (true, f) :: runtime.mouse_subs
  | Sub.On_paste f -> runtime.paste_subs <- (false, f) :: runtime.paste_subs
  | Sub.On_paste_all f -> runtime.paste_subs <- (true, f) :: runtime.paste_subs
  | Sub.On_resize f -> runtime.resize_sub <- Some f
  | Sub.On_focus msg -> runtime.focus_sub <- Some msg
  | Sub.On_blur msg -> runtime.blur_sub <- Some msg
  | Sub.On_color_scheme f -> runtime.color_scheme_sub <- Some f

(* Arm the loop's one-shot wakeup at the earliest every-sub deadline, or clear
   it when no timer is pending. Runs after every subscription recollection and
   after every frame callback — both can move the earliest deadline. *)
let arm_every_wakeup runtime =
  let deadline =
    List.fold_left
      (fun acc (_, due, _) ->
        match acc with None -> Some due | Some acc -> Some (Float.min acc due))
      None runtime.every_subs
  in
  Matrix.schedule_wakeup runtime.matrix_app deadline

let update_subscriptions runtime =
  let prev_every = runtime.every_subs in
  runtime.key_subs <- [];
  runtime.mouse_subs <- [];
  runtime.paste_subs <- [];
  runtime.resize_sub <- None;
  runtime.tick_sub <- None;
  runtime.every_subs <- [];
  runtime.focus_sub <- None;
  runtime.blur_sub <- None;
  runtime.color_scheme_sub <- None;
  collect_subs runtime (runtime.app.subscriptions runtime.model);
  (* Assign each recollected timer its deadline: a timer matching a previous
     entry's interval keeps that entry's deadline — re-evaluating
     subscriptions must not reset time in flight — and a new timer is due one
     interval from now. Each previous entry is consumed at most once and the
     pairing walks declaration order on both sides (collection prepends,
     hence the reverse; the stored list stays in declaration order), so two
     timers with the same interval keep independent phases and a timer added
     later cannot steal an earlier timer's deadline. *)
  let now = Matrix.now runtime.matrix_app in
  let remaining_prev = ref prev_every in
  runtime.every_subs <-
    List.map
      (fun (interval, _, f) ->
        let rec take acc = function
          | [] -> (now +. interval, List.rev acc)
          | (prev_iv, prev_due, _) :: rest when Float.equal prev_iv interval ->
              (prev_due, List.rev_append acc rest)
          | entry :: rest -> take (entry :: acc) rest
        in
        let due, rest = take [] !remaining_prev in
        remaining_prev := rest;
        (interval, due, f))
      (List.rev runtime.every_subs);
  (* Track subscription-driven liveness: only tick subscriptions require the
     render cadence — a tick's [dt] is frame time by definition. Every-subs
     are timers, not animations: they wake the loop at their deadline
     ([arm_every_wakeup]) and the messages they dispatch request their own
     redraws, so a pending timer costs one wakeup, not a full render pipeline
     per frame over the whole mounted tree. *)
  let has_live_subs = runtime.tick_sub <> None in
  if has_live_subs && not runtime.sub_live_active then (
    runtime.sub_live_active <- true;
    Matrix.request_live runtime.matrix_app)
  else if (not has_live_subs) && runtime.sub_live_active then (
    runtime.sub_live_active <- false;
    Matrix.drop_live runtime.matrix_app);
  arm_every_wakeup runtime

let dispatch runtime msg =
  let model', cmd = runtime.app.update msg runtime.model in
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

let handle_key runtime (event : Event.key) =
  let consumed = Event.Key.default_prevented event in
  List.iter
    (fun (all_events, f) ->
      if all_events || not consumed then
        match f event with Some msg -> dispatch runtime msg | None -> ())
    runtime.key_subs

let handle_mouse runtime (event : Event.mouse) =
  let consumed = Event.Mouse.default_prevented event in
  List.iter
    (fun (all_events, f) ->
      if all_events || not consumed then
        match f event with Some msg -> dispatch runtime msg | None -> ())
    runtime.mouse_subs

let handle_paste runtime (event : Event.paste) =
  let consumed = Event.Paste.default_prevented event in
  List.iter
    (fun (all_events, f) ->
      if all_events || not consumed then
        match f event with Some msg -> dispatch runtime msg | None -> ())
    runtime.paste_subs

let handle_resize runtime ~width ~height =
  match runtime.resize_sub with
  | Some f -> dispatch runtime (f ~width ~height)
  | None -> ()

let handle_tick runtime ~dt =
  match runtime.tick_sub with Some f -> push_msg runtime (f ~dt) | None -> ()

(* Timers hold absolute deadlines, so a frame landing exactly on the armed
   wakeup compares the very float [arm_every_wakeup] scheduled — no
   re-accumulated delta, no epsilon. Re-arming at [due +. interval] keeps the
   cadence fixed-rate: a late frame fires once and leaves the next deadline in
   the past, catching up one fire per frame. *)
let handle_every_subs runtime =
  let now = Matrix.now runtime.matrix_app in
  runtime.every_subs <-
    List.map
      (fun (interval, due, f) ->
        if now >= due then begin
          push_msg runtime (f ());
          (interval, due +. interval, f)
        end
        else (interval, due, f))
      runtime.every_subs

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
  | Matrix.Input.Color_scheme scheme -> (
      match runtime.color_scheme_sub with
      | Some f -> (
          match f scheme with Some msg -> dispatch runtime msg | None -> ())
      | None -> ())
  | Matrix.Input.Key key_event ->
      let ev = Renderer.dispatch_key runtime.renderer key_event in
      handle_key runtime ev
  | Matrix.Input.Mouse mouse_event ->
      let ev = Renderer.dispatch_mouse runtime.renderer mouse_event in
      handle_mouse runtime ev
  | Matrix.Input.Paste text ->
      let ev = Renderer.dispatch_paste runtime.renderer text in
      handle_paste runtime ev
  | _ -> ()

let render runtime =
  process_pending_msgs runtime;
  let viewport_width, _ = Matrix.effective_size runtime.matrix_app in
  let viewport_width = max 1 viewport_width in
  let view = runtime.app.view runtime.model in
  let vnode = compile ~dispatch:(push_msg runtime) view in
  Reconciler.render runtime.reconciler ~viewport_width vnode

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
  let renderer =
    Renderer.create ?style:renderer_style ~screen:(Matrix.screen matrix_app) ()
  in
  let container = Renderer.root renderer in
  let reconciler = Reconciler.create ~container in
  let runtime =
    {
      model;
      pending_msgs = Atomic.make [];
      pending_focus = [];
      app;
      matrix_app;
      process_perform;
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
      color_scheme_sub = None;
      sub_live_active = false;
    }
  in
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
  let primary_required_rows (_app : Matrix.app) =
    match Matrix.mode matrix_app with
    | `Primary ->
        Some (max 1 (Renderable.height (Renderer.root runtime.renderer)))
    | `Alt -> None
  in

  Matrix.run runtime.matrix_app ~primary_required_rows
    ~on_frame:(fun _app ~dt ->
      handle_tick runtime ~dt;
      handle_every_subs runtime;
      process_pending_msgs runtime;
      (* A fired timer moved its deadline even when no message re-collected
         subscriptions, so the wakeup re-arms here. *)
      arm_every_wakeup runtime;
      (* Matrix reports [dt] in seconds; the render pipeline runs on
         milliseconds. Convert once at the boundary. *)
      frame_delta := dt *. 1000.)
    ~on_input:(fun _app input -> handle_input runtime input)
    ~on_resize:(fun _app ~cols ~rows ->
      handle_resize runtime ~width:cols ~height:rows)
    ~on_render:(fun _app ->
      render runtime;
      let width, height = Matrix.effective_size runtime.matrix_app in
      Renderer.render_frame runtime.renderer ~width ~height ~delta:!frame_delta;
      (match Matrix.mode runtime.matrix_app with
      | `Alt -> ()
      | `Primary ->
          let required =
            max 1 (Renderable.height (Renderer.root runtime.renderer))
          in
          if required > height then
            Renderer.render_frame runtime.renderer ~layout_height:required
              ~width ~height ~delta:!frame_delta);
      (* A widget can discover geometry while rendering and request a pass that
         includes a newly visible child. Matrix consumed the current redraw
         before invoking us, so explicitly retain that follow-up request. *)
      if Renderer.needs_render runtime.renderer then
        Matrix.request_redraw runtime.matrix_app;
      (* The renderer built directly into the runtime's screen; Matrix.submit
         presents it with a single diff. Forward the frame's hardware scroll
         hint so a scrolled viewport diffs only its revealed rows. *)
      (match Renderer.take_scroll_hint runtime.renderer with
      | Some hint -> Matrix.set_scroll_hint runtime.matrix_app hint
      | None -> ());
      let cursor = Matrix.Screen.cursor (Renderer.screen runtime.renderer) in
      (* Keep the terminal cursor hidden unless the focused renderable exposes
         an explicit cursor position. *)
      Matrix.set_cursor
        ~visible:(cursor.visible && Option.is_some cursor.position)
        runtime.matrix_app;
      Matrix.set_cursor_style runtime.matrix_app ~style:cursor.style
        ~blinking:cursor.blinking;
      (match cursor.position with
      | Some (x, y) ->
          Matrix.set_cursor_position runtime.matrix_app ~row:(y + 1) ~col:(x + 1)
      | None -> ());
      (match cursor.color with
      | Some (r, g, b) ->
          Matrix.set_cursor_color runtime.matrix_app
            ~r:(Float.of_int r /. 255.)
            ~g:(Float.of_int g /. 255.)
            ~b:(Float.of_int b /. 255.)
      | None -> ());
      process_pending_focus runtime)

let empty = Vnode.empty
let fragment = Vnode.fragment
let embed = Vnode.embed
let viewport_switch = Vnode.viewport_switch

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

let box ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?border ?border_style ?border_sides
    ?border_color ?focused_border_color ?background ?fill ?title
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
  Vnode.box ?key ?id ~style ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?border ?border_style
    ?border_sides ?border_color ?focused_border_color ?background ?fill ?title
    ?title_alignment children

let text ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?style ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?text_style ?wrap ?selectable ?selection_bg
    ?selection_fg ?tab_width ?truncate content =
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
  Vnode.text ?key ?id ~style:layout_style ?visible ?z_index ?opacity ?focusable
    ?autofocus ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?text_style
    ?wrap ?selectable ?selection_bg ?selection_fg ?tab_width ?truncate content

let slider ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?orientation ?value ?min ?max ?viewport_size
    ?track_color ?thumb_color ?on_value_change () =
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
  Vnode.slider ?key ?id ~style ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?orientation ?value ?min
    ?max ?viewport_size ?track_color ?thumb_color ?on_value_change ()

let input ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?value ?cursor ?selection ?placeholder
    ?max_length ?text_color ?background_color ?focused_text_color
    ?focused_background_color ?placeholder_color ?selection_color ?selection_fg
    ?cursor_style ?cursor_color ?cursor_blinking ?selectable ?show_cursor
    ?key_bindings ?key_aliases ?on_input ?on_change ?on_submit ?on_cursor () =
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
  Vnode.input ?key ?id ~style ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?value ?cursor ?selection
    ?placeholder ?max_length ?text_color ?background_color ?focused_text_color
    ?focused_background_color ?placeholder_color ?selection_color ?selection_fg
    ?cursor_style ?cursor_color ?cursor_blinking ?selectable ?show_cursor
    ?key_bindings ?key_aliases ?on_input ?on_change ?on_submit ?on_cursor ()

let select ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?selected_index ?background ?text_color
    ?focused_background ?focused_text_color ?selected_background
    ?selected_text_color ?description_color ?selected_description_color
    ?show_description ?show_scroll_indicator ?wrap_selection ?item_spacing
    ?fast_scroll_step ?on_change ?on_activate options =
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
  Vnode.select ?key ?id ~style ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ~options ?selected_index
    ?background ?text_color ?focused_background ?focused_text_color
    ?selected_background ?selected_text_color ?description_color
    ?selected_description_color ?show_description ?show_scroll_indicator
    ?wrap_selection ?item_spacing ?fast_scroll_step ?on_change ?on_activate ()

let tab_select ?key ?id ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?selected ?tab_width
    ?background ?text_color ?focused_background ?focused_text_color
    ?selected_background ?selected_text_color ?description_color
    ?selected_description_color ?show_underline ?show_description
    ?show_scroll_arrows ?wrap_selection ?on_change ?on_activate options =
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
  Vnode.tab_select ?key ?id ~style ?visible ?z_index ?opacity ?focusable
    ?autofocus ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ~options
    ?selected ?tab_width ?background ?text_color ?focused_background
    ?focused_text_color ?selected_background ?selected_text_color
    ?description_color ?selected_description_color ?show_underline
    ?show_description ?show_scroll_arrows ?wrap_selection ?on_change
    ?on_activate ()

let canvas ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?respect_alpha draw =
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
  Vnode.canvas ?key ?id ~style ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?respect_alpha
    ~on_draw:draw ()

let spinner ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?frame_set ?color () =
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
  Vnode.spinner ?key ?id ~style ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?frame_set ?color ()

let progress_bar ?key ?id ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?value ?min ?max
    ?orientation ?filled_color ?empty_color () =
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
  Vnode.progress_bar ?key ?id ~style ?visible ?z_index ?opacity ?focusable
    ?autofocus ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?value ?min ?max
    ?orientation ?filled_color ?empty_color ()

let scroll_bar ?key ?id ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?orientation ?show_arrows
    ?track_color ?thumb_color ?arrow_fg ?arrow_bg ?on_change () =
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
  Vnode.scroll_bar ?key ?id ~style ?visible ?z_index ?opacity ?focusable
    ?autofocus ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?orientation
    ?show_arrows ?track_color ?thumb_color ?arrow_fg ?arrow_bg ?on_change ()

let scroll_box ?key ?id ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?scroll_x ?scroll_y
    ?sticky_scroll ?sticky_start ?background ?show_scrollbars ?reveal ?scroll_by
    ?reset_sticky ?on_scroll ?on_scroll_by_applied ?on_reset_sticky_applied
    children =
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
  Vnode.scroll_box ?key ?id ~style ?visible ?z_index ?opacity ?focusable
    ?autofocus ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?scroll_x
    ?scroll_y ?sticky_scroll ?sticky_start ?background ?show_scrollbars ?reveal
    ?scroll_by ?reset_sticky ?on_scroll ?on_scroll_by_applied
    ?on_reset_sticky_applied children

let textarea ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?value ?cursor ?selection ?spans ?ghost_text
    ?ghost_text_color ?placeholder ?wrap ?text_color ?background_color
    ?focused_text_color ?focused_background_color ?placeholder_color
    ?selection_color ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking
    ?selectable ?show_cursor ?key_bindings ?key_aliases ?on_input ?on_change
    ?on_submit ?on_cursor () =
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
  Vnode.textarea ?key ?id ~style ?visible ?z_index ?opacity ?focusable
    ?autofocus ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?value ?cursor
    ?selection ?spans ?ghost_text ?ghost_text_color ?placeholder ?wrap
    ?text_color ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?on_input ?on_change
    ?key_bindings ?key_aliases ?on_submit ?on_cursor ()

let code ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?syntax ?text_style ?wrap ?tab_width ?selectable
    ?selection_bg ?selection_fg ?on_selection content =
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
  Vnode.code ?key ?id ~style ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?syntax ?text_style ?wrap
    ?tab_width ?selectable ?selection_bg ?selection_fg ?on_selection content

let line_number ?key ?id ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?fg ?bg ?min_width
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
  Vnode.line_number ?key ?id ~style ?visible ?z_index ?opacity ?focusable
    ?autofocus ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?fg ?bg
    ?min_width ?padding_right ?show_line_numbers ?line_number_offset
    ?line_colors ?line_signs ?hidden_line_numbers child

let diff ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?layout ?theme ?highlight ?line_highlights
    ?line_spans ?line_signs ?show_line_numbers ?wrap ?selectable ?text_style
    ?on_line_click patch =
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
  Vnode.diff ?key ?id ~style ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?layout ?theme ?highlight
    ?line_highlights ?line_spans ?line_signs ?show_line_numbers ?wrap
    ?selectable ?text_style ?on_line_click patch

let markdown ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?md_style ?conceal ?streaming ?selectable
    ?selection_bg ?selection_fg ?code_syntax ?on_selection content =
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
  Vnode.markdown ?key ?id ~style ?visible ?z_index ?opacity ?focusable
    ?autofocus ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?md_style
    ?conceal ?streaming ?selectable ?selection_bg ?selection_fg ?code_syntax
    ?on_selection content

let table ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?columns ?rows ?selected_row ?border
    ?border_style ?show_header ?show_column_separator ?show_row_separator
    ?cell_padding ?header_color ?header_background ?text_color ?background
    ?selected_text_color ?selected_background ?focused_selected_text_color
    ?focused_selected_background ?selection_visible ?show_scroll_indicator
    ?activate_on_click ?wheel_navigation ?row_styles ?wrap_selection
    ?fast_scroll_step ?on_change ?on_activate ?on_hover () =
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
  Vnode.table ?key ?id ~style ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?columns ?rows
    ?selected_row ?border ?border_style ?show_header ?show_column_separator
    ?show_row_separator ?cell_padding ?header_color ?header_background
    ?text_color ?background ?selected_text_color ?selected_background
    ?focused_selected_text_color ?focused_selected_background ?selection_visible
    ?show_scroll_indicator ?activate_on_click ?wheel_navigation ?row_styles
    ?wrap_selection ?fast_scroll_step ?on_change ?on_activate ?on_hover ()

let tree ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?items ?selected_index ?expand_depth
    ?indent_size ?show_guides ?guide_style ?expand_icon ?collapse_icon
    ?leaf_icon ?background ?text_color ?selected_background ?selected_text_color
    ?focused_selected_background ?focused_selected_text_color ?guide_color
    ?icon_color ?wrap_selection ?fast_scroll_step ?on_change ?on_activate
    ?on_expand () =
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
  Vnode.tree ?key ?id ~style ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?items ?selected_index
    ?expand_depth ?indent_size ?show_guides ?guide_style ?expand_icon
    ?collapse_icon ?leaf_icon ?background ?text_color ?selected_background
    ?selected_text_color ?focused_selected_background
    ?focused_selected_text_color ?guide_color ?icon_color ?wrap_selection
    ?fast_scroll_step ?on_change ?on_activate ?on_expand ()

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
