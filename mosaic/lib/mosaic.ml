module Ui = Ui
module Style = Ui.Style
module Cmd = Engine.Cmd
module Sub = Engine.Sub
module Input = Input
module Deps = Deps
module Tile = Tile
module Reducer_id = Hook.Reducer_id

let src = Logs.Src.create "mosaic" ~doc:"Mosaic framework events"

module Log = (val Logs.src_log src : Logs.LOG)

type 'a context = 'a Context.t

let create_context = Context.create
let provide = Context.provide
let use_state = Hook.use_state
let use_effect = Hook.use_effect
let use_context = Hook.use_context
let use_subscription = Hook.use_subscription
let dispatch_cmd = Hook.dispatch_cmd
let use_reducer = Hook.use_reducer
let use_reducer_latest = Hook.use_reducer_latest
let use_memo = Hook.use_memo
let use_ref = Hook.use_ref
let use_callback = Hook.use_callback

(* Generate a stable key for UI elements *)
let use_key ~prefix =
  let key_ref = use_ref None in
  match !key_ref with
  | Some key -> key
  | None ->
      let key = Ui.Key.create ~prefix in
      key_ref := Some key;
      key

(* Subscribe to keyboard shortcuts *)
let use_keyboard = Hook.use_key

(* Subscribe to animation ticks *)
let use_tick = Hook.use_tick

(* Subscribe to repeating timers *)
let use_timer = Hook.use_timer

(* Subscribe to scrolling with optional momentum *)
let use_scroll = Hook.use_scroll

(* Event handlers - these return subscriptions *)
let on_click key callback =
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      match Runtime_context.current () with
      | None -> None
      | Some ctx ->
          let handler = Engine.Input_router.Click (key, callback) in
          let id = Engine.Input_router.subscribe ctx.input_router handler in
          Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id));
  Engine.Sub.none

let on_hover key callback =
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      match Runtime_context.current () with
      | None -> None
      | Some ctx ->
          let handler = Engine.Input_router.Hover (key, callback) in
          let id = Engine.Input_router.subscribe ctx.input_router handler in
          Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id));
  Engine.Sub.none

let on_drag key callback =
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      match Runtime_context.current () with
      | None -> None
      | Some ctx ->
          let handler = Engine.Input_router.Drag (key, callback) in
          let id = Engine.Input_router.subscribe ctx.input_router handler in
          Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id));
  Engine.Sub.none

let on_focus key callback =
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      match Runtime_context.current () with
      | None -> None
      | Some ctx ->
          let handler =
            Engine.Input_router.Focus
              ( key,
                fun has_focus ->
                  callback has_focus;
                  () )
          in
          let id = Engine.Input_router.subscribe ctx.input_router handler in
          Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id));
  Engine.Sub.none

let on_key key callback =
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      match Runtime_context.current () with
      | None -> None
      | Some ctx ->
          let handler =
            Engine.Input_router.KeyPress
              ( key,
                fun evt ->
                  callback evt;
                  () )
          in
          let id = Engine.Input_router.subscribe ctx.input_router handler in
          Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id));
  Engine.Sub.none

let on_scroll key callback =
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      match Runtime_context.current () with
      | None -> None
      | Some ctx ->
          let handler = Engine.Input_router.Scroll (key, callback) in
          let id = Engine.Input_router.subscribe ctx.input_router handler in
          Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id));
  Engine.Sub.none

let run_eio ~sw ~env ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 30)
    ?debug root_fn =
  let root_fiber = Fiber.create_root root_fn in
  let debug_log =
    match debug with
    | Some path -> Some (open_out_gen [ Open_wronly; Open_creat ] 0o666 path)
    | None -> None
  in
  let cfg =
    Engine.Program.config ?terminal ~alt_screen ~mouse ~fps ?debug_log ()
  in

  (* Create runtime context for input routing and focus management *)
  let runtime_ctx = Runtime_context.create () in
  Runtime_context.set_current (Some runtime_ctx);

  (* Keep these in refs so our callbacks can use them *)
  let prog_ref : Engine.Program.t option ref = ref None in
  let process_cmd_unit_ref : ((unit -> unit) -> unit Cmd.t -> unit) option ref =
    ref None
  in

  let flush_pending () =
    match !process_cmd_unit_ref with
    | None -> ()
    | Some process_cmd ->
        Fiber.collect_pending_commands root_fiber
        |> List.iter (fun (Fiber.Erased_cmd cmd) ->
               process_cmd (fun () -> ()) cmd)
  in

  let render () =
    (* Provide the runtime context to hook consumers during render *)
    let ui =
      Context.provide Runtime_context.context runtime_ctx (fun () ->
          Fiber.reconcile root_fiber)
    in
    (* Effects during render may enqueue commands like Quit/Tick/etc. *)
    flush_pending ();
    ui
  in

  let on_input event =
    (* First, route the event through our input router *)
    (match event with
    | Input.Mouse mouse_evt ->
        (* Extract position and convert to our mouse event type *)
        (* Convert from 1-based terminal coordinates to 0-based *)
        let dec n = if n > 0 then n - 1 else 0 in
        let x, y, evt =
          match mouse_evt with
          | Input.Button_press (x, y, button, _) ->
              (dec x, dec y, Engine.Input_router.(Button_down button))
          | Input.Button_release (x, y, button, _) ->
              (dec x, dec y, Engine.Input_router.(Button_up button))
          | Input.Motion (x, y, _, _) ->
              (dec x, dec y, Engine.Input_router.Motion)
        in
        Engine.Input_router.on_mouse runtime_ctx.input_router ~x ~y evt
    | Input.Key evt -> (
        (* Handle tab navigation specially *)
        match evt.key with
        | Input.Tab ->
            Engine.Focus_manager.handle_tab runtime_ctx.focus_manager
              ~forward:(not evt.modifier.shift)
        | _ -> Engine.Input_router.on_keyboard runtime_ctx.input_router evt)
    | _ -> ());

    (* ------------- always run after any event ------------- *)

    (* Run original subscriptions *)
    let event' = Engine.Event.Input event in
    Fiber.collect_subscriptions root_fiber
    |> List.iter (fun (Fiber.Erased_sub sub) ->
           Fiber.with_handler root_fiber (fun () ->
               Sub.run ~dispatch:(fun _ -> ()) event' sub));

    (* Execute any commands enqueued by subs immediately (e.g., Cmd.quit) *)
    flush_pending ();

    (* If we dirtied state, ask for a repaint *)
    if Fiber.is_dirty root_fiber || Fiber.has_pending_commands root_fiber then (
      Log.debug (fun m ->
          m "Requesting render after input event (dirty: %b, pending_cmds: %b)"
            (Fiber.is_dirty root_fiber)
            (Fiber.has_pending_commands root_fiber));
      Option.iter Engine.Program.request_render !prog_ref)
  in

  let on_resize ~w ~h =
    let ev = Engine.Event.Input (Input.Resize (w, h)) in
    Fiber.collect_subscriptions root_fiber
    |> List.iter (fun (Fiber.Erased_sub sub) ->
           Fiber.with_handler root_fiber (fun () ->
               Sub.run ~dispatch:(fun _ -> ()) ev sub));
    flush_pending ();
    if Fiber.is_dirty root_fiber then
      Option.iter Engine.Program.force_full_redraw !prog_ref
  in

  let tick ~elapsed =
    let event = Engine.Event.Tick elapsed in
    Fiber.collect_subscriptions root_fiber
    |> List.iter (fun (Fiber.Erased_sub sub) ->
           Fiber.with_handler root_fiber (fun () ->
               Sub.run ~dispatch:(fun _ -> ()) event sub));
    flush_pending ()
    (* Note: No need to explicitly request render here as mark_dirty 
       will call request_render_ref when reaching root *)
  in

  let on_snapshot =
    if mouse then
      Some
        (fun snapshot -> Runtime_context.update_snapshot runtime_ctx snapshot)
    else None
  in

  let prog, process_cmd =
    Engine.Program.start ~sw ~env cfg ~render ~on_input ~on_resize ?on_snapshot
      ~tick ()
  in
  prog_ref := Some prog;
  process_cmd_unit_ref := Some process_cmd;

  (* Set up the render request callback for Fiber.mark_dirty *)
  (Fiber.request_render_ref :=
     fun () -> Option.iter Engine.Program.request_render !prog_ref);
  ()

(** Run the application - main entry point *)
let run ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 30) ?debug
    root_fn =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  run_eio ~sw ~env ?terminal ~alt_screen ~mouse ~fps ?debug root_fn
