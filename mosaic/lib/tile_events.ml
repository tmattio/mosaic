(** Event subscription implementation *)

(* Event types *)
type key_event = Input.key_event
type drag_phase = [ `Start | `Move | `End ]

type drag = {
  phase : drag_phase;
  x : int;
  y : int;
  dx : int;
  dy : int;
  start_x : int;
  start_y : int;
}

(* Convert internal drag event to our public drag type *)
let convert_drag_event (evt : Engine.Input_router.drag_event) : drag =
  {
    phase = evt.phase;
    x = evt.x;
    y = evt.y;
    dx = evt.dx;
    dy = evt.dy;
    start_x = evt.start_x;
    start_y = evt.start_y;
  }

(* Get the runtime context from the global context or React context *)
let get_runtime_context () =
  match Runtime_context.current () with
  | Some ctx -> ctx
  | None -> Hook.use_context Runtime_context.context

let use_click key on_click =
  let callback_ref = Hook.use_ref on_click in
  Hook.use_effect ~deps:Deps.always (fun () ->
      callback_ref := on_click;
      None);
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      let ctx = get_runtime_context () in
      let handler =
        Engine.Input_router.Click (key, fun () -> !callback_ref ())
      in
      let id = Engine.Input_router.subscribe ctx.input_router handler in
      Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id))

let use_hover key =
  let hovered, set_hovered, _ = Hook.use_state false in

  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      let ctx = get_runtime_context () in
      let handler =
        Engine.Input_router.Hover (key, fun entering -> set_hovered entering)
      in
      let id = Engine.Input_router.subscribe ctx.input_router handler in
      Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id));

  hovered

let use_drag key on_drag =
  let callback_ref = Hook.use_ref on_drag in
  Hook.use_effect ~deps:Deps.always (fun () ->
      callback_ref := on_drag;
      None);
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      let ctx = get_runtime_context () in
      let handler =
        Engine.Input_router.Drag
          ( key,
            fun evt ->
              !callback_ref (convert_drag_event evt);
              ()
            (* Return Some msg - will be fixed *) )
      in
      let id = Engine.Input_router.subscribe ctx.input_router handler in
      Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id))

let use_focus key =
  let focused, set_focused, _ = Hook.use_state false in

  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      let ctx = get_runtime_context () in
      let handler =
        Engine.Input_router.Focus
          ( key,
            fun has_focus ->
              set_focused has_focus;
              ()
            (* Return Some msg - will be fixed *) )
      in
      let id = Engine.Input_router.subscribe ctx.input_router handler in
      Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id));

  focused

let use_key_press key on_key =
  let callback_ref = Hook.use_ref on_key in
  Hook.use_effect ~deps:Deps.always (fun () ->
      callback_ref := on_key;
      None);
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      let ctx = get_runtime_context () in
      let handler =
        Engine.Input_router.KeyPress
          ( key,
            fun evt ->
              !callback_ref evt;
              ()
            (* Return Some msg - will be fixed *) )
      in
      let id = Engine.Input_router.subscribe ctx.input_router handler in
      Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id))

let use_bounds key =
  let ctx = get_runtime_context () in
  match ctx.snapshot with
  | Some snapshot -> (
      match Ui.Layout_snapshot.get snapshot key with
      | Some entry -> Some entry.rect
      | None -> None)
  | None -> None

let use_focusable key ?tab_index ?(auto_focus = false) () =
  let tab_index = Option.map (fun x -> x) tab_index in
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      let ctx = get_runtime_context () in
      let focusable =
        Engine.Focus_manager.
          { key; tab_index; auto_focus; order = 0 (* ignored by register *) }
      in
      Engine.Focus_manager.register ctx.focus_manager focusable;
      Some (fun () -> Engine.Focus_manager.unregister ctx.focus_manager key))

let use_keyboard ?ctrl ?alt ?shift key cmd =
  Hook.use_keyboard ?ctrl ?alt ?shift key cmd

(* Direct event subscription functions that return unit (for composability) *)
let on_click = use_click

let on_hover key callback =
  let callback_ref = Hook.use_ref callback in
  Hook.use_effect ~deps:Deps.always (fun () ->
      callback_ref := callback;
      None);
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      match Runtime_context.current () with
      | None -> None
      | Some ctx ->
          let handler = Engine.Input_router.Hover (key, fun entering -> !callback_ref entering) in
          let id = Engine.Input_router.subscribe ctx.input_router handler in
          Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id))

let on_focus key callback =
  let callback_ref = Hook.use_ref callback in
  Hook.use_effect ~deps:Deps.always (fun () ->
      callback_ref := callback;
      None);
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
                  !callback_ref has_focus;
                  () )
          in
          let id = Engine.Input_router.subscribe ctx.input_router handler in
          Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id))

let on_drag key callback =
  let callback_ref = Hook.use_ref callback in
  Hook.use_effect ~deps:Deps.always (fun () ->
      callback_ref := callback;
      None);
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      match Runtime_context.current () with
      | None -> None
      | Some ctx ->
          let handler =
            Engine.Input_router.Drag
              ( key,
                fun evt ->
                  !callback_ref (convert_drag_event evt);
                  () )
          in
          let id = Engine.Input_router.subscribe ctx.input_router handler in
          Some (fun () -> Engine.Input_router.unsubscribe ctx.input_router id))

let on_key = use_key_press
let focusable = use_focusable
