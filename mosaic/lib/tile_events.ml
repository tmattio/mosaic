(** Event subscription implementation *)

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
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      let ctx = get_runtime_context () in
      let handler =
        Engine.Input_router.Drag
          ( key,
            fun evt ->
              on_drag evt;
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
  Hook.use_effect
    ~deps:(Deps.keys [ Deps.ui_key key ])
    (fun () ->
      let ctx = get_runtime_context () in
      let handler =
        Engine.Input_router.KeyPress
          ( key,
            fun evt ->
              on_key evt;
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
