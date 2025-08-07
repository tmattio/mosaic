(** Mosaic React - A React-style declarative UI library for terminal applications *)

(** Re-export Mosaic UI and Engine modules *)
module Ui = Ui
module Cmd = Engine.Cmd
module Sub = Engine.Sub
module Input = Input

(** Context API *)
type 'a context = 'a Context.t

let create_context = Context.create
let provide = Context.provide

(** Hooks *)
let use_state = Hook.use_state
let use_effect = Hook.use_effect
let use_context = Hook.use_context
let use_subscription = Hook.use_subscription
let dispatch_cmd = Hook.dispatch_cmd

(** Component Creation *)
module type Component = sig
  type props
  val render : props -> Ui.element
end

let component (type p) (module C : Component with type props = p) props =
  C.render props

(** Run with Eio environment - allows more control over the runtime *)
let run_eio ~sw ~env ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    ?debug root_fn =
  (* Build root fiber *)
  let root_fiber = Fiber.create_root root_fn in

  let cfg =
    Engine.Program.config ?terminal ~alt_screen ~mouse ~fps ?debug_log:debug ()
  in

  (* Create a message queue for commands *)
  let cmd_queue = Queue.create () in
  
  (* We'll store the program reference here once created *)
  let prog_ref = ref None in
  
  (* Render callback *)
  let render () = 
    let ui = Fiber.reconcile root_fiber in
    (* Check for pending commands and queue them *)
    if Fiber.has_pending_commands root_fiber then begin
      let cmds = Fiber.collect_pending_commands root_fiber in
      Queue.add cmds cmd_queue
    end;
    ui
  in

  (* Input event handler *)
  let on_input event =
    (* Collect current subscriptions from the fiber tree *)
    Fiber.collect_subscriptions root_fiber
    |> List.iter (fun (Fiber.ErasedSub sub) ->
           (* We ignore the message, React style *)
           Sub.run ~dispatch:(fun _ -> ()) event sub);
    
    (* Request a re-render if the fiber tree is dirty or has pending commands *)
    if Fiber.is_dirty root_fiber || Fiber.has_pending_commands root_fiber then
      Option.iter Engine.Program.request_render !prog_ref
  in
  
  (* Resize event handler *)
  let on_resize ~w ~h =
    let resize_event = Input.Resize (w, h) in
    (* Collect current subscriptions and process resize event *)
    Fiber.collect_subscriptions root_fiber
    |> List.iter (fun (Fiber.ErasedSub sub) ->
           Sub.run ~dispatch:(fun _ -> ()) resize_event sub)
  in

  let prog, process_cmd =
    Engine.Program.start ~sw ~env cfg ~render ~on_input ~on_resize ()
  in
  prog_ref := Some prog;
  
  (* Process queued commands *)
  Eio.Fiber.fork ~sw (fun () ->
    while true do
      if not (Queue.is_empty cmd_queue) then begin
        let cmd = Queue.take cmd_queue in
        process_cmd (fun () -> ()) cmd
      end;
      Eio.Fiber.yield ()
    done
  );
  
  ()

(** Run the application - main entry point *)
let run ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60) ?debug
    root_fn =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  run_eio ~sw ~env ?terminal ~alt_screen ~mouse ~fps ?debug root_fn