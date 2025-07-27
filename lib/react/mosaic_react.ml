(* mosaic.ml - React-based API for Mosaic TUI framework *)

open Engine

(* Type identity module for context identification *)
module Type = struct
  module Id = struct
    type 'a t = int

    let counter = ref 0

    let make () =
      incr counter;
      !counter

    let equal : type a b. a t -> b t -> bool = fun a b -> a = b
  end
end

(* Public modules *)
module Style = Ui.Style
module Ui = Ui
module Cmd = Cmd
module Sub = Sub
module Input = Input
module Component = Component

type msg = Msg : 'a * ('a -> unit) -> msg

(* Effects for hooks *)
type _ Effect.t +=
  | Use_state : 'a -> ('a * ('a -> unit)) Effect.t
  | Use_effect :
      (unit -> (unit -> unit) option) * 'a array option
      -> unit Effect.t
  | Use_context : 'ctx Type.Id.t -> 'ctx Effect.t
  | Dispatch_cmd : msg Cmd.t -> unit Effect.t
  | Use_subscription : msg Sub.t -> unit Effect.t

(* Helper types *)
type 'a deps = 'a array option

(* Context abstraction *)
type context_value =
  | Context_value : { id : 'a Type.Id.t; value : 'a } -> context_value

(* Hook abstraction - we need to keep the raw values *)
type hook_state =
  | State : { mutable value : 'a; setter : 'a -> unit } -> hook_state
  | Effect :
      (unit -> (unit -> unit) option) * 'b deps * (unit -> unit) option
      -> hook_state
  | Subscription : msg Sub.t -> hook_state

type fiber = {
  mutable id : int;
  mutable parent : fiber option;
  mutable child_slots : fiber list;
  mutable component : Component.any_component option;
  mutable hooks : hook_state list;
  mutable hook_index : int;
  mutable ui : Ui.element option;
  mutable dirty : bool;
  mutable contexts : context_value list;
  mutable pending_cmds : msg Cmd.t;
  mutable root_program : Program.t option;
  mutable pending_updates : bool;
}

type model = fiber

let next_id = ref 0
let current_fiber : fiber option ref = ref None

let rec mark_dirty f =
  f.dirty <- true;
  Option.iter mark_dirty f.parent

let schedule_update f =
  mark_dirty f;
  f.pending_updates <- true;
  match f.root_program with
  | Some p -> Program.set_pending_updates p true
  | None -> ()

let deps_equal (type a b) (a : a deps) (b : b deps) : bool =
  match (a, b) with
  | None, None -> true
  | Some a_arr, Some b_arr when Array.length a_arr = Array.length b_arr -> (
      (* We can only compare deps if they have the same length.
         Since deps are used to determine if an effect should re-run,
         we use physical equality (==) which works across types. *)
      try
        let equal = ref true in
        for i = 0 to Array.length a_arr - 1 do
          if
            not
              (Obj.magic (Array.unsafe_get a_arr i)
              == Obj.magic (Array.unsafe_get b_arr i))
          then equal := false
        done;
        !equal
      with _ -> false)
  | _ -> false

let rec destroy_fiber f =
  List.iter
    (function Effect (_, _, Some cleanup) -> cleanup () | _ -> ())
    f.hooks;
  List.iter destroy_fiber f.child_slots

let fiber_handler (fiber : fiber) (type b) (eff : b Effect.t)
    (k : (b, _) Effect.Deep.continuation) =
  match eff with
  | Use_state initial -> (
      let idx = fiber.hook_index in
      fiber.hook_index <- idx + 1;
      match List.nth_opt fiber.hooks idx with
      | Some (State { value; setter }) ->
          (* SAFETY: We rely on hooks being called in the same order with same types.
             The effect system ensures type safety at the API boundary. *)
          let v = Obj.magic value in
          let s = Obj.magic setter in
          Effect.Deep.continue k (v, s)
      | None ->
          let value = initial in
          let setter v =
            (* Find and update this specific hook *)
            let rec update_hook i = function
              | [] -> failwith "Hook not found"
              | State h :: rest when i = idx ->
                  h.value <- Obj.magic v;
                  schedule_update fiber;
                  State h :: rest
              | h :: rest -> h :: update_hook (i + 1) rest
            in
            fiber.hooks <- update_hook 0 fiber.hooks
          in
          fiber.hooks <-
            fiber.hooks
            @ [ State { value = Obj.magic value; setter = Obj.magic setter } ];
          Effect.Deep.continue k (value, setter)
      | _ -> failwith "Hook mismatch: expected State")
  | Use_effect (setup, deps) ->
      let idx = fiber.hook_index in
      fiber.hook_index <- idx + 1;
      (match List.nth_opt fiber.hooks idx with
      | Some (Effect (_, old_deps, cleanup)) ->
          if not (deps_equal old_deps deps) then (
            Option.iter (fun c -> c ()) cleanup;
            let new_cleanup = setup () in
            fiber.hooks <-
              List.mapi
                (fun i h ->
                  if i = idx then Effect (setup, deps, new_cleanup) else h)
                fiber.hooks)
      | None ->
          let new_cleanup = setup () in
          fiber.hooks <- fiber.hooks @ [ Effect (setup, deps, new_cleanup) ]
      | _ -> failwith "Hook mismatch: expected Effect");
      Effect.Deep.continue k ()
  | Use_context id ->
      let rec find f =
        match f.contexts with
        | [] -> (
            match f.parent with
            | Some p -> find p
            | None -> failwith "Context not found")
        | Context_value h :: rest ->
            if Type.Id.equal h.id id then
              (* We know the types match because of the id equality *)
              Effect.Deep.continue k (Obj.magic h.value)
            else find { f with contexts = rest }
      in
      find fiber
  | Dispatch_cmd cmd ->
      fiber.pending_cmds <- Cmd.batch [ fiber.pending_cmds; cmd ];
      Effect.Deep.continue k ()
  | Use_subscription sub ->
      let idx = fiber.hook_index in
      fiber.hook_index <- idx + 1;
      (match List.nth_opt fiber.hooks idx with
      | Some (Subscription _) ->
          fiber.hooks <-
            List.mapi
              (fun i h -> if i = idx then Subscription sub else h)
              fiber.hooks
      | None -> fiber.hooks <- fiber.hooks @ [ Subscription sub ]
      | _ -> failwith "Hook mismatch: expected Subscription");
      Effect.Deep.continue k ()
  | _ ->
      (* Re-perform the effect to let it be handled by an outer handler *)
      let result = Effect.perform eff in
      Effect.Deep.continue k result

let create_fiber ?parent ?root_program component =
  incr next_id;
  {
    id = !next_id;
    parent;
    child_slots = [];
    component;
    hooks = [];
    hook_index = 0;
    ui = None;
    dirty = true;
    contexts = [];
    pending_cmds = Cmd.none;
    root_program;
    pending_updates = false;
  }

let rec reconcile (fiber : fiber) : unit =
  if not fiber.dirty then ()
  else (
    fiber.dirty <- false;
    let old_child_slots = fiber.child_slots in
    fiber.child_slots <- [];
    fiber.hook_index <- 0;
    let new_ui =
      match fiber.component with
      | None -> failwith "No component to reconcile"
      | Some (Component.Any ((module C), props)) ->
          let old_current = !current_fiber in
          current_fiber := Some fiber;
          let result = C.make props in
          current_fiber := old_current;
          result
    in
    fiber.ui <- Some new_ui;
    let new_len = List.length fiber.child_slots in
    (if new_len < List.length old_child_slots then
       let rec drop n lst =
         match (n, lst) with
         | 0, _ -> lst
         | _, [] -> []
         | n, _ :: t -> drop (n - 1) t
       in
       let excess = drop new_len old_child_slots in
       List.iter destroy_fiber excess);
    List.iter reconcile fiber.child_slots)

let render_subcomponent (type p) (c : p Component.t) (props : p) : Ui.element =
  match !current_fiber with
  | None -> failwith "render_subcomponent outside fiber"
  | Some parent ->
      let idx = List.length parent.child_slots in
      let child =
        match List.nth_opt parent.child_slots idx with
        | Some old ->
            old.component <- Some (Component.Any (c, props));
            old.dirty <- true;
            old
        | None ->
            create_fiber ~parent ?root_program:parent.root_program
              (Some (Component.Any (c, props)))
      in
      parent.child_slots <- parent.child_slots @ [ child ];
      let old_current = !current_fiber in
      current_fiber := Some child;
      if child.dirty then reconcile child;
      let ui = Option.get child.ui in
      current_fiber := old_current;
      ui

(* Context API *)
type 'ctx t = { id : 'ctx Type.Id.t; default : 'ctx option }

let create ?default () = { id = Type.Id.make (); default }

let provide { id; default = _ } value children =
  match !current_fiber with
  | None -> failwith "provide outside fiber"
  | Some f ->
      let old_contexts = f.contexts in
      f.contexts <- Context_value { id; value } :: f.contexts;
      let ui = children () in
      f.contexts <- old_contexts;
      ui

(* Hooks API *)
let use_state initial = Effect.perform (Use_state initial)
let use_effect ?deps f = Effect.perform (Use_effect (f, deps))
let use_context ctx = Effect.perform (Use_context ctx.id)
let dispatch_cmd cmd = Effect.perform (Dispatch_cmd cmd)
let use_subscription sub = Effect.perform (Use_subscription sub)

(* Collect functions *)
let rec collect_pending (f : fiber) : msg Cmd.t =
  let c = f.pending_cmds in
  f.pending_cmds <- Cmd.none;
  Cmd.batch (c :: List.map collect_pending f.child_slots)

let rec collect_subs (f : fiber) : msg Sub.t =
  let local =
    List.fold_left
      (fun acc -> function Subscription s -> Sub.batch [ acc; s ] | _ -> acc)
      Sub.none f.hooks
  in
  Sub.batch (local :: List.map collect_subs f.child_slots)

let run_eio ~sw ~env ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    ?(debug = false) (root : unit -> Ui.element) =
  let debug_log = if debug then Some (open_out "mosaic-debug.log") else None in
  let config = { Program.terminal; alt_screen; mouse; fps; debug_log } in

  (* Create the program handle *)
  let p = Program.create ~sw ~env config in

  (* Create root fiber *)
  let root_fiber = create_fiber ?root_program:(Some p) None in
  Program.set_model p root_fiber;

  (* Command stream for processing commands from hooks *)
  let cmd_stream = Eio.Stream.create 100 in

  (* The dispatch function for our React app *)
  let dispatch (Msg (v, handler)) =
    handler v;
    (* Imperatively update component state *)
    root_fiber.pending_updates <- true;
    Program.set_pending_updates p true
  in

  (* Handle input events *)
  let handle_input_event event =
    let msgs =
      Program.with_state_mutex p ~protect:true (fun () ->
          (* Collect subs from the fiber tree *)
          let subs = collect_subs root_fiber in
          match event with
          | Input.Key key_event ->
              let keyboard_handlers = Sub.collect_keyboard [] subs in
              List.filter_map (fun f -> f key_event) keyboard_handlers
          | Input.Focus ->
              let focus_handlers = Sub.collect_focus [] subs in
              List.filter_map (fun f -> f ()) focus_handlers
          | Input.Blur ->
              let blur_handlers = Sub.collect_blur [] subs in
              List.filter_map (fun f -> f ()) blur_handlers
          | Input.Mouse mouse_event ->
              let mouse_handlers = Sub.collect_mouse [] subs in
              List.filter_map (fun f -> f mouse_event) mouse_handlers
          | Input.Paste s ->
              let paste_handlers = Sub.collect_paste [] subs in
              List.filter_map (fun f -> f s) paste_handlers
          | Input.Resize (w, h) ->
              Program.invalidate_buffer p;
              let window_handlers = Sub.collect_window [] subs in
              let size = { Sub.width = w; Sub.height = h } in
              List.filter_map (fun f -> f size) window_handlers
          | _ -> [])
    in
    List.iter dispatch msgs
  in

  (* Handle resize events *)
  let handle_resize (w, h) = handle_input_event (Input.Resize (w, h)) in

  (* Process commands from the stream *)
  let message_loop () =
    while Program.is_running p do
      (* Process commands generated by hooks *)
      let cmd = Eio.Stream.take cmd_stream in
      Program.process_cmd p dispatch cmd
    done
  in

  (* Render loop with reconciliation *)
  let get_element () =
    (* The core reconciliation logic *)
    if Program.get_pending_updates p then
      Program.with_state_mutex p ~protect:false (fun () ->
          reconcile root_fiber;
          root_fiber.pending_updates <- false;
          Program.set_pending_updates p false;
          (* Collect new commands and add them to the stream *)
          let new_cmds = collect_pending root_fiber in
          if new_cmds <> Cmd.none then Eio.Stream.add cmd_stream new_cmds);
    Option.get root_fiber.ui
  in

  Fun.protect
    ~finally:(fun () ->
      Option.iter close_out debug_log;
      Program.cleanup p)
    (fun () ->
      (* Set up the effect handler for the entire program *)
      let run_with_effects () =
        (* Initial render of the root component *)
        let old_current = !current_fiber in
        current_fiber := Some root_fiber;
        let ui = root () in
        current_fiber := old_current;
        root_fiber.ui <- Some ui;

        (* Collect initial commands *)
        let initial_cmds = collect_pending root_fiber in
        if initial_cmds <> Cmd.none then Eio.Stream.add cmd_stream initial_cmds;

        Program.setup_terminal p;
        Program.setup_signal_handlers p;

        Eio.Fiber.all
          [
            (fun () -> Program.run_input_loop p handle_input_event);
            (fun () -> Program.run_render_loop p get_element);
            message_loop;
            (fun () -> Program.run_resize_loop p handle_resize);
          ]
      in

      Effect.Deep.try_with run_with_effects ()
        {
          Effect.Deep.effc =
            (fun (type b) (eff : b Effect.t) ->
              match eff with
              | Use_state _ | Use_effect _ | Use_context _ | Dispatch_cmd _
              | Use_subscription _ ->
                  (* Use the current fiber for handling *)
                  Option.map
                    (fun fiber -> fun k -> fiber_handler fiber eff k)
                    !current_fiber
              | _ -> None);
        })

let run ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    ?(debug = false) root =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  run_eio ~sw ~env ?terminal ~alt_screen ~mouse ~fps ~debug root

let component = Component.make
