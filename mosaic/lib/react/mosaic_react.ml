open Engine
open Effect.Deep
module Style = Ui.Style
module Ui = Ui
module Cmd = Cmd
module Sub = Sub
module Input = Input
module Component = Component

(*  Type‑identity helper (for Contexts) *)

module Type = struct
  module Id = struct
    type 'a t = int

    let counter = ref 0

    let make () =
      incr counter;
      !counter

    let equal : type a b. a t -> b t -> bool = ( = )
  end
end

(*  Internal message wrapper – allows heterogenous payloads *)

type msg = Msg : 'a * ('a -> unit) -> msg

(*  Effects for hooks *)

type _ Effect.t +=
  | Use_state : 'a -> ('a * ('a -> unit)) Effect.t
  | Use_effect :
      (unit -> (unit -> unit) option) * 'a array option
      -> unit Effect.t
  | Use_context : 'ctx Type.Id.t -> 'ctx Effect.t
  | Dispatch_cmd : msg Cmd.t -> unit Effect.t
  | Use_subscription : msg Sub.t -> unit Effect.t

(* Helper alia *)
type 'a deps = 'a array option

(*  Context & Hook runtime data structures *)

type context_value =
  | Context_value : { id : 'a Type.Id.t; value : 'a } -> context_value

(* Each hook instance lives in [hooks] list, order is significan *)
type hook_state =
  | State of { mutable value : Obj.t; setter : Obj.t -> unit }
  | Effect of
      (unit -> (unit -> unit) option) * Obj.t deps * (unit -> unit) option
  | Subscription of msg Sub.t

(* A React‑style fibre (component instance *)
and fiber = {
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

type model = fiber (* exposed for debug / Program.set_mode *)

(*  Global mutable references (per process) *)

let next_id = ref 0
let current_fiber : fiber option ref = ref None

let deps_equal (type a b) (x : a deps) (y : b deps) : bool =
  match (x, y) with
  | None, None -> true
  | Some xa, Some ya when Array.length xa = Array.length ya ->
      let equal = ref true in
      for i = 0 to Array.length xa - 1 do
        if
          Obj.magic (Array.unsafe_get xa i) != Obj.magic (Array.unsafe_get ya i)
        then equal := false
      done;
      !equal
  | _ -> false

(*  Scheduler helpers *)

let rec mark_dirty f =
  if not f.dirty then (
    f.dirty <- true;
    Option.iter mark_dirty f.parent)

let schedule_update f =
  mark_dirty f;
  f.pending_updates <- true;
  Option.iter Program.request_render f.root_program

(*  Hook interpreter *)

let rec update_hook_list i new_val = function
  | [] -> failwith "Hook not found"
  | State s :: tl when i = 0 ->
      State s :: tl (* unchanged here; updated elsewher *)
  | h :: tl -> h :: update_hook_list (i - 1) new_val tl

let fiber_handler (fiber : fiber) (type b) (eff : b Effect.t)
    (k : (b, _) continuation) =
  match eff with
  | Use_state init -> (
      let idx = fiber.hook_index in
      fiber.hook_index <- idx + 1;
      match List.nth_opt fiber.hooks idx with
      | Some (State { value; setter }) ->
          continue k (Obj.magic value, Obj.magic setter)
      | _ ->
          let cell = ref (Obj.repr init) in
          let setter v =
            cell := Obj.repr v;
            schedule_update fiber
          in
          fiber.hooks <-
            fiber.hooks
            @ [
                State { value = !cell; setter = (fun v -> setter (Obj.obj v)) };
              ];
          continue k (init, setter))
  | Use_effect (setup, deps) -> (
      let idx = fiber.hook_index in
      fiber.hook_index <- idx + 1;
      match List.nth_opt fiber.hooks idx with
      | Some (Effect (_, old_deps, _cleanup)) when deps_equal old_deps deps ->
          continue k ()
      | Some (Effect (_, _, cleanup)) ->
          Option.iter (fun c -> c ()) cleanup;
          let new_cleanup = setup () in
          fiber.hooks <-
            List.mapi
              (fun i h ->
                if i = idx then Effect (setup, Obj.magic deps, new_cleanup)
                else h)
              fiber.hooks;
          continue k ()
      | _ ->
          let c = setup () in
          fiber.hooks <- fiber.hooks @ [ Effect (setup, Obj.magic deps, c) ];
          continue k ())
  | Use_context id ->
      let rec find f =
        match
          List.find_opt
            (function Context_value { id = id'; _ } -> Type.Id.equal id id')
            f.contexts
        with
        | Some (Context_value { value; _ }) -> continue k (Obj.magic value)
        | None -> (
            match f.parent with
            | Some p -> find p
            | None -> failwith "Context not found")
      in
      find fiber
  | Dispatch_cmd cmd ->
      fiber.pending_cmds <- Cmd.batch [ fiber.pending_cmds; cmd ];
      continue k ()
  | Use_subscription sub ->
      let idx = fiber.hook_index in
      fiber.hook_index <- idx + 1;
      (match List.nth_opt fiber.hooks idx with
      | Some (Subscription _) ->
          fiber.hooks <-
            List.mapi
              (fun i h -> if i = idx then Subscription sub else h)
              fiber.hooks
      | _ -> fiber.hooks <- fiber.hooks @ [ Subscription sub ]);
      continue k ()
  | _ -> continue k (Effect.perform eff)

(*  Fiber construction & reconciliation *)

let rec destroy_fiber f =
  List.iter (function Effect (_, _, Some c) -> c () | _ -> ()) f.hooks;
  List.iter destroy_fiber f.child_slots

let create_fiber ?parent ~root_program component =
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

let rec reconcile fiber =
  if not fiber.dirty then ()
  else (
    fiber.dirty <- false;
    fiber.child_slots <- [];
    fiber.hook_index <- 0;
    let new_ui =
      match fiber.component with
      | None -> Ui.spacer ()
      | Some (Component.Any ((module C), props)) ->
          let prev = !current_fiber in
          current_fiber := Some fiber;
          let r = C.make props in
          current_fiber := prev;
          r
    in
    fiber.ui <- Some new_ui;
    List.iter reconcile fiber.child_slots)

(* Helpers to render a child componen *)
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
            create_fiber ~parent ~root_program:parent.root_program
              (Some (Component.Any (c, props)))
      in
      parent.child_slots <- parent.child_slots @ [ child ];
      let prev = !current_fiber in
      current_fiber := Some child;
      if child.dirty then reconcile child;
      let out = Option.get child.ui in
      current_fiber := prev;
      out

(*  Context API *)

type 'ctx context = { id : 'ctx Type.Id.t; default : 'ctx option }

let create_context ?default () = { id = Type.Id.make (); default }

let provide ctx value children =
  match !current_fiber with
  | None -> failwith "provide outside fiber"
  | Some f ->
      let old = f.contexts in
      f.contexts <- Context_value { id = ctx.id; value } :: old;
      let out = children () in
      f.contexts <- old;
      out

(*  Hook wrappers exposed to user code *)

let use_state v = Effect.perform (Use_state v)
let use_effect ?deps f = Effect.perform (Use_effect (f, deps))
let use_context ctx = Effect.perform (Use_context ctx.id)
let dispatch_cmd cmd = Effect.perform (Dispatch_cmd cmd)
let use_subscription sub = Effect.perform (Use_subscription sub)

(*  Collect helpers *)

let rec collect_pending f =
  let here = f.pending_cmds in
  f.pending_cmds <- Cmd.none;
  Cmd.batch (here :: List.map collect_pending f.child_slots)

let rec collect_subs f =
  let local =
    List.fold_left
      (fun acc -> function Subscription s -> Sub.batch [ acc; s ] | _ -> acc)
      Sub.none f.hooks
  in
  Sub.batch (local :: List.map collect_subs f.child_slots)

(*  Runtime (Eio) *)

let run_eio ~sw ~env ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    ?(debug = false) (root : unit -> Ui.element) =
  let debug_log = if debug then Some (open_out "mosaic-debug.log") else None in
  let cfg = Program.config ?terminal ~alt_screen ~mouse ~fps ?debug_log () in

  (* Stub handles to be filled after star *)
  let prog_ref : Program.t option ref = ref None in
  let root_fiber_ref : fiber option ref = ref None in

  (* Message stream for hook‑generated command *)
  let cmd_stream = Eio.Stream.create 128 in

  (* Input / resize handlers *)
  let dispatch_msg (Msg (v, handler)) = handler v in

  let handle_input_event (event : Input.event) =
    let root_fiber = Option.get !root_fiber_ref in
    let subs = collect_subs root_fiber in
    let msgs =
      match event with
      | Input.Key k ->
          Sub.collect_keyboard [] subs |> List.filter_map (fun f -> f k)
      | Input.Focus ->
          Sub.collect_focus [] subs |> List.filter_map (fun f -> f ())
      | Input.Blur ->
          Sub.collect_blur [] subs |> List.filter_map (fun f -> f ())
      | Input.Mouse m_ev ->
          Sub.collect_mouse [] subs |> List.filter_map (fun f -> f m_ev)
      | Input.Paste s ->
          Sub.collect_paste [] subs |> List.filter_map (fun f -> f s)
      | Input.Resize (w, h) ->
          Program.request_render (Option.get !prog_ref);
          let size = { Sub.width = w; height = h } in
          Sub.collect_window [] subs |> List.filter_map (fun f -> f size)
      | _ -> []
    in
    List.iter dispatch_msg msgs
  in

  let handle_resize ~w ~h = handle_input_event (Input.Resize (w, h)) in

  (* render callback for Program *)
  let render () =
    let root_fiber = Option.get !root_fiber_ref in
    if root_fiber.pending_updates then (
      reconcile root_fiber;
      root_fiber.pending_updates <- false;
      (* push any freshly collected cmds to strea *)
      let cmds = collect_pending root_fiber in
      if cmds <> Cmd.none then Eio.Stream.add cmd_stream cmds);
    Option.get root_fiber.ui
  in

  (* Start Program runtime *)
  let prog, process_cmd =
    Program.start ~sw ~env cfg ~render ~on_input:handle_input_event
      ~on_resize:handle_resize ()
  in
  prog_ref := Some prog;

  (* create root fiber now that prog exist *)
  let root_fiber = create_fiber ~root_program:(Some prog) None in
  root_fiber_ref := Some root_fiber;

  (* initial moun *)
  let prev = !current_fiber in
  current_fiber := Some root_fiber;
  let initial_ui = root () in
  current_fiber := prev;
  root_fiber.ui <- Some initial_ui;
  let init_cmds = collect_pending root_fiber in
  if init_cmds <> Cmd.none then Eio.Stream.add cmd_stream init_cmds;

  (* message loop (process_cmd) *)
  let message_loop () =
    try
      while true do
        let cmds = Eio.Stream.take cmd_stream in
        process_cmd dispatch_msg cmds
      done
    with End_of_file -> ()
  in
  Eio.Fiber.fork ~sw message_loop

let run ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    ?(debug = false) root =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  run_eio ~sw ~env ?terminal ~alt_screen ~mouse ~fps ~debug root

(*  Component helper *)

let component = Component.make
