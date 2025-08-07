module Sub = Engine.Sub
module Cmd = Engine.Cmd

type t = {
  parent : t option;
  mutable children : t array;
  hooks : Hook_array.t;
  mutable hook_idx : int;
  render_fn : unit -> Ui.element;
  mutable ui : Ui.element option;
  mutable dirty : bool;
}

(* Domain-local pointer to the fiber that is *currently rendering*. *)
let current_key : t option Domain.DLS.key = Domain.DLS.new_key (fun () -> None)
let get_current () = Domain.DLS.get current_key
let set_current f = Domain.DLS.set current_key (Some f)

let make ?parent render_fn =
  {
    parent;
    children = [||];
    hooks = Hook_array.create ();
    hook_idx = 0;
    render_fn;
    ui = None;
    dirty = true;
  }

let create_root render = make render

let with_current f k =
  let old = get_current () in
  set_current f;
  Fun.protect k ~finally:(fun () -> Domain.DLS.set current_key old)

let mark_dirty f =
  let rec bubble n =
    if not n.dirty then (
      n.dirty <- true;
      match n.parent with Some p -> bubble p | None -> ())
  in
  bubble f

let is_dirty f = f.dirty

(* Type-erased command for React *)
type erased_cmd = ErasedCmd : 'msg Cmd.t -> erased_cmd

(* Pending commands stored globally for the current render *)
let pending_cmds : erased_cmd list ref = ref []

(* Add a command to the pending queue *)
let add_pending_cmd (type msg) (cmd : msg Cmd.t) =
  pending_cmds := ErasedCmd cmd :: !pending_cmds

(* Hook effect handler *)
let with_handler f k =
  let open Effect.Deep in
  try_with k ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Hook.Use_state init ->
              Some
                (fun (cont : (a, _) continuation) ->
                  let idx = f.hook_idx in
                  f.hook_idx <- idx + 1;
                  let slot = Hook_array.get_slot f.hooks idx in
                  match Hook_array.get slot with
                  | Hook_array.State r ->
                      let r = Obj.obj (Obj.repr r) in
                      continue cont
                        ( !r,
                          fun v ->
                            r := v;
                            mark_dirty f )
                  | _ ->
                      let r = ref init in
                      Hook_array.set slot
                        (Hook_array.State (Obj.obj (Obj.repr r)));
                      continue cont
                        ( init,
                          fun v ->
                            r := v;
                            mark_dirty f ))
          | Hook.Use_effect (setup, deps) ->
              Some
                (fun (cont : (a, _) continuation) ->
                  let idx = f.hook_idx in
                  f.hook_idx <- idx + 1;
                  let slot = Hook_array.get_slot f.hooks idx in
                  (match Hook_array.get slot with
                  | Hook_array.Effect { deps = old; _ } when old = deps -> ()
                  | Hook_array.Effect { cleanup; _ } ->
                      Option.iter (fun f -> f ()) cleanup
                  | _ -> ());
                  let cleanup = setup () in
                  Hook_array.set slot (Hook_array.Effect { cleanup; deps });
                  continue cont ())
          | Hook.Use_context ctx ->
              Some
                (fun (cont : (a, _) continuation) ->
                  continue cont (Context.use ctx))
          | Hook.Use_subscription sub ->
              Some
                (fun (cont : (a, _) continuation) ->
                  let idx = f.hook_idx in
                  f.hook_idx <- idx + 1;
                  let slot = Hook_array.get_slot f.hooks idx in
                  Hook_array.set slot (Hook_array.Sub sub);
                  continue cont ())
          | Hook.Dispatch_cmd cmd ->
              Some
                (fun (cont : (a, _) continuation) ->
                  add_pending_cmd cmd;
                  continue cont ())
          | _ -> None (* Let other effects propagate *));
    }

(*  Reconciliation (positional, no keyed diff yet)                   *)
let rec reconcile f : Ui.element =
  if not f.dirty then Option.get f.ui
  else (
    f.dirty <- false;
    f.hook_idx <- 0;
    let ui = with_current f (fun () -> with_handler f f.render_fn) in
    f.ui <- Some ui;
    f.children <- Array.map (fun child -> reconcile_child child) f.children;
    ui)

and reconcile_child child =
  ignore (reconcile child);
  child

let destroy _f = () (* left as an exercise; run clean-ups, etc. *)

(* Type-erased subscription for React *)
type erased_sub = ErasedSub : 'msg Sub.t -> erased_sub

(* Collect all subscriptions from this fiber and its children *)
let rec collect_subscriptions_internal f acc =
  (* Collect subscriptions from this fiber's hooks *)
  let local_subs = ref acc in
  for i = 0 to Hook_array.length f.hooks - 1 do
    let slot = Hook_array.get_slot f.hooks i in
    match Hook_array.get slot with
    | Hook_array.Sub sub -> local_subs := ErasedSub sub :: !local_subs
    | _ -> ()
  done;
  (* Collect from children *)
  Array.fold_left (fun acc child ->
    collect_subscriptions_internal child acc
  ) !local_subs f.children

let collect_subscriptions f = collect_subscriptions_internal f []

(* Check if there are pending commands *)
let has_pending_commands _f =
  !pending_cmds <> []

(* Collect and clear pending commands *)
let collect_pending_commands _f =
  let cmds = !pending_cmds in
  pending_cmds := [];
  ignore cmds;
  (* For React, we don't actually use commands in the traditional way *)
  (* They're handled differently through effects *)
  Cmd.none