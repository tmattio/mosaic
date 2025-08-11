module Sub = Engine.Sub
module Cmd = Engine.Cmd

let src = Logs.Src.create "fiber" ~doc:"Fiber lifecycle events"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  parent : t option;
  mutable children : t array;
  hooks : Hook_array.t;
  mutable hook_idx : int;
  render_fn : unit -> Ui.element;
  mutable ui : Ui.element option;
  mutable dirty : bool;
  mutable alive : bool;
}

and erased_sub = Erased_sub : 'msg Sub.t -> erased_sub

(* Global pointer to the fiber that is *currently rendering*. *)
let current_fiber : t option ref = ref None
let get_current () = !current_fiber
let set_current f = current_fiber := Some f

let make ?parent render_fn =
  {
    parent;
    children = [||];
    hooks = Hook_array.create ();
    hook_idx = 0;
    render_fn;
    ui = None;
    dirty = true;
    alive = true;
  }

let create_root render = make render

let with_current f k =
  let old = get_current () in
  set_current f;
  Fun.protect k ~finally:(fun () -> current_fiber := old)

(* Global ref for requesting renders when bubbling to root *)
let request_render_ref : (unit -> unit) ref = ref (fun () -> ())

let mark_dirty ?(reason = "unknown") f =
  if not f.alive then ()
  else (
    Log.info (fun m -> m "Marking fiber dirty (reason: %s)" reason);
    let rec bubble n =
      if not n.dirty then (
        n.dirty <- true;
        match n.parent with
        | Some p -> bubble p
        | None ->
            Log.info (fun m -> m "Render requested (trigger: %s)" reason);
            !request_render_ref ())
    in
    bubble f)

let is_dirty f = f.dirty

(* Type-erased command for React *)
type erased_cmd = Erased_cmd : unit Cmd.t -> erased_cmd

(* Global storage for pending commands *)
let pending_cmds : erased_cmd list ref = ref []

(* Add a command to the pending queue – map its message type to unit *)
let add_pending_cmd (type msg) (cmd : msg Cmd.t) =
  let cmd' : unit Cmd.t = Cmd.map (fun _ -> ()) cmd in
  pending_cmds := Erased_cmd cmd' :: !pending_cmds

let has_pending_commands _f = !pending_cmds <> []

(* Collect and clear pending commands (FIFO) *)
let collect_pending_commands _f : erased_cmd list =
  let cmds = List.rev !pending_cmds in
  pending_cmds := [];
  cmds

(* Helper to check if deps changed for effects and memos *)
let check_effect_deps_changed slot deps =
  match Hook_array.get slot with
  | Hook_array.P (Hook_array.Effect { deps = Some old_deps; _ }) ->
      Deps.changed old_deps deps
  | Hook_array.P (Hook_array.Effect _) -> true
  | _ -> true

let check_memo_deps_changed slot deps =
  match Hook_array.get slot with
  | Hook_array.P (Hook_array.Memo (_, Some old_deps)) ->
      Deps.changed old_deps deps
  | Hook_array.P (Hook_array.Memo _) -> true
  | _ -> true

(* Helper to run cleanup for effects *)
let run_effect_cleanup slot =
  match Hook_array.get slot with
  | Hook_array.P (Hook_array.Effect { cleanup = Some c; _ }) -> (
      try c () with _ -> ())
  | _ -> ()

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
                  let state_ref =
                    match Hook_array.get slot with
                    | Hook_array.P (Hook_array.State r) -> Obj.obj (Obj.repr r)
                    | _ ->
                        let r = ref init in
                        Hook_array.set slot
                          (Hook_array.State (Obj.obj (Obj.repr r)));
                        r
                  in
                  continue cont
                    ( !state_ref,
                      fun upd ->
                        state_ref := upd !state_ref;
                        mark_dirty ~reason:"state_update" f ))
          | Hook.Use_effect (setup, deps) ->
              Some
                (fun (cont : (a, _) continuation) ->
                  let idx = f.hook_idx in
                  f.hook_idx <- idx + 1;
                  let slot = Hook_array.get_slot f.hooks idx in
                  let rerun = check_effect_deps_changed slot deps in
                  if rerun then (
                    run_effect_cleanup slot;
                    let cleanup = setup () in
                    Hook_array.set slot
                      (Hook_array.Effect { cleanup; deps = Some deps }));
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
          | Hook.Use_reducer (rid, reducer, initial, dynamic) ->
              Some
                (fun (cont : (a, _) continuation) ->
                  let idx = f.hook_idx in
                  f.hook_idx <- idx + 1;
                  let slot = Hook_array.get_slot f.hooks idx in
                  match Hook_array.get slot with
                  | Hook_array.P
                      (Hook_array.Reducer
                         { sid; aid; state; reducer = rref; dynamic = _ }) -> (
                      (* Ensure type equality: rid.(s/a) must match stored sid/aid *)
                      match
                        ( Type.Id.provably_equal sid rid.sid,
                          Type.Id.provably_equal aid rid.aid )
                      with
                      | Some Type.Equal, Some Type.Equal ->
                          (* Types match - we can safely use the stored state *)
                          if dynamic then rref := reducer;
                          (* latest semantics *)
                          continue cont
                            ( !state,
                              fun action ->
                                state := !rref !state action;
                                mark_dirty ~reason:"reducer_action" f )
                      | _ ->
                          (* Type mismatch at the same hook index: treat as remount *)
                          let state_ref = ref initial in
                          let reducer_ref = ref reducer in
                          Hook_array.set slot
                            (Hook_array.Reducer
                               {
                                 sid = rid.sid;
                                 aid = rid.aid;
                                 state = state_ref;
                                 reducer = reducer_ref;
                                 dynamic;
                               });
                          continue cont
                            ( !state_ref,
                              fun action ->
                                state_ref := !reducer_ref !state_ref action;
                                mark_dirty ~reason:"reducer_action" f ))
                  | _ ->
                      (* First mount *)
                      let state_ref = ref initial in
                      let reducer_ref = ref reducer in
                      let reducer_slot : _ Hook_array.kind =
                        Hook_array.Reducer
                          {
                            sid = rid.sid;
                            aid = rid.aid;
                            state = state_ref;
                            reducer = reducer_ref;
                            dynamic;
                          }
                      in
                      Hook_array.set slot reducer_slot;
                      continue cont
                        ( !state_ref,
                          fun action ->
                            state_ref := !reducer_ref !state_ref action;
                            mark_dirty ~reason:"reducer_action" f ))
          | Hook.Use_memo (compute, deps) ->
              Some
                (fun (cont : (a, _) continuation) ->
                  let idx = f.hook_idx in
                  f.hook_idx <- idx + 1;
                  let slot = Hook_array.get_slot f.hooks idx in
                  let recompute = check_memo_deps_changed slot deps in
                  if recompute then (
                    let value = compute () in
                    Hook_array.set slot
                      (Hook_array.Memo (Obj.obj (Obj.repr value), Some deps));
                    continue cont value)
                  else
                    match Hook_array.get slot with
                    | Hook_array.P (Hook_array.Memo (value, _)) ->
                        continue cont (Obj.obj (Obj.repr value))
                    | _ -> failwith "Unexpected hook state")
          | Hook.Use_ref initial ->
              Some
                (fun (cont : (a, _) continuation) ->
                  let idx = f.hook_idx in
                  f.hook_idx <- idx + 1;
                  let slot = Hook_array.get_slot f.hooks idx in
                  match Hook_array.get slot with
                  | Hook_array.P (Hook_array.Ref r) ->
                      continue cont (Obj.obj (Obj.repr r))
                  | _ ->
                      let r = ref initial in
                      Hook_array.set slot
                        (Hook_array.Ref (Obj.obj (Obj.repr r)));
                      continue cont r)
          | _ -> None (* Let other effects propagate *));
    }

(*  Reconciliation (positional, no keyed diff yet)                   *)
let rec reconcile f : Ui.element =
  if not f.dirty then Option.get f.ui
  else
    let start_time = Unix.gettimeofday () in
    (* Only log if actually reconciling *)
    Log.debug (fun m -> m "Reconciling fiber (was dirty)");
    f.dirty <- false;
    f.hook_idx <- 0;
    let ui = with_current f (fun () -> with_handler f f.render_fn) in
    f.ui <- Some ui;
    let new_children =
      Array.mapi
        (fun i child ->
          (* Yield per child to prevent blocking during large tree reconciliations *)
          if i > 0 && i mod 10 = 0 then Eio.Fiber.yield ();
          reconcile_child child)
        f.children
    in
    f.children <- new_children;
    let elapsed_ms = (Unix.gettimeofday () -. start_time) *. 1000. in
    Log.debug (fun m -> m "Reconciliation completed in %.2fms" elapsed_ms);
    ui

and reconcile_child child =
  ignore (reconcile child);
  child

let rec destroy f =
  if not f.alive then ()
  else (
    f.alive <- false;
    (* Run cleanup for all effects *)
    for i = 0 to Hook_array.length f.hooks - 1 do
      run_effect_cleanup (Hook_array.get_slot f.hooks i)
    done;
    (* Destroy all children *)
    Array.iter destroy f.children;
    (* Clear UI reference *)
    f.ui <- None;
    f.dirty <- false)

(* Collect all subscriptions from this fiber and its children *)
let rec collect_subscriptions f =
  (* Collect subscriptions from this fiber's hooks *)
  let local_subs = ref [] in
  for i = 0 to Hook_array.length f.hooks - 1 do
    match Hook_array.get (Hook_array.get_slot f.hooks i) with
    | Hook_array.P (Hook_array.Sub sub) ->
        local_subs := Erased_sub sub :: !local_subs
    | _ -> ()
  done;
  (* Yield after processing hooks to allow other fibers to run *)
  if Hook_array.length f.hooks > 0 then Eio.Fiber.yield ();
  (* Collect from children and combine *)
  Array.fold_left
    (fun acc child ->
      (* Yield per child for large trees *)
      if Array.length f.children > 1 then Eio.Fiber.yield ();
      collect_subscriptions child @ acc)
    !local_subs f.children
