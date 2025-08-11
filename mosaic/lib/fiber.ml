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
  (* NEW: For subscription caching *)
  mutable subs_cache : erased_sub list option;
  mutable subs_dirty : bool;
}
and erased_sub = Erased_sub : 'msg Sub.t -> erased_sub

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
    alive = true;
    subs_cache = None;  (* NEW *)
    subs_dirty = true;   (* NEW: Start dirty to force initial collection *)
  }

let create_root render = make render

let with_current f k =
  let old = get_current () in
  set_current f;
  Fun.protect k ~finally:(fun () -> Domain.DLS.set current_key old)

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

(* NEW: Similar to mark_dirty, but for subscription changes *)
let mark_subs_dirty ?(reason = "subs_change") f =
  if not f.alive then ()
  else (
    Log.debug (fun m -> m "Marking fiber subs dirty (reason: %s)" reason);
    let rec bubble n =
      if not n.subs_dirty then (
        n.subs_dirty <- true;
        n.subs_cache <- None;  (* Invalidate cache *)
        match n.parent with
        | Some p -> bubble p
        | None -> ()
      )
    in
    bubble f
  )

let is_dirty f = f.dirty

(* Type-erased command for React *)
type erased_cmd = Erased_cmd : unit Cmd.t -> erased_cmd

(* Domain-local storage for pending commands *)
let pending_cmds_key : erased_cmd list Domain.DLS.key =
  Domain.DLS.new_key (fun () -> [])

(* Add a command to the pending queue – map its message type to unit *)
let add_pending_cmd (type msg) (cmd : msg Cmd.t) =
  let cmd' : unit Cmd.t = Cmd.map (fun _ -> ()) cmd in
  let cmds = Domain.DLS.get pending_cmds_key in
  Domain.DLS.set pending_cmds_key (Erased_cmd cmd' :: cmds)

let has_pending_commands _f = Domain.DLS.get pending_cmds_key <> []

(* Collect and clear pending commands (FIFO) *)
let collect_pending_commands _f : erased_cmd list =
  let cmds = List.rev (Domain.DLS.get pending_cmds_key) in
  Domain.DLS.set pending_cmds_key [];
  cmds

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
                  | Hook_array.P (Hook_array.State r) ->
                      let r = Obj.obj (Obj.repr r) in
                      continue cont
                        ( !r,
                          fun upd ->
                            r := upd !r;
                            mark_dirty ~reason:"state_update" f )
                  | _ ->
                      let r = ref init in
                      Hook_array.set slot
                        (Hook_array.State (Obj.obj (Obj.repr r)));
                      continue cont
                        ( init,
                          fun upd ->
                            r := upd !r;
                            mark_dirty ~reason:"state_update" f ))
          | Hook.Use_effect (setup, deps) ->
              Some
                (fun (cont : (a, _) continuation) ->
                  let idx = f.hook_idx in
                  f.hook_idx <- idx + 1;
                  let slot = Hook_array.get_slot f.hooks idx in
                  let rerun =
                    match Hook_array.get slot with
                    | Hook_array.P
                        (Hook_array.Effect { deps = Some old_deps; _ }) ->
                        Deps.changed old_deps deps
                    | Hook_array.P (Hook_array.Effect _) -> true
                    | _ -> true
                  in
                  if rerun then (
                    (* Run cleanup from previous effect if any *)
                    (match Hook_array.get slot with
                    | Hook_array.P (Hook_array.Effect { cleanup = Some c; _ })
                      -> (
                        try c () with _ -> ())
                    | _ -> ());
                    (* Run the new effect *)
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
                  (* NEW: Mark subs dirty when adding/changing a subscription *)
                  (* We always mark dirty since we can't compare existentially typed subs *)
                  (match Hook_array.get slot with
                   | Hook_array.P (Hook_array.Sub _) -> () (* Already had a sub, assume unchanged *)
                   | _ -> mark_subs_dirty f);  (* New or changed type *)
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
                  let recompute =
                    match Hook_array.get slot with
                    | Hook_array.P (Hook_array.Memo (_, Some old_deps)) ->
                        Deps.changed old_deps deps
                    | Hook_array.P (Hook_array.Memo _) -> true
                    | _ -> true
                  in
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
    let old_children = f.children in  (* NEW: Save old for comparison *)
    let new_children = Array.mapi (fun i child ->
      (* Yield per child to prevent blocking during large tree reconciliations *)
      if i > 0 && i mod 10 = 0 then Eio.Fiber.yield ();
      reconcile_child child) old_children in
    f.children <- new_children;
    (* NEW: Check if children changed; if so, mark subs dirty *)
    let children_changed =
      Array.length old_children <> Array.length new_children ||
      (let changed = ref false in
       for i = 0 to Array.length new_children - 1 do
         if i < Array.length old_children && old_children.(i) != new_children.(i) then 
           changed := true
       done;
       !changed)
    in
    if children_changed then mark_subs_dirty ~reason:"children_changed" f;
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
      match Hook_array.get (Hook_array.get_slot f.hooks i) with
      | Hook_array.P (Hook_array.Effect { cleanup = Some c; _ }) -> (
          try c () with _ -> ())
      | _ -> ()
    done;
    (* Destroy all children *)
    Array.iter destroy f.children;
    (* Clear UI reference *)
    f.ui <- None;
    f.dirty <- false;
    (* NEW: Clear sub cache *)
    f.subs_cache <- None;
    f.subs_dirty <- true)


(* Collect all subscriptions from this fiber and its children *)
let rec collect_subscriptions_internal f acc =
  (* NEW: Use cache if not dirty *)
  if not f.subs_dirty then
    match f.subs_cache with
    | Some cached -> cached @ acc  (* Append to acc for consistency *)
    | None -> failwith "Invariant violation: clean but no cache"
  else (
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
    (* Collect from children *)
    let all_subs =
      Array.fold_left
        (fun acc child ->
          (* Yield per child for large trees *)
          if Array.length f.children > 1 then Eio.Fiber.yield ();
          collect_subscriptions_internal child acc)
        !local_subs f.children
    in
    (* NEW: Cache the result and clear dirty flag *)
    f.subs_cache <- Some all_subs;
    f.subs_dirty <- false;
    all_subs @ acc
  )

let collect_subscriptions f = collect_subscriptions_internal f []
