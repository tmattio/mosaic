(** Runtime context implementation *)

type t = {
  input_router : Engine.Input_router.t;
  focus_manager : Engine.Focus_manager.t;
  mutable snapshot : Ui.Layout_snapshot.t option;
}

(* Domain-local reference for the current context *)
let current_key : t option Domain.DLS.key = Domain.DLS.new_key (fun () -> None)

let create () =
  let router = Engine.Input_router.create () in
  let focus_mgr = Engine.Focus_manager.create () in
  Engine.Focus_manager.set_router focus_mgr router;
  { input_router = router; focus_manager = focus_mgr; snapshot = None }

let current () = Domain.DLS.get current_key
let set_current ctx = Domain.DLS.set current_key ctx

let update_snapshot t snapshot =
  t.snapshot <- Some snapshot;
  Engine.Input_router.set_snapshot t.input_router snapshot

let get_snapshot t = t.snapshot

(* Create a React-style context for the runtime *)
let context = Context.create ~name:"runtime" ()
