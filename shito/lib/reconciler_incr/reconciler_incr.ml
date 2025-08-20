module Make (Host : Reconciler.Host_config.S) (Incr : Incremental.S) = struct
  (* module Core = (val (module Reconciler.Make(Host)) : module type of Reconciler.Make(Host)) *)
  (* OCaml can’t easily “module-of” a functor result in value form; instead: *)
  module CoreR = Reconciler.Make (Host)

  type element = (Host.primitive, Host.props) Reconciler.Element.t

  let render (container : Host.container)
      (root_component : unit -> element Incr.t) : unit =
    let element_incr = root_component () in
    let obs = Incr.observe element_incr in
    let current_root : CoreR.root option ref = ref None in

    Incr.Observer.on_update_exn obs ~f:(function
      | Incr.Observer.Update.Initialized tree -> (
          (* First render *)
          match !current_root with
          | None -> current_root := Some (CoreR.mount ~container tree)
          | Some r -> CoreR.update r tree)
      | Incr.Observer.Update.Changed (_old, tree) -> (
          match !current_root with
          | None -> current_root := Some (CoreR.mount ~container tree)
          | Some r -> CoreR.update r tree)
      | Incr.Observer.Update.Invalidated -> (
          match !current_root with
          | None -> ()
          | Some r ->
              CoreR.unmount r;
              current_root := None));

    (* Kick the initial stabilization so Initialized fires at least once. *)
    Incr.stabilize ()

  let create_root (container : Host.container) =
   fun (component : unit -> element Incr.t) -> render container component
end
