module Make (Incr : Incremental.S) = struct
  type 'a t = 'a Incr.t

  type ('primitive, 'host_props, 'props) component =
    'props -> ('primitive, 'host_props) Reconciler.Element.t t

  let component f = f

  module Hooks = struct
    type 'a state = 'a t * ('a -> unit)

    let use_state (initial : 'a) : 'a state =
      let v = Incr.Var.create initial in
      (Incr.Var.watch v, fun x -> Incr.Var.set v x)

    let use_reducer (reducer : 's -> 'a -> 's) ~(init : 's) :
        's t * ('a -> unit) =
      let v = Incr.Var.create init in
      let dispatch (action : 'a) =
        let curr = Incr.Var.value v in
        let next = reducer curr action in
        Incr.Var.set v next
      in
      (Incr.Var.watch v, dispatch)

    let use_memo (f : 'd -> 'r) ~(deps : 'd t) : 'r t = Incr.map deps ~f

    let use_callback (f : 'd -> 'arg -> 'r) ~(deps : 'd t) : ('arg -> 'r) t =
      Incr.map deps ~f:(fun d -> fun arg -> f d arg)

    type 'a ref_ = { mutable current : 'a }

    let use_ref (init : 'a) : 'a ref_ = { current = init }

    module Context = struct
      type 'a t = 'a Incr.Var.t

      let create (initial : 'a) : 'a t = Incr.Var.create initial
      let provide (ctx : 'a t) (value : 'a) = Incr.Var.set ctx value
      let use_context (ctx : 'a t) : 'a Incr.t = Incr.Var.watch ctx
    end

    let run_effect_with_observer (create : unit -> (unit -> unit) option)
        ~(deps : 'd t) : unit -> unit =
      let obs = Incr.observe deps in
      let cleanup = ref None in
      Incr.Observer.on_update_exn obs ~f:(function
        | Incr.Observer.Update.Initialized _ ->
            (match !cleanup with Some c -> c () | None -> ());
            cleanup := create ()
        | Incr.Observer.Update.Changed (_old, _new) ->
            (match !cleanup with Some c -> c () | None -> ());
            cleanup := create ()
        | Incr.Observer.Update.Invalidated ->
            (match !cleanup with Some c -> c () | None -> ());
            cleanup := None);
      (* Return cancel handle *)
      fun () ->
        Incr.Observer.disallow_future_use obs;
        match !cleanup with
        | Some c ->
            cleanup := None;
            c ()
        | None -> ()

    let use_effect create ~(deps : 'd t) = run_effect_with_observer create ~deps

    let use_layout_effect create ~(deps : 'd t) =
      (* Same timing as use_effect in this minimal version. *)
      run_effect_with_observer create ~deps
  end
end
