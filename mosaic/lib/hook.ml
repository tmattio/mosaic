module Reducer_id = struct
  (** Type witnesses for reducers *)

  type ('s, 'a) t = { sid : 's Type.Id.t; aid : 'a Type.Id.t }

  let make () = { sid = Type.Id.make (); aid = Type.Id.make () }
end

(* custom effects *)
type _ Effect.t +=
  | Use_state : 'a -> ('a * (('a -> 'a) -> unit)) Effect.t
  | Use_effect : (unit -> (unit -> unit) option) * Deps.t -> unit Effect.t
  | Use_context : 'a Context.t -> 'a Effect.t
  | Use_subscription : 'msg Engine.Sub.t -> unit Effect.t
  | Dispatch_cmd : 'msg Engine.Cmd.t -> unit Effect.t
  | Use_reducer :
      (('s, 'a) Reducer_id.t * ('s -> 'a -> 's) * 's * bool)
      -> ('s * ('a -> unit)) Effect.t
  | Use_memo : (unit -> 'b) * Deps.t -> 'b Effect.t
  | Use_ref : 'a -> 'a ref Effect.t

(* public wrappers *)
let use_state v =
  let value, update = Effect.perform (Use_state v) in
  let set x = update (fun _ -> x) in
  (value, set, update)

let use_effect ?(deps = Deps.always) f = Effect.perform (Use_effect (f, deps))
let use_context ctx = Effect.perform (Use_context ctx)
let use_subscription sub = Effect.perform (Use_subscription sub)
let dispatch_cmd cmd = Effect.perform (Dispatch_cmd cmd)
let use_memo f ~deps = Effect.perform (Use_memo (f, deps))
let use_ref initial = Effect.perform (Use_ref initial)
let use_callback f ~deps = use_memo (fun () -> f) ~deps

let use_reducer ?id reducer initial =
  (* React semantics: reducer is fixed after mount *)
  let id =
    match id with
    | Some id -> id
    | None -> (
        (* Keep a stable id per callsite *)
        let rid = use_ref None in
        match !rid with
        | Some id -> id
        | None ->
            let fresh = Reducer_id.make () in
            rid := Some fresh;
            fresh)
  in
  Effect.perform (Use_reducer (id, reducer, initial, false))

let use_reducer_latest ?id reducer initial =
  (* Optional variant: update reducer every render *)
  let id =
    match id with
    | Some id -> id
    | None -> (
        let rid = use_ref None in
        match !rid with
        | Some id -> id
        | None ->
            let fresh = Reducer_id.make () in
            rid := Some fresh;
            fresh)
  in
  Effect.perform (Use_reducer (id, reducer, initial, true))

let use_keyboard ?(ctrl = false) ?(alt = false) ?(shift = false) key cmd =
  use_subscription
    (Engine.Sub.keyboard_filter (fun event ->
         if
           event.Input.key = key
           && event.Input.modifier.ctrl = ctrl
           && event.Input.modifier.alt = alt
           && event.Input.modifier.shift = shift
         then (
           dispatch_cmd cmd;
           Some ())
         else None))

let use_tick f = use_subscription (Engine.Sub.on_tick f)
let use_timer ~every f = use_subscription (Engine.Sub.timer ~every f)

let use_scroll ?(initial = 0) ?(min_offset = 0) ?(max_offset = max_int)
    ?(momentum = false) ?(friction = 2.0) ?(impulse_scale = 3.0)
    ?(inertia_threshold = 0.05) ?(alpha = 0.8) ?(max_dt = 0.1) () =
  let offset, set_offset, update_offset = use_state initial in
  let velocity, set_velocity, update_velocity = use_state 0.0 in
  (* Always call *)
  let _accumulated, _, update_accumulated = use_state 0.0 in
  (* Always call *)
  let last_input_time = use_ref (Unix.gettimeofday ()) in
  (* Always call *)
  let last_scroll_time = use_ref (Unix.gettimeofday ()) in
  (* New: for dt calc *)

  (* Helper to clamp offset *)
  let clamp v = max min_offset (min max_offset v) in

  (* Reset velocity when momentum toggles off *)
  use_effect
    ~deps:(Deps.keys [ Deps.bool momentum ])
    (fun () ->
      if not momentum then set_velocity 0.0;
      None);

  (* Always subscribe, but use Sub.none when !momentum *)
  use_subscription
    (if momentum then
       Engine.Sub.on_tick (fun dt ->
           let now = Unix.gettimeofday () in
           if
             now -. !last_input_time > inertia_threshold
             && abs_float velocity > 0.01
           then (
             let delta = velocity *. dt in
             update_accumulated (fun acc ->
                 let new_acc = acc +. delta in
                 let int_delta = int_of_float (Float.floor new_acc) in
                 if int_delta <> 0 then (
                   update_offset (fun o -> clamp (o + int_delta));
                   new_acc -. Float.floor new_acc)
                 else new_acc);
             update_velocity (fun v -> v *. exp (-.friction *. dt))))
     else Engine.Sub.none);

  let scroll_by delta =
    let now = Unix.gettimeofday () in
    let dt = now -. !last_scroll_time in
    last_scroll_time := now;
    update_offset (fun o -> clamp (o + delta));
    if momentum then (
      last_input_time := now;
      let instant_vel =
        if dt > 0. && dt <= max_dt then float_of_int delta /. dt
        else
          float_of_int delta *. impulse_scale (* Fallback for large/stale dt *)
      in
      update_velocity (fun v -> (alpha *. instant_vel) +. ((1. -. alpha) *. v)))
  in

  let set_offset_clamped v = set_offset (clamp v) in

  (* Return offset, scroll_by function, and setter *)
  (offset, scroll_by, set_offset_clamped)
