(** Public hook API (effect-based). *)

module Reducer_id : sig
  type ('s, 'a) t = { sid : 's Type.Id.t; aid : 'a Type.Id.t }

  val make : unit -> ('a, 'b) t
end

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

val use_state : 'a -> 'a * ('a -> unit) * (('a -> 'a) -> unit)
(** Returns (value, set, update). [set v] sets the state to v. [update f]
    applies [f] to the latest state. *)

val use_effect : ?deps:Deps.t -> (unit -> (unit -> unit) option) -> unit
(** Registers an effect with typed dependencies. *)

val use_context : 'a Context.t -> 'a
(** Subscribe to a React-style context *)

val use_subscription : 'msg Engine.Sub.t -> unit
(** Register a subscription *)

val dispatch_cmd : 'msg Engine.Cmd.t -> unit
(** Dispatch a command *)

val use_reducer :
  ?id:('s, 'a) Reducer_id.t -> ('s -> 'a -> 's) -> 's -> 's * ('a -> unit)
(** use_reducer reducer initial_state - React semantics (reducer fixed after
    mount) *)

val use_reducer_latest :
  ?id:('s, 'a) Reducer_id.t -> ('s -> 'a -> 's) -> 's -> 's * ('a -> unit)
(** use_reducer_latest reducer initial_state - Updates reducer every render *)

val use_memo : (unit -> 'b) -> deps:Deps.t -> 'b
(** Memoize a computation based on dependencies *)

val use_ref : 'a -> 'a ref
(** Create a stable mutable reference *)

val use_callback : 'a -> deps:Deps.t -> 'a
(** Memoize a callback function *)

val use_keyboard :
  ?ctrl:bool ->
  ?alt:bool ->
  ?shift:bool ->
  Input.key ->
  'msg Engine.Cmd.t ->
  unit
(** Subscribe to keyboard shortcuts *)

val use_tick : (float -> unit) -> unit
(** Subscribe to animation tick events. The callback receives elapsed time in
    seconds since the last tick. Useful for animations and time-based updates.
*)

val use_timer : every:float -> (unit -> unit) -> unit
(** Subscribe to a repeating timer that fires every [every] seconds. The timer
    automatically resets after firing and continues repeating. For one-shot
    delays, use [dispatch_cmd (Cmd.after ~delay msg)] instead. *)

val use_scroll :
  ?initial:int ->
  ?min_offset:int ->
  ?max_offset:int ->
  ?momentum:bool ->
  ?friction:float ->
  ?impulse_scale:float ->
  ?inertia_threshold:float ->
  ?alpha:float ->
  ?max_dt:float ->
  unit ->
  int * (int -> unit) * (int -> unit)
(** [use_scroll ()] creates a scrolling state with optional momentum physics.

    Returns [(offset, scroll_by, set_offset)] where:
    - [offset] is the current scroll position
    - [scroll_by delta] scrolls by the given amount (positive = down)
    - [set_offset v] sets the scroll position directly

    Optional parameters:
    - [initial]: Initial scroll offset (default: 0)
    - [min_offset]: Minimum allowed offset (default: 0)
    - [max_offset]: Maximum allowed offset (default: max_int)
    - [momentum]: Enable momentum scrolling with inertia (default: false)
    - [friction]: Friction coefficient for momentum decay (default: 2.0)
    - [impulse_scale]: Scale factor for scroll impulses (default: 3.0)
    - [inertia_threshold]: Time after input before momentum kicks in (default:
      0.05s)
    - [alpha]: Smoothing factor for velocity calculation (default: 0.8)
    - [max_dt]: Maximum time delta for velocity calculation (default: 0.1)

    When momentum is disabled (default), scrolling is immediate and stops
    instantly. When momentum is enabled, scrolling has inertia and gradually
    decelerates with smooth velocity based on input speed. *)
