open Types

(* Root state for hydration and SSR *)
type ('props, 'host_element, 'host_text, 'host_container) root_state = {
  element: ('props, 'host_element, 'host_text, 'host_container) element option;
  is_dehydrated: bool;
  cache: cache option;
}

(* Cache type *)
and cache = {
  controller: unit;
  data: (string, unit) Hashtbl.t;
  refcount: int;
}

(* Lane map type *)
type 'a lane_map = 'a array

(* Error info for error handlers *)
type error_info = {
  component_stack: string option;
  error_boundary: unit option;
}

(* Suspense hydration callbacks *)
type suspense_hydration_callbacks = {
  on_hydrated: (suspense_instance -> unit) option;
  on_deleted: (suspense_instance -> unit) option;
}

and suspense_instance = {
  mutable dehydrated: bool;
  mutable pending_boundary_fiber: unit option;
  mutable retry_timer: int option;
}

(* The FiberRoot type - top level container for a React tree *)
type ('props, 'state, 'host_element, 'host_text, 'host_container) t = {
  tag: Root_tags.t;
  container_info: 'host_container;
  mutable pending_children: unit option;
  mutable current: ('props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t;
  ping_cache: (unit, unit) Hashtbl.t option;
  mutable timeout_handle: int option;
  mutable cancel_pending_commit: (unit -> unit) option;
  mutable context: unit option;
  mutable pending_context: unit option;
  mutable next: ('props, 'state, 'host_element, 'host_text, 'host_container) t option;
  mutable callback_node: unit option;
  mutable callback_priority: lane;
  expiration_times: int lane_map;
  hidden_updates: unit list lane_map;
  mutable pending_lanes: lanes;
  mutable suspended_lanes: lanes;
  mutable pinged_lanes: lanes;
  mutable warm_lanes: lanes;
  mutable expired_lanes: lanes;
  mutable indicator_lanes: lanes;
  mutable error_recovery_disabled_lanes: lanes;
  mutable shell_suspend_counter: int;
  mutable entangled_lanes: lanes;
  entanglements: lanes lane_map;
  mutable pooled_cache: cache option;
  mutable pooled_cache_lanes: lanes;
  identifier_prefix: string;
  on_uncaught_error: (exn -> error_info -> unit);
  on_caught_error: (exn -> error_info -> unit);
  on_recoverable_error: (exn -> error_info -> unit);
  on_default_transition_indicator: (unit -> (unit -> unit) option);
  mutable pending_indicator: (unit -> unit) option;
  form_state: unit option;
  hydration_callbacks: suspense_hydration_callbacks option;
  mutable transition_types: unit option;
  mutable pending_gestures: unit option;
  mutable stopping_gestures: unit option;
  mutable gesture_clone: unit option;
  incomplete_transitions: (string, unit) Hashtbl.t;
  transition_callbacks: unit option;
  transition_lanes: unit option lane_map;
  mutable effect_duration: float;
  mutable passive_effect_duration: float;
  memoized_updaters: unit list;
  pending_updaters_lane_map: unit list lane_map;
  debug_root_type: string;
}

(* Create a lane map *)
val create_lane_map : 'a -> 'a lane_map

(* Create a new fiber root *)
val create_fiber_root :
  container_info:'host_container ->
  tag:Root_tags.t ->
  hydrate:bool ->
  initial_children:('props, 'host_element, 'host_text, 'host_container) element option ->
  hydration_callbacks:suspense_hydration_callbacks option ->
  is_strict_mode:bool ->
  identifier_prefix:string ->
  form_state:unit option ->
  on_uncaught_error:(exn -> error_info -> unit) ->
  on_caught_error:(exn -> error_info -> unit) ->
  on_recoverable_error:(exn -> error_info -> unit) ->
  on_default_transition_indicator:(unit -> (unit -> unit) option) ->
  transition_callbacks:unit option ->
  (('props, 'host_element, 'host_text, 'host_container) root_state, 
   ('props, 'host_element, 'host_text, 'host_container) root_state, 
   'host_element, 'host_text, 'host_container) t

(* Mark root as having pending work *)
val mark_root_updated : 
  ('props, 'state, 'host_element, 'host_text, 'host_container) t -> 
  lane -> 
  int -> 
  unit

(* Get next lanes to work on *)
val get_next_lanes : 
  ('props, 'state, 'host_element, 'host_text, 'host_container) t -> 
  lanes

(* Mark root as finished *)
val mark_root_finished : 
  ('props, 'state, 'host_element, 'host_text, 'host_container) t -> 
  lanes -> 
  lanes