open Types

(* Root state for hydration and SSR *)
type ('props, 'host_element, 'host_text, 'host_container) root_state = {
  element: ('props, 'host_element, 'host_text, 'host_container) element option;
  is_dehydrated: bool;
  cache: cache option;
}

(* Cache type - simplified for now *)
and cache = {
  controller: unit;
  data: (string, unit) Hashtbl.t;
  refcount: int;
}

(* Lane map - array indexed by lane *)
type 'a lane_map = 'a array

(* Error info for error handlers *)
type error_info = {
  component_stack: string option;
  error_boundary: unit option; (* Will be fiber option when we have error boundaries *)
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
  (* The type of root (legacy, concurrent, etc.) *)
  tag: Root_tags.t;
  
  (* Host container info *)
  container_info: 'host_container;
  
  (* Used only by persistent updates *)
  mutable pending_children: unit option;
  
  (* The currently active root fiber *)
  mutable current: ('props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t;
  
  (* Ping cache for Suspense *)
  ping_cache: (unit, unit) Hashtbl.t option;
  
  (* Timeout handle for canceling scheduled work *)
  mutable timeout_handle: int option;
  
  (* Cancel pending commit *)
  mutable cancel_pending_commit: (unit -> unit) option;
  
  (* Context for subtree rendering *)
  mutable context: unit option;
  mutable pending_context: unit option;
  
  (* Linked list of roots with pending work *)
  mutable next: ('props, 'state, 'host_element, 'host_text, 'host_container) t option;
  
  (* Scheduler callback *)
  mutable callback_node: unit option;
  mutable callback_priority: lane;
  
  (* Lane maps *)
  expiration_times: int lane_map;
  hidden_updates: unit list lane_map;
  
  (* Lane tracking *)
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
  
  (* Cache pooling *)
  mutable pooled_cache: cache option;
  mutable pooled_cache_lanes: lanes;
  
  (* Identifier prefix for useId *)
  identifier_prefix: string;
  
  (* Error handlers *)
  on_uncaught_error: (exn -> error_info -> unit);
  on_caught_error: (exn -> error_info -> unit);
  on_recoverable_error: (exn -> error_info -> unit);
  on_default_transition_indicator: (unit -> (unit -> unit) option);
  
  (* Transition indicator *)
  mutable pending_indicator: (unit -> unit) option;
  
  (* Form state *)
  form_state: unit option;
  
  (* Hydration callbacks for Suspense *)
  hydration_callbacks: suspense_hydration_callbacks option;
  
  (* Transition types *)
  mutable transition_types: unit option;
  
  (* Gesture transitions *)
  mutable pending_gestures: unit option;
  mutable stopping_gestures: unit option;
  mutable gesture_clone: unit option;
  
  (* Incomplete transitions tracking *)
  incomplete_transitions: (string, unit) Hashtbl.t;
  
  (* Transition callbacks *)
  transition_callbacks: unit option;
  transition_lanes: unit option lane_map;
  
  (* Profiler timings *)
  mutable effect_duration: float;
  mutable passive_effect_duration: float;
  
  (* Updater tracking *)
  memoized_updaters: unit list;
  pending_updaters_lane_map: unit list lane_map;
  
  (* Debug info *)
  debug_root_type: string;
}

(* Create a lane map initialized with a default value *)
let create_lane_map default_value =
  Array.make Lanes.total_lanes default_value

(* Create a new fiber root *)
let create_fiber_root 
    ~(container_info : 'host_container)
    ~tag 
    ~hydrate 
    ~(initial_children : ('props, 'host_element, 'host_text, 'host_container) element option)
    ~hydration_callbacks 
    ~is_strict_mode 
    ~identifier_prefix 
    ~form_state
    ~on_uncaught_error
    ~on_caught_error
    ~on_recoverable_error
    ~on_default_transition_indicator
    ~transition_callbacks 
    : (('props, 'host_element, 'host_text, 'host_container) root_state, 
       ('props, 'host_element, 'host_text, 'host_container) root_state, 
       'host_element, 'host_text, 'host_container) t =
  
  (* Create the root fiber *)
  let mode = 
    if is_strict_mode then 
      Type_of_mode.strict_legacy_mode 
    else 
      Type_of_mode.no_mode in
  
  (* Root fiber has a special props type - the root state *)
  let root_props : ('props, 'host_element, 'host_text, 'host_container) root_state = {
    element = initial_children;
    is_dehydrated = hydrate;
    cache = None;
  } in
  let root_fiber : (('props, 'host_element, 'host_text, 'host_container) root_state, 
                    ('props, 'host_element, 'host_text, 'host_container) root_state, 
                    'host_element, 'host_text, 'host_container) Fiber.t = 
    Fiber.create_fiber Work_tags.Host_root root_props None mode in
  
  (* Create the fiber root *)
  let fiber_root = {
    tag;
    container_info;
    pending_children = None;
    current = root_fiber;
    ping_cache = if hydrate then Some (Hashtbl.create 16) else None;
    timeout_handle = None;
    cancel_pending_commit = None;
    context = None;
    pending_context = None;
    next = None;
    callback_node = None;
    callback_priority = Lanes.no_lane;
    expiration_times = create_lane_map Lanes.no_timestamp;
    hidden_updates = create_lane_map [];
    pending_lanes = Lanes.no_lanes;
    suspended_lanes = Lanes.no_lanes;
    pinged_lanes = Lanes.no_lanes;
    warm_lanes = Lanes.no_lanes;
    expired_lanes = Lanes.no_lanes;
    indicator_lanes = Lanes.no_lanes;
    error_recovery_disabled_lanes = Lanes.no_lanes;
    shell_suspend_counter = 0;
    entangled_lanes = Lanes.no_lanes;
    entanglements = create_lane_map Lanes.no_lanes;
    pooled_cache = None;
    pooled_cache_lanes = Lanes.no_lanes;
    identifier_prefix;
    on_uncaught_error;
    on_caught_error;
    on_recoverable_error;
    on_default_transition_indicator;
    pending_indicator = None;
    form_state;
    hydration_callbacks;
    transition_types = None;
    pending_gestures = None;
    stopping_gestures = None;
    gesture_clone = None;
    incomplete_transitions = Hashtbl.create 16;
    transition_callbacks;
    transition_lanes = create_lane_map None;
    effect_duration = 0.0;
    passive_effect_duration = 0.0;
    memoized_updaters = [];
    pending_updaters_lane_map = create_lane_map [];
    debug_root_type = 
      if hydrate then 
        (match tag with
        | Concurrent_root -> "hydrateRoot()"
        | Legacy_root -> "hydrate()")
      else
        (match tag with
        | Concurrent_root -> "createRoot()"
        | Legacy_root -> "render()");
  } in
  
  (* Set up bidirectional reference *)
  root_fiber.state_node <- Host_root_state container_info;
  
  (* Initialize update queue for root fiber *)
  Update_queue.initialize_update_queue root_fiber root_props;
  
  fiber_root

(* Mark root as having pending work *)
let mark_root_updated fiber_root lane event_time =
  fiber_root.pending_lanes <- Lanes.merge_lanes fiber_root.pending_lanes lane;
  
  (* Update expiration time for this lane *)
  if lane <> Lanes.idle_lane then
    let lane_index = Lanes.get_lane_index lane in
    fiber_root.expiration_times.(lane_index) <- event_time

(* Get next lanes to work on *)
let get_next_lanes fiber_root =
  let pending_lanes = fiber_root.pending_lanes in
  
  if pending_lanes = Lanes.no_lanes then
    Lanes.no_lanes
  else
    (* Get highest priority lanes *)
    let next_lanes = Lanes.get_highest_priority_lanes pending_lanes in
    
    (* Check for expired work *)
    if fiber_root.expired_lanes <> Lanes.no_lanes then
      Lanes.merge_lanes next_lanes fiber_root.expired_lanes
    else
      next_lanes

(* Mark root as finished *)
let mark_root_finished fiber_root remaining_lanes =
  let no_longer_pending_lanes = 
    Lanes.remove_lanes fiber_root.pending_lanes remaining_lanes in
  
  fiber_root.pending_lanes <- remaining_lanes;
  
  (* Clear expiration times for finished lanes *)
  Array.iteri (fun i _ ->
    let lane = 1 lsl i in
    if Lanes.includes_blocking_lane lane && 
       not (Lanes.is_subset_of_lanes lane remaining_lanes) then
      fiber_root.expiration_times.(i) <- Lanes.no_timestamp
  ) fiber_root.expiration_times;
  
  no_longer_pending_lanes