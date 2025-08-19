(* Lane types from Types module *)
type lane = Types.lane
type lanes = Types.lanes

(* Lane constants *)
val total_lanes : int

val no_lanes : lanes
val no_lane : lane

val sync_hydration_lane : lane
val sync_lane : lane
val sync_lane_index : int

val input_continuous_hydration_lane : lane
val input_continuous_lane : lane

val default_hydration_lane : lane
val default_lane : lane

val sync_update_lanes : lanes
val gesture_lane : lane

val transition_hydration_lane : lane
val transition_lanes : lanes

val retry_lanes : lanes
val some_retry_lane : lane

val selective_hydration_lane : lane

val non_idle_lanes : lanes

val idle_hydration_lane : lane
val idle_lane : lane

val offscreen_lane : lane
val deferred_lane : lane

val update_lanes : lanes
val hydration_lanes : lanes

val no_timestamp : int

(* Lane operations *)
val get_highest_priority_lane : lanes -> int
val get_highest_priority_lanes : lanes -> lanes
val get_lane_index : lane -> int

val is_subset_of_lanes : lanes -> lanes -> bool
val merge_lanes : lanes -> lanes -> lanes
val remove_lanes : lanes -> lanes -> lanes
val intersect_lanes : lanes -> lanes -> lanes

val includes_non_idle_work : lanes -> bool
val includes_only_retries : lanes -> bool
val includes_only_transitions : lanes -> bool
val includes_blocking_lane : lanes -> bool
val includes_expired_lane : lanes -> bool

val is_transition_lane : lane -> bool
val is_retry_lane : lane -> bool
val is_hydration_lane : lane -> bool

val claim_next_transition_lane : unit -> lane
val claim_next_retry_lane : unit -> lane

val get_label_for_lane : lane -> string