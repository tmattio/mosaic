(* Lane types from Types module *)
type lane = Types.lane
type lanes = Types.lanes

(* Total number of lanes *)
let total_lanes = 31

(* Lane values - kept in sync with React *)
let no_lanes                      = 0b0000000000000000000000000000000
let no_lane                        = 0b0000000000000000000000000000000

let sync_hydration_lane            = 0b0000000000000000000000000000001
let sync_lane                      = 0b0000000000000000000000000000010
let sync_lane_index = 1

let input_continuous_hydration_lane = 0b0000000000000000000000000000100
let input_continuous_lane          = 0b0000000000000000000000000001000

let default_hydration_lane         = 0b0000000000000000000000000010000
let default_lane                   = 0b0000000000000000000000000100000

let sync_update_lanes = 
  sync_lane lor input_continuous_lane lor default_lane

let gesture_lane                   = 0b0000000000000000000000001000000

let transition_hydration_lane      = 0b0000000000000000000000010000000
let transition_lanes               = 0b0000000001111111111111100000000
let transition_lane_1              = 0b0000000000000000000000100000000
let _transition_lane_2              = 0b0000000000000000000001000000000
let _transition_lane_3              = 0b0000000000000000000010000000000
let _transition_lane_4              = 0b0000000000000000000100000000000
let _transition_lane_5              = 0b0000000000000000001000000000000
let _transition_lane_6              = 0b0000000000000000010000000000000
let _transition_lane_7              = 0b0000000000000000100000000000000
let _transition_lane_8              = 0b0000000000000001000000000000000
let _transition_lane_9              = 0b0000000000000010000000000000000
let _transition_lane_10             = 0b0000000000000100000000000000000
let _transition_lane_11             = 0b0000000000001000000000000000000
let _transition_lane_12             = 0b0000000000010000000000000000000
let _transition_lane_13             = 0b0000000000100000000000000000000
let _transition_lane_14             = 0b0000000001000000000000000000000

let retry_lanes                    = 0b0000011110000000000000000000000
let retry_lane_1                   = 0b0000000010000000000000000000000
let _retry_lane_2                   = 0b0000000100000000000000000000000
let _retry_lane_3                   = 0b0000001000000000000000000000000
let _retry_lane_4                   = 0b0000010000000000000000000000000

let some_retry_lane = retry_lane_1

let selective_hydration_lane       = 0b0000100000000000000000000000000

let non_idle_lanes                 = 0b0000111111111111111111111111111

let idle_hydration_lane            = 0b0001000000000000000000000000000
let idle_lane                      = 0b0010000000000000000000000000000

let offscreen_lane                 = 0b0100000000000000000000000000000
let deferred_lane                  = 0b1000000000000000000000000000000

(* Any lane that might schedule an update *)
let update_lanes = 
  sync_lane lor input_continuous_lane lor default_lane lor transition_lanes

(* All hydration lanes *)
let hydration_lanes =
  sync_hydration_lane lor
  input_continuous_hydration_lane lor
  default_hydration_lane lor
  transition_hydration_lane lor
  selective_hydration_lane lor
  idle_hydration_lane

(* No timestamp constant *)
let no_timestamp = -1

(* Mutable state for lane allocation *)
let next_transition_lane = ref transition_lane_1
let next_retry_lane = ref retry_lane_1

(* Count leading zeros - used for priority calculation *)
let clz32 n =
  if n = 0 then 32
  else
    let rec count n acc =
      if n land 0x80000000 <> 0 then acc
      else count (n lsl 1) (acc + 1)
    in
    count n 0

(* Get the highest priority lane from a set of lanes *)
let get_highest_priority_lane lanes =
  (* This is equivalent to 31 - clz32(lanes) in JS *)
  31 - clz32 lanes

(* Get the index (rightmost bit position) of a lane *)
let get_lane_index lane =
  31 - clz32 lane

(* Get highest priority lanes *)
let get_highest_priority_lanes lanes =
  let pending_sync_lanes = lanes land sync_update_lanes in
  if pending_sync_lanes <> 0 then
    pending_sync_lanes
  else
    let highest_lane = 1 lsl get_highest_priority_lane lanes in
    match highest_lane with
    | n when n = sync_hydration_lane -> sync_hydration_lane
    | n when n = sync_lane -> sync_lane
    | n when n = input_continuous_hydration_lane -> input_continuous_hydration_lane
    | n when n = input_continuous_lane -> input_continuous_lane
    | n when n = default_hydration_lane -> default_hydration_lane
    | n when n = default_lane -> default_lane
    | n when n = gesture_lane -> gesture_lane
    | n when n = transition_hydration_lane -> transition_hydration_lane
    | n when n land transition_lanes <> 0 -> lanes land transition_lanes
    | n when n land retry_lanes <> 0 -> lanes land retry_lanes
    | n when n = selective_hydration_lane -> selective_hydration_lane
    | n when n = idle_hydration_lane -> idle_hydration_lane
    | n when n = idle_lane -> idle_lane
    | n when n = offscreen_lane -> offscreen_lane
    | n when n = deferred_lane -> deferred_lane
    | _ -> lanes

(* Check if a set of lanes is a subset of another *)
let is_subset_of_lanes a b =
  (a land b) = a

(* Merge lanes *)
let merge_lanes a b = a lor b

(* Remove lanes *)
let remove_lanes set subset =
  set land (lnot subset)

(* Intersect lanes *)
let intersect_lanes a b = a land b

(* Check if lanes are non-idle *)
let includes_non_idle_work lanes =
  (lanes land non_idle_lanes) <> 0

(* Check if lanes include only retries *)
let includes_only_retries lanes =
  (lanes land retry_lanes) = lanes

(* Check if lanes include only transitions *)
let includes_only_transitions lanes =
  (lanes land transition_lanes) = lanes

(* Check if blocking lanes are included *)
let includes_blocking_lane lanes =
  (lanes land (sync_lane lor input_continuous_lane lor default_lane)) <> 0

(* Check if lanes include expired work *)
let includes_expired_lane lanes =
  (lanes land sync_lane) <> 0

(* Check if a lane is a transition lane *)
let is_transition_lane lane =
  (lane land transition_lanes) <> 0

(* Check if a lane is a retry lane *)
let is_retry_lane lane =
  (lane land retry_lanes) <> 0

(* Check if a lane is a hydration lane *)
let is_hydration_lane lane =
  (lane land hydration_lanes) <> 0

(* Get next transition lane *)
let claim_next_transition_lane () =
  let lane = !next_transition_lane in
  next_transition_lane := lane lsl 1;
  if !next_transition_lane land transition_lanes = 0 then
    next_transition_lane := transition_lane_1;
  lane

(* Get next retry lane *)
let claim_next_retry_lane () =
  let lane = !next_retry_lane in
  next_retry_lane := lane lsl 1;
  if !next_retry_lane land retry_lanes = 0 then
    next_retry_lane := retry_lane_1;
  lane

(* Get label for lane - useful for debugging *)
let get_label_for_lane lane =
  if lane land sync_hydration_lane <> 0 then "SyncHydrationLane"
  else if lane land sync_lane <> 0 then "Sync"
  else if lane land input_continuous_hydration_lane <> 0 then "InputContinuousHydration"
  else if lane land input_continuous_lane <> 0 then "InputContinuous"
  else if lane land default_hydration_lane <> 0 then "DefaultHydration"
  else if lane land default_lane <> 0 then "Default"
  else if lane land transition_hydration_lane <> 0 then "TransitionHydration"
  else if lane land transition_lanes <> 0 then "Transition"
  else if lane land retry_lanes <> 0 then "Retry"
  else if lane land selective_hydration_lane <> 0 then "SelectiveHydration"
  else if lane land idle_hydration_lane <> 0 then "IdleHydration"
  else if lane land idle_lane <> 0 then "Idle"
  else if lane land offscreen_lane <> 0 then "Offscreen"
  else if lane land deferred_lane <> 0 then "Deferred"
  else "Unknown"