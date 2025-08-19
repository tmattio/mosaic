open Types

(* Initialize an update queue for a fiber *)
let initialize_update_queue fiber initial_state =
  let queue = {
    base_state = initial_state;
    first_base_update = None;
    last_base_update = None;
    shared = {
      pending = None;
      lanes = Lanes.no_lanes;
      hidden_callbacks = [];
    };
    callbacks = [];
  } in
  fiber.Fiber.update_queue <- Some queue

(* Clone an update queue from current fiber to work-in-progress fiber *)
let clone_update_queue current work_in_progress =
  match work_in_progress.Fiber.update_queue with
  | Some queue when queue == Option.get current.Fiber.update_queue ->
      (* Clone the queue if it's the same reference *)
      let current_queue = Option.get current.Fiber.update_queue in
      let clone = {
        base_state = current_queue.base_state;
        first_base_update = current_queue.first_base_update;
        last_base_update = current_queue.last_base_update;
        shared = current_queue.shared; (* Shared is not cloned *)
        callbacks = current_queue.callbacks;
      } in
      work_in_progress.Fiber.update_queue <- Some clone
  | _ -> ()

(* Create a new update *)
let create_update ~lane ~tag ~payload ~callback =
  {
    lane;
    tag;
    payload;
    callback;
    next = None;
  }

(* Enqueue an update to both the current and work-in-progress queues *)
let enqueue_update fiber update lane =
  match fiber.Fiber.update_queue with
  | None -> ()
  | Some update_queue ->
      let shared_queue = update_queue.shared in
      
      (* Append the update to the end of the pending list *)
      let pending = shared_queue.pending in
      shared_queue.pending <- 
        (match pending with
        | None ->
            (* Create circular list *)
            let rec circular_update = {
              lane = update.lane;
              tag = update.tag;
              payload = update.payload;
              callback = update.callback;
              next = Some circular_update;
            } in
            Some circular_update
        | Some pending_update ->
            (* Insert into circular list *)
            let new_update = {
              update with next = pending_update.next
            } in
            let updated_pending = {
              pending_update with next = Some new_update
            } in
            Some updated_pending);
      
      (* Update lanes *)
      shared_queue.lanes <- Lanes.merge_lanes shared_queue.lanes lane

(* Process the update queue during render phase *)
let process_update_queue fiber render_lanes =
  match fiber.Fiber.update_queue with
  | None -> ()
  | Some queue ->
      let first_base_update = queue.first_base_update in
      let last_base_update = queue.last_base_update in
      let pending_queue = queue.shared.pending in
      
      if pending_queue <> None then
        (* Transfer pending updates to base queue *)
        let pending = Option.get pending_queue in
        queue.shared.pending <- None;
        
        (* Break circular list *)
        let last_pending = pending in
        let first_pending = 
          match pending.next with
          | Some next -> next
          | None -> pending in
        let last_pending = { last_pending with next = None } in
        
        (* Append to base queue *)
        (match last_base_update with
        | None ->
            queue.first_base_update <- Some first_pending;
            queue.last_base_update <- Some last_pending
        | Some last ->
            queue.last_base_update <- Some { last with next = Some first_pending };
            queue.last_base_update <- Some last_pending);
      
      (* Process updates *)
      if first_base_update <> None then
        let rec process_updates update new_state =
          match update with
          | None -> new_state
          | Some u ->
              if Lanes.is_subset_of_lanes u.lane render_lanes then
                (* Process this update *)
                let next_state = 
                  match u.payload with
                  | State_update state -> state
                  | Function_update f -> f new_state fiber.Fiber.pending_props
                in
                
                (* Add callback if present *)
                (match u.callback with
                | Some callback -> 
                    queue.callbacks <- callback :: queue.callbacks
                | None -> ());
                
                process_updates u.next next_state
              else
                (* Skip this update - insufficient priority *)
                process_updates u.next new_state
        in
        
        let new_state = process_updates first_base_update queue.base_state in
        fiber.Fiber.memoized_state <- Class_state new_state

(* Check if there are pending updates *)
let has_pending_updates fiber =
  match fiber.Fiber.update_queue with
  | None -> false
  | Some queue -> queue.shared.pending <> None

(* Get pending update lanes *)
let get_pending_lanes fiber =
  match fiber.Fiber.update_queue with
  | None -> Lanes.no_lanes
  | Some queue -> queue.shared.lanes