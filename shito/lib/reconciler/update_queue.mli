open Types

(* Initialize an update queue for a fiber with initial state *)
val initialize_update_queue : 
  ('props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t -> 
  'state -> 
  unit

(* Clone an update queue from current fiber to work-in-progress fiber *)
val clone_update_queue : 
  ('props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t -> 
  ('props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t -> 
  unit

(* Create a new update *)
val create_update : 
  lane:lane -> 
  tag:int -> 
  payload:('state, 'props) update_payload -> 
  callback:(unit -> unit) option -> 
  ('state, 'props) update

(* Enqueue an update to a fiber's update queue *)
val enqueue_update : 
  ('props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t -> 
  ('state, 'props) update -> 
  lane -> 
  unit

(* Process the update queue during render phase *)
val process_update_queue : 
  ('props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t -> 
  lanes -> 
  unit

(* Check if there are pending updates *)
val has_pending_updates : 
  ('props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t -> 
  bool

(* Get pending update lanes *)
val get_pending_lanes : 
  ('props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t -> 
  lanes