open Types

(* Child reconciliation - the heart of React's diffing algorithm *)

(* Reconcile children with side effect tracking (for updates) *)
val reconcile_child_fibers :
  return_fiber:(('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t ->
  current_first_child:(('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t option ->
  new_child:('props, 'host_element, 'host_text, 'host_container) child ->
  lanes:lanes ->
  (('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t option

(* Mount children without side effect tracking (for initial mount) *)
val mount_child_fibers :
  return_fiber:(('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t ->
  current_first_child:(('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t option ->
  new_child:('props, 'host_element, 'host_text, 'host_container) child ->
  lanes:lanes ->
  (('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t option

(* Clone existing children *)
val clone_child_fibers :
  current:(('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t option ->
  work_in_progress:(('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) Fiber.t ->
  unit

(* Reset thenable state on unwind *)
val reset_child_reconciler_on_unwind : unit -> unit