open Types

type ('props, 'state, 'host_element, 'host_text, 'host_container) t = {
  (* Instance fields *)
  mutable tag : Work_tags.t;
  mutable key : key;
  mutable element_type :
    ('props, 'host_element, 'host_text, 'host_container) element_type option;
  mutable typ : ('props, 'host_element, 'host_text, 'host_container) type_;
  mutable state_node :
    ('host_element, 'host_text, 'host_container, 'props) state_node;
  (* Fiber fields *)
  mutable return :
    ('props, 'state, 'host_element, 'host_text, 'host_container) t option;
  mutable child :
    ('props, 'state, 'host_element, 'host_text, 'host_container) t option;
  mutable sibling :
    ('props, 'state, 'host_element, 'host_text, 'host_container) t option;
  mutable index : int;
  mutable ref_ : 'host_element ref_;
  mutable ref_cleanup : (unit -> unit) option;
  mutable pending_props : 'props;
  mutable memoized_props : 'props option;
  mutable update_queue : ('state, 'props) update_queue option;
  mutable memoized_state : ('state, 'props) memoized_state;
  mutable dependencies : dependencies option;
  mutable mode : Type_of_mode.t;
  (* Effects *)
  mutable flags : Fiber_flags.t;
  mutable subtree_flags : Fiber_flags.t;
  mutable deletions :
    ('props, 'state, 'host_element, 'host_text, 'host_container) t list;
  mutable lanes : lanes;
  mutable child_lanes : lanes;
  mutable alternate :
    ('props, 'state, 'host_element, 'host_text, 'host_container) t option;
  (* Profiler timer fields *)
  mutable actual_duration : float;
  mutable actual_start_time : float;
  mutable self_base_duration : float;
  mutable tree_base_duration : float;
  (* Dev mode fields *)
  mutable debug_info : debug_info option;
  mutable debug_owner :
    ('props, 'state, 'host_element, 'host_text, 'host_container) t option;
  mutable debug_stack : exn option;
  mutable debug_task : string option;
  mutable debug_needs_remount : bool;
  mutable debug_hook_types : hook_type list;
}

and ('props, 'host_element, 'host_text, 'host_container) type_ =
  | String_type of string
  | Function_type of
      ('props -> ('props, 'host_element, 'host_text, 'host_container) element)
  | Class_type of
      ('props, 'host_element, 'host_text, 'host_container) class_component_type
  | Forward_ref_type of
      ('props ->
      'host_element ref_ ->
      ('props, 'host_element, 'host_text, 'host_container) element)
  | Memo_type of
      ('props, 'host_element, 'host_text, 'host_container) element_type
  | Lazy_type of
      (unit -> ('props, 'host_element, 'host_text, 'host_container) element)
  | Context_type :
      'a context
      -> ('props, 'host_element, 'host_text, 'host_container) type_
  | Portal_type of 'host_container
  | Fragment_type
  | Suspense_type
  | Profiler_type
  | Other_type

val create_fiber :
  Work_tags.t ->
  'props ->
  key ->
  Type_of_mode.t ->
  ('props, 'state, 'host_element, 'host_text, 'host_container) t

(* Create work in progress fiber from current fiber *)
val create_work_in_progress :
  (('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) t ->
  ('props, 'host_element, 'host_text, 'host_container) fiber_props ->
  (('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) t

(* Create fiber from element *)
val create_fiber_from_element :
  ('props, 'host_element, 'host_text, 'host_container) child ->
  Type_of_mode.t ->
  lanes ->
  (('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) t

(* Create fiber from text *)
val create_fiber_from_text :
  string ->
  Type_of_mode.t ->
  lanes ->
  (('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) t

(* Create fiber from fragment *)
val create_fiber_from_fragment :
  ('props, 'host_element, 'host_text, 'host_container) child list ->
  Type_of_mode.t ->
  lanes ->
  key ->
  (('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) t

(* Create fiber from portal *)
val create_fiber_from_portal :
  children:('props, 'host_element, 'host_text, 'host_container) child list ->
  container_info:'host_container ->
  implementation:unit ->
  key:key ->
  Type_of_mode.t ->
  lanes ->
  (('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) t

(* Check if fiber can be reused *)
val can_reuse_fiber :
  (('props, 'host_element, 'host_text, 'host_container) fiber_props, 'state, 'host_element, 'host_text, 'host_container) t ->
  ('props, 'host_element, 'host_text, 'host_container) child ->
  bool

(* Get element props *)
val get_element_props :
  ('props, 'host_element, 'host_text, 'host_container) child ->
  'props

(* Get element key *)
val get_element_key :
  ('props, 'host_element, 'host_text, 'host_container) element ->
  key
