(* Core types for the reconciler using proper OCaml types *)

(* Key type for React elements *)
type key = string option

(* Lanes for priority system *)
type lane = int
type lanes = int

(* Generic ref type - parameterized by host element type *)
type 'host_element ref_ = 
  | Null
  | Callback of ('host_element -> unit)
  | Ref_object of 'host_element option ref

(* Element type - what kind of component this is *)
type ('props, 'host_element, 'host_text, 'host_container) element_type =
  | Host_component of string (* e.g., "div", "span" for DOM *)
  | Host_text
  | Function_component of ('props -> ('props, 'host_element, 'host_text, 'host_container) element)
  | Class_component of ('props, 'host_element, 'host_text, 'host_container) class_component_type
  | Fragment
  | Suspense
  | Lazy of (unit -> ('props, 'host_element, 'host_text, 'host_container) element)
  | Forward_ref of ('props -> 'host_element ref_ -> ('props, 'host_element, 'host_text, 'host_container) element)
  | Memo of ('props, 'host_element, 'host_text, 'host_container) element_type * ('props -> 'props -> bool) option
  | Context_provider : 'a context -> ('props, 'host_element, 'host_text, 'host_container) element_type
  | Context_consumer : 'a context -> ('props, 'host_element, 'host_text, 'host_container) element_type  
  | Portal of 'host_container
  | Profiler
  | Strict_mode
  | Suspense_list
  | Offscreen
  | Legacy_hidden
  | Scope
  | Cache
  | Tracing_marker

and ('props, 'host_element, 'host_text, 'host_container) element = {
  type_: ('props, 'host_element, 'host_text, 'host_container) element_type;
  key: key;
  ref_: 'host_element ref_;
  props: 'props;
}

and 'a context = {
  id: int;
  mutable current_value: 'a;
  mutable default_value: 'a;
}

and ('props, 'host_element, 'host_text, 'host_container) class_component_type = {
  constructor: 'props -> ('props, 'host_element, 'host_text, 'host_container) class_instance;
  default_props: 'props option;
  context_type: unit context option;
}

and ('props, 'host_element, 'host_text, 'host_container) class_instance = {
  mutable props: 'props;
  mutable state: class_state;
  mutable context: class_context;
  mutable refs: (string * 'host_element) list;
  mutable updater: class_updater;
  component: ('props, 'host_element, 'host_text, 'host_container) class_component_type;
}

and class_state = State : 'a -> class_state
and class_context = Context : 'a -> class_context

and class_updater = {
  enqueue_set_state: class_state -> (class_state -> unit) option -> unit;
  enqueue_replace_state: class_state -> (class_state -> unit) option -> unit;
  enqueue_force_update: (class_state -> unit) option -> unit;
}

(* State node - what the fiber represents in the host environment *)
type ('host_element, 'host_text, 'host_container, 'props) state_node =
  | Null_state
  | Host_element_state of 'host_element
  | Host_text_state of 'host_text
  | Host_root_state of 'host_container
  | Class_instance_state : ('props, 'host_element, 'host_text, 'host_container) class_instance -> ('host_element, 'host_text, 'host_container, 'props) state_node
  | Suspense_state
  | Portal_state of 'host_container

(* Memoized state *)
type ('state, 'props) memoized_state =
  | No_state
  | Hook_state of ('state, 'props) hook
  | Class_state of 'state

and ('state, 'props) hook = {
  memoized_state: 'state;
  base_state: 'state;
  base_queue: ('state, 'props) update option;
  queue: ('state, 'props) update_queue option;
  next: ('state, 'props) hook option;
}

(* Update queue types *)
and ('state, 'props) update = {
  lane: lane;
  tag: int; (* 0 = UpdateState, 1 = ReplaceState, 2 = ForceUpdate, 3 = CaptureUpdate *)
  payload: ('state, 'props) update_payload;
  callback: (unit -> unit) option;
  next: ('state, 'props) update option;
}

and ('state, 'props) update_payload =
  | State_update of 'state
  | Function_update of ('state -> 'props -> 'state)

and ('state, 'props) shared_queue = {
  mutable pending: ('state, 'props) update option;
  mutable lanes: lanes;
  mutable hidden_callbacks: (unit -> unit) list;
}

and ('state, 'props) update_queue = {
  mutable base_state: 'state;
  mutable first_base_update: ('state, 'props) update option;
  mutable last_base_update: ('state, 'props) update option;
  mutable shared: ('state, 'props) shared_queue;
  mutable callbacks: (unit -> unit) list;
}

(* Update tags *)
let update_state = 0
let replace_state = 1
let force_update = 2
let capture_update = 3

(* Effect instance - stores the destroy function for effects *)
type effect_instance = {
  mutable destroy: (unit -> unit) option;
}

(* Effect type for hooks like useEffect, useLayoutEffect *)
type 'deps hook_effect = {
  tag: Hook_effect_tags.t;
  inst: effect_instance;
  create: unit -> (unit -> unit) option;
  deps: 'deps option;
  next: 'deps hook_effect option;
}

(* Hook types *)
type hook_type =
  | Use_state
  | Use_reducer
  | Use_context
  | Use_ref
  | Use_effect
  | Use_effect_event
  | Use_insertion_effect
  | Use_layout_effect
  | Use_callback
  | Use_memo
  | Use_imperative_handle
  | Use_debug_value
  | Use_deferred_value
  | Use_transition
  | Use_sync_external_store
  | Use_id
  | Use_cache_refresh
  | Use_optimistic
  | Use_form_state
  | Use_action_state

(* Context dependency *)
type 'a context_dependency = {
  context: 'a context;
  next: context_dependency_any option;
  memoized_value: 'a;
}
and context_dependency_any = Context_dep : 'a context_dependency -> context_dependency_any

(* Dependencies *)
type dependencies = {
  lanes: lanes;
  first_context: context_dependency_any option;
}

(* Debug info *)
type debug_info = {
  component_stack: string;
  source: source option;
}

and source = {
  file_name: string;
  line_number: int;
  column_number: int;
}

(* Child type - represents all possible child values during reconciliation *)
type ('props, 'host_element, 'host_text, 'host_container) child =
  | Child_null
  | Child_boolean of bool
  | Child_string of string
  | Child_number of string  (* Converted to string *)
  | Child_bigint of string  (* Converted to string *)
  | Child_element of ('props, 'host_element, 'host_text, 'host_container) element
  | Child_portal of ('props, 'host_element, 'host_text, 'host_container) portal_child
  | Child_array of ('props, 'host_element, 'host_text, 'host_container) child list
  | Child_list of ('props, 'host_element, 'host_text, 'host_container) child list
  | Child_fragment of ('props, 'host_element, 'host_text, 'host_container) element  (* Fragment element *)
  | Child_thenable of (unit -> ('props, 'host_element, 'host_text, 'host_container) child)
  | Child_context of unit  (* Context consumer *)

and ('props, 'host_element, 'host_text, 'host_container) portal_child = 
  | Portal_child of {
      children: ('props, 'host_element, 'host_text, 'host_container) child list;
      container_info: 'host_container;
      implementation: unit;
      key: key;
    }

(* Props types for different fiber types *)
type ('props, 'host_element, 'host_text, 'host_container) fiber_props =
  | Element_props of 'props
  | Text_props of string
  | Fragment_props of ('props, 'host_element, 'host_text, 'host_container) child list
  | Portal_props of ('props, 'host_element, 'host_text, 'host_container) child list
  | Root_props of ('props, 'host_element, 'host_text, 'host_container) element option