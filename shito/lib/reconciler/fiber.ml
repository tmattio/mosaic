open Types

(* The fiber type is parameterized by:
   - 'props: the type of props
   - 'state: the type of component state
   - 'host_element: the type of host elements (e.g., DOM elements)
   - 'host_text: the type of host text nodes
   - 'host_container: the type of host containers
*)
type ('props, 'state, 'host_element, 'host_text, 'host_container) t = {
  (* Instance fields *)
  mutable tag: Work_tags.t;
  mutable key: key;
  mutable element_type: ('props, 'host_element, 'host_text, 'host_container) element_type option;
  mutable typ: ('props, 'host_element, 'host_text, 'host_container) type_;
  mutable state_node: ('host_element, 'host_text, 'host_container, 'props) state_node;

  (* Fiber fields *)
  mutable return: ('props, 'state, 'host_element, 'host_text, 'host_container) t option;
  mutable child: ('props, 'state, 'host_element, 'host_text, 'host_container) t option;
  mutable sibling: ('props, 'state, 'host_element, 'host_text, 'host_container) t option;
  mutable index: int;

  mutable ref_: 'host_element ref_;
  mutable ref_cleanup: (unit -> unit) option;

  mutable pending_props: 'props;
  mutable memoized_props: 'props option;
  mutable update_queue: ('state, 'props) update_queue option;
  mutable memoized_state: ('state, 'props) memoized_state;
  mutable dependencies: dependencies option;

  mutable mode: Type_of_mode.t;

  (* Effects *)
  mutable flags: Fiber_flags.t;
  mutable subtree_flags: Fiber_flags.t;
  mutable deletions: ('props, 'state, 'host_element, 'host_text, 'host_container) t list;

  mutable lanes: lanes;
  mutable child_lanes: lanes;

  mutable alternate: ('props, 'state, 'host_element, 'host_text, 'host_container) t option;

  (* Profiler timer fields *)
  mutable actual_duration: float;
  mutable actual_start_time: float;
  mutable self_base_duration: float;
  mutable tree_base_duration: float;

  (* Dev mode fields *)
  mutable debug_info: debug_info option;
  mutable debug_owner: ('props, 'state, 'host_element, 'host_text, 'host_container) t option;
  mutable debug_stack: exn option;
  mutable debug_task: string option;
  mutable debug_needs_remount: bool;
  mutable debug_hook_types: hook_type list;
}

(* Type for what a fiber represents *)
and ('props, 'host_element, 'host_text, 'host_container) type_ =
  | String_type of string (* for host components *)
  | Function_type of ('props -> ('props, 'host_element, 'host_text, 'host_container) element)
  | Class_type of ('props, 'host_element, 'host_text, 'host_container) class_component_type
  | Forward_ref_type of ('props -> 'host_element ref_ -> ('props, 'host_element, 'host_text, 'host_container) element)
  | Memo_type of ('props, 'host_element, 'host_text, 'host_container) element_type
  | Lazy_type of (unit -> ('props, 'host_element, 'host_text, 'host_container) element)
  | Context_type : 'a context -> ('props, 'host_element, 'host_text, 'host_container) type_
  | Portal_type of 'host_container
  | Fragment_type
  | Suspense_type
  | Profiler_type
  | Other_type

let create_fiber tag pending_props key mode = {
  tag;
  key;
  element_type = None;
  typ = Other_type;
  state_node = Null_state;

  return = None;
  child = None;
  sibling = None;
  index = 0;

  ref_ = Null;
  ref_cleanup = None;

  pending_props;
  memoized_props = None;
  update_queue = None;
  memoized_state = No_state;
  dependencies = None;

  mode;

  flags = Fiber_flags.no_flags;
  subtree_flags = Fiber_flags.no_flags;
  deletions = [];

  lanes = 0; (* NoLanes *)
  child_lanes = 0; (* NoLanes *)

  alternate = None;

  actual_duration = 0.0;
  actual_start_time = -1.1;
  self_base_duration = 0.0;
  tree_base_duration = 0.0;

  debug_info = None;
  debug_owner = None;
  debug_stack = None;
  debug_task = None;
  debug_needs_remount = false;
  debug_hook_types = [];
}

(* Create work in progress from current fiber *)
let create_work_in_progress current pending_props =
  (* For now, create a new fiber with the same configuration *)
  let work_in_progress = create_fiber current.tag pending_props current.key current.mode in
  work_in_progress.element_type <- current.element_type;
  work_in_progress.typ <- current.typ;
  work_in_progress.state_node <- current.state_node;
  work_in_progress.memoized_props <- current.memoized_props;
  work_in_progress.memoized_state <- current.memoized_state;
  work_in_progress.update_queue <- current.update_queue;
  work_in_progress.dependencies <- current.dependencies;
  work_in_progress.alternate <- Some current;
  work_in_progress

(* Create fiber from element *)
let create_fiber_from_element child mode lanes =
  match child with
  | Child_element element ->
    let tag = match element.type_ with
      | Host_component _ -> Work_tags.Host_component
      | Host_text -> Work_tags.Host_text
      | Function_component _ -> Work_tags.Function_component
      | Class_component _ -> Work_tags.Class_component
      | Fragment -> Work_tags.Fragment
      | Suspense -> Work_tags.Suspense_component
      | Portal _ -> Work_tags.Host_portal
      | _ -> Work_tags.Function_component  (* Default for unknown types *)
    in
    (* Wrap element props in Element_props *)
    let props = Element_props element.props in
    let fiber = create_fiber tag props element.key mode in
    (* Store element type and typ - these will have mismatched types but that's OK
       as we control how they're used and this matches React's design *)
    (* fiber.element_type <- Some element.type_; -- skip for now due to type issues *)
    fiber.typ <- (match element.type_ with
      | Host_component name -> String_type name
      | Fragment -> Fragment_type
      | Suspense -> Suspense_type
      | Portal container -> Portal_type container
      | _ -> Other_type);
    fiber.lanes <- lanes;
    fiber
  | _ -> failwith "Expected Child_element"

(* Create fiber from text *)
let create_fiber_from_text content mode lanes =
  (* Text fibers wrap the content in Text_props *)
  let props = Text_props content in
  let fiber = create_fiber Work_tags.Host_text props None mode in
  fiber.lanes <- lanes;
  fiber

(* Create fiber from fragment *)
let create_fiber_from_fragment children mode lanes key =
  (* Fragment fibers wrap children in Fragment_props *)
  let props = Fragment_props children in
  let fiber = create_fiber Work_tags.Fragment props key mode in
  fiber.lanes <- lanes;
  fiber

(* Create fiber from portal *)
let create_fiber_from_portal ~children ~container_info ~implementation:_ ~key mode lanes =
  (* Portal fibers wrap children in Portal_props *)
  let props = Portal_props children in
  let fiber = create_fiber Work_tags.Host_portal props key mode in
  (* Store the portal state information *)
  fiber.state_node <- Portal_state container_info;
  fiber.lanes <- lanes;
  fiber

(* Check if fiber can be reused *)
let can_reuse_fiber existing child =
  match child with
  | Child_element element ->
    (* For now, just check tag and key match since element_type has type issues *)
    let tag_matches = match element.type_ with
      | Host_component _ -> existing.tag = Work_tags.Host_component
      | Function_component _ -> existing.tag = Work_tags.Function_component  
      | Class_component _ -> existing.tag = Work_tags.Class_component
      | Fragment -> existing.tag = Work_tags.Fragment
      | Suspense -> existing.tag = Work_tags.Suspense_component
      | Portal _ -> existing.tag = Work_tags.Host_portal
      | _ -> false
    in
    tag_matches && existing.key = element.key
  | _ -> false

(* Get element props *)
let get_element_props child =
  match child with
  | Child_element element -> element.props
  | _ -> failwith "Expected Child_element"

(* Get element key *)
let get_element_key (element : ('props, 'host_element, 'host_text, 'host_container) element) = element.key