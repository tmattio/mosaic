type t =
  | Function_component
  | Class_component
  | Host_root
  | Host_portal
  | Host_component
  | Host_text
  | Fragment
  | Mode
  | Context_consumer
  | Context_provider
  | Forward_ref
  | Profiler
  | Suspense_component
  | Memo_component
  | Simple_memo_component
  | Lazy_component
  | Incomplete_class_component
  | Dehydrated_fragment
  | Suspense_list_component
  | Scope_component
  | Offscreen_component
  | Legacy_hidden_component
  | Cache_component
  | Tracing_marker_component
  | Host_hoistable
  | Host_singleton
  | Incomplete_function_component
  | Throw
  | View_transition_component
  | Activity_component

val to_int : t -> int
val of_int : int -> t option