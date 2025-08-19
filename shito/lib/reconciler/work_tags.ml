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

let to_int = function
  | Function_component -> 0
  | Class_component -> 1
  | Host_root -> 3
  | Host_portal -> 4
  | Host_component -> 5
  | Host_text -> 6
  | Fragment -> 7
  | Mode -> 8
  | Context_consumer -> 9
  | Context_provider -> 10
  | Forward_ref -> 11
  | Profiler -> 12
  | Suspense_component -> 13
  | Memo_component -> 14
  | Simple_memo_component -> 15
  | Lazy_component -> 16
  | Incomplete_class_component -> 17
  | Dehydrated_fragment -> 18
  | Suspense_list_component -> 19
  | Scope_component -> 21
  | Offscreen_component -> 22
  | Legacy_hidden_component -> 23
  | Cache_component -> 24
  | Tracing_marker_component -> 25
  | Host_hoistable -> 26
  | Host_singleton -> 27
  | Incomplete_function_component -> 28
  | Throw -> 29
  | View_transition_component -> 30
  | Activity_component -> 31

let of_int = function
  | 0 -> Some Function_component
  | 1 -> Some Class_component
  | 3 -> Some Host_root
  | 4 -> Some Host_portal
  | 5 -> Some Host_component
  | 6 -> Some Host_text
  | 7 -> Some Fragment
  | 8 -> Some Mode
  | 9 -> Some Context_consumer
  | 10 -> Some Context_provider
  | 11 -> Some Forward_ref
  | 12 -> Some Profiler
  | 13 -> Some Suspense_component
  | 14 -> Some Memo_component
  | 15 -> Some Simple_memo_component
  | 16 -> Some Lazy_component
  | 17 -> Some Incomplete_class_component
  | 18 -> Some Dehydrated_fragment
  | 19 -> Some Suspense_list_component
  | 21 -> Some Scope_component
  | 22 -> Some Offscreen_component
  | 23 -> Some Legacy_hidden_component
  | 24 -> Some Cache_component
  | 25 -> Some Tracing_marker_component
  | 26 -> Some Host_hoistable
  | 27 -> Some Host_singleton
  | 28 -> Some Incomplete_function_component
  | 29 -> Some Throw
  | 30 -> Some View_transition_component
  | 31 -> Some Activity_component
  | _ -> None