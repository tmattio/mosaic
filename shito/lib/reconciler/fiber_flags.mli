type t = int

val no_flags : t
val performed_work : t
val placement : t
val did_capture : t
val hydrating : t
val update : t
val cloned : t
val child_deletion : t
val content_reset : t
val callback : t
val force_client_render : t
val ref_ : t
val snapshot : t
val passive : t
val visibility : t
val store_consistency : t

val hydrate : t
val schedule_retry : t
val should_suspend_commit : t
val view_transition_named_mount : t
val did_defer : t
val form_reset : t
val affected_parent_layout : t

val lifecycle_effect_mask : t
val host_effect_mask : t

val incomplete : t
val should_capture : t
val force_update_for_legacy_suspense : t
val did_propagate_context : t
val needs_propagation : t
val forked : t

val snapshot_static : t
val layout_static : t
val ref_static : t
val passive_static : t
val may_suspend_commit : t
val view_transition_named_static : t
val view_transition_static : t

val placement_dev : t
val mount_layout_dev : t
val mount_passive_dev : t

val before_mutation_mask : t
val before_and_after_mutation_transition_mask : t
val mutation_mask : t
val layout_mask : t
val passive_mask : t
val passive_transition_mask : t
val static_mask : t

val has_flag : t -> t -> bool
val add_flag : t -> t -> t
val remove_flag : t -> t -> t
val toggle_flag : t -> t -> t