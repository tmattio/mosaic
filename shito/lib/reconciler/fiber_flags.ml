type t = int

let no_flags                       = 0b0000000000000000000000000000000
let performed_work                 = 0b0000000000000000000000000000001
let placement                      = 0b0000000000000000000000000000010
let did_capture                    = 0b0000000000000000000000010000000
let hydrating                      = 0b0000000000000000001000000000000

let update                         = 0b0000000000000000000000000000100
let cloned                         = 0b0000000000000000000000000001000

let child_deletion                 = 0b0000000000000000000000000010000
let content_reset                  = 0b0000000000000000000000000100000
let callback                       = 0b0000000000000000000000001000000

let force_client_render            = 0b0000000000000000000000100000000
let ref_                           = 0b0000000000000000000001000000000
let snapshot                       = 0b0000000000000000000010000000000
let passive                        = 0b0000000000000000000100000000000

let visibility                     = 0b0000000000000000010000000000000
let store_consistency              = 0b0000000000000000100000000000000

let hydrate = callback
let schedule_retry = store_consistency
let should_suspend_commit = visibility
let view_transition_named_mount = should_suspend_commit
let did_defer = content_reset
let form_reset = snapshot
let affected_parent_layout = content_reset

let lifecycle_effect_mask = 
  passive lor update lor callback lor ref_ lor snapshot lor store_consistency

let host_effect_mask              = 0b0000000000000000111111111111111

let incomplete                     = 0b0000000000000001000000000000000
let should_capture                 = 0b0000000000000010000000000000000
let force_update_for_legacy_suspense = 0b0000000000000100000000000000000
let did_propagate_context          = 0b0000000000001000000000000000000
let needs_propagation              = 0b0000000000010000000000000000000
let forked                         = 0b0000000000100000000000000000000

let snapshot_static                = 0b0000000001000000000000000000000
let layout_static                  = 0b0000000010000000000000000000000
let ref_static = layout_static
let passive_static                 = 0b0000000100000000000000000000000
let may_suspend_commit             = 0b0000001000000000000000000000000
let view_transition_named_static = snapshot_static lor may_suspend_commit
let view_transition_static         = 0b0000010000000000000000000000000

let placement_dev                  = 0b0000100000000000000000000000000
let mount_layout_dev               = 0b0001000000000000000000000000000
let mount_passive_dev              = 0b0010000000000000000000000000000

let before_mutation_mask = snapshot

let before_and_after_mutation_transition_mask =
  snapshot lor update lor placement lor child_deletion lor visibility lor content_reset

let mutation_mask =
  placement lor update lor child_deletion lor content_reset lor ref_ lor hydrating lor visibility lor form_reset

let layout_mask = update lor callback lor ref_ lor visibility

let passive_mask = passive lor visibility lor child_deletion

let passive_transition_mask = passive_mask lor update lor placement

let static_mask =
  layout_static lor passive_static lor ref_static lor may_suspend_commit lor view_transition_static lor view_transition_named_static

let has_flag flags flag = (flags land flag) <> 0
let add_flag flags flag = flags lor flag
let remove_flag flags flag = flags land (lnot flag)
let toggle_flag flags flag = flags lxor flag