type t = int

let no_mode               = 0b0000000
let concurrent_mode       = 0b0000001
let profile_mode          = 0b0000010
let strict_legacy_mode    = 0b0001000
let strict_effects_mode   = 0b0010000
let suspensey_images_mode = 0b0100000

let has_mode mode flag = (mode land flag) <> 0
let add_mode mode flag = mode lor flag
let remove_mode mode flag = mode land (lnot flag)