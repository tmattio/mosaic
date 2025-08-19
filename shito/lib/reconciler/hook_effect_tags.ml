type t = int

let no_flags  = 0b0000
let has_effect = 0b0001
let insertion = 0b0010
let layout    = 0b0100
let passive   = 0b1000

let has_flag flags flag = (flags land flag) <> 0
let add_flag flags flag = flags lor flag
let remove_flag flags flag = flags land (lnot flag)