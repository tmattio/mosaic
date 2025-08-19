type t = int

val no_flags : t
val has_effect : t
val insertion : t
val layout : t
val passive : t

val has_flag : t -> t -> bool
val add_flag : t -> t -> t
val remove_flag : t -> t -> t