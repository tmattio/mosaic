type t = int

val no_mode : t
val concurrent_mode : t
val profile_mode : t
val strict_legacy_mode : t
val strict_effects_mode : t
val suspensey_images_mode : t

val has_mode : t -> t -> bool
val add_mode : t -> t -> t
val remove_mode : t -> t -> t