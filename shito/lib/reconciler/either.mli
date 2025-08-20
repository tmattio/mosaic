(** Either type used to pass host instances (element vs text). *)

type ('a, 'b) t = Left of 'a | Right of 'b

val is_left : ('a, 'b) t -> bool
val is_right : ('a, 'b) t -> bool
val map_left : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
val map_right : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val bimap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
val fold : left:('a -> 'r) -> right:('b -> 'r) -> ('a, 'b) t -> 'r
val swap : ('a, 'b) t -> ('b, 'a) t
val to_result : ('a, 'b) t -> ('a, 'b) result
val of_result : ('a, 'b) result -> ('a, 'b) t
val to_option_left : ('a, 'b) t -> 'a option
val to_option_right : ('a, 'b) t -> 'b option

val pp :
  pp_left:(Format.formatter -> 'a -> unit) ->
  pp_right:(Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  ('a, 'b) t ->
  unit
