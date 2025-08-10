type 'a id = 'a Type.Id.t
type t

val pack : 'a id -> 'a -> t

val unpack : 'b id -> t -> 'b option
(** [unpack id box] returns [Some v] if [box] contains a value of the type
    denoted by [id], otherwise [None]. *)
