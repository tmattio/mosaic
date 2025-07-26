(** Padding specifications for spacing around content. *)

type t = { top : int; right : int; bottom : int; left : int }
(** Non-negative padding values per side. *)

val make : ?top:int -> ?right:int -> ?bottom:int -> ?left:int -> unit -> t
(** Create padding. Defaults to 0, enforces non-negative. *)

val no_padding : t
(** All sides 0. *)

val all : int -> t
(** Uniform padding on all sides. *)

val xy : int -> int -> t
(** Horizontal (x) and vertical (y) padding. *)

val top : t -> int
val right : t -> int
val bottom : t -> int

val left : t -> int
(** Accessors. *)

val pad :
  ?all:int ->
  ?x:int ->
  ?y:int ->
  ?top:int ->
  ?right:int ->
  ?bottom:int ->
  ?left:int ->
  unit ->
  t
(** Shorthand creation with cascading defaults. *)
