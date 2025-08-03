(** Spacing specifications for spacing around content. *)

type t = { top : int; right : int; bottom : int; left : int }
(** Non-negative spacing values per side. *)

val make : ?top:int -> ?right:int -> ?bottom:int -> ?left:int -> unit -> t
(** Create spacing. Defaults to 0, enforces non-negative. *)

val no_spacing : t
(** All sides 0. *)

val all : int -> t
(** Uniform spacing on all sides. *)

val xy : int -> int -> t
(** Horizontal (x) and vertical (y) spacing. *)

(** Accessors. *)

val top : t -> int
val right : t -> int
val bottom : t -> int
val left : t -> int
