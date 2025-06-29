(** Windows event source using ReadConsoleInput *)

type t

val create : mouse:bool -> Terminal.t -> t

val read :
  t -> timeout:float option -> [ `Event of Input.event | `Timeout | `Eof ]
