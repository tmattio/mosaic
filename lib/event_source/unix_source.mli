(** Unix event source using terminal input and ANSI parser *)

type t

val create : Terminal.t -> t

val read :
  t -> timeout:float option -> [ `Event of Input.event | `Timeout | `Eof ]
