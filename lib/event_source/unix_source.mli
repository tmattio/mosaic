(** Unix event source using terminal input and ANSI parser *)

type t

val create : Terminal.t -> t

val read :
  t ->
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  timeout:float option ->
  [ `Event of Input.event | `Timeout | `Eof ]
