(** Event source abstraction layer for cross-platform input handling *)

type t
(** Abstract event source *)

val create : ?mouse:bool -> Terminal.t -> t
(** Create an event source appropriate for the current platform.
    @param mouse Enable mouse event capture (default: false) *)

val read :
  t ->
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  timeout:float option ->
  [ `Event of Input.event | `Timeout | `Eof ]
(** Read the next event with optional timeout in seconds.

    Platform differences:
    - [`Eof] is typically only returned on Unix-like systems when the input
      stream is closed. This event is not generated on Windows.
    - [`Event (Input.Paste _)] is generated from ANSI bracketed paste sequences
      on Unix. On Windows, paste is detected heuristically based on the timing
      of key events (characters arriving within 10ms of each other).
    - Mouse events require explicit enabling via the [mouse] parameter in
      [create]. *)
