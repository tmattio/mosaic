(** Windows-specific event source.

    Uses native console input, tracks state for mouse, timing for paste,
    auto-manages cleanup. *)

val create :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  mouse:bool ->
  paste_threshold:float ->
  paste_min_chars:int ->
  Terminal.t ->
  Input.event Eio.Stream.t
(** Creates source, sets console mode, spawns producer. Cleanup on switch
    release. *)
