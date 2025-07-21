val create :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  mouse:bool ->
  Terminal.t ->
  Input.event Eio.Stream.t
