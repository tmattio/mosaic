val create :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  mouse:bool ->
  Tty_eio.t ->
  Input.event Eio.Stream.t
