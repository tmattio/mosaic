(** ASCII/TXT renderer for vcr - renders terminal output as plain text *)

type config = {
  separator : string;  (** Separator between frames, default is 80 dashes *)
}
(** Renderer configuration *)

type t
(** Renderer state *)

val default_config : config
(** Default configuration with 80-dash separator like VHS *)

val create : rows:int -> cols:int -> config -> t
(** [create ~rows ~cols config] creates a new ASCII renderer *)

val write_frame :
  t ->
  Renderer_intf.frame ->
  incremental:bool ->
  writer:(bytes -> int -> int -> unit) ->
  unit
(** Write a frame *)

val finalize : t -> writer:(bytes -> int -> int -> unit) -> unit
(** Finalize the ASCII output *)
