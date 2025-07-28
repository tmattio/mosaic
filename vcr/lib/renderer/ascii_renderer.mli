(** ASCII/TXT renderer for vcr - renders terminal output as plain text *)

type config = {
  separator : string;  (** Separator between frames, default is 80 dashes *)
}
(** Renderer configuration *)

type t
(** Renderer state *)

val default_config : config
(** Default configuration with 80-dash separator like VHS *)

val create : Vte.t -> config -> t
(** [create vte config] creates a new ASCII renderer for the given VTE *)

val capture_frame : t -> unit
(** [capture_frame t] captures the current terminal state as a text frame *)

val add_pending_delay : t -> float -> unit
(** [add_pending_delay t delay] adds delay (in seconds) - ignored for ASCII
    renderer *)

val render : t -> string
(** [render t] renders all captured frames as a text string *)
