(** SVG renderer for vcr using Vg library *)

type theme = {
  fg : Gg.color;
  bg : Gg.color;
  black : Gg.color;
  red : Gg.color;
  green : Gg.color;
  yellow : Gg.color;
  blue : Gg.color;
  magenta : Gg.color;
  cyan : Gg.color;
  white : Gg.color;
  bright_black : Gg.color;
  bright_red : Gg.color;
  bright_green : Gg.color;
  bright_yellow : Gg.color;
  bright_blue : Gg.color;
  bright_magenta : Gg.color;
  bright_cyan : Gg.color;
  bright_white : Gg.color;
}
(** Color theme for terminal rendering *)

val default_theme : theme
(** Default terminal theme *)

type config = {
  font_family : string;
  font_size : float;
  line_height : float;
  theme : theme;
}
(** Renderer configuration *)

type t
(** Renderer state *)

val create : Vte.t -> config -> t
(** [create vte config] creates a new SVG renderer for the given VTE *)

val capture_frame : t -> unit
(** [capture_frame t] captures the current terminal state as a frame *)

val add_pending_delay : t -> float -> unit
(** [add_pending_delay t delay] adds delay (in seconds) - ignored for SVG
    renderer *)

val render : t -> string
(** [render t] renders all captured frames as an SVG image *)
