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

include Renderer_intf.S with type config := config
