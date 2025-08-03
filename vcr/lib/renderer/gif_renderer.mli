(** GIF renderer for vcr - renders terminal output as animated GIF *)

type theme = {
  bg : int * int * int;
  fg : int * int * int;
  black : int * int * int;
  red : int * int * int;
  green : int * int * int;
  yellow : int * int * int;
  blue : int * int * int;
  magenta : int * int * int;
  cyan : int * int * int;
  white : int * int * int;
  bright_black : int * int * int;
  bright_red : int * int * int;
  bright_green : int * int * int;
  bright_yellow : int * int * int;
  bright_blue : int * int * int;
  bright_magenta : int * int * int;
  bright_cyan : int * int * int;
  bright_white : int * int * int;
}
(** Color theme for terminal rendering *)

val default_theme : theme
(** Default terminal theme *)

type config = {
  char_width : int;  (** Width of a character in pixels *)
  char_height : int;  (** Height of a character in pixels *)
  theme : theme;
  font_path : string option;  (** Path to font file, None for built-in font *)
  font_size : int;  (** Font size in pixels *)
  target_width : int option;  (** Desired output width in pixels *)
  target_height : int option;  (** Desired output height in pixels *)
  padding : int;  (** Padding around terminal content in pixels *)
}
(** Renderer configuration *)

include Renderer_intf.S with type config := config

val set_timing : Vcr_common.Timing.t option -> unit
(** Set global timing tracker for performance monitoring *)
