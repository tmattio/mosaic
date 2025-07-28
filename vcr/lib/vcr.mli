(** VCR - Terminal recording from tape scripts *)

type config = {
  shell : string;
  width : int;
  height : int;
  typing_speed : float;
  font_size : int;
  font_family : string option;
  padding : int;
  framerate : int;
  playback_speed : float;
  theme : string option;
  letter_spacing : float option;
  line_height : float option;
  loop_offset : float option;
  border_radius : int option;
  margin : int option;
  margin_fill : string option;
  cursor_blink : bool;
  window_bar : bool option;
  window_bar_size : int option;
}
(** Configuration for VCR *)

val default_config : config

(** Renderer types *)
type renderer_type =
  | SVG of {
      renderer :
        (module Renderer.S
           with type t = Renderer.Vg_renderer.t
            and type config = Renderer.Vg_renderer.config);
      state : Renderer.Vg_renderer.t;
    }
  | GIF of {
      renderer :
        (module Renderer.S
           with type t = Renderer.Gif_renderer.t
            and type config = Renderer.Gif_renderer.config);
      state : Renderer.Gif_renderer.t;
    }
  | ASCII of {
      renderer :
        (module Renderer.S
           with type t = Renderer.Ascii_renderer.t
            and type config = Renderer.Ascii_renderer.config);
      state : Renderer.Ascii_renderer.t;
    }

val run : Tape_lang.Ast.command list -> string option -> unit
(** Main entry point *)

val process_tape_config : Tape_lang.Ast.command list -> config
(** Helper functions for async implementation *)

val apply_setting_to_config :
  config -> Tape_lang.Ast.setting -> Tape_lang.Ast.value -> config

val create_renderer : config -> Vte.t -> string -> renderer_type
val generate_output : renderer_type -> string -> unit
val key_to_sequence : Tape_lang.Ast.key -> string
