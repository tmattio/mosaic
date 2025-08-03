(** VCR configuration module *)

type t = {
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
(** Configuration for VCR recording *)

val default : t
(** Default configuration *)

val apply_setting :
  t -> Tape_lang.Ast.setting -> Tape_lang.Ast.value -> (t, string) result
(** Apply a tape setting to configuration *)

val from_tape : Tape_lang.Ast.tape -> (t, string) result
(** Extract configuration from tape commands *)
