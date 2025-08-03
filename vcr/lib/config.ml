(** VCR configuration module *)

open Tape_lang.Ast

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

let default =
  {
    shell = "sh";
    width = 640;
    height = 480;
    typing_speed = 0.05;
    font_size = 22;
    font_family = None;
    padding = 60;
    framerate = 50;
    playback_speed = 1.0;
    theme = None;
    letter_spacing = None;
    line_height = None;
    loop_offset = None;
    border_radius = None;
    margin = None;
    margin_fill = None;
    cursor_blink = true;
    window_bar = None;
    window_bar_size = None;
  }

let apply_setting t setting value =
  match (setting, value) with
  | Shell, String s -> Ok { t with shell = s }
  | Width, Float f when f > 0.0 -> Ok { t with width = int_of_float f }
  | Width, Int i when i > 0 -> Ok { t with width = i }
  | Height, Float f when f > 0.0 -> Ok { t with height = int_of_float f }
  | Height, Int i when i > 0 -> Ok { t with height = i }
  | TypingSpeed, Float f when f >= 0.0 -> Ok { t with typing_speed = f }
  | FontSize, Float f when f > 0.0 -> Ok { t with font_size = int_of_float f }
  | FontSize, Int i when i > 0 -> Ok { t with font_size = i }
  | FontFamily, String s -> Ok { t with font_family = Some s }
  | Padding, Float f when f >= 0.0 -> Ok { t with padding = int_of_float f }
  | Padding, Int i when i >= 0 -> Ok { t with padding = i }
  | Framerate, Float f when f > 0.0 -> Ok { t with framerate = int_of_float f }
  | Framerate, Int i when i > 0 -> Ok { t with framerate = i }
  | PlaybackSpeed, Float f when f > 0.0 -> Ok { t with playback_speed = f }
  | Theme, String s -> Ok { t with theme = Some s }
  | Theme, Json s -> Ok { t with theme = Some s }
  | LetterSpacing, Float f -> Ok { t with letter_spacing = Some f }
  | LineHeight, Float f when f > 0.0 -> Ok { t with line_height = Some f }
  | LoopOffset, Float f when f >= 0.0 -> Ok { t with loop_offset = Some f }
  | BorderRadius, Float f when f >= 0.0 ->
      Ok { t with border_radius = Some (int_of_float f) }
  | BorderRadius, Int i when i >= 0 -> Ok { t with border_radius = Some i }
  | Margin, Float f when f >= 0.0 ->
      Ok { t with margin = Some (int_of_float f) }
  | Margin, Int i when i >= 0 -> Ok { t with margin = Some i }
  | MarginFill, String s -> Ok { t with margin_fill = Some s }
  | CursorBlink, Bool b -> Ok { t with cursor_blink = b }
  | WindowBar, Bool b -> Ok { t with window_bar = Some b }
  | WindowBar, String s ->
      Ok { t with window_bar = Some (s = "true" || s = "1") }
  | WindowBarSize, Float f when f > 0.0 ->
      Ok { t with window_bar_size = Some (int_of_float f) }
  | WindowBarSize, Int i when i > 0 -> Ok { t with window_bar_size = Some i }
  | Width, _ -> Error "Width expects positive number"
  | Height, _ -> Error "Height expects positive number"
  | TypingSpeed, _ -> Error "TypingSpeed expects non-negative number"
  | FontSize, _ -> Error "FontSize expects positive number"
  | Padding, _ -> Error "Padding expects non-negative number"
  | Framerate, _ -> Error "Framerate expects positive number"
  | PlaybackSpeed, _ -> Error "PlaybackSpeed expects positive number"
  | LineHeight, _ -> Error "LineHeight expects positive number"
  | LoopOffset, _ -> Error "LoopOffset expects non-negative number"
  | BorderRadius, _ -> Error "BorderRadius expects non-negative number"
  | Margin, _ -> Error "Margin expects non-negative number"
  | WindowBarSize, _ -> Error "WindowBarSize expects positive number"
  | _ ->
      Error
        (Printf.sprintf "Invalid value for %s setting"
           (Tape_lang.Ast.setting_to_string setting))

let from_tape tape =
  let open Error.Syntax in
  List.fold_left
    (fun cfg_res cmd ->
      let* cfg = cfg_res in
      match cmd with
      | Set (setting, value) -> apply_setting cfg setting value
      | _ -> Ok cfg)
    (Ok default) tape
