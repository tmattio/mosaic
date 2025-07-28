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
  frame_delay : int;  (** Delay between frames in 1/100th of a second *)
  theme : theme;
  font_path : string option;  (** Path to font file, None for built-in font *)
  font_size : int;  (** Font size in pixels *)
  target_width : int option;  (** Desired output width in pixels *)
  target_height : int option;  (** Desired output height in pixels *)
  padding : int;  (** Padding around terminal content in pixels *)
}
(** Renderer configuration *)

type t
(** Renderer state *)

val create : Vte.t -> config -> t
(** [create vte config] creates a new GIF renderer for the given VTE *)

val capture_frame : t -> unit
(** [capture_frame t] captures the current terminal state as a frame *)

val add_pending_delay : t -> float -> unit
(** [add_pending_delay t delay] adds delay (in seconds) to be included in the
    next captured frame *)

val render : t -> string
(** [render t] renders all captured frames as an animated GIF and returns the
    binary data *)

val frame_count : t -> int
(** [frame_count t] returns the number of frames captured so far. For testing.
*)

type frame_info = { width : int; height : int; x_offset : int; y_offset : int }
(** Information about a captured frame for testing *)

val get_frame_info : t -> int -> frame_info option
(** [get_frame_info t idx] returns information about frame at index [idx]
    (0-based, most recent first) *)

val render_streaming : t -> out_channel -> unit
(** [render_streaming t out_channel] renders all captured frames as an animated
    GIF and writes directly to the output channel. This is more memory-efficient
    than [render] for large GIFs. *)
