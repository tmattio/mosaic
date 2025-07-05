(** An animated spinner component for indicating loading states

    This component provides various animated spinner styles that can be used to
    indicate loading, processing, or waiting states in your TUI application.

    Example:
    {[
      (* Initialize a spinner with default style *)
      let spinner_model, spinner_cmd = Spinner.init ()

      (* Or with a specific style *)
      let spinner_model, spinner_cmd =
        Spinner.init ~style:Spinner.dots ()
          (* In your view *)
          hbox
          [ Spinner.view model.spinner; text " Loading data..." ]
    ]} *)

open Mosaic

type model
(** The internal state of the spinner *)

type msg
(** Messages the spinner can handle *)

val component : (model, msg) Mosaic.app
(** The component definition *)

(** {2 Spinner Styles} *)

type spinner_style = {
  frames : string array;
      (** Array of strings representing each frame of the animation *)
  interval : float;  (** Time in seconds between frames *)
}
(** A spinner style definition *)

(** {3 Predefined Styles} *)

val dots : spinner_style
(** Classic dots spinner: ⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏ *)

val line : spinner_style
(** Simple line spinner: - \ | / *)

val arrow : spinner_style
(** Arrow spinner: ← ↖ ↑ ↗ → ↘ ↓ ↙ *)

val box_bounce : spinner_style
(** Box bounce spinner: ▖ ▘ ▝ ▗ *)

val circle : spinner_style
(** Circle spinner: ◐ ◓ ◑ ◒ *)

val square : spinner_style
(** Square spinner: ◰ ◳ ◲ ◱ *)

val triangle : spinner_style
(** Triangle spinner: ◢ ◣ ◤ ◥ *)

val bar : spinner_style
(** Progress bar style: ▏ ▎ ▍ ▌ ▋ ▊ ▉ █ *)

val pulse : spinner_style
(** Pulsing block: ▁ ▂ ▃ ▄ ▅ ▆ ▇ █ ▇ ▆ ▅ ▄ ▃ ▂ ▁ *)

val bounce : spinner_style
(** Bouncing ball: ⠁ ⠂ ⠄ ⠂ *)

(** {2 Initialization} *)

val init : ?style:spinner_style -> ?color:Style.t -> unit -> model * msg Cmd.t
(** [init ?style ?color ()] creates a new spinner.

    @param style The animation style to use (default: {!dots})
    @param color The color/style for the spinner (default: no styling) *)

(** {2 Control} *)

val start : model -> model * msg Cmd.t
(** Start the spinner animation *)

val stop : model -> model
(** Stop the spinner animation *)

val is_spinning : model -> bool
(** Check if the spinner is currently animating *)

(** {2 Customization} *)

val set_style : spinner_style -> model -> model
(** Change the spinner style *)

val set_color : Style.t -> model -> model
(** Change the spinner color/style *)

(** {2 Custom Spinner Styles} *)

val custom : string array -> float -> spinner_style
(** [custom frames interval] creates a custom spinner style.

    @param frames Array of strings for each animation frame
    @param interval Time in seconds between frames *)

val from_string : string -> spinner_style
(** [from_string s] creates a spinner from a string where each character is a
    frame. Useful for simple spinners.

    Example: [from_string "◴◷◶◵"] creates a clock spinner. *)
