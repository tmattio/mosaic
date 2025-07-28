(** An animated spinner component for indicating loading, processing, or waiting
    states in terminal user interfaces.

    This component provides various animated spinner styles with customizable
    colors and frame rates. It efficiently handles animation timing and provides
    a clean API for starting and stopping animations.

    {2 Architecture}

    State tracks running status, current frame index, and spinner style. Frame
    advances via tick subscription at configurable intervals.

    {2 Key Invariants}

    - Only one frame is displayed at a time
    - Animation stops immediately when requested
    - Frame index wraps around to create continuous animation
    - Custom spinners must have at least one frame
    - Timer subscriptions are only active when spinning

    {2 Example}

    {[
      (* Initialize a spinner with dots style *)
      let spinner_model, spinner_cmd =
        Spinner.init ~style:Spinner.dots ()

      (* Or with custom color *)
      let spinner_model, spinner_cmd =
        Spinner.init
          ~style:Spinner.dots
          ~color:Style.(fg (Ansi (Blue)))
          ()

      (* Start the animation *)
      let model, cmd = Spinner.start model

      (* In your view *)
      let open Ui in
      hbox ~gap:1 [
        Spinner.view model.spinner;
        text " Loading data...";
      ]

      (* Create a custom spinner *)
      let hearts = Spinner.from_string "♥♡"
      let model = Spinner.set_style hearts model

      (* Stop when done *)
      let model = Spinner.stop model
    ]} *)

open Mosaic

type model
(** The internal state of the spinner containing animation state, current frame,
    style, and color configuration. *)

type msg
(** Messages that the spinner can handle, primarily for frame updates. *)

val component : (model, msg) Mosaic.app
(** The spinner component definition following The Elm Architecture. *)

(** {2 Spinner Styles} *)

type spinner_style = {
  frames : string array;
      (** Array of strings representing each frame of the animation *)
  interval : float;  (** Time in seconds between frames *)
}
(** A spinner style definition with animation frames and timing.

    - [frames] - Array of strings where each string is one animation frame
    - [interval] - Time in seconds between frame transitions *)

(** {3 Predefined Styles} *)

val dots : spinner_style
(** [dots] is the classic braille dots spinner: ⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏. *)

val line : spinner_style
(** [line] is a simple line spinner: - \ | /. *)

val arrow : spinner_style
(** [arrow] is an arrow spinner rotating through all directions: ← ↖ ↑ ↗ → ↘ ↓
    ↙. *)

val box_bounce : spinner_style
(** [box_bounce] is a box corner bounce spinner: ▖ ▘ ▝ ▗. *)

val circle : spinner_style
(** [circle] is a circle quadrant spinner: ◐ ◓ ◑ ◒. *)

val square : spinner_style
(** [square] is a square quadrant spinner: ◰ ◳ ◲ ◱. *)

val triangle : spinner_style
(** [triangle] is a triangle corner spinner: ◢ ◣ ◤ ◥. *)

val bar : spinner_style
(** [bar] is a progress bar style spinner: ▏ ▎ ▍ ▌ ▋ ▊ ▉ █. *)

val pulse : spinner_style
(** [pulse] is a pulsing block animation: ▁ ▂ ▃ ▄ ▅ ▆ ▇ █ ▇ ▆ ▅ ▄ ▃ ▂ ▁. *)

val bounce : spinner_style
(** [bounce] is a bouncing ball animation: ⠁ ⠂ ⠄ ⠂. *)

(** {2 Initialization} *)

val init : ?style:spinner_style -> ?color:Style.t -> unit -> model * msg Cmd.t
(** [init ?style ?color ()] creates a new spinner.

    @param style The animation style to use (default: {!dots})
    @param color The color/style for the spinner (default: no styling) *)

(** {2 Control} *)

val start : model -> model * msg Cmd.t
(** [start model] starts the spinner animation. Returns a command to begin the
    animation loop. *)

val stop : model -> model
(** [stop model] stops the spinner animation immediately. *)

val is_spinning : model -> bool
(** [is_spinning model] returns whether the spinner is currently animating. *)

(** {2 Customization} *)

val set_style : spinner_style -> model -> model
(** [set_style style model] changes the spinner style. The animation continues
    with the new style if currently spinning. *)

val set_color : Style.t -> model -> model
(** [set_color color model] changes the spinner color/style attributes. *)

(** {2 Custom Spinner Styles} *)

val custom : string array -> float -> spinner_style
(** [custom frames interval] creates a custom spinner style.

    @param frames Array of strings for each animation frame
    @param interval Time in seconds between frames *)

val from_string : string -> spinner_style
(** [from_string s] creates a spinner from a string where each character is a
    frame. Uses a default interval of 0.08 seconds between frames.

    Example: [from_string "◴◷◶◵"] creates a clock spinner. *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] handles messages and updates the spinner state, advancing
    the animation frame. *)

val view : model -> Ui.element
(** [view model] renders the current spinner frame with the configured style. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns timer subscriptions for animation updates when
    the spinner is active. *)
