(** A progress bar component for visualizing the completion status of operations
    and processes.

    This component displays a horizontal progress bar with customizable width,
    colors, and optional percentage display. It supports both solid colors and
    gradient fills for visual appeal.

    {2 Architecture}

    State tracks progress percentage (0.0-1.0), width, and fill style
    (solid/gradient). Optional percentage text overlay.

    {2 Key Invariants}

    - Progress percentage is always clamped between 0.0 and 1.0
    - Bar width must be positive (minimum 1 character)
    - Gradient colors interpolate smoothly across the fill
    - Complete status is true only at exactly 100%
    - Visual fill accurately represents the percentage

    {2 Example}

    {[
      (* Initialize a progress bar *)
      let progress_model, progress_cmd =
        Progress.init
          ~percent:0.0
          ~width:50
          ~show_percentage:true
          ~style_mode:(Gradient (Ansi Green, Ansi Blue))
          ()

      (* In your update function *)
      | Progress_msg msg ->
          let new_progress, cmd = Progress.update msg model.progress in
          ({ model with progress = new_progress },
           Cmd.map (fun m -> Progress_msg m) cmd)

      (* In your view *)
      let open Ui in
      vbox ~gap:(`Cells 1) [
        text "Download progress:";
        Progress.view model.progress;
        if Progress.is_complete model.progress then
          text ~style:Style.(fg Green) "Download complete!"
        else
          empty;
      ]

      (* Update progress *)
      let progress = calculate_progress downloaded total in
      let model, cmd = Progress.set_percent progress model.progress

      (* Increment progress *)
      let model, cmd = Progress.increment 0.1 model.progress

      (* Custom theme *)
      let theme = {
        Progress.default_theme with
        full_char = "█";
        empty_char = "░";
      } in
      let model = Progress.with_theme theme model
    ]} *)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the progress bar containing percentage, display
    settings, and theme configuration. *)

type msg
(** Messages that the progress bar can handle for updating progress state. *)

type style_mode =
  | Solid of Style.color
  | Gradient of Style.color * Style.color
      (** Progress bar fill style options.

          - [Solid color] - Fill with a single solid color
          - [Gradient (start_color, end_color)] - Fill with a gradient
            transitioning from start to end color *)

val component : (model, msg) Mosaic.app
(** The progress bar component definition following The Elm Architecture. *)

(** {2 Initialization} *)

val init :
  ?percent:float ->
  ?width:int ->
  ?show_percentage:bool ->
  ?style_mode:style_mode ->
  unit ->
  model * msg Cmd.t
(** [init ?percent ?width ?show_percentage ?style_mode ()] creates a new
    progress bar.

    @param percent Initial progress (0.0 to 1.0, default: 0.0)
    @param width Width of the progress bar (default: 40)
    @param show_percentage Display percentage text (default: true)
    @param style_mode Fill style (default: Solid blue) *)

(** {2 Accessors} *)

val percent : model -> float
(** [percent model] returns the current progress as a value between 0.0 (0%) and
    1.0 (100%). *)

val is_complete : model -> bool
(** [is_complete model] returns whether the progress has reached 100%. *)

val width : model -> int
(** [width model] returns the display width of the progress bar in characters.
*)

(** {2 Actions} *)

val set_percent : float -> model -> model * msg Cmd.t
(** [set_percent p model] sets the progress to [p], automatically clamping the
    value between 0.0 and 1.0. *)

val increment : float -> model -> model * msg Cmd.t
(** [increment amt model] increases the progress by [amt]. The result is clamped
    to not exceed 1.0. *)

val decrement : float -> model -> model * msg Cmd.t
(** [decrement amt model] decreases the progress by [amt]. The result is clamped
    to not go below 0.0. *)

val complete : model -> model * msg Cmd.t
(** [complete model] immediately sets the progress to 100%. *)

val reset : model -> model * msg Cmd.t
(** [reset model] resets the progress to 0%. *)

val set_width : int -> model -> model
(** [set_width w model] sets the progress bar display width to [w] characters.
*)

val show_percentage : bool -> model -> model
(** [show_percentage enabled model] enables or disables the percentage text
    display. *)

val set_style_mode : style_mode -> model -> model
(** [set_style_mode style model] changes the progress bar fill style to solid
    color or gradient. *)

(** {2 Theming} *)

type theme = {
  full_char : string;
  empty_char : string;
  percentage_style : Style.t;
  bar_style : Style.t;
}
(** Theme configuration for customizing the progress bar appearance.

    - [full_char] - Character used for filled portion (default: "█")
    - [empty_char] - Character used for empty portion (default: "░")
    - [percentage_style] - Style for percentage text
    - [bar_style] - Base style for the progress bar container *)

val default_theme : theme
(** The default theme with standard progress bar styling. *)

val with_theme : theme -> model -> model
(** [with_theme theme model] applies a custom theme to the progress bar. *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] handles messages and updates the progress bar state. *)

val view : model -> Ui.element
(** [view model] renders the progress bar with fill, optional percentage, and
    current theme. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns subscriptions for the progress bar. Currently
    returns no subscriptions as progress bars are not interactive. *)
