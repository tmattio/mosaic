(** A progress bar component for displaying completion status of long-running
    operations. *)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the progress bar *)

type msg
(** Messages the progress bar can handle *)

type style_mode =
  | Solid of Style.color
  | Gradient of Style.color * Style.color
      (** Progress bar fill style - either solid color or gradient between two
          colors *)

val component : (model, msg) Mosaic.app
(** The component definition *)

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
(** Get the current progress percentage (0.0 to 1.0) *)

val is_complete : model -> bool
(** Check if progress is at 100% *)

val width : model -> int
(** Get the progress bar width *)

(** {2 Actions} *)

val set_percent : float -> model -> model * msg Cmd.t
(** Set the progress percentage (clamped to 0.0-1.0 range) *)

val increment : float -> model -> model * msg Cmd.t
(** Increase progress by the given amount *)

val decrement : float -> model -> model * msg Cmd.t
(** Decrease progress by the given amount *)

val complete : model -> model * msg Cmd.t
(** Set progress to 100% *)

val reset : model -> model * msg Cmd.t
(** Reset progress to 0% *)

val set_width : int -> model -> model
(** Update the progress bar width *)

val show_percentage : bool -> model -> model
(** Show or hide the percentage text *)

val set_style_mode : style_mode -> model -> model
(** Change the fill style *)

(** {2 Theming} *)

type theme = {
  full_char : string;
  empty_char : string;
  percentage_style : Style.t;
  bar_style : Style.t;
}
(** Theme configuration for the progress bar *)

val default_theme : theme
(** Default progress bar theme *)

val with_theme : theme -> model -> model
(** Apply a custom theme to the progress bar *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** Handle messages and update the progress bar state *)

val view : model -> Ui.element
(** Render the progress bar *)

val subscriptions : model -> msg Sub.t
(** Progress bars don't need subscriptions *)
