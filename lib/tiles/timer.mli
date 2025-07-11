(** A countdown timer component for tracking time remaining.

    This component counts down from a specified duration and can trigger events
    when time runs out. Useful for timeouts, limited-time actions, or any
    countdown display. *)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the timer *)

type msg
(** Messages the timer can handle *)

(** Timer display format *)
type format =
  | Hours_minutes_seconds  (** Display as HH:MM:SS *)
  | Minutes_seconds  (** Display as MM:SS *)
  | Total_seconds  (** Display as total seconds *)
  | Custom of (float -> string)  (** Custom formatting function *)

val component : (model, msg) Mosaic.app
(** The component definition *)

(** {2 Initialization} *)

val init :
  ?duration:float ->
  ?format:format ->
  ?auto_start:bool ->
  unit ->
  model * msg Cmd.t
(** [init ?duration ?format ?auto_start ()] creates a new timer.

    @param duration Initial duration in seconds (default: 60.0)
    @param format Display format (default: MinutesSeconds)
    @param auto_start Start immediately after init (default: false) *)

(** {2 Accessors} *)

val remaining : model -> float
(** Get the time remaining in seconds *)

val duration : model -> float
(** Get the initial duration in seconds *)

val is_running : model -> bool
(** Check if the timer is currently running *)

val is_expired : model -> bool
(** Check if the timer has reached zero *)

val elapsed_percentage : model -> float
(** Get the elapsed time as a percentage (0.0 to 1.0) *)

(** {2 Actions} *)

val start : model -> model * msg Cmd.t
(** Start the timer *)

val stop : model -> model * msg Cmd.t
(** Stop/pause the timer *)

val toggle : model -> model * msg Cmd.t
(** Toggle between running and stopped states *)

val reset : model -> model * msg Cmd.t
(** Reset the timer to its initial duration *)

val set_duration : float -> model -> model * msg Cmd.t
(** Set a new duration (resets the timer) *)

val add_time : float -> model -> model
(** Add seconds to the remaining time *)

val set_format : format -> model -> model
(** Change the display format *)

(** {2 Events} *)

type event_handler = unit -> msg Cmd.t
(** Event handler type *)

val on_expire : event_handler -> model -> model
(** Set a handler to be called when the timer expires *)

(** {2 Theming} *)

type theme = {
  time_style : Style.t;
  expired_style : Style.t;
  label_style : Style.t;
}
(** Theme configuration for the timer *)

val default_theme : theme
(** Default timer theme *)

val with_theme : theme -> model -> model
(** Apply a custom theme to the timer *)

val with_label : string option -> model -> model
(** Set an optional label to display with the timer *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** Handle messages and update the timer state *)

val view : model -> Ui.element
(** Render the timer *)

val subscriptions : model -> msg Sub.t
(** Subscribe to timer ticks *)
