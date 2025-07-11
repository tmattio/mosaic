(** A stopwatch component for measuring elapsed time.

    This component counts up from zero and can be used for timing operations,
    measuring durations, or displaying elapsed time. *)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the stopwatch *)

type msg
(** Messages the stopwatch can handle *)

(** Stopwatch display format *)
type format =
  | Hours_minutes_seconds  (** Display as HH:MM:SS *)
  | Minutes_seconds  (** Display as MM:SS *)
  | Total_seconds  (** Display as total seconds *)
  | Milliseconds  (** Display with millisecond precision *)
  | Custom of (float -> string)  (** Custom formatting function *)

val component : (model, msg) Mosaic.app
(** The component definition *)

(** {2 Initialization} *)

val init : ?format:format -> ?auto_start:bool -> unit -> model * msg Cmd.t
(** [init ?format ?auto_start ()] creates a new stopwatch.

    @param format Display format (default: Minutes_seconds)
    @param auto_start Start immediately after init (default: false) *)

(** {2 Accessors} *)

val elapsed : model -> float
(** Get the elapsed time in seconds *)

val is_running : model -> bool
(** Check if the stopwatch is currently running *)

val split_times : model -> float list
(** Get all recorded split times *)

val last_split : model -> float option
(** Get the most recent split time *)

(** {2 Actions} *)

val start : model -> model * msg Cmd.t
(** Start the stopwatch *)

val stop : model -> model * msg Cmd.t
(** Stop/pause the stopwatch *)

val toggle : model -> model * msg Cmd.t
(** Toggle between running and stopped states *)

val reset : model -> model * msg Cmd.t
(** Reset the stopwatch to 00:00:00 *)

val split : model -> model * msg Cmd.t
(** Record a split time at the current elapsed time *)

val clear_splits : model -> model
(** Clear all recorded split times *)

val set_format : format -> model -> model
(** Change the display format *)

(** {2 Theming} *)

type theme = {
  time_style : Style.t;
  label_style : Style.t;
  split_style : Style.t;
}
(** Theme configuration for the stopwatch *)

val default_theme : theme
(** Default stopwatch theme *)

val with_theme : theme -> model -> model
(** Apply a custom theme to the stopwatch *)

val with_label : string option -> model -> model
(** Set an optional label to display with the stopwatch *)

val with_show_splits : bool -> model -> model
(** Enable or disable split time display *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** Handle messages and update the stopwatch state *)

val view : model -> Ui.element
(** Render the stopwatch *)

val subscriptions : model -> msg Sub.t
(** Subscribe to timer ticks *)
