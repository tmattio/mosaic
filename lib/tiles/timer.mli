(** A countdown timer component for tracking and displaying time remaining until
    a deadline.

    This component counts down from a specified duration, displaying the
    remaining time in various formats. It can trigger custom events when time
    expires and supports pause/resume functionality.

    {2 Architecture}

    State tracks duration, remaining time, and running status. Fires expiration
    event when reaching zero. Supports dynamic duration adjustments.

    {2 Key Invariants}

    - Remaining time never goes below zero
    - Timer stops automatically when reaching zero
    - Expired state persists until reset
    - Time adjustments respect the zero lower bound
    - Only one expiration event fires per countdown cycle

    {2 Example}

    {[
      (* Initialize a 5-minute timer *)
      let timer_model, timer_cmd =
        Timer.init
          ~duration:300.0  (* 5 minutes *)
          ~format:Hours_minutes_seconds
          ~auto_start:true
          ()

      (* Set up expiration handler *)
      let timer_model =
        Timer.on_expire
          (fun () ->
            print_endline "Time's up!";
            Cmd.none)
          timer_model

      (* In your update function *)
      | Timer_msg msg ->
          let new_timer, cmd = Timer.update msg model.timer in
          ({ model with timer = new_timer },
           Cmd.map (fun m -> Timer_msg m) cmd)

      (* In your view *)
      let open Ui in
      vbox ~gap:1 [
        text "Time remaining:";
        Timer.view model.timer;
        if Timer.is_expired model.timer then
          text ~style:Style.(fg Red) "Timer expired!"
        else
          empty;
      ]

      (* Control the timer *)
      let model, cmd = Timer.toggle model.timer
      let model, cmd = Timer.add_time 30.0 model.timer  (* Add 30 seconds *)
      let model, cmd = Timer.reset model.timer
    ]} *)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the timer containing duration, remaining time, run
    state, and display configuration. *)

type msg
(** Messages that the timer can handle, including tick updates and control
    actions. *)

(** Timer display format options.

    - [Hours_minutes_seconds] - Format as "01:23:45"
    - [Minutes_seconds] - Format as "23:45"
    - [Total_seconds] - Format as "1425" (total seconds)
    - [Custom f] - Use custom function [f] to format seconds *)
type format =
  | Hours_minutes_seconds  (** Display as HH:MM:SS *)
  | Minutes_seconds  (** Display as MM:SS *)
  | Total_seconds  (** Display as total seconds *)
  | Custom of (float -> string)  (** Custom formatting function *)

val component : (model, msg) Mosaic.app
(** The timer component definition following The Elm Architecture. *)

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
(** [remaining model] returns the time remaining in seconds. Returns 0.0 if the
    timer has expired. *)

val duration : model -> float
(** [duration model] returns the initial duration that was set for the timer. *)

val is_running : model -> bool
(** [is_running model] returns whether the timer is actively counting down. *)

val is_expired : model -> bool
(** [is_expired model] returns whether the timer has reached zero. *)

val elapsed_percentage : model -> float
(** [elapsed_percentage model] returns the elapsed time as a percentage from 0.0
    (just started) to 1.0 (expired). *)

(** {2 Actions} *)

val start : model -> model * msg Cmd.t
(** [start model] starts the countdown timer. Has no effect if already running.
*)

val stop : model -> model * msg Cmd.t
(** [stop model] pauses the timer, preserving the remaining time. *)

val toggle : model -> model * msg Cmd.t
(** [toggle model] toggles between running and paused states. *)

val reset : model -> model * msg Cmd.t
(** [reset model] resets the timer to its initial duration and stops it. *)

val set_duration : float -> model -> model * msg Cmd.t
(** [set_duration seconds model] sets a new duration and resets the timer. The
    timer is stopped after setting the new duration. *)

val add_time : float -> model -> model
(** [add_time seconds model] adds the specified number of seconds to the
    remaining time. Negative values subtract time. *)

val set_format : format -> model -> model
(** [set_format format model] changes how the remaining time is displayed. *)

(** {2 Events} *)

type event_handler = unit -> msg Cmd.t
(** Event handler type for timer expiration callbacks. *)

val on_expire : event_handler -> model -> model
(** [on_expire handler model] sets a handler to be called when the timer reaches
    zero. The handler is called only once per countdown cycle. *)

(** {2 Theming} *)

type theme = {
  time_style : Style.t;
  expired_style : Style.t;
  label_style : Style.t;
}
(** Theme configuration for customizing the timer appearance.

    - [time_style] - Style for the time display when running
    - [expired_style] - Style for the time display when expired
    - [label_style] - Style for any label text *)

val default_theme : theme
(** The default theme with standard styling for timer states. *)

val with_theme : theme -> model -> model
(** [with_theme theme model] applies a custom theme to the timer. *)

val with_label : string option -> model -> model
(** [with_label label model] sets an optional label to display alongside the
    timer. Pass [None] to remove the label. *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] handles messages and updates the timer state, including
    countdown ticks and expiration events. *)

val view : model -> Ui.element
(** [view model] renders the timer with the current remaining time and optional
    label. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns timer tick subscriptions when the timer is
    running. *)
