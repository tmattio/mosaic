(** A stopwatch component for measuring and displaying elapsed time with split
    time functionality.

    This component counts up from zero, providing precise time measurement with
    support for start/stop controls, split times, and various display formats.
    It is useful for timing operations, benchmarking, or any scenario requiring
    elapsed time tracking.

    {2 Architecture}

    State tracks elapsed milliseconds, running status, and split times.
    Pause/resume preserves timing accuracy using timestamp deltas.

    {2 Key Invariants}

    - Elapsed time continuously increases while running
    - Split times are recorded at the exact moment requested
    - Pausing preserves the exact elapsed time
    - Reset clears both elapsed time and splits
    - Time accuracy depends on system timer resolution

    {2 Example}

    {[
      (* Initialize a stopwatch *)
      let stopwatch_model, stopwatch_cmd =
        Stopwatch.init
          ~format:Milliseconds
          ~auto_start:true
          ()

      (* In your update function *)
      | Stopwatch_msg msg ->
          let new_stopwatch, cmd = Stopwatch.update msg model.stopwatch in
          ({ model with stopwatch = new_stopwatch },
           Cmd.map (fun m -> Stopwatch_msg m) cmd)

      (* In your view *)
      let open Ui in
      vbox ~gap:1 [
        text "Elapsed time:";
        Stopwatch.view model.stopwatch;
        hbox ~gap:2 [
          text "Controls: ";
          text "[Space] Start/Stop";
          text "[S] Split";
          text "[R] Reset";
        ];
      ]

      (* Record split times *)
      let model, cmd = Stopwatch.split model.stopwatch
      let splits = Stopwatch.split_times model.stopwatch
      List.iteri (fun i time ->
        Printf.printf "Split %d: %.2fs\n" (i + 1) time
      ) splits

      (* Control the stopwatch *)
      let model, cmd = Stopwatch.toggle model.stopwatch
      let model, cmd = Stopwatch.reset model.stopwatch
    ]} *)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the stopwatch containing elapsed time, run state,
    split times, and display configuration. *)

type msg
(** Messages that the stopwatch can handle, including tick updates and control
    actions. *)

(** Stopwatch display format options.

    - [Hours_minutes_seconds] - Format as "01:23:45"
    - [Minutes_seconds] - Format as "23:45"
    - [Total_seconds] - Format as "1425.0" (total seconds)
    - [Milliseconds] - Format as "23:45.678" (with milliseconds)
    - [Custom f] - Use custom function [f] to format seconds *)
type format =
  | Hours_minutes_seconds  (** Display as HH:MM:SS *)
  | Minutes_seconds  (** Display as MM:SS *)
  | Total_seconds  (** Display as total seconds *)
  | Milliseconds  (** Display with millisecond precision *)
  | Custom of (float -> string)  (** Custom formatting function *)

val component : (model, msg) Mosaic.app
(** The stopwatch component definition following The Elm Architecture. *)

(** {2 Initialization} *)

val init : ?format:format -> ?auto_start:bool -> unit -> model * msg Cmd.t
(** [init ?format ?auto_start ()] creates a new stopwatch.

    @param format Display format (default: Minutes_seconds)
    @param auto_start Start immediately after init (default: false) *)

(** {2 Accessors} *)

val elapsed : model -> float
(** [elapsed model] returns the total elapsed time in seconds. *)

val is_running : model -> bool
(** [is_running model] returns whether the stopwatch is actively counting. *)

val split_times : model -> float list
(** [split_times model] returns all recorded split times in chronological order.
*)

val last_split : model -> float option
(** [last_split model] returns the most recently recorded split time, if any. *)

(** {2 Actions} *)

val start : model -> model * msg Cmd.t
(** [start model] starts the stopwatch. Has no effect if already running. *)

val stop : model -> model * msg Cmd.t
(** [stop model] pauses the stopwatch, preserving the elapsed time. *)

val toggle : model -> model * msg Cmd.t
(** [toggle model] toggles between running and paused states. *)

val reset : model -> model * msg Cmd.t
(** [reset model] resets the stopwatch to 00:00:00 and clears all split times.
*)

val split : model -> model * msg Cmd.t
(** [split model] records a split time at the current elapsed time. Split times
    are only recorded while the stopwatch is running. *)

val clear_splits : model -> model
(** [clear_splits model] removes all recorded split times without affecting the
    elapsed time. *)

val set_format : format -> model -> model
(** [set_format format model] changes how the elapsed time is displayed. *)

(** {2 Theming} *)

type theme = {
  time_style : Style.t;
  label_style : Style.t;
  split_style : Style.t;
}
(** Theme configuration for customizing the stopwatch appearance.

    - [time_style] - Style for the elapsed time display
    - [label_style] - Style for any label text
    - [split_style] - Style for split time display *)

val default_theme : theme
(** The default theme with standard styling for stopwatch display. *)

val with_theme : theme -> model -> model
(** [with_theme theme model] applies a custom theme to the stopwatch. *)

val with_label : string option -> model -> model
(** [with_label label model] sets an optional label to display alongside the
    stopwatch. Pass [None] to remove the label. *)

val with_show_splits : bool -> model -> model
(** [with_show_splits enabled model] enables or disables the display of split
    times in the view. *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] handles messages and updates the stopwatch state,
    including time progression and control actions. *)

val view : model -> Ui.element
(** [view model] renders the stopwatch with elapsed time, optional label, and
    split times if enabled. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns timer tick subscriptions when the stopwatch is
    running. *)
