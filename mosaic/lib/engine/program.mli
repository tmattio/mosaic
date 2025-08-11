(** Program – Mosaic’s event / render runtime This interface is intentionally
    minimal and stable. *)

open Eio

(** {1 Configuration} *)

type config

val config :
  ?terminal:Tty_eio.t ->
  ?alt_screen:bool ->
  (* default = true  *)
  ?mouse:bool ->
  (* default = false *)
  ?fps:int ->
  (* default = 60    *)
  ?debug_log:out_channel ->
  (* default = none  *)
  unit ->
  config
(** Build a configuration record. All fields have sensible defaults. *)

(** {1 Handle} *)

type t
(** Opaque handle to a running program. *)

(** {1 Running} *)

val start :
  sw:Switch.t ->
  env:Eio_unix.Stdenv.base ->
  config ->
  render:(unit -> Ui.element) ->
  on_input:(Input.event -> unit) ->
  on_resize:(w:int -> h:int -> unit) ->
  ?on_snapshot:(Ui.Layout_snapshot.t -> unit) ->
  ?tick:(elapsed:float -> unit) ->
  unit ->
  t * (('msg -> unit) -> 'msg Cmd.t -> unit)
(** [start ~sw ~env cfg ~render ~on_input ~on_resize ?tick ()]
    - Prepares the terminal according to [cfg].
    - Spawns the internal render, input and resize fibres on [sw].
    - Returns 1. a handle [t] that can later be stopped, and 2. a
      function [process_cmd] — use it to execute a ['msg Cmd.t], supplying your
      own ['msg -> unit] dispatcher.

    The call returns immediately; the program keeps running in fibres. *)

val stop : t -> unit
(** Gracefully shut the program down (restores terminal state, joins fibres). *)

val is_running : t -> bool
(** Check if the program is still running. *)

val get_snapshot : t -> Ui.Layout_snapshot.t option
(** Get the layout snapshot if mouse mode is enabled. *)

(** {1 Lifecycle Management} *)

(** {2 Creation and Setup} *)

val create : config -> sw:Switch.t -> env:Eio_unix.Stdenv.base -> t
(** [create cfg ~sw ~env] creates a program handle without starting any loops.
    The terminal is not modified and no fibers are spawned. *)

val setup_terminal : t -> unit
(** [setup_terminal t] configures the terminal for the program (raw mode, mouse,
    etc). Should be called before starting event loops. *)

val cleanup_terminal : t -> unit
(** [cleanup_terminal t] restores the terminal to its original state. Called
    automatically by [stop]. *)

(** {2 Event Processing} *)

val set_input_handler : t -> (Input.event -> unit) -> unit
(** [set_input_handler t handler] sets the input event handler. *)

val set_resize_handler : t -> (w:int -> h:int -> unit) -> unit
(** [set_resize_handler t handler] sets the resize event handler. *)

val set_tick_handler : t -> (elapsed:float -> unit) option -> unit
(** [set_tick_handler t handler] sets the tick handler (or None to disable). *)

val set_snapshot_handler : t -> (Ui.Layout_snapshot.t -> unit) option -> unit
(** [set_snapshot_handler t handler] sets the snapshot callback for mouse
    support. *)

val process_cmd : t -> ('msg -> unit) -> 'msg Cmd.t -> unit
(** [process_cmd t dispatch cmd] executes a command with the given dispatcher.
*)

(** {2 Rendering} *)

val render : t -> Ui.element -> unit
(** [render t ui] renders the given UI element to the terminal. *)

val schedule_render : t -> unit
(** [schedule_render t] schedules a render on the next frame. Same as
    [request_render]. *)

val force_full_redraw : t -> unit
(** [force_full_redraw t] forces a full redraw on the next render loop
    iteration. This is useful when the entire UI needs to be re-evaluated, such
    as after a resize. *)

(** {2 Input Handling} *)

(** {2 Event Loop Control} *)

val run_event_loop : t -> unit
(** [run_event_loop t] runs the input event loop. Blocks until [stop] is called.
*)

val run_render_loop : t -> (unit -> Ui.element) -> unit
(** [run_render_loop t get_ui] runs the render loop. Blocks until [stop] is
    called. *)

val run_resize_loop : t -> unit
(** [run_resize_loop t] runs the resize detection loop. Blocks until [stop] is
    called. Uses the handler set with [set_resize_handler]. *)

val run_tick_loop : t -> unit
(** [run_tick_loop t] runs the tick loop at the configured FPS. Blocks until
    [stop] is called. Uses the handler set with [set_tick_handler]. *)
