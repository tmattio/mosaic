(** Program - Core Mosaic runtime toolkit *)

type t
(** A handle to the core Mosaic runtime state. *)

type config = {
  terminal : Tty.t option;
  alt_screen : bool;
  mouse : bool;
  fps : int;
  debug_log : out_channel option;
}
(** Configuration for creating a program. *)

val create : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> config -> t
(** [create ~sw ~env config] creates a new program handle. *)

val cleanup : t -> unit
(** [cleanup t] gracefully shuts down the terminal and releases resources. *)

val process_cmd : t -> ('msg -> unit) -> 'msg Cmd.t -> unit
(** [process_cmd t dispatch cmd] executes a command. The [dispatch] function is
    called with any message produced by the command. *)

val render : t -> Ui.element -> unit
(** [render t element] renders a UI element to the terminal. This function
    contains all the complex diffing and buffering logic. *)

val run_input_loop : t -> (Input.event -> unit) -> unit
(** [run_input_loop t handle_event] runs a loop that listens for terminal input
    and calls [handle_event] for each event. Blocks until the program is
    stopped. *)

val run_render_loop : t -> (unit -> Ui.element) -> unit
(** [run_render_loop t get_element] runs a loop that calls [get_element] and
    [render] at the configured FPS. Blocks until the program is stopped. *)

val setup_terminal : t -> unit
(** [setup_terminal t] sets up the terminal with the configured settings. *)

val setup_signal_handlers : t -> unit
(** [setup_signal_handlers t] sets up signal handlers for graceful shutdown. *)

val is_running : t -> bool
(** [is_running t] returns whether the program is still running. *)

val set_running : t -> bool -> unit
(** [set_running t running] sets the running state of the program. *)

val is_quit_pending : t -> bool
(** [is_quit_pending t] returns whether a quit is pending. *)

val set_quit_pending : t -> bool -> unit
(** [set_quit_pending t pending] sets the quit pending state. *)

val set_model : t -> 'model -> unit
(** [set_model t model] updates the model stored in the program. This is used by
    TEA-style APIs to update their state. *)

val get_model : t -> 'model
(** [get_model t] retrieves the model stored in the program. *)

val with_state_mutex : t -> protect:bool -> (unit -> 'a) -> 'a
(** [with_state_mutex t ~protect f] executes [f] while holding the state mutex.
*)

val with_terminal_mutex : t -> protect:bool -> (unit -> 'a) -> 'a
(** [with_terminal_mutex t ~protect f] executes [f] while holding the terminal
    mutex. *)

val invalidate_buffer : t -> unit
(** [invalidate_buffer t] invalidates the previous render buffer, forcing a full
    redraw. *)

val add_static_element : t -> Ui.element -> unit
(** [add_static_element t element] adds a static element to be rendered. *)

val clear_static_elements : t -> unit
(** [clear_static_elements t] clears all static elements. *)

val log_debug : t -> string -> unit
(** [log_debug t message] logs a debug message if debug logging is enabled. *)

val clock : t -> float Eio.Time.clock_ty Eio.Std.r
(** [clock t] returns the program's clock. *)

val switch : t -> Eio.Switch.t
(** [switch t] returns the program's switch. *)

val env : t -> Eio_unix.Stdenv.base
(** [env t] returns the program's environment. *)

val get_pending_updates : t -> bool
(** [get_pending_updates t] returns whether there are pending updates to render.
*)

val run_resize_loop : t -> (int * int -> unit) -> unit
(** [run_resize_loop t handle_resize] runs a loop that handles terminal resize
    events. *)

val resize_condition : t -> Eio.Condition.t
(** [resize_condition t] returns the resize condition variable. *)

val set_pending_updates : t -> bool -> unit
(** [set_pending_updates t pending] sets whether there are pending updates to
    render. *)

val terminal : t -> Tty.t
(** [terminal t] returns the terminal associated with the program. *)
