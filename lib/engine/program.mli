(** Program - Core Mosaic runtime toolkit *)

(** A handle to the core Mosaic runtime state. *)
type t

(** Configuration for creating a program. *)
type config = {
  terminal : Terminal.t option;
  alt_screen : bool;
  mouse : bool;
  fps : int;
  debug_log : out_channel option;
}

(** [create ~sw ~env config] creates a new program handle. *)
val create : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> config -> t

(** [cleanup t] gracefully shuts down the terminal and releases resources. *)
val cleanup : t -> unit

(** [process_cmd t dispatch cmd] executes a command.
    The [dispatch] function is called with any message produced by the command. *)
val process_cmd : t -> ('msg -> unit) -> 'msg Cmd.t -> unit

(** [render t element] renders a UI element to the terminal.
    This function contains all the complex diffing and buffering logic. *)
val render : t -> Ui.element -> unit

(** [run_input_loop t handle_event] runs a loop that listens for terminal
    input and calls [handle_event] for each event. Blocks until the program is stopped. *)
val run_input_loop : t -> (Input.event -> unit) -> unit

(** [run_render_loop t get_element] runs a loop that calls [get_element] and
    [render] at the configured FPS. Blocks until the program is stopped. *)
val run_render_loop : t -> (unit -> Ui.element) -> unit

(** [run_resize_loop t handle_resize] runs a loop that handles terminal resize events. *)
val run_resize_loop : t -> (int * int -> unit) -> unit

(** [setup_terminal t] sets up the terminal with the configured settings. *)
val setup_terminal : t -> unit

(** [setup_signal_handlers t] sets up signal handlers for graceful shutdown. *)
val setup_signal_handlers : t -> unit

(** [is_running t] returns whether the program is still running. *)
val is_running : t -> bool

(** [set_running t running] sets the running state of the program. *)
val set_running : t -> bool -> unit

(** [is_quit_pending t] returns whether a quit is pending. *)
val is_quit_pending : t -> bool

(** [set_quit_pending t pending] sets the quit pending state. *)
val set_quit_pending : t -> bool -> unit

(** [set_model t model] updates the model stored in the program.
    This is used by TEA-style APIs to update their state. *)
val set_model : t -> 'model -> unit

(** [get_model t] retrieves the model stored in the program. *)
val get_model : t -> 'model

(** [with_state_mutex t ~protect f] executes [f] while holding the state mutex. *)
val with_state_mutex : t -> protect:bool -> (unit -> 'a) -> 'a

(** [with_terminal_mutex t ~protect f] executes [f] while holding the terminal mutex. *)
val with_terminal_mutex : t -> protect:bool -> (unit -> 'a) -> 'a

(** [invalidate_buffer t] invalidates the previous render buffer, forcing a full redraw. *)
val invalidate_buffer : t -> unit

(** [add_static_element t element] adds a static element to be rendered. *)
val add_static_element : t -> Ui.element -> unit

(** [clear_static_elements t] clears all static elements. *)
val clear_static_elements : t -> unit

(** [log_debug t message] logs a debug message if debug logging is enabled. *)
val log_debug : t -> string -> unit

(** [resize_condition t] returns the resize condition variable. *)
val resize_condition : t -> Eio.Condition.t

(** [clock t] returns the program's clock. *)
val clock : t -> float Eio.Time.clock_ty Eio.Std.r

(** [switch t] returns the program's switch. *)
val switch : t -> Eio.Switch.t

(** [env t] returns the program's environment. *)
val env : t -> Eio_unix.Stdenv.base

(** [get_pending_updates t] returns whether there are pending updates to render. *)
val get_pending_updates : t -> bool

(** [set_pending_updates t pending] sets whether there are pending updates to render. *)
val set_pending_updates : t -> bool -> unit