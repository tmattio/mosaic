(** Commands represent asynchronous operations that produce messages *)

type 'msg exec_cmd = { run : unit -> unit; on_complete : 'msg }

type 'msg t =
  | None
  | Msg of 'msg
  | Batch of 'msg t list
  | Perform of (unit -> 'msg option)
  | Exec of 'msg exec_cmd
  | Tick of float * (float -> 'msg)
  | Sequence of 'msg t list
  | Quit
  | Log of string
  | Set_window_title of string

val none : 'msg t
(** A command that does nothing *)

val msg : 'msg -> 'msg t
(** Create a command that immediately produces the given message *)

val batch : 'msg t list -> 'msg t
(** Batch multiple commands to run in parallel *)

val perform : (unit -> 'msg option) -> 'msg t
(** Create a command from a function that performs IO and optionally produces a
    message *)

val exec : (unit -> unit) -> 'msg -> 'msg t
(** [exec f msg] temporarily releases the terminal, executes function f,
    restores terminal, and produces msg when complete *)

val release_and_run : (unit -> unit) -> 'msg -> 'msg t
(** [release_and_run f msg] temporarily releases the terminal, executes function
    f, restores terminal, and produces msg when complete. Useful for running
    external editors, pagers, etc. This is an alias for [exec]. *)

val quit : 'msg t
(** A command that quits the program *)

val tick : float -> (float -> 'msg) -> 'msg t
(** [tick duration f] creates a command that waits for [duration] seconds, then
    calls [f] with the actual elapsed time to produce a message *)

val sequence : 'msg t list -> 'msg t
(** [sequence cmds] creates a command that runs commands sequentially, waiting
    for each command to complete before starting the next *)

val seq : 'msg t list -> 'msg t
(** Alias for [sequence] *)

val after : float -> 'msg -> 'msg t
(** [after delay msg] sends a message after delay (seconds) *)

val log : string -> 'msg t
(** [log message] creates a command that prints a message outside the main
    application view. This is useful for debugging without corrupting the UI.
    The message is written directly to stderr with a newline appended. *)

val set_window_title : string -> 'msg t
(** [set_window_title title] creates a command that sets the terminal window
    title. Note: Not all terminals support this feature. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Transform messages produced by a command *)

val to_list : 'msg t -> 'msg t list
(** Internal: flatten a command to a list of atomic commands *)

val pp :
  (Format.formatter -> 'msg -> unit) -> Format.formatter -> 'msg t -> unit
(** Pretty-printing *)
