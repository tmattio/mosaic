(** Unix runtime for Matrix applications.

    Provides Unix-based I/O (file descriptors, select, signals) to drive a
    {!Matrix.app}. Handles raw mode, capability probing, wakeup pipe,
    SIGWINCH, and shutdown signal handlers.

    {[
      let app = Matrix.create ~mode:`Alt () in
      Matrix_unix.run app ~on_render:(fun app ->
        Grid.put_string (Matrix.grid app) ~row:1 ~col:1 "Hello")
    ]} *)

val run :
  ?on_frame:(Matrix.app -> dt:float -> unit) ->
  ?on_input:(Matrix.app -> Matrix.Input.t -> unit) ->
  ?on_resize:(Matrix.app -> cols:int -> rows:int -> unit) ->
  on_render:(Matrix.app -> unit) ->
  ?output:[ `Stdout | `Fd of Unix.file_descr ] ->
  ?signal_handlers:bool ->
  ?initial_caps:Matrix.Terminal.capabilities ->
  Matrix.app ->
  unit
(** [run app ~on_render] sets up Unix I/O, attaches it to [app], runs the
    event loop, and cleans up on exit.

    @param output
      Output target for rendered frames. Defaults to [`Stdout].
    @param signal_handlers
      Whether to install SIGTERM/SIGINT/SIGQUIT handlers that run cleanup on
      abnormal exit. Defaults to [true].
    @param initial_caps
      Seed capabilities passed to {!Matrix.Terminal.make}, bypassing probing for
      known terminals. *)

val install_signal_handlers : unit -> unit
(** [install_signal_handlers ()] installs shutdown signal handlers
    (SIGTERM, SIGINT, SIGQUIT, SIGABRT) manually. Idempotent. *)
