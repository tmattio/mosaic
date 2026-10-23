(** Mosaic programs on an emulated terminal.

    {!run} runs a program with {!Mosaic.run} on a {!Matrix_test} session of a
    terminal, as a test writes it:

    {[
    let term = Matrix_test.Terminal.create ~cols:40 ~rows:10 () in
    Mosaic_test.run term app @@ fun s ->
    Matrix_test.input s (Matrix_test.Terminal.text term "hello");
    print_string (Matrix_test.Screen.text (Matrix_test.Terminal.screen term))
    ]}

    The session's functions ({!Matrix_test.input}, {!Matrix_test.advance}, ...)
    drive the program, and the terminal's readers ({!Matrix_test.Terminal}) show
    what it did.

    A worker ({!Mosaic.Sub.val-worker}) runs as long as the program declares it,
    so it runs on an executor of its own, with a [quiesce] that returns once
    every piece of work waits. Under Eio's mock scheduler ([eio.mock]), work
    runs on fibers, and a sleep of 1 ns returns only once every other fiber
    waits:

    {[
    Eio_mock.Backend.run_full @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let process_perform job =
      Eio.Fiber.fork_daemon ~sw (fun () ->
          job ();
          `Stop_daemon)
    in
    let quiesce () = Eio.Time.Mono.sleep env#mono_clock 1e-9 in
    Mosaic_test.run ~process_perform ~quiesce term app @@ fun s -> ...
    ]} *)

val run :
  ?config:Matrix.config ->
  ?now:int ->
  ?process_perform:((unit -> unit) -> unit) ->
  ?quiesce:(unit -> unit) ->
  Matrix_test.Terminal.t ->
  ('model, 'msg) Mosaic.app ->
  (Matrix_test.t -> 'a) ->
  'a
(** [run term app body] runs [app] with {!Mosaic.run} on a session [s] of
    [term], as {!Matrix_test.run} runs a program, then is [body s]. When [body]
    returns or raises, the input ends and the program ends with it.

    - [config] -- what the run sets up, as {!Mosaic.run} takes it. Defaults to
      {!Matrix.val-config}[ ()].
    - [now] -- the session's clock at the start, in nanoseconds. Defaults to
      [0].
    - [process_perform] -- the executor of the program's work off the loop
      ({!Mosaic.Cmd.val-perform}, {!Mosaic.Sub.val-worker}). Defaults to the
      session's queue ({!Matrix_test.perform}), which settling runs. A worker
      that runs as long as the program needs its own executor, with a [quiesce].
    - [quiesce] -- returns once the work [process_perform] was given has
      finished or waits for the test, as {!Matrix_test.attach} takes it.
      Defaults to running the session's queue.

    Raises what [app] raises, at the call of [body] that had it go on. *)
