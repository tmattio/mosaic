open Eio.Std

(* Returns once one of the program's descriptors is readable or its deadline
   has come. Cancelling the fiber ends the wait. The deadline is on the run's
   clock, [Matrix.clock], so only the time left until it is handed to Eio,
   whose own monotonic clock measures it. *)
let wait program =
  let deadline () =
    match Mosaic.deadline program with
    | None -> Fiber.await_cancel ()
    | Some deadline ->
        let seconds = Float.of_int (deadline - Matrix.clock ()) *. 1e-9 in
        if seconds > 0. then Eio_unix.sleep seconds
  in
  let readable fd () = Eio_unix.await_readable fd in
  Fiber.any (deadline :: List.map readable (Mosaic.fds program))

(* The work of the program, run as fibers of the calling domain. A switch
   belongs to the domain that made it, so the program's domain hands the
   work over through a stream, which any domain may add to, and a fiber of
   the calling domain forks it. The fibers are daemons: they end with the
   switch, once the program has ended. *)
let run ?input ?output ?env ?config domain_mgr app =
  Switch.run @@ fun sw ->
  let work = Eio.Stream.create max_int in
  Fiber.fork_daemon ~sw (fun () ->
      while true do
        let f = Eio.Stream.take work in
        Fiber.fork_daemon ~sw (fun () ->
            f ();
            `Stop_daemon)
      done;
      `Stop_daemon);
  Eio.Domain_manager.run domain_mgr @@ fun () ->
  Mosaic.start ?input ?output ?env ?config
    ~process_perform:(Eio.Stream.add work) app
  @@ fun program ->
  let rec loop () =
    match Mosaic.step program ~now:(Matrix.clock ()) with
    | `Stepped -> loop ()
    | `Idle ->
        wait program;
        loop ()
    | `Stopped -> ()
  in
  loop ()
