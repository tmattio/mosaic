(* The program runs on a domain of its own, as [Mosaic.run] runs it. Its
   work goes to Lwt's thread as a job of [Lwt_preemptive], which takes jobs
   from any thread or domain. *)
let run ?input ?output ?env ?stop ?config app =
  let stop =
    match stop with Some stop -> stop | None -> Matrix.Stop.create ()
  in
  let process_perform work =
    Lwt_preemptive.run_in_main_dont_wait
      (fun () ->
        work ();
        Lwt.return_unit)
      ignore
  in
  (* The domain closes [closed] once the terminal is given back, which makes
     [ended] readable: Lwt waits for that without holding its thread, then
     joins the domain, which has returned or is about to. *)
  let ended, closed = Unix.pipe ~cloexec:true () in
  let program () =
    Fun.protect
      ~finally:(fun () -> Unix.close closed)
      (fun () ->
        Mosaic.run ?input ?output ?env ~stop ?config ~process_perform app)
  in
  let domain =
    try Domain.spawn program
    with exn ->
      Unix.close ended;
      Unix.close closed;
      raise exn
  in
  let ended = Lwt_unix.of_unix_file_descr ended in
  let finished =
    Lwt.finalize
      (fun () ->
        Lwt.map (fun () -> Domain.join domain) (Lwt_unix.wait_read ended))
      (fun () -> Lwt_unix.close ended)
  in
  (* Cancelling the promise leaves [finished] pending, so the domain is
     still joined and the descriptor closed once the terminal is given
     back. *)
  let program = Lwt.protected finished in
  Lwt.on_cancel program (fun () -> Matrix.Stop.request stop);
  program
