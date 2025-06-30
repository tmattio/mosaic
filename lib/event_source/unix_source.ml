type t = {
  terminal : Terminal.t;
  parser : Input.parser;
  mutable event_queue : Input.event list;
}

let create terminal = { terminal; parser = Input.create (); event_queue = [] }

let read t ~sw:_ ~clock ~timeout =
  (* First check if we have queued events *)
  match t.event_queue with
  | event :: rest ->
      t.event_queue <- rest;
      `Event event
  | [] -> (
      let unix_fd = Terminal.input_fd t.terminal in
      let buf = Bytes.create 4096 in

      let do_single_read () =
        (* Wait for the FD to be readable *)
        Eio_unix.await_readable unix_fd;

        (* Read from it - this should not block since await_readable returned *)
        try
          match Unix.read unix_fd buf 0 4096 with 0 -> `Eof | n -> `Read n
        with
        | Unix.Unix_error (Unix.EINTR, _, _) -> `Read 0 (* Interrupted, retry *)
        | End_of_file -> `Eof
      in

      let rec process_events () =
        match do_single_read () with
        | `Eof -> `Eof
        | `Read n -> (
            let events = Input.feed t.parser buf 0 n in
            match events with
            | [] -> process_events () (* Incomplete event, read more *)
            | event :: rest ->
                t.event_queue <- rest;
                `Event event)
      in

      match timeout with
      | None -> process_events ()
      | Some s -> (
          match
            Eio.Time.with_timeout clock s (fun () -> Ok (process_events ()))
          with
          | Ok result -> result
          | Error `Timeout -> `Timeout))
