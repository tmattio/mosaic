let sigwinch = 28

let create ~sw ~env:_ ~mouse:_ terminal =
  (* Get the underlying Matrix TTY for compatibility *)
  let matrix_tty = Tty_eio.get_matrix_tty terminal in
  let input_fd = Tty.input_fd matrix_tty in
  let is_tty = Unix.isatty input_fd in

  let events = Eio.Stream.create 1024 in
  let pipe_r, pipe_w = Unix.pipe () in
  Unix.set_nonblock pipe_r;
  let old_sigwinch =
    Sys.signal sigwinch
      (Sys.Signal_handle
         (fun _ -> ignore (Unix.write pipe_w (Bytes.of_string "\x00") 0 1)))
  in
  let prev_size = ref (Tty.size matrix_tty) in
  (* Push initial resize event only for real TTYs *)
  (if is_tty then
     let w, h = !prev_size in
     Eio.Stream.add events (Input.Resize (w, h)));

  let parser = Input.create () in

  (* Input fiber: use Eio flow for non-blocking reads *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let buf = Bytes.create 4096 in
      try
        while true do
          (* Non-blocking read through Tty_eio *)
          let n = Tty_eio.read terminal buf 0 4096 in
          if n = 0 then raise Exit;
          (* EOF, exit fiber *)
          let parsed = Input.feed parser buf 0 n in
          List.iter (Eio.Stream.add events) parsed
        done
      with
      | Exit -> `Stop_daemon (* EOF exit *)
      | Eio.Cancel.Cancelled _ -> `Stop_daemon (* Switch release *)
      | exn -> raise exn (* Propagate errors *));

  (* SIGWINCH fiber: cooperative await for resize signals *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let dummy_buf = Bytes.create 1 in
      try
        while true do
          Eio_unix.await_readable pipe_r;
          ignore (Unix.read pipe_r dummy_buf 0 1);
          let size = Tty.size matrix_tty in
          if size <> !prev_size then (
            prev_size := size;
            let w, h = size in
            Eio.Stream.add events (Input.Resize (w, h)))
        done
      with Eio.Cancel.Cancelled _ -> `Stop_daemon (* Switch release *));

  Eio.Switch.on_release sw (fun () ->
      (* Terminal cleanup is handled by Program.cleanup *)
      Sys.set_signal sigwinch old_sigwinch;
      Unix.close pipe_r;
      Unix.close pipe_w);

  events
