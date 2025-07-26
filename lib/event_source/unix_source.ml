let sigwinch = 28

let enable_seqs output_fd mouse =
  let seq =
    "\x1b[?2004h\x1b[?1004h"
    ^ if mouse then "\x1b[?1000h\x1b[?1002h\x1b[?1006h" else ""
  in
  ignore (Unix.write output_fd (Bytes.of_string seq) 0 (String.length seq))

let disable_seqs output_fd mouse =
  let seq =
    "\x1b[?2004l\x1b[?1004l"
    ^ if mouse then "\x1b[?1000l\x1b[?1002l\x1b[?1006l" else ""
  in
  ignore (Unix.write output_fd (Bytes.of_string seq) 0 (String.length seq))

let create ~sw ~env:_ ~mouse terminal =
  let input_fd = Terminal.input_fd terminal in
  let output_fd = Terminal.output_fd terminal in
  let is_tty = Unix.isatty input_fd in
  let old_attr_opt =
    if is_tty then try Some (Unix.tcgetattr input_fd) with _ -> None else None
  in
  (match old_attr_opt with
  | Some old_attr ->
      let raw_attr =
        {
          old_attr with
          c_icanon = false;
          c_echo = false;
          c_isig = false;
          c_vmin = 1;
          c_vtime = 0;
          c_ignbrk = true;
          c_inpck = false;
          c_istrip = false;
          c_icrnl = false;
          c_ixoff = false;
          c_opost = false;
          c_csize = 8;
        }
      in
      Unix.tcsetattr input_fd Unix.TCSANOW raw_attr;
      enable_seqs output_fd mouse
  | None -> ());

  let events = Eio.Stream.create 1024 in
  let pipe_r, pipe_w = Unix.pipe () in
  Unix.set_nonblock pipe_r;
  let old_sigwinch =
    Sys.signal sigwinch
      (Sys.Signal_handle
         (fun _ -> ignore (Unix.write pipe_w (Bytes.of_string "\x00") 0 1)))
  in
  let prev_size = ref (Terminal.size terminal) in
  (* Push initial resize event only for real TTYs *)
  (if is_tty then
     let w, h = !prev_size in
     Eio.Stream.add events (Input.Resize (w, h)));

  let parser = Input.create () in

  (* Input fiber: cooperative await for input *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let buf = Bytes.create 4096 in
      try
        while true do
          Eio_unix.await_readable input_fd;
          let n = Unix.read input_fd buf 0 4096 in
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
          let size = Terminal.size terminal in
          if size <> !prev_size then (
            prev_size := size;
            let w, h = size in
            Eio.Stream.add events (Input.Resize (w, h)))
        done
      with Eio.Cancel.Cancelled _ -> `Stop_daemon (* Switch release *));

  Eio.Switch.on_release sw (fun () ->
      (match old_attr_opt with
      | Some old_attr ->
          disable_seqs output_fd mouse;
          Unix.tcsetattr input_fd Unix.TCSANOW old_attr
      | None -> ());
      Sys.set_signal sigwinch old_sigwinch;
      Unix.close pipe_r;
      Unix.close pipe_w);

  events
