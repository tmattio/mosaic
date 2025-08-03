open Unix

let run_vttest () =
  (* Create VTE with standard size *)
  let rows = 24 in
  let cols = 80 in
  let vte = Vte.create ~rows ~cols () in

  (* Set up window size *)
  let winsize = Pty.{ rows; cols; x = 0; y = 0 } in

  (* Set up environment *)
  let env =
    Array.append (Unix.environment ())
      [|
        "TERM=xterm";
        "LINES=" ^ string_of_int rows;
        "COLUMNS=" ^ string_of_int cols;
      |]
  in

  (* Spawn vttest in a PTY *)
  let pty = Pty.spawn ~env ~winsize ~prog:"vttest" ~args:[] () in

  (* Set up non-blocking I/O *)
  let pty_fd = Pty.out_fd pty in
  set_nonblock stdin;
  set_nonblock pty_fd;

  (* Terminal setup *)
  let old_termios = try Some (tcgetattr stdin) with _ -> None in

  (* Set raw mode *)
  (try
     let termios = tcgetattr stdin in
     (* Raw mode: no echo, no canonical processing, etc *)
     termios.c_echo <- false;
     termios.c_icanon <- false;
     termios.c_vmin <- 1;
     termios.c_vtime <- 0;
     tcsetattr stdin TCSANOW termios
   with _ -> ());

  (* Buffer for I/O *)
  let buffer = Bytes.create 4096 in
  let last_output = ref "" in

  (* Main loop *)
  let running = ref true in

  (* Set up signal handler *)
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> running := false));

  while !running do
    (* Use select to wait for input *)
    let read_fds = [ stdin; pty_fd ] in
    let ready, _, _ =
      try select read_fds [] [] 0.05
      with Unix_error (EINTR, _, _) -> ([], [], [])
    in

    (* Read from stdin and write to PTY *)
    (if List.mem stdin ready then
       try
         let n = read stdin buffer 0 (Bytes.length buffer) in
         if n > 0 then
           (* Check for Ctrl-C *)
           let data = Bytes.sub_string buffer 0 n in
           if String.contains data '\003' then running := false
           else
             let _ = Pty.write pty buffer 0 n in
             ()
       with
       | Unix_error (EAGAIN, _, _) | Unix_error (EWOULDBLOCK, _, _) -> ()
       | End_of_file -> running := false);

    (* Read from PTY and process through VTE *)
    if List.mem pty_fd ready then
      try
        let n = Pty.read pty buffer 0 (Bytes.length buffer) in
        if n > 0 then (
          (* Feed to VTE *)
          Vte.feed vte buffer 0 n;

          (* Get the VTE rendering *)
          let output = Vte.to_string_grid vte in

          (* Only update display if content changed *)
          if output <> !last_output then (
            last_output := output;
            (* Clear screen and display VTE output *)
            Printf.printf "\x1b[2J\x1b[H";
            print_string output;

            (* Show cursor position for debugging *)
            let row, col = Vte.cursor_pos vte in
            Printf.printf "\x1b[%d;%dH" (row + 1) (col + 1);
            flush Stdlib.stdout))
        else if n = 0 then
          (* EOF from PTY - vttest exited *)
          running := false
      with
      | Unix_error (EAGAIN, _, _) | Unix_error (EWOULDBLOCK, _, _) -> ()
      | End_of_file -> running := false
  done;

  (* Restore terminal *)
  (match old_termios with
  | Some t -> tcsetattr stdin TCSANOW t
  | None -> ());

  (* Clean up *)
  Pty.close pty;
  Printf.printf "\n\nVTE test completed.\n"

let () =
  Printf.printf "Starting vttest through VTE...\n";
  Printf.printf "Press Ctrl-C to exit\n\n";
  flush Stdlib.stdout;
  Unix.sleep 1;

  try run_vttest ()
  with e ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);
    exit 1
