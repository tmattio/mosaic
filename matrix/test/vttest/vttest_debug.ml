(* Debug runner for vttest - shows escape sequences and VTE state *)
open Unix

let () =
  let vte = Vte.create ~rows:24 ~cols:80 () in
  let winsize = Pty.{ rows = 24; cols = 80; x = 0; y = 0 } in
  let env = Array.append (Unix.environment ()) 
    [| "TERM=vt100"; "LINES=24"; "COLUMNS=80" |] in
  let pty = Pty.spawn ~env ~winsize ~prog:"vttest" ~args:[] () in
  let pty_fd = Pty.out_fd pty in
  set_nonblock pty_fd;
  
  let buffer = Bytes.create 4096 in
  
  Printf.printf "Starting vttest debug runner...\n";
  Printf.printf "Waiting for Device Attributes request...\n\n";
  
  (* Initial read loop *)
  Unix.sleep 1;
  let rec read_loop attempts =
    if attempts > 0 then (
      let ready, _, _ = select [pty_fd] [] [] 0.5 in
      if List.mem pty_fd ready then (
        try
          let n = Pty.read pty buffer 0 (Bytes.length buffer) in
          if n > 0 then (
            Printf.printf "Read %d bytes\n" n;
            
            (* Check for Device Attributes *)
            let data = Bytes.sub_string buffer 0 n in
            if String.contains data 'c' && String.contains data '\x1b' then (
              Printf.printf "Device Attributes detected, responding...\n";
              let response = "\x1b[?1;0c" in
              let _ = Pty.write pty (Bytes.of_string response) 0 (String.length response) in
              ()
            );
            
            (* Feed to VTE *)
            Vte.feed vte buffer 0 n;
            
            (* Continue reading *)
            Unix.sleep 1;
            read_loop (attempts - 1)
          )
        with _ -> ()
      ) else read_loop (attempts - 1)
    )
  in
  
  read_loop 10;
  
  Printf.printf "\n=== Final screen content ===\n";
  print_string (Vte.to_string_grid vte);
  Printf.printf "\n";
  
  let row, col = Vte.cursor_pos vte in
  Printf.printf "Cursor position: (%d, %d)\n" row col;
  
  Pty.close pty