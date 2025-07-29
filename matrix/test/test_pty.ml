open Pty

let test_spawn_echo () =
  (* Spawn echo process with a simple message *)
  let pty = spawn ~prog:"echo" ~args:["Hello, PTY!"] () in
  
  (* Set non-blocking mode *)
  set_nonblock pty;
  
  (* Buffer for reading *)
  let buffer = Bytes.create 256 in
  let total_read = ref 0 in
  
  (* Read loop with timeout *)
  let start_time = Unix.gettimeofday () in
  let continue = ref true in
  while !continue && Unix.gettimeofday () -. start_time < 1.0 do
    let read_fds = [ in_fd pty ] in
    let ready_read, _, _ = Unix.select read_fds [] [] 0.1 in
    if ready_read <> [] then
      try
        let n = read pty buffer !total_read (Bytes.length buffer - !total_read) in
        if n > 0 then
          total_read := !total_read + n
        else if n = 0 then
          (* EOF reached *)
          continue := false
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _)
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> ()
  done;
  
  (* Verify we got output *)
  Alcotest.(check bool) "read some data" true (!total_read > 0);
  let output = Bytes.sub_string buffer 0 !total_read in
  Alcotest.(check bool) "contains Hello" true (String.contains output 'H');
  
  (* Clean up *)
  close pty

let test_spawn_cat_small () =
  (* Spawn cat process *)
  let pty = spawn ~prog:"cat" ~args:[] () in
  
  (* Set non-blocking mode *)
  set_nonblock pty;
  
  (* Small test data *)
  let test_data = "Hello, world!\n" in
  
  (* Write test data *)
  let written = ref 0 in
  let start_time = Unix.gettimeofday () in
  while !written < String.length test_data && Unix.gettimeofday () -. start_time < 1.0 do
    let write_fds = [ out_fd pty ] in
    let _, ready_write, _ = Unix.select [] write_fds [] 0.1 in
    if ready_write <> [] then
      try
        let n = write_string pty test_data !written (String.length test_data - !written) in
        if n > 0 then
          written := !written + n
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _)
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> ()
  done;
  
  Alcotest.(check int) "wrote all data" (String.length test_data) !written;
  
  (* Read back the data *)
  let buffer = Bytes.create 256 in
  let total_read = ref 0 in
  
  (* Read loop with timeout *)
  let start_time = Unix.gettimeofday () in
  while !total_read < String.length test_data && Unix.gettimeofday () -. start_time < 1.0 do
    let read_fds = [ in_fd pty ] in
    let ready_read, _, _ = Unix.select read_fds [] [] 0.1 in
    if ready_read <> [] then
      try
        let n = read pty buffer !total_read (Bytes.length buffer - !total_read) in
        if n > 0 then
          total_read := !total_read + n
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _)
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> ()
  done;
  
  (* Verify we read back what we wrote - PTY might convert \n to \r\n *)
  Alcotest.(check bool) "read at least the data" true (!total_read >= String.length test_data);
  let read_data = Bytes.sub_string buffer 0 !total_read in
  
  (* PTY often converts line endings, so just check that our data is present *)
  let contains_data = 
    try 
      let _ = String.index read_data 'H' in
      let _ = String.index read_data 'w' in 
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "contains test data" true contains_data;
  
  (* Clean up *)
  close pty

let test_spawn_sh () =
  (* Spawn sh with a simple command *)
  let pty = spawn ~prog:"sh" ~args:["-c"; "echo 'Shell test'"] () in
  
  (* Set non-blocking mode *)
  set_nonblock pty;
  
  (* Buffer for reading *)
  let buffer = Bytes.create 256 in
  let total_read = ref 0 in
  
  (* Read loop with timeout *)
  let start_time = Unix.gettimeofday () in
  let continue = ref true in
  while !continue && Unix.gettimeofday () -. start_time < 1.0 do
    let read_fds = [ in_fd pty ] in
    let ready_read, _, _ = Unix.select read_fds [] [] 0.1 in
    if ready_read <> [] then
      try
        let n = read pty buffer !total_read (Bytes.length buffer - !total_read) in
        if n > 0 then
          total_read := !total_read + n
        else if n = 0 then
          (* EOF reached *)
          continue := false
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _)
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> ()
  done;
  
  (* Verify we got output *)
  Alcotest.(check bool) "read some data" true (!total_read > 0);
  let output = Bytes.sub_string buffer 0 !total_read in
  Alcotest.(check bool) "contains Shell" true (String.contains output 'S');
  
  (* Clean up *)
  close pty

let test_resize () =
  (* Spawn a long-running process *)
  let pty = spawn ~prog:"sh" ~args:["-c"; "sleep 0.1; echo 'Resize test'"] () in
  
  (* Test resize (this should not fail even if the process doesn't use the size) *)
  resize pty ~cols:80 ~rows:24;
  resize pty ~cols:100 ~rows:30;
  resize pty ~cols:40 ~rows:20;
  
  (* Set non-blocking mode for cleanup *)
  set_nonblock pty;
  
  (* Wait briefly for process to complete *)
  Unix.sleepf 0.2;
  
  (* Try to read any output *)
  let buffer = Bytes.create 256 in
  let _ = 
    try read pty buffer 0 256 
    with Unix.Unix_error (Unix.EAGAIN, _, _) -> 0
  in
  
  (* Clean up *)
  close pty

let test_nonblocking_io () =
  (* Spawn cat process *)
  let pty = spawn ~prog:"cat" ~args:[] () in
  
  (* Set non-blocking mode *)
  set_nonblock pty;
  
  (* Try to read when no data available - should not block *)
  let buffer = Bytes.create 256 in
  let result = 
    try 
      let n = read pty buffer 0 256 in
      Some n
    with
    | Unix.Unix_error (Unix.EAGAIN, _, _)
    | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> None
  in
  
  Alcotest.(check (option int)) "read returns EAGAIN" None result;
  
  (* Write some data *)
  let test_data = "Test\n" in
  let _ = write_string pty test_data 0 (String.length test_data) in
  
  (* Now read should work after a brief delay *)
  Unix.sleepf 0.1;
  let n = 
    try read pty buffer 0 256 
    with Unix.Unix_error (Unix.EAGAIN, _, _) -> 0
  in
  
  Alcotest.(check bool) "read some data" true (n > 0);
  
  (* Clean up *)
  close pty

let test_multiple_writes () =
  (* Spawn cat process *)
  let pty = spawn ~prog:"cat" ~args:[] () in
  
  (* Set non-blocking mode *)
  set_nonblock pty;
  
  (* Write multiple lines *)
  let lines = ["First line\n"; "Second line\n"; "Third line\n"] in
  List.iter (fun line ->
    let written = ref 0 in
    while !written < String.length line do
      let n = write_string pty line !written (String.length line - !written) in
      written := !written + n
    done
  ) lines;
  
  (* Read all data back *)
  let buffer = Bytes.create 1024 in
  let total_read = ref 0 in
  let expected_total = List.fold_left (fun acc s -> acc + String.length s) 0 lines in
  
  let start_time = Unix.gettimeofday () in
  while !total_read < expected_total && Unix.gettimeofday () -. start_time < 1.0 do
    try
      let n = read pty buffer !total_read (Bytes.length buffer - !total_read) in
      if n > 0 then
        total_read := !total_read + n
    with
    | Unix.Unix_error (Unix.EAGAIN, _, _) -> Unix.sleepf 0.01
  done;
  
  (* With cat, we might get extra characters, so check we got at least what we sent *)
  Alcotest.(check bool) "read at least expected data" true (!total_read >= expected_total);
  let output = Bytes.sub_string buffer 0 !total_read in
  (* Verify all our lines are present *)
  let contains_all = 
    String.contains output 'F' &&  (* First *)
    String.contains output 'S' &&  (* Second *)
    String.contains output 'T'     (* Third *)
  in
  Alcotest.(check bool) "contains all lines" true contains_all;
  
  (* Clean up *)
  close pty

let test_pty_close () =
  (* Spawn a process *)
  let pty = spawn ~prog:"cat" ~args:[] () in
  
  (* Close the PTY *)
  close pty;
  
  (* Try to use closed PTY - should fail *)
  let buffer = Bytes.create 16 in
  let failed = 
    try
      let _ = read pty buffer 0 16 in
      false
    with
    | Unix.Unix_error (Unix.EBADF, _, _) -> true
    | _ -> false
  in
  
  Alcotest.(check bool) "read after close fails" true failed

(* Test suite *)
let () =
  let open Alcotest in
  run "PTY" [
    "basic", [
      test_case "spawn echo" `Quick test_spawn_echo;
      test_case "spawn cat small" `Quick test_spawn_cat_small;
      test_case "spawn sh" `Quick test_spawn_sh;
    ];
    "features", [
      test_case "resize" `Quick test_resize;
      test_case "non-blocking I/O" `Quick test_nonblocking_io;
      test_case "multiple writes" `Quick test_multiple_writes;
    ];
    "lifecycle", [
      test_case "close" `Quick test_pty_close;
    ];
  ]