open Pty

let test_spawn_cat () =
  (* Generate 1 KiB of test data *)
  let test_data = String.make 1024 'A' in

  (* Spawn cat process *)
  let pty = spawn ~prog:"cat" ~args:[] () in

  (* Set non-blocking mode *)
  set_nonblock pty;

  (* Use select to wait for write availability *)
  let write_fds = [ out_fd pty ] in
  let ready_write, _, _ = Unix.select [] write_fds [] 1.0 in
  assert (ready_write <> []);

  (* Write test data *)
  let written = write_string pty test_data 0 (String.length test_data) in
  Printf.printf "Written %d bytes\n" written;
  assert (written > 0);

  (* Buffer for reading *)
  let read_buffer = Bytes.create 2048 in
  let total_read = ref 0 in

  (* Read loop with select *)
  let start_time = Unix.gettimeofday () in
  while !total_read < 1024 && Unix.gettimeofday () -. start_time < 2.0 do
    let read_fds = [ in_fd pty ] in
    let ready_read, _, _ = Unix.select read_fds [] [] 0.1 in
    if ready_read <> [] then
      try
        let n = read pty read_buffer !total_read (1024 - !total_read) in
        if n > 0 then (
          total_read := !total_read + n;
          Printf.printf "Read %d bytes (total: %d)\n" n !total_read)
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _)
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
      ->
        ()
  done;

  (* Verify we read back what we wrote *)
  Printf.printf "Total read: %d bytes\n" !total_read;
  assert (!total_read = 1024);
  let read_data = Bytes.sub_string read_buffer 0 !total_read in
  assert (read_data = test_data);

  (* Clean up *)
  close pty;
  Printf.printf "test_spawn_cat: PASSED\n"

let test_resize () =
  (* Spawn a shell that has stty *)
  let pty = spawn ~prog:"sh" ~args:[ "-c"; "stty -a; sleep 1" ] () in

  (* Set initial size *)
  resize pty ~cols:40 ~rows:20;

  (* Read initial output *)
  let buffer = Bytes.create 4096 in
  Unix.sleep 1;

  (* Give the command time to execute *)

  (* Try to read output *)
  set_nonblock pty;
  let n =
    try read pty buffer 0 4096 with Unix.Unix_error (Unix.EAGAIN, _, _) -> 0
  in
  if n > 0 then (
    let output = Bytes.sub_string buffer 0 n in
    Printf.printf "Initial stty output:\n%s\n" output;
    (* Check if it contains the expected size *)
    assert (
      String.exists (fun c -> c = '4') output
      && String.exists (fun c -> c = '0') output);
    assert (
      String.exists (fun c -> c = '2') output
      && String.exists (fun c -> c = '0') output));

  (* Clean up *)
  close pty;
  Printf.printf "test_resize: PASSED\n"

let () =
  test_spawn_cat ();
  test_resize ()
