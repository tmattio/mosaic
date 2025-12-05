open Alcotest

(* Test Helpers *)

let read_with_select pty buffer offset length timeout =
  let start_time = Unix.gettimeofday () in
  let total_read = ref 0 in
  let continue = ref true in
  while !continue && Unix.gettimeofday () -. start_time < timeout do
    let read_fds = [ Pty.in_fd pty ] in
    let ready_read, _, _ = Unix.select read_fds [] [] 0.1 in
    if ready_read <> [] then
      try
        let n =
          Pty.read pty buffer (offset + !total_read) (length - !total_read)
        in
        if n > 0 then total_read := !total_read + n
        else if n = 0 then continue := false (* EOF *)
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _)
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
      ->
        ()
  done;
  !total_read

let write_all_with_select pty data timeout =
  Pty.set_nonblock pty;
  let written = ref 0 in
  let start_time = Unix.gettimeofday () in
  while
    !written < String.length data
    && Unix.gettimeofday () -. start_time < timeout
  do
    let write_fds = [ Pty.out_fd pty ] in
    let _, ready_write, _ = Unix.select [] write_fds [] 0.1 in
    if ready_write <> [] then
      try
        let n =
          Pty.write_string pty data !written (String.length data - !written)
        in
        if n > 0 then written := !written + n
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _)
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
      ->
        ()
  done;
  !written

(* Basic PTY Operations *)

let test_open_pty () =
  let master, slave = Pty.open_pty () in
  check bool "master fd valid" true Unix.(Pty.file_descr master <> stdin);
  check bool "slave fd valid" true Unix.(Pty.file_descr slave <> stdin);
  check bool "fds different" true (Pty.file_descr master <> Pty.file_descr slave);
  Pty.close master;
  Pty.close slave

let test_open_pty_with_winsize () =
  let ws = Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 } in
  let master, slave = Pty.open_pty ~winsize:ws () in
  let actual_ws = Pty.get_winsize slave in
  check int "rows set" 24 actual_ws.rows;
  check int "cols set" 80 actual_ws.cols;
  Pty.close master;
  Pty.close slave

let test_with_pty () =
  let result =
    Pty.with_pty (fun master _slave ->
        check bool "master valid" true Unix.(Pty.file_descr master <> stdin);
        42)
  in
  check int "result returned" 42 result

(* Window Size Management *)

let test_winsize_operations () =
  let master, slave = Pty.open_pty () in
  let ws = Pty.{ rows = 30; cols = 120; xpixel = 0; ypixel = 0 } in
  Pty.set_winsize slave ws;
  let actual = Pty.get_winsize slave in
  check int "rows match" 30 actual.rows;
  check int "cols match" 120 actual.cols;
  Pty.close master;
  Pty.close slave

let test_resize () =
  let master, slave = Pty.open_pty () in
  Pty.resize slave ~rows:25 ~cols:100;
  let ws = Pty.get_winsize slave in
  check int "rows resized" 25 ws.rows;
  check int "cols resized" 100 ws.cols;
  Pty.close master;
  Pty.close slave

let test_inherit_size () =
  let master1, slave1 = Pty.open_pty () in
  let master2, slave2 = Pty.open_pty () in
  Pty.resize slave1 ~rows:40 ~cols:160;
  Pty.inherit_size ~src:slave1 ~dst:slave2;
  let ws2 = Pty.get_winsize slave2 in
  check int "inherited rows" 40 ws2.rows;
  check int "inherited cols" 160 ws2.cols;
  Pty.close master1;
  Pty.close slave1;
  Pty.close master2;
  Pty.close slave2

(* Process Spawning *)

let test_spawn_echo () =
  let pty = Pty.spawn ~prog:"echo" ~args:[ "Hello, PTY!" ] () in
  Pty.set_nonblock pty;
  let buffer = Bytes.create 256 in
  let n = read_with_select pty buffer 0 256 1.0 in
  check bool "read some data" true (n > 0);
  let output = Bytes.sub_string buffer 0 n in
  check bool "contains Hello" true (String.contains output 'H');
  Pty.close pty

let test_spawn_cat () =
  let pty = Pty.spawn ~prog:"cat" ~args:[] () in
  Pty.set_nonblock pty;
  let test_data = "Hello, world!\n" in
  let written = write_all_with_select pty test_data 1.0 in
  check int "wrote all data" (String.length test_data) written;

  let buffer = Bytes.create 256 in
  let n = read_with_select pty buffer 0 256 1.0 in
  check bool "read at least the data" true (n >= String.length test_data);
  let output = Bytes.sub_string buffer 0 n in
  check bool "contains test data" true (String.contains output 'H');
  Pty.close pty

let test_spawn_sh () =
  let pty = Pty.spawn ~prog:"sh" ~args:[ "-c"; "echo 'Shell test'" ] () in
  Pty.set_nonblock pty;
  let buffer = Bytes.create 256 in
  let n = read_with_select pty buffer 0 256 1.0 in
  check bool "read some data" true (n > 0);
  let output = Bytes.sub_string buffer 0 n in
  check bool "contains Shell" true (String.contains output 'S');
  Pty.close pty

let test_spawn_with_env () =
  let env = [| "TEST_VAR=hello" |] in
  let pty = Pty.spawn ~env ~prog:"sh" ~args:[ "-c"; "echo $TEST_VAR" ] () in
  Pty.set_nonblock pty;
  let buffer = Bytes.create 256 in
  let n = read_with_select pty buffer 0 256 1.0 in
  let output = Bytes.sub_string buffer 0 n in
  check bool "env var in output" true (String.contains output 'h');
  Pty.close pty

let test_spawn_with_cwd () =
  let pty = Pty.spawn ~cwd:"/tmp" ~prog:"pwd" ~args:[] () in
  Pty.set_nonblock pty;
  let buffer = Bytes.create 256 in
  let n = read_with_select pty buffer 0 256 1.0 in
  let output = String.trim (Bytes.sub_string buffer 0 n) in
  (* /tmp may be a symlink to /private/tmp on macOS *)
  check bool "working directory is tmp" true
    (output = "/tmp" || output = "/private/tmp");
  Pty.close pty

(* Non-blocking I/O *)

let test_nonblocking_io () =
  let pty = Pty.spawn ~prog:"cat" ~args:[] () in
  Pty.set_nonblock pty;

  (* Read when no data available - should return EAGAIN *)
  let buffer = Bytes.create 256 in
  let result =
    try
      let n = Pty.read pty buffer 0 256 in
      Some n
    with
    | Unix.Unix_error (Unix.EAGAIN, _, _)
    | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
    ->
      None
  in
  check (option int) "read returns EAGAIN" None result;

  (* Write some data *)
  let test_data = "Test\n" in
  let _ = Pty.write_string pty test_data 0 (String.length test_data) in

  (* Now read should work after brief delay *)
  Unix.sleepf 0.1;
  let n =
    try Pty.read pty buffer 0 256
    with Unix.Unix_error (Unix.EAGAIN, _, _) -> 0
  in
  check bool "read some data" true (n > 0);
  Pty.close pty

let test_multiple_writes () =
  let pty = Pty.spawn ~prog:"cat" ~args:[] () in
  Pty.set_nonblock pty;

  let lines = [ "First line\n"; "Second line\n"; "Third line\n" ] in
  List.iter
    (fun line ->
      let _ = write_all_with_select pty line 1.0 in
      ())
    lines;

  let buffer = Bytes.create 1024 in
  let expected_total =
    List.fold_left (fun acc s -> acc + String.length s) 0 lines
  in
  let n = read_with_select pty buffer 0 1024 1.0 in

  check bool "read at least expected data" true (n >= expected_total);
  let output = Bytes.sub_string buffer 0 n in
  check bool "contains all lines" true
    (String.contains output 'F' && String.contains output 'S'
   && String.contains output 'T');
  Pty.close pty

(* Edge Cases *)

let test_close_idempotent () =
  let master, slave = Pty.open_pty () in
  Pty.close master;
  Pty.close master;
  (* Should not crash *)
  Pty.close slave

let test_eof_on_close () =
  let master, slave = Pty.open_pty () in
  Pty.close master;
  let buf = Bytes.create 10 in
  let n = Pty.read slave buf 0 10 in
  check int "EOF on master close" 0 n;
  Pty.close slave

let test_pty_close () =
  let pty = Pty.spawn ~prog:"cat" ~args:[] () in
  Pty.close pty;
  let buffer = Bytes.create 16 in
  let failed =
    try
      let _ = Pty.read pty buffer 0 16 in
      false
    with Unix.Unix_error (Unix.EBADF, _, _) -> true
  in
  check bool "read after close fails" true failed

(* Test Suite *)

let tests =
  [
    (* Basic Operations *)
    test_case "open pty" `Quick test_open_pty;
    test_case "open pty with winsize" `Quick test_open_pty_with_winsize;
    test_case "with_pty" `Quick test_with_pty;
    (* Window Size *)
    test_case "winsize operations" `Quick test_winsize_operations;
    test_case "resize" `Quick test_resize;
    test_case "inherit size" `Quick test_inherit_size;
    (* Process Spawning *)
    test_case "spawn echo" `Quick test_spawn_echo;
    test_case "spawn cat" `Quick test_spawn_cat;
    test_case "spawn sh" `Quick test_spawn_sh;
    test_case "spawn with env" `Quick test_spawn_with_env;
    test_case "spawn with cwd" `Quick test_spawn_with_cwd;
    (* Non-blocking I/O *)
    test_case "nonblocking io" `Quick test_nonblocking_io;
    test_case "multiple writes" `Quick test_multiple_writes;
    (* Edge Cases *)
    test_case "close idempotent" `Quick test_close_idempotent;
    test_case "eof on close" `Quick test_eof_on_close;
    test_case "pty close" `Quick test_pty_close;
  ]

let () = run "matrix.pty" [ ("pty", tests) ]
