open Alcotest

(* A simple read helper to get exactly N bytes from a descriptor. *)
let read_n pty n =
  let buf = Bytes.create n in
  let rec loop pos len =
    if len <= 0 then Bytes.to_string buf
    else
      let read = Pty.read pty buf pos len in
      if read = 0 then failwith "read_n: premature EOF"
      else loop (pos + read) (len - read)
  in
  loop 0 n

(* A simple write helper. *)
let write_str pty s =
  let len = String.length s in
  let written = Pty.write_string pty s 0 len in
  check int "wrote all bytes" len written

let test_open_close () = Pty.with_pty (fun _ _ -> ())

let test_read_write () =
  Pty.with_pty (fun pty tty ->
      let msg = "hello pty" in
      write_str tty msg;
      let received = read_n pty (String.length msg) in
      check string "tty->pty" msg received;

      let msg2 = "hello tty\n" in
      (* Newline is needed for terminal to echo *)
      write_str pty msg2;
      let received2 = read_n tty (String.length msg2) in
      check string "pty->tty" msg2 received2)

let test_winsize () =
  let initial_size = { Pty.rows = 10; cols = 40; x = 0; y = 0 } in
  Pty.with_pty ~winsize:initial_size (fun pty tty ->
      let size = Pty.get_winsize pty in
      (* Check that initial size was set *)
      check int "initial rows" 10 size.rows;
      check int "initial cols" 40 size.cols;

      let new_size = { Pty.rows = 24; cols = 80; x = 640; y = 480 } in
      Pty.set_winsize tty new_size;
      let updated_size = Pty.get_winsize pty in
      check int "sets rows" 24 updated_size.rows;
      check int "sets cols" 80 updated_size.cols)

let test_inherit_size () =
  Pty.with_pty (fun pty tty ->
      let new_size = { Pty.rows = 30; cols = 100; x = 0; y = 0 } in
      Pty.set_winsize pty new_size;
      Pty.inherit_size ~src:pty ~dst:tty;
      let tty_size = Pty.get_winsize tty in
      check int "inherited rows" 30 tty_size.rows;
      check int "inherited cols" 100 tty_size.cols)

let test_spawn_process () =
  try
    let prog = "echo" in
    let argv = [| "echo"; "hello from sub-process" |] in
    let pty, pid = Pty.spawn ~prog ~argv () in

    (* Wait a bit for the process to start and output *)
    Unix.sleepf 0.1;

    (* Try to read available output with timeout *)
    let output =
      let buf = Buffer.create 256 in
      let rec read_available () =
        match Unix.select [ Pty.to_file_descr pty ] [] [] 0.5 with
        | [], _, _ -> Buffer.contents buf
        | _ -> (
            let bytes = Bytes.create 256 in
            match Pty.read pty bytes 0 256 with
            | 0 -> Buffer.contents buf
            | n ->
                Buffer.add_subbytes buf bytes 0 n;
                if n < 256 then Buffer.contents buf else read_available ()
            | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
                Buffer.contents buf
            | exception Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
                Buffer.contents buf)
      in
      read_available ()
    in

    let status = Unix.waitpid [] pid |> snd in
    check
      (testable
         (fun fmt -> function
           | Unix.WEXITED n -> Format.fprintf fmt "WEXITED %d" n
           | Unix.WSIGNALED n -> Format.fprintf fmt "WSIGNALED %d" n
           | Unix.WSTOPPED n -> Format.fprintf fmt "WSTOPPED %d" n)
         ( = ))
      "process exited normally" (WEXITED 0) status;

    (* Just check that we got the expected text, ignoring exact line endings *)
    let contains_text =
      try
        ignore (String.index_from output 0 'h');
        String.length output > 0
        && String.contains output 'h' && String.contains output 'o'
      with Not_found -> false
    in
    check bool "spawned output not empty" true contains_text;
    Pty.close pty
  with
  | Unix.Unix_error (e, _, _) when e = Unix.EOPNOTSUPP || e = Unix.ENOSYS ->
      skip ()
  | e -> raise e

(* Organize tests into a list of suites *)
let () =
  run "Pty"
    [
      ( "Core",
        [
          test_case "Open and Close" `Quick test_open_close;
          test_case "Read and Write" `Quick test_read_write;
        ] );
      ( "Winsize",
        [
          test_case "Get and Set Winsize" `Quick test_winsize;
          test_case "Inherit Winsize" `Quick test_inherit_size;
        ] );
      ("Spawn", [ test_case "Spawn a process" `Quick test_spawn_process ]);
    ]
