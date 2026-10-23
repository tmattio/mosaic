let descriptor resource = Eio_unix.Fd.use_exn "Matrix_eio" resource Fun.id

let backend ~clock ~stdin ~stdout () =
  let input = descriptor (Eio_unix.Resource.fd stdin) in
  let output = descriptor (Eio_unix.Resource.fd stdout) in
  let unix = Matrix.Backend.unix ~input ~output () in
  let wake_fd = Matrix.Backend.wake_fd () in
  let input_cs = Cstruct.create 4096 in
  let read_input buf off len =
    let cs =
      if len >= Cstruct.length input_cs then input_cs
      else Cstruct.sub input_cs 0 len
    in
    match Eio.Flow.single_read stdin cs with
    | n ->
        Cstruct.blit_to_bytes cs 0 buf off n;
        n
    | exception End_of_file -> 0
  in
  let write_output buf off len =
    Eio.Flow.write stdout [ Cstruct.of_bytes ~off ~len buf ]
  in
  let drain () =
    let buf = Bytes.create 64 in
    let rec go () =
      match Unix.read wake_fd buf 0 64 with
      | 64 -> go ()
      | _ -> ()
      | exception Unix.Unix_error _ -> ()
    in
    go ()
  in
  (* Input, wake and deadline race as peers; the first to finish cancels the
     others. *)
  let wait ~until ~reading =
    let input () =
      Eio_unix.await_readable input;
      true
    in
    let woken () =
      Eio_unix.await_readable wake_fd;
      false
    in
    let deadline until () =
      let seconds = Float.of_int (until - unix.now ()) *. 1e-9 in
      if seconds > 0. then Eio.Time.Mono.sleep clock seconds;
      false
    in
    let peers = [ woken ] in
    let peers = if reading then input :: peers else peers in
    let peers =
      match until with Some until -> deadline until :: peers | None -> peers
    in
    let readable = Eio.Fiber.any peers in
    drain ();
    readable
  in
  { unix with wait; read_input; write_output; protect = Eio.Cancel.protect }
