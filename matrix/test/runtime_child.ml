(* A program for test_runtime_pty, run in a pseudo-terminal: it draws
   READY, and acts on keys: [e] runs a shell command with the terminal given
   to it, [i] gives the terminal away while it sleeps, [a] aborts, [q]
   quits. *)

let () =
  Matrix.run @@ fun app ->
  let frames = ref 0 in
  let rec loop () =
    match Matrix.next app with
    | Stopped -> ()
    | Frame ->
        incr frames;
        Matrix.draw app (fun grid ->
            Matrix.Grid.draw_text grid ~x:0 ~y:0
              ~text:(Printf.sprintf "READY %d" !frames));
        loop ()
    | Event (Key { key = Char c; _ }) -> (
        match Uchar.to_int c with
        | 0x65 (* e *) ->
            Matrix.release app (fun () ->
                ignore (Sys.command "stty -a; printf CHILD" : int));
            loop ()
        | 0x69 (* i *) ->
            Matrix.release app (fun () -> Unix.sleepf 0.5);
            loop ()
        | 0x61 (* a *) ->
            Unix.kill (Unix.getpid ()) Sys.sigabrt;
            loop ()
        | 0x71 (* q *) -> ()
        | _ -> loop ())
    | Event _ | Timer _ | Posted _ -> loop ()
  in
  loop ()
