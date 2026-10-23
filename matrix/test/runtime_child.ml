(* A program for test_runtime_pty, run in a pseudo-terminal: it draws
   READY, and acts on keys: [e] runs a shell command with the terminal given
   to it, [i] gives the terminal away while it sleeps, [a] aborts, [q]
   quits. With the arguments [served PATH], it also runs a second session on
   the terminal at [PATH], in a domain of its own, which draws SERVED; with
   [only-served PATH], it runs that session alone. *)

let ready app =
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

let served path =
  Domain.spawn (fun () ->
      let fd = Unix.openfile path [ O_RDWR; O_NOCTTY; O_CLOEXEC ] 0 in
      Matrix.run ~backend:(Matrix.Backend.unix ~input:fd ~output:fd ())
      @@ fun app ->
      let rec loop () =
        match Matrix.next app with
        | Stopped -> ()
        | Frame ->
            Matrix.draw app (fun grid ->
                Matrix.Grid.draw_text grid ~x:0 ~y:0 ~text:"SERVED");
            loop ()
        | Event _ | Timer _ | Posted _ -> loop ()
      in
      loop ())

let () =
  match Sys.argv with
  | [| _; "only-served"; path |] -> Domain.join (served path)
  | [| _; "primary" |] ->
      Matrix.run ~config:(Matrix.config ~mode:`Primary ()) ready
  | [| _; "served"; path |] ->
      let session = served path in
      Matrix.run ready;
      Domain.join session
  | _ -> Matrix.run ready
