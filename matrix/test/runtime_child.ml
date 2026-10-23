(* A program for test_runtime_pty, run in a pseudo-terminal: it draws
   READY, and acts on keys: [e] runs a shell command with the terminal given
   to it, [i] gives the terminal away while it sleeps, [a] aborts, [q]
   quits. With the arguments [served PATH], it also runs a second session on
   the terminal at [PATH], in a domain of its own, which draws SERVED; with
   [only-served PATH], it runs that session alone. With [keys], it draws
   every letter typed, until [q]. With [goodbye], it draws its size until
   [q], then prints GOODBYE on the primary screen. With [fill], each letter
   typed fills the screen with it, under a first row that names it, until
   [q]. Without arguments, [x] calls [exit 3] inside the run. *)

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
        | 0x78 (* x *) -> exit 3
        | _ -> loop ())
    | Event _ | Timer _ | Posted _ -> loop ()
  in
  loop ()

let keys app =
  let typed = Buffer.create 64 in
  let rec loop () =
    match Matrix.next app with
    | Stopped -> ()
    | Frame ->
        Matrix.draw app (fun grid ->
            Matrix.Grid.draw_text grid ~x:0 ~y:0
              ~text:("KEYS:" ^ Buffer.contents typed ^ "."));
        loop ()
    | Event (Key { key = Char c; _ }) when Uchar.to_int c = 0x71 (* q *) -> ()
    | Event (Key { key = Char c; _ }) when Uchar.to_int c < 0x80 ->
        Buffer.add_char typed (Char.chr (Uchar.to_int c));
        Matrix.request_redraw app;
        loop ()
    | Event _ | Timer _ | Posted _ -> loop ()
  in
  loop ()

let goodbye app =
  let rec loop () =
    match Matrix.next app with
    | Stopped -> ()
    | Frame ->
        let cols, rows = Matrix.size app in
        Matrix.draw app (fun grid ->
            Matrix.Grid.draw_text grid ~x:0 ~y:0
              ~text:(Printf.sprintf "SIZE %dx%d" cols rows));
        loop ()
    | Event (Key { key = Char c; _ }) when Uchar.to_int c = 0x71 (* q *) -> ()
    | Event _ | Timer _ | Posted _ -> loop ()
  in
  loop ()

let fill app =
  let letter = ref '.' in
  let rec loop () =
    match Matrix.next app with
    | Stopped -> ()
    | Frame ->
        let cols, rows = Matrix.size app in
        let row = String.make cols !letter in
        Matrix.draw app (fun grid ->
            for y = 1 to rows - 1 do
              Matrix.Grid.draw_text grid ~x:0 ~y ~text:row
            done;
            Matrix.Grid.draw_text grid ~x:0 ~y:0
              ~text:(Printf.sprintf "FILLED %c" !letter));
        loop ()
    | Event (Key { key = Char c; _ }) when Uchar.to_int c = 0x71 (* q *) -> ()
    | Event (Key { key = Char c; _ }) when Uchar.to_int c < 0x80 ->
        letter := Char.chr (Uchar.to_int c);
        Matrix.request_redraw app;
        loop ()
    | Event _ | Timer _ | Posted _ -> loop ()
  in
  loop ()

let served path =
  Domain.spawn (fun () ->
      let fd = Unix.openfile path [ O_RDWR; O_NOCTTY; O_CLOEXEC ] 0 in
      Matrix.run ~input:fd ~output:fd @@ fun app ->
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
  | [| _; "keys" |] -> Matrix.run keys
  | [| _; "fill" |] -> Matrix.run fill
  | [| _; "goodbye" |] ->
      Matrix.run goodbye;
      print_string "GOODBYE\n"
  | [| _; "primary" |] ->
      Matrix.run ~config:(Matrix.config ~mode:`Primary ()) ready
  | [| _; "served"; path |] ->
      let session = served path in
      Matrix.run ready;
      Domain.join session
  | _ -> Matrix.run ready
