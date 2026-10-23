(** A program fed by a thread it did not start, and stopped by another. *)

open Mosaic

type model = { lines : string list; paused : bool }
type msg = Line of string | Toggle | Quit

let init () = ({ lines = []; paused = false }, Cmd.none)

let update msg model =
  match msg with
  | Line line when not model.paused ->
      (* The last twenty lines. *)
      let lines = model.lines @ [ line ] in
      let extra = List.length lines - 20 in
      ( { model with lines = List.filteri (fun i _ -> i >= extra) lines },
        Cmd.none )
  | Line _ -> (model, Cmd.none)
  | Toggle -> ({ model with paused = not model.paused }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let view model =
  box ~flex_direction:Column ~padding:(padding 1) ~gap:(gap 1)
    [
      text ~style:(Ansi.Style.make ~bold:true ()) "Thread feed";
      box ~flex_direction:Column (List.map (fun line -> text line) model.lines);
      text ~style:muted
        (if model.paused then "p resume  •  q quit" else "p pause  •  q quit");
    ]

let subscriptions _ =
  Sub.on_keys
    [
      (Shortcut.char 'p', Toggle);
      (Shortcut.char 'q', Quit);
      (Shortcut.escape, Quit);
    ]

(* Returns once one of the program's descriptors is readable or its
   deadline comes, as Mosaic.run waits. *)
let wait program =
  let timeout =
    match Mosaic.deadline program with
    | None -> -1.
    | Some deadline ->
        Float.max 0. (Float.of_int (deadline - Matrix.clock ()) *. 1e-9)
  in
  try ignore (Unix.select (Mosaic.fds program) [] [] timeout)
  with Unix.Unix_error (EINTR, _, _) -> ()

(* A thread that is not the program's work gives it lines with [dispatch].
   It starts once the program runs, and ends with it: [dispatch] does
   nothing after the run, and the thread sees [over]. With [--for SECONDS],
   a watchdog thread stops the program after that long, with the stop
   request made before the program started. *)
let () =
  let limit =
    match Sys.argv with
    | [| _; "--for"; seconds |] -> Some (float_of_string seconds)
    | _ -> None
  in
  let stop = Matrix.Stop.create () in
  Option.iter
    (fun seconds ->
      ignore
        (Thread.create
           (fun () ->
             Thread.delay seconds;
             Matrix.Stop.request stop)
           ()
          : Thread.t))
    limit;
  let over = Atomic.make false in
  Mosaic.start ~stop { init; update; view; subscriptions } @@ fun program ->
  let feed =
    Thread.create
      (fun () ->
        let n = ref 0 in
        while not (Atomic.get over) do
          incr n;
          Mosaic.dispatch program
            (Line (Printf.sprintf "line %d from a thread" !n));
          Thread.delay 0.25
        done)
      ()
  in
  let rec loop () =
    match Mosaic.step program ~now:(Matrix.clock ()) with
    | `Stepped -> loop ()
    | `Idle ->
        wait program;
        loop ()
    | `Stopped -> ()
  in
  Fun.protect
    ~finally:(fun () ->
      Atomic.set over true;
      Thread.join feed)
    loop
