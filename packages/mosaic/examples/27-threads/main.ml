(** A program fed by a worker thread, and stopped by a thread it did not start.

    A thread the program did not start, such as a library's callback thread,
    gives it messages with the [dispatch] of a [Cmd.perform], which it may keep
    after the perform returns.

    p pauses or resumes the feed; q or Esc quits. *)

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

(* A worker gives the program a line every quarter second, on a thread
   of its own, until its stop is requested when the program ends. *)
let feed stop { dispatch; _ } =
  let n = ref 0 in
  while not (Sub.Stop.requested stop) do
    incr n;
    dispatch (Line (Printf.sprintf "line %d from a thread" !n));
    Thread.delay 0.25
  done

let subscriptions _ =
  Sub.batch
    [
      Sub.worker ~id:"feed" feed;
      Sub.on_keys
        [
          (Shortcut.char 'p', Toggle);
          (Shortcut.char 'q', Quit);
          (Shortcut.escape, Quit);
        ];
    ]

(* With [--for SECONDS], a watchdog thread the program did not start stops
   it after that long, with a stop request made before the program
   started. *)
let () =
  let stop = Matrix.Stop.create () in
  (match Sys.argv with
  | [| _; "--for"; seconds |] ->
      let seconds = float_of_string seconds in
      ignore
        (Thread.create
           (fun () ->
             Thread.delay seconds;
             Matrix.Stop.request stop)
           ()
          : Thread.t)
  | _ -> ());
  Mosaic.run ~stop { init; update; view; subscriptions }
