(* Full-loop tests: a real [Mosaic.run] application driven headlessly through
   matrix.test — virtual clock, byte-level input, frame snapshots. The driver
   is a scripted [on_idle]: each time the loop goes idle, one step runs, so
   every step observes a fully rendered, quiescent frame. *)

type model = {
  keys : string list; (* decoded key names, oldest first *)
  ticks : int;
  size : (int * int) option;
  note : string;
}

type msg = Pressed of string | Tick | Resized of int * int | Note of string

let initial = { keys = []; ticks = 0; size = None; note = "-" }

let key_name key =
  let data = Mosaic.Event.Key.data key in
  match data.Matrix.Input.Key.key with
  | Matrix.Input.Key.Char u when Uchar.is_char u ->
      String.make 1 (Uchar.to_char u)
  | Matrix.Input.Key.Up -> "<up>"
  | Matrix.Input.Key.Escape -> "<esc>"
  | _ -> "<other>"

let update msg model =
  match msg with
  | Pressed name ->
      ({ model with keys = model.keys @ [ name ] }, Mosaic.Cmd.none)
  | Tick -> ({ model with ticks = model.ticks + 1 }, Mosaic.Cmd.none)
  | Resized (w, h) -> ({ model with size = Some (w, h) }, Mosaic.Cmd.none)
  | Note note -> ({ model with note }, Mosaic.Cmd.none)

let view model =
  Mosaic.text
    (Printf.sprintf "keys:[%s] ticks:%d size:%s note:%s"
       (String.concat "," model.keys)
       model.ticks
       (match model.size with
       | None -> "-"
       | Some (w, h) -> Printf.sprintf "%dx%d" w h)
       model.note)

let app ?(subs = fun _ -> Mosaic.Sub.none) ?(init_cmd = Mosaic.Cmd.none) () =
  {
    Mosaic.init = (fun () -> (initial, init_cmd));
    update;
    view;
    subscriptions = subs;
  }

let snap t =
  Matrix_test.screen t |> String.split_on_char '\n'
  |> List.iter (fun row -> print_endline ("|" ^ row))

let drive ?(width = 48) ?(height = 2) ?probe ?process_perform application steps
    =
  let steps = ref steps in
  let on_idle t ~timeout:_ =
    match !steps with
    | [] -> Matrix_test.stop t
    | step :: rest -> (
        steps := rest;
        match step with
        | `Feed bytes -> Matrix_test.feed t bytes
        | `Resize (width, height) -> Matrix_test.resize t ~width ~height
        | `Advance dt -> Matrix_test.set_now t (Matrix_test.now t +. dt)
        | `Snap -> snap t
        | `Run f -> f t)
  in
  let t = Matrix_test.create ~on_idle ~width ~height () in
  Mosaic.run ~matrix:(Matrix_test.app t) ?process_perform ?probe application

let on_keys _ = Mosaic.Sub.on_key (fun key -> Some (Pressed (key_name key)))

let%expect_test "boots, renders, and reacts to byte-decoded input" =
  drive (app ~subs:on_keys ()) [ `Snap; `Feed "a"; `Snap; `Feed "b"; `Snap ];
  [%expect
    {||keys:[] ticks:0 size:- note:-
|
|keys:[a] ticks:0 size:- note:-
|
|keys:[a,b] ticks:0 size:- note:-
||}]

let%expect_test "escape sequences decode as single keys" =
  (* "\027[A" is three bytes but one Up key: the real parser runs. *)
  drive (app ~subs:on_keys ()) [ `Feed "\027[A"; `Snap ];
  [%expect {||keys:[<up>] ticks:0 size:- note:-
||}]

let%expect_test "virtual time drives every-subscriptions deterministically" =
  drive
    (app ~subs:(fun _ -> Mosaic.Sub.every 1.0 (fun () -> Tick)) ())
    [
      `Snap;
      `Advance 1.0;
      `Snap;
      `Advance 1.0;
      `Snap;
      `Advance 0.5;
      `Snap;
      `Advance 0.5;
      `Snap;
    ];
  [%expect
    {||keys:[] ticks:0 size:- note:-
|
|keys:[] ticks:1 size:- note:-
|
|keys:[] ticks:2 size:- note:-
|
|keys:[] ticks:2 size:- note:-
|
|keys:[] ticks:3 size:- note:-
||}]

let%expect_test "a pending every-timer renders only when it fires" =
  (* A timer is not an animation: while it is merely pending, the loop stays
     idle — no view pass, no reconcile, no paint. The view-call count is the
     witness: it must not move as virtual time approaches the deadline, and
     must move exactly once when the timer fires. *)
  let views = ref 0 in
  let base = app ~subs:(fun _ -> Mosaic.Sub.every 1.0 (fun () -> Tick)) () in
  let application =
    {
      base with
      Mosaic.view =
        (fun model ->
          incr views;
          base.Mosaic.view model);
    }
  in
  let baseline = ref 0 in
  drive application
    [
      `Run (fun _ -> baseline := !views);
      `Advance 0.4;
      `Advance 0.4;
      `Run
        (fun _ ->
          Printf.printf "renders while pending: %d\n" (!views - !baseline));
      `Advance 0.4;
      `Run
        (fun _ -> Printf.printf "renders after fire: %d\n" (!views - !baseline));
      `Snap;
    ];
  [%expect
    {|
    renders while pending: 0
    renders after fire: 1
    |keys:[] ticks:1 size:- note:-
    |
    |}]

let%expect_test "resize reaches the application and the grid" =
  drive
    (app
       ~subs:(fun _ ->
         Mosaic.Sub.on_resize (fun ~width ~height -> Resized (width, height)))
       ())
    [ `Snap; `Resize (60, 3); `Snap ];
  [%expect
    {||keys:[] ticks:0 size:48x2 note:-
|
|keys:[] ticks:0 size:60x3 note:-
|
||}]

let%expect_test "probe reports perform and message quiescence" =
  let pending : (unit -> unit) Queue.t = Queue.create () in
  let probe = ref None in
  let fact name value = Printf.printf "%s: %b\n" name value in
  let probed () = Option.get !probe in
  drive
    ~process_perform:(fun thunk -> Queue.push thunk pending)
    ~probe:(fun p -> probe := Some p)
    (app
       ~init_cmd:(Mosaic.Cmd.perform (fun dispatch -> dispatch (Note "loaded")))
       ())
    [
      `Snap;
      `Run
        (fun _ ->
          fact "performs pending while queued"
            (Mosaic.Probe.performs_pending (probed ()));
          fact "settled while perform queued"
            (Mosaic.Probe.is_settled (probed ())));
      `Run (fun _ -> Queue.pop pending ());
      `Snap;
      `Run
        (fun _ ->
          fact "settled after perform ran" (Mosaic.Probe.is_settled (probed ())));
    ];
  [%expect
    {||keys:[] ticks:0 size:- note:-
|
performs pending while queued: true
settled while perform queued: false
|keys:[] ticks:0 size:- note:loaded
|
settled after perform ran: true|}]

let%expect_test "replaying the output stream through a VTE matches the grid" =
  let result = ref None in
  drive (app ~subs:on_keys ())
    [
      `Feed "a";
      `Feed "b";
      `Run
        (fun t ->
          let vte = Vte.create ~rows:2 ~cols:48 () in
          Vte.feed_string vte (Matrix_test.output t);
          result := Some (Vte.to_string vte, Matrix_test.screen t));
    ];
  let replayed, screen = Option.get !result in
  Printf.printf "replay matches grid: %b\n"
    (String.equal (String.trim replayed) (String.trim screen));
  print_endline "replayed:";
  String.split_on_char '\n' replayed
  |> List.iter (fun row -> print_endline ("|" ^ row));
  [%expect
    {|replay matches grid: true
replayed:
|keys:[a,b] ticks:0 size:- note:-
||}]

let%expect_test "an every-timer stepped exactly onto its deadline keeps firing" =
  (* Advancing by exactly the interval makes the frame delta [now -. last_time]
     the subtraction of two accumulated floats, which can fall an ULP short of
     the interval (0.1 +. 0.1 +. ... never lands on a clean multiple). A strict
     deadline test would then skip the fire while the wakeup re-arms as
     immediately due — a busy loop. The tick count must keep climbing one per
     step, never stalling and never doubling. *)
  let steps = ref [] in
  for _ = 1 to 8 do
    steps := `Snap :: `Advance 0.1 :: !steps
  done;
  drive
    (app ~subs:(fun _ -> Mosaic.Sub.every 0.1 (fun () -> Tick)) ())
    (List.rev !steps);
  [%expect
    {||keys:[] ticks:1 size:- note:-
|
|keys:[] ticks:2 size:- note:-
|
|keys:[] ticks:3 size:- note:-
|
|keys:[] ticks:4 size:- note:-
|
|keys:[] ticks:5 size:- note:-
|
|keys:[] ticks:6 size:- note:-
|
|keys:[] ticks:7 size:- note:-
|
|keys:[] ticks:8 size:- note:-
||}]
