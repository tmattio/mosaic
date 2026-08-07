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

let substring_index needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec matches_at i j =
    j = needle_len
    || i + j < haystack_len
       && Char.equal
            (String.unsafe_get haystack (i + j))
            (String.unsafe_get needle j)
       && matches_at i (j + 1)
  in
  let rec loop i =
    if i + needle_len > haystack_len then None
    else if matches_at i 0 then Some i
    else loop (i + 1)
  in
  if needle_len = 0 then Some 0 else loop 0

let is_before ~first ~second s =
  match (substring_index first s, substring_index second s) with
  | Some i, Some j -> i < j
  | _ -> false

let drive ?mode ?(width = 48) ?(height = 2) ?probe ?process_perform application
    steps =
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
  let t = Matrix_test.create ?mode ~on_idle ~width ~height () in
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

let%expect_test "in-frame geometry changes settle without input" =
  let base = app () in
  let application =
    {
      base with
      Mosaic.view =
        (fun _model ->
          Mosaic.scroll_box
            ~size:(Mosaic.size ~width:20 ~height:4)
            (List.init 10 (fun i -> Mosaic.text (Printf.sprintf "Line %d" i))));
    }
  in
  drive ~width:20 ~height:4 application [ `Snap ];
  [%expect_exact
    {||Line 0             █
|Line 1             ▀
|Line 2
|Line 3
|}]

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

let%expect_test "live animation frames deliver a millisecond delta" =
  (* Matrix reports elapsed seconds; the runtime converts once at the boundary
     so the whole render pipeline (renderable.mli) runs on milliseconds. While
     a live widget (the spinner) animates, the virtual clock advances at the
     60 fps render cadence, so widgets must observe 1000/60 = 16.7 ms frames
     (a runtime that forgot to convert would deliver 0.0). A canvas records
     the delta it is drawn with; once the timer fires the spinner leaves the
     view, the loop goes idle, and the recording is checked. *)
  let last_delta = ref Float.nan in
  let application =
    {
      (app ~subs:(fun _ -> Mosaic.Sub.every 0.2 (fun () -> Tick)) ()) with
      Mosaic.view =
        (fun model ->
          if model.ticks = 0 then
            Mosaic.box
              [
                Mosaic.spinner ();
                Mosaic.canvas ~size:(Mosaic.size ~width:2 ~height:1)
                  (fun _ ~delta -> last_delta := delta);
              ]
          else Mosaic.text "done");
    }
  in
  drive application
    [ `Run (fun _ -> Printf.printf "live frame delta: %.1f ms\n" !last_delta) ];
  [%expect {|live frame delta: 16.7 ms|}]

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

let%expect_test "a transparent table realigns every row when it widens" =
  let columns =
    [
      Mosaic.Table.column ~width:(`Fixed 2) "";
      Mosaic.Table.column ~width:(`Flex 4.) "session";
      Mosaic.Table.column ~width:`Auto "lifecycle";
      Mosaic.Table.column ~width:`Auto "phase";
      Mosaic.Table.column ~width:`Auto ~alignment:`Right "turns";
      Mosaic.Table.column ~width:`Auto "updated";
      Mosaic.Table.column ~width:`Auto "recency";
    ]
  in
  let rows =
    [
      [|
        Mosaic.Table.cell "❯";
        Mosaic.Table.cell "Retained parser investigation";
        Mosaic.Table.cell "active";
        Mosaic.Table.cell "idle";
        Mosaic.Table.cell "1 turn";
        Mosaic.Table.cell "just now";
        Mosaic.Table.cell "today";
      |];
      [|
        Mosaic.Table.cell "";
        Mosaic.Table.cell "session-visual-2";
        Mosaic.Table.cell "active";
        Mosaic.Table.cell "idle";
        Mosaic.Table.cell "1 turn";
        Mosaic.Table.cell "just now";
        Mosaic.Table.cell "today";
      |];
    ]
  in
  let base = app ~subs:on_keys () in
  let application =
    {
      base with
      Mosaic.view =
        (fun model ->
          let width = if model.keys = [] then 76 else 80 in
          Mosaic.table ~columns ~rows ~selected_row:0 ~border:false
            ~show_header:true ~cell_padding:0
            ~selected_background:(Matrix.Ansi.Color.grayscale ~level:3)
            ~size:(Mosaic.size ~width ~height:3)
            ());
    }
  in
  drive ~width:80 ~height:3 application [ `Snap; `Feed "w"; `Snap ];
  [%expect
    {||   session                           lifecycle phase  turns updated  recency
|❯  Retained parser investigation     active    idle  1 turn just now today
|   session-visual-2                  active    idle  1 turn just now today
|   session                               lifecycle phase  turns updated  recency
|❯  Retained parser investigation         active    idle  1 turn just now today
|   session-visual-2                      active    idle  1 turn just now today|}]

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
          Printf.printf "pending while queued: %s\n"
            (String.concat "," (Mosaic.Probe.pending (probed ())));
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
pending while queued: performs
settled while perform queued: false
|keys:[] ticks:0 size:- note:loaded
|
settled after perform ran: true|}]

let%expect_test "probe composes application-owned pending work" =
  let external_pending = ref true in
  let probe = ref None in
  let probed () = Option.get !probe in
  drive
    ~probe:(fun runtime_probe ->
      probe :=
        Some
          (Mosaic.Probe.with_pending ~name:"external"
             (fun () -> !external_pending)
             runtime_probe))
    (app ())
    [
      `Run
        (fun _ ->
          Printf.printf "pending: %s\n"
            (String.concat "," (Mosaic.Probe.pending (probed ())));
          Printf.printf "settled: %b\n" (Mosaic.Probe.is_settled (probed ())));
      `Run
        (fun _ ->
          external_pending := false;
          Printf.printf "settled after external completion: %b\n"
            (Mosaic.Probe.is_settled (probed ())));
    ];
  [%expect
    {|
    pending: external
    settled: false
    settled after external completion: true
    |}]

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

let%expect_test
    "startup focus emits blinking cursor style when it becomes visible" =
  let base = app ~init_cmd:(Mosaic.Cmd.focus "editor") () in
  let application =
    {
      base with
      Mosaic.view =
        (fun _ ->
          Mosaic.input ~id:"editor" ~cursor_style:`Block ~cursor_blinking:true
            ~cursor_color:Matrix.Ansi.Color.red ());
    }
  in
  drive application
    [
      `Run
        (fun t ->
          let output = Matrix_test.output t in
          Printf.printf "blinking style follows cursor visibility: %b\n"
            (is_before ~first:"\027[?25h" ~second:"\027[1 q" output));
    ];
  [%expect {|blinking style follows cursor visibility: true|}]

let%expect_test "an every-timer stepped exactly onto its deadline keeps firing"
    =
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

let%expect_test
    "a lone ESC resolves to Escape once the clock passes the timeout" =
  (* A bare ESC is ambiguous (Escape vs. the start of an Alt/CSI sequence), so
     the parser holds it on a ~50 ms deadline. With no further bytes it must
     still resolve to Escape once virtual time passes that deadline — the
     backend drains at the current instant on every read, not only when a chunk
     arrives. The key must not appear before the step, and must appear after. *)
  drive
    (app
       ~subs:(fun _ ->
         Mosaic.Sub.on_key (fun key -> Some (Pressed (key_name key))))
       ())
    [ `Feed "\027"; `Snap; `Advance 0.06; `Snap ];
  [%expect
    {||keys:[] ticks:0 size:- note:-
|
|keys:[<esc>] ticks:0 size:- note:-
||}]

let%expect_test "perform dispatches from concurrent threads are all delivered" =
  (* Cmd.perform runs its callback on a fresh thread (the default
     [process_perform]), and mosaic.mli promises dispatch may be called from
     any thread. Several threads hammer dispatch while the loop drains
     concurrently; systhread preemption lands inside queue operations, so a
     non-atomic pending queue loses (or resurrects) messages. The final tick
     count must be exact. *)
  (* Each burst must outlast the ~50 ms systhread tick so preemption lands
     mid-push with other threads still active — a shorter burst completes
     within one scheduler slice and never races. *)
  let thread_count = 3 in
  let per_thread = 2_000_000 in
  let hammer =
    Mosaic.Cmd.batch
      (List.init thread_count (fun _ ->
           Mosaic.Cmd.perform (fun dispatch ->
               for _ = 1 to per_thread do
                 dispatch Tick
               done)))
  in
  let probe = ref None in
  let on_idle t ~timeout:_ =
    match !probe with
    | Some p when Mosaic.Probe.is_settled p -> Matrix_test.stop t
    | _ -> Thread.yield ()
  in
  let t = Matrix_test.create ~on_idle ~width:48 ~height:2 () in
  Mosaic.run ~matrix:(Matrix_test.app t)
    ~probe:(fun p -> probe := Some p)
    (app ~init_cmd:hammer ());
  snap t;
  [%expect {||keys:[] ticks:6000000 size:- note:-
||}]

let%expect_test "a paste consumed by a focused editor skips on_paste" =
  (* Sub.on_paste must only see pastes the UI tree left unconsumed; a focused
     editor inserting the paste consumes it. Sub.on_paste_all sees both. *)
  let subs _ =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.on_paste (fun ev ->
            Some (Pressed ("p:" ^ Mosaic.Event.Paste.text ev)));
        Mosaic.Sub.on_paste_all (fun ev ->
            Some (Pressed ("P:" ^ Mosaic.Event.Paste.text ev)));
      ]
  in
  let base = app ~subs () in
  let with_editor =
    {
      base with
      Mosaic.view =
        (fun model ->
          Mosaic.box
            [
              Mosaic.textarea ~autofocus:true
                ~size:(Mosaic.size ~width:10 ~height:1)
                ();
              base.Mosaic.view model;
            ]);
    }
  in
  drive ~height:3 with_editor [ `Feed "\027[200~hi\027[201~"; `Snap ];
  drive base [ `Feed "\027[200~hi\027[201~"; `Snap ];
  [%expect
    {||          keys:[P:hi] ticks:0 size:- note:-
|
|
|keys:[P:hi,p:hi] ticks:0 size:- note:-
||}]

let%expect_test "a mouse press consumed by a widget skips on_mouse" =
  (* A widget handler that calls prevent_default consumes the event for
     Sub.on_mouse, while Sub.on_mouse_all still observes it. *)
  let log_downs tag ev =
    match Mosaic.Event.Mouse.kind ev with
    | Mosaic.Event.Mouse.Down _ -> Some (Pressed tag)
    | _ -> None
  in
  let subs _ =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.on_mouse (log_downs "m");
        Mosaic.Sub.on_mouse_all (log_downs "M");
      ]
  in
  let base = app ~subs () in
  let consume_down ev =
    (match Mosaic.Event.Mouse.kind ev with
    | Mosaic.Event.Mouse.Down _ -> Mosaic.Event.Mouse.prevent_default ev
    | _ -> ());
    None
  in
  let view consuming model =
    Mosaic.box
      ?on_mouse:(if consuming then Some consume_down else None)
      ~size:(Mosaic.size ~width:48 ~height:2)
      [ base.Mosaic.view model ]
  in
  let press = "\027[<0;41;2M\027[<0;41;2m" in
  drive { base with Mosaic.view = view true } [ `Feed press; `Snap ];
  drive { base with Mosaic.view = view false } [ `Feed press; `Snap ];
  [%expect
    {||keys:[M] ticks:0 size:- note:-
|
|keys:[M,m] ticks:0 size:- note:-
||}]

let%expect_test "Cmd.focus lands on a reused element whose id changed" =
  (* A fiber reused across renders must track its id attr: with the stale id,
     focus addressed at the new id never lands even though the element
     visibly renders it. The focused input's cursor becoming visible is the
     witness that focus landed. *)
  let base =
    app ~subs:(fun _ -> Mosaic.Sub.on_key (fun _ -> Some (Note "x"))) ()
  in
  let application =
    {
      base with
      Mosaic.update =
        (fun msg model ->
          match msg with
          | Note note ->
              ({ model with note }, Mosaic.Cmd.focus ("field-" ^ note))
          | msg -> update msg model);
      Mosaic.view =
        (fun model ->
          Mosaic.box
            [
              Mosaic.input ~id:("field-" ^ model.note)
                ~size:(Mosaic.size ~width:10 ~height:1)
                ();
              base.Mosaic.view model;
            ]);
    }
  in
  drive ~height:3 application
    [
      `Feed "x";
      `Run
        (fun t ->
          Printf.printf "cursor shown after focus: %b\n"
            (Option.is_some
               (substring_index "\027[?25h" (Matrix_test.output t))));
    ];
  [%expect {|cursor shown after focus: true|}]

let%expect_test "same-interval timers keep independent phases" =
  (* Two Sub.every timers with the same interval must not share deadline
     state: a timer added mid-flight (here at t=1.6 via a key) is due one
     interval later, at t=2.6 — not at the first timer's t=2.0 deadline.
     Elapsed-time bookkeeping matched previous timers by interval alone, so
     the newcomer inherited the first timer's phase and fired early. *)
  let subs model =
    Mosaic.Sub.batch
      (Mosaic.Sub.on_key (fun _ -> Some (Note "on"))
      :: Mosaic.Sub.every 1.0 (fun () -> Pressed "a")
      ::
      (if String.equal model.note "on" then
         [ Mosaic.Sub.every 1.0 (fun () -> Pressed "b") ]
       else []))
  in
  drive (app ~subs ())
    [
      `Advance 1.0 (* t=1.0: a fires *);
      `Advance 0.6 (* t=1.6 *);
      `Feed "x" (* adds the second timer mid-interval *);
      `Advance 0.4 (* t=2.0: a fires; b must NOT *);
      `Snap;
      `Advance 0.6 (* t=2.6: b fires *);
      `Snap;
    ];
  [%expect
    {||keys:[a,a] ticks:0 size:- note:on
|
|keys:[a,a,b] ticks:0 size:- note:on
||}]

let%expect_test "an unmatched Cmd.focus expires after one deferred attempt" =
  (* Cmd.focus retries once after the next completed render (covering an
     element the same update's view creates), then drops. An element whose id
     only appears two updates later must not be focused — and the runtime
     must not keep scanning the tree for the id forever. *)
  let base =
    app ~subs:(fun _ -> Mosaic.Sub.on_key (fun _ -> Some (Note "shown"))) ()
  in
  let application =
    {
      base with
      Mosaic.init =
        (fun () -> ({ initial with note = "hidden" }, Mosaic.Cmd.focus "late"));
      Mosaic.view =
        (fun model ->
          Mosaic.box
            [
              (if String.equal model.note "shown" then
                 Mosaic.input ~id:"late"
                   ~size:(Mosaic.size ~width:10 ~height:1)
                   ()
               else Mosaic.empty);
              base.Mosaic.view model;
            ]);
    }
  in
  drive ~height:3 application
    [
      `Feed "x" (* creates input#late, well after the focus request *);
      `Run
        (fun t ->
          Printf.printf "late element stole focus: %b\n"
            (Option.is_some
               (substring_index "\027[?25h" (Matrix_test.output t))));
    ];
  [%expect {|late element stole focus: false|}]

let%expect_test "static commit emits content taller than the viewport" =
  (* Cmd.static_commit renders offscreen at the content's natural height:
     every line of a static view taller than the terminal must reach the
     output stream, exercising the measure-then-grow pass. *)
  let lines = List.init 5 (fun i -> Printf.sprintf "static line %d" i) in
  let static_view = Mosaic.fragment (List.map Mosaic.text lines) in
  drive ~mode:`Primary ~width:40 ~height:3
    (app ~init_cmd:(Mosaic.Cmd.static_commit static_view) ())
    [
      `Run
        (fun t ->
          let out = Matrix_test.output t in
          List.iter
            (fun line ->
              Printf.printf "%s emitted: %b\n" line
                (Option.is_some (substring_index line out)))
            lines);
    ];
  [%expect
    {|
    static line 0 emitted: true
    static line 1 emitted: true
    static line 2 emitted: true
    static line 3 emitted: true
    static line 4 emitted: true
    |}]
