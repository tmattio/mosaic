(* Full-loop tests: a real [Mosaic.run] application driven headlessly through
   matrix.test — virtual clock, byte-level input, frame snapshots. The driver
   is a scripted [on_idle]: each time the loop goes idle, one step runs, so
   every step observes a fully rendered, quiescent frame. *)

open Windtrap

type model = {
  keys : string list; (* decoded key names, oldest first *)
  ticks : int;
  size : (int * int) option;
  note : string;
}

type msg =
  | Pressed of string
  | Tick
  | Resized of int * int
  | Note of string
  | Ignored

let initial = { keys = []; ticks = 0; size = None; note = "-" }

let key_name key =
  let data = Mosaic.Event.Key.data key in
  match data.Matrix.Input.Keystroke.key with
  | Char u when Uchar.is_char u -> String.make 1 (Uchar.to_char u)
  | Up -> "<up>"
  | Escape -> "<esc>"
  | _ -> "<other>"

let update msg model =
  match msg with
  | Pressed name ->
      ({ model with keys = model.keys @ [ name ] }, Mosaic.Cmd.none)
  | Tick -> ({ model with ticks = model.ticks + 1 }, Mosaic.Cmd.none)
  | Resized (w, h) -> ({ model with size = Some (w, h) }, Mosaic.Cmd.none)
  | Note note -> ({ model with note }, Mosaic.Cmd.none)
  | Ignored -> (model, Mosaic.Cmd.none)

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

(* The round-trip oracle: whatever bytes the runtime emitted must, when
   replayed through matrix's own VTE, reconstruct the grid the backend
   believes it painted. It is a strong invariant and a free one — every
   frame this file snapshots is already both an ANSI stream and a grid — and
   it catches the class of bug a golden cannot: a frame that looks right in
   the backend's own accounting but whose escape sequences do not put it on
   a real terminal. *)
(* The two sides represent an all-blank tail differently — Matrix_test.screen
   right-trims each row, Vte.to_string pads to the full width — so rows are
   compared without their trailing blanks. Nothing about cell content is
   relaxed: a space the grid actually holds inside a row still counts. *)
let rstrip_rows s =
  String.split_on_char '\n' s
  |> List.map (fun row ->
      let n = ref (String.length row) in
      while !n > 0 && row.[!n - 1] = ' ' do
        decr n
      done;
      String.sub row 0 !n)
  |> String.concat "\n" |> String.trim

let replay_matches t ~width ~height =
  let vte = Vte.create ~rows:height ~cols:width () in
  ignore (Vte.feed_string vte (Matrix_test.output t) : string);
  equal ~msg:"replaying the emitted ANSI reconstructs the grid" text
    (rstrip_rows (Matrix_test.screen t))
    (rstrip_rows (Vte.to_string vte))

let drive ?mode ?(width = 48) ?(height = 2) ?probe ?process_perform application
    steps =
  let steps = ref steps in
  (* Tracked so the replay VTE is always the size the backend currently is. *)
  let dims = ref (width, height) in
  let on_idle t ~timeout =
    match !steps with
    | [] -> Matrix_test.stop t
    | `Step_to target :: rest ->
        (* A driver that honours the loop's requested wait: the clock moves to
           the armed deadline, or to [target] when that comes first, one idle
           generation at a time. Every timer deadline before [target] is
           observed as its own quiescent frame, and the step ends with the
           clock at exactly [target]. *)
        let now = Matrix_test.now t in
        if now >= target then steps := rest
        else
          let step =
            match timeout with
            | Some wait when wait > 0. -> Float.min wait (target -. now)
            | Some _ | None -> target -. now
          in
          Matrix_test.set_now t (now +. step)
    | ((`Feed _ | `Resize _ | `Advance _ | `Snap | `Run _) as step) :: rest -> (
        steps := rest;
        match step with
        | `Feed bytes -> Matrix_test.feed t bytes
        | `Resize (width, height) ->
            dims := (width, height);
            Matrix_test.resize t ~width ~height
        | `Advance dt -> Matrix_test.set_now t (Matrix_test.now t +. dt)
        | `Snap ->
            let width, height = !dims in
            replay_matches t ~width ~height;
            snap t
        | `Run f -> f t)
  in
  let t = Matrix_test.create ?mode ~on_idle ~width ~height () in
  Mosaic.run ~matrix:(Matrix_test.app t) ?process_perform ?probe application

let on_keys _ = Mosaic.Sub.on_key (fun key -> Some (Pressed (key_name key)))

let%expect_test "boots, renders, and reacts to byte-decoded input" =
  drive (app ~subs:on_keys ()) [ `Snap; `Feed "a"; `Snap; `Feed "b"; `Snap ];
  [%expect
    {|
    |keys:[] ticks:0 size:- note:-
    |
    |keys:[a] ticks:0 size:- note:-
    |
    |keys:[a,b] ticks:0 size:- note:-
    |
    |}]

let%expect_test "in-frame geometry changes settle without input" =
  let base = app () in
  let application =
    {
      base with
      Mosaic.view =
        (fun _model ->
          Mosaic.scroll_box ~scroll:(Mosaic.Scroll.at 0)
            ~on_scroll:(fun _ -> Ignored)
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
  [%expect {|
    |keys:[<up>] ticks:0 size:- note:-
    |
    |}]

let%expect_test "virtual time drives every-subscriptions deterministically" =
  drive
    (app ~subs:(fun _ -> Mosaic.Sub.every ~id:"timer" 1.0 (fun () -> Tick)) ())
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
    {|
    |keys:[] ticks:0 size:- note:-
    |
    |keys:[] ticks:1 size:- note:-
    |
    |keys:[] ticks:2 size:- note:-
    |
    |keys:[] ticks:2 size:- note:-
    |
    |keys:[] ticks:3 size:- note:-
    |
    |}]

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
      (app
         ~subs:(fun _ -> Mosaic.Sub.every ~id:"timer" 0.2 (fun () -> Tick))
         ())
      with
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
  [%expect {| live frame delta: 16.7 ms |}]

let%expect_test "a pending every-timer renders only when it fires" =
  (* A timer is not an animation: while it is merely pending, the loop stays
     idle — no view pass, no reconcile, no paint. The view-call count is the
     witness: it must not move as virtual time approaches the deadline, and
     must move exactly once when the timer fires. *)
  let views = ref 0 in
  let base =
    app ~subs:(fun _ -> Mosaic.Sub.every ~id:"timer" 1.0 (fun () -> Tick)) ()
  in
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
    {|
    |keys:[] ticks:0 size:48x2 note:-
    |
    |keys:[] ticks:0 size:60x3 note:-
    |
    |
    |}]

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
      Mosaic.Table.row "first"
        [|
          Mosaic.Table.cell "❯";
          Mosaic.Table.cell "Retained parser investigation";
          Mosaic.Table.cell "active";
          Mosaic.Table.cell "idle";
          Mosaic.Table.cell "1 turn";
          Mosaic.Table.cell "just now";
          Mosaic.Table.cell "today";
        |];
      Mosaic.Table.row "second"
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
          Mosaic.table
            ~size:(Mosaic.size ~width ~height:3)
            ~columns ~rows ~border:false ~equal:String.equal
            ~selected:(Some "first")
            ~on_change:(fun _ -> Ignored)
            ~show_header:true ~cell_padding:0
            ~selected_background:(Matrix.Ansi.Color.grayscale ~level:3)
            ());
    }
  in
  drive ~width:80 ~height:3 application [ `Snap; `Feed "w"; `Snap ];
  [%expect
    {|
    |   session                           lifecycle phase  turns updated  recency
    |❯  Retained parser investigation     active    idle  1 turn just now today
    |   session-visual-2                  active    idle  1 turn just now today
    |   session                               lifecycle phase  turns updated  recency
    |❯  Retained parser investigation         active    idle  1 turn just now today
    |   session-visual-2                      active    idle  1 turn just now today
    |}]

let%expect_test "probe reports perform and message quiescence" =
  let pending : (unit -> unit) Queue.t = Queue.create () in
  let probe = ref None in
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
          equal ~msg:"pending while a perform is queued" (list string)
            [ "performs" ]
            (Mosaic.Probe.pending (probed ()));
          is_false ~msg:"not settled while a perform is queued"
            (Mosaic.Probe.is_settled (probed ())));
      `Run (fun _ -> Queue.pop pending ());
      `Snap;
      `Run
        (fun _ ->
          is_true ~msg:"settled after the perform ran"
            (Mosaic.Probe.is_settled (probed ())));
    ];
  [%expect
    {|
    |keys:[] ticks:0 size:- note:-
    |
    |keys:[] ticks:0 size:- note:loaded
    |
    |}]

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
          equal ~msg:"application-owned work shows in pending" (list string)
            [ "external" ]
            (Mosaic.Probe.pending (probed ()));
          is_false ~msg:"not settled while external work is pending"
            (Mosaic.Probe.is_settled (probed ())));
      `Run
        (fun _ ->
          external_pending := false;
          is_true ~msg:"settled after external completion"
            (Mosaic.Probe.is_settled (probed ())));
    ];
  [%expect {| |}]

let%expect_test "replaying the output stream through a VTE matches the grid" =
  let result = ref None in
  drive (app ~subs:on_keys ())
    [
      `Feed "a";
      `Feed "b";
      `Run
        (fun t ->
          let vte = Vte.create ~rows:2 ~cols:48 () in
          ignore (Vte.feed_string vte (Matrix_test.output t) : string);
          result := Some (Vte.to_string vte, Matrix_test.screen t));
    ];
  let replayed, screen = Option.get !result in
  equal ~msg:"replaying the output stream reconstructs the grid" text
    (String.trim screen) (String.trim replayed);
  print_endline "replayed:";
  String.split_on_char '\n' replayed
  |> List.iter (fun row -> print_endline ("|" ^ row));
  [%expect {|
    replayed:
    |keys:[a,b] ticks:0 size:- note:-
    |
    |}]

let%expect_test
    "startup focus emits blinking cursor style when it becomes visible" =
  let base = app () in
  let application =
    {
      base with
      Mosaic.view =
        (fun _ ->
          Mosaic.input ~state:Mosaic.Editor.empty
            ~on_change:(fun _ -> Ignored)
            ~focused:true ~cursor_style:`Block ~cursor_blinking:true
            ~cursor_color:Matrix.Ansi.Color.red ());
    }
  in
  drive application
    [
      `Run
        (fun t ->
          let output = Matrix_test.output t in
          in_order ~msg:"blinking style follows cursor visibility"
            ~subs:[ "\027[?25h"; "\027[1 q" ]
            output);
    ];
  [%expect {| |}]

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
    (app ~subs:(fun _ -> Mosaic.Sub.every ~id:"timer" 0.1 (fun () -> Tick)) ())
    (List.rev !steps);
  [%expect
    {|
    |keys:[] ticks:1 size:- note:-
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
    |
    |}]

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
    {|
    |keys:[] ticks:0 size:- note:-
    |
    |keys:[<esc>] ticks:0 size:- note:-
    |
    |}]

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
  [%expect {|
    |keys:[] ticks:6000000 size:- note:-
    |
    |}]

let%expect_test "paste capture runs before the focused editor" =
  (* Capture listeners run in declaration order before the insertion. A later
     editor proposal cannot retroactively consume their delivery. *)
  let order = ref [] in
  let record step = order := step :: !order in
  let subs _ =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.on_paste ~phase:Capture (fun ev ->
            record "capture";
            Some (Pressed ("p:" ^ Mosaic.Event.Paste.text ev)));
        Mosaic.Sub.on_paste ~phase:Capture (fun ev ->
            record "all";
            Some (Pressed ("P:" ^ Mosaic.Event.Paste.text ev)));
      ]
  in
  let base = app ~subs () in
  let with_editor =
    {
      base with
      Mosaic.view =
        (fun model ->
          Mosaic.box ~flex_direction:Row
            [
              Mosaic.textarea ~state:Mosaic.Editor.empty
                ~on_change:(fun editor ->
                  record "editor";
                  equal ~msg:"paste reaches the editor proposal" string "hi"
                    (Mosaic.Editor.text editor);
                  Ignored)
                ~size:(Mosaic.size ~width:10 ~height:1)
                ~focused:true ();
              base.Mosaic.view model;
            ]);
    }
  in
  drive ~height:3 with_editor [ `Feed "\027[200~hi\027[201~"; `Snap ];
  equal ~msg:"capture precedes the editor default action" (list string)
    [ "capture"; "all"; "editor" ]
    (List.rev !order);
  order := [];
  drive base [ `Feed "\027[200~hi\027[201~"; `Snap ];
  equal ~msg:"unfocused delivery keeps declaration order" (list string)
    [ "capture"; "all" ] (List.rev !order);
  [%expect
    {|
    |          keys:[p:hi,P:hi] ticks:0 size:- note:-
    |
    |
    |keys:[p:hi,P:hi] ticks:0 size:- note:-
    |
    |}]

let%expect_test "a mouse press consumed by a widget skips on_mouse" =
  (* A capture listener sees the press before the widget; a bubble listener
     after it, and only if the widget did not use it. *)
  let log_downs tag ev =
    match Mosaic.Event.Mouse.kind ev with
    | Mosaic.Event.Mouse.Down _ -> Some (Pressed tag)
    | _ -> None
  in
  let subs _ =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.on_mouse (log_downs "m");
        Mosaic.Sub.on_mouse ~phase:Capture (log_downs "M");
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
      ~size:(Mosaic.size ~width:48 ~height:2)
      ?on_mouse:(if consuming then Some consume_down else None)
      [ base.Mosaic.view model ]
  in
  let press = "\027[<0;41;2M\027[<0;41;2m" in
  drive { base with Mosaic.view = view true } [ `Feed press; `Snap ];
  drive { base with Mosaic.view = view false } [ `Feed press; `Snap ];
  [%expect
    {|
    |keys:[M] ticks:0 size:- note:-
    |
    |keys:[M,m] ticks:0 size:- note:-
    |
    |}]

let%expect_test "model focus follows a remounted editor" =
  let observed = ref Mosaic.Editor.empty in
  let application =
    {
      Mosaic.init = (fun () -> ((0, Mosaic.Editor.empty), Mosaic.Cmd.none));
      update =
        (fun msg (generation, editor) ->
          match msg with
          | `Remount -> ((generation + 1, editor), Mosaic.Cmd.none)
          | `Edited editor ->
              observed := editor;
              ((generation, editor), Mosaic.Cmd.none));
      view =
        (fun (generation, editor) ->
          Mosaic.input ~key:(string_of_int generation) ~focused:true
            ~state:editor
            ~on_change:(fun editor -> `Edited editor)
            ());
      subscriptions =
        (fun _ ->
          Mosaic.Sub.on_key (fun ev ->
              if Mosaic.Shortcut.matches (Mosaic.Shortcut.f 2) ev then (
                Mosaic.Event.Key.prevent_default ev;
                Some `Remount)
              else None));
    }
  in
  drive ~width:20 ~height:1 application
    [
      `Feed "a";
      `Feed "\027OQ";
      `Feed "b";
      `Run
        (fun t ->
          equal ~msg:"accepted editor survives remount" string "ab"
            (Mosaic.Editor.text !observed);
          equal ~msg:"remounted focused editor paints accepted state" string
            "ab"
            (String.trim (Matrix_test.screen t)));
    ];
  [%expect {| |}]

let%expect_test "same-interval timers keep independent phases" =
  (* Two Sub.every timers with the same interval must not share deadline
     state: a timer added mid-flight (here at t=1.6 via a key) is due one
     interval later, at t=2.6 — not at the first timer's t=2.0 deadline.
     Elapsed-time bookkeeping matched previous timers by interval alone, so
     the newcomer inherited the first timer's phase and fired early. *)
  let subs model =
    Mosaic.Sub.batch
      (Mosaic.Sub.on_key (fun _ -> Some (Note "on"))
      :: Mosaic.Sub.every ~id:"first" 1.0 (fun () -> Pressed "a")
      ::
      (if String.equal model.note "on" then
         [ Mosaic.Sub.every ~id:"second" 1.0 (fun () -> Pressed "b") ]
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
    {|
    |keys:[a,a] ticks:0 size:- note:on
    |
    |keys:[a,a,b] ticks:0 size:- note:on
    |
    |}]

let%expect_test "a timer stepped deadline by deadline keeps the nominal grid" =
  (* A driver that moves the clock to each armed wakeup and stops exactly at
     t=2.0 must see a 0.1 s timer fire twenty times: the twentieth deadline is
     [20 * 0.1], the nominal instant. Carrying the deadline forward by
     repeated addition drifted it past 2.0 by a few ULPs after twenty steps,
     and an exact comparison then lost the final fire beyond the stop (19,
     then 29). The accumulating driver above is the mirror image: there the
     clock is the side that drifts. *)
  let subs _ = Mosaic.Sub.every ~id:"timer" 0.1 (fun () -> Tick) in
  drive (app ~subs ()) [ `Step_to 2.0; `Snap; `Step_to 3.0; `Snap ];
  [%expect
    {||keys:[] ticks:20 size:- note:-
|
|keys:[] ticks:30 size:- note:-
||}]

let%expect_test "a late unfocused editor never steals focus" =
  let base =
    app ~subs:(fun _ -> Mosaic.Sub.on_key (fun _ -> Some (Note "shown"))) ()
  in
  let application =
    {
      base with
      Mosaic.init =
        (fun () -> ({ initial with note = "hidden" }, Mosaic.Cmd.none));
      view =
        (fun model ->
          Mosaic.box
            [
              (if model.note = "shown" then
                 Mosaic.input ~state:Mosaic.Editor.empty ~focused:false
                   ~on_change:(fun _ -> Ignored)
                   ()
               else Mosaic.empty);
              base.Mosaic.view model;
            ]);
    }
  in
  drive ~height:3 application
    [
      `Feed "x";
      `Run
        (fun t ->
          not_contains ~msg:"late element must not steal focus" ~sub:"\027[?25h"
            (Matrix_test.output t));
    ];
  [%expect {| |}]

let%expect_test "a late width method re-measures existing text" =
  (* The terminal reports wcwidth (DECRPM 2027) after the first frame. A
     joined emoji is one 2-cell cluster under Unicode and 4 cells under
     wcwidth, so every widget that measured it must measure it again: the
     bars after Text, Code and an input move, and wrapped text takes more
     rows. *)
  let emoji = "\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbb" in
  let row child =
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row
      [ child; Mosaic.text "|" ]
  in
  let base = app () in
  let application =
    {
      base with
      Mosaic.view =
        (fun _ ->
          Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column
            [
              row (Mosaic.text (emoji ^ "t"));
              row (Mosaic.code (emoji ^ "c"));
              row
                (Mosaic.input ~flex_shrink:0.
                   ~state:(Mosaic.Editor.of_string (emoji ^ "e"))
                   ~on_change:(fun _ -> Ignored)
                   ());
              row
                (Mosaic.box
                   ~size:(Mosaic.size_wh (Mosaic.px 4) Mosaic.auto)
                   [ Mosaic.text ~wrap:`Char (emoji ^ emoji) ]);
              Mosaic.text "end";
            ]);
    }
  in
  (* Each cell a grapheme starts, and [-] for the cells it spans after its
     first. *)
  let cells t =
    let grid = Matrix.current_grid (Matrix_test.app t) in
    let width = Matrix.Grid.width grid in
    for y = 0 to Matrix.Grid.height grid - 1 do
      let row = Buffer.create width in
      for x = 0 to width - 1 do
        let i = (y * width) + x in
        Buffer.add_string row
          (if Matrix.Grid.is_continuation grid i then "-"
           else
             match Matrix.Grid.get_text grid i with "" -> " " | text -> text)
      done;
      print_endline ("|" ^ String.trim (Buffer.contents row))
    done
  in
  drive ~width:12 ~height:7 application
    [ `Run cells; `Feed "\027[?2027;2$y"; `Run cells ];
  [%expect
    {|
    |👩‍💻-t|
    |👩‍💻-c|
    |👩‍💻-e |
    |👩‍💻-👩‍💻-|
    |end
    |
    |
    |👩‍-💻-t|
    |👩‍-💻-c|
    |👩‍-💻-e |
    |👩‍-💻-|
    |👩‍-💻-
    |end
    |
    |}]

let%expect_test "static output follows the terminal's width method" =
  (* Once the terminal reports wcwidth, a static commit lays its text out by
     it too: two joined emoji take 8 cells, which wrap in 6 columns. *)
  let emoji = "\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbb" in
  let base = app ~subs:on_keys () in
  let application =
    {
      base with
      Mosaic.update =
        (fun msg model ->
          match msg with
          | Pressed "s" ->
              ( model,
                Mosaic.Cmd.static_commit
                  (Mosaic.text ~wrap:`Char (emoji ^ emoji)) )
          | _ -> (model, Mosaic.Cmd.none));
      view = (fun _ -> Mosaic.text "live");
    }
  in
  drive ~mode:`Primary ~width:6 ~height:4 application
    [
      `Feed "\027[?2027;2$y";
      `Feed "s";
      `Run
        (fun t ->
          let vte = Vte.create ~rows:4 ~cols:6 () in
          ignore (Vte.feed_string vte (Matrix_test.output t) : string);
          print_string (rstrip_rows (Vte.to_string vte)));
    ];
  [%expect {|
    👩‍💻
    👩‍💻
    live
    |}]

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
            (fun line -> contains ~msg:(line ^ " emitted") ~sub:line out)
            lines);
    ];
  [%expect {| |}]

let join_worker worker =
  let joined = Atomic.make false in
  ignore
    (Thread.create
       (fun () ->
         Thread.join worker;
         Atomic.set joined true)
       ()
      : Thread.t);
  let tries = ref 0 in
  while (not (Atomic.get joined)) && !tries < 5000 do
    incr tries;
    Thread.delay 0.001
  done;
  is_true ~msg:"worker exits within five seconds" (Atomic.get joined)

let%expect_test "threaded highlighting wakes an idle application" =
  let released = Atomic.make false in
  let worker = Atomic.make None in
  let highlighter =
    Mosaic.Code.Highlighter.threaded (fun ~language:_ ~content ->
        Atomic.set worker (Some (Thread.self ()));
        while not (Atomic.get released) do
          Thread.delay 0.001
        done;
        let meta =
          { Mosaic.Syntax_highlight.default_meta with conceal = Some "done" }
        in
        Mosaic.Syntax_highlight.make ~content
          [
            Mosaic.Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:3
              ~scope:"keyword" ();
          ])
  in
  let syntax = Mosaic.Code.with_highlighter ~language:"example" highlighter in
  let base = app () in
  let application =
    { base with Mosaic.view = (fun _ -> Mosaic.code ~syntax "let x") }
  in
  let phase = ref 0 in
  let on_idle t ~timeout:_ =
    if !phase = 0 then begin
      phase := 1;
      equal ~msg:"plain initial frame" string "let x"
        (String.trim (Matrix_test.screen t));
      Atomic.set released true;
      let tries = ref 0 in
      while Atomic.get worker = None && !tries < 5000 do
        incr tries;
        Thread.delay 0.001
      done;
      let thread =
        match Atomic.get worker with
        | Some thread -> thread
        | None -> fail "worker did not start"
      in
      join_worker thread;
      is_true ~msg:"completion requests a host frame"
        (Matrix.redraw_requested (Matrix_test.app t))
    end
    else begin
      equal ~msg:"highlight applied without user input" string "done x"
        (String.trim (Matrix_test.screen t));
      Matrix_test.stop t
    end
  in
  let t = Matrix_test.create ~on_idle ~width:20 ~height:1 () in
  Fun.protect
    ~finally:(fun () -> Atomic.set released true)
    (fun () -> Mosaic.run ~matrix:(Matrix_test.app t) application);
  [%expect {| |}]

let%expect_test "application exit cancels queued highlighting" =
  List.iter
    (fun exceptional ->
      let released = Atomic.make false in
      let worker = Atomic.make None in
      let calls = Atomic.make [] in
      let highlighter =
        Mosaic.Code.Highlighter.threaded (fun ~language:_ ~content ->
            Atomic.set worker (Some (Thread.self ()));
            Atomic.set calls (content :: Atomic.get calls);
            while not (Atomic.get released) do
              Thread.delay 0.001
            done;
            Mosaic.Syntax_highlight.make ~content [])
      in
      let syntax =
        Mosaic.Code.with_highlighter ~language:"example" highlighter
      in
      let base = app () in
      let application =
        {
          base with
          Mosaic.view =
            (fun _ ->
              Mosaic.box
                [ Mosaic.code ~syntax "first"; Mosaic.code ~syntax "queued" ]);
        }
      in
      let on_idle t ~timeout:_ =
        let tries = ref 0 in
        while Atomic.get worker = None && !tries < 5000 do
          incr tries;
          Thread.delay 0.001
        done;
        is_true ~msg:"first worker started" (Option.is_some (Atomic.get worker));
        if exceptional then failwith "application failed"
        else Matrix_test.stop t
      in
      let t = Matrix_test.create ~on_idle ~width:20 ~height:2 () in
      Fun.protect
        ~finally:(fun () -> Atomic.set released true)
        (fun () ->
          if exceptional then
            raises_match ~msg:"application failure preserved"
              (function
                | Failure message -> message = "application failed" | _ -> false)
              (fun () -> Mosaic.run ~matrix:(Matrix_test.app t) application)
          else Mosaic.run ~matrix:(Matrix_test.app t) application);
      join_worker (Option.get (Atomic.get worker));
      equal ~msg:"closed application leaves no queued work" (list string)
        [ "first" ] (Atomic.get calls))
    [ false; true ];
  [%expect {| |}]

let%expect_test
    "rejected edits do not become the base of the next key in a burst" =
  let proposals = ref [] in
  let accepted = ref Mosaic.Editor.empty in
  let application =
    {
      Mosaic.init = (fun () -> (Mosaic.Editor.empty, Mosaic.Cmd.none));
      update =
        (fun proposal editor ->
          let text = Mosaic.Editor.text proposal in
          proposals := text :: !proposals;
          let editor = if text = "a" then editor else proposal in
          accepted := editor;
          (editor, Mosaic.Cmd.none));
      view =
        (fun editor ->
          Mosaic.input ~state:editor ~focused:true
            ~on_change:(fun state -> state)
            ());
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  drive ~width:20 ~height:1 application
    [
      `Feed "ab";
      `Run
        (fun t ->
          equal ~msg:"second proposal starts from the accepted empty state"
            (list string) [ "a"; "b" ] (List.rev !proposals);
          equal ~msg:"model accepts only second key" string "b"
            (Mosaic.Editor.text !accepted);
          equal ~msg:"screen agrees with model" string "b"
            (String.trim (Matrix_test.screen t)));
    ];
  [%expect {| |}]

let%expect_test "normalization is authoritative before the next key in a burst"
    =
  let proposals = ref [] in
  let accepted = ref Mosaic.Editor.empty in
  let application =
    {
      Mosaic.init = (fun () -> (Mosaic.Editor.empty, Mosaic.Cmd.none));
      update =
        (fun proposal _ ->
          let text = Mosaic.Editor.text proposal in
          proposals := text :: !proposals;
          let editor = Mosaic.Editor.of_string (String.uppercase_ascii text) in
          accepted := editor;
          (editor, Mosaic.Cmd.none));
      view =
        (fun editor ->
          Mosaic.input ~state:editor ~focused:true
            ~on_change:(fun state -> state)
            ());
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  drive ~width:20 ~height:1 application
    [
      `Feed "ab";
      `Run
        (fun t ->
          equal ~msg:"second proposal starts from normalized text" (list string)
            [ "a"; "Ab" ] (List.rev !proposals);
          equal ~msg:"model retains normalization" string "AB"
            (Mosaic.Editor.text !accepted);
          equal ~msg:"screen agrees with normalized model" string "AB"
            (String.trim (Matrix_test.screen t)));
    ];
  [%expect {| |}]

let%expect_test "global capture updates focus and editor before default editing"
    =
  let accepted = ref (Mosaic.Editor.empty, Mosaic.Editor.empty) in
  let application =
    {
      Mosaic.init =
        (fun () ->
          ( (true, Mosaic.Editor.of_string "L", Mosaic.Editor.empty),
            Mosaic.Cmd.none ));
      update =
        (fun msg (left_focused, left, right) ->
          let model =
            match msg with
            | `Transfer -> (false, left, Mosaic.Editor.of_string "R")
            | `Left editor -> (left_focused, editor, right)
            | `Right editor -> (left_focused, left, editor)
          in
          let _, left, right = model in
          accepted := (left, right);
          (model, Mosaic.Cmd.none));
      view =
        (fun (left_focused, left, right) ->
          Mosaic.box ~flex_direction:Column
            [
              Mosaic.input ~state:left ~focused:left_focused
                ~on_change:(fun state -> `Left state)
                ();
              Mosaic.input ~state:right ~focused:(not left_focused)
                ~on_change:(fun state -> `Right state)
                ();
            ]);
      subscriptions =
        (fun _ ->
          Mosaic.Sub.on_key ~phase:Capture (fun ev ->
              if Mosaic.Shortcut.matches (Mosaic.Shortcut.char 'x') ev then
                Some `Transfer
              else None));
    }
  in
  drive ~width:20 ~height:2 application
    [
      `Feed "x";
      `Run
        (fun t ->
          let left, right = !accepted in
          equal ~msg:"previously focused editor is untouched" string "L"
            (Mosaic.Editor.text left);
          equal ~msg:"default action edits newly accepted target" string "Rx"
            (Mosaic.Editor.text right);
          equal ~msg:"both accepted editors are painted" string "L\nRx"
            (String.trim (Matrix_test.screen t)));
    ];
  [%expect {| |}]

let%expect_test "non-key subscriptions accumulate in declaration order" =
  let observed = ref initial in
  let base =
    app
      ~subs:(fun _ ->
        Mosaic.Sub.batch
          [
            Mosaic.Sub.on_resize (fun ~width:_ ~height:_ -> Pressed "r1");
            Mosaic.Sub.on_resize (fun ~width:_ ~height:_ -> Pressed "r2");
            Mosaic.Sub.on_focus (Pressed "f1");
            Mosaic.Sub.on_focus (Pressed "f2");
            Mosaic.Sub.on_blur (Pressed "b1");
            Mosaic.Sub.on_blur (Pressed "b2");
            Mosaic.Sub.on_color_scheme (fun _ -> Some (Pressed "c1"));
            Mosaic.Sub.on_color_scheme (fun _ -> Some (Pressed "c2"));
          ])
      ()
  in
  let application =
    {
      base with
      Mosaic.update =
        (fun msg model ->
          let model, cmd = update msg model in
          observed := model;
          (model, cmd));
    }
  in
  drive ~width:70 ~height:1 application
    [
      `Run
        (fun _ ->
          equal ~msg:"startup geometry reaches both resize subscribers"
            (list string) [ "r1"; "r2" ] !observed.keys);
      `Resize (71, 1);
      `Run (fun t -> Matrix_test.feed_event t Matrix.Focus_out);
      `Run (fun t -> Matrix_test.feed_event t Matrix.Focus_in);
      `Run (fun t -> Matrix_test.feed_event t (Matrix.Color_scheme `Dark));
      `Run
        (fun t ->
          equal ~msg:"every subscriber sees its event in source order"
            (list string)
            [ "r1"; "r2"; "r1"; "r2"; "b1"; "b2"; "f1"; "f2"; "c1"; "c2" ]
            !observed.keys;
          contains ~msg:"screen reflects all delivered messages"
            ~sub:"keys:[r1,r2,r1,r2,b1,b2,f1,f2,c1,c2]" (Matrix_test.screen t));
    ];
  [%expect {| |}]

let%expect_test
    "reordering timer IDs preserves independent equal-interval phases" =
  let observed = ref initial in
  let subs model =
    let a = Mosaic.Sub.every ~id:"a" 1.0 (fun () -> Pressed "a") in
    let b = Mosaic.Sub.every ~id:"b" 1.0 (fun () -> Pressed "b") in
    let timers =
      match model.note with "-" -> [ a ] | "added" -> [ a; b ] | _ -> [ b; a ]
    in
    Mosaic.Sub.batch
      (Mosaic.Sub.on_key (fun _ ->
           Some (Note (if model.note = "-" then "added" else "reordered")))
      :: timers)
  in
  let base = app ~subs () in
  let application =
    {
      base with
      Mosaic.update =
        (fun msg model ->
          let model, cmd = update msg model in
          observed := model;
          (model, cmd));
    }
  in
  drive ~width:70 ~height:1 application
    [
      `Step_to 1.0;
      `Step_to 1.4;
      `Feed "+";
      `Step_to 1.6;
      `Feed "+";
      `Step_to 2.0;
      `Run
        (fun _ ->
          equal ~msg:"reorder preserves first deadline" (list string)
            [ "a"; "a" ] !observed.keys);
      `Step_to 2.4;
      `Run
        (fun t ->
          equal ~msg:"newer timer keeps its later phase" (list string)
            [ "a"; "a"; "b" ] !observed.keys;
          contains ~msg:"screen records timer order" ~sub:"keys:[a,a,b]"
            (Matrix_test.screen t));
    ];
  [%expect {| |}]

let%expect_test
    "pointer focus requests precede caret defaults and keyboard input" =
  let observed =
    ref (true, Mosaic.Editor.of_string "L", Mosaic.Editor.of_string "old")
  in
  let events = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> (!observed, Mosaic.Cmd.none));
      update =
        (fun msg (left_focused, left, right) ->
          let model =
            match msg with
            | `Focus ->
                events := "focus" :: !events;
                (false, left, Mosaic.Editor.of_string "abcd")
            | `Left next -> (left_focused, next, right)
            | `Right next ->
                events := "edit" :: !events;
                (left_focused, left, next)
          in
          observed := model;
          (model, Mosaic.Cmd.none));
      view =
        (fun (left_focused, left, right) ->
          Mosaic.box ~flex_direction:Column
            [
              Mosaic.input
                ~size:(Mosaic.size ~width:20 ~height:1)
                ~state:left ~focused:left_focused
                ~on_change:(fun next -> `Left next)
                ();
              Mosaic.input
                ~size:(Mosaic.size ~width:20 ~height:1)
                ~state:right ~focused:(not left_focused)
                ~on_focus_request:`Focus
                ~on_change:(fun next -> `Right next)
                ();
            ]);
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  drive ~width:20 ~height:2 application
    [
      `Feed "\027[<0;2;2M\027[<0;2;2mX";
      `Run
        (fun _ ->
          let left_focused, left, right = !observed in
          is_false left_focused;
          equal string "L" (Mosaic.Editor.text left);
          equal ~msg:"caret uses the source accepted during focus" string
            "aXbcd" (Mosaic.Editor.text right);
          equal int 2 (Mosaic.Editor.cursor right);
          equal ~msg:"focus accepted before any editor proposal" string "focus"
            (List.hd (List.rev !events)));
    ];
  [%expect {| |}]

let%expect_test "rejected or unmounted focus targets cannot edit or steal focus"
    =
  List.iter
    (fun unmount ->
      let right = Mosaic.Editor.of_string "right" in
      let observed = ref (true, Mosaic.Editor.of_string "L", right) in
      let requested = ref 0 and edited_right = ref 0 in
      let clicks = ref 0 in
      let application =
        {
          Mosaic.init = (fun () -> (!observed, Mosaic.Cmd.none));
          update =
            (fun msg (visible, left, right) ->
              let model =
                match msg with
                | `Focus ->
                    incr requested;
                    (not unmount, left, right)
                | `Left next -> (visible, next, right)
                | `Right next ->
                    incr edited_right;
                    (visible, left, next)
              in
              observed := model;
              (model, Mosaic.Cmd.none));
          view =
            (fun (visible, left, right) ->
              Mosaic.box ~flex_direction:Column
                [
                  Mosaic.input
                    ~size:(Mosaic.size ~width:20 ~height:1)
                    ~state:left ~focused:true
                    ~on_change:(fun next -> `Left next)
                    ();
                  (if visible then
                     Mosaic.input
                       ~size:(Mosaic.size ~width:20 ~height:1)
                       ~state:right ~focused:false ~on_focus_request:`Focus
                       ~on_click:(fun _ ->
                         incr clicks;
                         None)
                       ~on_change:(fun next -> `Right next)
                       ()
                   else Mosaic.empty);
                ]);
          subscriptions = (fun _ -> Mosaic.Sub.none);
        }
      in
      drive ~width:20 ~height:2 application
        [
          `Feed "\027[<0;2;2M\027[<0;2;2mX";
          `Run
            (fun _ ->
              let _, left, current_right = !observed in
              equal int 1 !requested;
              equal ~msg:"no stale default editor callback" int 0 !edited_right;
              equal ~msg:"rejected press does not click on release" int 0
                !clicks;
              equal string "LX" (Mosaic.Editor.text left);
              is_true ~msg:"unaccepted target remains unchanged"
                (current_right == right));
        ])
    [ false; true ];
  [%expect {| |}]

let%expect_test "memo and message mapping defer focus requests until input" =
  let mapped = ref 0 and delivered = ref 0 in
  let map_message message =
    incr mapped;
    message
  in
  let application =
    {
      Mosaic.init = (fun () -> ((), Mosaic.Cmd.none));
      update =
        (fun message () ->
          (match message with
          | `Focus -> incr delivered
          | `Edited -> fail "unaccepted edit");
          ((), Mosaic.Cmd.none));
      view =
        (fun () ->
          Mosaic.memo ~key:() (fun () ->
              Mosaic.input
                ~size:(Mosaic.size ~width:20 ~height:1)
                ~state:Mosaic.Editor.empty ~focused:false
                ~on_focus_request:`Focus
                ~on_change:(fun _ -> `Edited)
                ())
          |> Mosaic.map map_message);
      subscriptions = (fun () -> Mosaic.Sub.none);
    }
  in
  drive ~width:20 ~height:1 application
    [
      `Run
        (fun _ ->
          equal int 0 !mapped;
          equal int 0 !delivered);
      `Feed "\027[<0;1;1M\027[<0;1;1m";
      `Run
        (fun _ ->
          equal int 1 !mapped;
          equal int 1 !delivered);
    ];
  [%expect {| |}]

(* Size reports settle by the depth rule: a response that resizes a node at
   the depth already reported waits for the frame's paint, so a layout that
   oscillates with its own size still presents every frame. *)
let%expect_test "an on_size response that resizes its own node presents frames"
    =
  let views = ref 0 and reports = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> (true, Mosaic.Cmd.none));
      update =
        (fun (`Size w) _wide ->
          reports := w :: !reports;
          ( w < 30,
            if List.length !reports = 6 then Mosaic.Cmd.quit
            else Mosaic.Cmd.none ));
      view =
        (fun wide ->
          incr views;
          if !views > 1_000 then failwith "the size loop never presents";
          Mosaic.box
            [
              Mosaic.box
                ~size:(Mosaic.size ~width:(if wide then 40 else 20) ~height:1)
                ~on_size:(fun ~w ~h:_ -> Some (`Size w))
                [ Mosaic.text (if wide then "wide" else "narrow") ];
            ]);
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  let screens = ref [] in
  let t =
    Matrix_test.create ~width:60 ~height:2
      ~on_idle:(fun t ~timeout:_ -> Matrix_test.stop t)
      ()
  in
  Mosaic.run ~matrix:(Matrix_test.app t)
    {
      application with
      update =
        (fun message model ->
          screens := String.trim (Matrix_test.screen t) :: !screens;
          application.update message model);
    };
  Printf.printf "reports: %s\n"
    (String.concat " " (List.rev_map string_of_int !reports));
  Printf.printf "views per report: %d/%d\n" !views (List.length !reports);
  Printf.printf "presented before each report: %s\n"
    (String.concat " | " (List.rev !screens));
  [%expect
    {|
    reports: 40 20 40 20 40 20 40 20
    views per report: 9/8
    presented before each report:  |  | wide | wide | wide | narrow | narrow | narrow
    |}]

let%expect_test "on_size reports settle within the frame or the next one" =
  let application =
    {
      Mosaic.init = (fun () -> ((None, None, None), Mosaic.Cmd.none));
      update =
        (fun message (outer, inner, sibling) ->
          ( (match message with
            | `Outer w -> (Some w, inner, sibling)
            | `Inner w -> (outer, Some w, sibling)
            | `Sibling w -> (outer, inner, Some w)),
            Mosaic.Cmd.none ));
      view =
        (fun (outer, inner, sibling) ->
          let show = function None -> "-" | Some w -> string_of_int w in
          (* The inner width follows the outer report, one level deeper; the
             sibling's width follows the inner report at the outer's depth. *)
          let inner_width = Option.fold ~none:4 ~some:(fun w -> w / 2) outer in
          let sibling_width =
            Option.fold ~none:4 ~some:(fun w -> w + 1) inner
          in
          Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column
            ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100))
            [
              Mosaic.box
                ~on_size:(fun ~w ~h:_ -> Some (`Outer w))
                [
                  Mosaic.box
                    ~size:(Mosaic.size ~width:inner_width ~height:1)
                    ~on_size:(fun ~w ~h:_ -> Some (`Inner w))
                    [];
                ];
              Mosaic.box
                ~size:(Mosaic.size ~width:sibling_width ~height:1)
                ~on_size:(fun ~w ~h:_ -> Some (`Sibling w))
                [
                  Mosaic.text
                    (Printf.sprintf "%s %s %s" (show outer) (show inner)
                       (show sibling));
                ];
            ]);
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  drive ~width:30 ~height:3 application [ `Snap; `Resize (40, 3); `Snap ];
  [%expect {|
    |
    |30 15 16
    |
    |
    |40 20 21
    |
    |}]

let%expect_test "timer ids must be unique across every and on_tick" =
  let subs _ =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.every ~id:"clock" 1. (fun () -> Tick);
        Mosaic.Sub.on_tick ~id:"clock" (fun ~dt:_ -> Tick);
      ]
  in
  (match drive (app ~subs ()) [] with
  | () -> print_endline "accepted"
  | exception Invalid_argument message -> print_endline message);
  [%expect {| Mosaic.Sub: empty or duplicate timer id: clock |}]

(* An application that records every message it receives, by name. *)
let recording subs =
  let observed = ref initial in
  let base = app ~subs () in
  ( observed,
    {
      base with
      Mosaic.update =
        (fun msg model ->
          let model, cmd = update msg model in
          observed := model;
          (model, cmd));
    } )

let%expect_test "a paste is delivered whole and a cut paste is dropped" =
  let observed, application =
    recording (fun _ ->
        Mosaic.Sub.on_paste (fun ev ->
            Some (Pressed ("p:" ^ Mosaic.Event.Paste.text ev))))
  in
  let feed event = `Run (fun t -> Matrix_test.feed_event t event) in
  drive ~width:70 ~height:1 application
    [
      feed Matrix.Paste_start;
      feed (Matrix.Paste_data "ab");
      feed (Matrix.Paste_data "cd");
      `Run
        (fun _ ->
          equal ~msg:"nothing is delivered before the end marker" (list string)
            [] !observed.keys);
      feed (Matrix.Paste_end `Complete);
      `Run
        (fun _ ->
          equal ~msg:"the pieces are delivered as one paste" (list string)
            [ "p:abcd" ] !observed.keys);
      feed Matrix.Paste_start;
      feed (Matrix.Paste_data "lost");
      feed (Matrix.Paste_end `Cut);
      `Feed "\027[200~he";
      `Feed "llo\027[201~";
      feed (Matrix.Text "é");
      `Run
        (fun _ ->
          equal
            ~msg:
              "a cut paste is dropped, a bracketed paste split across reads \
               arrives whole, and text without a key is a paste"
            (list string)
            [ "p:abcd"; "p:hello"; "p:é" ]
            !observed.keys);
    ];
  [%expect {| |}]

let%expect_test "a stalled paste is reported, and the application aborts it" =
  let observed, application =
    recording (fun _ ->
        Mosaic.Sub.batch
          [
            Mosaic.Sub.on_paste_stalled 2. (Pressed "<stalled>");
            Mosaic.Sub.on_paste (fun ev ->
                Some (Pressed ("p:" ^ Mosaic.Event.Paste.text ev)));
            Mosaic.Sub.on_key (fun key -> Some (Pressed (key_name key)));
          ])
  in
  let application =
    {
      application with
      Mosaic.update =
        (fun msg model ->
          let model, cmd = application.Mosaic.update msg model in
          match msg with
          | Pressed "<stalled>" -> (model, Mosaic.Cmd.abort_paste)
          | _ -> (model, cmd));
    }
  in
  drive ~width:70 ~height:1 application
    [
      `Feed "\027[200~ab";
      `Step_to 1.;
      `Feed "c\003";
      `Step_to 2.5;
      `Run
        (fun _ ->
          equal ~msg:"data, Ctrl+C too, keeps the paste going" (list string) []
            !observed.keys);
      `Step_to 3.5;
      `Feed "x";
      `Step_to 8.;
      `Run
        (fun _ ->
          equal ~msg:"reported once, aborted and dropped, then typing"
            (list string) [ "<stalled>"; "x" ] !observed.keys);
    ];
  [%expect {| |}]

let%expect_test "key releases reach on_key_release, not key handlers" =
  let observed, application =
    recording (fun _ ->
        Mosaic.Sub.batch
          [
            Mosaic.Sub.on_key (fun key -> Some (Pressed (key_name key)));
            Mosaic.Sub.on_key_release (fun release ->
                match release.key with
                | Char u when Uchar.is_char u ->
                    Some (Pressed ("r:" ^ String.make 1 (Uchar.to_char u)))
                | _ -> None);
          ])
  in
  let release : Matrix.Input.Keystroke.release =
    {
      key = Char (Uchar.of_char 'q');
      mods = Matrix.Input.Keystroke.no_mods;
      base = None;
      keypad = false;
    }
  in
  drive ~width:70 ~height:1 application
    [
      `Feed "q";
      `Run (fun t -> Matrix_test.feed_event t (Matrix.Key_release release));
      `Run
        (fun _ ->
          equal ~msg:"the press reaches key handlers, the release does not"
            (list string) [ "q"; "r:q" ] !observed.keys);
    ];
  [%expect {| |}]

(* ── Event delivery ── *)

(* Two inputs, A focused above B, each accepting its proposals and focus
   requests, under [subs]. Returns the texts accepted after [bytes]. *)
let two_inputs ?(subs = fun _ -> Mosaic.Sub.none) bytes =
  let texts = ref ("", "") in
  let application =
    {
      Mosaic.init =
        (fun () ->
          ((Mosaic.Editor.empty, Mosaic.Editor.empty, `A), Mosaic.Cmd.none));
      update =
        (fun msg (a, b, focus) ->
          let ((a, b, _) as model) =
            match msg with
            | `Edit_a a -> (a, b, focus)
            | `Edit_b b -> (a, b, focus)
            | `Focus focus -> (a, b, focus)
          in
          texts := (Mosaic.Editor.text a, Mosaic.Editor.text b);
          (model, Mosaic.Cmd.none));
      view =
        (fun (a, b, focus) ->
          let input state name edit =
            Mosaic.input
              ~size:(Mosaic.size ~width:20 ~height:1)
              ~state ~focused:(focus = name) ~on_focus_request:(`Focus name)
              ~on_change:edit ()
          in
          Mosaic.box ~flex_direction:Column
            [ input a `A (fun a -> `Edit_a a); input b `B (fun b -> `Edit_b b) ]);
      subscriptions = subs;
    }
  in
  drive ~width:20 ~height:2 application [ `Feed bytes ];
  !texts

let pair = pair string string

let%expect_test "K1: a capture listener that moves focus moves the typing too" =
  (* The elements' handlers keep the path taken; the insertion runs on the
     element focused when it runs. *)
  let subs _ =
    Mosaic.Sub.on_key ~phase:Capture (fun key ->
        if Mosaic.Shortcut.matches (Mosaic.Shortcut.char 'x') key then
          Some (`Focus `B)
        else None)
  in
  equal ~msg:"x lands in B" pair ("", "x") (two_inputs ~subs "x");
  [%expect {| |}]

let%expect_test "K2: a key that moves focus moves the next key of the read" =
  let subs _ = Mosaic.Sub.on_keys [ (Mosaic.Shortcut.tab, `Focus `B) ] in
  equal ~msg:"Tab types nothing, bubbles, and focuses B for x" pair ("", "x")
    (two_inputs ~subs "\tx");
  [%expect {| |}]

let%expect_test "K3: a click's focus request moves the next key of the read" =
  equal ~msg:"x goes to B, focused by the click" pair ("", "x")
    (two_inputs "\027[<0;3;2M\027[<0;3;2mx");
  [%expect {| |}]

let%expect_test "K4: a keyed reorder keeps focus for the next key of the read" =
  let final = ref [] in
  let application =
    {
      Mosaic.init =
        (fun () ->
          ( [
              ("k1", Mosaic.Editor.empty);
              ("k2", Mosaic.Editor.empty);
              ("k3", Mosaic.Editor.empty);
            ],
            Mosaic.Cmd.none ));
      update =
        (fun msg items ->
          let items =
            match msg with
            | `Reverse -> List.rev items
            | `Edit (key, editor) ->
                List.map
                  (fun (k, e) -> if k = key then (k, editor) else (k, e))
                  items
          in
          final := List.map (fun (k, e) -> (k, Mosaic.Editor.text e)) items;
          (items, Mosaic.Cmd.none));
      view =
        (fun items ->
          Mosaic.box ~flex_direction:Column
            (List.map
               (fun (key, state) ->
                 Mosaic.input ~key
                   ~size:(Mosaic.size ~width:20 ~height:1)
                   ~state ~focused:(key = "k1")
                   ~on_change:(fun e -> `Edit (key, e))
                   ())
               items));
      subscriptions =
        (fun _ -> Mosaic.Sub.on_keys [ (Mosaic.Shortcut.ctrl 'r', `Reverse) ]);
    }
  in
  drive ~width:20 ~height:3 application [ `Feed "\018x" ];
  equal ~msg:"x goes to k1, wherever it moved" (list pair)
    [ ("k3", ""); ("k2", ""); ("k1", "x") ]
    !final;
  [%expect {| |}]

let%expect_test "K5: a click hits the layout the previous key left" =
  let clicked = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> ([ "r0"; "r1"; "r2" ], Mosaic.Cmd.none));
      update =
        (fun msg rows ->
          match msg with
          | `Delete -> (List.tl rows, Mosaic.Cmd.none)
          | `Click row ->
              clicked := row :: !clicked;
              (rows, Mosaic.Cmd.none));
      view =
        (fun rows ->
          Mosaic.box ~flex_direction:Column
            (List.map
               (fun row ->
                 Mosaic.box ~key:row
                   ~size:(Mosaic.size ~width:10 ~height:1)
                   ~on_click:(fun _ -> Some (`Click row))
                   [ Mosaic.text row ])
               rows));
      subscriptions =
        (fun _ -> Mosaic.Sub.on_keys [ (Mosaic.Shortcut.char 'd', `Delete) ]);
    }
  in
  drive ~width:10 ~height:3 application [ `Feed "d\027[<0;2;1M\027[<0;2;1m" ];
  equal ~msg:"the click hits r1, now on the first row" (list string) [ "r1" ]
    !clicked;
  [%expect {| |}]

(* Messages by name, in the order [update] received them. *)
let names subs view bytes =
  let seen = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> ((), Mosaic.Cmd.none));
      update =
        (fun name () ->
          seen := name :: !seen;
          ((), Mosaic.Cmd.none));
      view = (fun () -> view);
      subscriptions = (fun () -> subs);
    }
  in
  drive ~width:10 ~height:1 application [ `Feed bytes ];
  List.rev !seen

let downs name ev =
  match Mosaic.Event.Mouse.kind ev with Down _ -> Some name | _ -> None

let%expect_test
    "E1: a widget that stops a press hides it from bubble listeners only" =
  let view =
    Mosaic.box
      ~size:(Mosaic.size ~width:10 ~height:1)
      ~on_mouse:(fun ev ->
        Mosaic.Event.Mouse.stop_propagation ev;
        downs "widget" ev)
      [ Mosaic.text "w" ]
  in
  let subs =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.on_mouse ~phase:Capture (downs "capture-1");
        Mosaic.Sub.on_mouse (downs "bubble");
        Mosaic.Sub.on_mouse ~phase:Capture (downs "capture-2");
      ]
  in
  equal ~msg:"both capture listeners, then the widget" (list string)
    [ "capture-1"; "capture-2"; "widget" ]
    (names subs view "\027[<0;2;1M");
  [%expect {| |}]

let%expect_test
    "E2: a capture listener that stops a key hides it from no other capture \
     listener" =
  let subs =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.on_key ~phase:Capture (fun key ->
            Mosaic.Event.Key.stop_propagation key;
            Some "stops");
        Mosaic.Sub.on_key (fun _ -> Some "bubble");
        Mosaic.Sub.on_key ~phase:Capture (fun _ -> Some "sees");
      ]
  in
  equal ~msg:"the application is one node" (list string) [ "stops"; "sees" ]
    (names subs (Mosaic.text "e2") "z");
  [%expect {| |}]

let%expect_test "E3: listeners run by phase, then in declaration order" =
  let subs =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.on_key (fun _ -> Some "a");
        Mosaic.Sub.map
          (fun s -> "map:" ^ s)
          (Mosaic.Sub.batch
             [
               Mosaic.Sub.on_key (fun _ -> Some "b");
               Mosaic.Sub.on_key ~phase:Capture (fun _ -> Some "c");
             ]);
        Mosaic.Sub.on_key (fun _ -> Some "d");
      ]
  in
  equal
    ~msg:"capture first, each phase in declaration order through batch and map"
    (list string)
    [ "map:c"; "a"; "map:b"; "d" ]
    (names subs (Mosaic.text "e3") "k");
  [%expect {| |}]

let%expect_test "S1: a shortcut does not take a key typed into an input" =
  let text = ref "" and quit = ref 0 in
  let application =
    {
      Mosaic.init = (fun () -> (Mosaic.Editor.empty, Mosaic.Cmd.none));
      update =
        (fun msg editor ->
          match msg with
          | `Quit ->
              incr quit;
              (editor, Mosaic.Cmd.none)
          | `Edit editor ->
              text := Mosaic.Editor.text editor;
              (editor, Mosaic.Cmd.none));
      view =
        (fun editor ->
          Mosaic.input
            ~size:(Mosaic.size ~width:20 ~height:1)
            ~state:editor ~focused:true
            ~on_change:(fun e -> `Edit e)
            ());
      subscriptions =
        (fun _ ->
          Mosaic.Sub.on_keys
            [
              (Mosaic.Shortcut.char 'q', `Quit);
              (Mosaic.Shortcut.ctrl 'q', `Quit);
            ]);
    }
  in
  drive ~width:20 ~height:1 application [ `Feed "aq" ];
  equal ~msg:"q is typed" string "aq" !text;
  equal ~msg:"q does not quit" int 0 !quit;
  drive ~width:20 ~height:1 application [ `Feed "\017" ];
  equal ~msg:"Ctrl+Q types nothing, so it bubbles to the shortcut" int 1 !quit;
  [%expect {| |}]

let%expect_test "removing the focused element leaves nothing focused" =
  (* The update that removes the focused input does not move focus: keys
     then reach the application only, until the model declares another. *)
  let keys = ref [] and inputs = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> (([ "a"; "b" ], Some "a"), Mosaic.Cmd.none));
      update =
        (fun msg (rows, focus) ->
          match msg with
          | `Remove ->
              (List.filter (fun r -> Some r <> focus) rows, focus)
              |> fun model -> (model, Mosaic.Cmd.none)
          | `Focus row -> ((rows, Some row), Mosaic.Cmd.none)
          | `Key name ->
              keys := name :: !keys;
              ((rows, focus), Mosaic.Cmd.none)
          | `Edit (row, text) ->
              inputs := (row, text) :: !inputs;
              ((rows, focus), Mosaic.Cmd.none));
      view =
        (fun (rows, focus) ->
          Mosaic.box ~flex_direction:Column
            (List.map
               (fun row ->
                 Mosaic.input ~key:row
                   ~size:(Mosaic.size ~width:20 ~height:1)
                   ~state:Mosaic.Editor.empty ~focused:(focus = Some row)
                   ~on_change:(fun e -> `Edit (row, Mosaic.Editor.text e))
                   ())
               rows));
      subscriptions =
        (fun _ ->
          Mosaic.Sub.batch
            [
              Mosaic.Sub.on_keys
                [
                  (Mosaic.Shortcut.f 2, `Remove);
                  (Mosaic.Shortcut.f 3, `Focus "b");
                ];
              Mosaic.Sub.on_key (fun key ->
                  match key_name key with
                  | "<other>" -> None
                  | name -> Some (`Key name));
            ]);
    }
  in
  drive ~width:20 ~height:2 application [ `Feed "\027OQx\027OR" ];
  equal ~msg:"x reached the application, no element" (list string) [ "x" ] !keys;
  equal ~msg:"no input took x" int 0 (List.length !inputs);
  drive ~width:20 ~height:2 application [ `Feed "\027OQ\027ORy" ];
  equal ~msg:"declaring b focused gives it the next key" (list pair)
    [ ("b", "y") ]
    !inputs;
  [%expect {| |}]
