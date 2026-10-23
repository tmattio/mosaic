open Windtrap

exception Injected_startup_failure

let[@inline never] raise_injected_startup_failure () =
  raise Injected_startup_failure

type fake_state = {
  mutable now_s : float;
  mutable read_calls : int;
  mutable cleanup_calls : int;
  mutable raw_restore_calls : int;
  mutable flush_input_calls : int;
  wake_calls : int Atomic.t;
  mutable pending_events : Matrix.event list;
  mutable pending_input_chunks : string list;
  mutable read_timeouts : float option list;
  output : Buffer.t;
}

let make_state events input_chunks =
  {
    now_s = 0.;
    read_calls = 0;
    cleanup_calls = 0;
    raw_restore_calls = 0;
    flush_input_calls = 0;
    wake_calls = Atomic.make 0;
    pending_events = events;
    pending_input_chunks = input_chunks;
    read_timeouts = [];
    output = Buffer.create 256;
  }

let output state = Buffer.contents state.output

type watched_signal = { name : string; number : int }

let watched_signals =
  [
    { name = "SIGTERM"; number = Sys.sigterm };
    { name = "SIGINT"; number = Sys.sigint };
    { name = "SIGQUIT"; number = Sys.sigquit };
    { name = "SIGABRT"; number = Sys.sigabrt };
    { name = "SIGHUP"; number = Sys.sighup };
    { name = "SIGWINCH"; number = Sys.sigwinch };
  ]

let peek_signal number =
  let behavior = Sys.signal number Sys.Signal_ignore in
  ignore (Sys.signal number behavior : Sys.signal_behavior);
  behavior

let with_distinct_signal_handlers fn =
  let installed = ref [] in
  let restore () =
    List.iter
      (fun (signal, previous, _) ->
        ignore (Sys.signal signal.number previous : Sys.signal_behavior))
      !installed
  in
  Fun.protect ~finally:restore (fun () ->
      List.iter
        (fun signal ->
          let handler _ = () in
          match Sys.signal signal.number (Sys.Signal_handle handler) with
          | previous -> installed := (signal, previous, handler) :: !installed
          | exception Invalid_argument _ -> ())
        watched_signals;
      fn (List.rev !installed))

let assert_distinct_handlers entries =
  List.iter
    (fun (signal, _, expected) ->
      match peek_signal signal.number with
      | Sys.Signal_handle actual when actual == expected -> ()
      | _ -> failf "%s prior disposition was not restored" signal.name)
    entries

(* The environment, as a value: the tests never read the process's. *)
let env vars =
  Matrix.Terminal.Caps.env (fun name -> List.assoc_opt name vars) ~win32:false

let no_env = env []

let key ?(mods = Matrix.Input.Keystroke.no_mods) ?(text = "") key =
  Matrix.Key { key; mods; text; base = None; keypad = false; repeat = false }

let char c = key ~text:(String.make 1 c) (Char (Uchar.of_char c))

let make_app ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(start_idle = false) ?(input_timeout = Some 0.) ?(terminal_tty = false)
    ?(advance_now = true) ?(sleep_until_timeout = false)
    ?(read_quantum_s = 0.0001) ?(emit_event_each_read = None) ?(events = [])
    ?(input_chunks = []) ?(startup_input = []) ?startup_cursor ?stop_after_reads
    ?end_after_reads ?(min_tui_height = 1) ?(resize_debounce = Some 0.)
    ?(width = 80) ?(height = 24) ?hyperlinks ?query_cursor_position
    ?(env = no_env) () =
  let state = make_state events input_chunks in
  let app_ref = ref None in
  let startup_chunks = ref startup_input in
  let cursor_reply = ref None in
  let output_is_tty =
    terminal_tty
    || Option.is_some startup_cursor
    || Option.is_some query_cursor_position
  in
  let input_is_tty =
    Option.is_some startup_cursor
    || Option.is_some query_cursor_position
    || startup_input <> []
  in
  let now () =
    if advance_now then state.now_s <- state.now_s +. 0.001;
    state.now_s
  in
  let write_output buf off len =
    Buffer.add_subbytes state.output buf off len;
    (* A terminal answers the startup probe at least with its DA1, unless the
       test gives the answer. *)
    if
      startup_input = []
      && String.starts_with ~prefix:"\027 F" (Bytes.sub_string buf off len)
    then startup_chunks := [ "\027[?62c" ];
    if Bytes.sub_string buf off len = "\027[6n" then
      cursor_reply :=
        match !app_ref with
        | None -> startup_cursor
        | Some _ ->
            Option.bind query_cursor_position (fun query -> query ~timeout:0.)
  in
  let wait ~timeout ~on_event =
    if Option.is_some !cursor_reply || !startup_chunks <> [] then true
    else if Option.is_none !app_ref then (
      state.now_s <- state.now_s +. Option.value timeout ~default:0.;
      false)
    else (
      state.read_calls <- state.read_calls + 1;
      state.read_timeouts <- timeout :: state.read_timeouts;
      let dt =
        if sleep_until_timeout then
          match timeout with
          | Some t when t > read_quantum_s -> t
          | _ -> read_quantum_s
        else read_quantum_s
      in
      state.now_s <- state.now_s +. dt;
      Option.iter on_event emit_event_each_read;
      let events = state.pending_events in
      state.pending_events <- [];
      List.iter on_event events;
      (match (stop_after_reads, !app_ref) with
      | Some max_reads, Some app when state.read_calls >= max_reads ->
          Matrix.stop app
      | _ -> ());
      match state.pending_input_chunks with
      | "" :: rest ->
          state.pending_input_chunks <- rest;
          false
      | _ :: _ -> true
      | [] ->
          Option.fold ~none:false
            ~some:(fun max_reads -> state.read_calls >= max_reads)
            end_after_reads)
  in
  let read_input buf off len =
    let chunk =
      match !cursor_reply with
      | Some (row, col) ->
          cursor_reply := None;
          Printf.sprintf "\027[%d;%dR" row col
      | None -> (
          match !startup_chunks with
          | chunk :: rest ->
              startup_chunks := rest;
              chunk
          | [] -> (
              match state.pending_input_chunks with
              | chunk :: rest ->
                  state.pending_input_chunks <- rest;
                  chunk
              | [] -> ""))
    in
    let n = String.length chunk in
    if n > len then fail "fake input chunk exceeds runtime read buffer";
    Bytes.blit_string chunk 0 buf off n;
    n
  in
  let backend : Matrix.Backend.t =
    {
      now;
      wake = (fun () -> Atomic.incr state.wake_calls);
      write_output;
      read_input;
      wait;
      input_is_tty;
      output_is_tty;
      terminal_size = (fun () -> (width, height));
      set_raw_mode =
        (fun enabled ->
          if not enabled then
            state.raw_restore_calls <- state.raw_restore_calls + 1);
      flush_input =
        (fun () -> state.flush_input_calls <- state.flush_input_calls + 1);
      cleanup = (fun () -> state.cleanup_calls <- state.cleanup_calls + 1);
      protect = (fun f -> f ());
      env;
    }
  in
  let caps =
    Option.map
      (fun hyperlinks ->
        {
          (Matrix.Terminal.Caps.derive (Matrix.Terminal.Caps.evidence no_env)) with
          hyperlinks;
        })
      hyperlinks
  in
  let app =
    Matrix.attach ~mode ~raw_mode ~target_fps ~start_idle ~input_timeout
      ~min_tui_height ~resize_debounce ?caps ~backend ()
  in
  app_ref := Some app;
  Buffer.clear state.output;
  (app, state)

let mouse kind x y =
  Matrix.Mouse { kind; x; y; shift = false; alt = false; ctrl = false }

let mouse_press = mouse (Press Left)
let mouse_release = mouse (Release (Some Left))
let mouse_motion = mouse (Motion (Some Left))
let mouse_scroll = mouse (Scroll `Down)

let set_sync_capable app =
  let terminal = Matrix.terminal app in
  let caps = Matrix.Terminal.capabilities terminal in
  Matrix.Terminal.set_capabilities terminal { caps with sync = true }

(* A terminal for the runtime: a VTE fed everything the runtime writes, whose
   replies, cursor position reports among them, are the runtime's input; the
   runtime anchors the primary screen on those reports. [shell] is the output
   before the application starts. Under {!Matrix.run}, each time the runtime
   waits for input it takes the next step of [script], and stops once there is
   none. *)
type term = {
  vte : Vte.t;
  mutable size : int * int;
  replies : Buffer.t;
  mutable script : (on_event:(Matrix.event -> unit) -> unit) list;
}

let vte_app ?(mode = `Primary) ?(min_tui_height = 1) ?(shell = "") ~cols ~rows
    () =
  let vte = Vte.create ~scrollback:100 ~rows ~cols () in
  ignore (Vte.feed_string vte shell : string);
  let term =
    { vte; size = (cols, rows); replies = Buffer.create 16; script = [] }
  in
  let now = ref 0. in
  let app_ref = ref None in
  let read_input buf off len =
    let n = min len (Buffer.length term.replies) in
    Buffer.blit term.replies 0 buf off n;
    let rest = Buffer.sub term.replies n (Buffer.length term.replies - n) in
    Buffer.clear term.replies;
    Buffer.add_string term.replies rest;
    if n = 0 then -1 else n
  in
  let wait ~timeout ~on_event =
    if Buffer.length term.replies = 0 then (
      now := !now +. Option.value timeout ~default:0.;
      match (term.script, !app_ref) with
      | step :: rest, Some _ ->
          term.script <- rest;
          step ~on_event
      | [], Some app -> Matrix.stop app
      | _, None -> ());
    Buffer.length term.replies > 0
  in
  let backend : Matrix.Backend.t =
    {
      now = (fun () -> !now);
      wake = ignore;
      write_output =
        (fun buf off len ->
          Buffer.add_string term.replies (Vte.feed vte buf off len));
      read_input;
      wait;
      input_is_tty = true;
      output_is_tty = true;
      terminal_size = (fun () -> term.size);
      set_raw_mode = ignore;
      flush_input = ignore;
      protect = (fun f -> f ());
      cleanup = ignore;
      env = no_env;
    }
  in
  let app =
    Matrix.attach ~mode ~target_fps:None ~mouse_enabled:false
      ~bracketed_paste:false ~kitty_keyboard:`Off ~resize_debounce:None
      ~pace_redraws:false ~min_tui_height ~backend ()
  in
  app_ref := Some app;
  (app, term)

let draw_rows app rows =
  List.iteri
    (fun y text -> Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y ~text)
    rows

let present ?required app rows =
  Matrix.prepare ?primary_required_rows:required app;
  draw_rows app rows;
  Matrix.submit app

let rtrim s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

let screen_rows term =
  List.map rtrim (String.split_on_char '\n' (Vte.to_string term.vte))

(* The rows above the live region, after those in the scrollback: what the
   transcript has become. *)
let transcript app term =
  let _, rows = Matrix.full_size app and _, live = Matrix.size app in
  Vte.scrollback_lines term.vte
  @ List.filteri (fun y _ -> y < rows - live) (screen_rows term)

let live_rows app term =
  let _, rows = Matrix.full_size app and _, live = Matrix.size app in
  List.filteri (fun y _ -> y >= rows - live) (screen_rows term)

let resize_step term ~cols ~rows ~on_event =
  Vte.resize term.vte ~rows ~cols;
  term.size <- (cols, rows);
  on_event (Matrix.Resize (cols, rows))

let test_request_redraw_while_paused () =
  let app, _state =
    make_app ~target_fps:(Some 30.) ~input_timeout:(Some 0.)
      ~stop_after_reads:5000 ()
  in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      match !frames with
      | 1 ->
          Matrix.pause app;
          Matrix.request_redraw app
      | 2 -> Matrix.stop app
      | _ -> Matrix.stop app);
  equal ~msg:"redraw triggers one-shot frame while paused" int 2 !frames

let test_cross_domain_redraw_during_render () =
  let app = fst (make_app ~target_fps:None ~stop_after_reads:5 ()) in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      is_false ~msg:"the current request is consumed before rendering"
        (Matrix.redraw_requested app);
      if !frames = 1 then begin
        let worker = Domain.spawn (fun () -> Matrix.request_redraw app) in
        Domain.join worker;
        is_true ~msg:"worker notification remains pending during rendering"
          (Matrix.redraw_requested app)
      end
      else Matrix.stop app);
  equal ~msg:"worker notification schedules the following frame" int 2 !frames

let test_redraw_races_lifecycle () =
  let app, state = make_app ~target_fps:None () in
  let finished = Atomic.make false in
  let started = Atomic.make false in
  let worker =
    Domain.spawn (fun () ->
        Atomic.set started true;
        while not (Atomic.get finished) do
          Matrix.request_redraw app;
          Domain.cpu_relax ()
        done)
  in
  Fun.protect
    ~finally:(fun () ->
      Atomic.set finished true;
      Domain.join worker;
      Matrix.close app)
    (fun () ->
      while not (Atomic.get started) do
        Domain.cpu_relax ()
      done;
      for _ = 1 to 50 do
        Matrix.suspend app;
        is_false ~msg:"concurrent notifications cannot redraw while suspended"
          (Matrix.redraw_requested app);
        Matrix.resume app;
        is_true ~msg:"resuming schedules a fresh frame"
          (Matrix.redraw_requested app)
      done;
      Matrix.close app;
      is_false ~msg:"concurrent notifications cannot redraw after close"
        (Matrix.redraw_requested app));
  let wakes = Atomic.get state.wake_calls in
  Matrix.request_redraw app;
  equal ~msg:"closed apps ignore new wake requests" int wakes
    (Atomic.get state.wake_calls)

let test_immediate_redraw_bypasses_live_cadence_once () =
  let app, state =
    make_app ~target_fps:(Some 1.) ~input_timeout:None ~sleep_until_timeout:true
      ~emit_event_each_read:(Some Matrix.Focus_in) ~stop_after_reads:5000 ()
  in
  let frames = ref 0 in
  let request_at = ref None in
  let second_frame_at = ref 0. in
  Matrix.run app
    ~on_input:(fun app _event ->
      match !request_at with
      | None ->
          request_at := Some state.now_s;
          Matrix.request_immediate_redraw app
      | Some _ -> ())
    ~on_render:(fun app ->
      incr frames;
      if !frames = 2 then (
        second_frame_at := state.now_s;
        Matrix.stop app));
  equal ~msg:"one immediate follow-up frame" int 2 !frames;
  match !request_at with
  | Some requested ->
      satisfies ~claim:"follow-up bypasses the one-second cadence (< 0.1s)"
        float_exact
        (fun dt -> dt < 0.1)
        (!second_frame_at -. requested)
  | None -> fail "missing redraw request"

let test_idle_does_not_force_live_loop () =
  let app, _state =
    make_app ~target_fps:None ~input_timeout:(Some 0.) ~stop_after_reads:1 ()
  in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun _app -> incr frames);
  equal ~msg:"idle renders only the initial frame" int 1 !frames

let test_live_requested_while_suspended () =
  let app, state =
    make_app ~start_idle:true ~input_timeout:None ~advance_now:false
      ~sleep_until_timeout:true ~read_quantum_s:0.05 ~stop_after_reads:8 ()
  in
  Matrix.suspend app;
  Matrix.request_live app;
  Matrix.resume app;
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      if !frames = 3 then Matrix.stop app);
  equal ~msg:"live demand acquired while suspended resumes cadence" int 3
    !frames;
  is_true ~msg:"cadence progresses before the watchdog" (state.read_calls < 8)

let test_uncapped_live_cadence () =
  List.iter
    (fun (name, activate) ->
      let app, state =
        make_app ~target_fps:None ~input_timeout:None ~advance_now:false
          ~stop_after_reads:8 ()
      in
      activate app;
      let frames = ref 0 and waits = ref [] in
      Matrix.run app ~on_render:(fun app ->
          incr frames;
          if !frames = 3 then begin
            waits := state.read_timeouts;
            Matrix.stop app
          end);
      equal ~msg:(name ^ " keeps uncapped cadence active") int 3 !frames;
      is_true ~msg:"uncapped frames never wait for a deadline"
        (List.for_all
           (function Some timeout -> timeout = 0. | None -> false)
           !waits))
    [ ("start", Matrix.start); ("request_live", Matrix.request_live) ]

let test_demand_changes_while_suspended () =
  List.iter
    (fun (name, change, expected_frames) ->
      let app, state =
        make_app ~start_idle:true ~input_timeout:None ~advance_now:false
          ~sleep_until_timeout:true ~read_quantum_s:0.05 ~stop_after_reads:8 ()
      in
      Matrix.suspend app;
      Buffer.clear state.output;
      change app;
      Matrix.request_redraw app;
      Matrix.submit app;
      equal
        ~msg:(name ^ " does not reclaim suspended terminal")
        string "" (output state);
      is_false
        ~msg:(name ^ " leaves redraw disabled while suspended")
        (Matrix.redraw_requested app);
      Matrix.resume app;
      let frames = ref 0 in
      Matrix.run app ~on_render:(fun app ->
          incr frames;
          if !frames = 3 then Matrix.stop app);
      equal
        ~msg:(name ^ " determines resumed cadence")
        int expected_frames !frames)
    [
      ("start", Matrix.start, 3);
      ( "start then pause",
        (fun app ->
          Matrix.start app;
          Matrix.pause app),
        1 );
      ( "request then drop",
        (fun app ->
          Matrix.request_live app;
          Matrix.drop_live app),
        1 );
      ( "drop while explicitly started",
        (fun app ->
          Matrix.start app;
          Matrix.request_live app;
          Matrix.drop_live app),
        3 );
      ( "request while paused",
        (fun app ->
          Matrix.pause app;
          Matrix.request_live app),
        1 );
    ]

let test_stopped_and_closed_frame_eligibility () =
  let app, state = make_app ~target_fps:None () in
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"X";
  Matrix.stop app;
  Buffer.clear state.output;
  Matrix.submit app;
  equal ~msg:"stopped app does not submit" string "" (output state);
  Matrix.start app;
  Matrix.submit app;
  equal ~msg:"start restores manual presentation" string "X"
    (Matrix.Grid.get_text (Matrix.current_grid app) 0);
  Matrix.close app;
  Buffer.clear state.output;
  Matrix.start app;
  Matrix.pause app;
  Matrix.request_live app;
  Matrix.resume app;
  Matrix.submit app;
  Matrix.run app ~on_render:(fun app ->
      ignore app;
      fail "closed app rendered");
  is_false ~msg:"closed app cannot restart" (Matrix.running app);
  equal ~msg:"closed control operations emit nothing" string "" (output state);
  equal ~msg:"close is not repeated" int 1 state.cleanup_calls;
  let stopped, stopped_state = make_app () in
  Matrix.stop stopped;
  Matrix.run stopped ~on_render:(fun app ->
      ignore app;
      fail "run restarted stopped app");
  equal ~msg:"run on a stopped app closes without reading" int 0
    stopped_state.read_calls;
  equal ~msg:"stopped app cleanup runs once" int 1 stopped_state.cleanup_calls

let test_stop_while_suspended_requires_start_after_resume () =
  let app, state = make_app () in
  Matrix.suspend app;
  Matrix.stop app;
  Matrix.resume app;
  is_false ~msg:"resume does not undo stop" (Matrix.running app);
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"X";
  Buffer.clear state.output;
  Matrix.submit app;
  equal ~msg:"resumed stopped app emits no frame" string "" (output state);
  Matrix.start app;
  Matrix.submit app;
  equal ~msg:"explicit start restores eligibility" string "X"
    (Matrix.Grid.get_text (Matrix.current_grid app) 0);
  Matrix.close app

let test_frame_callbacks_respect_lifecycle () =
  List.iter
    (fun (name, control, presents) ->
      List.iter
        (fun before_render ->
          let app, state = make_app ~stop_after_reads:8 () in
          let renders = ref 0 in
          Matrix.run app
            ~on_frame:(fun app ~dt ->
              ignore dt;
              if before_render then control app)
            ~on_render:(fun app ->
              incr renders;
              Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"X";
              if not before_render then control app);
          equal
            ~msg:(name ^ " controls on_render eligibility")
            int
            (if before_render && not presents then 0 else 1)
            !renders;
          equal
            ~msg:(name ^ " controls presentation eligibility")
            bool presents
            (Matrix.Grid.get_text (Matrix.current_grid app) 0 = "X");
          equal ~msg:(name ^ " closes once") int 1 state.cleanup_calls)
        [ true; false ])
    [
      ("stop", Matrix.stop, false);
      ("close", Matrix.close, false);
      ("suspend", (fun app -> Matrix.suspend app), false);
      ("pause", Matrix.pause, true);
    ]

let test_suspended_events_defer_terminal_operations () =
  let app, state =
    make_app ~terminal_tty:true ~target_fps:None ~resize_debounce:None
      ~input_timeout:None ~stop_after_reads:8
      ~events:
        [
          Matrix.Focus_out;
          Matrix.Resize (100, 30);
          Matrix.Focus_in;
          Matrix.Focus_in;
        ]
      ()
  in
  Matrix.suspend app;
  Buffer.clear state.output;
  let seen = ref 0 and frames = ref 0 in
  Matrix.run app
    ~on_input:(fun app event ->
      ignore event;
      incr seen;
      equal ~msg:"suspended resize and focus emit no terminal operations" string
        "" (output state);
      is_false ~msg:"paste remains disabled while suspended"
        (Matrix.Terminal.bracketed_paste_enabled (Matrix.terminal app));
      equal ~msg:"resize is deferred until resume" (pair int int) (80, 24)
        (Matrix.full_size app);
      if !seen = 4 then Matrix.resume app)
    ~on_render:(fun app ->
      incr frames;
      equal ~msg:"deferred resize is applied before resumed frame"
        (pair int int) (100, 30) (Matrix.full_size app);
      Matrix.stop app);
  equal ~msg:"input callbacks can still arrange resume" int 4 !seen;
  equal ~msg:"resume renders once" int 1 !frames

let test_run_closes_on_exception () =
  let app, state = make_app () in
  let raised =
    try
      Matrix.run app ~on_render:(fun _ -> failwith "boom");
      false
    with
    | Failure _ -> true
    | _ -> false
  in
  is_true ~msg:"exception propagated" raised;
  equal ~msg:"cleanup called on exception" int 1 state.cleanup_calls

let test_native_event_close_skips_ready_input () =
  let app, state =
    make_app ~target_fps:None
      ~events:[ Matrix.Resize (100, 30) ]
      ~input_chunks:[ "unread" ] ()
  in
  Matrix.run app ~on_input:(fun app _ -> Matrix.close app) ~on_render:ignore;
  equal ~msg:"closing during native event delivery leaves ready input unread"
    (list string) [ "unread" ] state.pending_input_chunks;
  equal ~msg:"native event closure cleans up once" int 1 state.cleanup_calls

let test_end_of_input_finalizes_parser_and_closes_once () =
  let app, state =
    make_app ~input_chunks:[ "\x1b[200~partial" ] ~end_after_reads:1 ()
  in
  let events = ref [] in
  Matrix.run app
    ~on_input:(fun _app event -> events := event :: !events)
    ~on_render:(fun _app -> ());
  equal ~msg:"input then EOF are read once each" int 2 state.read_calls;
  equal ~msg:"end-of-input closes backend once" int 1 state.cleanup_calls;
  equal ~msg:"end-of-input restores raw mode once" int 1 state.raw_restore_calls;
  is_false ~msg:"application is no longer running" (Matrix.running app);
  match List.rev !events with
  | [ Matrix.Paste_start; Paste_data "partial"; Paste_end `Cut ] -> ()
  | events -> failf "expected a cut paste, got %d events" (List.length events)

(* Regression: a stray CPR arriving through the normal event loop (xterm's
   Shift+F3, leftover shell-integration reports) must not flip the width
   capabilities for the rest of the session. *)
let test_stray_cpr_does_not_flip_capabilities () =
  let app, _state =
    make_app ~input_chunks:[ "\x1b[1;2R"; "\x1b[1;3R" ] ~stop_after_reads:3 ()
  in
  Matrix.run app ~on_render:(fun _app -> ());
  let caps = Matrix.capabilities app in
  is_false ~msg:"stray CPR does not enable explicit width" caps.explicit_width;
  is_false ~msg:"stray CPR does not enable scaled text" caps.scaled_text

let test_startup_and_late_cpr_do_not_enable_width () =
  let app, state =
    make_app ~mode:`Primary ~startup_cursor:(1, 3)
      ~startup_input:[ "\027[1;1R\027[1;1R\027[?62;4c" ]
      ~input_chunks:[ "\027[1;2R"; "\027[1;3R" ]
      ~stop_after_reads:3 ()
  in
  let check_width phase =
    let caps = Matrix.capabilities app in
    equal ~msg:phase (pair bool bool) (false, false)
      (caps.explicit_width, caps.scaled_text)
  in
  check_width "startup cursor report is not width evidence";
  Matrix.run app ~on_render:(fun _app -> ());
  check_width "late reports cannot reopen completed probes";
  equal ~msg:"late input was read" int 3 state.read_calls

let test_ghostty_profile_reaches_runtime () =
  let env =
    env
      [
        ("TERM", "xterm-ghostty");
        ("TERM_PROGRAM", "ghostty");
        ("TERM_PROGRAM_VERSION", "1.3.1");
      ]
  in
  let app, state =
    make_app ~env ~terminal_tty:true
      ~startup_input:[ "\027[?2027;2$y\027[1;1R\027[1;1R\027[?62;4c" ]
      ()
  in
  is_true ~msg:"probe preserves the wide profile"
    ((Matrix.capabilities app).unicode_width = `Unicode_wide);
  let grid = Matrix.Screen.next_grid (Matrix.screen app) in
  is_true ~msg:"screen uses the detected profile"
    (Matrix.Grid.width_method grid = `Unicode_wide);
  Matrix.Terminal.set_unicode_width (Matrix.terminal app) `Wcwidth;
  Matrix.Terminal.set_unicode_width (Matrix.terminal app) `Unicode_wide;
  contains ~sub:"\027[?2027h" (Buffer.contents state.output);
  Matrix.close app

let input_event = Testable.make ~pp:Matrix.pp_event ~equal:( = )

let input_width =
  Testable.make ~equal:( = ) ~pp:(fun ppf -> function
    | `Unicode -> Format.pp_print_string ppf "Unicode"
    | `Unicode_wide -> Format.pp_print_string ppf "Unicode_wide"
    | `Wcwidth -> Format.pp_print_string ppf "Wcwidth")

let test_escape_expires_without_more_input () =
  let app, state =
    make_app ~target_fps:None ~input_timeout:None ~advance_now:false
      ~sleep_until_timeout:true ~input_chunks:[ "\027" ] ~stop_after_reads:10 ()
  in
  let events = ref [] in
  Matrix.run app
    ~on_input:(fun app event ->
      events := event :: !events;
      Matrix.stop app)
    ~on_render:(fun _app -> ());
  equal ~msg:"timeout delivers the buffered Escape" (list input_event)
    [ key Escape ]
    (List.rev !events);
  is_true ~msg:"Escape required a wait after the input read"
    (state.read_calls > 1)

let test_startup_preserves_input_and_partial_paste () =
  let app, state =
    make_app ~target_fps:None ~startup_input:[ "a\027[200~par" ]
      ~input_chunks:[ "tial\027[201~b" ] ~end_after_reads:1 ()
  in
  satisfies ~msg:"startup probe respects the injected clock"
    ~claim:"at most one second of virtual time" float_exact
    (fun now -> now <= 1.)
    state.now_s;
  let events = ref [] in
  Matrix.run app
    ~on_input:(fun _app event -> events := event :: !events)
    ~on_render:(fun _app -> ());
  equal ~msg:"startup and runtime bytes share one parser in input order"
    (list input_event)
    [
      char 'a';
      Paste_start;
      Paste_data "par";
      Paste_data "tial";
      Paste_end `Complete;
      char 'b';
    ]
    (List.rev !events);
  equal ~msg:"EOF closes the backend after replaying startup input" int 1
    state.cleanup_calls

let test_startup_eof_finalizes_input_without_reading_again () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None
      ~startup_input:[ "a\027[200~par"; "" ] ()
  in
  let events = ref [] in
  Matrix.run app
    ~on_input:(fun _app event -> events := event :: !events)
    ~on_render:(fun _app -> ());
  equal ~msg:"startup EOF preserves completed input and finalizes partial paste"
    (list input_event)
    [ char 'a'; Paste_start; Paste_data "par"; Paste_end `Cut ]
    (List.rev !events);
  equal ~msg:"startup EOF prevents further input waits" int 0 state.read_calls;
  equal ~msg:"startup EOF closes the backend once" int 1 state.cleanup_calls

let inert_backend () : Matrix.Backend.t =
  {
    now = (fun () -> 0.);
    wake = (fun () -> ());
    write_output = (fun _ _ _ -> ());
    read_input = (fun _ _ _ -> 0);
    wait = (fun ~timeout:_ ~on_event:_ -> false);
    input_is_tty = false;
    output_is_tty = true;
    terminal_size = (fun () -> (80, 24));
    set_raw_mode = (fun _ -> ());
    flush_input = (fun () -> ());
    cleanup = (fun () -> ());
    protect = (fun f -> f ());
    env = no_env;
  }

let test_close_restores_raw_mode_if_terminal_close_raises () =
  let fail_output = ref false in
  let cleanup_calls = ref 0 in
  let raw_restore_calls = ref 0 in
  let backend =
    {
      (inert_backend ()) with
      write_output =
        (fun _ _ _ -> if !fail_output then failwith "closed output");
      set_raw_mode = (fun enabled -> if not enabled then incr raw_restore_calls);
      cleanup = (fun () -> incr cleanup_calls);
    }
  in
  let app =
    Matrix.attach ~mode:`Primary ~raw_mode:true ~mouse_enabled:false
      ~bracketed_paste:false ~kitty_keyboard:`Off ~target_fps:None
      ~input_timeout:(Some 0.) ~backend ()
  in
  fail_output := true;
  Matrix.close app;
  equal ~msg:"raw mode restored" int 1 !raw_restore_calls;
  equal ~msg:"cleanup called" int 1 !cleanup_calls

let test_close_restores_owned_signal_dispositions_once () =
  with_distinct_signal_handlers @@ fun entries ->
  let read_fd, write_fd = Unix.pipe ~cloexec:true () in
  Fun.protect
    ~finally:(fun () ->
      Unix.close read_fd;
      Unix.close write_fd)
    (fun () ->
      let app =
        Matrix.create ~raw_mode:false ~target_fps:None ~mouse_enabled:false
          ~bracketed_paste:false ~kitty_keyboard:`Off ~output:(`Fd write_fd) ()
      in
      (match
         List.find_opt
           (fun (signal, _, _) -> signal.number = Sys.sighup)
           entries
       with
      | Some (signal, _, prior_handler) -> (
          match peek_signal signal.number with
          | Sys.Signal_handle current ->
              is_false ~msg:"live app owns SIGHUP" (current == prior_handler)
          | _ -> ())
      | None -> ());
      Matrix.close app;
      assert_distinct_handlers entries;
      Matrix.close app;
      assert_distinct_handlers entries)

let test_attach_failure_preserves_signal_dispositions () =
  with_distinct_signal_handlers @@ fun entries ->
  let backend =
    {
      (inert_backend ()) with
      write_output = (fun _ _ _ -> raise_injected_startup_failure ());
    }
  in
  let failure =
    try
      ignore
        (Matrix.attach ~mode:`Alt ~raw_mode:true ~target_fps:None ~backend ());
      None
    with exn -> Some exn
  in
  (match failure with
  | Some Injected_startup_failure -> ()
  | Some exn ->
      failf "unexpected startup exception: %s" (Printexc.to_string exn)
  | None -> fail "startup failure did not escape Matrix.attach");
  assert_distinct_handlers entries

let test_nested_apps_restore_signal_dispositions_in_reverse () =
  with_distinct_signal_handlers @@ fun entries ->
  let read_fd, write_fd = Unix.pipe ~cloexec:true () in
  Fun.protect
    ~finally:(fun () ->
      Unix.close read_fd;
      Unix.close write_fd)
    (fun () ->
      let create () =
        Matrix.create ~raw_mode:false ~target_fps:None ~mouse_enabled:false
          ~bracketed_paste:false ~kitty_keyboard:`Off ~output:(`Fd write_fd) ()
      in
      let outer = create () in
      let outer_winch = peek_signal Sys.sigwinch in
      let inner = create () in
      Matrix.close inner;
      (match (outer_winch, peek_signal Sys.sigwinch) with
      | Sys.Signal_handle expected, Sys.Signal_handle actual ->
          is_true ~msg:"inner close restores outer SIGWINCH" (actual == expected)
      | _ -> fail "nested Matrix apps did not own SIGWINCH handlers");
      Matrix.close outer;
      assert_distinct_handlers entries)

let test_attach_rolls_back_partially_applied_terminal_modes () =
  let writes = ref 0 in
  let terminal_output = Buffer.create 256 in
  let lifecycle_trace = Buffer.create 256 in
  let raw_changes = ref [] in
  let cleanup_calls = ref 0 in
  let recording_backtraces = Printexc.backtrace_status () in
  Printexc.record_backtrace true;
  let failure =
    Fun.protect
      ~finally:(fun () -> Printexc.record_backtrace recording_backtraces)
      (fun () ->
        try
          ignore
            (Matrix.attach ~mode:`Alt ~raw_mode:true ~target_fps:None
               ~backend:
                 {
                   (inert_backend ()) with
                   write_output =
                     (fun buf off len ->
                       incr writes;
                       Buffer.add_subbytes lifecycle_trace buf off len;
                       if !writes = 2 then raise_injected_startup_failure ();
                       Buffer.add_subbytes terminal_output buf off len);
                   set_raw_mode =
                     (fun enabled ->
                       Buffer.add_string lifecycle_trace
                         (if enabled then "<raw:on>" else "<raw:off>");
                       raw_changes := enabled :: !raw_changes);
                   cleanup =
                     (fun () ->
                       Buffer.add_string lifecycle_trace "<cleanup>";
                       incr cleanup_calls);
                 }
               ());
          None
        with exn -> Some (exn, Printexc.get_raw_backtrace ()))
  in
  (match failure with
  | Some (Injected_startup_failure, backtrace) ->
      contains ~msg:"rollback preserves the startup failure backtrace"
        ~sub:"raise_injected_startup_failure"
        (Printexc.raw_backtrace_to_string backtrace)
  | Some (exn, _) ->
      failf "unexpected startup exception: %s" (Printexc.to_string exn)
  | None -> fail "startup failure did not escape Matrix.attach");
  equal ~msg:"raw mode acquired then restored" (list bool) [ true; false ]
    (List.rev !raw_changes);
  equal ~msg:"backend cleanup runs once" int 1 !cleanup_calls;
  contains ~msg:"partially entered alternate screen is left during rollback"
    ~sub:"\027[?1049l"
    (Buffer.contents terminal_output);
  let lifecycle_trace = Buffer.contents lifecycle_trace in
  in_order ~msg:"raw mode is acquired before terminal protocols"
    ~subs:[ "<raw:on>"; "\027[?1049h" ]
    lifecycle_trace;
  in_order ~msg:"terminal protocols roll back before raw mode"
    ~subs:[ "\027[?1049l"; "<raw:off>" ]
    lifecycle_trace;
  in_order ~msg:"raw mode rolls back before backend cleanup"
    ~subs:[ "<raw:off>"; "<cleanup>" ]
    lifecycle_trace

let test_focus_restore_runs_only_after_blur_once () =
  let app, state =
    make_app ~terminal_tty:true ~target_fps:None ~input_timeout:(Some 0.)
      ~events:[ Matrix.Focus_in ] ()
  in
  let first_focus_output = ref "" in
  Matrix.run app
    ~on_input:(fun app _event ->
      first_focus_output := output state;
      Matrix.stop app)
    ~on_render:(fun _app -> ());
  (* Frames and terminal modes share one output stream. Restoring modes emits
     the enabled paste protocol; ordinary frame output cannot contain it. *)
  not_contains ~msg:"focus without prior blur does not restore terminal modes"
    ~sub:"\027[?2004h" !first_focus_output;

  let app, state =
    make_app ~terminal_tty:true ~target_fps:None ~input_timeout:(Some 0.)
      ~events:[ Matrix.Focus_out; Matrix.Focus_in; Matrix.Focus_in ]
      ()
  in
  let seen = ref [] in
  Matrix.run app
    ~on_input:(fun app event ->
      seen := (event, output state) :: !seen;
      match List.length !seen with 3 -> Matrix.stop app | _ -> ())
    ~on_render:(fun _app -> ());
  match List.rev !seen with
  | [
   (Matrix.Focus_out, after_blur);
   (Matrix.Focus_in, after_first_focus);
   (Matrix.Focus_in, after_second_focus);
  ] ->
      not_contains ~msg:"blur itself does not restore terminal modes"
        ~sub:"\027[?2004h" after_blur;
      contains ~msg:"first focus after blur restores terminal modes"
        ~sub:"\027[?2004h" after_first_focus;
      equal ~msg:"second focus does not restore modes again" string
        after_first_focus after_second_focus
  | _ -> fail "expected blur, focus, focus input trace"

let test_late_capability_response_requests_redraw () =
  let app, _state =
    make_app ~terminal_tty:true ~target_fps:None ~input_timeout:(Some 0.)
      ~input_chunks:[ "\027[?2026;1$y" ] ~stop_after_reads:4 ()
  in
  let terminal = Matrix.terminal app in
  let caps = Matrix.Terminal.capabilities terminal in
  Matrix.Terminal.set_capabilities terminal { caps with sync = false };
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      if !frames >= 2 then Matrix.stop app);
  is_true ~msg:"late sync capability is folded"
    (Matrix.Terminal.capabilities (Matrix.terminal app)).sync;
  equal ~msg:"capability change requests a repaint" int 2 !frames

let test_startup_capability_window_defers_split_response () =
  let app, _state =
    make_app ~terminal_tty:true ~target_fps:None ~input_timeout:(Some 0.)
      ~read_quantum_s:0.2
      ~input_chunks:[ "\027[?2026;"; ""; "1$y" ]
      ~stop_after_reads:6 ()
  in
  let terminal = Matrix.terminal app in
  let caps = Matrix.Terminal.capabilities terminal in
  Matrix.Terminal.set_capabilities terminal { caps with sync = false };
  Matrix.run app ~on_render:(fun app ->
      if (Matrix.Terminal.capabilities (Matrix.terminal app)).sync then
        Matrix.stop app);
  is_true ~msg:"split late capability response is preserved"
    (Matrix.Terminal.capabilities (Matrix.terminal app)).sync

let test_resume_reanchors_primary_from_bottom_cursor () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~target_fps:None
      ~input_timeout:(Some 0.) ~startup_cursor:(1, 1)
      ~query_cursor_position:(fun ~timeout:_ -> Some (24, 3))
      ()
  in
  Matrix.suspend app;
  Matrix.resume app;
  equal ~msg:"a prompt on the bottom row leaves a one-row live viewport"
    (pair int int) (80, 1) (Matrix.size app);
  Buffer.clear state.output;
  Matrix.submit app;
  contains ~msg:"the next frame scrolls the prompt up first"
    ~sub:"\027[24;1H\r\n" (output state)

let test_resume_preserves_resize_during_cursor_query () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~read_quantum_s:0.1
      ~stop_after_reads:20
      ~query_cursor_position:(fun ~timeout:_ -> None)
      ()
  in
  Matrix.suspend app;
  state.pending_events <- [ Matrix.Resize (100, 30) ];
  Matrix.resume app;
  let resized = ref false in
  Matrix.run app ~on_render:ignore ~on_input:(fun app -> function
    | Matrix.Resize (100, 30) ->
        resized := true;
        Matrix.stop app
    | _ -> ());
  is_true ~msg:"resize received during cursor query reaches application"
    !resized;
  equal ~msg:"resize received during cursor query updates dimensions"
    (pair int int) (100, 30) (Matrix.full_size app)

let test_target_fps_respected_without_input_storm () =
  let app, state =
    make_app ~target_fps:(Some 13.) ~input_timeout:None ~advance_now:false
      ~sleep_until_timeout:true ()
  in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      if state.now_s >= 1. then Matrix.stop app);
  is_true ~msg:"13fps target should stay near cadence without input"
    (!frames >= 8 && !frames <= 20)

let test_target_fps_respected_with_input_storm () =
  let app, state =
    make_app ~target_fps:(Some 13.) ~input_timeout:None ~advance_now:false
      ~sleep_until_timeout:false ~emit_event_each_read:(Some Matrix.Focus_in) ()
  in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      if state.now_s >= 1. then Matrix.stop app);
  is_true ~msg:"input events must not bypass active fps cap during input storms"
    (!frames >= 8 && !frames <= 20)

let test_unchanged_submit_emits_no_bytes () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
  Matrix.submit app;
  equal ~msg:"unchanged frame skips terminal write" int 0
    (String.length (output state))

(* Regression: the Alt-mode preamble used to append a cursor-home after
   [preamble_len] was captured, so the skip check never fired and an idle
   Alt app wrote sync/cursor traffic every frame forever. *)
let test_unchanged_alt_submit_emits_no_bytes () =
  let app, state =
    make_app ~mode:`Alt ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
  Matrix.submit app;
  satisfies ~claim:"first alt frame writes (non-empty output)" string
    (fun s -> String.length s > 0)
    (output state);
  Buffer.clear state.output;
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
  Matrix.submit app;
  equal ~msg:"unchanged alt frame skips terminal write" int 0
    (String.length (output state))

(* Regression: a second suspend used to overwrite [previous_control_state]
   with the suspended state, so resume could never leave it — redraws stayed
   disabled forever while the terminal was left raw. *)
let test_suspend_is_idempotent () =
  let app, _state = make_app ~target_fps:None ~input_timeout:(Some 0.) () in
  Matrix.suspend app;
  Matrix.suspend app;
  Matrix.resume app;
  Matrix.request_redraw app;
  is_true ~msg:"double suspend then resume leaves the app resumable"
    (Matrix.redraw_requested app)

(* Bytes held across a suspend are dropped, and a paste the application has
   begun to receive is cut. *)
let test_suspend_discards_partial_input () =
  List.iter
    (fun (partial, expected) ->
      let app, state =
        make_app ~target_fps:None ~input_chunks:[ partial ]
          ~emit_event_each_read:(Some Matrix.Focus_in) ~stop_after_reads:5 ()
      in
      let received = ref [] in
      Matrix.run app
        ~on_render:(fun app ->
          if state.read_calls = 1 then (
            Matrix.suspend app;
            Matrix.resume app;
            state.pending_input_chunks <- [ "q" ]))
        ~on_input:(fun app event ->
          match event with
          | Matrix.Focus_in -> ()
          | Key _ ->
              received := event :: !received;
              Matrix.stop app
          | _ -> received := event :: !received);
      equal
        ~msg:(Printf.sprintf "discard partial %S across suspend" partial)
        (list input_event) expected (List.rev !received))
    [
      ("\027", [ char 'q' ]);
      ("\226", [ char 'q' ]);
      ( "\027[200~unfinished",
        [ Paste_start; Paste_data "unfinished"; Paste_end `Cut; char 'q' ] );
    ]

let test_cursor_visibility_preserves_blinking () =
  let app, state = make_app ~terminal_tty:true ~target_fps:None () in
  Matrix.set_cursor_style app ~style:`Line ~blinking:true;
  Matrix.set_cursor ~visible:true app;
  Matrix.set_cursor_position app ~row:1 ~col:1;
  Matrix.prepare app;
  Matrix.submit app;
  contains ~msg:"visibility preserves the requested blinking style"
    ~sub:"\027[5 q" (output state);
  Matrix.close app

(* Regression: close must not clobber terminal appearance the app never
   touched (window title, cursor colour, cursor style). *)
let test_close_leaves_untouched_appearance_alone () =
  let app, state =
    make_app ~terminal_tty:true ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.close app;
  let out = Buffer.contents state.output in
  not_contains ~msg:"close does not clear the title" ~sub:"\027]0;" out;
  not_contains ~msg:"close does not reset cursor color" ~sub:"\027]112" out;
  not_contains ~msg:"close does not reset cursor style" ~sub:"\027[0 q" out

let test_close_restores_touched_cursor_appearance () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"x";
  Matrix.set_cursor ~visible:true app;
  Matrix.set_cursor_style app ~style:`Line ~blinking:false;
  Matrix.set_cursor_color app ~r:1. ~g:0. ~b:0.;
  Matrix.set_cursor_position app ~row:1 ~col:1;
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.close app;
  let out = Buffer.contents state.output in
  contains ~msg:"close resets the emitted cursor style" ~sub:"\027[0 q" out;
  contains ~msg:"close resets the emitted cursor color" ~sub:"\027]112" out

let major_words_allocated f =
  Gc.full_major ();
  let before = Gc.quick_stat () in
  f ();
  let after = Gc.quick_stat () in
  after.major_words -. before.major_words

let test_submit_reuses_render_capacity () =
  let app, _state =
    make_app ~target_fps:None ~input_timeout:(Some 0.) ~raw_mode:false ()
  in
  Matrix.prepare app;
  Matrix.submit app;
  Matrix.prepare app;
  Matrix.submit app;
  let unchanged_major_words =
    major_words_allocated (fun () ->
        Matrix.prepare app;
        Matrix.submit app)
  in
  let changed_major_words =
    major_words_allocated (fun () ->
        Matrix.prepare app;
        Matrix.Grid.set_uchar (Matrix.grid app) ~x:0 ~y:0
          ~uchar:(Uchar.of_char 'X') ~fg:Matrix.Ansi.Color.white
          ~bg:Matrix.Ansi.Color.black ~attrs:Matrix.Ansi.Attr.empty ();
        Matrix.submit app)
  in
  is_true
    ~msg:
      (Printf.sprintf
         "unchanged submit allocated %.0f major words after warm-up"
         unchanged_major_words)
    (unchanged_major_words <= 1024.);
  is_true
    ~msg:
      (Printf.sprintf "changed submit allocated %.0f major words after warm-up"
         changed_major_words)
    (changed_major_words <= 1024.)

let test_cursor_only_submit_emits_output () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
  Matrix.set_cursor_position app ~row:1 ~col:3;
  Matrix.submit app;
  let output = output state in
  satisfies ~claim:"cursor-only frame emits terminal output (non-empty)" string
    (fun s -> String.length s > 0)
    output;
  contains ~msg:"cursor-only frame moves the cursor" ~sub:"\027[1;3H" output

let test_first_submit_emits_cursor_style () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
  Matrix.set_cursor ~visible:true app;
  Matrix.set_cursor_style app ~style:`Block ~blinking:true;
  Matrix.set_cursor_position app ~row:1 ~col:1;
  Matrix.submit app;
  (* The terminal's cursor style at startup is unknown: the first frame must
     emit DECSCUSR even when the requested shape matches our own default. *)
  contains ~msg:"first frame emits the cursor style" ~sub:"\027[1 q"
    (output state)

let test_visible_cursor_reemits_hidden_visual_state () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  let draw ~visible =
    Matrix.prepare app;
    Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
    Matrix.set_cursor ~visible app;
    Matrix.set_cursor_style app ~style:`Block ~blinking:true;
    Matrix.set_cursor_color app ~r:1. ~g:0. ~b:0.;
    Matrix.set_cursor_position app ~row:1 ~col:1;
    Matrix.submit app
  in
  draw ~visible:false;
  let hidden_output = output state in
  not_contains ~msg:"hidden frame does not emit cursor style" ~sub:"\027[1 q"
    hidden_output;
  not_contains ~msg:"hidden frame does not emit cursor color"
    ~sub:"\027]12;#ff0000\007" hidden_output;
  Buffer.clear state.output;
  draw ~visible:true;
  let visible_output = output state in
  contains ~msg:"visible frame emits cursor style" ~sub:"\027[1 q"
    visible_output;
  contains ~msg:"visible frame emits cursor color" ~sub:"\027]12;#ff0000\007"
    visible_output

let test_submit_emits_frame_above_default_capacity () =
  let width = 96 in
  let height = 96 in
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ~width
      ~height ~hyperlinks:true ()
  in
  let payload = String.make 240 'x' in
  let draw () =
    let grid = Matrix.grid app in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let style =
          Matrix.Ansi.Style.hyperlink
            (Printf.sprintf "https://example.com/%d/%s"
               ((y * width) + x)
               payload)
            Matrix.Ansi.Style.default
        in
        Matrix.Grid.draw_text grid ~x ~y ~text:"X" ~style
      done
    done
  in
  Matrix.prepare app;
  draw ();
  Matrix.submit app;
  satisfies ~claim:"complete frame above the default 2 MiB buffer size" int
    (fun len -> len > 2 * 1024 * 1024)
    (String.length (output state));
  Buffer.clear state.output;
  Matrix.prepare app;
  draw ();
  let retained_major_words =
    major_words_allocated (fun () -> Matrix.submit app)
  in
  equal ~msg:"successful retry commits the frame atomically" int 0
    (String.length (output state));
  is_true
    ~msg:
      (Printf.sprintf
         "submit allocated %.0f major words below its previous high-water mark"
         retained_major_words)
    (retained_major_words <= 1024.)

let test_initial_resize_fires_before_first_render () =
  let app, _state =
    make_app ~target_fps:None ~input_timeout:(Some 0.) ~stop_after_reads:5000 ()
  in
  let events = ref [] in
  Matrix.run app
    ~on_resize:(fun _app ~cols ~rows ->
      events := ("resize", cols, rows) :: !events)
    ~on_render:(fun app ->
      events := ("render", 0, 0) :: !events;
      Matrix.stop app);
  match List.rev !events with
  | [ ("resize", 80, 24); ("render", 0, 0) ] -> ()
  | got ->
      failf "expected resize(80,24) before first render, got: %d event(s)"
        (List.length got)

let test_resize_zero_dimensions_are_ignored () =
  let app, _state =
    make_app ~resize_debounce:None ~target_fps:None ~input_timeout:(Some 0.)
      ~events:[ Matrix.Resize (0, 0) ]
      ~stop_after_reads:1 ()
  in
  let events = ref [] in
  Matrix.run app
    ~on_resize:(fun app ~cols ~rows ->
      events := (cols, rows, Matrix.full_size app, Matrix.size app) :: !events)
    ~on_render:(fun _app -> ());
  (* The degenerate report is dropped: the applied size never changes, so only
     the initial callback fires. *)
  match List.rev !events with
  | [ (80, 24, (80, 24), (80, 24)) ] -> ()
  | got -> failf "unexpected resize trace: %d event(s)" (List.length got)

(* Regression: the initial on_resize reported the live-viewport size while the
   event path forwarded raw terminal dimensions — a Primary-mode subscriber
   saw two different row semantics. Both now report {!Matrix.size}. *)
let test_on_resize_reports_usable_size () =
  let app, _state =
    make_app ~mode:`Primary ~startup_cursor:(15, 1) ~resize_debounce:None
      ~target_fps:None ~input_timeout:(Some 0.)
      ~events:[ Matrix.Resize (100, 30) ]
      ~stop_after_reads:1 ()
  in
  let events = ref [] in
  Matrix.run app
    ~on_resize:(fun app ~cols ~rows ->
      events := ((cols, rows), Matrix.size app) :: !events)
    ~on_render:(fun _app -> ());
  match List.rev !events with
  | [ (initial, initial_size); (resized, resized_size) ] ->
      equal ~msg:"initial callback reports the usable size" (pair int int)
        initial_size initial;
      equal ~msg:"resize callback reports the usable size" (pair int int)
        resized_size resized;
      is_true ~msg:"primary viewport differs from raw terminal rows"
        (snd resized <> 30)
  | got -> failf "expected two resize callbacks, got %d" (List.length got)

let test_resize_clamps_primary_render_offset () =
  let app, _state =
    make_app ~mode:`Primary ~startup_cursor:(21, 1) ~resize_debounce:None
      ~target_fps:None ~input_timeout:(Some 0.)
      ~events:[ Matrix.Resize (80, 12) ]
      ~stop_after_reads:1 ()
  in
  Matrix.run app ~on_render:(fun _app -> ());
  equal ~msg:"terminal size follows resize event" (pair int int) (80, 12)
    (Matrix.full_size app);
  equal ~msg:"primary offset clamps to preserve minimum live height"
    (pair int int) (80, 1) (Matrix.size app)

let test_resize_debounce_applies_latest_pending_resize () =
  let app, _state =
    make_app ~resize_debounce:(Some 0.5) ~advance_now:false
      ~sleep_until_timeout:true ~target_fps:None ~input_timeout:None
      ~events:[ Matrix.Resize (100, 30); Matrix.Resize (120, 40) ]
      ()
  in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      if Matrix.full_size app = (120, 40) || !frames > 5 then Matrix.stop app);
  equal ~msg:"debounced resize applies latest pending dimensions" (pair int int)
    (120, 40) (Matrix.full_size app)

let test_same_size_debounced_resize_settles () =
  (* A settling resize whose dimensions equal the size already applied is a
     no-op to draw, but it must still be consumed from [pending_resize].
     Otherwise the pending entry lingers with an elapsed debounce window, which
     pins the loop's requested timeout at zero and spins it — starving input —
     until a differently-sized resize clears the entry. *)
  let app, state =
    make_app ~resize_debounce:(Some 0.1) ~advance_now:false
      ~sleep_until_timeout:true ~target_fps:None ~input_timeout:None
      ~events:[ Matrix.Resize (100, 30); Matrix.Resize (100, 30) ]
      ~stop_after_reads:40 ()
  in
  Matrix.run app ~on_render:(fun _ -> ());
  let classify = function
    | None -> "idle"
    | Some t when t <= 0. -> "spin"
    | Some _ -> "wait"
  in
  let recent =
    List.filteri (fun i _ -> i < 5) state.read_timeouts |> List.map classify
  in
  equal
    ~msg:
      "a resize debounced to the applied size settles the loop into an idle \
       wait rather than a zero-timeout spin"
    (list string)
    [ "idle"; "idle"; "idle"; "idle"; "idle" ]
    recent

let test_primary_required_rows_expands_primary_region () =
  let app, _state =
    make_app ~mode:`Primary ~startup_cursor:(23, 1) ~target_fps:(Some 30.)
      ~input_timeout:(Some 0.) ~stop_after_reads:5000 ()
  in
  let frames = ref 0 in
  Matrix.run app
    ~primary_required_rows:(fun _ -> Some 8)
    ~on_render:(fun app ->
      incr frames;
      if !frames >= 3 then Matrix.stop app);
  let _w, h = Matrix.size app in
  equal ~msg:"primary required rows hint grows dynamic viewport" int 8 h

let test_primary_required_rows_ignored_in_alt_mode () =
  let app, _state =
    make_app ~mode:`Alt ~target_fps:(Some 30.) ~input_timeout:(Some 0.)
      ~stop_after_reads:5000 ()
  in
  Matrix.run app
    ~primary_required_rows:(fun _ -> Some 3)
    ~on_render:(fun app -> Matrix.stop app);
  let _w, h = Matrix.size app in
  equal ~msg:"alt mode size remains full terminal height" int 24 h

let static_grid text =
  let lines = String.split_on_char '\n' text in
  let lines =
    match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
  in
  let width =
    List.fold_left (fun width line -> max width (String.length line)) 1 lines
  in
  let grid = Matrix.Grid.create ~width ~height:(max 1 (List.length lines)) () in
  List.iteri (fun y text -> Matrix.Grid.draw_text grid ~x:0 ~y ~text) lines;
  grid

let test_partial_frame_restores_transient_modes () =
  List.iter
    (fun (cut, fail_restore) ->
      List.iter
        (fun suspend ->
          let fail_output = ref false in
          let bytes = Buffer.create 128 in
          let cleaned = ref false in
          let protected = ref false in
          let restore_failed = ref false in
          let backend =
            {
              (inert_backend ()) with
              protect =
                (fun f ->
                  protected := true;
                  Fun.protect f ~finally:(fun () -> protected := false));
              write_output =
                (fun buf off len ->
                  let frame = Bytes.sub_string buf off len in
                  if !fail_output then (
                    contains ~msg:"frame reaches the injected failure point"
                      ~sub:cut frame;
                    let stop = ref (String.length cut) in
                    while
                      String.sub frame
                        (!stop - String.length cut)
                        (String.length cut)
                      <> cut
                    do
                      incr stop
                    done;
                    Buffer.add_substring bytes frame 0 !stop;
                    failwith "partial frame write")
                  else (
                    Buffer.add_string bytes frame;
                    if
                      frame = "\027[?2026l" || frame = "\027[r"
                      || frame = "\027[?7h"
                    then
                      is_true ~msg:"frame mode recovery is protected" !protected;
                    if fail_restore && frame = "\027[?2026l" then (
                      restore_failed := true;
                      failwith "failed synchronized output reset")));
              cleanup = (fun () -> cleaned := true);
            }
          in
          let app = Matrix.attach ~mode:`Primary ~backend () in
          set_sync_capable app;
          Matrix.static_write app
            (static_grid (String.concat "\n" (List.init 24 (fun _ -> "row"))));
          Matrix.prepare app;
          Buffer.clear bytes;
          fail_output := true;
          (match Matrix.submit app with
          | () -> failf "expected injected partial write failure"
          | exception Failure message ->
              equal string "partial frame write" message);
          fail_output := false;
          if suspend then Matrix.suspend app else Matrix.close app;
          in_order ~msg:"terminal handoff attempts every transient mode reset"
            ~subs:[ cut; "\027[?2026l"; "\027[r"; "\027[?7h" ]
            (Buffer.contents bytes);
          equal ~msg:"reset failure case is exercised" bool fail_restore
            !restore_failed;
          Matrix.close app;
          is_true ~msg:"backend cleanup still runs" !cleaned)
        [ false; true ])
    [ ("\027[?2026h", false); ("\027[?7l", false); ("row", true) ]

let test_primary_effective_size_tracks_pending_static () =
  let app, _state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  equal ~msg:"initial primary height" int 24 (snd (Matrix.size app));
  Matrix.static_write app (static_grid "alpha\nbeta\n");
  let effective = Matrix.effective_size app in
  equal ~msg:"pending static write reduces effective height" (pair int int)
    (80, 22) effective;
  Matrix.prepare app;
  equal ~msg:"the next frame applies pending effective size" (pair int int)
    effective (Matrix.size app)

let test_prepare_uses_pending_static_effective_region () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.static_write app (static_grid "alpha\nbeta\n");
  let _, effective_height = Matrix.effective_size app in
  Matrix.prepare app;
  let grid = Matrix.grid app in
  equal ~msg:"prepare sizes grid to pending effective height" int
    effective_height (Matrix.Grid.height grid);
  Matrix.Grid.draw_text grid ~x:0 ~y:(effective_height - 1) ~text:"live";
  Matrix.submit app;
  let out = output state in
  contains ~msg:"static output is emitted" ~sub:"alpha" out;
  in_order ~msg:"live output is rendered after static output"
    ~subs:[ "alpha"; "live" ] out

let test_static_writes_are_flushed_fifo () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.static_write app (static_grid "first\n");
  Matrix.static_write app (static_grid "second\n");
  Matrix.prepare app;
  Matrix.submit app;
  let output = output state in
  in_order ~msg:"first static write appears before second"
    ~subs:[ "first"; "second" ] output

let test_static_write_snapshots_selected_rows () =
  let app, state = make_app ~mode:`Primary ~target_fps:None () in
  let grid = static_grid "first\nsecond\nignored" in
  Matrix.static_write ~rows:2 app grid;
  Matrix.Grid.clear grid;
  Matrix.prepare app;
  Matrix.submit app;
  let output = output state in
  in_order ~msg:"snapshot preserves selected rows and CRLF separation"
    ~subs:[ "first"; "\r\n"; "second"; "\r\n" ]
    output;
  not_contains ~msg:"unselected rows are excluded" ~sub:"ignored" output

let test_static_clear_is_immediate_and_forces_full_repaint () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_clear app;
  equal ~msg:"static_clear sends clear-and-home immediately" string
    "\027[H\027[2J" (output state);
  Buffer.clear state.output;
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  contains ~msg:"next submit repaints unchanged content in full" ~sub:"live"
    (output state);
  not_contains ~msg:"the immediate clear is not queued again into the frame"
    ~sub:"\027[H\027[2J" (output state)

(* A screen full of shell output, the prompt on the bottom row: static rows
   push the transcript up into the scrollback, and the live region keeps the
   bottom row. *)
let full_shell = "$0\r\n$1\r\n$2\r\n$ app"

let test_pinned_static_write_scrolls_transcript () =
  let app, term = vte_app ~shell:full_shell ~cols:8 ~rows:4 () in
  present app [ "live" ];
  Matrix.static_write app (static_grid "pinned\n");
  present app [ "live" ];
  equal ~msg:"static row joins the transcript" (list string)
    [ "$0"; "$1"; "$2"; "$ app"; "pinned" ]
    (transcript app term);
  equal ~msg:"live region keeps the bottom row" (list string) [ "live" ]
    (live_rows app term)

let test_multiline_static_write_at_the_pin () =
  let app, term = vte_app ~shell:full_shell ~cols:8 ~rows:4 () in
  present app [ "live" ];
  Matrix.static_write app (static_grid "line-a\nline-b\n");
  present app [ "live" ];
  equal ~msg:"both rows join the transcript in order" (list string)
    [ "$0"; "$1"; "$2"; "$ app"; "line-a"; "line-b" ]
    (transcript app term);
  equal ~msg:"live region keeps its row" (pair int int) (8, 1) (Matrix.size app);
  equal ~msg:"live region keeps the bottom row" (list string) [ "live" ]
    (live_rows app term)

let test_static_write_moves_live_region_down () =
  let app, term = vte_app ~shell:"$ cmd\r\n" ~cols:8 ~rows:6 () in
  present app [ "live" ];
  Matrix.static_write app (static_grid "settle\n");
  present app [ "live" ];
  equal ~msg:"static row follows the shell's" (list string)
    [ "$ cmd"; "settle" ] (transcript app term);
  equal ~msg:"live region gives up the row" (pair int int) (8, 4)
    (Matrix.size app);
  equal ~msg:"live region follows the static row" (list string)
    [ "live"; ""; ""; "" ] (live_rows app term)

let test_static_write_and_live_repaint_share_sync_frame () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~startup_cursor:(24, 1)
      ~min_tui_height:1 ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  set_sync_capable app;
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app (static_grid "pinned\n");
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  let output = output state in
  in_order ~msg:"synchronized output begins before static payload"
    ~subs:[ "\027[?2026h"; "pinned" ]
    output;
  in_order ~msg:"static payload appears before live repaint"
    ~subs:[ "pinned"; "live" ] output;
  in_order ~msg:"live repaint is inside synchronized output"
    ~subs:[ "live"; "\027[?2026l" ] output

let test_pinned_static_write_keeps_live_size () =
  let app, _state =
    make_app ~mode:`Primary ~startup_cursor:(24, 1) ~min_tui_height:1
      ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  let before = Matrix.size app in
  Matrix.static_write app (static_grid "a\nb\nc\n");
  equal ~msg:"pinned pending output keeps effective live size" (pair int int)
    before
    (Matrix.effective_size app);
  Matrix.submit app;
  equal ~msg:"pinned output keeps live size after submit" (pair int int) before
    (Matrix.size app)

let test_primary_required_rows_apply_after_static_flush () =
  let app, state =
    make_app ~mode:`Primary ~startup_cursor:(11, 1) ~min_tui_height:1
      ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app (static_grid "prompt\n");
  Matrix.prepare ~primary_required_rows:20 app;
  equal ~msg:"the frame has the rows it requires" (pair int int) (80, 20)
    (Matrix.size app);
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:19 ~text:"live";
  Matrix.submit app;
  let output = output state in
  equal ~msg:"the static row does not take one of them" (pair int int) (80, 20)
    (Matrix.size app);
  in_order ~msg:"static output, then growth, then the frame"
    ~subs:[ "prompt"; "\027[24;1H\r\n"; "live" ]
    output

let test_full_height_static_write_scrolls_into_scrollback () =
  let app, term = vte_app ~min_tui_height:4 ~cols:12 ~rows:4 () in
  present app [ "live" ];
  Matrix.static_write app (static_grid "INSERTED00\nINSERTED01\n");
  equal ~msg:"pending write keeps the full-height live region" (pair int int)
    (12, 4)
    (Matrix.effective_size app);
  present app [ "live" ];
  equal ~msg:"static rows go straight to the scrollback" (list string)
    [ "INSERTED00"; "INSERTED01" ]
    (Vte.scrollback_lines term.vte);
  equal ~msg:"live region keeps the screen" (list string) [ "live"; ""; ""; "" ]
    (live_rows app term)

(* The frame that asks for more rows is drawn at the height it asked for. *)
let test_required_rows_show_in_their_frame () =
  let app, term =
    vte_app ~shell:"$ ls\r\n$ \r\n$ \r\n$ \r\n" ~cols:12 ~rows:6 ()
  in
  present app [ "one" ];
  equal ~msg:"a live region of two rows" (pair int int) (12, 2)
    (Matrix.size app);
  present ~required:4 app [ "one"; "two"; "three"; "four" ];
  equal ~msg:"grows by scrolling, and shows the rows of its frame" (list string)
    [ "one"; "two"; "three"; "four" ]
    (live_rows app term);
  equal ~msg:"the shell's rows go to the transcript" (list string)
    [ "$ ls"; "$"; "$"; "$" ] (transcript app term)

(* A program run while the application is suspended goes on from the
   transcript: the live region is erased and the cursor shown at its start,
   after the static rows the frame being drawn had already placed. *)
let test_suspend_hands_over_below_the_transcript () =
  let app, term = vte_app ~shell:"$ ls\r\n" ~cols:12 ~rows:6 () in
  present app [ "ui one"; "ui two" ];
  Matrix.static_write app (static_grid "done\n");
  Matrix.prepare app;
  Matrix.suspend app;
  equal ~msg:"the transcript, then nothing" (list string)
    [ "$ ls"; "done"; ""; ""; ""; "" ]
    (screen_rows term);
  equal ~msg:"the cursor waits below the transcript" (pair int int) (2, 0)
    (Vte.cursor_pos term.vte);
  is_true ~msg:"and shows" (Vte.modes term.vte).cursor_visible;
  ignore (Vte.feed_string term.vte "$ make\r\nok\r\n" : string);
  Matrix.resume app;
  present app [ "ui one"; "ui two" ];
  equal ~msg:"the program's output joins the transcript" (list string)
    [ "$ ls"; "done"; "$ make"; "ok" ]
    (transcript app term);
  equal ~msg:"the live region is below it" (list string) [ "ui one"; "ui two" ]
    (live_rows app term)

let test_static_snapshots_scroll_exactly_once_per_row () =
  List.iter
    (fun rows ->
      let app, state =
        make_app ~mode:`Primary ~width:6 ~height:3 ~min_tui_height:3
          ~target_fps:None ()
      in
      let lines = List.init rows (fun i -> Printf.sprintf "R%02d" i) in
      Matrix.static_write app (static_grid (String.concat "\n" lines));
      Matrix.prepare app;
      Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
      Matrix.submit app;
      let terminal = Vte.create ~rows:3 ~cols:6 ~scrollback:32 () in
      ignore (Vte.feed_string terminal (output state) : string);
      equal
        ~msg:(Printf.sprintf "%d snapshot rows reach history once" rows)
        (list string) lines
        (Vte.scrollback_lines terminal);
      contains ~msg:"live view remains visible after transcript insertion"
        ~sub:"live" (Vte.to_string terminal);
      Matrix.close app)
    [ 1; 3; 4; 8 ]

let test_static_snapshot_clears_suffixes_and_keeps_blank_rows () =
  let app, state =
    make_app ~mode:`Primary ~width:3 ~height:3 ~min_tui_height:3
      ~target_fps:None ()
  in
  Matrix.static_write app (static_grid "A\n\nB");
  Matrix.prepare app;
  Matrix.submit app;
  (* A terminal widened after enqueue has stale cells beyond the old width. *)
  let terminal = Vte.create ~rows:3 ~cols:6 ~scrollback:16 () in
  ignore (Vte.feed_string terminal "STALE!\r\nSTALE!\r\nSTALE!" : string);
  ignore (Vte.feed_string terminal (output state) : string);
  equal ~msg:"complete rows clear suffixes and preserve intentional blanks"
    (list string) [ "A"; ""; "B" ]
    (Vte.scrollback_lines terminal);
  Matrix.close app

let test_queued_static_snapshot_keeps_row_count_after_resize () =
  let app, state =
    make_app ~mode:`Primary ~width:6 ~height:3 ~min_tui_height:3
      ~target_fps:None
      ~events:[ Matrix.Focus_in; Matrix.Resize (3, 3) ]
      ~stop_after_reads:2 ()
  in
  Matrix.run app
    ~on_input:(fun app -> function
      | Matrix.Focus_in ->
          Buffer.clear state.output;
          Matrix.static_write app (static_grid "ABCDEF\nGHIJKL")
      | _ -> ())
    ~on_render:ignore;
  equal ~msg:"resize applies before the queued static frame" (pair int int)
    (3, 3) (Matrix.full_size app);
  let terminal = Vte.create ~rows:3 ~cols:3 ~scrollback:16 () in
  ignore (Vte.feed_string terminal "old\r\nold\r\nold" : string);
  ignore (Vte.feed_string terminal (output state) : string);
  let history = Vte.scrollback_lines terminal in
  equal ~msg:"narrowing a queued snapshot does not add transcript rows" int 2
    (List.length history);
  List.iter
    (fun row ->
      not_contains ~msg:"snapshot replaces stale row content" ~sub:"old" row)
    history

let test_primary_column_one_anchor_starts_at_cursor_row () =
  let app, term = vte_app ~shell:"$ cmd\r\n" ~cols:10 ~rows:4 () in
  Matrix.static_write app (static_grid "HEADER\n");
  present app [ "live" ];
  equal ~msg:"the static row follows the shell's" (list string)
    [ "$ cmd"; "HEADER" ] (transcript app term);
  equal ~msg:"the live region follows it" (list string) [ "live"; "" ]
    (live_rows app term)

let test_preserved_static_write_keeps_live_region () =
  let app, term = vte_app ~cols:12 ~rows:4 () in
  present app [ "live" ];
  Matrix.static_write ~preserve_live_region:true app
    (static_grid "PRESERVED00\nPRESERVED01\n");
  equal ~msg:"pending preserved write keeps effective live size" (pair int int)
    (12, 4)
    (Matrix.effective_size app);
  present app [ "live" ];
  equal ~msg:"preserved rows scroll into the scrollback" (list string)
    [ "PRESERVED00"; "PRESERVED01" ]
    (Vte.scrollback_lines term.vte);
  equal ~msg:"preserved write keeps live region" (pair int int) (12, 4)
    (Matrix.size app)

let test_preserved_static_write_after_transcript_rows () =
  let app, term =
    vte_app ~shell:"$0\r\n$1\r\n$2\r\n" ~min_tui_height:2 ~cols:8 ~rows:6 ()
  in
  present app [ "live" ];
  Matrix.static_write ~preserve_live_region:true app
    (static_grid "AAAA\nBBBB\n");
  equal ~msg:"pending preserved write keeps the live size" (pair int int) (8, 3)
    (Matrix.effective_size app);
  present app [ "live" ];
  equal ~msg:"rows follow the transcript, which scrolls up" (list string)
    [ "$0"; "$1"; "$2"; "AAAA"; "BBBB" ]
    (transcript app term);
  equal ~msg:"live region keeps its height" (pair int int) (8, 3)
    (Matrix.size app)

let test_mixed_static_writes () =
  let app, term = vte_app ~min_tui_height:5 ~cols:8 ~rows:6 () in
  present app [ "live" ];
  Matrix.static_write ~preserve_live_region:true app (static_grid "KEEP\n");
  Matrix.static_write app (static_grid "GROW\n");
  present app [ "live" ];
  equal ~msg:"both rows join the transcript in order" (list string)
    [ "KEEP"; "GROW" ] (transcript app term);
  equal ~msg:"only the second takes a live row" (pair int int) (8, 5)
    (Matrix.size app)

let test_full_height_consecutive_static_writes_scroll_once_per_row () =
  let app, term = vte_app ~min_tui_height:4 ~cols:8 ~rows:4 () in
  present app [ "live" ];
  Matrix.static_write app (static_grid "first");
  Matrix.static_write app (static_grid "second\n");
  present app [ "live" ];
  equal ~msg:"each row reaches the scrollback once, in order" (list string)
    [ "first"; "second" ]
    (Vte.scrollback_lines term.vte)

let test_static_write_ignored_in_alt_mode () =
  let app, state =
    make_app ~mode:`Alt ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.static_write app (static_grid "ignored\n");
  equal ~msg:"alt effective size unchanged" (pair int int) (80, 24)
    (Matrix.effective_size app);
  Matrix.submit app;
  not_contains ~msg:"alt mode does not emit static text" ~sub:"ignored"
    (output state)

let test_static_clear_resets_primary_layout () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~startup_cursor:(11, 1)
      ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  equal ~msg:"attached primary starts below transcript" (pair int int) (80, 14)
    (Matrix.size app);
  Matrix.static_write app (static_grid "pending\n");
  (* Pending output takes its row from the live region. *)
  equal ~msg:"pending output affects effective size" (pair int int) (80, 13)
    (Matrix.effective_size app);
  Buffer.clear state.output;
  Matrix.static_clear app;
  contains ~msg:"static clear is an immediate terminal reset"
    ~sub:"\027[H\027[2J"
    (Buffer.contents state.output);
  equal ~msg:"static clear restores full primary height" (pair int int) (80, 24)
    (Matrix.size app);
  equal ~msg:"effective size also reset" (pair int int) (80, 24)
    (Matrix.effective_size app);
  Buffer.clear state.output;
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"after-clear";
  Matrix.submit app;
  let output = output state in
  contains ~msg:"next live frame renders after static clear" ~sub:"after-clear"
    output;
  not_contains ~msg:"static clear discards pending static output" ~sub:"pending"
    output

let test_static_replace_replays_inside_next_frame () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~startup_cursor:(11, 1)
      ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  set_sync_capable app;
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_replace app (static_grid "replacement-a\nreplacement-b\n");
  equal ~msg:"static replacement is deferred until the next frame" string ""
    (Buffer.contents state.output);
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live-after";
  Matrix.submit app;
  let output = output state in
  contains ~msg:"replacement clears terminal scrollback" ~sub:"\027[3J" output;
  in_order ~msg:"replacement content precedes the live repaint"
    ~subs:[ "replacement-b"; "live-after" ]
    output;
  in_order ~msg:"replacement opens inside the synchronized update"
    ~subs:[ "\027[?2026h"; "replacement-a" ]
    output;
  in_order ~msg:"live repaint closes inside the synchronized update"
    ~subs:[ "live-after"; "\027[?2026l" ]
    output

let test_primary_full_redraw_does_not_erase_past_terminal_bottom () =
  let app, state =
    make_app ~mode:`Primary ~startup_cursor:(11, 1) ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  let _w, live_height = Matrix.size app in
  equal ~msg:"test setup uses 14 live rows" int 14 live_height;
  Matrix.prepare app;
  let grid = Matrix.grid app in
  Matrix.Grid.resize grid ~width:(Matrix.Grid.width grid) ~height:live_height;
  Matrix.Grid.draw_text grid ~x:0 ~y:(live_height - 1) ~text:"Elapsed: 5.5s";
  Matrix.submit app;
  let output = output state in
  contains ~msg:"bottom live row is rendered" ~sub:"Elapsed: 5.5s" output;
  not_contains
    ~msg:"full redraw must not erase from one row past terminal bottom"
    ~sub:"\027[25;1H\027[J" output

let test_primary_full_redraw_erases_stale_rows_below_content () =
  let app, state =
    make_app ~mode:`Primary ~startup_cursor:(11, 1) ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  let grid = Matrix.grid app in
  Matrix.Grid.resize grid ~width:(Matrix.Grid.width grid) ~height:13;
  Matrix.Grid.draw_text grid ~x:0 ~y:12 ~text:"Press Q to quit";
  Matrix.submit app;
  let output = output state in
  contains ~msg:"content row is rendered" ~sub:"Press Q to quit" output;
  contains ~msg:"full redraw erases rows below active content"
    ~sub:"\027[24;1H\027[J" output

let test_primary_required_rows_to_terminal_height_does_not_erase_past_bottom ()
    =
  let app, state =
    make_app ~mode:`Primary ~startup_cursor:(11, 1) ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare ~primary_required_rows:24 app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:23 ~text:"bottom";
  Matrix.submit app;
  let output = output state in
  equal ~msg:"primary required rows can claim the whole terminal" (pair int int)
    (80, 24) (Matrix.size app);
  contains ~msg:"bottom row is rendered after primary growth" ~sub:"bottom"
    output;
  not_contains
    ~msg:"primary growth must not erase from row past terminal bottom"
    ~sub:"\027[25;1H\027[J" output

(* Through a terminal *)

let test_alt_default_cursor_is_shown () =
  let app, term = vte_app ~mode:`Alt ~cols:6 ~rows:3 () in
  let screen = Matrix.screen app in
  Matrix.Screen.set_cursor screen
    { (Matrix.Screen.cursor screen) with position = None; visible = true };
  present app [ "x" ];
  is_true ~msg:"a visible cursor at its default position shows"
    (Vte.modes term.vte).cursor_visible;
  equal ~msg:"it waits on the bottom row" (pair int int) (2, 0)
    (Vte.cursor_pos term.vte)

let test_primary_full_redraw_keeps_cursor_row () =
  let app, term = vte_app ~cols:6 ~rows:4 () in
  let screen = Matrix.screen app in
  Matrix.Screen.set_cursor screen
    {
      (Matrix.Screen.cursor screen) with
      position = Some (2, 3);
      visible = true;
    };
  (* The first frame is a full redraw, which paints the one row that shows
     anything. *)
  present app [ "x" ];
  equal ~msg:"the cursor is on its row, below the painted one" (pair int int)
    (3, 2) (Vte.cursor_pos term.vte)

let test_primary_full_redraw_paints_background_rows () =
  let app, term = vte_app ~cols:6 ~rows:4 () in
  Matrix.prepare app;
  let grid = Matrix.grid app in
  Matrix.Grid.draw_text grid ~x:0 ~y:0 ~text:"x";
  Matrix.Grid.fill_rect grid ~x:0 ~y:1 ~width:6 ~height:1
    ~color:Matrix.Ansi.Color.blue;
  Matrix.submit app;
  let g = Vte.grid term.vte in
  is_true ~msg:"a row of background only is painted"
    (Matrix.Ansi.Color.equal
       (Matrix.Grid.get_bg g (Matrix.Grid.idx g ~x:5 ~y:1))
       Matrix.Ansi.Color.blue)

let test_primary_anchor_scrolls_shell_rows () =
  (* The minimum live height does not fit below the prompt: the shell's rows
     scroll into the scrollback rather than being overwritten. *)
  let app, term =
    vte_app ~shell:"$0\r\n$1" ~min_tui_height:3 ~cols:4 ~rows:4 ()
  in
  present app [ "live" ];
  equal ~msg:"the shell's rows stay in the transcript" (list string)
    [ "$0"; "$1" ] (transcript app term);
  equal ~msg:"the live region has its minimum height" (pair int int) (4, 3)
    (Matrix.size app)

let test_primary_one_row_static_area () =
  let app, term = vte_app ~cols:4 ~rows:2 () in
  present app [ "live" ];
  Matrix.static_write app (static_grid "S0\n");
  present app [ "live" ];
  equal ~msg:"the static row is above the live region" (list string) [ "S0" ]
    (transcript app term);
  equal ~msg:"the live region keeps its row" (list string) [ "live" ]
    (live_rows app term)

let test_primary_minimum_height_survives_shrink () =
  let app, term = vte_app ~min_tui_height:3 ~cols:8 ~rows:8 () in
  term.script <-
    [
      (fun ~on_event -> resize_step term ~cols:8 ~rows:2 ~on_event);
      (fun ~on_event -> resize_step term ~cols:8 ~rows:8 ~on_event);
      (fun ~on_event:_ ->
        Matrix.static_write app
          (static_grid
             (String.concat "\n" (List.init 8 (Printf.sprintf "S%d"))));
        Matrix.request_redraw app);
      (fun ~on_event:_ ->
        equal ~msg:"static rows leave the minimum to the live region"
          (pair int int) (8, 3) (Matrix.size app));
    ];
  Matrix.run app ~on_render:(fun app -> draw_rows app [ "live" ])

(* A shell's history fills the screen and the application's three rows sit
   at the bottom. A terminal that grows pulls history back from its
   scrollback, and one that shrinks drops the rows below its cursor and then
   moves the top ones into its scrollback: the live region moves with the
   rows. *)
let test_primary_resize_keeps_live_region_below_transcript () =
  let history = List.init 12 (Printf.sprintf "history %d") in
  let shell =
    String.concat "" (List.map (fun line -> line ^ "\r\n") history) ^ "$ app"
  in
  let app, term = vte_app ~shell ~cols:16 ~rows:8 () in
  let ui = [ "+------+"; "| UI   |"; "+------+" ] in
  let expect what =
    equal
      ~msg:(what ^ ": the transcript is whole")
      (list string) (history @ [ "$ app" ]) (transcript app term);
    equal
      ~msg:(what ^ ": the live region shows the frame")
      (list string) ui (live_rows app term)
  in
  let redraw ~on_event:_ = Matrix.request_redraw app in
  term.script <-
    [
      redraw;
      (fun ~on_event ->
        expect "before";
        resize_step term ~cols:16 ~rows:11 ~on_event);
      (fun ~on_event ->
        expect "grown";
        resize_step term ~cols:16 ~rows:6 ~on_event);
      redraw;
      (fun ~on_event:_ -> expect "shrunk");
    ];
  Matrix.run app
    ~primary_required_rows:(fun _ -> Some 3)
    ~on_render:(fun app -> draw_rows app ui)

let test_primary_mouse_event_is_offset_into_live_region () =
  let app, _state =
    make_app ~mode:`Primary ~startup_cursor:(11, 1) ~target_fps:None
      ~input_timeout:(Some 0.)
      ~events:[ mouse_press 4 12 ]
      ()
  in
  let inputs = ref [] in
  Matrix.run app
    ~on_input:(fun app event ->
      inputs := event :: !inputs;
      Matrix.stop app)
    ~on_render:(fun _app -> ());
  match List.rev !inputs with
  | [ Matrix.Mouse { y; kind = Press _; _ } ] ->
      equal ~msg:"mouse y is relative to live viewport" int 2 y
  | _ -> fail "expected one mouse press event"

let test_primary_mouse_event_above_live_region_maps_to_minus_one () =
  let app, _state =
    make_app ~mode:`Primary ~startup_cursor:(11, 1) ~target_fps:None
      ~input_timeout:(Some 0.)
      ~events:[ mouse_press 4 5 ]
      ()
  in
  let inputs = ref [] in
  Matrix.run app
    ~on_input:(fun app event ->
      inputs := event :: !inputs;
      Matrix.stop app)
    ~on_render:(fun _app -> ());
  match List.rev !inputs with
  | [ Matrix.Mouse { y; kind = Press _; _ } ] ->
      equal ~msg:"mouse y above live region maps outside UI" int (-1) y
  | _ -> fail "expected one mouse press event"

let test_primary_adjusts_all_mouse_like_events () =
  let cases =
    [
      ("release", mouse_release 4 12, mouse_release 4 2);
      ("motion", mouse_motion 4 12, mouse_motion 4 2);
      ("scroll", mouse_scroll 4 12, mouse_scroll 4 2);
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      let app, _state =
        make_app ~mode:`Primary ~startup_cursor:(11, 1) ~target_fps:None
          ~input_timeout:(Some 0.) ~events:[ input ] ()
      in
      let got = ref None in
      Matrix.run app
        ~on_input:(fun app event ->
          got := Some event;
          Matrix.stop app)
        ~on_render:(fun _app -> ());
      match !got with
      | Some event when event = expected -> ()
      | Some _ -> failf "unexpected adjusted %s event" name
      | None -> failf "missing adjusted %s event" name)
    cases

(* The Unix backend must render a frame requested from another thread while no
   input arrives: readiness of the wakeup pipe alone must never lead to a
   blocking read of the input fd. Runs against real fds — stdin is temporarily
   a pipe that stays empty, so a regressed loop parks in [Unix.read] and only
   the watchdog byte (well past the asserted deadline) unblocks it. *)
let test_wakeup_renders_without_input () =
  let input_r, input_w = Unix.pipe ~cloexec:false () in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  let saved_stdin = Unix.dup Unix.stdin in
  Unix.dup2 input_r Unix.stdin;
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved_stdin Unix.stdin;
      List.iter
        (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
        [ saved_stdin; input_r; input_w; devnull ])
    (fun () ->
      let app =
        Matrix.create ~raw_mode:false ~target_fps:None ~input_timeout:None
          ~signal_handlers:false ~output:(`Fd devnull) ()
      in
      let frames = ref 0 in
      let second_frame_elapsed = ref infinity in
      let started = Unix.gettimeofday () in
      let finished = Atomic.make false in
      let requester =
        Thread.create
          (fun () ->
            Thread.delay 0.05;
            Matrix.request_redraw app)
          ()
      in
      let watchdog =
        Thread.create
          (fun () ->
            let rec wait n =
              if n > 0 && not (Atomic.get finished) then (
                Thread.delay 0.05;
                wait (n - 1))
            in
            wait 50;
            (* A regressed loop re-enters the blocking read after every
               unblocked byte, so keep feeding input until the run returns. *)
            let rec unblock n =
              if n > 0 && not (Atomic.get finished) then (
                (try ignore (Unix.write input_w (Bytes.of_string "q") 0 1 : int)
                 with Unix.Unix_error _ -> ());
                Thread.delay 0.1;
                unblock (n - 1))
            in
            unblock 100)
          ()
      in
      Matrix.run app ~on_render:(fun app ->
          incr frames;
          if !frames = 2 then (
            second_frame_elapsed := Unix.gettimeofday () -. started;
            Matrix.stop app));
      Atomic.set finished true;
      Thread.join requester;
      Thread.join watchdog;
      equal ~msg:"wakeup produces the requested frame" int 2 !frames;
      satisfies ~claim:"frame renders without waiting for input (< 1s)"
        float_exact
        (fun dt -> dt < 1.)
        !second_frame_elapsed)

(* Input on a virtual clock. Bytes arrive at their times: [typed] as the user
   types them, and the terminal's replies, which [answer] gives for each write
   of the runtime, [rtt] after it. The clock moves only when the runtime
   waits: to the next arrival within its timeout, or by the whole timeout. A
   run stops at [limit]. *)
type clocked = {
  mutable now : float;
  mutable arrivals : (float * string) list;
  written : Buffer.t;
  mutable waits : (float * float option) list;  (** When, and how long. *)
  mutable seen : (float * Matrix.event) list;  (** Delivered, latest first. *)
}

let clocked_app ?(mode = `Alt) ?(env = no_env) ?(exit_on_ctrl_c = true)
    ?(limit = 10.) ?(rtt = 0.) ?(answer = fun _ -> []) typed =
  let c =
    {
      now = 0.;
      arrivals = [];
      written = Buffer.create 256;
      waits = [];
      seen = [];
    }
  in
  let arrive at bytes =
    c.arrivals <-
      List.stable_sort
        (fun (a, _) (b, _) -> Float.compare a b)
        (c.arrivals @ [ (at, bytes) ])
  in
  List.iter (fun (at, bytes) -> arrive at bytes) typed;
  let app_ref = ref None in
  let write_output buf off len =
    let s = Bytes.sub_string buf off len in
    Buffer.add_string c.written s;
    List.iter (arrive (c.now +. rtt)) (answer s)
  in
  let wait ~timeout ~on_event:_ =
    c.waits <- (c.now, timeout) :: c.waits;
    let deadline = Option.fold ~none:infinity ~some:(( +. ) c.now) timeout in
    match c.arrivals with
    | (at, _) :: _ when at <= deadline ->
        c.now <- Float.max c.now at;
        true
    | _ when deadline > limit ->
        c.now <- limit;
        Option.iter Matrix.stop !app_ref;
        false
    | _ ->
        c.now <- deadline;
        false
  in
  let read_input buf off len =
    let due, later = List.partition (fun (at, _) -> at <= c.now) c.arrivals in
    let s = String.concat "" (List.map snd due) in
    let n = min len (String.length s) in
    c.arrivals <-
      (if n < String.length s then
         (c.now, String.sub s n (String.length s - n)) :: later
       else later);
    Bytes.blit_string s 0 buf off n;
    if n = 0 then -1 else n
  in
  let backend : Matrix.Backend.t =
    {
      now = (fun () -> c.now);
      wake = ignore;
      write_output;
      read_input;
      wait;
      input_is_tty = true;
      output_is_tty = true;
      terminal_size = (fun () -> (80, 24));
      set_raw_mode = ignore;
      flush_input = ignore;
      protect = (fun f -> f ());
      cleanup = ignore;
      env;
    }
  in
  let app =
    Matrix.attach ~mode ~target_fps:None ~exit_on_ctrl_c ~resize_debounce:None
      ~pace_redraws:false ~backend ()
  in
  app_ref := Some app;
  (app, c)

let run_clocked ?(on_event = fun _ _ -> ()) (app, c) =
  Matrix.run app ~on_render:ignore ~on_input:(fun app event ->
      c.seen <- (c.now, event) :: c.seen;
      on_event app event)

let events c = List.rev_map snd c.seen

(* An xterm-like terminal: it answers the startup probe, which ends with DA1,
   with its two cursor reports and DA1. *)
let answers_probe s =
  if
    String.ends_with ~suffix:"\027[c" s && String.starts_with ~prefix:"\027 F" s
  then [ "\027[1;1R\027[1;1R\027[?64;1c" ]
  else []

let wait_at c time =
  List.filter_map
    (fun (at, timeout) ->
      if Float.abs (at -. time) < 1e-9 then timeout else None)
    c.waits

let close_to a b = Float.abs (a -. b) < 1e-9

(* An ESC waits max(50 ms, 2 RTT) for what follows, and a sequence begun max
   (500 ms, 4 RTT), the round trip measured from the probe's DA1. *)
let test_waits_follow_round_trip () =
  let ((app, c) as t) =
    clocked_app ~rtt:0.2 ~answer:answers_probe ~limit:5.
      [ (1., "\027"); (3., "\027[1;") ]
  in
  ignore app;
  run_clocked t;
  (match c.seen with
  | [ (at, Key { key = Escape; _ }) ] ->
      is_true ~msg:"Escape after 2 RTT of silence" (close_to at 1.4)
  | _ -> fail "expected one Escape");
  is_true ~msg:"the sequence waits 4 RTT"
    (List.exists (close_to 0.8) (wait_at c 3.));
  let ((_, c) as t) = clocked_app ~limit:2. [ (1., "\027") ] in
  run_clocked t;
  match c.seen with
  | [ (at, Key { key = Escape; _ }) ] ->
      is_true ~msg:"50 ms with no round trip" (close_to at 1.05)
  | _ -> fail "expected one Escape"

(* A batch is owed until its answer, so a reply that comes after the runtime
   stopped waiting for it is still a reply. A batch the terminal never answers
   is given up after 5 s, and what comes after decodes as input: a modified F3
   is a key and [ESC \]] is Alt+\]. *)
let test_unanswered_batch_is_given_up () =
  let ((_, c) as t) =
    clocked_app ~limit:7. [ (1., "\027[1;2R"); (6., "\027[1;2R\027]1x") ]
  in
  is_true ~msg:"startup waits 500 ms for the probe" (close_to c.now 0.5);
  run_clocked t;
  let key ?(text = "") ?(mods = Matrix.Input.Keystroke.no_mods) key =
    Matrix.Key { key; mods; text; base = None; keypad = false; repeat = false }
  in
  let alt = { Matrix.Input.Keystroke.no_mods with alt = true } in
  let shift = { Matrix.Input.Keystroke.no_mods with shift = true } in
  equal (list input_event)
    [
      key ~mods:shift (F 3);
      key ~mods:alt (Char (Uchar.of_char ']'));
      key ~text:"1" (Char (Uchar.of_char '1'));
      key ~text:"x" (Char (Uchar.of_char 'x'));
    ]
    (events c)

let index_of s sub =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length s then None
    else if String.sub s i n = sub then Some i
    else go (i + 1)
  in
  go 0

(* A terminal that answers every query it knows, as xterm does: mode
   reports, cursor reports with the cursor on row 10 unless the write moved
   it (the probe's width tests ignore OSC 66, so their column is 1), XTVERSION
   when [xtversion], DA1 and the pixel size. *)
let xterm ?(xtversion = true) s =
  let module P = Matrix.Ansi.Parser in
  let row = ref 10 and out = Buffer.create 64 in
  P.parse s
  |> List.iter (function
    | P.Csi { final = 'H'; marker = None; params; _ } ->
        row := P.Params.pn params 0 ~default:1
    | Csi { final = 'n'; marker = None; params; _ }
      when P.Params.value params 0 = Some 6 ->
        Buffer.add_string out (Printf.sprintf "\027[%d;1R" !row)
    | Csi { final = 'p'; marker = Some '?'; intermediates = "$"; params } ->
        Buffer.add_string out
          (Printf.sprintf "\027[?%d;2$y" (P.Params.ps params 0 ~default:0))
    | Csi { final = 'q'; marker = Some '>'; _ } ->
        if xtversion then Buffer.add_string out "\027P>|XTerm(398)\027\\"
    | Csi { final = 'c'; marker = None; params; _ }
      when P.Params.count params = 0 ->
        Buffer.add_string out "\027[?64;1c"
    | Csi { final = 't'; marker = None; params; _ }
      when P.Params.value params 0 = Some 14 ->
        Buffer.add_string out "\027[4;480;800t"
    | _ -> ());
  if Buffer.length out = 0 then [] else [ Buffer.contents out ]

(* However slow the link, up to the 5 s a batch stays owed, no reply is
   typed: the application receives exactly what the user typed. A cursor
   report anchors the live region only where the report it answers says. *)
let test_late_replies_never_typed () =
  let letters = "abcdefghijklmnopqrstuvwxyz" in
  let typed =
    List.init 26 (fun i ->
        (0.1 +. (0.25 *. Float.of_int i), String.make 1 letters.[i]))
  in
  List.iter
    (fun mode ->
      List.iter
        (fun latency ->
          let ((app, c) as t) =
            clocked_app ~mode ~rtt:latency ~answer:xterm ~limit:12. typed
          in
          run_clocked t;
          let msg =
            Printf.sprintf "%s, latency %g s"
              (match mode with `Alt -> "alt" | `Primary -> "primary")
              latency
          in
          let keys =
            List.filter_map
              (function
                | Matrix.Key { key = Char u; mods; _ }
                  when mods = Matrix.Input.Keystroke.no_mods ->
                    Some (Uchar.to_char u)
                | Resize _ | Color_scheme _ -> None
                | e -> Some (failf "%s: %a" msg Matrix.pp_event e))
              (events c)
          in
          equal ~msg string letters (String.of_seq (List.to_seq keys));
          (* An answer later than the wait for it anchors nothing: the live
             region stays where the runtime planned it, the whole screen. *)
          if mode = `Primary then
            equal
              ~msg:(msg ^ ": anchored at row 10, or not at all")
              (pair int int)
              (if latency <= 1. then (80, 15) else (80, 24))
              (Matrix.size app))
        [ 0.01; 0.3; 0.55; 0.8; 1.; 2.; 3.; 4.; 4.9 ])
    [ `Alt; `Primary ]

(* The probe's width tests report from the second row, so their cursor
   reports are never a modified F3, even after the probe was given up: from a
   terminal that answers with no control string, nothing is typed however
   late it answers. *)
let test_probe_reports_never_keys () =
  List.iter
    (fun latency ->
      let ((_, c) as t) =
        clocked_app ~rtt:latency ~answer:(xterm ~xtversion:false) ~limit:20. []
      in
      run_clocked t;
      equal
        ~msg:(Printf.sprintf "latency %g s" latency)
        (list input_event) [] (events c))
    [ 6.; 10. ]

(* The startup payload starts with S7C1T and sets the reports that need no
   answer; a late answer changes the modes by their difference: the kitty
   flags are pushed and asked for again, modifyOtherKeys goes, and
   metaSendsEscape is set where it was reset, and reset on exit. *)
let test_late_answer_changes_modes () =
  let ((app, c) as t) =
    clocked_app ~limit:3. [ (1., "\027[?0u\027[?1036;2$y") ]
  in
  let startup = Buffer.contents c.written in
  is_true ~msg:"S7C1T first" (String.starts_with ~prefix:"\027 F" startup);
  List.iter
    (fun sub -> contains ~msg:"startup" ~sub startup)
    [ "\027[?1004h"; "\027[?2031h"; "\027[?2048h"; "\027[?2004h"; "\027[>4;1m" ];
  not_contains ~msg:"no kitty flags before an answer" ~sub:"\027[>5u" startup;
  not_contains ~msg:"metaSendsEscape untouched" ~sub:"\027[?1036h" startup;
  Buffer.clear c.written;
  run_clocked t;
  (* The run closes the application as it ends. *)
  let late = Buffer.contents c.written in
  (match (index_of late "\027[>5u", index_of late "\027[?u") with
  | Some push, Some query ->
      is_true ~msg:"flags asked for after the push" (query > push)
  | _ -> fail "expected the kitty flags pushed and asked for");
  contains ~msg:"modifyOtherKeys off" ~sub:"\027[>4;0m" late;
  contains ~msg:"metaSendsEscape set" ~sub:"\027[?1036h" late;
  equal ~msg:"capabilities" (option int) (Some 0)
    (Matrix.capabilities app).kitty_keyboard;
  List.iter
    (fun sub -> contains ~msg:"close" ~sub late)
    [ "\027[<u"; "\027[?1036l"; "\027[?1004l"; "\027[?2031l"; "\027[?2048l" ]

(* A terminal that has metaSendsEscape set keeps it. *)
let test_meta_sends_escape_kept () =
  let app, c =
    clocked_app ~limit:1.
      ~answer:(fun s ->
        if String.starts_with ~prefix:"\027 F" s then
          [ "\027[?1036;1$y\027[1;1R\027[1;1R\027[?64;1c" ]
        else [])
      []
  in
  Matrix.close app;
  not_contains ~sub:"\027[?1036"
    ( Buffer.contents c.written |> fun s ->
      match index_of s "\027[?1036$p" with
      | Some i -> String.sub s (i + 9) (String.length s - i - 9)
      | None -> s )

(* Paste reaches the application as it arrives, with no limit;
   [Matrix.abort_paste] ends it, and the bytes after decode as input. *)
let test_paste_streams_and_aborts () =
  let big = String.make (1024 * 1024) 'p' in
  let ((_, c) as t) =
    clocked_app ~limit:5.
      [
        (1., "\027[200~abc");
        (2., "def");
        (3., "\027[201~x");
        (4., "\027[200~");
        (4.1, big);
        (4.2, "\027[201~");
      ]
  in
  run_clocked t ~on_event:(fun app -> function
    | Paste_data "def" -> Matrix.abort_paste app
    | _ -> ());
  let events = events c in
  let rec take n = function
    | x :: rest when n > 0 -> x :: take (n - 1) rest
    | _ -> []
  in
  equal ~msg:"cut by the application" (list input_event)
    [
      Paste_start;
      Paste_data "abc";
      Paste_data "def";
      Paste_end `Cut;
      Matrix.Key
        {
          key = Char (Uchar.of_char 'x');
          mods = Matrix.Input.Keystroke.no_mods;
          text = "x";
          base = None;
          keypad = false;
          repeat = false;
        };
      Paste_start;
    ]
    (take 6 events);
  let data =
    String.concat ""
      (List.filter_map
         (function Matrix.Paste_data d -> Some d | _ -> None)
         (List.filteri (fun i _ -> i >= 6) events))
  in
  equal ~msg:"a large paste whole" int (String.length big) (String.length data);
  is_true ~msg:"complete" (List.mem (Matrix.Paste_end `Complete) events)

(* The replies that are application facts reach it: the color scheme, in
   reply to a query or on a change, and the in-band resize when the size
   changed. Other replies never do. *)
let test_application_replies () =
  let ((app, c) as t) =
    clocked_app ~limit:3.
      [
        (1., "\027[?997;1n\027[48;24;80;480;800t\027[?2026;1$y");
        (2., "\027[48;30;100;600;1000t\027[?64;1c\027[1;5R");
      ]
  in
  run_clocked t;
  equal (list input_event)
    [
      Color_scheme `Dark;
      Resize (100, 30);
      Matrix.Key
        {
          key = F 3;
          mods = { Matrix.Input.Keystroke.no_mods with ctrl = true };
          text = "";
          base = None;
          keypad = false;
          repeat = false;
        };
    ]
    (events c);
  equal ~msg:"pixels"
    (option (pair int int))
    (Some (1000, 600))
    (Matrix.pixel_resolution app)

(* Ctrl+C closes on a press: a release never does, whatever the encoding. *)
let test_ctrl_c_ignores_releases () =
  List.iter
    (fun (name, bytes, closes) ->
      let ((_, c) as t) =
        clocked_app ~limit:3. [ (1., "\027[99;5:3u"); (2., bytes) ]
      in
      run_clocked t;
      equal ~msg:name bool closes (close_to c.now 2.))
    [
      ("legacy", "\003", true);
      ("kitty", "\027[99;5u", true);
      ("kitty, Shift consumed", "\027[99:67;6u", true);
      ("Ctrl+D", "\004", false);
    ]

(* Inside tmux, the probe goes to tmux; once it answers, passthrough is
   asked about with a wrapped DA1, and only once that answers are the
   questions about the outer terminal sent, wrapped, each group closed by its
   own wrapped DA1. *)
let test_tmux_groups () =
  let wrapped_da1 = "\027Ptmux;\027\027[c\027\\" in
  let tmux ~passthrough s =
    if String.starts_with ~prefix:"\027 F" s then
      [ "\027P>|tmux 3.5a\027\\\027[1;1R\027[1;1R\027[?1;2;4c" ]
    else if s = wrapped_da1 && passthrough then [ "\027[?64;1c" ]
    else if String.starts_with ~prefix:"\027Ptmux;\027\027_G" s && passthrough
    then [ "\027_Gi=31337;OK\027\\\027[?64;1c" ]
    else []
  in
  let app, c =
    clocked_app
      ~env:(env [ ("TMUX", "s") ])
      ~rtt:0.01 ~limit:2. ~answer:(tmux ~passthrough:true) []
  in
  run_clocked (app, c);
  let out = Buffer.contents c.written in
  (match (index_of out wrapped_da1, index_of out "\027Ptmux;\027\027_G") with
  | Some check, Some outer ->
      is_true ~msg:"outer after the check" (outer > check)
  | _ -> fail "expected the passthrough check and the outer group");
  let caps = Matrix.capabilities app in
  is_true ~msg:"outer graphics" caps.kitty_graphics;
  is_true ~msg:"tmux" (caps.multiplexer = Some `Tmux);
  equal ~msg:"tmux's widths" input_width `Wcwidth caps.unicode_width;
  let app, c =
    clocked_app
      ~env:(env [ ("TMUX", "s") ])
      ~rtt:0.01 ~limit:2. ~answer:(tmux ~passthrough:false) []
  in
  run_clocked (app, c);
  let out = Buffer.contents c.written in
  contains ~msg:"passthrough asked" ~sub:wrapped_da1 out;
  not_contains ~msg:"nothing wrapped without passthrough" ~sub:"\027_G" out;
  is_false ~msg:"no graphics" (Matrix.capabilities app).kitty_graphics

let () =
  run "matrix.runtime"
    [
      group "Control"
        [
          test "request_redraw while paused" test_request_redraw_while_paused;
          test "live requested while suspended"
            test_live_requested_while_suspended;
          test "uncapped live cadence" test_uncapped_live_cadence;
          test "demand changes while suspended"
            test_demand_changes_while_suspended;
          test "stopped and closed frame eligibility"
            test_stopped_and_closed_frame_eligibility;
          test "stop while suspended requires start after resume"
            test_stop_while_suspended_requires_start_after_resume;
          test "frame callbacks respect lifecycle"
            test_frame_callbacks_respect_lifecycle;
          test "suspended events defer terminal operations"
            test_suspended_events_defer_terminal_operations;
          test "cross-domain redraw during render"
            test_cross_domain_redraw_during_render;
          test "redraw races lifecycle" test_redraw_races_lifecycle;
          test "immediate redraw bypasses live cadence once"
            test_immediate_redraw_bypasses_live_cadence_once;
          test "idle does not force live loop"
            test_idle_does_not_force_live_loop;
        ];
      group "Lifecycle"
        [
          test "run closes on exception" test_run_closes_on_exception;
          test "end-of-input finalizes parser and closes once"
            test_end_of_input_finalizes_parser_and_closes_once;
          test "native event closure skips ready input"
            test_native_event_close_skips_ready_input;
          test "Escape expires without more input"
            test_escape_expires_without_more_input;
          test "startup preserves input and partial paste"
            test_startup_preserves_input_and_partial_paste;
          test "startup EOF finalizes input without reading again"
            test_startup_eof_finalizes_input_without_reading_again;
          test "close restores raw mode if terminal close raises"
            test_close_restores_raw_mode_if_terminal_close_raises;
          test "close restores owned signal dispositions once"
            test_close_restores_owned_signal_dispositions_once;
          test "attach failure preserves signal dispositions"
            test_attach_failure_preserves_signal_dispositions;
          test "nested apps restore signal dispositions in reverse"
            test_nested_apps_restore_signal_dispositions_in_reverse;
          test "attach rolls back partially applied terminal modes"
            test_attach_rolls_back_partially_applied_terminal_modes;
          test "focus restore runs only after blur once"
            test_focus_restore_runs_only_after_blur_once;
          test "late capability response requests redraw"
            test_late_capability_response_requests_redraw;
          test "stray CPR does not flip capabilities"
            test_stray_cpr_does_not_flip_capabilities;
          test "Ghostty profile reaches runtime"
            test_ghostty_profile_reaches_runtime;
          test "startup and late CPR do not enable width"
            test_startup_and_late_cpr_do_not_enable_width;
          test "startup capability window defers split response"
            test_startup_capability_window_defers_split_response;
          test "resume reanchors primary from bottom cursor"
            test_resume_reanchors_primary_from_bottom_cursor;
          test "resume preserves resize during cursor query"
            test_resume_preserves_resize_during_cursor_query;
          test "suspend is idempotent" test_suspend_is_idempotent;
          test "suspend discards partial input"
            test_suspend_discards_partial_input;
          test "cursor visibility preserves blinking"
            test_cursor_visibility_preserves_blinking;
          test "close leaves untouched appearance alone"
            test_close_leaves_untouched_appearance_alone;
          test "close restores touched cursor appearance"
            test_close_restores_touched_cursor_appearance;
        ];
      group "Frame pacing"
        [
          test "target fps respected without input storm"
            test_target_fps_respected_without_input_storm;
          test "target fps respected with input storm"
            test_target_fps_respected_with_input_storm;
        ];
      group "Frame output"
        [
          test "unchanged submit emits no bytes"
            test_unchanged_submit_emits_no_bytes;
          test "unchanged alt submit emits no bytes"
            test_unchanged_alt_submit_emits_no_bytes;
          test "submit reuses render capacity"
            test_submit_reuses_render_capacity;
          test "cursor-only submit emits output"
            test_cursor_only_submit_emits_output;
          test "first submit emits cursor style"
            test_first_submit_emits_cursor_style;
          test "visible cursor re-emits hidden visual state"
            test_visible_cursor_reemits_hidden_visual_state;
          test "submit emits frame above default capacity"
            test_submit_emits_frame_above_default_capacity;
        ];
      group "Resize"
        [
          test "initial resize fires before first render"
            test_initial_resize_fires_before_first_render;
          test "zero dimensions are ignored"
            test_resize_zero_dimensions_are_ignored;
          test "on_resize reports usable size"
            test_on_resize_reports_usable_size;
          test "primary render offset clamps after resize"
            test_resize_clamps_primary_render_offset;
          test "debounce applies latest pending resize"
            test_resize_debounce_applies_latest_pending_resize;
          test "same-size debounced resize settles the loop"
            test_same_size_debounced_resize_settles;
        ];
      group "Primary sizing"
        [
          test "primary required rows expands primary region"
            test_primary_required_rows_expands_primary_region;
          test "primary required rows ignored in alt mode"
            test_primary_required_rows_ignored_in_alt_mode;
          test "effective_size tracks pending static writes"
            test_primary_effective_size_tracks_pending_static;
          test "prepare uses pending static effective region"
            test_prepare_uses_pending_static_effective_region;
          test "static_clear resets primary layout"
            test_static_clear_resets_primary_layout;
          test "static_replace replays inside next frame"
            test_static_replace_replays_inside_next_frame;
          test "primary full redraw does not erase past terminal bottom"
            test_primary_full_redraw_does_not_erase_past_terminal_bottom;
          test "primary full redraw erases stale rows below content"
            test_primary_full_redraw_erases_stale_rows_below_content;
          test
            "primary required rows to terminal height does not erase past \
             bottom"
            test_primary_required_rows_to_terminal_height_does_not_erase_past_bottom;
        ];
      group "Primary static output"
        [
          test "static writes are flushed FIFO"
            test_static_writes_are_flushed_fifo;
          test "static write snapshots selected complete rows"
            test_static_write_snapshots_selected_rows;
          test "column-one anchor starts at the cursor row"
            test_primary_column_one_anchor_starts_at_cursor_row;
          test "static_clear is immediate and forces a full repaint"
            test_static_clear_is_immediate_and_forces_full_repaint;
          test "pinned static write scrolls the transcript"
            test_pinned_static_write_scrolls_transcript;
          test "multiline static write at the pin"
            test_multiline_static_write_at_the_pin;
          test "static write moves the live region down"
            test_static_write_moves_live_region_down;
          test "one-row static area" test_primary_one_row_static_area;
          test "static write and live repaint share sync frame"
            test_static_write_and_live_repaint_share_sync_frame;
          test "pinned static write keeps live size"
            test_pinned_static_write_keeps_live_size;
          test "primary required rows apply after static flush"
            test_primary_required_rows_apply_after_static_flush;
          test "required rows show in their frame"
            test_required_rows_show_in_their_frame;
          test "suspend hands over below the transcript"
            test_suspend_hands_over_below_the_transcript;
          test "full-height static write scrolls into scrollback"
            test_full_height_static_write_scrolls_into_scrollback;
          test "partial frame restores transient modes"
            test_partial_frame_restores_transient_modes;
          test "queued snapshot keeps row count after resize"
            test_queued_static_snapshot_keeps_row_count_after_resize;
          test "static snapshots scroll exactly once per row"
            test_static_snapshots_scroll_exactly_once_per_row;
          test "static snapshots clear suffixes and preserve blank rows"
            test_static_snapshot_clears_suffixes_and_keeps_blank_rows;
          test "preserved static write keeps live region"
            test_preserved_static_write_keeps_live_region;
          test "preserved static write after transcript rows"
            test_preserved_static_write_after_transcript_rows;
          test "mixed static writes" test_mixed_static_writes;
          test "full-height consecutive static writes scroll once per row"
            test_full_height_consecutive_static_writes_scroll_once_per_row;
          test "static_write is ignored in alt mode"
            test_static_write_ignored_in_alt_mode;
        ];
      group "Through a terminal"
        [
          test "alt default cursor is shown" test_alt_default_cursor_is_shown;
          test "full redraw keeps the cursor row"
            test_primary_full_redraw_keeps_cursor_row;
          test "full redraw paints background rows"
            test_primary_full_redraw_paints_background_rows;
          test "anchor scrolls shell rows for the minimum height"
            test_primary_anchor_scrolls_shell_rows;
          test "minimum height survives a shrink"
            test_primary_minimum_height_survives_shrink;
          test "resize keeps the live region below the transcript"
            test_primary_resize_keeps_live_region_below_transcript;
        ];
      group "Input"
        [
          test "waits follow the round trip" test_waits_follow_round_trip;
          test "an unanswered batch is given up"
            test_unanswered_batch_is_given_up;
          test "late replies are never typed" test_late_replies_never_typed;
          test "probe reports are never keys" test_probe_reports_never_keys;
          test "a late answer changes modes" test_late_answer_changes_modes;
          test "metaSendsEscape set is kept" test_meta_sends_escape_kept;
          test "paste streams and aborts" test_paste_streams_and_aborts;
          test "Ctrl+C ignores releases" test_ctrl_c_ignores_releases;
          test "replies that are application facts" test_application_replies;
          test "tmux groups" test_tmux_groups;
        ];
      group "Primary input"
        [
          test "mouse event is offset into live region"
            test_primary_mouse_event_is_offset_into_live_region;
          test "mouse event above live region maps to -1"
            test_primary_mouse_event_above_live_region_maps_to_minus_one;
          test "adjusts all mouse-like events"
            test_primary_adjusts_all_mouse_like_events;
        ];
      group "Unix backend"
        [
          test "wakeup renders without input" test_wakeup_renders_without_input;
        ];
    ]
