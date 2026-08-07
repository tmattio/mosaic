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
  mutable wake_calls : int;
  mutable pending_events : Matrix.Input.t list;
  mutable pending_input_chunks : string list;
  mutable read_timeouts : float option list;
  output : Buffer.t;
  terminal_output : Buffer.t;
}

let make_state events input_chunks =
  {
    now_s = 0.;
    read_calls = 0;
    cleanup_calls = 0;
    raw_restore_calls = 0;
    flush_input_calls = 0;
    wake_calls = 0;
    pending_events = events;
    pending_input_chunks = input_chunks;
    read_timeouts = [];
    output = Buffer.create 256;
    terminal_output = Buffer.create 256;
  }

let output state = Buffer.contents state.output

let contains_substring needle haystack =
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
    i + needle_len <= haystack_len && (matches_at i 0 || loop (i + 1))
  in
  needle_len = 0 || loop 0

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

let count_substring needle haystack =
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
  let rec loop i count =
    if needle_len = 0 || i + needle_len > haystack_len then count
    else if matches_at i 0 then loop (i + needle_len) (count + 1)
    else loop (i + 1) count
  in
  loop 0 0

let is_before ~first ~second s =
  match (substring_index first s, substring_index second s) with
  | Some i, Some j -> i < j
  | _ -> false

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

let make_app ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(input_timeout = Some 0.) ?(terminal_tty = false) ?(advance_now = true)
    ?(sleep_until_timeout = false) ?(read_quantum_s = 0.0001)
    ?(emit_event_each_read = None) ?(events = []) ?(input_chunks = [])
    ?stop_after_reads ?end_after_reads ?render_offset ?static_needs_newline
    ?(min_tui_height = 1) ?(resize_debounce = Some 0.) ?(width = 80)
    ?(height = 24) ?hyperlinks ?(query_cursor_position = fun ~timeout:_ -> None)
    () =
  let state = make_state events input_chunks in
  let terminal =
    Matrix.Terminal.make
      ~output:(fun s -> Buffer.add_string state.terminal_output s)
      ~tty:terminal_tty ()
  in
  Option.iter
    (fun hyperlinks ->
      let caps = Matrix.Terminal.capabilities terminal in
      Matrix.Terminal.set_capabilities terminal { caps with hyperlinks })
    hyperlinks;
  let parser = Matrix.Input.Parser.create () in
  let app_ref = ref None in
  let now () =
    if advance_now then state.now_s <- state.now_s +. 0.001;
    state.now_s
  in
  let read_events ~timeout ~on_event ~on_response =
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
    (match state.pending_input_chunks with
    | [] -> ()
    | chunk :: rest ->
        state.pending_input_chunks <- rest;
        let input = Bytes.of_string chunk in
        Matrix.Input.Parser.feed parser input 0 (Bytes.length input)
          ~now:state.now_s ~on_event ~on_response;
        Matrix.Input.Parser.drain parser ~now:state.now_s ~on_event ~on_response);
    match (stop_after_reads, !app_ref) with
    | Some max_reads, Some app when state.read_calls >= max_reads ->
        Matrix.stop app;
        `Continue
    | _ -> (
        match end_after_reads with
        | Some max_reads when state.read_calls >= max_reads -> `End
        | Some _ | None -> `Continue)
  in
  (* Serve the fake cursor-position reply through the byte-level primitives:
     the runtime's shared query pumps [wait_readable]/[read_input] and parses
     the CPR reply like any other terminal response. *)
  let read_input buf off _len =
    match query_cursor_position ~timeout:0. with
    | None -> 0
    | Some (row, col) ->
        let s = Printf.sprintf "\027[%d;%dR" row col in
        Bytes.blit_string s 0 buf off (String.length s);
        String.length s
  in
  let wait_readable ~timeout =
    Option.is_some (query_cursor_position ~timeout)
  in
  let backend : Matrix.Backend.t =
    {
      now;
      wake = (fun () -> state.wake_calls <- state.wake_calls + 1);
      write_output =
        (fun buf off len ->
          Buffer.add_string state.output (Bytes.sub_string buf off len));
      read_input;
      wait_readable;
      read_events;
      terminal_size = (fun () -> (width, height));
      set_raw_mode =
        (fun enabled ->
          if not enabled then
            state.raw_restore_calls <- state.raw_restore_calls + 1);
      flush_input =
        (fun () -> state.flush_input_calls <- state.flush_input_calls + 1);
      cleanup = (fun () -> state.cleanup_calls <- state.cleanup_calls + 1);
    }
  in
  let app =
    Matrix.attach ~mode ~raw_mode ~target_fps ~input_timeout ~min_tui_height
      ~resize_debounce ~signal_handlers:false ?render_offset
      ?static_needs_newline ~backend ~parser ~terminal ~width ~height ()
  in
  app_ref := Some app;
  (app, state)

let mouse_press x y = Matrix.Input.mouse_press x y Matrix.Input.Mouse.Left

let mouse_release x y =
  Matrix.Input.mouse_release x y (Some Matrix.Input.Mouse.Left)

let mouse_motion x y = Matrix.Input.mouse_drag x y Matrix.Input.Mouse.Left

let mouse_scroll x y =
  Matrix.Input.mouse_scroll x y Matrix.Input.Mouse.Scroll_down

let set_sync_capable app =
  let terminal = Matrix.terminal app in
  let caps = Matrix.Terminal.capabilities terminal in
  Matrix.Terminal.set_capabilities terminal { caps with sync = true }

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

let test_immediate_redraw_bypasses_live_cadence_once () =
  let app, state =
    make_app ~target_fps:(Some 1.) ~input_timeout:None ~sleep_until_timeout:true
      ~emit_event_each_read:(Some Matrix.Input.Focus) ~stop_after_reads:5000 ()
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
      is_true ~msg:"follow-up bypasses one-second cadence"
        (!second_frame_at -. requested < 0.1)
  | None -> fail "missing redraw request"

let test_idle_does_not_force_live_loop () =
  let app, _state =
    make_app ~target_fps:None ~input_timeout:(Some 0.) ~stop_after_reads:1 ()
  in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun _app -> incr frames);
  equal ~msg:"idle renders only the initial frame" int 1 !frames

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

let test_end_of_input_finalizes_parser_and_closes_once () =
  let app, state =
    make_app ~input_chunks:[ "\x1b[200~partial" ] ~end_after_reads:1 ()
  in
  let events = ref [] in
  Matrix.run app
    ~on_input:(fun _app event -> events := event :: !events)
    ~on_render:(fun _app -> ());
  equal ~msg:"end-of-input is read once" int 1 state.read_calls;
  equal ~msg:"end-of-input closes backend once" int 1 state.cleanup_calls;
  equal ~msg:"end-of-input restores raw mode once" int 1 state.raw_restore_calls;
  is_false ~msg:"application is no longer running" (Matrix.running app);
  match List.rev !events with
  | [ Matrix.Input.Error (Matrix.Input.Error.Unterminated_paste { received }) ]
    ->
      equal ~msg:"EOF finalizes the open paste" int 7 received
  | events ->
      failf "expected one unterminated-paste event, got %d" (List.length events)

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

let test_close_restores_raw_mode_if_terminal_close_raises () =
  let fail_output = ref false in
  let terminal =
    Matrix.Terminal.make ~tty:true
      ~output:(fun _ -> if !fail_output then failwith "closed output")
      ()
  in
  let parser = Matrix.Input.Parser.create () in
  let cleanup_calls = ref 0 in
  let raw_restore_calls = ref 0 in
  let app =
    Matrix.attach ~mode:`Primary ~raw_mode:true ~mouse_enabled:false
      ~bracketed_paste:false ~focus_reporting:false ~kitty_keyboard:`Disabled
      ~target_fps:None ~input_timeout:(Some 0.) ~signal_handlers:false
      ~backend:
        {
          now = (fun () -> 0.);
          wake = (fun () -> ());
          write_output = (fun _buf _off _len -> ());
          read_input = (fun _ _ _ -> 0);
          wait_readable = (fun ~timeout:_ -> false);
          read_events = (fun ~timeout:_ ~on_event:_ ~on_response:_ -> `Continue);
          terminal_size = (fun () -> (80, 24));
          set_raw_mode =
            (fun enabled -> if not enabled then incr raw_restore_calls);
          flush_input = (fun () -> ());
          cleanup = (fun () -> incr cleanup_calls);
        }
      ~parser ~terminal ~width:80 ~height:24 ()
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
          ~bracketed_paste:false ~focus_reporting:false
          ~kitty_keyboard:`Disabled ~output:(`Fd write_fd) ()
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
  let terminal =
    Matrix.Terminal.make ~tty:true
      ~output:(fun _ -> raise_injected_startup_failure ())
      ()
  in
  let parser = Matrix.Input.Parser.create () in
  let failure =
    try
      ignore
        (Matrix.attach ~mode:`Alt ~raw_mode:true ~target_fps:None
           ~signal_handlers:true
           ~backend:
             {
               now = (fun () -> 0.);
               wake = (fun () -> ());
               write_output = (fun _ _ _ -> ());
               read_input = (fun _ _ _ -> 0);
               wait_readable = (fun ~timeout:_ -> false);
               read_events =
                 (fun ~timeout:_ ~on_event:_ ~on_response:_ -> `Continue);
               terminal_size = (fun () -> (80, 24));
               set_raw_mode = (fun _ -> ());
               flush_input = (fun () -> ());
               cleanup = (fun () -> ());
             }
           ~parser ~terminal ~width:80 ~height:24 ());
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
          ~bracketed_paste:false ~focus_reporting:false
          ~kitty_keyboard:`Disabled ~output:(`Fd write_fd) ()
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
  let terminal =
    Matrix.Terminal.make ~tty:true
      ~output:(fun output ->
        incr writes;
        Buffer.add_string lifecycle_trace output;
        if !writes = 2 then raise_injected_startup_failure ();
        Buffer.add_string terminal_output output)
      ()
  in
  let parser = Matrix.Input.Parser.create () in
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
                   now = (fun () -> 0.);
                   wake = (fun () -> ());
                   write_output = (fun _ _ _ -> ());
                   read_input = (fun _ _ _ -> 0);
                   wait_readable = (fun ~timeout:_ -> false);
                   read_events =
                     (fun ~timeout:_ ~on_event:_ ~on_response:_ -> `Continue);
                   terminal_size = (fun () -> (80, 24));
                   set_raw_mode =
                     (fun enabled ->
                       Buffer.add_string lifecycle_trace
                         (if enabled then "<raw:on>" else "<raw:off>");
                       raw_changes := enabled :: !raw_changes);
                   flush_input = (fun () -> ());
                   cleanup =
                     (fun () ->
                       Buffer.add_string lifecycle_trace "<cleanup>";
                       incr cleanup_calls);
                 }
               ~parser ~terminal ~width:80 ~height:24 ());
          None
        with exn -> Some (exn, Printexc.get_raw_backtrace ()))
  in
  (match failure with
  | Some (Injected_startup_failure, backtrace) ->
      is_true ~msg:"rollback preserves the startup failure backtrace"
        (contains_substring "raise_injected_startup_failure"
           (Printexc.raw_backtrace_to_string backtrace))
  | Some (exn, _) ->
      failf "unexpected startup exception: %s" (Printexc.to_string exn)
  | None -> fail "startup failure did not escape Matrix.attach");
  equal ~msg:"raw mode acquired then restored" (list bool) [ true; false ]
    (List.rev !raw_changes);
  equal ~msg:"backend cleanup runs once" int 1 !cleanup_calls;
  is_true ~msg:"partially entered alternate screen is left during rollback"
    (contains_substring "\027[?1049l" (Buffer.contents terminal_output));
  let lifecycle_trace = Buffer.contents lifecycle_trace in
  is_true ~msg:"raw mode is acquired before terminal protocols"
    (is_before ~first:"<raw:on>" ~second:"\027[?1049h" lifecycle_trace);
  is_true ~msg:"terminal protocols roll back before raw mode"
    (is_before ~first:"\027[?1049l" ~second:"<raw:off>" lifecycle_trace);
  is_true ~msg:"raw mode rolls back before backend cleanup"
    (is_before ~first:"<raw:off>" ~second:"<cleanup>" lifecycle_trace)

let test_focus_restore_runs_only_after_blur_once () =
  let app, state =
    make_app ~terminal_tty:true ~target_fps:None ~input_timeout:(Some 0.)
      ~events:[ Matrix.Input.Focus ] ()
  in
  let first_focus_output = ref (-1) in
  Buffer.clear state.terminal_output;
  Matrix.run app
    ~on_input:(fun app _event ->
      first_focus_output := Buffer.length state.terminal_output;
      Matrix.stop app)
    ~on_render:(fun _app -> ());
  equal ~msg:"focus without prior blur does not restore terminal modes" int 0
    !first_focus_output;

  let app, state =
    make_app ~terminal_tty:true ~target_fps:None ~input_timeout:(Some 0.)
      ~events:[ Matrix.Input.Blur; Matrix.Input.Focus; Matrix.Input.Focus ]
      ()
  in
  let seen = ref [] in
  Buffer.clear state.terminal_output;
  Matrix.run app
    ~on_input:(fun app event ->
      seen := (event, Buffer.length state.terminal_output) :: !seen;
      match List.length !seen with 3 -> Matrix.stop app | _ -> ())
    ~on_render:(fun _app -> ());
  match List.rev !seen with
  | [
   (Matrix.Input.Blur, after_blur);
   (Matrix.Input.Focus, after_first_focus);
   (Matrix.Input.Focus, after_second_focus);
  ] ->
      equal ~msg:"blur itself does not restore terminal modes" int 0 after_blur;
      is_true ~msg:"first focus after blur restores terminal modes"
        (after_first_focus > after_blur);
      equal ~msg:"second focus does not restore modes again" int
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
      ~input_timeout:(Some 0.)
      ~query_cursor_position:(fun ~timeout:_ -> Some (24, 1))
      ()
  in
  Matrix.suspend app;
  Buffer.clear state.terminal_output;
  Matrix.resume app;
  is_true ~msg:"bottom-row resume writes newline before reanchoring"
    (contains_substring "\r\n" (Buffer.contents state.terminal_output));
  equal ~msg:"bottom-row cursor anchors one-row live viewport" (pair int int)
    (80, 1) (Matrix.size app)

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
      ~sleep_until_timeout:false ~emit_event_each_read:(Some Matrix.Input.Focus)
      ()
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
  is_true ~msg:"first alt frame writes" (String.length (output state) > 0);
  Buffer.clear state.output;
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
  Matrix.submit app;
  equal ~msg:"unchanged alt frame skips terminal write" int 0
    (String.length (output state))

(* Regression: close must not clobber terminal appearance the app never
   touched (window title, cursor colour, cursor style). *)
let test_close_leaves_untouched_appearance_alone () =
  let app, state =
    make_app ~terminal_tty:true ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  Matrix.submit app;
  Matrix.close app;
  let out = Buffer.contents state.terminal_output in
  is_false ~msg:"close does not clear the title" (contains_substring "\027]0;" out);
  is_false ~msg:"close does not reset cursor color"
    (contains_substring "\027]112" out);
  is_false ~msg:"close does not reset cursor style"
    (contains_substring "\027[0 q" out)

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
  Matrix.close app;
  let out = Buffer.contents state.terminal_output in
  is_true ~msg:"close resets the emitted cursor style"
    (contains_substring "\027[0 q" out);
  is_true ~msg:"close resets the emitted cursor color"
    (contains_substring "\027]112" out)

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
        Matrix.Grid.set_cell (Matrix.grid app) ~x:0 ~y:0
          ~cell:(Matrix.Grid.Cell.of_uchar (Uchar.of_char 'X'))
          ~fg:Matrix.Ansi.Color.white ~bg:Matrix.Ansi.Color.black
          ~attrs:Matrix.Ansi.Attr.empty ();
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
  is_true ~msg:"cursor-only frame emits terminal output"
    (String.length output > 0);
  is_true ~msg:"cursor-only frame moves the cursor"
    (contains_substring "\027[1;3H" output)

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
  is_true ~msg:"first frame emits the cursor style"
    (contains_substring "\027[1 q" (output state))

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
  is_false ~msg:"hidden frame does not emit cursor style"
    (contains_substring "\027[1 q" hidden_output);
  is_false ~msg:"hidden frame does not emit cursor color"
    (contains_substring "\027]12;#ff0000\007" hidden_output);
  Buffer.clear state.output;
  draw ~visible:true;
  let visible_output = output state in
  is_true ~msg:"visible frame emits cursor style"
    (contains_substring "\027[1 q" visible_output);
  is_true ~msg:"visible frame emits cursor color"
    (contains_substring "\027]12;#ff0000\007" visible_output)

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
  is_true ~msg:"submit emits the complete frame above the default buffer size"
    (String.length (output state) > 2 * 1024 * 1024);
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
      ~events:[ Matrix.Input.Resize (0, 0) ]
      ~stop_after_reads:1 ()
  in
  let events = ref [] in
  Matrix.run app
    ~on_resize:(fun app ~cols ~rows ->
      events := (cols, rows, Matrix.full_size app, Matrix.size app) :: !events)
    ~on_render:(fun _app -> ());
  match List.rev !events with
  | [ (80, 24, (80, 24), (80, 24)); (0, 0, (80, 24), (80, 24)) ] -> ()
  | got -> failf "unexpected resize trace: %d event(s)" (List.length got)

let test_resize_clamps_primary_render_offset () =
  let app, _state =
    make_app ~mode:`Primary ~render_offset:20 ~resize_debounce:None
      ~target_fps:None ~input_timeout:(Some 0.)
      ~events:[ Matrix.Input.Resize (80, 12) ]
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
      ~events:[ Matrix.Input.Resize (100, 30); Matrix.Input.Resize (120, 40) ]
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
      ~events:[ Matrix.Input.Resize (100, 30); Matrix.Input.Resize (100, 30) ]
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
    make_app ~mode:`Primary ~render_offset:22 ~target_fps:(Some 30.)
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

let test_primary_effective_size_tracks_pending_static () =
  let app, _state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  equal ~msg:"initial primary height" int 24 (snd (Matrix.size app));
  Matrix.static_write app ~rows:2 "alpha\nbeta\n";
  let effective = Matrix.effective_size app in
  equal ~msg:"pending static write reduces effective height" (pair int int)
    (80, 21) effective;
  Matrix.submit app;
  equal ~msg:"submit applies pending effective size" (pair int int) effective
    (Matrix.size app)

let test_prepare_uses_pending_static_effective_region () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.static_write app ~rows:2 "alpha\nbeta\n";
  let _, effective_height = Matrix.effective_size app in
  Matrix.prepare app;
  let grid = Matrix.grid app in
  equal ~msg:"prepare sizes grid to pending effective height" int
    effective_height (Matrix.Grid.height grid);
  Matrix.Grid.draw_text grid ~x:0 ~y:(effective_height - 1) ~text:"live";
  Matrix.submit app;
  let out = output state in
  is_true ~msg:"static output is emitted" (contains_substring "alpha" out);
  is_true ~msg:"live output is rendered after static output"
    (is_before ~first:"alpha" ~second:"live" out)

let test_static_writes_are_flushed_fifo () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.static_write app ~rows:1 "first\n";
  Matrix.static_write app ~rows:1 "second\n";
  Matrix.submit app;
  let output = output state in
  match (substring_index "first" output, substring_index "second" output) with
  | Some first, Some second ->
      is_true ~msg:"first static write appears before second" (first < second)
  | _ -> fail "expected both static writes in frame output"

let test_static_write_normalizes_lf_in_raw_mode () =
  let app, state =
    make_app ~mode:`Primary ~raw_mode:true ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.static_write app ~rows:1 "raw\n";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"raw mode normalizes lone LF to CRLF"
    (contains_substring "raw\r\n" output);
  is_false ~msg:"raw mode does not emit lone LF"
    (contains_substring "raw\n" output)

let test_static_write_preserves_lf_outside_raw_mode () =
  let app, state =
    make_app ~mode:`Primary ~raw_mode:false ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.static_write app ~rows:1 "plain\n";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"non-raw mode preserves LF" (contains_substring "plain\n" output);
  is_false ~msg:"non-raw mode does not normalize LF to CRLF"
    (contains_substring "plain\r\n" output)

let test_static_write_tracks_mid_line_continuation () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:23 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "first";
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "second\n";
  Matrix.submit app;
  is_true ~msg:"mid-line static output anchors next write on a fresh row"
    (contains_substring "\r\nsecond" (output state));

  let app, state =
    make_app ~mode:`Primary ~render_offset:23 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "first\n";
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "second\n";
  Matrix.submit app;
  is_false ~msg:"line-ended static output does not add an extra leading CRLF"
    (contains_substring "\r\nsecond" (output state))

let test_pinned_static_write_uses_bounded_scroll_region () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:23 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "pinned\n";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"pinned append sets upper-pane scroll region"
    (contains_substring "\027[1;23r" output);
  is_true ~msg:"pinned append resets scroll region"
    (contains_substring "\027[r" output);
  is_true ~msg:"scroll region is set before payload"
    (is_before ~first:"\027[1;23r" ~second:"pinned" output);
  is_true ~msg:"scroll region reset follows payload"
    (is_before ~first:"pinned" ~second:"\027[r" output);
  is_false ~msg:"pinned append does not use broad erase"
    (contains_substring "\027[J" output)

let test_pinned_static_write_resets_scroll_region_before_live_render () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:23 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "pinned\n";
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"static payload appears before live render"
    (is_before ~first:"pinned" ~second:"live" output);
  is_true ~msg:"scroll region resets before live render"
    (is_before ~first:"\027[r" ~second:"live" output)

let test_pinned_static_write_repaints_unchanged_live_view () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:23 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"prompt";
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "static\n";
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"prompt";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"static payload appears before live repaint"
    (is_before ~first:"static" ~second:"prompt" output);
  is_true ~msg:"scroll region resets before unchanged live repaint"
    (is_before ~first:"\027[r" ~second:"prompt" output)

let test_multiline_static_write_uses_scroll_region_when_it_reaches_pin () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:22 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:2 "line-a\nline-b\n";
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  let output = output state in
  equal ~msg:"multiline append reaches pinned live viewport" (pair int int)
    (80, 1) (Matrix.size app);
  is_true ~msg:"pinning append sets bounded scroll region"
    (contains_substring "\027[1;23r" output);
  is_true ~msg:"pinning append resets bounded scroll region"
    (contains_substring "\027[r" output);
  is_true ~msg:"bounded scroll region is active before payload"
    (is_before ~first:"\027[1;23r" ~second:"line-a" output);
  is_true ~msg:"bounded scroll region resets before live repaint"
    (is_before ~first:"\027[r" ~second:"live" output)

let test_unpinned_static_write_settles_without_scroll_region () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:1 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "settle\n";
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  let output = output state in
  equal ~msg:"static output moves live region downward" (pair int int) (80, 22)
    (Matrix.size app);
  is_true ~msg:"static payload appears before live repaint"
    (is_before ~first:"settle" ~second:"live" output);
  is_false ~msg:"settling static output avoids pinned DECSTBM"
    (contains_substring "\027[1;23r" output);
  is_false ~msg:"settling static output does not reset DECSTBM"
    (contains_substring "\027[r" output)

let test_static_write_and_live_repaint_share_sync_frame () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~render_offset:23
      ~min_tui_height:1 ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  set_sync_capable app;
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "pinned\n";
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"synchronized output begins before static payload"
    (is_before ~first:"\027[?2026h" ~second:"pinned" output);
  is_true ~msg:"static payload appears before DECSTBM reset"
    (is_before ~first:"pinned" ~second:"\027[r" output);
  is_true ~msg:"DECSTBM resets before live repaint"
    (is_before ~first:"\027[r" ~second:"live" output);
  is_true ~msg:"live repaint is inside synchronized output"
    (is_before ~first:"live" ~second:"\027[?2026l" output)

let test_pinned_static_write_keeps_live_size () =
  let app, _state =
    make_app ~mode:`Primary ~render_offset:23 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  let before = Matrix.size app in
  Matrix.static_write app ~rows:3 "a\nb\nc\n";
  equal ~msg:"pinned pending output keeps effective live size" (pair int int)
    before
    (Matrix.effective_size app);
  Matrix.submit app;
  equal ~msg:"pinned output keeps live size after submit" (pair int int) before
    (Matrix.size app)

let test_primary_required_rows_apply_before_static_flush () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:10 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "prompt\n";
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app ~primary_required_rows:20;
  let output = output state in
  equal ~msg:"static output settles against the expanded primary region"
    (pair int int) (80, 19) (Matrix.size app);
  is_true ~msg:"primary growth happens before static output is flushed"
    (is_before ~first:"\027[24;1H" ~second:"prompt" output)

let test_full_height_static_write_scrolls_into_scrollback () =
  let app, state =
    make_app ~mode:`Primary ~min_tui_height:24 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  let before = Matrix.size app in
  equal ~msg:"full-height live viewport starts at terminal height"
    (pair int int) (80, 24) before;
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:2 "INSERTED00\nINSERTED01\n";
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"full-height static payload is emitted"
    (contains_substring "INSERTED00" output);
  is_true ~msg:"full-height static rows are scrolled into history"
    (contains_substring "\027[2S" output);
  is_true ~msg:"payload is emitted before the hardware scroll"
    (is_before ~first:"INSERTED01" ~second:"\027[2S" output);
  is_true ~msg:"live viewport repaints after static rows are scrolled"
    (is_before ~first:"\027[2S" ~second:"live" output);
  is_false ~msg:"full-height path does not use bounded DECSTBM"
    (contains_substring "\027[r" output);
  is_false ~msg:"full-height path does not use the old broad-erase preflush"
    (contains_substring "\027[1;1H\027[J" output);
  equal ~msg:"full-height static write keeps live size" (pair int int) before
    (Matrix.size app)

let test_primary_column_one_anchor_preserves_current_row () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~target_fps:None
      ~input_timeout:(Some 0.) ~static_needs_newline:true ()
  in
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "HEADER\n";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"static payload is emitted" (contains_substring "HEADER" output);
  is_true ~msg:"column-one anchor preserves the shell-owned row"
    (contains_substring "\r\nHEADER" output)

let test_preserved_static_write_keeps_live_region () =
  let app, state =
    make_app ~mode:`Primary ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  let before = Matrix.size app in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write ~preserve_live_region:true app ~rows:2
    "PRESERVED00\nPRESERVED01\n";
  equal ~msg:"pending preserved write keeps effective live size" (pair int int)
    before
    (Matrix.effective_size app);
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"preserved rows scroll directly into terminal history"
    (contains_substring "\027[2S" output);
  equal ~msg:"preserved write keeps live region" (pair int int) before
    (Matrix.size app)

let test_preserved_static_write_grows_after_static_rows () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:3 ~min_tui_height:5 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write ~preserve_live_region:true app ~rows:2 "AAAA\nBBBB\n";
  equal ~msg:"pending preserved write projects the grown live size"
    (pair int int) (80, 19)
    (Matrix.effective_size app);
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"content is written, not dropped"
    (contains_substring "AAAA\r\nBBBB" output);
  is_false ~msg:"partial layout does not scroll into history"
    (contains_substring "\027[2S" output);
  equal ~msg:"write grows the static area instead" (pair int int) (80, 19)
    (Matrix.size app)

let test_mixed_static_writes_fall_back_to_growth () =
  let app, state =
    make_app ~mode:`Primary ~min_tui_height:5 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write ~preserve_live_region:true app ~rows:1 "KEEP\n";
  Matrix.static_write app ~rows:1 "GROW\n";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"first write is not overwritten"
    (is_before ~first:"KEEP" ~second:"GROW" output);
  is_true ~msg:"second write lands on the next row"
    (contains_substring "\027[2;1H" output);
  is_false ~msg:"mixed queue does not scroll into history"
    (contains_substring "\027[1S" output)

let test_full_height_consecutive_static_writes_scroll_once_per_row () =
  let app, state =
    make_app ~mode:`Primary ~min_tui_height:24 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "first";
  Matrix.static_write app ~rows:1 "second\n";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"consecutive full-height writes stay ordered"
    (is_before ~first:"first" ~second:"second" output);
  equal ~msg:"full-height writes scroll once per consumed row" int 2
    (count_substring "\027[1S" output)

let test_static_write_ignored_in_alt_mode () =
  let app, state =
    make_app ~mode:`Alt ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.static_write app ~rows:2 "ignored\n";
  equal ~msg:"alt effective size unchanged" (pair int int) (80, 24)
    (Matrix.effective_size app);
  Matrix.submit app;
  is_false ~msg:"alt mode does not emit static text"
    (contains_substring "ignored" (output state))

let test_static_clear_resets_primary_layout () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~render_offset:10
      ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  equal ~msg:"attached primary starts below transcript" (pair int int) (80, 14)
    (Matrix.size app);
  Matrix.static_write app ~rows:1 "pending\n";
  equal ~msg:"pending output affects effective size" (pair int int) (80, 13)
    (Matrix.effective_size app);
  Buffer.clear state.terminal_output;
  Matrix.static_clear app;
  is_true ~msg:"static clear is an immediate terminal reset"
    (contains_substring "\027[H\027[2J" (Buffer.contents state.terminal_output));
  equal ~msg:"static clear restores full primary height" (pair int int) (80, 24)
    (Matrix.size app);
  equal ~msg:"effective size also reset" (pair int int) (80, 24)
    (Matrix.effective_size app);
  Buffer.clear state.output;
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"after-clear";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"next live frame renders after static clear"
    (contains_substring "after-clear" output);
  is_false ~msg:"static clear discards pending static output"
    (contains_substring "pending" output)

let test_static_replace_replays_inside_next_frame () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~render_offset:10
      ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  set_sync_capable app;
  Matrix.submit app;
  Buffer.clear state.output;
  Buffer.clear state.terminal_output;
  Matrix.static_replace app ~rows:2 "replacement-a\nreplacement-b\n";
  equal ~msg:"static replacement is deferred until the next frame" string ""
    (Buffer.contents state.terminal_output);
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live-after";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"replacement clears terminal scrollback"
    (contains_substring "\027[3J" output);
  is_true ~msg:"replacement content precedes the live repaint"
    (is_before ~first:"replacement-b" ~second:"live-after" output);
  is_true ~msg:"replacement and live repaint share synchronized output"
    (is_before ~first:"\027[?2026h" ~second:"replacement-a" output
    && is_before ~first:"live-after" ~second:"\027[?2026l" output)

let test_primary_full_redraw_does_not_erase_past_terminal_bottom () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
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
  is_true ~msg:"bottom live row is rendered"
    (contains_substring "Elapsed: 5.5s" output);
  is_false ~msg:"full redraw must not erase from one row past terminal bottom"
    (contains_substring "\027[25;1H\027[J" output)

let test_primary_full_redraw_erases_stale_rows_below_content () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  let grid = Matrix.grid app in
  Matrix.Grid.resize grid ~width:(Matrix.Grid.width grid) ~height:13;
  Matrix.Grid.draw_text grid ~x:0 ~y:12 ~text:"Press Q to quit";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"content row is rendered"
    (contains_substring "Press Q to quit" output);
  is_true ~msg:"full redraw erases rows below active content"
    (contains_substring "\027[24;1H\027[J" output)

let test_primary_required_rows_to_terminal_height_does_not_erase_past_bottom ()
    =
  let app, state =
    make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  let grid = Matrix.grid app in
  Matrix.Grid.resize grid ~width:(Matrix.Grid.width grid) ~height:24;
  Matrix.Grid.draw_text grid ~x:0 ~y:23 ~text:"bottom";
  Matrix.submit app ~primary_required_rows:24;
  let output = output state in
  equal ~msg:"primary required rows can claim the whole terminal" (pair int int)
    (80, 24) (Matrix.size app);
  is_true ~msg:"bottom row is rendered after primary growth"
    (contains_substring "bottom" output);
  is_false ~msg:"primary growth must not erase from row past terminal bottom"
    (contains_substring "\027[25;1H\027[J" output)

let test_primary_mouse_event_is_offset_into_live_region () =
  let app, _state =
    make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
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
  | [ Matrix.Input.Mouse { Matrix.Input.Mouse.y; kind = Down _; _ } ] ->
      equal ~msg:"mouse y is relative to live viewport" int 2 y
  | _ -> fail "expected one mouse press event"

let test_primary_mouse_event_above_live_region_maps_to_minus_one () =
  let app, _state =
    make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
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
  | [ Matrix.Input.Mouse { Matrix.Input.Mouse.y; kind = Down _; _ } ] ->
      equal ~msg:"mouse y above live region maps outside UI" int (-1) y
  | _ -> fail "expected one mouse press event"

let test_primary_adjusts_all_mouse_like_events () =
  let cases =
    [
      ( "release",
        mouse_release 4 12,
        Matrix.Input.mouse_release 4 2 (Some Matrix.Input.Mouse.Left) );
      ( "motion",
        mouse_motion 4 12,
        Matrix.Input.mouse_drag 4 2 Matrix.Input.Mouse.Left );
      ( "scroll",
        mouse_scroll 4 12,
        Matrix.Input.mouse_scroll 4 2 Matrix.Input.Mouse.Scroll_down );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      let app, _state =
        make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
          ~input_timeout:(Some 0.) ~events:[ input ] ()
      in
      let got = ref None in
      Matrix.run app
        ~on_input:(fun app event ->
          got := Some event;
          Matrix.stop app)
        ~on_render:(fun _app -> ());
      match !got with
      | Some event when Matrix.Input.equal event expected -> ()
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
      is_true ~msg:"frame renders without waiting for input"
        (!second_frame_elapsed < 1.))

let () =
  run "matrix.runtime"
    [
      group "Control"
        [
          test "request_redraw while paused" test_request_redraw_while_paused;
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
          test "startup capability window defers split response"
            test_startup_capability_window_defers_split_response;
          test "resume reanchors primary from bottom cursor"
            test_resume_reanchors_primary_from_bottom_cursor;
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
          test "static write normalizes LF in raw mode"
            test_static_write_normalizes_lf_in_raw_mode;
          test "static write preserves LF outside raw mode"
            test_static_write_preserves_lf_outside_raw_mode;
          test "column-one anchor preserves current row"
            test_primary_column_one_anchor_preserves_current_row;
          test "static write tracks mid-line continuation"
            test_static_write_tracks_mid_line_continuation;
          test "pinned static write uses bounded scroll region"
            test_pinned_static_write_uses_bounded_scroll_region;
          test "pinned static write resets scroll region before live render"
            test_pinned_static_write_resets_scroll_region_before_live_render;
          test "pinned static write repaints unchanged live view"
            test_pinned_static_write_repaints_unchanged_live_view;
          test "multiline static write uses scroll region when it reaches pin"
            test_multiline_static_write_uses_scroll_region_when_it_reaches_pin;
          test "unpinned static write settles without scroll region"
            test_unpinned_static_write_settles_without_scroll_region;
          test "static write and live repaint share sync frame"
            test_static_write_and_live_repaint_share_sync_frame;
          test "pinned static write keeps live size"
            test_pinned_static_write_keeps_live_size;
          test "primary required rows apply before static flush"
            test_primary_required_rows_apply_before_static_flush;
          test "full-height static write scrolls into scrollback"
            test_full_height_static_write_scrolls_into_scrollback;
          test "preserved static write keeps live region"
            test_preserved_static_write_keeps_live_region;
          test "preserved static write grows after static rows"
            test_preserved_static_write_grows_after_static_rows;
          test "mixed static writes fall back to growth"
            test_mixed_static_writes_fall_back_to_growth;
          test "full-height consecutive static writes scroll once per row"
            test_full_height_consecutive_static_writes_scroll_once_per_row;
          test "static_write is ignored in alt mode"
            test_static_write_ignored_in_alt_mode;
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
