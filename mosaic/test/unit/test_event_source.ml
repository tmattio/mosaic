(** Tests for the Event_source module *)

open Engine

let make_test_terminal ~sw input =
  let term, get_output, _close = Tty_eio.create_from_strings ~sw input in
  (term, get_output)

(** Run a function within Eio context - helper to reduce boilerplate *)
let run_eio f = Eio_main.run (fun env -> Eio.Switch.run (fun sw -> f env sw))

(** Helper to create source and read event *)
let read_event_from_string input ~timeout =
  run_eio (fun env sw ->
      let term, _ = make_test_terminal ~sw input in
      let source = Event_source.create ~sw ~env term in
      Event_source.read source ~clock:(Eio.Stdenv.clock env) ~timeout)

let test_single_key_event () =
  match read_event_from_string "a" ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; modifier; _ }) ->
      Alcotest.(check char) "key is 'a'" 'a' (Uchar.to_char c);
      Alcotest.(check bool) "no ctrl" false modifier.ctrl;
      Alcotest.(check bool) "no alt" false modifier.alt;
      Alcotest.(check bool) "no shift" false modifier.shift
  | `Event e -> Alcotest.failf "Unexpected event: %a" Input.pp_event e
  | `Timeout -> Alcotest.fail "Unexpected timeout"

let test_ctrl_c_event () =
  (* This is the critical test for Ctrl+C *)
  match read_event_from_string "\x03" ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; modifier = { ctrl; alt; shift; _ }; _ })
    ->
      Alcotest.(check char) "key is 'C'" 'C' (Uchar.to_char c);
      Alcotest.(check bool) "ctrl is pressed" true ctrl;
      Alcotest.(check bool) "alt not pressed" false alt;
      Alcotest.(check bool) "shift not pressed" false shift
  | `Event e -> Alcotest.failf "Unexpected event: %a" Input.pp_event e
  | `Timeout -> Alcotest.fail "Unexpected timeout"

let test_event_queue () =
  (* Test multiple events in single read *)
  run_eio @@ fun env sw ->
  let term, _ = make_test_terminal ~sw "abc" in
  let source = Event_source.create ~sw ~env term in
  let clock = Eio.Stdenv.clock env in

  (* First read should get 'a' *)
  (match Event_source.read source ~clock ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; _ }) ->
      Alcotest.(check char) "first key is 'a'" 'a' (Uchar.to_char c)
  | _ -> Alcotest.fail "Expected key event for 'a'");

  (* Second read should get 'b' *)
  (match Event_source.read source ~clock ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; _ }) ->
      Alcotest.(check char) "second key is 'b'" 'b' (Uchar.to_char c)
  | _ -> Alcotest.fail "Expected key event for 'b'");

  (* Third read should get 'c' *)
  match Event_source.read source ~clock ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; _ }) ->
      Alcotest.(check char) "third key is 'c'" 'c' (Uchar.to_char c)
  | _ -> Alcotest.fail "Expected key event for 'c'"

let test_timeout_behavior () =
  (* Test with empty input *)
  match read_event_from_string "" ~timeout:(Some 0.01) with
  | `Timeout -> () (* Expected *)
  | `Event e -> Alcotest.failf "Unexpected event: %a" Input.pp_event e

let test_eof_handling () =
  (* Test EOF after some input *)
  run_eio @@ fun env sw ->
  let term, _ = make_test_terminal ~sw "a" in
  let source = Event_source.create ~sw ~env term in
  let clock = Eio.Stdenv.clock env in

  (* Read the 'a' *)
  (match Event_source.read source ~clock ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; _ }) ->
      Alcotest.(check char) "key is 'a'" 'a' (Uchar.to_char c)
  | _ -> Alcotest.fail "Expected key event");

  (* Next read should be EOF or timeout *)
  match Event_source.read source ~clock ~timeout:(Some 0.01) with
  | `Timeout -> () (* Acceptable for timeout *)
  | `Event e ->
      Alcotest.failf "Unexpected event after input: %a" Input.pp_event e

let test_escape_sequence () =
  (* Test arrow key *)
  match read_event_from_string "\x1b[A" ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Up; modifier; _ }) ->
      Alcotest.(check bool)
        "no modifiers" false
        (modifier.ctrl || modifier.alt || modifier.shift)
  | `Event e -> Alcotest.failf "Unexpected event: %a" Input.pp_event e
  | `Timeout -> Alcotest.fail "Unexpected timeout"

let test_mouse_event () =
  (* Test SGR mouse click *)
  match read_event_from_string "\x1b[<0;5;10M" ~timeout:(Some 0.1) with
  | `Event (Input.Mouse (Button_press (x, y, Left, _))) ->
      Alcotest.(check int) "x coordinate" 4 x;
      Alcotest.(check int) "y coordinate" 9 y
  | `Event e -> Alcotest.failf "Unexpected event: %a" Input.pp_event e
  | `Timeout -> Alcotest.fail "Unexpected timeout"

let test_paste_events () =
  (* Test bracketed paste *)
  run_eio @@ fun env sw ->
  let term, _ = make_test_terminal ~sw "\x1b[200~test\x1b[201~" in
  let source = Event_source.create ~sw ~env term in
  let clock = Eio.Stdenv.clock env in

  (* Should get paste start *)
  (match Event_source.read source ~clock ~timeout:(Some 0.1) with
  | `Event Input.Paste_start -> ()
  | `Event e -> Alcotest.failf "Expected paste start, got: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected paste start event");

  (* Should get paste content *)
  (match Event_source.read source ~clock ~timeout:(Some 0.1) with
  | `Event (Input.Paste content) ->
      Alcotest.(check string) "paste content" "test" content
  | `Event e ->
      Alcotest.failf "Expected paste content, got: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected paste content event");

  (* Should get paste end *)
  match Event_source.read source ~clock ~timeout:(Some 0.1) with
  | `Event Input.Paste_end -> ()
  | `Event e -> Alcotest.failf "Expected paste end, got: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected paste end event"

(** Test kitty keyboard mode events *)
let test_kitty_keyboard_events () =
  (* Test enhanced key reporting with modifiers *)
  match read_event_from_string "\x1b[97;5u" ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; modifier = { ctrl; _ }; _ }) ->
      Alcotest.(check char) "key is 'a'" 'a' (Uchar.to_char c);
      Alcotest.(check bool) "ctrl is pressed" true ctrl
  | `Event e -> Alcotest.failf "Unexpected event: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected key event"

(** Test resize events *)
let test_resize_event () =
  (* Test terminal resize sequence *)
  match read_event_from_string "\x1b[8;24;80t" ~timeout:(Some 0.1) with
  | `Event (Input.Resize (width, height)) ->
      Alcotest.(check int) "width" 80 width;
      Alcotest.(check int) "height" 24 height
  | `Event e -> Alcotest.failf "Expected resize, got: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected resize event"

(** Test focus/blur events *)
let test_focus_blur_events () =
  (* Test focus in *)
  (match read_event_from_string "\x1b[I" ~timeout:(Some 0.1) with
  | `Event Input.Focus -> ()
  | `Event e -> Alcotest.failf "Expected focus, got: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected focus event");

  (* Test focus out *)
  match read_event_from_string "\x1b[O" ~timeout:(Some 0.1) with
  | `Event Input.Blur -> ()
  | `Event e -> Alcotest.failf "Expected blur, got: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected blur event"

(** Test edge cases *)
let test_edge_cases () =
  (* Empty input immediate timeout *)
  (match read_event_from_string "" ~timeout:(Some 0.0) with
  | `Timeout -> ()
  | `Event e -> Alcotest.failf "Unexpected event on empty: %a" Input.pp_event e);

  (* Invalid escape sequence *)
  (match read_event_from_string "\x1b[999999Z" ~timeout:(Some 0.1) with
  | `Event _ -> () (* Some event expected, exact type depends on parser *)
  | `Timeout -> ());

  (* Partial escape at EOF *)
  match read_event_from_string "\x1b[" ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Escape; _ }) | `Timeout -> ()
  | `Event e ->
      Alcotest.failf "Unexpected event for partial escape: %a" Input.pp_event e

(** Test rapid event processing *)
let test_rapid_events () =
  run_eio @@ fun env sw ->
  let input =
    String.concat ""
      (List.init 100 (fun i -> String.make 1 (Char.chr (65 + (i mod 26)))))
  in
  let term, _ = make_test_terminal ~sw input in
  let source = Event_source.create ~sw ~env term in
  let clock = Eio.Stdenv.clock env in

  (* Read all events *)
  let rec read_all count =
    if count > 110 then Alcotest.fail "Too many events"
    else
      match Event_source.read source ~clock ~timeout:(Some 0.001) with
      | `Event (Input.Key _) -> read_all (count + 1)
      | `Timeout -> count
      | `Event e -> Alcotest.failf "Unexpected event type: %a" Input.pp_event e
  in
  let count = read_all 0 in
  Alcotest.(check int) "read all rapid events" 100 count

(** Test large paste *)
let test_large_paste () =
  run_eio @@ fun env sw ->
  let paste_content = String.make 1000 'X' in
  let input = Printf.sprintf "\x1b[200~%s\x1b[201~" paste_content in
  let term, _ = make_test_terminal ~sw input in
  let source = Event_source.create ~sw ~env term in
  let clock = Eio.Stdenv.clock env in

  (* Should get paste start *)
  (match Event_source.read source ~clock ~timeout:(Some 0.1) with
  | `Event Input.Paste_start -> ()
  | _ -> Alcotest.fail "Expected paste start");

  (* Should get paste content *)
  (match Event_source.read source ~clock ~timeout:(Some 0.1) with
  | `Event (Input.Paste content) ->
      Alcotest.(check string) "paste content" paste_content content;
      Alcotest.(check int) "paste content length" 1000 (String.length content)
  | `Event e ->
      Alcotest.failf "Expected paste content, got: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected paste content");

  (* Should get paste end *)
  match Event_source.read source ~clock ~timeout:(Some 0.1) with
  | `Event Input.Paste_end -> ()
  | `Event e -> Alcotest.failf "Expected paste end, got: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected paste end"

(** Test mouse modes *)
let test_mouse_modes () =
  (* Test X10 mouse mode - only button press *)
  (match read_event_from_string "\x1b[M !!" ~timeout:(Some 0.1) with
  | `Event (Input.Mouse _) -> ()
  | `Event e -> Alcotest.failf "Expected mouse event, got: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected mouse event");

  (* Test SGR mouse with release *)
  (match read_event_from_string "\x1b[<0;5;10m" ~timeout:(Some 0.1) with
  | `Event (Input.Mouse (Button_release (x, y, Left, _))) ->
      Alcotest.(check int) "x coordinate" 4 x;
      Alcotest.(check int) "y coordinate" 9 y
  | `Event e ->
      Alcotest.failf "Expected mouse release, got: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected mouse release");

  (* Test mouse motion *)
  match read_event_from_string "\x1b[<32;5;10M" ~timeout:(Some 0.1) with
  | `Event (Input.Mouse (Motion _)) -> ()
  | `Event e -> Alcotest.failf "Expected mouse motion, got: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected mouse motion"

(** Test special key combinations *)
let test_special_keys () =
  (* Test function keys *)
  let test_cases =
    [
      ("\x1b[11~", Input.F 1);
      ("\x1b[12~", Input.F 2);
      ("\x1b[13~", Input.F 3);
      ("\x1b[14~", Input.F 4);
      ("\x1b[15~", Input.F 5);
      ("\x1b[17~", Input.F 6);
      ("\x1b[18~", Input.F 7);
      ("\x1b[19~", Input.F 8);
      ("\x1b[20~", Input.F 9);
      ("\x1b[21~", Input.F 10);
      ("\x1b[23~", Input.F 11);
      ("\x1b[24~", Input.F 12);
    ]
  in

  List.iter
    (fun (input, expected_key) ->
      match read_event_from_string input ~timeout:(Some 0.1) with
      | `Event (Input.Key { key; _ }) when key = expected_key -> ()
      | `Event e -> Alcotest.failf "Expected F key, got: %a" Input.pp_event e
      | _ -> Alcotest.fail "Expected function key event")
    test_cases

(** Test alt key combinations *)
let test_alt_combinations () =
  (* Alt+a *)
  match read_event_from_string "\x1ba" ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; modifier = { alt; _ }; _ }) ->
      Alcotest.(check char) "key is 'a'" 'a' (Uchar.to_char c);
      Alcotest.(check bool) "alt is pressed" true alt
  | `Event e -> Alcotest.failf "Unexpected event: %a" Input.pp_event e
  | _ -> Alcotest.fail "Expected alt+a event"

(** Test input buffer handling *)
let test_input_buffer () =
  (* Test that partial sequences timeout correctly *)
  match read_event_from_string "\x1b[" ~timeout:(Some 0.01) with
  | `Timeout -> () (* Expected - incomplete sequence *)
  | `Event (Input.Key { key = Escape; _ }) -> () (* Also acceptable *)
  | `Event e ->
      Alcotest.failf "Unexpected event on partial: %a" Input.pp_event e

let tests =
  [
    ("single key event", `Quick, test_single_key_event);
    ("ctrl+c event", `Quick, test_ctrl_c_event);
    ("event queue", `Quick, test_event_queue);
    ("timeout behavior", `Quick, test_timeout_behavior);
    ("eof handling", `Quick, test_eof_handling);
    ("escape sequence", `Quick, test_escape_sequence);
    ("mouse event", `Quick, test_mouse_event);
    ("paste events", `Quick, test_paste_events);
    ("kitty keyboard events", `Quick, test_kitty_keyboard_events);
    ("resize event", `Quick, test_resize_event);
    ("focus/blur events", `Quick, test_focus_blur_events);
    ("edge cases", `Quick, test_edge_cases);
    ("rapid events", `Quick, test_rapid_events);
    ("large paste", `Quick, test_large_paste);
    ("mouse modes", `Quick, test_mouse_modes);
    ("special keys", `Quick, test_special_keys);
    ("alt combinations", `Quick, test_alt_combinations);
    ("input buffer", `Quick, test_input_buffer);
  ]

let () = Alcotest.run "Event_source" [ ("reading", tests) ]
