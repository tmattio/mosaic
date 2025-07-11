(** Tests for the Event_source module *)

open Test_utils

(** Helper to create source and read event *)
let read_event_from_string input ~timeout =
  run_eio (fun env sw ->
      let term, _ = make_test_terminal input in
      let source = Event_source.create term in
      Event_source.read source ~sw ~clock:(Eio.Stdenv.clock env) ~timeout)

let test_single_key_event () =
  match read_event_from_string "a" ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; modifier }) ->
      Alcotest.(check char) "key is 'a'" 'a' (Uchar.to_char c);
      Alcotest.(check bool) "no ctrl" false modifier.ctrl;
      Alcotest.(check bool) "no alt" false modifier.alt;
      Alcotest.(check bool) "no shift" false modifier.shift
  | `Event e -> Alcotest.failf "Unexpected event: %s" (show_event e)
  | `Timeout -> Alcotest.fail "Unexpected timeout"
  | `Eof -> Alcotest.fail "Unexpected EOF"

let test_ctrl_c_event () =
  (* This is the critical test for Ctrl+C *)
  match read_event_from_string "\x03" ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; modifier = { ctrl; alt; shift } }) ->
      Alcotest.(check char) "key is 'C'" 'C' (Uchar.to_char c);
      Alcotest.(check bool) "ctrl is pressed" true ctrl;
      Alcotest.(check bool) "alt not pressed" false alt;
      Alcotest.(check bool) "shift not pressed" false shift
  | `Event e -> Alcotest.failf "Unexpected event: %s" (show_event e)
  | `Timeout -> Alcotest.fail "Unexpected timeout"
  | `Eof -> Alcotest.fail "Unexpected EOF"

let test_event_queue () =
  (* Test multiple events in single read *)
  run_eio @@ fun env sw ->
  let term, _ = make_test_terminal "abc" in
  let source = Event_source.create term in
  let clock = Eio.Stdenv.clock env in

  (* First read should get 'a' *)
  (match Event_source.read source ~sw ~clock ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; _ }) ->
      Alcotest.(check char) "first key is 'a'" 'a' (Uchar.to_char c)
  | _ -> Alcotest.fail "Expected key event for 'a'");

  (* Second read should get 'b' *)
  (match Event_source.read source ~sw ~clock ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; _ }) ->
      Alcotest.(check char) "second key is 'b'" 'b' (Uchar.to_char c)
  | _ -> Alcotest.fail "Expected key event for 'b'");

  (* Third read should get 'c' *)
  match Event_source.read source ~sw ~clock ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; _ }) ->
      Alcotest.(check char) "third key is 'c'" 'c' (Uchar.to_char c)
  | _ -> Alcotest.fail "Expected key event for 'c'"

let test_timeout_behavior () =
  (* Test with empty input *)
  match read_event_from_string "" ~timeout:(Some 0.01) with
  | `Timeout -> () (* Expected *)
  | `Event e -> Alcotest.failf "Unexpected event: %s" (show_event e)
  | `Eof -> () (* Also acceptable for empty input *)

let test_eof_handling () =
  (* Test EOF after some input *)
  run_eio @@ fun env sw ->
  let term, _ = make_test_terminal "a" in
  let source = Event_source.create term in
  let clock = Eio.Stdenv.clock env in

  (* Read the 'a' *)
  (match Event_source.read source ~sw ~clock ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Char c; _ }) ->
      Alcotest.(check char) "key is 'a'" 'a' (Uchar.to_char c)
  | _ -> Alcotest.fail "Expected key event");

  (* Next read should be EOF or timeout *)
  match Event_source.read source ~sw ~clock ~timeout:(Some 0.01) with
  | `Eof | `Timeout -> () (* Both are acceptable *)
  | `Event e -> Alcotest.failf "Unexpected event after input: %s" (show_event e)

let test_escape_sequence () =
  (* Test arrow key *)
  match read_event_from_string "\x1b[A" ~timeout:(Some 0.1) with
  | `Event (Input.Key { key = Up; modifier }) ->
      Alcotest.(check bool)
        "no modifiers" false
        (modifier.ctrl || modifier.alt || modifier.shift)
  | `Event e -> Alcotest.failf "Unexpected event: %s" (show_event e)
  | `Timeout -> Alcotest.fail "Unexpected timeout"
  | `Eof -> Alcotest.fail "Unexpected EOF"

let test_mouse_event () =
  (* Test SGR mouse click *)
  match read_event_from_string "\x1b[<0;5;10M" ~timeout:(Some 0.1) with
  | `Event (Input.Mouse (Press (x, y, Left, _))) ->
      Alcotest.(check int) "x coordinate" 4 x;
      Alcotest.(check int) "y coordinate" 9 y
  | `Event e -> Alcotest.failf "Unexpected event: %s" (show_event e)
  | `Timeout -> Alcotest.fail "Unexpected timeout"
  | `Eof -> Alcotest.fail "Unexpected EOF"

let test_paste_events () =
  (* Test bracketed paste *)
  run_eio @@ fun env sw ->
  let term, _ = make_test_terminal "\x1b[200~test\x1b[201~" in
  let source = Event_source.create term in
  let clock = Eio.Stdenv.clock env in

  (* Should get paste start *)
  (match Event_source.read source ~sw ~clock ~timeout:(Some 0.1) with
  | `Event Input.Paste_start -> ()
  | `Event e -> Alcotest.failf "Expected paste start, got: %s" (show_event e)
  | _ -> Alcotest.fail "Expected paste start event");

  (* Should get characters *)
  let rec read_until_paste_end count =
    if count > 10 then Alcotest.fail "Too many events"
    else
      match Event_source.read source ~sw ~clock ~timeout:(Some 0.1) with
      | `Event Input.Paste_end -> ()
      | `Event (Input.Key _) -> read_until_paste_end (count + 1)
      | `Event e -> Alcotest.failf "Unexpected event: %s" (show_event e)
      | _ -> Alcotest.fail "Unexpected non-event"
  in
  read_until_paste_end 0

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
  ]

let () = Alcotest.run "Event_source" [ ("reading", tests) ]
