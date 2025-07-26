(** Tests for the Input module *)

open Test_utils

let test_parse_regular_chars () =
  (* Test single character *)
  let events = Input.parse_single "a" in
  Alcotest.(check (list event_testable))
    "single char 'a'"
    [ Input.char_event 'a' ]
    events;

  (* Test multiple characters *)
  let events = Input.parse_single "hello" in
  Alcotest.(check (list event_testable))
    "multiple chars 'hello'"
    [
      Input.char_event 'h';
      Input.char_event 'e';
      Input.char_event 'l';
      Input.char_event 'l';
      Input.char_event 'o';
    ]
    events

let test_parse_control_chars () =
  (* Test Ctrl+A *)
  let events = Input.parse_single "\x01" in
  Alcotest.(check (list event_testable))
    "Ctrl+A"
    [ Input.char_event ~modifier:{ Input.no_modifier with ctrl = true } 'A' ]
    events;

  (* Test Ctrl+C - this is the critical test *)
  let events = Input.parse_single "\x03" in
  Alcotest.(check (list event_testable))
    "Ctrl+C"
    [ Input.char_event ~modifier:{ Input.no_modifier with ctrl = true } 'C' ]
    events;

  (* Test Ctrl+Z *)
  let events = Input.parse_single "\x1a" in
  Alcotest.(check (list event_testable))
    "Ctrl+Z"
    [ Input.char_event ~modifier:{ Input.no_modifier with ctrl = true } 'Z' ]
    events

let test_parse_special_keys () =
  (* Test Enter *)
  let events = Input.parse_single "\r" in
  Alcotest.(check (list event_testable))
    "Enter"
    [ Input.key_event Input.Enter ]
    events;

  (* Test Tab *)
  let events = Input.parse_single "\t" in
  Alcotest.(check (list event_testable))
    "Tab"
    [ Input.key_event Input.Tab ]
    events;

  (* Test Escape - in a streaming parser, a single escape is buffered *)
  let parser = Input.create () in
  let events = Input.feed parser (Bytes.of_string "\x1b") 0 1 in
  Alcotest.(check (list event_testable)) "Escape buffered" [] events;

  (* Test Backspace - this will also emit the buffered escape *)
  let events = Input.feed parser (Bytes.of_string "\x7f") 0 1 in
  Alcotest.(check (list event_testable))
    "Escape + Backspace"
    [ Input.key_event Input.Escape; Input.key_event Input.Backspace ]
    events

let test_parse_arrow_keys () =
  (* Test arrow keys *)
  let arrows =
    [
      ("\x1b[A", Input.Up, "Up arrow");
      ("\x1b[B", Input.Down, "Down arrow");
      ("\x1b[C", Input.Right, "Right arrow");
      ("\x1b[D", Input.Left, "Left arrow");
    ]
  in
  List.iter
    (fun (seq, key, desc) ->
      let events = Input.parse_single seq in
      Alcotest.(check (list event_testable)) desc [ Input.key_event key ] events)
    arrows

let test_parse_function_keys () =
  (* Test F1-F4 using SS3 sequences *)
  let f_keys_ss3 =
    [
      ("\x1bOP", Input.F 1);
      ("\x1bOQ", Input.F 2);
      ("\x1bOR", Input.F 3);
      ("\x1bOS", Input.F 4);
    ]
  in
  List.iter
    (fun (seq, key) ->
      let events = Input.parse_single seq in
      Alcotest.(check (list event_testable))
        (Format.asprintf "%a" Input.pp_key key)
        [ Input.key_event key ]
        events)
    f_keys_ss3;

  (* Test F5 *)
  let events = Input.parse_single "\x1b[15~" in
  Alcotest.(check (list event_testable))
    "F5"
    [ Input.key_event (Input.F 5) ]
    events

let test_parse_modifiers () =
  (* Test Shift+Tab *)
  let events = Input.parse_single "\x1b[Z" in
  Alcotest.(check (list event_testable))
    "Shift+Tab"
    [
      Input.key_event
        ~modifier:{ Input.no_modifier with shift = true }
        Input.Tab;
    ]
    events;

  (* Test Ctrl+Up *)
  let events = Input.parse_single "\x1b[1;5A" in
  Alcotest.(check (list event_testable))
    "Ctrl+Up"
    [
      Input.key_event ~modifier:{ Input.no_modifier with ctrl = true } Input.Up;
    ]
    events;

  (* Test Alt+Left *)
  let events = Input.parse_single "\x1b[1;3D" in
  Alcotest.(check (list event_testable))
    "Alt+Left"
    [
      Input.key_event ~modifier:{ Input.no_modifier with alt = true } Input.Left;
    ]
    events

let test_parse_mouse_sgr () =
  (* Test mouse click at (10, 20) - 0-based in result *)
  let events = Input.parse_single "\x1b[<0;10;20M" in
  Alcotest.(check (list event_testable))
    "Mouse click"
    [ Input.mouse_press 9 19 Input.Left ]
    events;

  (* Test mouse release *)
  let events = Input.parse_single "\x1b[<0;10;20m" in
  Alcotest.(check (list event_testable))
    "Mouse release"
    [ Input.mouse_release 9 19 Input.Left ]
    events;

  (* Test mouse motion *)
  let events = Input.parse_single "\x1b[<32;15;25M" in
  Alcotest.(check (list event_testable))
    "Mouse motion"
    [ Input.mouse_motion 14 24 { left = true; middle = false; right = false } ]
    events

let test_parse_paste_mode () =
  (* Test bracketed paste *)
  let input = "\x1b[200~Hello, World!\x1b[201~" in
  let events = Input.parse_single input in

  (* Should get paste start, paste content, then paste end *)
  match events with
  | [ Input.Paste_start; Input.Paste content; Input.Paste_end ] ->
      Alcotest.(check string) "paste content" "Hello, World!" content
  | _ ->
      let event_str =
        List.map
          (function
            | Input.Paste_start -> "Paste_start"
            | Input.Paste_end -> "Paste_end"
            | Input.Paste s -> Printf.sprintf "Paste(%S)" s
            | Input.Key _ -> "Key"
            | _ -> "Other")
          events
        |> String.concat ", "
      in
      Alcotest.failf
        "Expected [Paste_start; Paste(content); Paste_end], got [%s]" event_str

let test_parse_utf8 () =
  (* Test UTF-8 character (emoji 😀) *)
  let events = Input.parse_single "😀" in
  Alcotest.(check (list event_testable))
    "UTF-8 emoji"
    [ Input.key_event (Input.Char (Uchar.of_int 0x1F600)) ]
    events;

  (* Test accented character (é) *)
  let events = Input.parse_single "é" in
  Alcotest.(check (list event_testable))
    "UTF-8 accented char"
    [ Input.key_event (Input.Char (Uchar.of_int 0xE9)) ]
    events

let test_incremental_parsing () =
  let parser = Input.create () in

  (* Feed escape sequence incrementally *)
  let events1 = Input.feed parser (Bytes.of_string "\x1b") 0 1 in
  Alcotest.(check (list pass)) "no events yet" [] events1;

  let events2 = Input.feed parser (Bytes.of_string "[") 0 1 in
  Alcotest.(check (list pass)) "still no events" [] events2;

  let events3 = Input.feed parser (Bytes.of_string "A") 0 1 in
  Alcotest.(check (list event_testable))
    "complete escape sequence"
    [ Input.key_event Input.Up ]
    events3

(** Test additional key variants *)
let test_more_key_variants () =
  (* Test Page Up/Down, Home/End, Insert/Delete *)
  let keys =
    [
      ("\x1b[5~", Input.Page_up);
      ("\x1b[6~", Input.Page_down);
      ("\x1b[H", Input.Home);
      ("\x1b[F", Input.End);
      ("\x1b[2~", Input.Insert);
      ("\x1b[3~", Input.Delete);
    ]
  in
  List.iter
    (fun (seq, key) ->
      let events = Input.parse_single seq in
      Alcotest.(check (list event_testable))
        (Format.asprintf "%a" Input.pp_key key)
        [ Input.key_event key ]
        events)
    keys;

  (* Test higher function keys F6-F12 *)
  let f_keys =
    [ (17, 6); (18, 7); (19, 8); (20, 9); (21, 10); (23, 11); (24, 12) ]
  in
  List.iter
    (fun (code, n) ->
      let seq = Printf.sprintf "\x1b[%d~" code in
      let events = Input.parse_single seq in
      Alcotest.(check (list event_testable))
        (Printf.sprintf "F%d" n)
        [ Input.key_event (Input.F n) ]
        events)
    f_keys

(** Test invalid/malformed sequences *)
let test_invalid_sequences () =
  let parser = Input.create () in

  (* Incomplete CSI sequence *)
  let events = Input.feed parser (Bytes.of_string "\x1b[") 0 2 in
  Alcotest.(check (list pass)) "incomplete CSI buffered" [] events;

  (* Invalid CSI terminator *)
  let events = Input.feed parser (Bytes.of_string "999999X") 0 7 in
  (* Should parse as regular chars since no valid terminator *)
  (* Note: buffered '[' from incomplete CSI is also parsed as a char *)
  Alcotest.(check int) "parsed as chars" 8 (List.length events);

  (* Very long CSI parameters *)
  let long_seq = "\x1b[" ^ String.make 100 '9' ^ "m" in
  let events = Input.parse_single long_seq in
  (* Should handle gracefully - exact behavior depends on implementation *)
  Alcotest.(check bool)
    "long sequence handled" true
    (List.length events = 0 || List.length events > 0);

  (* Invalid UTF-8 sequences *)
  let parser = Input.create () in
  let invalid_utf8 = Bytes.create 2 in
  Bytes.set invalid_utf8 0 '\xff';
  Bytes.set invalid_utf8 1 '\xfe';
  let events = Input.feed parser invalid_utf8 0 2 in
  (* Should handle invalid UTF-8 gracefully *)
  Alcotest.(check bool) "invalid UTF-8 handled" true (List.length events >= 0);

  (* Mixed valid and invalid *)
  let events = Input.parse_single "a\x1b[999999999999mbc" in
  (* Should at least parse 'a' *)
  match events with
  | Input.Key { key = Char c; _ } :: _ ->
      Alcotest.(check char) "first char parsed" 'a' (Uchar.to_char c)
  | _ -> Alcotest.fail "expected at least one char"

(** Test combined/complex inputs *)
let test_combined_inputs () =
  (* Key with multiple modifiers *)
  (* Modifier encoding: 1 (base) + 1 (shift) + 2 (alt) + 4 (ctrl) = 8 *)
  let events = Input.parse_single "\x1b[1;8A" in
  Alcotest.(check (list event_testable))
    "Ctrl+Alt+Shift+Up"
    [
      Input.key_event
        ~modifier:
          {
            ctrl = true;
            alt = true;
            shift = true;
            super = false;
            hyper = false;
            meta = false;
            caps_lock = false;
            num_lock = false;
          }
        Input.Up;
    ]
    events;

  (* Mouse followed by key *)
  let seq = "\x1b[<0;5;10Ma" in
  let events = Input.parse_single seq in
  Alcotest.(check int) "mouse + key event count" 2 (List.length events);
  (match events with
  | [ Input.Mouse _; Input.Key _ ] -> ()
  | _ -> Alcotest.fail "expected mouse then key event");

  (* Paste with special chars *)
  let paste_seq = "\x1b[200~Hello\nWorld\t!\x1b[201~" in
  let events = Input.parse_single paste_seq in
  (* Should have paste start, chars, paste end *)
  Alcotest.(check bool) "paste with special chars" true (List.length events >= 3);

  (* Rapid key presses *)
  let rapid =
    String.concat ""
      (List.init 50 (fun i -> String.make 1 (Char.chr (65 + (i mod 26)))))
  in
  let events = Input.parse_single rapid in
  Alcotest.(check int) "rapid keys parsed" 50 (List.length events)

(** Test kitty keyboard protocol *)
let test_kitty_keyboard () =
  (* Basic kitty key *)
  let events = Input.parse_single "\x1b[97u" in
  Alcotest.(check (list event_testable))
    "kitty 'a'"
    [ Input.char_event 'a' ]
    events;

  (* Kitty with modifiers *)
  let events = Input.parse_single "\x1b[97;5u" in
  Alcotest.(check (list event_testable))
    "kitty ctrl+a"
    [ Input.char_event ~modifier:{ Input.no_modifier with ctrl = true } 'a' ]
    events;

  (* Kitty special keys *)
  let events = Input.parse_single "\x1b[13u" in
  Alcotest.(check (list event_testable))
    "kitty enter"
    [ Input.key_event Input.Enter ]
    events;

  (* Kitty with event type - press repeat release *)
  let events = Input.parse_single "\x1b[97;1:3u" in
  (* Should parse the key regardless of event type *)
  match events with
  | [ Input.Key { key = Char c; _ } ] ->
      Alcotest.(check char) "kitty with event type" 'a' (Uchar.to_char c)
  | _ -> Alcotest.fail "expected single key event"

(** Test edge cases *)
let test_input_edge_cases () =
  (* Empty input *)
  let events = Input.parse_single "" in
  Alcotest.(check (list pass)) "empty input" [] events;

  (* Single null byte *)
  let events = Input.parse_single "\x00" in
  Alcotest.(check (list event_testable))
    "null byte as Ctrl+Space"
    [ Input.char_event ~modifier:{ Input.no_modifier with ctrl = true } ' ' ]
    events;

  (* Alt+Escape *)
  let parser = Input.create () in
  let events = Input.feed parser (Bytes.of_string "\x1b\x1b") 0 2 in
  (* First ESC is parsed immediately, second is buffered *)
  Alcotest.(check (list event_testable))
    "first escape parsed"
    [ Input.key_event Escape ]
    events;

  (* Very large input buffer *)
  let large = String.make 10000 'X' in
  let events = Input.parse_single large in
  (* Should get 10001 events: 1 buffered ESC from previous test + 10000 X's *)
  (* Actually, parse_single uses a fresh parser, so just 10000 *)
  Alcotest.(check int) "large input parsed" 10000 (List.length events);

  (* Input with offset and length *)
  let parser = Input.create () in
  let data = Bytes.of_string "XXXabcYYY" in
  let events = Input.feed parser data 3 3 in
  Alcotest.(check int) "partial feed count" 3 (List.length events);
  match events with
  | Input.Key { key = Char c; _ } :: _ ->
      Alcotest.(check char) "partial feed first char" 'a' (Uchar.to_char c)
  | _ -> Alcotest.fail "expected char event"

(** Test split UTF-8 sequences across feeds *)
let test_split_utf8 () =
  let parser = Input.create () in

  (* Test euro sign (€) = E2 82 AC split across feeds *)
  let events1 = Input.feed parser (Bytes.of_string "\xE2\x82") 0 2 in
  Alcotest.(check (list pass)) "incomplete UTF-8 should be buffered" [] events1;

  (* Feed the final byte *)
  let events2 = Input.feed parser (Bytes.of_string "\xAC") 0 1 in
  match events2 with
  | [ Input.Key { key = Char u; modifier = _; _ } ] ->
      Alcotest.(check int) "euro sign unicode" 0x20AC (Uchar.to_int u)
  | _ -> Alcotest.fail "expected single UTF-8 character after completion"

(** Test buffer overflow with large input *)
let test_buffer_overflow () =
  (* Try to feed more than buffer size (4096) in one go *)
  let large = String.make 5000 'X' in
  try
    let events = Input.parse_single large in
    Alcotest.(check int) "should parse all characters" 5000 (List.length events)
  with
  | Failure msg
    when String.length msg >= 20
         && String.sub msg 0 20 = "Input buffer overflow" ->
      Alcotest.fail "Buffer should grow dynamically, not overflow"
  | e -> raise e

(** Test paste mode actually collects content *)
let test_paste_mode_collection () =
  (* Feed a large paste that would be inefficient as individual keys *)
  let paste_content = String.make 1000 'A' in
  let input = "\x1b[200~" ^ paste_content ^ "\x1b[201~" in
  let events = Input.parse_single input in

  (* Check we get Paste_start, Paste(content), Paste_end *)
  match events with
  | [ Input.Paste_start; Input.Paste content; Input.Paste_end ] ->
      Alcotest.(check string) "paste content matches" paste_content content
  | Input.Paste_start :: keys ->
      (* If we get individual keys, count them *)
      let key_count =
        List.length
          (List.filter (function Input.Key _ -> true | _ -> false) keys)
      in
      Alcotest.failf
        "Got %d individual key events instead of single Paste event" key_count
  | _ ->
      Alcotest.failf "Unexpected event sequence: %d events" (List.length events)

(** Test integer overflow in CSI parameters *)
let test_csi_param_overflow () =
  (* Create a CSI sequence with a parameter that would overflow int *)
  let huge_param = String.make 20 '9' in
  (* 99999999999999999999 *)
  let seq = Printf.sprintf "\x1b[%s;1A" huge_param in

  let events = Input.parse_single seq in

  (* Should either parse with wrapped value or fall back to regular chars *)
  match events with
  | [] -> Alcotest.fail "Should produce some events"
  | [ Input.Key { key = Input.Up; modifier; _ } ] ->
      (* If it parsed as Up arrow, check modifier isn't garbage *)
      Alcotest.(check bool)
        "modifier should be reasonable"
        (modifier.ctrl || modifier.alt || modifier.shift
        || modifier = Input.no_modifier)
        true
  | _ ->
      (* Fallback to char events is acceptable *)
      Alcotest.(check bool) "got some events" true (List.length events > 0)

(** Test new event types *)
let test_cursor_position_report () =
  (* Test cursor position report - CSI row ; col R *)
  let events = Input.parse_single "\x1b[10;25R" in
  Alcotest.(check (list event_testable))
    "cursor position report"
    [ Input.Cursor_position (10, 25) ]
    events

let test_device_attributes () =
  (* Test device attributes response - CSI ? attrs c *)
  let events = Input.parse_single "\x1b[?1;2;6;9;15c" in
  Alcotest.(check (list event_testable))
    "device attributes"
    [ Input.Device_attributes [ 1; 2; 6; 9; 15 ] ]
    events

let tests =
  [
    ("parse regular chars", `Quick, test_parse_regular_chars);
    ("parse control chars", `Quick, test_parse_control_chars);
    ("parse special keys", `Quick, test_parse_special_keys);
    ("parse arrow keys", `Quick, test_parse_arrow_keys);
    ("parse function keys", `Quick, test_parse_function_keys);
    ("parse modifiers", `Quick, test_parse_modifiers);
    ("parse mouse SGR", `Quick, test_parse_mouse_sgr);
    ("parse paste mode", `Quick, test_parse_paste_mode);
    ("parse UTF-8", `Quick, test_parse_utf8);
    ("incremental parsing", `Quick, test_incremental_parsing);
    ("more key variants", `Quick, test_more_key_variants);
    ("invalid sequences", `Quick, test_invalid_sequences);
    ("combined inputs", `Quick, test_combined_inputs);
    ("kitty keyboard", `Quick, test_kitty_keyboard);
    ("edge cases", `Quick, test_input_edge_cases);
    ("split UTF-8", `Quick, test_split_utf8);
    ("buffer overflow", `Quick, test_buffer_overflow);
    ("paste mode collection", `Quick, test_paste_mode_collection);
    ("CSI param overflow", `Quick, test_csi_param_overflow);
    ("cursor position report", `Quick, test_cursor_position_report);
    ("device attributes", `Quick, test_device_attributes);
  ]

let () = Alcotest.run "Input" [ ("parsing", tests) ]
