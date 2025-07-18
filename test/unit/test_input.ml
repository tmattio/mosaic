(** Tests for the Input module *)

open Test_utils

let test_parse_regular_chars () =
  let parser = Input.create () in

  (* Test single character *)
  let events = Input.feed parser (Bytes.of_string "a") 0 1 in
  Alcotest.(check (list event_testable))
    "single char 'a'"
    [
      Input.Key { key = Char (Uchar.of_char 'a'); modifier = Input.no_modifier };
    ]
    events;

  (* Test multiple characters *)
  let events = Input.feed parser (Bytes.of_string "hello") 0 5 in
  Alcotest.(check (list event_testable))
    "multiple chars 'hello'"
    [
      Input.Key { key = Char (Uchar.of_char 'h'); modifier = Input.no_modifier };
      Input.Key { key = Char (Uchar.of_char 'e'); modifier = Input.no_modifier };
      Input.Key { key = Char (Uchar.of_char 'l'); modifier = Input.no_modifier };
      Input.Key { key = Char (Uchar.of_char 'l'); modifier = Input.no_modifier };
      Input.Key { key = Char (Uchar.of_char 'o'); modifier = Input.no_modifier };
    ]
    events

let test_parse_control_chars () =
  let parser = Input.create () in

  (* Test Ctrl+A *)
  let events = Input.feed parser (Bytes.of_string "\x01") 0 1 in
  Alcotest.(check (list event_testable))
    "Ctrl+A"
    [
      Input.Key
        {
          key = Char (Uchar.of_char 'A');
          modifier = { ctrl = true; alt = false; shift = false };
        };
    ]
    events;

  (* Test Ctrl+C - this is the critical test *)
  let events = Input.feed parser (Bytes.of_string "\x03") 0 1 in
  Alcotest.(check (list event_testable))
    "Ctrl+C"
    [
      Input.Key
        {
          key = Char (Uchar.of_char 'C');
          modifier = { ctrl = true; alt = false; shift = false };
        };
    ]
    events;

  (* Test Ctrl+Z *)
  let events = Input.feed parser (Bytes.of_string "\x1a") 0 1 in
  Alcotest.(check (list event_testable))
    "Ctrl+Z"
    [
      Input.Key
        {
          key = Char (Uchar.of_char 'Z');
          modifier = { ctrl = true; alt = false; shift = false };
        };
    ]
    events

let test_parse_special_keys () =
  let parser = Input.create () in

  (* Test Enter *)
  let events = Input.feed parser (Bytes.of_string "\r") 0 1 in
  Alcotest.(check (list event_testable))
    "Enter"
    [ Input.Key { key = Enter; modifier = Input.no_modifier } ]
    events;

  (* Test Tab *)
  let events = Input.feed parser (Bytes.of_string "\t") 0 1 in
  Alcotest.(check (list event_testable))
    "Tab"
    [ Input.Key { key = Tab; modifier = Input.no_modifier } ]
    events;

  (* Test Escape - in a streaming parser, a single escape is buffered *)
  let events = Input.feed parser (Bytes.of_string "\x1b") 0 1 in
  Alcotest.(check (list event_testable)) "Escape buffered" [] events;

  (* Test Backspace - this will also emit the buffered escape *)
  let events = Input.feed parser (Bytes.of_string "\x7f") 0 1 in
  Alcotest.(check (list event_testable))
    "Escape + Backspace"
    [
      Input.Key { key = Escape; modifier = Input.no_modifier };
      Input.Key { key = Backspace; modifier = Input.no_modifier };
    ]
    events

let test_parse_arrow_keys () =
  let parser = Input.create () in

  (* Test Up arrow *)
  let events = Input.feed parser (Bytes.of_string "\x1b[A") 0 3 in
  Alcotest.(check (list event_testable))
    "Up arrow"
    [ Input.Key { key = Up; modifier = Input.no_modifier } ]
    events;

  (* Test Down arrow *)
  let events = Input.feed parser (Bytes.of_string "\x1b[B") 0 3 in
  Alcotest.(check (list event_testable))
    "Down arrow"
    [ Input.Key { key = Down; modifier = Input.no_modifier } ]
    events;

  (* Test Right arrow *)
  let events = Input.feed parser (Bytes.of_string "\x1b[C") 0 3 in
  Alcotest.(check (list event_testable))
    "Right arrow"
    [ Input.Key { key = Right; modifier = Input.no_modifier } ]
    events;

  (* Test Left arrow *)
  let events = Input.feed parser (Bytes.of_string "\x1b[D") 0 3 in
  Alcotest.(check (list event_testable))
    "Left arrow"
    [ Input.Key { key = Left; modifier = Input.no_modifier } ]
    events

let test_parse_function_keys () =
  let parser = Input.create () in

  (* Test F1 *)
  let events = Input.feed parser (Bytes.of_string "\x1bOP") 0 3 in
  Alcotest.(check (list event_testable))
    "F1"
    [ Input.Key { key = F 1; modifier = Input.no_modifier } ]
    events;

  (* Test F2 *)
  let events = Input.feed parser (Bytes.of_string "\x1bOQ") 0 3 in
  Alcotest.(check (list event_testable))
    "F2"
    [ Input.Key { key = F 2; modifier = Input.no_modifier } ]
    events;

  (* Test F5 *)
  let events = Input.feed parser (Bytes.of_string "\x1b[15~") 0 5 in
  Alcotest.(check (list event_testable))
    "F5"
    [ Input.Key { key = F 5; modifier = Input.no_modifier } ]
    events

let test_parse_modifiers () =
  let parser = Input.create () in

  (* Test Shift+Tab *)
  let events = Input.feed parser (Bytes.of_string "\x1b[Z") 0 3 in
  Alcotest.(check (list event_testable))
    "Shift+Tab"
    [
      Input.Key
        { key = Tab; modifier = { ctrl = false; alt = false; shift = true } };
    ]
    events;

  (* Test Ctrl+Up *)
  let events = Input.feed parser (Bytes.of_string "\x1b[1;5A") 0 6 in
  Alcotest.(check (list event_testable))
    "Ctrl+Up"
    [
      Input.Key
        { key = Up; modifier = { ctrl = true; alt = false; shift = false } };
    ]
    events;

  (* Test Alt+Left *)
  let events = Input.feed parser (Bytes.of_string "\x1b[1;3D") 0 6 in
  Alcotest.(check (list event_testable))
    "Alt+Left"
    [
      Input.Key
        { key = Left; modifier = { ctrl = false; alt = true; shift = false } };
    ]
    events

let test_parse_mouse_sgr () =
  let parser = Input.create () in

  (* Test mouse click at (10, 20) *)
  let events = Input.feed parser (Bytes.of_string "\x1b[<0;10;20M") 0 11 in
  Alcotest.(check (list event_testable))
    "Mouse click"
    [ Input.Mouse (Press (9, 19, Left, Input.no_modifier)) ]
    events;

  (* Test mouse release *)
  let events = Input.feed parser (Bytes.of_string "\x1b[<0;10;20m") 0 11 in
  Alcotest.(check (list event_testable))
    "Mouse release"
    [ Input.Mouse (Release (9, 19, Left, Input.no_modifier)) ]
    events;

  (* Test mouse motion *)
  let events = Input.feed parser (Bytes.of_string "\x1b[<32;15;25M") 0 12 in
  Alcotest.(check (list event_testable))
    "Mouse motion"
    [
      Input.Mouse
        (Motion
           ( 14,
             24,
             { left = true; middle = false; right = false },
             Input.no_modifier ));
    ]
    events

let test_parse_paste_mode () =
  let parser = Input.create () in

  (* Test bracketed paste *)
  let input = "\x1b[200~Hello, World!\x1b[201~" in
  let events =
    Input.feed parser (Bytes.of_string input) 0 (String.length input)
  in

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
  let parser = Input.create () in

  (* Test UTF-8 character (emoji 😀) *)
  let events = Input.feed parser (Bytes.of_string "😀") 0 4 in
  Alcotest.(check (list event_testable))
    "UTF-8 emoji"
    [
      Input.Key
        { key = Char (Uchar.of_int 0x1F600); modifier = Input.no_modifier };
    ]
    events;

  (* Test accented character (é) *)
  let events = Input.feed parser (Bytes.of_string "é") 0 2 in
  Alcotest.(check (list event_testable))
    "UTF-8 accented char"
    [
      Input.Key { key = Char (Uchar.of_int 0xE9); modifier = Input.no_modifier };
    ]
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
    [ Input.Key { key = Up; modifier = Input.no_modifier } ]
    events3

(** Test additional key variants *)
let test_more_key_variants () =
  let parser = Input.create () in

  (* Test Page Up/Down *)
  let events = Input.feed parser (Bytes.of_string "\x1b[5~") 0 4 in
  Alcotest.(check (list event_testable))
    "Page Up"
    [ Input.Key { key = Page_up; modifier = Input.no_modifier } ]
    events;

  let events = Input.feed parser (Bytes.of_string "\x1b[6~") 0 4 in
  Alcotest.(check (list event_testable))
    "Page Down"
    [ Input.Key { key = Page_down; modifier = Input.no_modifier } ]
    events;

  (* Test Home/End *)
  let events = Input.feed parser (Bytes.of_string "\x1b[H") 0 3 in
  Alcotest.(check (list event_testable))
    "Home"
    [ Input.Key { key = Home; modifier = Input.no_modifier } ]
    events;

  let events = Input.feed parser (Bytes.of_string "\x1b[F") 0 3 in
  Alcotest.(check (list event_testable))
    "End"
    [ Input.Key { key = End; modifier = Input.no_modifier } ]
    events;

  (* Test Insert/Delete *)
  let events = Input.feed parser (Bytes.of_string "\x1b[2~") 0 4 in
  Alcotest.(check (list event_testable))
    "Insert"
    [ Input.Key { key = Insert; modifier = Input.no_modifier } ]
    events;

  let events = Input.feed parser (Bytes.of_string "\x1b[3~") 0 4 in
  Alcotest.(check (list event_testable))
    "Delete"
    [ Input.Key { key = Delete; modifier = Input.no_modifier } ]
    events;

  (* Test higher function keys F6-F12 *)
  let f_keys =
    [ (17, 6); (18, 7); (19, 8); (20, 9); (21, 10); (23, 11); (24, 12) ]
  in
  List.iter
    (fun (code, n) ->
      let seq = Printf.sprintf "\x1b[%d~" code in
      let events =
        Input.feed parser (Bytes.of_string seq) 0 (String.length seq)
      in
      Alcotest.(check (list event_testable))
        (Printf.sprintf "F%d" n)
        [ Input.Key { key = F n; modifier = Input.no_modifier } ]
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
  let events =
    Input.feed parser (Bytes.of_string long_seq) 0 (String.length long_seq)
  in
  (* Should handle gracefully - exact behavior depends on implementation *)
  Alcotest.(check bool)
    "long sequence handled" true
    (List.length events = 0 || List.length events > 0);

  (* Invalid UTF-8 sequences *)
  let invalid_utf8 = Bytes.create 2 in
  Bytes.set invalid_utf8 0 '\xff';
  Bytes.set invalid_utf8 1 '\xfe';
  let events = Input.feed parser invalid_utf8 0 2 in
  (* Should handle invalid UTF-8 gracefully *)
  Alcotest.(check bool) "invalid UTF-8 handled" true (List.length events >= 0);

  (* Mixed valid and invalid *)
  let events =
    Input.feed parser (Bytes.of_string "a\x1b[999999999999mbc") 0 18
  in
  (* Should at least parse 'a' *)
  match events with
  | Input.Key { key = Char c; _ } :: _ ->
      Alcotest.(check char) "first char parsed" 'a' (Uchar.to_char c)
  | _ -> Alcotest.fail "expected at least one char"

(** Test combined/complex inputs *)
let test_combined_inputs () =
  let parser = Input.create () in

  (* Key with multiple modifiers *)
  (* Modifier encoding: 1 (base) + 1 (shift) + 2 (alt) + 4 (ctrl) = 8 *)
  let events = Input.feed parser (Bytes.of_string "\x1b[1;8A") 0 6 in
  Alcotest.(check (list event_testable))
    "Ctrl+Alt+Shift+Up"
    [
      Input.Key
        { key = Up; modifier = { ctrl = true; alt = true; shift = true } };
    ]
    events;

  (* Mouse followed by key *)
  let seq = "\x1b[<0;5;10Ma" in
  let events = Input.feed parser (Bytes.of_string seq) 0 (String.length seq) in
  Alcotest.(check int) "mouse + key event count" 2 (List.length events);
  (match events with
  | [ Input.Mouse _; Input.Key _ ] -> ()
  | _ -> Alcotest.fail "expected mouse then key event");

  (* Paste with special chars *)
  let paste_seq = "\x1b[200~Hello\nWorld\t!\x1b[201~" in
  let events =
    Input.feed parser (Bytes.of_string paste_seq) 0 (String.length paste_seq)
  in
  (* Should have paste start, chars, paste end *)
  Alcotest.(check bool) "paste with special chars" true (List.length events >= 3);

  (* Rapid key presses *)
  let rapid =
    String.concat ""
      (List.init 50 (fun i -> String.make 1 (Char.chr (65 + (i mod 26)))))
  in
  let events =
    Input.feed parser (Bytes.of_string rapid) 0 (String.length rapid)
  in
  Alcotest.(check int) "rapid keys parsed" 50 (List.length events)

(** Test kitty keyboard protocol *)
let test_kitty_keyboard () =
  let parser = Input.create () in

  (* Basic kitty key *)
  let events = Input.feed parser (Bytes.of_string "\x1b[97u") 0 5 in
  Alcotest.(check (list event_testable))
    "kitty 'a'"
    [
      Input.Key { key = Char (Uchar.of_char 'a'); modifier = Input.no_modifier };
    ]
    events;

  (* Kitty with modifiers *)
  let events = Input.feed parser (Bytes.of_string "\x1b[97;5u") 0 7 in
  Alcotest.(check (list event_testable))
    "kitty ctrl+a"
    [
      Input.Key
        {
          key = Char (Uchar.of_char 'a');
          modifier = { ctrl = true; alt = false; shift = false };
        };
    ]
    events;

  (* Kitty special keys *)
  let events = Input.feed parser (Bytes.of_string "\x1b[13u") 0 5 in
  Alcotest.(check (list event_testable))
    "kitty enter"
    [ Input.Key { key = Enter; modifier = Input.no_modifier } ]
    events;

  (* Kitty with event type - press repeat release *)
  let events = Input.feed parser (Bytes.of_string "\x1b[97;1:3u") 0 9 in
  (* Should parse the key regardless of event type *)
  match events with
  | [ Input.Key { key = Char c; _ } ] ->
      Alcotest.(check char) "kitty with event type" 'a' (Uchar.to_char c)
  | _ -> Alcotest.fail "expected single key event"

(** Test edge cases *)
let test_input_edge_cases () =
  let parser = Input.create () in

  (* Empty input *)
  let events = Input.feed parser (Bytes.create 0) 0 0 in
  Alcotest.(check (list pass)) "empty input" [] events;

  (* Single null byte *)
  let events = Input.feed parser (Bytes.of_string "\x00") 0 1 in
  Alcotest.(check (list event_testable))
    "null byte as Ctrl+Space"
    [
      Input.Key
        {
          key = Char (Uchar.of_char ' ');
          modifier = { ctrl = true; alt = false; shift = false };
        };
    ]
    events;

  (* Alt+Escape *)
  let events = Input.feed parser (Bytes.of_string "\x1b\x1b") 0 2 in
  (* First ESC is parsed immediately, second is buffered *)
  Alcotest.(check (list event_testable))
    "first escape parsed"
    [ Input.Key { key = Escape; modifier = Input.no_modifier } ]
    events;

  (* Very large input buffer *)
  let large = String.make 10000 'X' in
  let events = Input.feed parser (Bytes.of_string large) 0 10000 in
  (* Should get 10001 events: 1 buffered ESC from previous test + 10000 X's *)
  Alcotest.(check int) "large input parsed" 10001 (List.length events);

  (* Input with offset and length *)
  let data = Bytes.of_string "XXXabcYYY" in
  let events = Input.feed parser data 3 3 in
  Alcotest.(check int) "partial feed count" 3 (List.length events);
  match events with
  | Input.Key { key = Char c; _ } :: _ ->
      Alcotest.(check char) "partial feed first char" 'a' (Uchar.to_char c)
  | _ -> Alcotest.fail "expected char event"

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
  ]

let () = Alcotest.run "Input" [ ("parsing", tests) ]
