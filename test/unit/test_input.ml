(** Tests for the Input module *)

open Mosaic
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

  (* Should get paste start, paste end, and paste content *)
  match events with
  | [ Input.Paste_start; Input.Paste_end; Input.Paste content ] ->
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
        "Expected [Paste_start; Paste_end; Paste(content)], got [%s]" event_str

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
  ]

let () = Alcotest.run "Input" [ ("parsing", tests) ]
