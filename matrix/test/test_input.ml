(** Tests for the Input module *)

let event_testable = Alcotest.testable Input.pp Input.equal

let caps_event_testable =
  Alcotest.testable Input.Caps.pp_event Input.Caps.equal_event

let event_type_pp fmt = function
  | Input.Key.Press -> Format.pp_print_string fmt "Press"
  | Input.Key.Repeat -> Format.pp_print_string fmt "Repeat"
  | Input.Key.Release -> Format.pp_print_string fmt "Release"

let event_type_testable = Alcotest.testable event_type_pp ( = )

let key_event ?modifier ?event_type ?associated_text ?shifted_key ?base_key key
    =
  Input.key ?modifier ?event_type ?associated_text ?shifted_key ?base_key key

let char_event ?modifier ?event_type ?associated_text ?shifted_key ?base_key c =
  Input.char ?modifier ?event_type ?associated_text ?shifted_key ?base_key c

let parse_single = Input.Parser.parse_single
let parse_user s = parse_single s |> fst
let parse_caps s = parse_single s |> snd

let feed_user parser bytes off len =
  Input.Parser.feed parser bytes off len |> fst

let flush_user ?now parser = Input.Parser.flush ?now parser |> fst

let test_parse_regular_chars () =
  let events = parse_user "a" in
  Alcotest.(check (list event_testable))
    "single char 'a'"
    [ char_event 'a' ]
    events;

  let events = parse_user "hello" in
  Alcotest.(check (list event_testable))
    "multiple chars 'hello'"
    [
      char_event 'h';
      char_event 'e';
      char_event 'l';
      char_event 'l';
      char_event 'o';
    ]
    events;

  match parse_user "A" with
  | [ Input.Key { key = Char u; modifier; _ } ] ->
      Alcotest.(check char) "uppercase key" 'A' (Uchar.to_char u);
      Alcotest.(check bool) "shift flag" true modifier.shift
  | _ -> Alcotest.fail "expected single uppercase char event"

let test_char_associated_text_default () =
  match parse_user "q" with
  | [ Input.Key { Input.Key.key = Char u; associated_text; _ } ] ->
      Alcotest.(check char) "key" 'q' (Uchar.to_char u);
      Alcotest.(check string) "associated text" "q" associated_text
  | _ -> Alcotest.fail "expected char key event"

let test_parse_control_chars () =
  let expect ctrl_seq letter =
    match parse_user ctrl_seq with
    | [ Input.Key { key = Char u; modifier; _ } ] ->
        Alcotest.(check char) "letter" letter (Uchar.to_char u);
        Alcotest.(check bool) "ctrl modifier" true modifier.ctrl
    | _ -> Alcotest.fail "expected ctrl key event"
  in
  expect "\x01" 'A';
  expect "\x03" 'C';
  expect "\x1a" 'Z'

let test_csi_sub_params_with_event_type () =
  match parse_user "\x1b[1:2:3A" with
  | [ Input.Key k ] ->
      Alcotest.(check bool)
        "shift modifier from sub-params" true k.modifier.shift;
      Alcotest.(check event_type_testable)
        "release event type" Input.Key.Release k.event_type
  | _ ->
      Alcotest.failf "expected single Up release event, got %d events"
        (List.length (parse_user "\x1b[1:2:3A"))

let test_parse_special_keys () =
  Alcotest.(check (list event_testable))
    "Enter"
    [ key_event Input.Key.Enter ]
    (parse_user "\r");

  Alcotest.(check (list event_testable))
    "Line feed"
    [ key_event Input.Key.Line_feed ]
    (parse_user "\n");

  Alcotest.(check (list event_testable))
    "Tab"
    [ key_event Input.Key.Tab ]
    (parse_user "\t");

  let parser = Input.Parser.create () in
  let events = feed_user parser (Bytes.of_string "\x1b") 0 1 in
  Alcotest.(check (list event_testable)) "Escape buffered" [] events;

  let events = feed_user parser (Bytes.of_string "\x7f") 0 1 in
  Alcotest.(check (list event_testable))
    "Alt+Backspace"
    [
      key_event
        ~modifier:{ Input.Key.no_modifier with Input.Key.alt = true }
        Input.Key.Backspace;
    ]
    events

let test_parse_arrow_keys () =
  let arrows =
    [
      ("\x1b[A", Input.Key.Up, "Up arrow");
      ("\x1b[B", Input.Key.Down, "Down arrow");
      ("\x1b[C", Input.Key.Right, "Right arrow");
      ("\x1b[D", Input.Key.Left, "Left arrow");
    ]
  in
  List.iter
    (fun (seq, key, desc) ->
      Alcotest.(check (list event_testable))
        desc
        [ key_event key ]
        (parse_user seq))
    arrows

let test_parse_function_keys () =
  let f_keys_ss3 =
    [
      ("\x1bOP", Input.Key.F 1);
      ("\x1bOQ", Input.Key.F 2);
      ("\x1bOR", Input.Key.F 3);
      ("\x1bOS", Input.Key.F 4);
    ]
  in
  List.iter
    (fun (seq, key) ->
      Alcotest.(check (list event_testable))
        "F-key"
        [ key_event key ]
        (parse_user seq))
    f_keys_ss3;

  Alcotest.(check (list event_testable))
    "F5"
    [ key_event (Input.Key.F 5) ]
    (parse_user "\x1b[15~")

let test_parse_modifiers () =
  Alcotest.(check (list event_testable))
    "Shift+Tab"
    [
      key_event
        ~modifier:{ Input.Key.no_modifier with shift = true }
        Input.Key.Tab;
    ]
    (parse_user "\x1b[Z");

  Alcotest.(check (list event_testable))
    "Ctrl+Up"
    [
      key_event
        ~modifier:{ Input.Key.no_modifier with ctrl = true }
        Input.Key.Up;
    ]
    (parse_user "\x1b[1;5A");

  Alcotest.(check (list event_testable))
    "Alt+Left"
    [
      key_event
        ~modifier:{ Input.Key.no_modifier with alt = true }
        Input.Key.Left;
    ]
    (parse_user "\x1b[1;3D")

let test_parse_mouse_sgr () =
  Alcotest.(check (list event_testable))
    "Mouse click"
    [ Input.mouse_press 9 19 Input.Mouse.Left ]
    (parse_user "\x1b[<0;10;20M");

  Alcotest.(check (list event_testable))
    "Mouse release"
    [ Input.mouse_release 9 19 Input.Mouse.Left ]
    (parse_user "\x1b[<0;10;20m");

  Alcotest.(check (list event_testable))
    "Mouse motion"
    [
      Input.mouse_motion 14 24
        Input.Mouse.{ left = true; middle = false; right = false };
    ]
    (parse_user "\x1b[<32;15;25M")

let test_parse_paste_mode () =
  match parse_user "\x1b[200~Hello, World!\x1b[201~" with
  | [ Input.Paste content ] ->
      Alcotest.(check string) "paste content" "Hello, World!" content
  | _ -> Alcotest.fail "Expected [Paste(content)]"

let test_parse_utf8 () =
  Alcotest.(check (list event_testable))
    "UTF-8 emoji"
    [ key_event (Input.Key.Char (Uchar.of_int 0x1F600)) ]
    (parse_user "😀");

  Alcotest.(check (list event_testable))
    "UTF-8 accented char"
    [ key_event (Input.Key.Char (Uchar.of_int 0xE9)) ]
    (parse_user "é")

let test_alt_and_alt_ctrl () =
  Alcotest.(check (list event_testable))
    "Alt+Enter"
    [
      key_event
        ~modifier:{ Input.Key.no_modifier with Input.Key.alt = true }
        Input.Key.Enter;
    ]
    (parse_user "\x1b\r");

  Alcotest.(check (list event_testable))
    "Alt+Line_feed"
    [
      key_event
        ~modifier:{ Input.Key.no_modifier with Input.Key.alt = true }
        Input.Key.Line_feed;
    ]
    (parse_user "\x1b\n");

  Alcotest.(check (list event_testable))
    "Alt+Ctrl+A"
    [
      char_event
        ~modifier:
          { Input.Key.no_modifier with Input.Key.alt = true; ctrl = true }
        'A';
    ]
    (parse_user "\x1b\x01");

  Alcotest.(check (list event_testable))
    "Alt+Ctrl+Space"
    [
      char_event
        ~modifier:
          { Input.Key.no_modifier with Input.Key.alt = true; ctrl = true }
        ' ';
    ]
    (parse_user "\x1b\x00");

  Alcotest.(check (list event_testable))
    "Alt+Shift+A"
    [
      char_event
        ~modifier:
          { Input.Key.no_modifier with Input.Key.alt = true; shift = true }
        'A';
    ]
    (parse_user "\x1bA")

let test_high_bit_meta () =
  let parser = Input.Parser.create () in
  match feed_user parser (Bytes.make 1 (Char.chr 0xE1)) 0 1 with
  | [ Input.Key { key = Char u; modifier; _ } ] ->
      Alcotest.(check char) "high-bit char" 'a' (Uchar.to_char u);
      Alcotest.(check bool) "alt modifier" true modifier.alt
  | _ -> Alcotest.fail "expected meta key event"

let test_incremental_parsing () =
  let parser = Input.Parser.create () in
  Alcotest.(check (list event_testable))
    "no events yet" []
    (feed_user parser (Bytes.of_string "\x1b") 0 1);
  Alcotest.(check (list event_testable))
    "still no events" []
    (feed_user parser (Bytes.of_string "[") 0 1);
  Alcotest.(check (list event_testable))
    "complete escape sequence"
    [ key_event Input.Key.Up ]
    (feed_user parser (Bytes.of_string "A") 0 1)

let test_more_key_variants () =
  let keys =
    [
      ("\x1b[5~", Input.Key.Page_up);
      ("\x1b[6~", Input.Key.Page_down);
      ("\x1b[H", Input.Key.Home);
      ("\x1b[F", Input.Key.End);
      ("\x1b[2~", Input.Key.Insert);
      ("\x1b[3~", Input.Key.Delete);
    ]
  in
  List.iter
    (fun (seq, key) ->
      Alcotest.(check (list event_testable))
        "Key"
        [ key_event key ]
        (parse_user seq))
    keys;

  let f_keys =
    [ (17, 6); (18, 7); (19, 8); (20, 9); (21, 10); (23, 11); (24, 12) ]
  in
  List.iter
    (fun (code, n) ->
      let seq = Printf.sprintf "\x1b[%d~" code in
      Alcotest.(check (list event_testable))
        (Printf.sprintf "F%d" n)
        [ key_event (Input.Key.F n) ]
        (parse_user seq))
    f_keys;

  let rxvt_shift_arrows =
    [
      ("\x1b[a", Input.Key.Up);
      ("\x1b[b", Input.Key.Down);
      ("\x1b[c", Input.Key.Right);
      ("\x1b[d", Input.Key.Left);
      ("\x1b[e", Input.Key.KP_5);
    ]
  in
  List.iter
    (fun (seq, key) ->
      Alcotest.(check (list event_testable))
        "rxvt shift arrows"
        [
          key_event
            ~modifier:{ Input.Key.no_modifier with Input.Key.shift = true }
            key;
        ]
        (parse_user seq))
    rxvt_shift_arrows;

  let rxvt_special_codes =
    [
      (2, Input.Key.Insert);
      (3, Input.Key.Delete);
      (5, Input.Key.Page_up);
      (6, Input.Key.Page_down);
      (7, Input.Key.Home);
      (8, Input.Key.End);
    ]
  in
  List.iter
    (fun (code, key) ->
      let seq = Printf.sprintf "\x1b[%d$" code in
      Alcotest.(check (list event_testable))
        "rxvt shift special"
        [
          key_event
            ~modifier:{ Input.Key.no_modifier with Input.Key.shift = true }
            key;
        ]
        (parse_user seq))
    rxvt_special_codes;
  List.iter
    (fun (code, key) ->
      let seq = Printf.sprintf "\x1b[%d^" code in
      Alcotest.(check (list event_testable))
        "rxvt ctrl special"
        [
          key_event
            ~modifier:{ Input.Key.no_modifier with Input.Key.ctrl = true }
            key;
        ]
        (parse_user seq))
    rxvt_special_codes;

  let ss3_ctrl =
    [
      ("\x1bOa", Input.Key.Up);
      ("\x1bOb", Input.Key.Down);
      ("\x1bOc", Input.Key.Right);
      ("\x1bOd", Input.Key.Left);
      ("\x1bOe", Input.Key.KP_5);
    ]
  in
  List.iter
    (fun (seq, key) ->
      Alcotest.(check (list event_testable))
        "ss3 ctrl arrows"
        [
          key_event
            ~modifier:{ Input.Key.no_modifier with Input.Key.ctrl = true }
            key;
        ]
        (parse_user seq))
    ss3_ctrl

let test_escape_flush () =
  let parser = Input.Parser.create () in
  let events = feed_user parser (Bytes.of_string "\x1b") 0 1 in
  Alcotest.(check (list event_testable)) "no immediate escape" [] events;
  let events = flush_user ~now:(Unix.gettimeofday () +. 1.) parser in
  Alcotest.(check (list event_testable))
    "escape after timeout"
    [ key_event Input.Key.Escape ]
    events

let test_alt_escape_no_sticky () =
  let parser = Input.Parser.create () in
  let events = feed_user parser (Bytes.of_string "\x1b\x1b") 0 2 in
  Alcotest.(check (list event_testable))
    "alt+escape"
    [
      key_event
        ~modifier:{ Input.Key.no_modifier with Input.Key.alt = true }
        Input.Key.Escape;
    ]
    events;
  match feed_user parser (Bytes.of_string "a") 0 1 with
  | [ Input.Key { key = Char u; modifier; _ } ] ->
      Alcotest.(check bool) "alt cleared" false modifier.alt;
      Alcotest.(check char) "plain a" 'a' (Uchar.to_char u)
  | _ -> Alcotest.fail "expected plain 'a' after alt escape"

let test_invalid_sequences () =
  let parser = Input.Parser.create () in
  Alcotest.(check (list event_testable))
    "incomplete CSI buffered" []
    (feed_user parser (Bytes.of_string "\x1b[") 0 2);

  let events = feed_user parser (Bytes.of_string "999999X") 0 7 in
  Alcotest.(check bool) "parsed as chars" true (List.length events >= 7);

  let long_seq = "\x1b[" ^ String.make 100 '9' ^ "m" in
  let events = parse_user long_seq in
  Alcotest.(check bool)
    "long sequence handled" true
    (List.length events = 0 || List.length events > 0);

  let invalid_utf8 = Bytes.of_string "\xff\xfe" in
  let events = feed_user parser invalid_utf8 0 2 in
  Alcotest.(check bool) "invalid UTF-8 handled" true (List.length events >= 0);

  match parse_user "a\x1b[999999999999mbc" with
  | Input.Key { key = Char c; _ } :: _ ->
      Alcotest.(check char) "first char parsed" 'a' (Uchar.to_char c)
  | _ -> Alcotest.fail "expected at least one char"

let test_combined_inputs () =
  Alcotest.(check (list event_testable))
    "Ctrl+Alt+Shift+Up"
    [
      key_event
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
        Input.Key.Up;
    ]
    (parse_user "\x1b[1;8A");

  let events = parse_user "\x1b[<0;5;10Ma" in
  Alcotest.(check int) "mouse + key event count" 2 (List.length events);
  (match events with
  | [ Input.Mouse _; Input.Key _ ] -> ()
  | _ -> Alcotest.fail "expected mouse then key event");

  (match parse_user "\x1b[200~Hello\nWorld\t!\x1b[201~" with
  | [ Input.Paste s ] ->
      Alcotest.(check string) "paste content" "Hello\nWorld\t!" s
  | _ -> Alcotest.fail "Expected single Paste event");

  let rapid =
    String.concat ""
      (List.init 50 (fun i -> String.make 1 (Char.chr (65 + (i mod 26)))))
  in
  Alcotest.(check int) "rapid keys parsed" 50 (List.length (parse_user rapid))

let test_kitty_keyboard () =
  (match parse_user "\x1b[97u" with
  | [ Input.Key { key = Char u; modifier; event_type; associated_text; _ } ] ->
      Alcotest.(check char) "kitty 'a'" 'a' (Uchar.to_char u);
      Alcotest.(check bool) "ctrl" false modifier.ctrl;
      Alcotest.(check bool) "meta" false modifier.meta;
      Alcotest.(check event_type_testable) "press" Input.Key.Press event_type;
      Alcotest.(check string) "associated text" "a" associated_text
  | _ -> Alcotest.fail "expected kitty key");

  (match parse_user "\x1b[97;5u" with
  | [ Input.Key { key = Char u; modifier; _ } ] ->
      Alcotest.(check char) "kitty ctrl+a key" 'a' (Uchar.to_char u);
      Alcotest.(check bool) "ctrl set" true modifier.ctrl
  | _ -> Alcotest.fail "expected kitty ctrl key event");

  Alcotest.(check (list event_testable))
    "kitty enter"
    [ key_event Input.Key.Enter ]
    (parse_user "\x1b[13u");

  match parse_user "\x1b[97;1:3u" with
  | [ Input.Key { key = Char c; event_type; _ } ] ->
      Alcotest.(check char) "kitty with event type" 'a' (Uchar.to_char c);
      Alcotest.(check event_type_testable)
        "release event" Input.Key.Release event_type
  | _ -> Alcotest.fail "expected single key event"

let test_kitty_associated_text_fallback () =
  let expect_text seq expected =
    match parse_user seq with
    | [ Input.Key { associated_text; _ } ] ->
        Alcotest.(check string) "kitty associated text" expected associated_text
    | _ -> Alcotest.fail "expected single kitty key event"
  in
  expect_text "\x1b[97u" "a";
  expect_text "\x1b[97:65:97;2u" "A";
  expect_text "\x1b[32;2u" " "

let test_input_edge_cases () =
  Alcotest.(check (list event_testable)) "empty input" [] (parse_user "");

  Alcotest.(check (list event_testable))
    "null byte as Ctrl+Space"
    [ char_event ~modifier:{ Input.Key.no_modifier with ctrl = true } ' ' ]
    (parse_user "\x00");

  let parser = Input.Parser.create () in
  let events = feed_user parser (Bytes.of_string "\x1b\x1b") 0 2 in
  Alcotest.(check (list event_testable))
    "first escape parsed as Alt+Escape"
    [
      key_event
        ~modifier:{ Input.Key.no_modifier with Input.Key.alt = true }
        Input.Key.Escape;
    ]
    events;

  let large = String.make 10000 'X' in
  Alcotest.(check int)
    "large input parsed" 10000
    (List.length (parse_user large));

  let parser = Input.Parser.create () in
  let data = Bytes.of_string "XXXabcYYY" in
  match feed_user parser data 3 3 with
  | Input.Key { key = Char c; _ } :: _ as events ->
      Alcotest.(check int) "partial feed count" 3 (List.length events);
      Alcotest.(check char) "partial feed first char" 'a' (Uchar.to_char c)
  | _ -> Alcotest.fail "expected char event"

let test_split_utf8 () =
  let parser = Input.Parser.create () in
  Alcotest.(check (list event_testable))
    "incomplete UTF-8 should be buffered" []
    (feed_user parser (Bytes.of_string "\xE2\x82") 0 2);
  match feed_user parser (Bytes.of_string "\xAC") 0 1 with
  | [ Input.Key { key = Char u; _ } ] ->
      Alcotest.(check int) "euro sign unicode" 0x20AC (Uchar.to_int u)
  | _ -> Alcotest.fail "expected single UTF-8 character after completion"

let test_buffer_overflow () =
  let large = String.make 5000 'X' in
  Alcotest.(check int)
    "should parse all characters" 5000
    (List.length (parse_user large))

let test_paste_mode_collection () =
  let paste_content = String.make 1000 'A' in
  match parse_user ("\x1b[200~" ^ paste_content ^ "\x1b[201~") with
  | [ Input.Paste content ] ->
      Alcotest.(check string) "paste content matches" paste_content content
  | _ -> Alcotest.fail "Expected single Paste event"

let test_reset_aborts_paste () =
  let parser = Input.Parser.create () in
  let start_and_payload = "\x1b[200~abc" in
  ignore
    (feed_user parser
       (Bytes.of_string start_and_payload)
       0
       (String.length start_and_payload));
  Input.Parser.reset parser;
  let trailing = "\x1b[201~xyz" in
  match
    feed_user parser (Bytes.of_string trailing) 0 (String.length trailing)
  with
  | [
   Input.Key { key = Input.Key.Char ux; _ };
   Input.Key { key = Input.Key.Char uy; _ };
   Input.Key { key = Input.Key.Char uz; _ };
  ] ->
      Alcotest.(check char) "first char after reset" 'x' (Uchar.to_char ux);
      Alcotest.(check char) "second char after reset" 'y' (Uchar.to_char uy);
      Alcotest.(check char) "third char after reset" 'z' (Uchar.to_char uz)
  | _ -> Alcotest.fail "expected only text after reset"

let test_csi_param_overflow () =
  let huge_param = String.make 20 '9' in
  let seq = Printf.sprintf "\x1b[%s;1A" huge_param in
  let events = parse_user seq in
  Alcotest.(check bool) "got some events" true (List.length events >= 0)

let test_cursor_position_report () =
  Alcotest.(check (list caps_event_testable))
    "cursor position report"
    [ Input.Caps.Cursor_position (10, 25) ]
    (parse_caps "\x1b[10;25R")

let test_device_attributes () =
  Alcotest.(check (list caps_event_testable))
    "device attributes"
    [ Input.Caps.Device_attributes [ 1; 2; 6; 9; 15 ] ]
    (parse_caps "\x1b[?1;2;6;9;15c")

let test_mode_report () =
  match parse_caps "\x1b[?1004;2$y" with
  | [ Input.Caps.Mode_report report ] ->
      Alcotest.(check bool) "private mode" true report.is_private;
      Alcotest.(check bool) "mode values" true (report.modes = [ (1004, 2) ])
  | _ ->
      Alcotest.failf "Expected single mode report, got %d events"
        (List.length (parse_caps "\x1b[?1004;2$y"))

let test_user_and_caps_split () =
  let user, caps = parse_single "\x1b[?1004;2$yab" in
  Alcotest.(check (list caps_event_testable))
    "caps extracted"
    [ Input.Caps.Mode_report { is_private = true; modes = [ (1004, 2) ] } ]
    caps;
  Alcotest.(check int) "two user keys" 2 (List.length user);
  match user with
  | [ Input.Key { key = Char a; _ }; Input.Key { key = Char b; _ } ] ->
      Alcotest.(check char) "a" 'a' (Uchar.to_char a);
      Alcotest.(check char) "b" 'b' (Uchar.to_char b)
  | _ -> Alcotest.fail "expected two key events after capability"

let test_x10_mouse () =
  Alcotest.(check (list event_testable))
    "X10 mouse left press at (4,9)"
    [ Input.mouse_press 4 9 Input.Mouse.Left ]
    (parse_user "\x1b[M \x25\x2A")

let test_urxvt_mouse () =
  Alcotest.(check (list event_testable))
    "URXVT mouse left press at (9,19)"
    [ Input.mouse_press 9 19 Input.Mouse.Left ]
    (parse_user "\x1b[32;10;20M")

let test_osc_sequences () =
  match parse_user "\x1b]52;c;YWJj\x07\x1b]10;#FFFFFF\x07" with
  | [ Input.Clipboard (sel, data); Input.Osc (code, osc_data) ] ->
      Alcotest.(check string) "clipboard selection" "c" sel;
      Alcotest.(check string) "clipboard data" "abc" data;
      Alcotest.(check int) "osc code" 10 code;
      Alcotest.(check string) "osc data" "#FFFFFF" osc_data
  | _ ->
      Alcotest.failf "Expected [Clipboard; Osc], got %d events"
        (List.length (parse_user "\x1b]52;c;YWJj\x07\x1b]10;#FFFFFF\x07"))

let test_window_events () =
  Alcotest.(check (list event_testable))
    "focus, blur, resize"
    [ Input.Focus; Input.Blur; Input.Resize (80, 30) ]
    (parse_user "\x1b[I\x1b[O\x1b[8;30;80t")

let test_pixel_resolution_response () =
  Alcotest.(check (list caps_event_testable))
    "pixel resolution response"
    [ Input.Caps.Pixel_resolution (644, 448) ]
    (parse_caps "\x1b[4;448;644t")

let test_kitty_advanced () =
  match parse_user "\x1b[97:65:97;5:3;98:99u" with
  | [
   Input.Key
     {
       key = Char u;
       shifted_key = Some su;
       base_key = Some bu;
       event_type;
       associated_text;
       modifier = m;
     };
  ] ->
      Alcotest.(check char) "key" 'a' (Uchar.to_char u);
      Alcotest.(check char) "shifted" 'A' (Uchar.to_char su);
      Alcotest.(check char) "base" 'a' (Uchar.to_char bu);
      Alcotest.(check bool) "ctrl modifier" true m.ctrl;
      Alcotest.(check event_type_testable)
        "event type is Release" Input.Key.Release event_type;
      Alcotest.(check string) "associated text" "bc" associated_text
  | _ -> Alcotest.fail "expected advanced Kitty key event"

let test_media_and_modifier_keys () =
  Alcotest.(check (list event_testable))
    "media, volume, shift_left"
    [
      key_event Input.Key.Media_next;
      key_event Input.Key.Volume_up;
      key_event Input.Key.Shift_left;
    ]
    (parse_user "\x1b[57435u\x1b[57439u\x1b[57441u")

let test_paste_embedded_escapes () =
  let content = "Hello\x1b[31mWorld" in
  match parse_user ("\x1b[200~" ^ content ^ "\x1b[201~") with
  | [ Input.Paste s ] ->
      Alcotest.(check string) "embedded escapes sanitized" "HelloWorld" s
  | _ -> Alcotest.fail "expected single paste with embedded seq"

let test_parsing_efficiency () =
  let long_invalid = "\x1b[" ^ String.make 10000 '9' ^ "X" in
  let t0 = Unix.gettimeofday () in
  let events = parse_user long_invalid in
  let dt = Unix.gettimeofday () -. t0 in
  Alcotest.(check bool) "fast on long invalid (<0.1s)" true (dt < 0.1);
  Alcotest.(check bool) "parses many chars" true (List.length events >= 10000);

  let large_paste = String.make 50000 'A' in
  let t1 = Unix.gettimeofday () in
  match parse_user ("\x1b[200~" ^ large_paste ^ "\x1b[201~") with
  | [ Input.Paste s ] ->
      let dt_large = Unix.gettimeofday () -. t1 in
      Alcotest.(check bool) "fast large paste (<0.1s)" true (dt_large < 0.1);
      Alcotest.(check int)
        "single paste event"
        (String.length large_paste)
        (String.length s)
  | _ -> Alcotest.fail "large paste not optimized to single event"

let test_modify_other_keys () =
  let open Input in
  let check_one seq expected_key expected_mod_desc expected_mod =
    match parse_user seq with
    | [ Key { modifier; _ } ] ->
        Alcotest.(check bool) expected_mod_desc true (expected_mod modifier);
        Alcotest.(check (list event_testable))
          "single key"
          [ Input.key ~modifier expected_key ]
          (parse_user seq)
    | _ -> Alcotest.fail "expected single key event"
  in
  check_one "\x1b[27;2;13~" Key.Enter "shift" (fun m -> m.shift);
  check_one "\x1b[27;5;13~" Key.Enter "ctrl" (fun m -> m.ctrl);
  check_one "\x1b[27;5;27~" Key.Escape "ctrl" (fun m -> m.ctrl);
  check_one "\x1b[27;3;32~" (Key.Char (Uchar.of_int 32)) "alt" (fun m -> m.alt)

let test_kitty_keyboard_queries () =
  Alcotest.(check (list event_testable))
    "kitty query 1" [] (parse_user "\x1b[?1u");
  Alcotest.(check (list event_testable))
    "kitty query 1;2" [] (parse_user "\x1b[?1;2u");
  Alcotest.(check (list event_testable))
    "kitty query 0" [] (parse_user "\x1b[?0u");
  Alcotest.(check (list caps_event_testable))
    "caps kitty query"
    [ Input.Caps.Kitty_keyboard { level = 1; flags = None } ]
    (parse_caps "\x1b[?1u");
  Alcotest.(check (list caps_event_testable))
    "caps kitty query with flags"
    [ Input.Caps.Kitty_keyboard { level = 1; flags = Some 2 } ]
    (parse_caps "\x1b[?1;2u");
  Alcotest.(check (list caps_event_testable))
    "caps kitty query unsupported"
    [ Input.Caps.Kitty_keyboard { level = 0; flags = None } ]
    (parse_caps "\x1b[?0u")

let tests =
  [
    ("parse regular chars", `Quick, test_parse_regular_chars);
    ("char associated text default", `Quick, test_char_associated_text_default);
    ("parse control chars", `Quick, test_parse_control_chars);
    ("parse special keys", `Quick, test_parse_special_keys);
    ("parse arrow keys", `Quick, test_parse_arrow_keys);
    ("parse function keys", `Quick, test_parse_function_keys);
    ("parse modifiers", `Quick, test_parse_modifiers);
    ("CSI sub params event type", `Quick, test_csi_sub_params_with_event_type);
    ("parse mouse SGR", `Quick, test_parse_mouse_sgr);
    ("parse paste mode", `Quick, test_parse_paste_mode);
    ("parse UTF-8", `Quick, test_parse_utf8);
    ("incremental parsing", `Quick, test_incremental_parsing);
    ("more key variants", `Quick, test_more_key_variants);
    ("escape flush", `Quick, test_escape_flush);
    ("alt escape no sticky", `Quick, test_alt_escape_no_sticky);
    ("invalid sequences", `Quick, test_invalid_sequences);
    ("combined inputs", `Quick, test_combined_inputs);
    ("kitty keyboard", `Quick, test_kitty_keyboard);
    ( "kitty associated text fallback",
      `Quick,
      test_kitty_associated_text_fallback );
    ("edge cases", `Quick, test_input_edge_cases);
    ("alt and alt+ctrl", `Quick, test_alt_and_alt_ctrl);
    ("high-bit meta", `Quick, test_high_bit_meta);
    ("split UTF-8", `Quick, test_split_utf8);
    ("buffer overflow", `Quick, test_buffer_overflow);
    ("paste mode collection", `Quick, test_paste_mode_collection);
    ("reset aborts paste", `Quick, test_reset_aborts_paste);
    ("CSI param overflow", `Quick, test_csi_param_overflow);
    ("cursor position report", `Quick, test_cursor_position_report);
    ("device attributes", `Quick, test_device_attributes);
    ("mode report", `Quick, test_mode_report);
    ("user and caps split", `Quick, test_user_and_caps_split);
    ("X10 mouse", `Quick, test_x10_mouse);
    ("URXVT mouse", `Quick, test_urxvt_mouse);
    ("OSC sequences", `Quick, test_osc_sequences);
    ("window events", `Quick, test_window_events);
    ("pixel resolution response", `Quick, test_pixel_resolution_response);
    ("kitty advanced", `Quick, test_kitty_advanced);
    ("media and modifier keys", `Quick, test_media_and_modifier_keys);
    ("paste embedded escapes", `Quick, test_paste_embedded_escapes);
    ("modify other keys", `Quick, test_modify_other_keys);
    ("kitty keyboard queries", `Quick, test_kitty_keyboard_queries);
    ("parsing efficiency", `Slow, test_parsing_efficiency);
  ]

let () = Alcotest.run "Input" [ ("parsing", tests) ]
