open Alcotest
open Ansi

(** Test SGR (Select Graphic Rendition) generation *)
let test_sgr () =
  check string "empty" "" (sgr []);
  check string "reset" "\x1b[0m" (sgr [ `Reset ]);
  check string "bold" "\x1b[1m" (sgr [ `Bold ]);
  check string "red fg" "\x1b[31m" (sgr [ `Fg Red ]);
  check string "blue bg" "\x1b[44m" (sgr [ `Bg Blue ]);
  check string "multiple attrs" "\x1b[31;44;1m"
    (sgr [ `Fg Red; `Bg Blue; `Bold ]);
  check string "rgb color" "\x1b[38;2;255;128;0m"
    (sgr [ `Fg (RGB (255, 128, 0)) ]);
  check string "complex" "\x1b[38;5;200;48;2;0;0;255;1;3;4m"
    (sgr [ `Fg (Index 200); `Bg (RGB (0, 0, 255)); `Bold; `Italic; `Underline ])

(** Test cursor movement *)
let test_cursor_movement () =
  check string "up 0" "" (cursor_up 0);
  check string "up 1" "\x1b[1A" (cursor_up 1);
  check string "up 10" "\x1b[10A" (cursor_up 10);

  check string "down 0" "" (cursor_down 0);
  check string "down 5" "\x1b[5B" (cursor_down 5);

  check string "forward 0" "" (cursor_forward 0);
  check string "forward 3" "\x1b[3C" (cursor_forward 3);

  check string "back 0" "" (cursor_back 0);
  check string "back 7" "\x1b[7D" (cursor_back 7);

  check string "position 1,1" "\x1b[1;1H" (cursor_position 1 1);
  check string "position 10,20" "\x1b[10;20H" (cursor_position 10 20)

(** Test cursor visibility and style *)
let test_cursor_style () =
  check string "save" "\x1b[s" cursor_save;
  check string "restore" "\x1b[u" cursor_restore;
  check string "show" "\x1b[?25h" cursor_show;
  check string "hide" "\x1b[?25l" cursor_hide;
  check string "blinking block" "\x1b[1 q" set_cursor_style_blinking_block;
  check string "steady block" "\x1b[2 q" set_cursor_style_steady_block;
  check string "blinking underline" "\x1b[3 q"
    set_cursor_style_blinking_underline;
  check string "steady underline" "\x1b[4 q" set_cursor_style_steady_underline;
  check string "blinking bar" "\x1b[5 q" set_cursor_style_blinking_bar;
  check string "steady bar" "\x1b[6 q" set_cursor_style_steady_bar

(** Test screen and line clearing *)
let test_clearing () =
  check string "clear screen" "\x1b[2J" clear_screen;
  check string "clear above" "\x1b[1J" clear_screen_above;
  check string "clear below" "\x1b[J" clear_screen_below;
  check string "clear line" "\x1b[2K" clear_line;
  check string "clear line left" "\x1b[1K" clear_line_left;
  check string "clear line right" "\x1b[K" clear_line_right

(** Test scrolling *)
let test_scrolling () =
  check string "scroll up 1" "\x1b[1S" (scroll_up 1);
  check string "scroll up 5" "\x1b[5S" (scroll_up 5);
  check string "scroll down 1" "\x1b[1T" (scroll_down 1);
  check string "scroll down 3" "\x1b[3T" (scroll_down 3)

(** Test screen switching and modes *)
let test_screen_modes () =
  check string "alt screen on" "\x1b[?1049h" alternate_screen_on;
  check string "alt screen off" "\x1b[?1049l" alternate_screen_off;
  check string "mouse on" "\x1b[?1000;1002;1006h" mouse_on;
  check string "mouse off" "\x1b[?1000;1002;1006l" mouse_off;
  check string "bracketed paste on" "\x1b[?2004h" bracketed_paste_on;
  check string "bracketed paste off" "\x1b[?2004l" bracketed_paste_off

(** Test window title *)
let test_window_title () =
  check string "empty title" "\x1b[]0;\x07" (set_window_title "");
  check string "simple title" "\x1b[]0;Hello\x07" (set_window_title "Hello");
  check string "title with spaces" "\x1b[]0;My Window\x07"
    (set_window_title "My Window")

(** Test ANSI stripping *)
let test_strip () =
  (* No ANSI codes *)
  check string "plain text" "Hello World" (strip "Hello World");
  check string "empty string" "" (strip "");

  (* Simple CSI sequences *)
  check string "color codes" "Hello" (strip "\x1b[31mHello\x1b[0m");
  check string "cursor movement" "Text" (strip "\x1b[10;20HText");
  check string "multiple attrs" "Bold" (strip "\x1b[1;31;44mBold");

  (* Complex sequences *)
  check string "sgr with params" "Test" (strip "\x1b[38;2;255;0;0mTest");
  check string "mixed content" "Hello World"
    (strip "\x1b[31mHello\x1b[0m World");

  (* OSC sequences *)
  check string "window title BEL" "Text" (strip "\x1b]0;Title\x07Text");
  check string "window title ST" "Text" (strip "\x1b]0;Title\x1b\\Text");
  check string "hyperlink" "Link"
    (strip "\x1b]8;;http://example.com\x1b\\Link\x1b]8;;\x1b\\");

  (* Other escape sequences *)
  check string "SS2" "" (strip "\x1bNX");
  (* SS2 consumes the next char *)
  check string "SS3" "" (strip "\x1bOY");
  (* SS3 consumes the next char *)
  check string "DCS" "End" (strip "\x1bPdata\x1b\\End");
  check string "SOS" "End" (strip "\x1bXdata\x1b\\End");
  check string "PM" "End" (strip "\x1b^data\x1b\\End");
  check string "APC" "End" (strip "\x1b_data\x1b\\End");

  (* Edge cases *)
  check string "lone ESC at end" "Text" (strip "Text\x1b");
  check string "incomplete CSI" "Start" (strip "Start\x1b[");
  check string "incomplete OSC no terminator" "Start"
    (strip "Start\x1b]0;Title");
  check string "nested sequences" "AB" (strip "\x1b[31mA\x1b[32mB\x1b[0m");

  (* Real-world examples *)
  let colored_prompt = "\x1b[1;32muser@host\x1b[0m:\x1b[1;34m~/dir\x1b[0m$ " in
  check string "colored prompt" "user@host:~/dir$ " (strip colored_prompt);

  let complex = "\x1b[H\x1b[2J\x1b[3J\x1b[1;1HHello\x1b[2;1HWorld" in
  check string "complex terminal output" "HelloWorld" (strip complex)

(** Test hyperlink generation *)
let test_hyperlink () =
  check string "simple link"
    "\x1b]8;;http://example.com\x1b\\Click\x1b]8;;\x1b\\"
    (hyperlink ~uri:"http://example.com" "Click");
  check string "empty text" "\x1b]8;;http://example.com\x1b\\\x1b]8;;\x1b\\"
    (hyperlink ~uri:"http://example.com" "");
  check string "complex uri"
    "\x1b]8;;https://example.com/path?q=1&r=2\x1b\\Link\x1b]8;;\x1b\\"
    (hyperlink ~uri:"https://example.com/path?q=1&r=2" "Link")

(** Test style function *)
let test_style_function () =
  check string "styled text" "\x1b[31mRed Text\x1b[39m"
    (style [ `Fg Red ] "Red Text");
  check string "multiple styles" "\x1b[1;4;31mBold Underline Red\x1b[22;24;39m"
    (style [ `Bold; `Underline; `Fg Red ] "Bold Underline Red");
  check string "empty attrs" "Plain" (style [] "Plain")

(** Test reset sequences *)
let test_reset_sequences () =
  check string "reset all" "\x1b[0m" reset;
  check string "reset bold/dim" "\x1b[22m" reset_bold_dim;
  check string "reset underline" "\x1b[24m" reset_underline;
  check string "reset blink" "\x1b[25m" reset_blink;
  check string "reset reverse" "\x1b[27m" reset_reverse;
  check string "reset strikethrough" "\x1b[29m" reset_strikethrough;
  check string "reset overline" "\x1b[55m" reset_overline

(** Test color types *)
let test_color_types () =
  (* Basic colors *)
  let basic_colors =
    [
      Black;
      Red;
      Green;
      Yellow;
      Blue;
      Magenta;
      Cyan;
      White;
      Default;
      Bright_black;
      Bright_red;
      Bright_green;
      Bright_yellow;
      Bright_blue;
      Bright_magenta;
      Bright_cyan;
      Bright_white;
    ]
  in

  (* Test that all basic colors work with SGR *)
  List.iter
    (fun color ->
      let fg_seq = sgr [ `Fg color ] in
      let bg_seq = sgr [ `Bg color ] in
      check bool "fg sequence not empty" true (String.length fg_seq > 0);
      check bool "bg sequence not empty" true (String.length bg_seq > 0))
    basic_colors;

  (* Test indexed colors *)
  check string "index 0" "\x1b[38;5;0m" (sgr [ `Fg (Index 0) ]);
  check string "index 255" "\x1b[38;5;255m" (sgr [ `Fg (Index 255) ]);

  (* Test RGB colors *)
  check string "rgb black" "\x1b[38;2;0;0;0m" (sgr [ `Fg (RGB (0, 0, 0)) ]);
  check string "rgb white" "\x1b[38;2;255;255;255m"
    (sgr [ `Fg (RGB (255, 255, 255)) ]);
  check string "rgb custom" "\x1b[38;2;128;64;192m"
    (sgr [ `Fg (RGB (128, 64, 192)) ])

(** Convert style variant to string for test descriptions *)
let string_of_style : attr -> string = function
  | `Bold -> "Bold"
  | `Dim -> "Dim"
  | `Italic -> "Italic"
  | `Underline -> "Underline"
  | `Double_underline -> "Double_underline"
  | `Blink -> "Blink"
  | `Reverse -> "Reverse"
  | `Conceal -> "Conceal"
  | `Strikethrough -> "Strikethrough"
  | `Overline -> "Overline"
  | `Framed -> "Framed"
  | `Encircled -> "Encircled"
  | `Reset -> "Reset"
  | `Fg _ -> "Fg"
  | `Bg _ -> "Bg"

(** Test style attributes *)
let test_style_attrs () =
  let styles : (attr * string) list =
    [
      (`Bold, "\x1b[1m");
      (`Dim, "\x1b[2m");
      (`Italic, "\x1b[3m");
      (`Underline, "\x1b[4m");
      (`Blink, "\x1b[5m");
      (`Reverse, "\x1b[7m");
      (`Strikethrough, "\x1b[9m");
      (`Double_underline, "\x1b[21m");
      (`Overline, "\x1b[53m");
    ]
  in

  List.iter
    (fun (attr, expected) ->
      check string (string_of_style attr) expected (sgr [ attr ]))
    styles

(** Test invalid inputs and edge cases *)
let test_invalid_inputs () =
  (* Test negative values in cursor movements - should be treated as 0 *)
  check string "negative cursor up" "" (cursor_up (-5));
  check string "negative cursor down" "" (cursor_down (-10));
  check string "negative cursor forward" "" (cursor_forward (-1));
  check string "negative cursor back" "" (cursor_back (-100));

  (* Test negative scroll values *)
  check string "negative scroll up" "" (scroll_up (-1));
  check string "negative scroll down" "" (scroll_down (-5));

  (* Test edge case cursor positions *)
  check string "zero cursor position" "\x1b[1;1H" (cursor_position 0 0);
  check string "negative cursor position" "\x1b[1;1H"
    (cursor_position (-5) (-10));
  check string "large cursor position" "\x1b[99999;99999H"
    (cursor_position 99999 99999);

  (* RGB colors with out-of-range values should be clamped *)
  check string "rgb negative values clamped" "\x1b[38;2;0;0;0m"
    (sgr [ `Fg (RGB (-10, -20, -30)) ]);
  check string "rgb over 255 clamped" "\x1b[38;2;255;255;255m"
    (sgr [ `Fg (RGB (300, 400, 500)) ]);

  (* Test index colors at boundaries - also clamped *)
  check string "index negative clamped" "\x1b[38;5;0m"
    (sgr [ `Fg (Index (-1)) ]);
  check string "index over 255 clamped" "\x1b[38;5;255m"
    (sgr [ `Fg (Index 256) ]);
  check string "index max int clamped" "\x1b[38;5;255m"
    (sgr [ `Fg (Index max_int) ])

(** Test complex nested and malformed sequences in strip *)
let test_strip_advanced () =
  (* Nested OSC and CSI sequences *)
  check string "osc inside csi" "" (strip "\x1b]0;\x1b[31mTitle\x07");
  check string "csi inside osc" "Text"
    (strip "\x1b[31m\x1b]0;Title\x07Text\x1b[0m");

  (* Incomplete sequences *)
  (* Note: 'T' is consumed as CSI terminator (scroll down command) *)
  check string "incomplete sgr params" "est" (strip "\x1b[38;2;255Test");
  check string "osc missing terminator at end" "" (strip "\x1b]0;Title");
  (* Note: \x1b[B is CSI B (cursor down), \x1b]C starts unterminated OSC *)
  check string "multiple incomplete" "A" (strip "A\x1b[B\x1b]C");

  (* Very long sequences *)
  let long_params = String.make 1000 '9' in
  check string "long csi params" "Text"
    (strip (Printf.sprintf "\x1b[%smText" long_params));

  (* Mixed terminators *)
  (* OSC without proper terminator consumes everything *)
  check string "osc with wrong terminator" "" (strip "\x1b]0;Title\x1b[mEnd");

  (* OSC should end with BEL or ST *)

  (* Multiple escape characters *)
  check string "double escape" "" (strip "\x1b\x1b");
  (* Note: Each ESC is handled separately, third ESC-T might be consumed as a command *)
  check string "triple escape with text" "ext" (strip "\x1b\x1b\x1bText");

  (* Real-world malformed sequences *)
  (* First CSI is incomplete, second ESC starts new CSI, 'm' terminates it *)
  check string "broken prompt" "m$ " (strip "\x1b[1;32\x1b[m$ ");
  check string "interleaved sequences" "HelloWorld"
    (strip "\x1b[31mHe\x1b]0;Title\x07llo\x1b[0mWorld")

(** Test hyperlink edge cases *)
let test_hyperlink_edge () =
  (* Empty URI *)
  check string "empty uri" "\x1b]8;;\x1b\\text\x1b]8;;\x1b\\"
    (hyperlink ~uri:"" "text");

  (* Very long URI *)
  let long_uri = String.make 2000 'a' in
  let expected = Printf.sprintf "\x1b]8;;%s\x1b\\text\x1b]8;;\x1b\\" long_uri in
  check string "long uri" expected (hyperlink ~uri:long_uri "text");

  (* Special characters in URI *)
  check string "uri with spaces"
    "\x1b]8;;http://example.com/path with spaces\x1b\\click\x1b]8;;\x1b\\"
    (hyperlink ~uri:"http://example.com/path with spaces" "click");
  check string "uri with unicode"
    "\x1b]8;;http://example.com/❤️\x1b\\heart\x1b]8;;\x1b\\"
    (hyperlink ~uri:"http://example.com/❤️" "heart");

  (* Text with ANSI codes *)
  check string "text with ansi"
    "\x1b]8;;http://example.com\x1b\\\x1b[31mRed Link\x1b[0m\x1b]8;;\x1b\\"
    (hyperlink ~uri:"http://example.com" "\x1b[31mRed Link\x1b[0m")

(** Test window title edge cases *)
let test_window_title_edge () =
  (* Special characters *)
  check string "title with bell" "\x1b[]0;Title\x07Text\x07"
    (set_window_title "Title\x07Text");
  check string "title with escape" "\x1b[]0;Title\x1bText\x07"
    (set_window_title "Title\x1bText");
  check string "title with null" "\x1b[]0;Title\x00Text\x07"
    (set_window_title "Title\x00Text");

  (* Very long title *)
  let long_title = String.make 1000 'X' in
  let expected = Printf.sprintf "\x1b[]0;%s\x07" long_title in
  check string "long title" expected (set_window_title long_title);

  (* Unicode in title *)
  check string "unicode title" "\x1b[]0;🌍 World\x07"
    (set_window_title "🌍 World")

(** Test style function with edge cases *)
let test_style_edge () =
  (* Empty text *)
  check string "empty text with styles" "\x1b[31m\x1b[39m"
    (style [ `Fg Red ] "");

  (* Reset in attrs *)
  check string "reset in attrs" "\x1b[0;31mText\x1b[0m"
    (style [ `Reset; `Fg Red ] "Text");

  (* Duplicate attributes *)
  check string "duplicate colors" "\x1b[31;32mText\x1b[39m"
    (style [ `Fg Red; `Fg Green ] "Text");

  (* Many attributes *)
  let many_attrs =
    [
      `Bold;
      `Dim;
      `Italic;
      `Underline;
      `Blink;
      `Reverse;
      `Strikethrough;
      `Fg Red;
      `Bg Blue;
    ]
  in
  let result = style many_attrs "X" in
  check bool "many attrs not empty" true (String.length result > 2)

(** Property test helpers - simple property-based tests without external libs *)
let test_properties () =
  (* Property: strip should be idempotent *)
  let test_idempotent s =
    let stripped = strip s in
    check string "idempotent strip" stripped (strip stripped)
  in

  List.iter test_idempotent
    [
      "plain text";
      "\x1b[31mcolored\x1b[0m";
      "\x1b]0;Title\x07text";
      "\x1b[1;2;3mcomplex\x1b[0m";
    ];

  (* Property: strip removes all escape sequences *)
  let has_escape s = String.contains s '\x1b' in

  let test_no_escapes s =
    let stripped = strip s in
    check bool "no escapes after strip" false (has_escape stripped)
  in

  List.iter test_no_escapes
    [
      "\x1b[31mred\x1b[0m";
      "\x1b]0;Title\x07\x1b[1mBold\x1b[0m";
      "text\x1b[31m\x1b[32m\x1b[0m";
    ];

  (* Property: style with no attrs should return unchanged text *)
  let test_no_style_change s =
    check string "no attrs no change" s (style [] s)
  in

  List.iter test_no_style_change [ "plain"; "with spaces"; "unicode 🎉" ];

  (* Property: cursor movements with 0 should produce empty string *)
  let movements = [ cursor_up; cursor_down; cursor_forward; cursor_back ] in
  List.iter (fun f -> check string "zero movement empty" "" (f 0)) movements

let () =
  run "ANSI"
    [
      ("SGR generation", [ test_case "sgr" `Quick test_sgr ]);
      ( "Cursor movement",
        [ test_case "cursor movement" `Quick test_cursor_movement ] );
      ("Cursor style", [ test_case "cursor style" `Quick test_cursor_style ]);
      ("Clearing", [ test_case "clearing" `Quick test_clearing ]);
      ("Scrolling", [ test_case "scrolling" `Quick test_scrolling ]);
      ("Screen modes", [ test_case "screen modes" `Quick test_screen_modes ]);
      ("Window title", [ test_case "window title" `Quick test_window_title ]);
      ("Strip ANSI", [ test_case "strip" `Quick test_strip ]);
      ("Hyperlinks", [ test_case "hyperlink" `Quick test_hyperlink ]);
      ("Style function", [ test_case "style" `Quick test_style_function ]);
      ("Reset sequences", [ test_case "reset" `Quick test_reset_sequences ]);
      ("Color types", [ test_case "color types" `Quick test_color_types ]);
      ("Style attributes", [ test_case "style attrs" `Quick test_style_attrs ]);
      ( "Invalid inputs",
        [ test_case "invalid inputs" `Quick test_invalid_inputs ] );
      ( "Strip advanced",
        [ test_case "strip advanced" `Quick test_strip_advanced ] );
      ( "Hyperlink edge cases",
        [ test_case "hyperlink edge" `Quick test_hyperlink_edge ] );
      ( "Window title edge cases",
        [ test_case "window title edge" `Quick test_window_title_edge ] );
      ("Style edge cases", [ test_case "style edge" `Quick test_style_edge ]);
      ("Properties", [ test_case "properties" `Quick test_properties ]);
    ]
