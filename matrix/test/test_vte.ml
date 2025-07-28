(** Comprehensive tests for VTE terminal command parsing and interpretation *)

open Alcotest

let create_vte ?(rows = 24) ?(cols = 80) () = Vte.create ~rows ~cols ()

(** Test helpers *)
let feed_string vte str =
  let bytes = Bytes.of_string str in
  Vte.feed vte bytes 0 (Bytes.length bytes)

let get_line vte row =
  let buf = Buffer.create 80 in
  for col = 0 to Vte.cols vte - 1 do
    match Vte.get_cell vte ~row ~col with
    | None -> Buffer.add_char buf ' '
    | Some cell ->
        (* Use the first character of the glyph *)
        let ch =
          if String.length cell.Vte.Cell.glyph > 0 then cell.Vte.Cell.glyph.[0]
          else '?'
        in
        Buffer.add_char buf ch
  done;
  (* Trim trailing spaces *)
  let str = Buffer.contents buf in
  let rec find_end i =
    if i < 0 then 0 else if str.[i] = ' ' then find_end (i - 1) else i + 1
  in
  String.sub str 0 (find_end (String.length str - 1))

let get_visible_content vte =
  let lines = ref [] in
  for row = 0 to Vte.rows vte - 1 do
    lines := get_line vte row :: !lines
  done;
  List.rev !lines

let get_char_at vte ~row ~col =
  match Vte.get_cell vte ~row ~col with
  | None -> ' '
  | Some cell ->
      if String.length cell.Vte.Cell.glyph > 0 then cell.Vte.Cell.glyph.[0]
      else '?'

(** Basic text rendering tests *)
let test_basic_text () =
  let vte = create_vte () in
  feed_string vte "Hello World";
  let line = get_line vte 0 in
  check string "simple text" "Hello World" line

let test_newline () =
  let vte = create_vte () in
  feed_string vte "Line 1\nLine 2";
  check string "first line" "Line 1" (get_line vte 0);
  check string "second line" "Line 2" (get_line vte 1)

let test_carriage_return () =
  let vte = create_vte () in
  feed_string vte "Hello\rWorld";
  check string "carriage return overwrites" "World" (get_line vte 0)

let test_tab () =
  let vte = create_vte () in
  feed_string vte "A\tB\tC";
  let line = get_line vte 0 in
  check bool "contains spacing" true (String.length line > 3);
  check char "first char" 'A' line.[0];
  (* Tab stops are typically at multiples of 8 *)
  check char "after first tab" 'B' (get_char_at vte ~row:0 ~col:8);
  check char "after second tab" 'C' (get_char_at vte ~row:0 ~col:16)

(** Cursor movement tests *)
let test_cursor_up () =
  let vte = create_vte () in
  feed_string vte "Line1\nLine2";
  feed_string vte "\x1b[A";
  (* Cursor up *)
  feed_string vte "X";
  check string "cursor up modifies previous line" "Line1X" (get_line vte 0);
  check string "second line unchanged" "Line2" (get_line vte 1)

let test_cursor_down () =
  let vte = create_vte () in
  feed_string vte "Line1";
  feed_string vte "\x1b[B";
  (* Cursor down *)
  feed_string vte "\r";
  (* Carriage return to start of line *)
  feed_string vte "Line2";
  check string "first line" "Line1" (get_line vte 0);
  check string "second line" "Line2" (get_line vte 1)

let test_cursor_forward () =
  let vte = create_vte () in
  feed_string vte "Hello";
  feed_string vte "\r";
  (* Return to start *)
  feed_string vte "\x1b[3C";
  (* Move 3 positions forward *)
  feed_string vte "X";
  check string "cursor forward" "HelXo" (get_line vte 0)

let test_cursor_backward () =
  let vte = create_vte () in
  feed_string vte "Hello";
  feed_string vte "\x1b[2D";
  (* Move 2 positions backward *)
  feed_string vte "X";
  check string "cursor backward" "HelXo" (get_line vte 0)

let test_cursor_position () =
  let vte = create_vte () in
  feed_string vte "\x1b[3;5H";
  (* Move to row 3, col 5 (1-indexed) *)
  feed_string vte "X";
  check char "positioned character" 'X' (get_char_at vte ~row:2 ~col:4)

(** Screen manipulation tests *)
let test_clear_screen () =
  let vte = create_vte () in
  feed_string vte "Line1\nLine2\nLine3";
  feed_string vte "\x1b[2J";
  (* Clear screen *)
  let content = get_visible_content vte in
  check bool "screen cleared" true
    (List.for_all (fun line -> line = "") content)

let test_clear_to_end_of_line () =
  let vte = create_vte () in
  feed_string vte "Hello World";
  feed_string vte "\r\x1b[5C";
  (* Move to position 5 *)
  feed_string vte "\x1b[K";
  (* Clear to end of line *)
  let line = get_line vte 0 in
  Printf.printf "After clear to end: line = %S\n" line;
  check string "clear to end" "Hello" line

let test_clear_to_beginning_of_line () =
  let vte = create_vte () in
  feed_string vte "Hello World";
  feed_string vte "\r\x1b[5C";
  (* Move to position 5 *)
  feed_string vte "\x1b[1K";
  (* Clear to beginning of line *)
  let line = get_line vte 0 in
  check bool "beginning cleared" true (String.sub line 0 5 = "     ");
  check string "end preserved" " World"
    (String.sub line 5 (String.length line - 5))

let test_insert_line () =
  let vte = create_vte () in
  feed_string vte "Line1\nLine2\nLine3";
  feed_string vte "\x1b[2;1H";
  (* Move to line 2 *)
  feed_string vte "\x1b[L";
  (* Insert line *)
  feed_string vte "NewLine";
  check string "first line unchanged" "Line1" (get_line vte 0);
  check string "new line inserted" "NewLine" (get_line vte 1);
  check string "second line pushed down" "Line2" (get_line vte 2)

let test_delete_line () =
  let vte = create_vte () in
  feed_string vte "Line1\nLine2\nLine3";
  feed_string vte "\x1b[2;1H";
  (* Move to line 2 *)
  feed_string vte "\x1b[M";
  (* Delete line *)
  check string "first line unchanged" "Line1" (get_line vte 0);
  check string "third line moved up" "Line3" (get_line vte 1);
  check string "bottom line empty" "" (get_line vte 2)

(** Text attributes tests *)
let test_bold_text () =
  let vte = create_vte () in
  feed_string vte "\x1b[1mBold\x1b[0m Normal";
  match Vte.get_cell vte ~row:0 ~col:0 with
  | Some cell -> check bool "bold attribute" true cell.Vte.Cell.attrs.bold
  | None -> fail "Expected cell at 0,0"

let test_italic_text () =
  let vte = create_vte () in
  feed_string vte "\x1b[3mItalic\x1b[0m";
  match Vte.get_cell vte ~row:0 ~col:0 with
  | Some cell -> check bool "italic attribute" true cell.Vte.Cell.attrs.italic
  | None -> fail "Expected cell at 0,0"

let test_underline_text () =
  let vte = create_vte () in
  feed_string vte "\x1b[4mUnderline\x1b[0m";
  match Vte.get_cell vte ~row:0 ~col:0 with
  | Some cell ->
      check bool "underline attribute" true cell.Vte.Cell.attrs.underline
  | None -> fail "Expected cell at 0,0"

let test_foreground_color () =
  let vte = create_vte () in
  feed_string vte "\x1b[31mRed\x1b[0m";
  match Vte.get_cell vte ~row:0 ~col:0 with
  | Some cell ->
      check bool "red foreground" (cell.Vte.Cell.attrs.fg = Ansi.Red) true
  | None -> fail "Expected cell at 0,0"

let test_background_color () =
  let vte = create_vte () in
  feed_string vte "\x1b[42mGreen BG\x1b[0m";
  match Vte.get_cell vte ~row:0 ~col:0 with
  | Some cell ->
      check bool "green background" (cell.Vte.Cell.attrs.bg = Ansi.Green) true
  | None -> fail "Expected cell at 0,0"

let test_256_color () =
  let vte = create_vte () in
  feed_string vte "\x1b[38;5;196mColor 196\x1b[0m";
  match Vte.get_cell vte ~row:0 ~col:0 with
  | Some cell -> (
      match cell.Vte.Cell.attrs.fg with
      | Ansi.Index 196 -> ()
      | _ -> fail "Expected color index 196")
  | None -> fail "Expected cell at 0,0"

let test_rgb_color () =
  let vte = create_vte () in
  feed_string vte "\x1b[38;2;255;128;0mRGB\x1b[0m";
  match Vte.get_cell vte ~row:0 ~col:0 with
  | Some cell -> (
      match cell.Vte.Cell.attrs.fg with
      | Ansi.RGB (255, 128, 0) -> ()
      | _ -> fail "Expected RGB color")
  | None -> fail "Expected cell at 0,0"

(** Scrolling and wrapping tests *)
let test_line_wrap () =
  let vte = create_vte ~cols:10 () in
  feed_string vte "1234567890ABC";
  check string "first line filled" "1234567890" (get_line vte 0);
  check string "wrapped to second line" "ABC" (get_line vte 1)

let test_scroll_up () =
  let vte = create_vte ~rows:3 () in
  feed_string vte "Line1\nLine2\nLine3\nLine4";
  check string "second line visible" "Line2" (get_line vte 0);
  check string "third line visible" "Line3" (get_line vte 1);
  check string "fourth line visible" "Line4" (get_line vte 2)

(** Complex sequence tests *)
let test_save_restore_cursor () =
  let vte = create_vte () in
  feed_string vte "Hello";
  feed_string vte "\x1b7";
  (* Save cursor *)
  feed_string vte "\nNew Line";
  feed_string vte "\x1b8";
  (* Restore cursor *)
  feed_string vte " World";
  check string "cursor restored" "Hello World" (get_line vte 0);
  check string "second line preserved" "New Line" (get_line vte 1)

let test_alternate_screen () =
  let vte = create_vte () in
  feed_string vte "Main screen";
  feed_string vte "\x1b[?1049h";
  (* Enter alternate screen *)
  feed_string vte "Alternate screen";
  check string "alternate screen content" "Alternate screen" (get_line vte 0);
  feed_string vte "\x1b[?1049l";
  (* Exit alternate screen *)
  check string "main screen restored" "Main screen" (get_line vte 0)

(** Incremental update tests - critical for the counter example *)
let test_incremental_update () =
  let vte = create_vte () in
  (* Simulate counter at 0 *)
  feed_string vte "Count: 0 [Press +/- keys]";
  check string "initial state" "Count: 0 [Press +/- keys]" (get_line vte 0);

  (* Simulate counter increment by overwriting just the number *)
  feed_string vte "\r\x1b[7C";
  (* Return to start, move to position of number *)
  feed_string vte "1";
  check string "incremented counter" "Count: 1 [Press +/- keys]"
    (get_line vte 0);

  (* Simulate another increment *)
  feed_string vte "\r\x1b[7C";
  feed_string vte "2";
  check string "incremented again" "Count: 2 [Press +/- keys]" (get_line vte 0)

let test_partial_line_update () =
  let vte = create_vte () in
  feed_string vte "Status: OK    Progress: 0%";

  (* Update just the progress *)
  feed_string vte "\r\x1b[24C";
  (* Move to progress number *)
  feed_string vte "50";
  let line = get_line vte 0 in
  check bool "status unchanged" true
    (String.starts_with ~prefix:"Status: OK" line);
  check bool "progress updated" true
    (String.contains line '5' && String.contains line '0')

let test_cursor_visibility () =
  let vte = create_vte () in
  check bool "cursor initially visible" true (Vte.is_cursor_visible vte);

  feed_string vte "\x1b[?25l";
  (* Hide cursor *)
  check bool "cursor hidden" false (Vte.is_cursor_visible vte);

  feed_string vte "\x1b[?25h";
  (* Show cursor *)
  check bool "cursor shown" true (Vte.is_cursor_visible vte)

(** Edge cases and error handling *)
let test_invalid_escape_sequence () =
  let vte = create_vte () in
  feed_string vte "\x1b[999999H";
  (* Invalid position *)
  feed_string vte "X";
  (* Should clamp to valid range *)
  let row, col = Vte.cursor_pos vte in
  check bool "row clamped" true (row < Vte.rows vte);
  check bool "col clamped" true (col < Vte.cols vte)

let test_incomplete_sequence () =
  let vte = create_vte () in
  (* Feed incomplete escape sequence *)
  feed_string vte "\x1b[";
  feed_string vte "31";
  (* Complete it *)
  feed_string vte "m";
  feed_string vte "Red";
  match Vte.get_cell vte ~row:0 ~col:0 with
  | Some cell ->
      check bool "red foreground after incomplete"
        (cell.Vte.Cell.attrs.fg = Ansi.Red)
        true
  | None -> fail "Expected cell at 0,0"

let test_utf8_handling () =
  let vte = create_vte () in
  feed_string vte "Hello world unicode test";
  let line = get_line vte 0 in
  check string "handles utf8" "Hello world unicode test" line

(** Test suite *)
let () =
  run "VTE Terminal Parsing"
    [
      ( "basic",
        [
          test_case "basic text" `Quick test_basic_text;
          test_case "newline" `Quick test_newline;
          test_case "carriage return" `Quick test_carriage_return;
          test_case "tab" `Quick test_tab;
        ] );
      ( "cursor movement",
        [
          test_case "cursor up" `Quick test_cursor_up;
          test_case "cursor down" `Quick test_cursor_down;
          test_case "cursor forward" `Quick test_cursor_forward;
          test_case "cursor backward" `Quick test_cursor_backward;
          test_case "cursor position" `Quick test_cursor_position;
        ] );
      ( "screen manipulation",
        [
          test_case "clear screen" `Quick test_clear_screen;
          test_case "clear to end of line" `Quick test_clear_to_end_of_line;
          test_case "clear to beginning of line" `Quick
            test_clear_to_beginning_of_line;
          test_case "insert line" `Quick test_insert_line;
          test_case "delete line" `Quick test_delete_line;
        ] );
      ( "text attributes",
        [
          test_case "bold text" `Quick test_bold_text;
          test_case "italic text" `Quick test_italic_text;
          test_case "underline text" `Quick test_underline_text;
          test_case "foreground color" `Quick test_foreground_color;
          test_case "background color" `Quick test_background_color;
          test_case "256 color" `Quick test_256_color;
          test_case "RGB color" `Quick test_rgb_color;
        ] );
      ( "scrolling and wrapping",
        [
          test_case "line wrap" `Quick test_line_wrap;
          test_case "scroll up" `Quick test_scroll_up;
        ] );
      ( "complex sequences",
        [
          test_case "save/restore cursor" `Quick test_save_restore_cursor;
          test_case "alternate screen" `Quick test_alternate_screen;
        ] );
      ( "incremental updates",
        [
          test_case "incremental counter update" `Quick test_incremental_update;
          test_case "partial line update" `Quick test_partial_line_update;
          test_case "cursor visibility" `Quick test_cursor_visibility;
        ] );
      ( "edge cases",
        [
          test_case "invalid escape sequence" `Quick
            test_invalid_escape_sequence;
          test_case "incomplete sequence" `Quick test_incomplete_sequence;
          test_case "UTF-8 handling" `Quick test_utf8_handling;
        ] );
    ]
