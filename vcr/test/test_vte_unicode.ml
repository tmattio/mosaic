open Alcotest

let test_unicode_characters () =
  let vte = Vte.create ~rows:5 ~cols:20 () in

  (* Test basic ASCII *)
  let ascii_text = Bytes.of_string "Hello World" in
  Vte.feed vte ascii_text 0 (Bytes.length ascii_text);
  let output = Vte.to_string_grid vte in
  check string "ASCII text" "Hello World" (String.trim output);

  (* Reset for next test *)
  Vte.reset vte;

  (* Test Unicode characters *)
  let unicode_text = Bytes.of_string "Hello 世界 🌍" in
  Vte.feed vte unicode_text 0 (Bytes.length unicode_text);
  let output = Vte.to_string_grid vte in
  check string "Unicode text" "Hello 世界 🌍" (String.trim output);

  (* Reset for next test *)
  Vte.reset vte;

  (* Test mixed content with newlines *)
  let mixed_text = Bytes.of_string "Line1\nΓειά σου\n日本語" in
  Vte.feed vte mixed_text 0 (Bytes.length mixed_text);
  let output = Vte.to_string_grid vte in
  let expected = "Line1\nΓειά σου\n日本語" in
  check string "Mixed unicode with newlines" expected (String.trim output)

let test_unicode_with_escapes () =
  let vte = Vte.create ~rows:5 ~cols:20 () in

  (* Test Unicode with ANSI color codes *)
  let colored_text = Bytes.of_string "\x1b[31mRed 文字\x1b[0m Normal" in
  Vte.feed vte colored_text 0 (Bytes.length colored_text);
  let output = Vte.to_string_grid vte in
  check string "Unicode with ANSI escapes" "Red 文字 Normal" (String.trim output)

let test_cursor_position () =
  let vte = Vte.create ~rows:5 ~cols:20 () in

  (* Test cursor position after Unicode characters *)
  let text = Bytes.of_string "Hello 世界" in
  Vte.feed vte text 0 (Bytes.length text);
  let row, col = Vte.cursor_pos vte in
  check (pair int int) "Cursor after unicode" (0, 8) (row, col)

let test_malformed_utf8 () =
  let vte = Vte.create ~rows:5 ~cols:20 () in

  (* Test handling of malformed UTF-8 sequences *)
  let malformed = Bytes.create 5 in
  Bytes.set malformed 0 'A';
  Bytes.set malformed 1 '\xff';
  (* Invalid UTF-8 *)
  Bytes.set malformed 2 '\xfe';
  (* Invalid UTF-8 *)
  Bytes.set malformed 3 'B';
  Bytes.set malformed 4 'C';

  Vte.feed vte malformed 0 5;
  let output = Vte.to_string_grid vte in
  (* Malformed sequences should be skipped *)
  check string "Malformed UTF-8 handling" "ABC" (String.trim output)

let () =
  run "VTE Unicode"
    [
      ( "Unicode support",
        [
          test_case "Unicode characters" `Quick test_unicode_characters;
          test_case "Unicode with ANSI escapes" `Quick test_unicode_with_escapes;
          test_case "Cursor position with Unicode" `Quick test_cursor_position;
          test_case "Malformed UTF-8 handling" `Quick test_malformed_utf8;
        ] );
    ]
