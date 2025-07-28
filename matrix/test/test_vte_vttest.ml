(* Integration test to verify VTE can handle vttest-like sequences *)

let test_dec_private_modes () =
  let vte = Vte.create ~rows:24 ~cols:80 () in

  (* Test DECAWM (auto wrap mode) *)
  let seq = "\x1b[?7h" in
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);
  (* Enable auto wrap *)
  Alcotest.(check bool) "auto wrap enabled" true (Vte.is_auto_wrap_mode vte);

  let seq = "\x1b[?7l" in
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);
  (* Disable auto wrap *)
  Alcotest.(check bool) "auto wrap disabled" false (Vte.is_auto_wrap_mode vte);

  (* Test DECCKM (cursor key mode) *)
  let seq = "\x1b[?1h" in
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);
  (* Application mode *)
  Alcotest.(check bool) "cursor key app mode" true (Vte.is_cursor_key_mode vte);

  let seq = "\x1b[?1l" in
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);
  (* Cursor mode *)
  Alcotest.(check bool)
    "cursor key normal mode" false
    (Vte.is_cursor_key_mode vte);

  (* Test IRM (insert mode) *)
  let seq = "\x1b[4h" in
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);
  (* Enable insert mode *)
  Alcotest.(check bool) "insert mode enabled" true (Vte.is_insert_mode vte);

  let seq = "\x1b[4l" in
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);
  (* Disable insert mode *)
  Alcotest.(check bool) "insert mode disabled" false (Vte.is_insert_mode vte)

let test_scroll_region () =
  let vte = Vte.create ~rows:24 ~cols:80 () in

  (* Set scroll region to lines 10-20 *)
  let seq = "\x1b[10;20r" in
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);

  (* Move to line 20 and write text that should trigger scrolling *)
  let seq = "\x1b[20;1H" in
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);
  (* Move to row 20, col 1 *)
  let seq = "Line 1\n" in
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);
  let seq = "Line 2\n" in
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);

  (* Verify cursor is still within scroll region *)
  let row, _col = Vte.cursor_pos vte in
  Alcotest.(check bool) "cursor within scroll region" true (row >= 9 && row < 20)

let test_wide_characters () =
  let vte = Vte.create ~rows:10 ~cols:20 () in

  (* First test normal ASCII *)
  let seq = "A" in
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);
  (* Check that character was placed *)
  (match Vte.get_cell vte ~row:0 ~col:0 with
  | Some cell ->
      Alcotest.(check string) "ASCII char placed" "A" cell.Vte.Cell.glyph
  | None -> Alcotest.fail "No cell at 0,0");
  let _row, col = Vte.cursor_pos vte in
  Alcotest.(check int) "cursor after ASCII" 1 col;

  (* Reset cursor *)
  Vte.feed vte (Bytes.of_string "\r") 0 1;

  (* Feed a wide character *)
  let seq = "你" in
  (* Chinese character (3 bytes UTF-8) *)
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);

  (* Check cursor position moved by 2 *)
  let row, col = Vte.cursor_pos vte in
  Alcotest.(check int) "cursor row" 0 row;
  Alcotest.(check int) "cursor col after wide char" 2 col;

  (* Check that cell has width 2 *)
  match Vte.get_cell vte ~row:0 ~col:0 with
  | Some cell ->
      Alcotest.(check int) "wide character width" 2 cell.Vte.Cell.width;
      Alcotest.(check string) "wide character glyph" "你" cell.Vte.Cell.glyph
  | None -> Alcotest.fail "Expected cell at 0,0"

let test_combining_characters () =
  let vte = Vte.create ~rows:10 ~cols:20 () in

  (* Feed base character *)
  let seq = "e" in
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);
  let _row, col = Vte.cursor_pos vte in
  Alcotest.(check int) "cursor after base char" 1 col;

  (* Feed combining mark separately *)
  let seq = "\xCC\x81" in
  (* combining acute accent *)
  Vte.feed vte (Bytes.of_string seq) 0 (String.length seq);

  (* The combining character should not advance cursor *)
  let _row, col = Vte.cursor_pos vte in
  Alcotest.(check int) "cursor after combining char" 1 col

let () =
  Alcotest.run "VTE vttest compatibility"
    [
      ( "dec_modes",
        [
          Alcotest.test_case "private modes" `Quick test_dec_private_modes;
          Alcotest.test_case "scroll region" `Quick test_scroll_region;
        ] );
      ( "unicode",
        [
          Alcotest.test_case "wide characters" `Quick test_wide_characters;
          Alcotest.test_case "combining characters" `Quick
            test_combining_characters;
        ] );
    ]
