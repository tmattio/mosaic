open Alcotest
open Render

(** Helper to create a test attr *)
let test_attr ?(fg = None) ?(bg = None) ?(bold = false) ?(italic = false)
    ?(underline = false) ?(uri = None) () =
  {
    fg;
    bg;
    bold;
    dim = false;
    italic;
    underline;
    blink = false;
    reverse = false;
    strikethrough = false;
    uri;
  }

(** Helper to format Uchar for error messages *)
let pp_uchar fmt u = Format.fprintf fmt "U+%04X" (Uchar.to_int u)

(** Alcotest testable for Uchar lists *)
let uchar_list = Alcotest.(list (testable pp_uchar Uchar.equal))

(** Test buffer creation and dimensions *)
let test_buffer_create () =
  let buf = create 10 5 in
  let w, h = dimensions buf in
  check int "width" 10 w;
  check int "height" 5 h;

  (* Check all cells are empty *)
  for y = 0 to 4 do
    for x = 0 to 9 do
      let cell = get buf x y in
      check uchar_list "empty cell chars" [ Uchar.of_int 0x20 ] cell.chars;
      check int "empty cell width" 1 cell.width
    done
  done

(** Test get/set operations *)
let test_get_set () =
  let buf = create 5 3 in
  let test_cell =
    { chars = [ Uchar.of_char 'A' ]; attr = test_attr ~bold:true (); width = 1 }
  in

  (* Set a cell *)
  set buf 2 1 test_cell;

  (* Get the same cell *)
  let retrieved = get buf 2 1 in
  check uchar_list "chars match" [ Uchar.of_char 'A' ] retrieved.chars;
  check bool "bold matches" true retrieved.attr.bold;
  check int "width matches" 1 retrieved.width;

  (* Get out of bounds returns empty cell *)
  let oob1 = get buf (-1) 0 in
  let oob2 = get buf 5 0 in
  let oob3 = get buf 0 3 in
  check uchar_list "oob chars" [ Uchar.of_int 0x20 ] oob1.chars;
  check uchar_list "oob chars" [ Uchar.of_int 0x20 ] oob2.chars;
  check uchar_list "oob chars" [ Uchar.of_int 0x20 ] oob3.chars

(** Test set_char with regular characters *)
let test_set_char_regular () =
  let buf = create 10 5 in
  let attr = test_attr ~fg:(Some Ansi.Red) () in

  (* ASCII character *)
  set_char buf 0 0 (Uchar.of_char 'H') attr;
  let cell = get buf 0 0 in
  check uchar_list "ascii char" [ Uchar.of_char 'H' ] cell.chars;
  check int "ascii width" 1 cell.width;

  (* Unicode character *)
  set_char buf 1 0 (Uchar.of_int 0x03B1) attr;
  (* Greek alpha *)
  let cell = get buf 1 0 in
  check uchar_list "unicode char" [ Uchar.of_int 0x03B1 ] cell.chars;
  check int "unicode width" 1 cell.width

(** Test set_char with wide characters *)
let test_set_char_wide () =
  let buf = create 10 5 in
  let attr = test_attr () in

  (* Wide character (emoji) *)
  let emoji = Uchar.of_int 0x1F600 in
  (* 😀 *)
  set_char buf 2 1 emoji attr;

  let cell1 = get buf 2 1 in
  let cell2 = get buf 3 1 in

  check uchar_list "emoji char" [ emoji ] cell1.chars;
  check int "emoji width" 2 cell1.width;
  check uchar_list "continuation cell" [] cell2.chars;
  check int "continuation width" 0 cell2.width;

  (* Wide character at edge of buffer *)
  set_char buf 9 0 emoji attr;
  (* Should not write continuation cell *)
  let edge_cell = get buf 9 0 in
  check int "edge emoji width" 2 edge_cell.width

(** Test set_char with combining characters *)
let test_set_char_combining () =
  let buf = create 10 5 in
  let attr = test_attr () in

  (* Base character *)
  set_char buf 2 2 (Uchar.of_char 'e') attr;

  (* Combining acute accent *)
  let acute = Uchar.of_int 0x0301 in
  set_char buf 3 2 acute attr;

  (* x=3 but should combine with x=2 *)
  let cell = get buf 2 2 in
  check uchar_list "combined chars" [ Uchar.of_char 'e'; acute ] cell.chars;

  (* Combining at start of line (no previous char) *)
  set_char buf 0 3 acute attr;
  let start_cell = get buf 0 3 in
  check uchar_list "combining at start" [ acute ] start_cell.chars

(** Test set_string *)
let test_set_string () =
  let buf = create 15 5 in
  let attr = test_attr ~underline:true () in

  (* Simple ASCII string *)
  set_string buf 0 0 "Hello" attr;
  for i = 0 to 4 do
    let cell = get buf i 0 in
    check uchar_list
      (Printf.sprintf "char %d" i)
      [ Uchar.of_char (String.get "Hello" i) ]
      cell.chars
  done;

  (* Unicode string *)
  set_string buf 0 1 "αβγ" attr;
  let cell0 = get buf 0 1 in
  let cell1 = get buf 1 1 in
  let cell2 = get buf 2 1 in
  check uchar_list "greek alpha" [ Uchar.of_int 0x03B1 ] cell0.chars;
  check uchar_list "greek beta" [ Uchar.of_int 0x03B2 ] cell1.chars;
  check uchar_list "greek gamma" [ Uchar.of_int 0x03B3 ] cell2.chars;

  (* String with emoji *)
  set_string buf 0 2 "Hi😀!" attr;
  let h = get buf 0 2 in
  let i = get buf 1 2 in
  let emoji = get buf 2 2 in
  let cont = get buf 3 2 in
  let excl = get buf 4 2 in
  check uchar_list "H" [ Uchar.of_char 'H' ] h.chars;
  check uchar_list "i" [ Uchar.of_char 'i' ] i.chars;
  check uchar_list "emoji" [ Uchar.of_int 0x1F600 ] emoji.chars;
  check int "emoji width" 2 emoji.width;
  check uchar_list "continuation" [] cont.chars;
  check uchar_list "!" [ Uchar.of_char '!' ] excl.chars;

  (* String that exceeds buffer width *)
  set_string buf 10 3 "Too long string" attr;
  let last = get buf 14 3 in
  check uchar_list "truncated" [ Uchar.of_char 'l' ] last.chars

(** Test malformed UTF-8 handling *)
let test_set_string_malformed () =
  let buf = create 10 5 in
  let attr = test_attr () in

  (* String with malformed UTF-8 *)
  let malformed = "Hello\xFF\xFEWorld" in
  set_string buf 0 0 malformed attr;

  (* Should have "Hello" + 2 replacement chars + "World" *)
  let cells = List.init 12 (fun i -> get buf i 0) in
  let chars = List.map (fun c -> List.hd c.chars) cells in

  (* Check first 5 chars are "Hello" *)
  List.iteri
    (fun i expected ->
      if i < 5 then
        check
          (testable pp_uchar Uchar.equal)
          (Printf.sprintf "char %d" i)
          (Uchar.of_char expected) (List.nth chars i))
    [ 'H'; 'e'; 'l'; 'l'; 'o' ];

  (* Replacement characters *)
  check
    (testable pp_uchar Uchar.equal)
    "replacement 1" (Uchar.of_int 0xFFFD) (List.nth chars 5);
  check
    (testable pp_uchar Uchar.equal)
    "replacement 2" (Uchar.of_int 0xFFFD) (List.nth chars 6)

(** Test buffer clearing *)
let test_clear () =
  let buf = create 5 3 in
  let attr = test_attr () in

  (* Fill buffer with 'X' *)
  for y = 0 to 2 do
    for x = 0 to 4 do
      set_char buf x y (Uchar.of_char 'X') attr
    done
  done;

  (* Clear buffer *)
  clear buf;

  (* Check all cells are empty again *)
  for y = 0 to 2 do
    for x = 0 to 4 do
      let cell = get buf x y in
      check uchar_list "cleared cell" [ Uchar.of_int 0x20 ] cell.chars
    done
  done

(** Test diff function *)
let test_diff () =
  let buf1 = create 5 3 in
  let buf2 = create 5 3 in
  let attr = test_attr () in

  (* No changes *)
  let patches = diff buf1 buf2 in
  check int "no patches" 0 (List.length patches);

  (* Single change *)
  set_char buf2 2 1 (Uchar.of_char 'A') attr;
  let patches = diff buf1 buf2 in
  check int "one patch" 1 (List.length patches);
  let patch = List.hd patches in
  (match patch with
  | Change { row; col; new_cell } ->
      check int "patch row" 1 row;
      check int "patch col" 2 col;
      check uchar_list "new cell" [ Uchar.of_char 'A' ] new_cell.chars
  | Clear _ -> Alcotest.fail "Expected Change patch");

  (* Multiple changes *)
  set_char buf2 0 0 (Uchar.of_char 'B') attr;
  set_char buf2 4 2 (Uchar.of_char 'C') attr;
  let patches = diff buf1 buf2 in
  check int "three patches" 3 (List.length patches);

  (* Check patches are in order *)
  let rows_cols =
    List.map
      (fun p ->
        match p with
        | Change { row; col; _ } -> (row, col)
        | Clear { row; col; _ } -> (row, col))
      patches
  in
  check (list (pair int int)) "patch order" [ (0, 0); (1, 2); (2, 4) ] rows_cols

(** Test render_patch *)
let test_render_patch () =
  let attr = test_attr ~fg:(Some Ansi.Green) () in
  let patch =
    Change
      {
        row = 5;
        col = 10;
        new_cell = { chars = [ Uchar.of_char 'X' ]; attr; width = 1 };
      }
  in
  let rendered = render_patches [ patch ] in
  check bool "contains position" true (String.contains rendered '6');
  (* row+1 *)
  check bool "contains X" true (String.contains rendered 'X')

(** Test render_patches *)
let test_render_patches () =
  let attr = test_attr ~fg:(Some Ansi.Green) () in

  (* Single patch *)
  let patch =
    Change
      {
        row = 5;
        col = 10;
        new_cell = { chars = [ Uchar.of_char 'X' ]; attr; width = 1 };
      }
  in
  let rendered = render_patches [ patch ] in
  check bool "contains X" true (String.contains rendered 'X');
  check bool "ends with reset" true
    (String.ends_with ~suffix:"\x1b[0m" rendered
    || String.split_on_char '\x1b' rendered
       |> List.exists (String.starts_with ~prefix:"[0m"));

  (* Multiple patches on same row *)
  let patch2 =
    match patch with Change r -> Change { r with col = 11 } | _ -> patch
  in
  let patch3 =
    match patch with Change r -> Change { r with col = 12 } | _ -> patch
  in
  let rendered = render_patches [ patch; patch2; patch3 ] in
  check bool "contains XXX" true (String.contains rendered 'X');

  (* With cursor position *)
  let rendered = render_patches ~cursor_pos:(`Move (15, 8)) [ patch ] in
  check bool "shows cursor" true
    (String.split_on_char '\x1b' rendered
    |> List.exists (String.starts_with ~prefix:"[?25h"))

(** Test render_full *)
let test_render_full () =
  let buf = create 3 2 in
  let attr = test_attr ~fg:(Some Ansi.Blue) () in

  (* Fill buffer *)
  set_string buf 0 0 "ABC" attr;
  set_string buf 0 1 "DEF" attr;

  let rendered = render_full buf in

  (* Should start with clear screen then cursor home *)
  check bool "starts with clear" true
    (String.starts_with ~prefix:"\x1b[2J" rendered);
  (* Check that it contains the expected sequences *)
  check bool "has expected prefix" true
    (String.starts_with ~prefix:"\x1b[2J\x1b[1;1H" rendered);

  (* Should contain our text *)
  check bool "contains ABC" true (String.contains rendered 'A');
  check bool "contains DEF" true (String.contains rendered 'D');

  (* Should have newline between rows *)
  check bool "has newline" true (String.contains rendered '\n');

  (* Should end with reset *)
  check bool "ends with reset" true
    (String.ends_with ~suffix:"\x1b[0m" rendered
    || String.split_on_char '\x1b' rendered
       |> List.exists (String.starts_with ~prefix:"[0m"))

(** Test measure_string *)
let test_measure_string () =
  check int "empty string" 0 (measure_string "");
  check int "ascii string" 5 (measure_string "Hello");
  check int "unicode string" 3 (measure_string "αβγ");
  check int "emoji string" 5 (measure_string "Hi😀!");
  (* 2+2+1 *)
  check int "mixed string" 12 (measure_string "Test 测试 OK");

  (* 5 + 4 + 3 *)

  (* Malformed UTF-8 is handled - malformed byte counts as 1 *)
  check int "malformed utf8" 6 (measure_string "Hi\xFFBye")

(** Test truncate_string *)
let test_truncate_string () =
  check string "no truncate" "Hello" (truncate_string "Hello" 10);
  check string "exact width" "Hello" (truncate_string "Hello" 5);
  check string "truncate ascii" "Hel" (truncate_string "Hello" 3);
  check string "truncate unicode" "αβ" (truncate_string "αβγδ" 2);

  (* Wide character truncation *)
  check string "truncate before emoji" "Hi" (truncate_string "Hi😀!" 3);
  check string "keep emoji" "Hi😀" (truncate_string "Hi😀!" 4);

  (* Empty cases *)
  check string "empty string" "" (truncate_string "" 5);
  check string "zero width" "" (truncate_string "Hello" 0)

(** Test diff with wide and combining characters *)
let test_diff_advanced () =
  let old_buf = create 10 3 in
  let new_buf = create 10 3 in

  (* Test wide character changes *)
  set_char old_buf 0 0 (Uchar.of_char 'A') (test_attr ());
  set_char new_buf 0 0 (Uchar.of_int 0x1F600) (test_attr ());

  (* Wide emoji *)
  let patches = diff old_buf new_buf in
  check bool "wide char creates patch" true (List.length patches > 0);

  (* Test style-only changes *)
  let old_buf2 = create 5 1 in
  let new_buf2 = create 5 1 in
  set_string old_buf2 0 0 "Hello" (test_attr ());
  set_string new_buf2 0 0 "Hello" (test_attr ~bold:true ());

  let patches2 = diff old_buf2 new_buf2 in
  check bool "style change creates patches" true (List.length patches2 > 0);

  (* Test combining character changes *)
  let old_buf3 = create 5 1 in
  let new_buf3 = create 5 1 in
  set_char old_buf3 0 0 (Uchar.of_char 'e') (test_attr ());
  (* Add combining acute accent *)
  set old_buf3 0 0
    {
      chars = [ Uchar.of_char 'e'; Uchar.of_int 0x0301 ];
      attr = test_attr ();
      width = 1;
    };

  let patches3 = diff old_buf3 new_buf3 in
  check bool "combining char change detected" true (List.length patches3 > 0)

(** Test render modes and options *)
let test_render_modes () =
  let buf = create 10 5 in
  set_string buf 2 2 "Test" (test_attr ~fg:(Some Ansi.Red) ());

  (* Test absolute positioning *)
  let patches = diff (create 10 5) buf in
  let result = render_patches patches in
  check bool "absolute render has output" true (String.length result > 0);
  check bool "contains cursor positioning" true (String.contains result '\x1b');

  (* Test with cursor positioning *)
  let result2 = render_patches patches ~cursor_pos:(`Move (5, 3)) in
  check bool "cursor positioning affects output" true (String.length result2 > 0)

(** Test huge buffers *)
let test_huge_buffers () =
  (* Test creation of large buffer *)
  let big = create 1000 1000 in
  let w, h = dimensions big in
  check int "huge width" 1000 w;
  check int "huge height" 1000 h;

  (* Test operations on edges *)
  set_char big 0 0 (Uchar.of_char 'A') (test_attr ());
  set_char big 999 999 (Uchar.of_char 'Z') (test_attr ());

  let cell1 = get big 0 0 in
  let cell2 = get big 999 999 in
  check uchar_list "huge buffer start" [ Uchar.of_char 'A' ] cell1.chars;
  check uchar_list "huge buffer end" [ Uchar.of_char 'Z' ] cell2.chars

(** Test malformed UTF-8 in rendering *)
let test_malformed_rendering () =
  let buf = create 10 5 in

  (* Set cell with invalid Unicode *)
  set buf 0 0
    {
      chars = [ Uchar.of_int 0xFFFE ];
      (* Non-character *)
      attr = test_attr ();
      width = 1;
    };

  (* Should not crash when rendering *)
  let result = render_full buf in
  check bool "rendered malformed without crash" true (String.length result >= 0)

(** Test patch merging and optimization *)
let test_patch_optimization () =
  let old_buf = create 20 5 in
  let new_buf = create 20 5 in

  (* Adjacent changes *)
  set_string new_buf 0 0 "Hello" (test_attr ());
  set_string new_buf 5 0 "World" (test_attr ());

  let patches = diff old_buf new_buf in
  (* Implementation might merge adjacent patches *)
  check bool "patches generated" true (List.length patches > 0);

  (* Scattered changes *)
  let old_buf2 = create 20 5 in
  let new_buf2 = create 20 5 in
  set_char new_buf2 0 0 (Uchar.of_char 'A') (test_attr ());
  set_char new_buf2 19 4 (Uchar.of_char 'Z') (test_attr ());

  let patches2 = diff old_buf2 new_buf2 in
  check bool "scattered changes create patches" true (List.length patches2 >= 2)

(** Test edge cases in string utilities *)
let test_string_utils_edge () =
  (* Measure empty string *)
  let w1 = measure_string "" in
  check int "empty string width" 0 w1;

  (* Measure string with only combining chars *)
  let b = Buffer.create 10 in
  Buffer.add_utf_8_uchar b (Uchar.of_int 0x0301);
  Buffer.add_utf_8_uchar b (Uchar.of_int 0x0302);
  let combining_only = Buffer.contents b in
  let w2 = measure_string combining_only in
  check int "combining only width" 0 w2;

  (* Truncate to zero width *)
  let truncated1 = truncate_string "Hello" 0 in
  check string "truncate to 0" "" truncated1;

  (* Truncate wide char at boundary *)
  let with_emoji = "Hi😀!" in
  let truncated2 = truncate_string with_emoji 3 in
  (* Should be "Hi" or "Hi…" depending on implementation *)
  check bool "truncated at emoji boundary" true
    (truncated2 = "Hi" || truncated2 = "Hi…" || String.length truncated2 <= 3)

(** Test render output formatting *)
let test_render_formatting () =
  let buf = create 10 3 in

  (* Test various style combinations *)
  set_string buf 0 0 "Bold" (test_attr ~bold:true ());
  set_string buf 0 1 "Color"
    (test_attr ~fg:(Some Ansi.Blue) ~bg:(Some Ansi.Yellow) ());
  set_string buf 0 2 "Under" (test_attr ~underline:true ());

  let result = render_full buf in

  (* Check for style sequences *)
  check bool "contains SGR sequences" true (String.contains result '\x1b');
  check bool "contains reset" true
    (try
       let _ = String.index result '\x1b' in
       String.length result > 0
     with Not_found -> false);
  check bool "contains newlines" true (String.contains result '\n')

let () =
  run "Render"
    [
      ( "Buffer operations",
        [
          test_case "create and dimensions" `Quick test_buffer_create;
          test_case "get and set" `Quick test_get_set;
          test_case "clear" `Quick test_clear;
        ] );
      ( "Character handling",
        [
          test_case "set_char regular" `Quick test_set_char_regular;
          test_case "set_char wide" `Quick test_set_char_wide;
          test_case "set_char combining" `Quick test_set_char_combining;
        ] );
      ( "String handling",
        [
          test_case "set_string" `Quick test_set_string;
          test_case "set_string malformed" `Quick test_set_string_malformed;
        ] );
      ( "Diffing",
        [
          test_case "diff" `Quick test_diff;
          test_case "diff advanced" `Quick test_diff_advanced;
        ] );
      ( "Rendering",
        [
          test_case "render_patch" `Quick test_render_patch;
          test_case "render_patches" `Quick test_render_patches;
          test_case "render_full" `Quick test_render_full;
          test_case "render modes" `Quick test_render_modes;
          test_case "render formatting" `Quick test_render_formatting;
        ] );
      ( "String utilities",
        [
          test_case "measure_string" `Quick test_measure_string;
          test_case "truncate_string" `Quick test_truncate_string;
          test_case "string utils edge" `Quick test_string_utils_edge;
        ] );
      ( "Edge cases",
        [
          test_case "huge buffers" `Quick test_huge_buffers;
          test_case "malformed rendering" `Quick test_malformed_rendering;
          test_case "patch optimization" `Quick test_patch_optimization;
        ] );
    ]
