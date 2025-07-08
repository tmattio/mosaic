open Alcotest
open Render

(** Helper to create a test style *)
let test_style ?(fg = None) ?(bg = None) ?(bold = false) ?(italic = false)
    ?(underline = false) ?(uri = None) () =
  {
    fg;
    bg;
    bold;
    dim = false;
    italic;
    underline;
    double_underline = false;
    blink = false;
    reverse = false;
    strikethrough = false;
    overline = false;
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
    {
      chars = [ Uchar.of_char 'A' ];
      style = test_style ~bold:true ();
      width = 1;
    }
  in

  (* Set a cell *)
  set buf 2 1 test_cell;

  (* Get the same cell *)
  let retrieved = get buf 2 1 in
  check uchar_list "chars match" [ Uchar.of_char 'A' ] retrieved.chars;
  check bool "bold matches" true retrieved.style.bold;
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
  let style = test_style ~fg:(Some Ansi.Red) () in

  (* ASCII character *)
  set_char buf 0 0 (Uchar.of_char 'H') style;
  let cell = get buf 0 0 in
  check uchar_list "ascii char" [ Uchar.of_char 'H' ] cell.chars;
  check int "ascii width" 1 cell.width;

  (* Unicode character *)
  set_char buf 1 0 (Uchar.of_int 0x03B1) style;
  (* Greek alpha *)
  let cell = get buf 1 0 in
  check uchar_list "unicode char" [ Uchar.of_int 0x03B1 ] cell.chars;
  check int "unicode width" 1 cell.width

(** Test set_char with wide characters *)
let test_set_char_wide () =
  let buf = create 10 5 in
  let style = test_style () in

  (* Wide character (emoji) *)
  let emoji = Uchar.of_int 0x1F600 in
  (* 😀 *)
  set_char buf 2 1 emoji style;

  let cell1 = get buf 2 1 in
  let cell2 = get buf 3 1 in

  check uchar_list "emoji char" [ emoji ] cell1.chars;
  check int "emoji width" 2 cell1.width;
  check uchar_list "continuation cell" [] cell2.chars;
  check int "continuation width" 0 cell2.width;

  (* Wide character at edge of buffer *)
  set_char buf 9 0 emoji style;
  (* Should not write continuation cell *)
  let edge_cell = get buf 9 0 in
  check int "edge emoji width" 2 edge_cell.width

(** Test set_char with combining characters *)
let test_set_char_combining () =
  let buf = create 10 5 in
  let style = test_style () in

  (* Base character *)
  set_char buf 2 2 (Uchar.of_char 'e') style;

  (* Combining acute accent *)
  let acute = Uchar.of_int 0x0301 in
  set_char buf 3 2 acute style;

  (* x=3 but should combine with x=2 *)
  let cell = get buf 2 2 in
  check uchar_list "combined chars" [ Uchar.of_char 'e'; acute ] cell.chars;

  (* Combining at start of line (no previous char) *)
  set_char buf 0 3 acute style;
  let start_cell = get buf 0 3 in
  check uchar_list "combining at start" [ acute ] start_cell.chars

(** Test set_string *)
let test_set_string () =
  let buf = create 15 5 in
  let style = test_style ~underline:true () in

  (* Simple ASCII string *)
  set_string buf 0 0 "Hello" style;
  for i = 0 to 4 do
    let cell = get buf i 0 in
    check uchar_list
      (Printf.sprintf "char %d" i)
      [ Uchar.of_char (String.get "Hello" i) ]
      cell.chars
  done;

  (* Unicode string *)
  set_string buf 0 1 "αβγ" style;
  let cell0 = get buf 0 1 in
  let cell1 = get buf 1 1 in
  let cell2 = get buf 2 1 in
  check uchar_list "greek alpha" [ Uchar.of_int 0x03B1 ] cell0.chars;
  check uchar_list "greek beta" [ Uchar.of_int 0x03B2 ] cell1.chars;
  check uchar_list "greek gamma" [ Uchar.of_int 0x03B3 ] cell2.chars;

  (* String with emoji *)
  set_string buf 0 2 "Hi😀!" style;
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
  set_string buf 10 3 "Too long string" style;
  let last = get buf 14 3 in
  check uchar_list "truncated" [ Uchar.of_char 'l' ] last.chars

(** Test malformed UTF-8 handling *)
let test_set_string_malformed () =
  let buf = create 10 5 in
  let style = test_style () in

  (* String with malformed UTF-8 *)
  let malformed = "Hello\xFF\xFEWorld" in
  set_string buf 0 0 malformed style;

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
  let style = test_style () in

  (* Fill buffer with 'X' *)
  for y = 0 to 2 do
    for x = 0 to 4 do
      set_char buf x y (Uchar.of_char 'X') style
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
  let style = test_style () in

  (* No changes *)
  let patches = diff buf1 buf2 in
  check int "no patches" 0 (List.length patches);

  (* Single change *)
  set_char buf2 2 1 (Uchar.of_char 'A') style;
  let patches = diff buf1 buf2 in
  check int "one patch" 1 (List.length patches);
  let patch = List.hd patches in
  check int "patch row" 1 patch.row;
  check int "patch col" 2 patch.col;
  check uchar_list "old cell" [ Uchar.of_int 0x20 ] patch.old_cell.chars;
  check uchar_list "new cell" [ Uchar.of_char 'A' ] patch.new_cell.chars;

  (* Multiple changes *)
  set_char buf2 0 0 (Uchar.of_char 'B') style;
  set_char buf2 4 2 (Uchar.of_char 'C') style;
  let patches = diff buf1 buf2 in
  check int "three patches" 3 (List.length patches);

  (* Check patches are in order *)
  let rows_cols = List.map (fun p -> (p.row, p.col)) patches in
  check (list (pair int int)) "patch order" [ (0, 0); (1, 2); (2, 4) ] rows_cols

(** Test diff with mismatched dimensions *)
let test_diff_mismatch () =
  let buf1 = create 5 3 in
  let buf2 = create 6 3 in

  check_raises "width mismatch"
    (Invalid_argument "Buffer dimensions must match: old=(5,3), new=(6,3)")
    (fun () -> ignore (diff buf1 buf2));

  let buf3 = create 5 4 in
  check_raises "height mismatch"
    (Invalid_argument "Buffer dimensions must match: old=(5,3), new=(5,4)")
    (fun () -> ignore (diff buf1 buf3))

(** Test render_patch *)
let test_render_patch () =
  let style = test_style ~fg:(Some Ansi.Green) () in
  let patch =
    {
      row = 5;
      col = 10;
      old_cell = empty_cell;
      new_cell = { chars = [ Uchar.of_char 'X' ]; style; width = 1 };
    }
  in
  let rendered = render_patch patch in
  check bool "contains position" true (String.contains rendered '6');
  (* row+1 *)
  check bool "contains X" true (String.contains rendered 'X')

(** Test render_patches *)
let test_render_patches () =
  let style = test_style ~fg:(Some Ansi.Green) () in

  (* Single patch *)
  let patch =
    {
      row = 5;
      col = 10;
      old_cell = empty_cell;
      new_cell = { chars = [ Uchar.of_char 'X' ]; style; width = 1 };
    }
  in
  let rendered = render_patches [ patch ] in
  check bool "contains X" true (String.contains rendered 'X');
  check bool "ends with reset" true
    (String.ends_with ~suffix:"\x1b[0m" rendered
    || String.split_on_char '\x1b' rendered
       |> List.exists (String.starts_with ~prefix:"[0m"));

  (* Multiple patches on same row *)
  let patch2 = { patch with col = 11 } in
  let patch3 = { patch with col = 12 } in
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
  let style = test_style ~fg:(Some Ansi.Blue) () in

  (* Fill buffer *)
  set_string buf 0 0 "ABC" style;
  set_string buf 0 1 "DEF" style;

  let rendered = render_full buf in

  (* Should start with cursor home and clear *)
  check bool "starts with home" true
    (String.starts_with ~prefix:"\x1b[1;1H" rendered);
  check bool "contains clear" true (String.contains rendered 'J');

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

  (* Malformed UTF-8 is handled *)
  check int "malformed utf8" 5 (measure_string "Hi\xFFBye")

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
          test_case "diff mismatch" `Quick test_diff_mismatch;
        ] );
      ( "Rendering",
        [
          test_case "render_patch" `Quick test_render_patch;
          test_case "render_patches" `Quick test_render_patches;
          test_case "render_full" `Quick test_render_full;
        ] );
      ( "String utilities",
        [
          test_case "measure_string" `Quick test_measure_string;
          test_case "truncate_string" `Quick test_truncate_string;
        ] );
    ]
