open Alcotest
open Glyph

(* Helpers *)

let encode_to_list pool ~width_method ~tab_width str =
  let lst = ref [] in
  encode pool ~width_method ~tab_width str (fun cell -> lst := cell :: !lst);
  List.rev !lst

let check_width msg expected actual = check int msg expected actual

(* 1. Conformance & Segmentation *)

let grapheme_break_test_path () =
  let here = Filename.dirname Sys.argv.(0) in
  let rec find_root dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else find_root parent
  in
  let root = Option.value (find_root (Sys.getcwd ())) ~default:here in
  let candidates =
    [
      Filename.concat here "data/GraphemeBreakTest.txt";
      Filename.concat root "matrix/test/data/GraphemeBreakTest.txt";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None -> Alcotest.fail "Could not find data/GraphemeBreakTest.txt"

let uchar_to_utf8 cp =
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf (Uchar.of_int cp);
  Buffer.contents buf

let run_grapheme_conformance () =
  let path = grapheme_break_test_path () in
  let ic = open_in path in
  try
    let rec loop lineno =
      try
        let line = input_line ic in
        let rule =
          match String.index_opt line '#' with
          | Some i -> String.sub line 0 i |> String.trim
          | None -> String.trim line
        in
        if rule <> "" then (
          let parts =
            String.split_on_char ' ' rule |> List.filter (( <> ) "")
          in
          (* Reconstruct full text and expected segments *)
          let full_text = Buffer.create 32 in
          let expected_segments = ref [] in
          let current_segment = Buffer.create 16 in

          let rec parse = function
            | "÷" :: rest ->
                if Buffer.length current_segment > 0 then (
                  expected_segments :=
                    Buffer.contents current_segment :: !expected_segments;
                  Buffer.clear current_segment);
                parse rest
            | "×" :: rest -> parse rest
            | hex :: rest ->
                let cp = int_of_string ("0x" ^ hex) in
                let s = uchar_to_utf8 cp in
                Buffer.add_string full_text s;
                Buffer.add_string current_segment s;
                parse rest
            | [] ->
                if Buffer.length current_segment > 0 then
                  expected_segments :=
                    Buffer.contents current_segment :: !expected_segments
          in
          parse parts;

          let text = Buffer.contents full_text in
          let expected = List.rev !expected_segments in

          let actual = ref [] in
          iter_graphemes
            (fun ~offset:off ~len ->
              actual := String.sub text off len :: !actual)
            text;
          let actual = List.rev !actual in

          if expected <> actual then (
            Printf.printf
              "Line %d failure:\nInput: %s\nExpected: %s\nActual:   %s\n" lineno
              rule
              (String.concat "|" expected)
              (String.concat "|" actual);
            fail "Grapheme break mismatch"));
        loop (lineno + 1)
      with End_of_file -> ()
    in
    loop 1
  with e ->
    close_in ic;
    raise e

(* 2. Measurement Semantics *)

type measure_case = {
  name : string;
  input : string;
  expected : int;
  method_ : width_method;
  tab_width : int;
}

let measurement_semantics () =
  let cases =
    [
      (* ASCII *)
      {
        name = "basic_ascii";
        input = "abc";
        expected = 3;
        method_ = `Unicode;
        tab_width = 8;
      };
      {
        name = "ascii_controls";
        input = "a\n\r\x00b";
        expected = 2;
        method_ = `Unicode;
        tab_width = 8;
      };
      (* Tabs *)
      {
        name = "tab_default";
        input = "\t";
        expected = 2;
        method_ = `Unicode;
        tab_width = 2;
      };
      {
        name = "tab_custom";
        input = "\t";
        expected = 4;
        method_ = `Unicode;
        tab_width = 4;
      };
      (* Unicode Standard *)
      {
        name = "emoji_simple";
        input = "👋";
        expected = 2;
        method_ = `Unicode;
        tab_width = 8;
      };
      {
        name = "emoji_zwj_seq";
        input = "👩\u{200D}🚀";
        expected = 2;
        method_ = `Unicode;
        tab_width = 8;
      };
      {
        name = "emoji_family";
        input = "👨\u{200D}👩\u{200D}👧\u{200D}👦";
        expected = 2;
        method_ = `Unicode;
        tab_width = 8;
      };
      {
        name = "regional_indicator";
        input = "🇺🇸";
        expected = 2;
        method_ = `Unicode;
        tab_width = 8;
      };
      {
        name = "skin_tone";
        input = "👍🏽";
        expected = 2;
        method_ = `Unicode;
        tab_width = 8;
      };
      (* Strategies *)
      {
        name = "no_zwj_strategy";
        input = "👩\u{200D}🚀";
        expected = 4;
        method_ = `No_zwj;
        tab_width = 8;
      };
      {
        name = "wcwidth_strategy";
        input = "👩\u{200D}🚀";
        expected = 4;
        method_ = `Wcwidth;
        tab_width = 8;
      };
      (* Edge Cases *)
      {
        name = "combining_accents";
        input = "a\u{0301}";
        expected = 1;
        method_ = `Unicode;
        tab_width = 8;
      };
      {
        name = "zero_width_control";
        input = "\u{200B}";
        expected = 0;
        method_ = `Unicode;
        tab_width = 8;
      };
    ]
  in

  List.iter
    (fun c ->
      let w = measure ~width_method:c.method_ ~tab_width:c.tab_width c.input in
      check_width c.name c.expected w)
    cases

let ascii_fast_path_consistency () =
  let s = "hello\tworld" in
  let unicode_w = measure ~width_method:`Unicode ~tab_width:4 s in
  let no_zwj_w = measure ~width_method:`No_zwj ~tab_width:4 s in
  let wcwidth_w = measure ~width_method:`Wcwidth ~tab_width:4 s in
  check_width "unicode vs no_zwj" unicode_w no_zwj_w;
  check_width "unicode vs wcwidth" unicode_w wcwidth_w

(* Regression test: measure must iterate grapheme-by-grapheme and sum widths.
   Previously, measure for Unicode mode called calculate_width on the entire
   string, but calculate_width is designed for single grapheme clusters and
   only returned the first grapheme's width. *)
let measure_multi_grapheme_regression () =
  (* ASCII + wide characters: "Hello 世界" should be 6 + 2 + 2 = 10 *)
  let s = "Hello 世界" in
  let w = measure ~width_method:`Unicode ~tab_width:2 s in
  check_width "ascii + wide chars" 10 w;

  (* Multiple wide characters *)
  let s2 = "日本語" in
  let w2 = measure ~width_method:`Unicode ~tab_width:2 s2 in
  check_width "three wide chars" 6 w2;

  (* Mixed: emoji + ascii + wide *)
  let s3 = "🚀Hi世" in
  let w3 = measure ~width_method:`Unicode ~tab_width:2 s3 in
  (* 🚀=2, H=1, i=1, 世=2 = 6 *)
  check_width "emoji + ascii + wide" 6 w3;

  (* Verify consistency with Wcwidth mode *)
  let w_wcwidth = measure ~width_method:`Wcwidth ~tab_width:2 s in
  check_width "unicode matches wcwidth for simple case" w w_wcwidth

let no_zwj_segmentation () =
  let pool = create_pool () in
  let text = "👩\u{200D}🚀" in
  let cells = encode_to_list pool ~width_method:`No_zwj ~tab_width:2 text in
  let starts = List.filter is_start cells in
  check int "two grapheme starts" 2 (List.length starts);
  check int "four cells" 4 (List.length cells);
  let total =
    List.fold_left
      (fun acc g -> if is_start g then acc + width ~tab_width:8 g else acc)
      0 cells
  in
  check_width "total width preserved (starts only)" 4 total

(* 3. Encoding & Storage *)

let simple_vs_complex_optimization () =
  let pool = create_pool () in
  let check_type str expect_simple =
    let lst = encode_to_list pool ~width_method:`Unicode ~tab_width:2 str in
    match lst with
    | [ g ] ->
        if is_simple g <> expect_simple then
          fail
            (Printf.sprintf "Expected %s to be %s" str
               (if expect_simple then "Simple" else "Complex"))
    | [] -> fail (Printf.sprintf "Expected glyph for %s" str)
    | _ -> fail (Printf.sprintf "Expected 1 glyph for %s" str)
  in
  check_type "a" true;
  check_type "\t" true;
  (* Tabs remain simple/ASCII encoded; control bytes like DEL are dropped *)
  let del_cells =
    encode_to_list pool ~width_method:`Unicode ~tab_width:2 "\x7F"
  in
  check int "DEL is zero-width and skipped" 0 (List.length del_cells);
  check_type "é" false;
  (* Latin-1 supplement > 127 is complex *)
  check_type "€" false

let zero_length_intern () =
  let pool = create_pool () in
  let g = intern pool "" in
  check bool "empty glyph" true (is_empty g);
  check int "empty width" 0 (width g);
  (* Empty maps to a NUL sentinel: zero width, single-byte payload *)
  check int "empty len" 1 (length pool g);
  check string "empty string" "\000" (to_string pool g)

let complex_clustering_behavior () =
  let pool = create_pool () in
  (* Devanagari: "Namaste" *)
  (* 'Na'(1) + 'Ma'(1) + 'S'(1)+Virama(0)+'Ta'(1)+Matra(0) -> fused cluster 'Ste'(2) *)
  let text = "नमस्ते" in
  let cells = encode_to_list pool ~width_method:`Unicode ~tab_width:2 text in

  match cells with
  | [ na; _ma; ste_start; ste_cont ] ->
      check_width "Na width" 1 (width na);
      check bool "Na simple" false (is_simple na);

      check_width "Ste width" 2 (width ste_start);
      check bool "Ste start" true (is_start ste_start);

      check bool "Ste cont flag" true (is_continuation ste_cont);
      check_width "Ste cont width" 2 (width ste_cont);

      (* Verify visual re-assembly *)
      let buf = Bytes.create 32 in
      let len = blit pool ste_start buf 0 in
      let s_out = Bytes.sub_string buf 0 len in
      check string "Ste content" "स्ते" s_out
  | _ -> fail "Unexpected clustering result for Devanagari"

let malformed_utf8_resilience () =
  let pool = create_pool () in
  (* Truncated sequence: C3 (start of 2-byte seq) without second byte *)
  let invalid = "\xC3" in
  try
    let cells =
      encode_to_list pool ~width_method:`Unicode ~tab_width:2 invalid
    in
    let w = List.fold_left (fun acc g -> acc + width g) 0 cells in
    check bool "produced output without crash" true (w >= 0)
  with _ -> fail "Crashed on invalid UTF-8"

(* 4. Lifecycle & Pool *)

let lifecycle_generation_safety () =
  let pool = create_pool () in
  (* 1. Allocate a complex glyph *)
  let g1 = intern pool "🚀" in
  check bool "is complex" false (is_simple g1);

  (* 2. Verify existence *)
  check string "content exists" "🚀" (to_string pool g1);

  (* 3. Decrement to free it *)
  decref pool g1;

  (* 4. Allocate NEW glyph. May reuse slot but bumps generation. *)
  let g2 = intern pool "🛸" in
  check string "new content correct" "🛸" (to_string pool g2);

  (* 5. SAFETY CHECK: Accessing g1 (Stale) should NOT return "🛸" *)
  check string "stale string empty" "" (to_string pool g1);
  let buf = Bytes.create 8 in
  check int "stale blit is empty" 0 (blit pool g1 buf 0)

let pool_resize_stress () =
  let pool = create_pool () in
  let n = 5000 in
  (* Exceeds default 4096 *)
  let handles = Array.make n 0 in

  (* Fill pool *)
  for i = 0 to n - 1 do
    (* Use unique strings to force separate allocations *)
    let s = "x" ^ string_of_int i in
    handles.(i) <- intern pool s
  done;

  (* Verify random access after resize *)
  let g_first = handles.(0) in
  check string "first element intact" "x0" (to_string pool g_first);

  let g_last = handles.(n - 1) in
  check string "last element intact"
    ("x" ^ string_of_int (n - 1))
    (to_string pool g_last)

let copy_safety () =
  let p1 = create_pool () in
  let p2 = create_pool () in

  let g = intern p1 "abc" in
  (* Simple *)
  let g_copy = copy p1 g p2 in
  check int "simple copy identity" g g_copy;

  let complex = intern p1 "🚀" in
  (* Consume complex, become Stale in p1 *)
  decref p1 complex;

  (* Force slot reuse to bump generation, then copying stale should return empty *)
  let _reused = intern p1 "reused" in
  let stale_copy = copy p1 complex p2 in
  check bool "copied stale is empty" true (is_empty stale_copy)

let decref_deduplicates_free_list () =
  (* Test that multiple decrements don't corrupt the pool *)
  let pool = create_pool () in
  let g = intern pool "wide" in
  incref pool g;
  incref pool g;
  decref pool g;
  decref pool g;
  decref pool g;
  (* Should be able to allocate new glyphs without issues *)
  let g2 = intern pool "next" in
  let g3 = intern pool "another" in
  check string "g2 content" "next" (to_string pool g2);
  check string "g3 content" "another" (to_string pool g3);
  (* Stale reference should return empty *)
  check string "stale g empty" "" (to_string pool g)

(* 5. API Surface *)

let tab_expansion_mechanics () =
  let pool = create_pool () in
  (* Test mixing tabs with content *)
  let cells_2 =
    encode_to_list pool ~width_method:`Unicode ~tab_width:2 "a\tb"
  in
  let w_2 =
    List.fold_left (fun acc g -> acc + width ~tab_width:2 g) 0 cells_2
  in
  check int "width with tab=2" (1 + 2 + 1) w_2;

  let cells_4 =
    encode_to_list pool ~width_method:`Unicode ~tab_width:4 "a\tb"
  in
  let w_4 =
    List.fold_left (fun acc g -> acc + width ~tab_width:4 g) 0 cells_4
  in
  check int "width with tab=4" (1 + 4 + 1) w_4;

  (* Ensure the tab glyph itself reports the correct width dynamically *)
  let tab_glyph = List.nth cells_4 1 in
  check int "tab glyph dynamic width" 4 (width ~tab_width:4 tab_glyph);
  check int "tab glyph dynamic width re-check" 8 (width ~tab_width:8 tab_glyph)

let tab_cache_offsets () =
  let pool = create_pool () in
  let cells =
    encode_to_list pool ~width_method:`Unicode ~tab_width:4 "a\tb\tc"
  in
  match cells with
  | [ _a; tab1; _b; tab2; _c ] ->
      check bool "tab1 simple" true (is_simple tab1);
      check bool "tab2 simple" true (is_simple tab2);
      check int "tab1 width" 4 (width ~tab_width:4 tab1);
      check int "tab2 width" 4 (width ~tab_width:4 tab2)
  | _ -> fail "Expected five glyphs with tabs at positions 1 and 3"

let max_width_clamping () =
  let pool = create_pool () in
  (* Force a width > 4 on a multi-byte grapheme; bit layout clamps to max 4. *)
  let g = intern pool ~width:10 "ab" in
  check int "clamps large width" 4 (width g)

let continuation_width_encoding () =
  let pool = create_pool () in
  let cells = encode_to_list pool ~width_method:`Unicode ~tab_width:2 "界" in
  (* CJK wide character -> width 2 and a continuation cell *)
  match cells with
  | [ start; cont ] ->
      check_width "start width" 2 (width start);
      check bool "start is start" true (is_start start);
      check bool "cont flag" true (is_continuation cont);
      check_width "cont width matches" 2 (width cont)
  | _ -> fail "Expected start+continuation for wide glyph"

(* 6. Text Segmentation (wrap_breaks / line_breaks) *)

(* Local test-only types for easy comparison *)
type wrap_break = { byte_offset : int; grapheme_offset : int }
type line_break = { pos : int; kind : line_break_kind }

(* Local helper: convert iter_wrap_breaks to array for tests *)
let wrap_breaks ?(width_method = `Unicode) s =
  let acc = ref [] in
  iter_wrap_breaks ~width_method
    (fun ~byte_offset ~grapheme_offset ->
      acc := { byte_offset; grapheme_offset } :: !acc)
    s;
  Array.of_list (List.rev !acc)

(* Local helper: convert iter_line_breaks to list for tests *)
let line_breaks_to_list s =
  let acc = ref [] in
  iter_line_breaks (fun ~pos ~kind -> acc := { pos; kind } :: !acc) s;
  List.rev !acc

let wrap_break_testable =
  Alcotest.testable
    (fun ppf { byte_offset; grapheme_offset } ->
      Format.fprintf ppf "{byte=%d; grapheme=%d}" byte_offset grapheme_offset)
    ( = )

let line_break_testable =
  Alcotest.testable
    (fun ppf { pos; kind } ->
      let kind_s =
        match kind with `LF -> "LF" | `CR -> "CR" | `CRLF -> "CRLF"
      in
      Format.fprintf ppf "{pos=%d; kind=%s}" pos kind_s)
    ( = )

let wrap_breaks_ascii_spaces () =
  (* Break after each space *)
  let breaks = wrap_breaks "hello world test" in
  check int "two breaks for two spaces" 2 (Array.length breaks);
  check wrap_break_testable "first break after 'hello '"
    { byte_offset = 6; grapheme_offset = 5 }
    breaks.(0);
  check wrap_break_testable "second break after 'world '"
    { byte_offset = 12; grapheme_offset = 11 }
    breaks.(1)

let wrap_breaks_ascii_punctuation () =
  (* Breaks after punctuation: - / . , ; : ! ? *)
  let breaks = wrap_breaks "a-b/c.d,e;f:g!h?i" in
  (* Each punctuation mark should create a break *)
  check int "8 breaks for 8 punctuation marks" 8 (Array.length breaks);
  (* First break after 'a-' at byte 2, grapheme 1 *)
  check wrap_break_testable "break after hyphen"
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_ascii_brackets () =
  (* Breaks after brackets: ( ) [ ] { } *)
  let breaks = wrap_breaks "(a)[b]{c}" in
  check int "6 breaks for 6 brackets" 6 (Array.length breaks)

let wrap_breaks_tabs () =
  let breaks = wrap_breaks "a\tb\tc" in
  check int "2 breaks for 2 tabs" 2 (Array.length breaks);
  check wrap_break_testable "break after first tab"
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_unicode_spaces () =
  (* NBSP (U+00A0), ZWSP (U+200B), Ideographic space (U+3000) *)
  let nbsp = "\xC2\xA0" in
  (* U+00A0 *)
  let zwsp = "\xE2\x80\x8B" in
  (* U+200B *)
  let ideographic = "\xE3\x80\x80" in
  (* U+3000 *)

  let breaks_nbsp = wrap_breaks ("a" ^ nbsp ^ "b") in
  check int "break after NBSP" 1 (Array.length breaks_nbsp);

  let breaks_zwsp = wrap_breaks ("a" ^ zwsp ^ "b") in
  check int "break after ZWSP" 1 (Array.length breaks_zwsp);

  let breaks_ideo = wrap_breaks ("a" ^ ideographic ^ "b") in
  check int "break after ideographic space" 1 (Array.length breaks_ideo)

let wrap_breaks_soft_hyphen () =
  (* U+00AD soft hyphen *)
  let soft_hyphen = "\xC2\xAD" in
  let breaks = wrap_breaks ("word" ^ soft_hyphen ^ "break") in
  check int "break after soft hyphen" 1 (Array.length breaks)

let wrap_breaks_en_space_range () =
  (* U+2000 to U+200A are all break opportunities *)
  let en_quad = "\xE2\x80\x80" in
  (* U+2000 *)
  let hair_space = "\xE2\x80\x8A" in
  (* U+200A *)

  let breaks1 = wrap_breaks ("a" ^ en_quad ^ "b") in
  check int "break after EN QUAD" 1 (Array.length breaks1);

  let breaks2 = wrap_breaks ("a" ^ hair_space ^ "b") in
  check int "break after HAIR SPACE" 1 (Array.length breaks2)

let wrap_breaks_no_break_in_plain_text () =
  let breaks = wrap_breaks "helloworld" in
  check int "no breaks in continuous text" 0 (Array.length breaks)

let wrap_breaks_grapheme_aware () =
  (* Emoji with skin tone modifier is one grapheme *)
  let emoji = "👍🏽" in
  (* thumbs up + skin tone *)
  let breaks = wrap_breaks ("a " ^ emoji ^ " b") in
  (* Should have breaks after "a " and after emoji+" " *)
  check int "breaks respect grapheme boundaries" 2 (Array.length breaks);
  (* grapheme_offset should count the emoji as 1 grapheme *)
  check wrap_break_testable "first break"
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_empty_string () =
  let breaks = wrap_breaks "" in
  check int "no breaks in empty string" 0 (Array.length breaks)

let wrap_breaks_width_method_no_zwj () =
  (* With No_zwj, ZWJ sequences are split into separate graphemes *)
  let zwj_seq = "👩\u{200D}🚀" in
  (* woman + ZWJ + rocket *)

  let breaks_unicode = wrap_breaks ~width_method:`Unicode zwj_seq in
  let breaks_no_zwj = wrap_breaks ~width_method:`No_zwj zwj_seq in

  (* In Unicode mode: 1 grapheme, no breaks (no break chars) *)
  check int "unicode: no breaks in ZWJ sequence" 0 (Array.length breaks_unicode);
  (* In No_zwj mode: ZWJ (U+200D) could be treated as separate, but it's not a wrap break char *)
  (* The grapheme count will differ though *)
  check int "no_zwj: still no breaks (ZWJ not a wrap break char)" 0
    (Array.length breaks_no_zwj)

let line_breaks_lf () =
  let breaks = line_breaks_to_list "a\nb\nc" in
  check int "two LF breaks" 2 (List.length breaks);
  check line_break_testable "first LF" { pos = 1; kind = `LF }
    (List.nth breaks 0);
  check line_break_testable "second LF" { pos = 3; kind = `LF }
    (List.nth breaks 1)

let line_breaks_cr () =
  let breaks = line_breaks_to_list "a\rb\rc" in
  check int "two CR breaks" 2 (List.length breaks);
  check line_break_testable "first CR" { pos = 1; kind = `CR }
    (List.nth breaks 0);
  check line_break_testable "second CR" { pos = 3; kind = `CR }
    (List.nth breaks 1)

let line_breaks_crlf () =
  let breaks = line_breaks_to_list "a\r\nb\r\nc" in
  check int "two CRLF breaks" 2 (List.length breaks);
  (* CRLF reports at LF position *)
  check line_break_testable "first CRLF" { pos = 2; kind = `CRLF }
    (List.nth breaks 0);
  check line_break_testable "second CRLF" { pos = 5; kind = `CRLF }
    (List.nth breaks 1)

let line_breaks_mixed () =
  let breaks = line_breaks_to_list "a\nb\r\nc\rd" in
  check int "three mixed breaks" 3 (List.length breaks);
  check line_break_testable "LF" { pos = 1; kind = `LF } (List.nth breaks 0);
  check line_break_testable "CRLF" { pos = 4; kind = `CRLF } (List.nth breaks 1);
  check line_break_testable "CR" { pos = 6; kind = `CR } (List.nth breaks 2)

let line_breaks_empty () =
  let breaks = line_breaks_to_list "" in
  check int "no breaks in empty" 0 (List.length breaks)

let line_breaks_no_newlines () =
  let breaks = line_breaks_to_list "hello world" in
  check int "no breaks without newlines" 0 (List.length breaks)

let line_breaks_consecutive_lf () =
  let breaks = line_breaks_to_list "a\n\n\nb" in
  check int "three consecutive LF" 3 (List.length breaks);
  check line_break_testable "first" { pos = 1; kind = `LF } (List.nth breaks 0);
  check line_break_testable "second" { pos = 2; kind = `LF } (List.nth breaks 1);
  check line_break_testable "third" { pos = 3; kind = `LF } (List.nth breaks 2)

let tests =
  [
    ( "Conformance",
      [
        test_case "UAX #29 Grapheme Boundaries" `Quick run_grapheme_conformance;
      ] );
    ( "Text Segmentation",
      [
        test_case "wrap breaks: ASCII spaces" `Quick wrap_breaks_ascii_spaces;
        test_case "wrap breaks: ASCII punctuation" `Quick
          wrap_breaks_ascii_punctuation;
        test_case "wrap breaks: ASCII brackets" `Quick
          wrap_breaks_ascii_brackets;
        test_case "wrap breaks: tabs" `Quick wrap_breaks_tabs;
        test_case "wrap breaks: Unicode spaces" `Quick
          wrap_breaks_unicode_spaces;
        test_case "wrap breaks: soft hyphen" `Quick wrap_breaks_soft_hyphen;
        test_case "wrap breaks: EN space range" `Quick
          wrap_breaks_en_space_range;
        test_case "wrap breaks: no break in plain text" `Quick
          wrap_breaks_no_break_in_plain_text;
        test_case "wrap breaks: grapheme aware" `Quick
          wrap_breaks_grapheme_aware;
        test_case "wrap breaks: empty string" `Quick wrap_breaks_empty_string;
        test_case "wrap breaks: width_method No_zwj" `Quick
          wrap_breaks_width_method_no_zwj;
        test_case "line breaks: LF" `Quick line_breaks_lf;
        test_case "line breaks: CR" `Quick line_breaks_cr;
        test_case "line breaks: CRLF" `Quick line_breaks_crlf;
        test_case "line breaks: mixed" `Quick line_breaks_mixed;
        test_case "line breaks: empty" `Quick line_breaks_empty;
        test_case "line breaks: no newlines" `Quick line_breaks_no_newlines;
        test_case "line breaks: consecutive LF" `Quick
          line_breaks_consecutive_lf;
      ] );
    ( "Measurement Semantics",
      [
        test_case "width tables" `Quick measurement_semantics;
        test_case "ascii fast path" `Quick ascii_fast_path_consistency;
        test_case "multi-grapheme measure" `Quick
          measure_multi_grapheme_regression;
        test_case "no zwj segmentation" `Quick no_zwj_segmentation;
      ] );
    ( "Encoding & Storage",
      [
        test_case "simple vs complex" `Quick simple_vs_complex_optimization;
        test_case "zero length intern" `Quick zero_length_intern;
        test_case "complex clustering" `Quick complex_clustering_behavior;
        test_case "malformed utf8" `Quick malformed_utf8_resilience;
      ] );
    ( "Lifecycle & Pool",
      [
        test_case "generation safety" `Quick lifecycle_generation_safety;
        test_case "pool resize" `Quick pool_resize_stress;
        test_case "copy safety" `Quick copy_safety;
        test_case "decref deduplication" `Quick decref_deduplicates_free_list;
      ] );
    ( "API Surface",
      [
        test_case "tab expansion" `Quick tab_expansion_mechanics;
        test_case "tab cache offsets" `Quick tab_cache_offsets;
        test_case "max width clamping" `Quick max_width_clamping;
        test_case "continuation width encoding" `Quick
          continuation_width_encoding;
      ] );
  ]

let () = run "matrix.glyph" tests
