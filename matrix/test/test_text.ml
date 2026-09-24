open Windtrap
open Matrix_text

let check_width msg expected actual = equal ~msg int expected actual

let expect_invalid_arg msg f =
  try
    f ();
    fail (msg ^ ": expected Invalid_argument")
  with Invalid_argument _ -> ()

let uchar_to_utf8 cp =
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf (Uchar.of_int cp);
  Buffer.contents buf

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
  | None -> fail "Could not find data/GraphemeBreakTest.txt"

let run_grapheme_conformance () =
  let path = grapheme_break_test_path () in
  let ic = open_in path in
  try
    let rec loop lineno =
      match input_line ic with
      | exception End_of_file -> ()
      | line ->
          let rule =
            match Stdlib.String.index_opt line '#' with
            | Some i -> Stdlib.String.sub line 0 i |> Stdlib.String.trim
            | None -> Stdlib.String.trim line
          in
          if rule <> "" then (
            let parts =
              Stdlib.String.split_on_char ' ' rule |> List.filter (( <> ) "")
            in
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
                  let s = uchar_to_utf8 (int_of_string ("0x" ^ hex)) in
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
                actual := Stdlib.String.sub text off len :: !actual)
              text;
            let actual = List.rev !actual in
            if expected <> actual then
              fail
                (Printf.sprintf "Grapheme break mismatch on line %d: %s" lineno
                   rule));
          loop (lineno + 1)
    in
    loop 1;
    close_in ic
  with exn ->
    close_in_noerr ic;
    raise exn

(* One test per row: a failure names the row it came from, and a single
   character class can be selected with -f. *)
let measurement_rows =
  [
    ("basic ASCII", "abc", `Unicode, 8, 3);
    ("ASCII controls", "a\n\r\x00b", `Unicode, 8, 2);
    ("tab default", "\t", `Unicode, 2, 2);
    ("tab custom", "\t", `Unicode, 4, 4);
    (* A non-positive tab_width falls back to the default rather than
       measuring the tab as nothing. Zero is the value that tells the clamp
       from its off-by-one: `w < 0` would let 0 through as a width. *)
    ("tab width zero", "\t", `Unicode, 0, 2);
    ("emoji simple", "👋", `Unicode, 8, 2);
    ("emoji ZWJ sequence", "👩\u{200D}🚀", `Unicode, 8, 2);
    ("emoji family", "👨\u{200D}👩\u{200D}👧\u{200D}👦", `Unicode, 8, 2);
    ("regional indicator pair", "🇺🇸", `Unicode, 8, 2);
    ("skin tone sequence", "👍🏽", `Unicode, 8, 2);
    ("wcwidth strategy", "👩\u{200D}🚀", `Wcwidth, 8, 4);
    ("combining accent", "a\u{0301}", `Unicode, 8, 1);
    ("Thai spacing mark", "น้ำ", `Unicode, 8, 2);
    ("Thai spacing mark wcwidth", "น้ำ", `Wcwidth, 8, 2);
    ("Thai word with spacing mark", "คำว่า", `Unicode, 8, 4);
    ("Thai tone mark", "ว่", `Unicode, 8, 1);
    ("Thai standalone spacing mark", "ำ", `Unicode, 8, 1);
    ("Thai repeated spacing marks", "น้ำำ", `Unicode, 8, 2);
    ("Thai repeated spacing marks wcwidth", "น้ำำ", `Wcwidth, 8, 3);
    ("Thai spacing mark after wide base", "界ำ", `Unicode, 8, 2);
    ("Lao spacing mark", "\u{0E99}\u{0EB3}", `Unicode, 8, 2);
    ("Indic spacing mark retains base width", "क\u{093E}", `Unicode, 8, 1);
    ("Indic spacing mark wcwidth", "क\u{093E}", `Wcwidth, 8, 2);
    ("zero width space", "\u{200B}", `Unicode, 8, 0);
    ("C1 control", uchar_to_utf8 0x008A, `Unicode, 8, 0);
  ]

let ascii_fast_path_consistency () =
  let s = "hello\tworld" in
  let unicode_w = measure ~width_method:`Unicode ~tab_width:4 s in
  let wcwidth_w = measure ~width_method:`Wcwidth ~tab_width:4 s in
  check_width "unicode vs wcwidth" unicode_w wcwidth_w

let measure_multi_grapheme_regression () =
  check_width "ASCII + wide chars" 10
    (measure ~width_method:`Unicode ~tab_width:2 "Hello 世界");
  check_width "three wide chars" 6
    (measure ~width_method:`Unicode ~tab_width:2 "日本語");
  check_width "emoji + ASCII + wide" 6
    (measure ~width_method:`Unicode ~tab_width:2 "🚀Hi世");
  check_width "unicode matches wcwidth for simple case" 10
    (measure ~width_method:`Wcwidth ~tab_width:2 "Hello 世界")

let malformed_utf8_measurement () =
  let invalid = "\xC3" in
  check_width "truncated sequence" 1
    (measure ~width_method:`Unicode ~tab_width:2 invalid);
  check_width "invalid middle byte" 3
    (measure ~width_method:`Unicode ~tab_width:2 ("a" ^ "\x80" ^ "b"));
  check_width "measure_sub invalid slice" 2
    (measure_sub ~width_method:`Unicode ~tab_width:2
       ("x" ^ invalid ^ "(")
       ~pos:1 ~len:2)

let measure_sub_validation () =
  check_width "sub ASCII" 3
    (measure_sub ~width_method:`Unicode ~tab_width:2 "xxabcxx" ~pos:2 ~len:3);
  check_width "sub non-positive length" 0
    (measure_sub ~width_method:`Unicode ~tab_width:2 "abc" ~pos:(-1) ~len:0);
  expect_invalid_arg "negative pos" (fun () ->
      ignore
        (measure_sub ~width_method:`Unicode ~tab_width:2 "abc" ~pos:(-1) ~len:1));
  expect_invalid_arg "oversized len" (fun () ->
      ignore
        (measure_sub ~width_method:`Unicode ~tab_width:2 "abc" ~pos:1 ~len:3))

let measure_sub_matches_slice () =
  let samples = [ "a中é👨‍💻z"; "\xF0\x9F\x80x\xC3\x80\xFF" ] in
  List.iter
    (fun width_method ->
      List.iter
        (fun text ->
          for pos = 0 to String.length text do
            for len = 0 to String.length text - pos do
              equal
                ~msg:(Printf.sprintf "slice %d+%d" pos len)
                int
                (measure ~width_method ~tab_width:2 (String.sub text pos len))
                (measure_sub ~width_method ~tab_width:2 text ~pos ~len)
            done
          done)
        samples)
    [ `Unicode; `Wcwidth ]

let grapheme_iteration () =
  let zwj_seq = "👩\u{200D}🚀" in
  let count_default = ref 0 in
  iter_graphemes (fun ~offset:_ ~len:_ -> incr count_default) zwj_seq;
  equal ~msg:"default: ZWJ sequence is one grapheme" int 1 !count_default;
  equal ~msg:"grapheme_count" int 3 (grapheme_count ("a" ^ zwj_seq ^ "b"))

let grapheme_info_skips_zero_width () =
  let seen = ref [] in
  iter_grapheme_info ~width_method:`Unicode ~tab_width:2
    (fun ~offset ~len ~width -> seen := (offset, len, width) :: !seen)
    ("a" ^ "\u{200B}" ^ "界");
  equal ~msg:"zero-width grapheme is skipped"
    (list (triple int int int))
    [ (0, 1, 1); (4, 3, 2) ]
    (List.rev !seen)

let grapheme_info_ascii_base_with_combining_mark () =
  let seen = ref [] in
  iter_grapheme_info ~width_method:`Unicode ~tab_width:2
    (fun ~offset ~len ~width -> seen := (offset, len, width) :: !seen)
    "e\u{0301}x";
  equal ~msg:"e + U+0301 is one 3-byte cluster"
    (list (triple int int int))
    [ (0, 3, 1); (3, 1, 1) ]
    (List.rev !seen);
  let seen = ref [] in
  iter_grapheme_info ~width_method:`Unicode ~tab_width:2
    (fun ~offset ~len ~width -> seen := (offset, len, width) :: !seen)
    "abce\u{0301}";
  equal ~msg:"mark extends the last byte of an ASCII run"
    (list (triple int int int))
    [ (0, 1, 1); (1, 1, 1); (2, 1, 1); (3, 3, 1) ]
    (List.rev !seen)

type wrap_break = { byte_offset : int; grapheme_offset : int }
type line_break = { pos : int; kind : line_break_kind }

let wrap_breaks s =
  let acc = ref [] in
  iter_wrap_breaks
    (fun ~break_byte_offset:_ ~next_byte_offset ~grapheme_offset ->
      acc := { byte_offset = next_byte_offset; grapheme_offset } :: !acc)
    s;
  Array.of_list (List.rev !acc)

let wrap_break_points s =
  let acc = ref [] in
  iter_wrap_breaks
    (fun ~break_byte_offset ~next_byte_offset:_ ~grapheme_offset ->
      acc := { byte_offset = break_byte_offset; grapheme_offset } :: !acc)
    s;
  Array.of_list (List.rev !acc)

let line_breaks_to_list s =
  let acc = ref [] in
  iter_line_breaks (fun ~pos ~kind -> acc := { pos; kind } :: !acc) s;
  List.rev !acc

let wrap_break_testable =
  Testable.make
    ~pp:(fun ppf { byte_offset; grapheme_offset } ->
      Format.fprintf ppf "{byte=%d; grapheme=%d}" byte_offset grapheme_offset)
    ~equal:( = )

let line_break_testable =
  Testable.make
    ~pp:(fun ppf { pos; kind } ->
      let kind_s =
        match kind with `LF -> "LF" | `CR -> "CR" | `CRLF -> "CRLF"
      in
      Format.fprintf ppf "{pos=%d; kind=%s}" pos kind_s)
    ~equal:( = )

let position_testable =
  Testable.make
    ~pp:(fun ppf { byte_offset; grapheme_count; columns_used } ->
      Format.fprintf ppf "{byte_offset=%d; grapheme_count=%d; columns_used=%d}"
        byte_offset grapheme_count columns_used)
    ~equal:( = )

let grapheme_testable =
  Testable.make
    ~pp:(fun ppf { byte_offset; byte_length; width } ->
      Format.fprintf ppf "{byte_offset=%d; byte_length=%d; width=%d}"
        byte_offset byte_length width)
    ~equal:( = )

let wrap_breaks_ascii_spaces () =
  let breaks = wrap_breaks "hello world test" in
  equal ~msg:"two breaks for two spaces" int 2 (Array.length breaks);
  equal ~msg:"first break after 'hello '" wrap_break_testable
    { byte_offset = 6; grapheme_offset = 5 }
    breaks.(0);
  equal ~msg:"second break after 'world '" wrap_break_testable
    { byte_offset = 12; grapheme_offset = 11 }
    breaks.(1)

let wrap_breaks_ascii_punctuation () =
  let breaks = wrap_breaks "a-b/c.d,e;f:g!h?i" in
  equal ~msg:"period excluded from seven ASCII breaks" int 7
    (Array.length breaks);
  equal ~msg:"break after hyphen" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_keep_periods_inside_tokens () =
  equal ~msg:"executable suffix is not a boundary"
    (array wrap_break_testable)
    [|
      { byte_offset = 1; grapheme_offset = 1 };
      { byte_offset = 5; grapheme_offset = 5 };
    |]
    (wrap_break_points "./bin/main.exe");
  equal ~msg:"dotted filename has no boundary"
    (array wrap_break_testable)
    [||]
    (wrap_break_points "release.tar.gz");
  equal ~msg:"decimal has no boundary"
    (array wrap_break_testable)
    [||]
    (wrap_break_points "3.1415")

let wrap_breaks_ascii_brackets () =
  let breaks = wrap_breaks "(a)[b]{c}" in
  equal ~msg:"6 breaks for 6 brackets" int 6 (Array.length breaks)

let wrap_breaks_tabs () =
  let breaks = wrap_breaks "a\tb\tc" in
  equal ~msg:"2 breaks for 2 tabs" int 2 (Array.length breaks);
  equal ~msg:"break after first tab" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_unicode_spaces () =
  let nbsp = "\xC2\xA0" in
  let zwsp = "\xE2\x80\x8B" in
  let ideographic = "\xE3\x80\x80" in
  equal ~msg:"break after NBSP" int 1
    (Array.length (wrap_breaks ("a" ^ nbsp ^ "b")));
  equal ~msg:"break after ZWSP" int 1
    (Array.length (wrap_breaks ("a" ^ zwsp ^ "b")));
  equal ~msg:"break after ideographic space" int 1
    (Array.length (wrap_breaks ("a" ^ ideographic ^ "b")))

let wrap_breaks_soft_hyphen () =
  let soft_hyphen = "\xC2\xAD" in
  equal ~msg:"break after soft hyphen" int 1
    (Array.length (wrap_breaks ("word" ^ soft_hyphen ^ "break")))

let wrap_breaks_en_space_range () =
  let en_quad = "\xE2\x80\x80" in
  let hair_space = "\xE2\x80\x8A" in
  equal ~msg:"break after EN QUAD" int 1
    (Array.length (wrap_breaks ("a" ^ en_quad ^ "b")));
  equal ~msg:"break after HAIR SPACE" int 1
    (Array.length (wrap_breaks ("a" ^ hair_space ^ "b")))

let wrap_breaks_no_break_in_plain_text () =
  equal ~msg:"no breaks in continuous text" int 0
    (Array.length (wrap_breaks "helloworld"))

let wrap_breaks_grapheme_aware () =
  let emoji = "👍🏽" in
  let breaks = wrap_breaks ("a " ^ emoji ^ " b") in
  equal ~msg:"breaks respect grapheme boundaries" int 2 (Array.length breaks);
  equal ~msg:"first break" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_empty_string () =
  equal ~msg:"no breaks in empty string" int 0 (Array.length (wrap_breaks ""))

let wrap_breaks_zwj_sequence () =
  let zwj_seq = "👩\u{200D}🚀" in
  equal ~msg:"unicode: no breaks in ZWJ sequence" int 0
    (Array.length (wrap_breaks zwj_seq))

let wrap_break_points_ascii_space () =
  let points = wrap_break_points "a b" in
  let resumes = wrap_breaks "a b" in
  equal ~msg:"one break point for one ASCII space" int 1 (Array.length points);
  equal ~msg:"point offset at space grapheme" wrap_break_testable
    { byte_offset = 1; grapheme_offset = 1 }
    points.(0);
  equal ~msg:"resume offset starts next grapheme" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 1 }
    resumes.(0)

let wrap_break_points_unicode_space () =
  let nbsp = "\xC2\xA0" in
  let s = "a" ^ nbsp ^ "b" in
  let points = wrap_break_points s in
  let resumes = wrap_breaks s in
  equal ~msg:"one break point for one NBSP" int 1 (Array.length points);
  equal ~msg:"point offset at NBSP grapheme start" wrap_break_testable
    { byte_offset = 1; grapheme_offset = 1 }
    points.(0);
  equal ~msg:"resume offset after NBSP grapheme" wrap_break_testable
    { byte_offset = 3; grapheme_offset = 1 }
    resumes.(0)

let wrap_breaks_mixed_cjk_ascii_transitions () =
  let cjk_ascii = "日本語abc" in
  let cjk_points = wrap_break_points cjk_ascii in
  let cjk_resumes = wrap_breaks cjk_ascii in
  equal ~msg:"CJK->ASCII one break" int 1 (Array.length cjk_points);
  equal ~msg:"CJK->ASCII point" wrap_break_testable
    { byte_offset = 6; grapheme_offset = 2 }
    cjk_points.(0);
  equal ~msg:"CJK->ASCII resume" wrap_break_testable
    { byte_offset = 9; grapheme_offset = 2 }
    cjk_resumes.(0);
  let ascii_cjk = "abc日本語" in
  let ascii_points = wrap_break_points ascii_cjk in
  let ascii_resumes = wrap_breaks ascii_cjk in
  equal ~msg:"ASCII->CJK one break" int 1 (Array.length ascii_points);
  equal ~msg:"ASCII->CJK point" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 2 }
    ascii_points.(0);
  equal ~msg:"ASCII->CJK resume" wrap_break_testable
    { byte_offset = 3; grapheme_offset = 2 }
    ascii_resumes.(0)

let wrap_breaks_fullwidth_cjk_punctuation () =
  let ideographic_full_stop = "日本語。abc" in
  let points = wrap_break_points ideographic_full_stop in
  let resumes = wrap_breaks ideographic_full_stop in
  equal ~msg:"CJK punctuation one break" int 1 (Array.length points);
  equal ~msg:"CJK punctuation point" wrap_break_testable
    { byte_offset = 9; grapheme_offset = 3 }
    points.(0);
  equal ~msg:"CJK punctuation resume" wrap_break_testable
    { byte_offset = 12; grapheme_offset = 3 }
    resumes.(0);
  let check_one label cp =
    let s = "a" ^ uchar_to_utf8 cp ^ "b" in
    equal ~msg:label int 1 (Array.length (wrap_breaks s))
  in
  check_one "ideographic comma" 0x3001;
  check_one "fullwidth exclamation" 0xFF01;
  check_one "fullwidth question" 0xFF1F

let line_breaks_lf () =
  let breaks = line_breaks_to_list "a\nb\nc" in
  equal ~msg:"two LF breaks" int 2 (List.length breaks);
  equal ~msg:"first LF" line_break_testable { pos = 1; kind = `LF }
    (List.nth breaks 0);
  equal ~msg:"second LF" line_break_testable { pos = 3; kind = `LF }
    (List.nth breaks 1)

let line_breaks_cr () =
  let breaks = line_breaks_to_list "a\rb\rc" in
  equal ~msg:"two CR breaks" int 2 (List.length breaks);
  equal ~msg:"first CR" line_break_testable { pos = 1; kind = `CR }
    (List.nth breaks 0);
  equal ~msg:"second CR" line_break_testable { pos = 3; kind = `CR }
    (List.nth breaks 1)

let line_breaks_crlf () =
  let breaks = line_breaks_to_list "a\r\nb\r\nc" in
  equal ~msg:"two CRLF breaks" int 2 (List.length breaks);
  equal ~msg:"first CRLF" line_break_testable { pos = 2; kind = `CRLF }
    (List.nth breaks 0);
  equal ~msg:"second CRLF" line_break_testable { pos = 5; kind = `CRLF }
    (List.nth breaks 1)

let line_breaks_mixed () =
  let breaks = line_breaks_to_list "a\nb\r\nc\rd" in
  equal ~msg:"three mixed breaks" int 3 (List.length breaks);
  equal ~msg:"LF" line_break_testable { pos = 1; kind = `LF }
    (List.nth breaks 0);
  equal ~msg:"CRLF" line_break_testable { pos = 4; kind = `CRLF }
    (List.nth breaks 1);
  equal ~msg:"CR" line_break_testable { pos = 6; kind = `CR }
    (List.nth breaks 2)

let line_breaks_empty () =
  equal ~msg:"no breaks in empty" int 0 (List.length (line_breaks_to_list ""))

let line_breaks_no_newlines () =
  equal ~msg:"no breaks without newlines" int 0
    (List.length (line_breaks_to_list "hello world"))

let line_breaks_consecutive_lf () =
  let breaks = line_breaks_to_list "a\n\n\nb" in
  equal ~msg:"three consecutive LF" int 3 (List.length breaks);
  equal ~msg:"first" line_break_testable { pos = 1; kind = `LF }
    (List.nth breaks 0);
  equal ~msg:"second" line_break_testable { pos = 2; kind = `LF }
    (List.nth breaks 1);
  equal ~msg:"third" line_break_testable { pos = 3; kind = `LF }
    (List.nth breaks 2)

let width_at_positions () =
  equal ~msg:"empty" int 0
    (width_at ~width_method:`Unicode ~tab_width:8 "" ~byte_offset:0);
  equal ~msg:"out of bounds" int 0
    (width_at ~width_method:`Unicode ~tab_width:8 "hello" ~byte_offset:10);
  equal ~msg:"ASCII" int 1
    (width_at ~width_method:`Unicode ~tab_width:8 "hello" ~byte_offset:1);
  equal ~msg:"tab fixed width" int 4
    (width_at ~width_method:`Unicode ~tab_width:4 "a\tb" ~byte_offset:1);
  equal ~msg:"CJK" int 2
    (width_at ~width_method:`Unicode ~tab_width:8 "世界" ~byte_offset:3);
  equal ~msg:"combining cluster" int 1
    (width_at ~width_method:`Unicode ~tab_width:8 "cafe\u{0301}" ~byte_offset:3);
  equal ~msg:"ZWJ cluster" int 2
    (width_at ~width_method:`Unicode ~tab_width:8 "👩\u{200D}🚀" ~byte_offset:0);
  equal ~msg:"flag cluster" int 2
    (width_at ~width_method:`Unicode ~tab_width:8 "🇺🇸" ~byte_offset:0);
  equal ~msg:"malformed byte" int 1
    (width_at ~width_method:`Unicode ~tab_width:8 "\xC3(" ~byte_offset:0)

let wcwidth_position_semantics () =
  let emoji = "👋🏿" in
  equal ~msg:"wcwidth first emoji codepoint" int 2
    (width_at ~width_method:`Wcwidth ~tab_width:4 emoji ~byte_offset:0);
  equal ~msg:"wcwidth skin-tone codepoint" int 2
    (width_at ~width_method:`Wcwidth ~tab_width:4 emoji ~byte_offset:4);
  let zwj = "👩\u{200D}🚀" in
  equal ~msg:"wcwidth ZWJ codepoint" int 0
    (width_at ~width_method:`Wcwidth ~tab_width:4 zwj ~byte_offset:4);
  equal ~msg:"wcwidth find_pos snap back" position_testable
    { byte_offset = 2; grapheme_count = 2; columns_used = 2 }
    (find_pos ~width_method:`Wcwidth ~tab_width:4 "AB👋🏿CD" ~columns:3);
  equal ~msg:"wcwidth find_pos include start" position_testable
    { byte_offset = 6; grapheme_count = 3; columns_used = 4 }
    (find_pos ~width_method:`Wcwidth ~tab_width:4 ~include_start_before:true
       "AB👋🏿CD" ~columns:3);
  equal ~msg:"wcwidth wrap through emoji" position_testable
    { byte_offset = 6; grapheme_count = 3; columns_used = 4 }
    (find_wrap_pos ~width_method:`Wcwidth ~tab_width:4 "Hi👋🏿Bye" ~max_columns:4);
  equal ~msg:"wcwidth previous skips zero-width ZWJ" (option grapheme_testable)
    (Some { byte_offset = 7; byte_length = 4; width = 2 })
    (prev_grapheme ~width_method:`Wcwidth ~tab_width:4 zwj ~byte_offset:11)

let malformed_wcwidth_positions () =
  let text = "\xF0\x9F" in
  equal ~msg:"position uses the measurement replacement policy"
    position_testable
    { byte_offset = 2; grapheme_count = 1; columns_used = 1 }
    (find_pos ~width_method:`Wcwidth ~tab_width:2 text ~columns:2);
  equal ~msg:"previous codepoint uses the same invalid prefix"
    (option grapheme_testable)
    (Some { byte_offset = 0; byte_length = 2; width = 1 })
    (prev_grapheme ~width_method:`Wcwidth ~tab_width:2 text ~byte_offset:2)

let wcwidth_decoder_matches_stdlib () =
  let check text =
    let offset = ref 0 in
    let count = ref 0 in
    while !offset < String.length text do
      let decoded = String.get_utf_8_uchar text !offset in
      offset := !offset + Uchar.utf_decode_length decoded;
      incr count
    done;
    let actual =
      find_pos ~width_method:`Wcwidth ~tab_width:2 text ~columns:max_int
    in
    equal ~msg:"maximal-subpart count" int !count actual.grapheme_count;
    equal ~msg:"measurement agrees" int
      (measure ~width_method:`Wcwidth ~tab_width:2 text)
      actual.columns_used
  in
  let bytes = Bytes.create 2 in
  for first = 0 to 255 do
    Bytes.set bytes 0 (Char.chr first);
    check (Bytes.sub_string bytes 0 1);
    for second = 0 to 255 do
      Bytes.set bytes 1 (Char.chr second);
      check (Bytes.unsafe_to_string bytes)
    done
  done;
  let boundaries = [ 0x7F; 0x80; 0x8F; 0x90; 0x9F; 0xA0; 0xBF; 0xC0 ] in
  List.iter
    (fun first ->
      List.iter
        (fun second ->
          List.iter
            (fun third ->
              List.iter
                (fun fourth ->
                  let text =
                    String.init 4 (function
                      | 0 -> Char.chr first
                      | 1 -> Char.chr second
                      | 2 -> Char.chr third
                      | _ -> Char.chr fourth)
                  in
                  check (String.sub text 0 3);
                  check text)
                boundaries)
            boundaries)
        boundaries)
    [ 0xE0; 0xE1; 0xED; 0xEF; 0xF0; 0xF1; 0xF4; 0xF5 ]

let unicode_position_semantics () =
  let input = "A🌍B世C" in
  equal ~msg:"find_pos after ASCII" position_testable
    { byte_offset = 1; grapheme_count = 1; columns_used = 1 }
    (find_pos ~width_method:`Unicode ~tab_width:8 input ~columns:2);
  equal ~msg:"find_pos include start before wide" position_testable
    { byte_offset = 5; grapheme_count = 2; columns_used = 3 }
    (find_pos ~width_method:`Unicode ~tab_width:8 ~include_start_before:true
       input ~columns:2);
  equal ~msg:"find_wrap_pos includes fitting wide grapheme" position_testable
    { byte_offset = 5; grapheme_count = 2; columns_used = 3 }
    (find_wrap_pos ~width_method:`Unicode ~tab_width:8 input ~max_columns:3);
  equal ~msg:"find_wrap_pos stops before too-wide grapheme" position_testable
    { byte_offset = 1; grapheme_count = 1; columns_used = 1 }
    (find_wrap_pos ~width_method:`Unicode ~tab_width:8 input ~max_columns:2);
  equal ~msg:"combining mark included" position_testable
    { byte_offset = 6; grapheme_count = 4; columns_used = 4 }
    (find_pos ~width_method:`Unicode ~tab_width:8 "cafe\u{0301}test" ~columns:4);
  equal ~msg:"malformed offsets" position_testable
    { byte_offset = 1; grapheme_count = 1; columns_used = 1 }
    (find_wrap_pos ~width_method:`Unicode ~tab_width:8 "\xC3(" ~max_columns:1)

let previous_grapheme_positions () =
  equal ~msg:"at start" (option grapheme_testable) None
    (prev_grapheme ~width_method:`Unicode ~tab_width:8 "hello" ~byte_offset:0);
  equal ~msg:"out of bounds" (option grapheme_testable) None
    (prev_grapheme ~width_method:`Unicode ~tab_width:8 "hello" ~byte_offset:10);
  equal ~msg:"ASCII previous" (option grapheme_testable)
    (Some { byte_offset = 4; byte_length = 1; width = 1 })
    (prev_grapheme ~width_method:`Unicode ~tab_width:8 "hello" ~byte_offset:5);
  equal ~msg:"inside CJK grapheme" (option grapheme_testable)
    (Some { byte_offset = 1; byte_length = 3; width = 2 })
    (prev_grapheme ~width_method:`Unicode ~tab_width:8 "a世界" ~byte_offset:2);
  equal ~msg:"at CJK boundary returns previous" (option grapheme_testable)
    (Some { byte_offset = 1; byte_length = 3; width = 2 })
    (prev_grapheme ~width_method:`Unicode ~tab_width:8 "a世界" ~byte_offset:4);
  equal ~msg:"combining cluster previous" (option grapheme_testable)
    (Some { byte_offset = 3; byte_length = 3; width = 1 })
    (prev_grapheme ~width_method:`Unicode ~tab_width:8 "cafe\u{0301}"
       ~byte_offset:6);
  equal ~msg:"emoji ZWJ previous" (option grapheme_testable)
    (Some { byte_offset = 1; byte_length = 11; width = 2 })
    (prev_grapheme ~width_method:`Unicode ~tab_width:8 ("a" ^ "👩\u{200D}🚀")
       ~byte_offset:12);
  equal ~msg:"flag previous" (option grapheme_testable)
    (Some { byte_offset = 2; byte_length = 8; width = 2 })
    (prev_grapheme ~width_method:`Unicode ~tab_width:8 "US🇺🇸" ~byte_offset:10)

let emoji_presentation_widths () =
  let check label cp expected_w =
    check_width label expected_w
      (measure ~width_method:`Unicode ~tab_width:2 (uchar_to_utf8 cp))
  in
  check "CJK U+4E2D" 0x4E2D 2;
  check "CJK U+3000 ideographic space" 0x3000 2;
  check "watch U+231A" 0x231A 2;
  check "hourglass U+231B" 0x231B 2;
  check "check mark U+2705" 0x2705 2;
  check "raised fist U+270A" 0x270A 2;
  check "sparkles U+2728" 0x2728 2;
  check "cross mark U+274C" 0x274C 2;
  check "question marks U+2753" 0x2753 2;
  check "exclamation U+2757" 0x2757 2;
  check "plus U+2795" 0x2795 2;
  check "rainbow U+1F308" 0x1F308 2;
  check "rocket U+1F680" 0x1F680 2;
  check "wave U+1F44B" 0x1F44B 2;
  check "thumbs up U+1F44D" 0x1F44D 2;
  check "face with tears U+1F602" 0x1F602 2;
  check "red heart U+2764" 0x2764 2;
  check "red heart + VS16 base" 0x2764 2;
  let heart_vs16 = uchar_to_utf8 0x2764 ^ uchar_to_utf8 0xFE0F in
  check_width "heart with VS16 = width 2" 2
    (measure ~width_method:`Unicode ~tab_width:2 heart_vs16);
  let flag = uchar_to_utf8 0x1F1FA ^ uchar_to_utf8 0x1F1F8 in
  check_width "flag pair = width 2" 2
    (measure ~width_method:`Unicode ~tab_width:2 flag);
  check "umbrella U+2614" 0x2614 2;
  check "hot beverage U+2615" 0x2615 2;
  check "white medium square U+25FD" 0x25FD 2;
  check "mahjong U+1F004" 0x1F004 2;
  check "playing card U+1F0CF" 0x1F0CF 2;
  check "skin tone U+1F3FD" 0x1F3FD 2;
  check "pizza U+1F355" 0x1F355 2;
  check "dog face U+1F436" 0x1F436 2;
  check "melting face U+1FAE0" 0x1FAE0 2

let c1_control_widths () =
  let check_zero label cp =
    check_width label 0
      (measure ~width_method:`Unicode ~tab_width:2 (uchar_to_utf8 cp))
  in
  check_zero "C1 U+0080" 0x0080;
  check_zero "C1 U+008A" 0x008A;
  check_zero "C1 U+009F" 0x009F

let scalar_width_agrees_with_measurement () =
  let bytes = Bytes.create 4 in
  List.iter
    (fun cp ->
      let uchar = Uchar.of_int cp in
      let len = Bytes.set_utf_8_uchar bytes 0 uchar in
      let text = Bytes.sub_string bytes 0 len in
      List.iter
        (fun width_method ->
          equal int
            (measure ~width_method ~tab_width:4 text)
            (uchar_width ~tab_width:4 uchar))
        [ `Unicode; `Wcwidth ])
    [
      0;
      9;
      32;
      127;
      0x85;
      0x301;
      0x915;
      0x94D;
      0x200D;
      0x754C;
      0xFE0F;
      0x1F1EB;
      0x1F680;
      0x10FFFF;
    ]

let layout_source_boundaries () =
  let s = Layout.Source.of_string "a\226\128\139b\r\n" in
  equal int 4 (Layout.Source.length s);
  equal int 2 (Layout.Source.line_count s);
  equal int 3 (Layout.Source.line_end s 0);
  equal int 4 (Layout.Source.line_start s 1);
  equal string "a\226\128\139b\r\n" (Layout.Source.slice s ~first:0 ~last:4)

let layout_truncated_source () =
  let source = Layout.Source.of_string "abcdef\nXY" in
  let t = Layout.create source ~width:3 ~wrap:`None ~truncate:true in
  equal int 7 (Layout.row_start t 1);
  equal (pair int int) (2, 6) (Layout.coverage t ~row:0 ~column:2);
  equal (pair int int) (7, 8) (Layout.coverage t ~row:1 ~column:0);
  equal (pair int int) (0, 2)
    (Layout.locate t Layout.{ grapheme = 4; affinity = Upstream });
  equal (pair int int) (0, 3)
    (Layout.locate t Layout.{ grapheme = 4; affinity = Downstream })

let layout_affinity () =
  let t = Layout.create (Layout.Source.of_string "abcd") ~width:2 ~wrap:`Char in
  equal (pair int int) (0, 2)
    (Layout.locate t Layout.{ grapheme = 2; affinity = Upstream });
  equal (pair int int) (1, 0)
    (Layout.locate t Layout.{ grapheme = 2; affinity = Downstream });
  let hit = Layout.hit t ~row:0 ~column:2 in
  equal int 2 hit.grapheme;
  is_true (hit.affinity = Layout.Upstream)

let layout_whole_clusters () =
  List.iter
    (fun width_method ->
      let source = Layout.Source.of_string "e\204\129👩‍💻界" in
      equal int 3 (Layout.Source.length source);
      let t = Layout.create ~width_method source ~width:1 ~wrap:`Char in
      equal int 3 (Layout.row_count t);
      for row = 0 to 2 do
        equal int row (Layout.row_start t row);
        equal int (row + 1) (Layout.row_end t row)
      done)
    [ `Unicode; `Wcwidth ]

let thai_spacing_mark_layout () =
  let text = "น้ำX" in
  let source = Layout.Source.of_string text in
  equal ~msg:"spacing mark stays in the first source cluster" int 2
    (Layout.Source.length source);
  List.iter
    (fun width_method ->
      let layout = Layout.create ~width_method source ~width:2 ~wrap:`Char in
      equal ~msg:"following character wraps" int 2 (Layout.row_count layout);
      equal ~msg:"first row spans two cells" int 2 (Layout.row_width layout 0);
      equal ~msg:"first row retains the whole cluster" string "น้ำ"
        (Layout.Source.slice source
           ~first:(Layout.row_start layout 0)
           ~last:(Layout.row_end layout 0));
      equal ~msg:"second cell selects the same cluster" (pair int int) (0, 1)
        (Layout.coverage layout ~row:0 ~column:1);
      equal ~msg:"caret after the cluster starts the next row" (pair int int)
        (1, 0)
        (Layout.locate layout Layout.{ grapheme = 1; affinity = Downstream }))
    [ `Unicode; `Wcwidth ];
  equal ~msg:"a one-column prefix cannot split the Thai cluster" int 0
    (find_wrap_pos ~width_method:`Unicode ~tab_width:2 text ~max_columns:1)
      .byte_offset;
  equal ~msg:"a two-column prefix fits the whole Thai cluster" int
    (String.length "น้ำ")
    (find_wrap_pos ~width_method:`Unicode ~tab_width:2 text ~max_columns:2)
      .byte_offset

let layout_zero_width_snapping () =
  let source = Layout.Source.of_string "a\226\128\139b" in
  let t = Layout.create source ~width:20 ~wrap:`None in
  equal int 1 (Layout.hit ~affinity:Upstream t ~row:0 ~column:1).grapheme;
  equal int 2 (Layout.hit ~affinity:Downstream t ~row:0 ~column:1).grapheme;
  let cells = ref [] in
  Layout.iter_cells t ~row:0 ~first_column:0 ~last_column:2
    (fun ~first ~last ~column:_ ~width:_ ~ellipsis:_ ->
      cells := (first, last) :: !cells);
  equal (list (pair int int)) [ (0, 1); (2, 3) ] (List.rev !cells);
  let overwide =
    Layout.create (Layout.Source.of_string "界\226\128\139") ~width:1 ~wrap:`Char
  in
  equal int 1 (Layout.row_count overwide);
  equal int 2 (Layout.row_end overwide 0)

let layout_bounded_iteration () =
  let source = Layout.Source.of_string (String.make 100_000 'x') in
  let t = Layout.create source ~width:100_000 ~wrap:`None in
  let visited = ref 0 in
  Layout.iter_cells t ~row:0 ~first_column:99_990 ~last_column:99_995
    (fun ~first ~last:_ ~column ~width:_ ~ellipsis:_ ->
      equal int first column;
      incr visited);
  equal int 5 !visited;
  let wrapped = Layout.create source ~width:1 ~wrap:`Char in
  let rows = ref [] in
  Layout.iter_rows wrapped ~first:99990 ~count:3
    (fun ~row ~logical_line:_ ~continuation ->
      rows := (row, continuation) :: !rows);
  equal
    (list (pair int int))
    [ (99990, 99990); (99991, 99991); (99992, 99992) ]
    (List.rev !rows)

let layout_edge_cases () =
  List.iter
    (fun text ->
      let source = Layout.Source.of_string text in
      equal string text
        (Layout.Source.slice source ~first:0
           ~last:(Layout.Source.length source));
      List.iter
        (fun width ->
          List.iter
            (fun wrap ->
              let t = Layout.create source ~width ~wrap in
              equal (pair int int)
                (Layout.row_count t, Layout.max_width t)
                (Layout.measure source ~width ~wrap);
              equal int 0 (Layout.row_start t 0);
              equal int
                (Layout.Source.length source)
                (Layout.row_end t (Layout.row_count t - 1)))
            [ `None; `Char; `Word ])
        [ 0; 1; 2; 7 ])
    [ ""; "\n"; "\r\n"; "hello world"; "界x"; "\255a\226\128\139" ]

let () =
  run "matrix.text"
    [
      group "Layout"
        [
          test "source boundaries retain invisible clusters and CRLF"
            layout_source_boundaries;
          test "truncation retains source coverage" layout_truncated_source;
          test "soft-wrap caret affinity" layout_affinity;
          test "whole clusters under both width methods" layout_whole_clusters;
          test "Thai spacing mark wrapping and source coverage"
            thai_spacing_mark_layout;
          test "zero-width snapping and visible iteration"
            layout_zero_width_snapping;
          test "bounded row and horizontal iteration" layout_bounded_iteration;
          test "measurement and rows agree at edges" layout_edge_cases;
        ];
      group "Conformance"
        [ test "UAX #29 grapheme boundaries" run_grapheme_conformance ];
      group "Measurement"
        [
          cases "semantics" measurement_rows
            ~name:(fun (name, _, _, _, _) -> name)
            (fun (name, input, width_method, tab_width, expected) ->
              check_width name expected (measure ~width_method ~tab_width input));
          test "ASCII fast path" ascii_fast_path_consistency;
          test "scalar width agrees with measurement"
            scalar_width_agrees_with_measurement;
          test "multi-grapheme regression" measure_multi_grapheme_regression;
          test "malformed UTF-8" malformed_utf8_measurement;
          test "measure_sub validation" measure_sub_validation;
          test "measure_sub matches independent slices"
            measure_sub_matches_slice;
          test "malformed wcwidth positions" malformed_wcwidth_positions;
          test "wcwidth decoder matches Stdlib" wcwidth_decoder_matches_stdlib;
        ];
      group "Graphemes"
        [
          test "iteration" grapheme_iteration;
          test "info skips zero-width" grapheme_info_skips_zero_width;
          test "info keeps combining marks with ASCII base"
            grapheme_info_ascii_base_with_combining_mark;
        ];
      group "Wrap Breaks"
        [
          test "ASCII spaces" wrap_breaks_ascii_spaces;
          test "ASCII punctuation" wrap_breaks_ascii_punctuation;
          test "periods stay inside tokens"
            wrap_breaks_keep_periods_inside_tokens;
          test "ASCII brackets" wrap_breaks_ascii_brackets;
          test "tabs" wrap_breaks_tabs;
          test "Unicode spaces" wrap_breaks_unicode_spaces;
          test "soft hyphen" wrap_breaks_soft_hyphen;
          test "EN space range" wrap_breaks_en_space_range;
          test "no break in plain text" wrap_breaks_no_break_in_plain_text;
          test "grapheme aware" wrap_breaks_grapheme_aware;
          test "empty string" wrap_breaks_empty_string;
          test "ZWJ sequence" wrap_breaks_zwj_sequence;
          test "ASCII space break point" wrap_break_points_ascii_space;
          test "Unicode NBSP break point" wrap_break_points_unicode_space;
          test "mixed CJK/ASCII transitions"
            wrap_breaks_mixed_cjk_ascii_transitions;
          test "fullwidth CJK punctuation" wrap_breaks_fullwidth_cjk_punctuation;
        ];
      group "Line Breaks"
        [
          test "LF" line_breaks_lf;
          test "CR" line_breaks_cr;
          test "CRLF" line_breaks_crlf;
          test "mixed" line_breaks_mixed;
          test "empty" line_breaks_empty;
          test "no newlines" line_breaks_no_newlines;
          test "consecutive LF" line_breaks_consecutive_lf;
        ];
      group "Positions"
        [
          test "width_at" width_at_positions;
          test "wcwidth semantics" wcwidth_position_semantics;
          test "unicode semantics" unicode_position_semantics;
          test "previous grapheme" previous_grapheme_positions;
        ];
      group "Width Tables"
        [
          test "emoji presentation widths" emoji_presentation_widths;
          test "C1 control widths" c1_control_widths;
        ];
    ]
