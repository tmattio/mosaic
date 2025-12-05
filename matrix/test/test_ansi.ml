open Alcotest
open Ansi

(* --- Helpers --- *)

let check_color = testable Color.pp Color.equal

(* Strict checking for escape sequences *)
let check_seq msg expected actual = check string msg expected actual

(* --- 1. Color Logic & Math --- *)

let color_parsing_hex () =
  let c = Color.of_hex_exn in
  (* Standard 6 digit *)
  check check_color "hex 6 digit" (Color.of_rgb 255 0 128) (c "#ff0080");
  (* Short 3 digit *)
  check check_color "hex 3 digit" (Color.of_rgb 255 0 255) (c "#f0f");
  (* Alpha 8 digit *)
  check check_color "hex 8 digit" (Color.of_rgba 0 255 0 128) (c "#00ff0080");
  (* Case insensitivity *)
  check check_color "hex case" (Color.of_rgb 170 187 204) (c "#AABBCC");
  (* Failures *)
  check bool "invalid hex len" true (Color.of_hex "12" = None);
  check bool "invalid hex char" true (Color.of_hex "GG0000" = None)

let color_hsl_roundtrip () =
  (* Pure Red: 0 deg, 100%, 50% *)
  let red = Color.red in
  let h, s, l, _ = Color.to_hsl red in
  check (float 0.01) "red hue" 0.0 h;
  check (float 0.01) "red saturation" 1.0 s;
  check (float 0.01) "red lightness" 0.5 l;

  (* Roundtrip reconstruction *)
  let reconstructed = Color.of_hsl ~h ~s ~l () in
  check check_color "red roundtrip" red reconstructed

let color_blending () =
  let bg = Color.of_rgb 0 0 0 in
  (* Black *)
  let fg = Color.of_rgba 255 255 255 128 in
  (* 50% White *)

  (* Linear blend: 50% white over black should be approx gray 127 *)
  let res = Color.blend ~mode:`Linear ~src:fg ~dst:bg () in
  match res with
  | Color.Rgb { r; g; b } ->
      check bool "blend result roughly gray" true
        (r > 120 && r < 135 && r = g && g = b)
  | _ -> fail "Expected opaque RGB result from blend"

let color_downgrade_semantics () =
  (* 1. Truecolor -> Truecolor (Identity) *)
  let c = Color.of_rgb 100 150 200 in
  check check_color "identity downgrade" c (Color.downgrade ~level:`Truecolor c);

  (* 2. Grayscale mapping in Ansi256 *)
  (* 256 palette has a grayscale ramp from 232 (dark) to 255 (light) *)
  (* Gray 10/10/10 should map low in the ramp *)
  let dark_gray = Color.of_rgb 8 8 8 in
  let down = Color.downgrade ~level:`Ansi256 dark_gray in
  match down with
  | Color.Extended n -> check bool "mapped to grayscale ramp" true (n = 232)
  | _ -> fail "Expected extended color index"

(* --- 2. Attributes & Style --- *)

let attr_bitmask_integrity () =
  (* Verify bitmasks don't overlap using a set operation *)
  let all_set =
    Attr.combine ~bold:true ~dim:true ~italic:true ~underline:true ~blink:true
      ~inverse:true ~hidden:true ~strikethrough:true ~double_underline:true
      ~overline:true ~framed:true ~encircled:true ()
  in
  let count = Attr.cardinal all_set in
  check int "all flags distinct" 12 count;

  (* Check that removing one specific flag works *)
  let minus_bold = Attr.remove Attr.Bold all_set in
  check bool "removed bold" false (Attr.mem Attr.Bold minus_bold);
  check bool "kept italic" true (Attr.mem Attr.Italic minus_bold)

let style_composition_and_hash () =
  let s1 = Style.make ~fg:Color.red () in
  let s2 = Style.make ~bg:Color.blue () in
  let s3 = Style.merge ~base:s1 ~overlay:s2 in

  check check_color "composed fg" Color.red (Option.get s3.fg);
  check check_color "composed bg" Color.blue (Option.get s3.bg);

  (* Hash consistency *)
  let s3_clone = Style.make ~fg:Color.red ~bg:Color.blue () in
  check int "hash stable" (Style.hash s3) (Style.hash s3_clone)

let attr_extended_ordering () =
  let style =
    Style.make ~double_underline:true ~overline:true ~framed:true
      ~encircled:true ()
  in
  check_seq "extended attr ordering" "\x1b[0;21;53;51;52m"
    (Style.sgr_sequence style);
  check string "no-op when unchanged" "" (Style.sgr_sequence ~prev:style style)

(* --- 3. SGR State Machine (Diffing) --- *)

let sgr_state_transitions () =
  let buf = Bytes.create 128 in
  let w = Escape.make buf in
  let state = Sgr_state.create () in

  (* 1. Initial: emits reset(0) + attributes *)
  Sgr_state.update state w ~fg_r:1.0 ~fg_g:0.0 ~fg_b:0.0 ~fg_a:1.0 (* Red *)
    ~bg_r:0.0 ~bg_g:0.0 ~bg_b:0.0 ~bg_a:0.0 (* Black (transparent) *)
    ~attrs:(Attr.pack Attr.bold);

  let out1 = Escape.slice w in
  (* Expect: Reset(0); FG(Red); Bold(1) *)
  (* Sequence: \x1b[0;38;2;255;0;0;1m *)
  check_seq "initial transition" "\x1b[0;38;2;255;0;0;1m" (Bytes.to_string out1);

  (* 2. No-op: update with exact same values *)
  let w2 = Escape.make buf in
  Sgr_state.update state w2 ~fg_r:1.0 ~fg_g:0.0 ~fg_b:0.0 ~fg_a:1.0 ~bg_r:0.0
    ~bg_g:0.0 ~bg_b:0.0 ~bg_a:0.0 ~attrs:(Attr.pack Attr.bold);
  check int "noop update empty" 0 (Escape.len w2);

  (* 3. Delta: Remove Bold, keep color *)
  let w3 = Escape.make buf in
  Sgr_state.update state w3 ~fg_r:1.0 ~fg_g:0.0 ~fg_b:0.0 ~fg_a:1.0 ~bg_r:0.0
    ~bg_g:0.0 ~bg_b:0.0 ~bg_a:0.0 ~attrs:0;

  (* No bold *)

  (* Expect: Reset(0); FG(Red) -- Bold is implicitly removed by Reset, need to restore Color *)
  let out3 = Bytes.to_string (Escape.slice w3) in
  check_seq "delta transition" "\x1b[0;38;2;255;0;0m" out3

let sgr_transparent_bg_resets () =
  let buf = Bytes.create 128 in
  let state = Sgr_state.create () in
  let w1 = Escape.make buf in
  Sgr_state.update state w1 ~fg_r:1.0 ~fg_g:0.0 ~fg_b:0.0 ~fg_a:1.0 ~bg_r:0.0
    ~bg_g:0.0 ~bg_b:1.0 ~bg_a:1.0 ~attrs:0;
  check_seq "bg applied" "\x1b[0;38;2;255;0;0;48;2;0;0;255m"
    (Bytes.to_string (Escape.slice w1));
  let w2 = Escape.make buf in
  Sgr_state.update state w2 ~fg_r:1.0 ~fg_g:0.0 ~fg_b:0.0 ~fg_a:1.0 ~bg_r:0.0
    ~bg_g:0.0 ~bg_b:1.0 ~bg_a:0.0 ~attrs:0;
  check_seq "bg cleared to default" "\x1b[0;38;2;255;0;0m"
    (Bytes.to_string (Escape.slice w2))

(* --- 4. Parser Resilience --- *)

let parser_chunked_utf8 () =
  let p = Parser.create () in
  (* Euro sign: \xE2\x82\xAC *)

  (* Feed byte 1 *)
  let t1 = Parser.feed p (Bytes.of_string "\xE2") 0 1 in
  check int "chunk 1 buffered" 0 (List.length t1);

  (* Feed byte 2 *)
  let t2 = Parser.feed p (Bytes.of_string "\x82") 0 1 in
  check int "chunk 2 buffered" 0 (List.length t2);

  (* Feed byte 3 + extra text *)
  let t3 = Parser.feed p (Bytes.of_string "\xACok") 0 3 in
  let combined =
    match t3 with
    | [ Parser.Text s ] -> s
    | [ Parser.Text utf8; Parser.Text plain ] -> utf8 ^ plain
    | _ -> ""
  in
  check string "utf8 reconstructed" "€ok" combined

let parser_chunked_escape () =
  let p = Parser.create () in
  (* Sequence: \x1b[31m (Red) *)

  (* Feed partial CSI *)
  let t1 = Parser.feed p (Bytes.of_string "\x1b[3") 0 3 in
  check int "partial csi buffered" 0 (List.length t1);

  (* Finish it *)
  let t2 = Parser.feed p (Bytes.of_string "1m") 0 2 in
  match t2 with
  | [ Parser.SGR [ `Fg c ] ] -> check check_color "parsed split csi" Color.red c
  | _ -> fail "Failed to reconstruct split CSI"

let parser_malformed_input () =
  (* 1. Lone ESC at end of string *)
  let t1 = Parser.parse "foo\x1b" in
  (* Should probably ignore the lone ESC or treat as text depending on implementation details.
     The current implementation buffers it waiting for more. Since parse calls feed with 0 at end... *)
  match t1 with
  | [ Parser.Text "foo" ] -> () (* It was dropped/buffered as incomplete *)
  | _ -> fail "Lone ESC handling unexpected"

let parser_max_length_overflow () =
  (* Construct a massive garbage sequence *)
  let huge_seq = "\x1b[" ^ String.make 300 '1' ^ "m" in
  let tokens = Parser.parse huge_seq in
  match tokens with
  | Parser.Control (Parser.Unknown msg) :: _ ->
      check string "overflow detected" "ESCAPE_TOO_LONG" msg
  | _ -> fail "Parser should reject oversized sequences"

let parser_osc8_st_terminated () =
  let seq = "\x1b]8;;http://foo\x1b\\txt\x1b]8;;\x1b\\" in
  match Parser.parse seq with
  | [
   Parser.Control (Parser.Hyperlink (Some ([], link)));
   Parser.Text "txt";
   Parser.Control (Parser.Hyperlink None);
  ] ->
      check string "parsed link url" "http://foo" link
  | _ -> fail "OSC 8 ST tokens mismatch"

let parser_osc8_bel_terminated () =
  let seq = "\x1b]8;;http://foo\x07txt\x1b]8;;\x07" in
  match Parser.parse seq with
  | [
   Parser.Control (Parser.Hyperlink (Some ([], link)));
   Parser.Text "txt";
   Parser.Control (Parser.Hyperlink None);
  ] ->
      check string "parsed link url bel" "http://foo" link
  | _ -> fail "OSC 8 BEL tokens mismatch"

(* --- 5. Writer & Low-Level Escape --- *)

let writer_utf8_encoding () =
  let buf = Bytes.create 16 in
  let w = Escape.make buf in

  (* 1 byte: A *)
  Escape.utf8 (Char.code 'A') w;
  (* 3 byte: € (0x20AC) *)
  Escape.utf8 0x20AC w;
  (* 4 byte: 🚀 (Rocket 0x1F680) *)
  Escape.utf8 0x1F680 w;

  let out = Bytes.to_string (Escape.slice w) in
  check string "utf8 encoding" "A€🚀" out

let writer_hyperlink_params () =
  let buf = Bytes.create 64 in
  let w = Escape.make buf in
  Escape.hyperlink_start ~params:"id=123" ~url:"http://foo" w;
  let out = Bytes.to_string (Escape.slice w) in
  (* OSC 8 ; params ; url ST (\x1b\\) *)
  check_seq "hyperlink with params" "\x1b]8;id=123;http://foo\x1b\\" out

let writer_invalid_scalar_replacement () =
  let buf = Bytes.create 8 in
  let w = Escape.make buf in
  (* Surrogate code point should be clamped to U+FFFD *)
  Escape.utf8 0xD800 w;
  check_seq "invalid scalar -> replacement" "\xef\xbf\xbd"
    (Bytes.to_string (Escape.slice w))

(* --- 6. High Level API --- *)

let strip_edge_cases () =
  (* Standard *)
  check string "strip standard" "foo" (Ansi.strip "\x1b[31mfoo\x1b[0m");
  (* Incomplete at end *)
  check string "strip incomplete" "foo" (Ansi.strip "foo\x1b[");
  (* Lone ESC *)
  check string "strip lone esc" "foo" (Ansi.strip "foo\x1b");
  (* OSC title *)
  check string "strip osc" "foo" (Ansi.strip "\x1b]0;Title\007foo")

let parameter_clamping () =
  (* Cursor movement: negative -> 0 -> empty string *)
  check_seq "negative lines" "" (Ansi.cursor_up ~n:(-5));
  check_seq "zero lines" "" (Ansi.cursor_up ~n:0);
  (* Absolute position: negative/zero -> 1 *)
  check_seq "clamp row/col" "\x1b[1;1H" (Ansi.cursor_position ~row:0 ~col:(-5))

let explicit_error_handling () =
  try
    let _ = Ansi.set_scrolling_region ~top:10 ~bottom:5 in
    fail "scrolling region validation failed"
  with Invalid_argument _ -> ()

let cursor_and_explicit_width_sequences () =
  check_seq "cursor color osc12" "\x1b]12;#010203\x07"
    (Ansi.cursor_color ~r:1 ~g:2 ~b:3);
  check_seq "explicit width osc66" "\x1b]66;w=5;hi\x1b\\"
    (Ansi.explicit_width ~width:5 ~text:"hi")

let hyperlink_lifecycle () =
  let link_style = Style.hyperlink "http://foo" Style.default in
  let rendered = Ansi.render [ (link_style, "hi") ] in
  check_seq "osc8 opened/closed with reset"
    "\x1b]8;;http://foo\x1b\\\x1b[0mhi\x1b]8;;\x1b\\\x1b[0m" rendered;
  let buf = Bytes.create 64 in
  let w = Escape.make buf in
  let _, active =
    Ansi.emit ~hyperlinks_enabled:false w [ (link_style, "hi") ]
  in
  check_seq "osc8 suppressed when disabled" "\x1b[0mhi"
    (Bytes.to_string (Escape.slice w));
  check (option string) "no active link returned" None active

let tests =
  [
    ( "Color Logic",
      [
        test_case "hex parsing" `Quick color_parsing_hex;
        test_case "hsl roundtrip" `Quick color_hsl_roundtrip;
        test_case "blending" `Quick color_blending;
        test_case "downgrade" `Quick color_downgrade_semantics;
      ] );
    ( "Attributes & Style",
      [
        test_case "bitmask integrity" `Quick attr_bitmask_integrity;
        test_case "style composition" `Quick style_composition_and_hash;
        test_case "extended attr ordering" `Quick attr_extended_ordering;
      ] );
    ( "SGR Engine",
      [
        test_case "state diffing" `Quick sgr_state_transitions;
        test_case "transparent bg reset" `Quick sgr_transparent_bg_resets;
      ] );
    ( "Parser",
      [
        test_case "chunked utf8" `Quick parser_chunked_utf8;
        test_case "chunked escape" `Quick parser_chunked_escape;
        test_case "malformed inputs" `Quick parser_malformed_input;
        test_case "overflow protection" `Quick parser_max_length_overflow;
        test_case "osc8 st terminated" `Quick parser_osc8_st_terminated;
        test_case "osc8 bel terminated" `Quick parser_osc8_bel_terminated;
      ] );
    ( "Writer & Seq",
      [
        test_case "utf8 encoding" `Quick writer_utf8_encoding;
        test_case "hyperlink params" `Quick writer_hyperlink_params;
        test_case "invalid scalar replacement" `Quick
          writer_invalid_scalar_replacement;
      ] );
    ( "High Level API",
      [
        test_case "strip semantics" `Quick strip_edge_cases;
        test_case "param clamping" `Quick parameter_clamping;
        test_case "input validation" `Quick explicit_error_handling;
        test_case "cursor & explicit width" `Quick
          cursor_and_explicit_width_sequences;
        test_case "hyperlink lifecycle" `Quick hyperlink_lifecycle;
      ] );
  ]

let () = run "matrix.ansi" tests
