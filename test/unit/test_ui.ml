(** Tests for the UI module *)

open Mosaic

let buffer_to_string buffer =
  let width, height = Render.dimensions buffer in
  let buf = Buffer.create ((width + 1) * height) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let cell = Render.get buffer x y in
      match cell.Render.chars with
      | [] -> Buffer.add_char buf ' '
      | ch :: _ -> Buffer.add_utf_8_uchar buf ch
    done;
    if y < height - 1 then Buffer.add_char buf '\n'
  done;
  Buffer.contents buf

let test_text_rendering () =
  let buffer = Render.create 20 1 in
  let element = Ui.text "Hello" in
  Ui.render buffer element;

  let output = buffer_to_string buffer in
  Alcotest.(check string) "text renders" "Hello               " output

let test_style_application () =
  let buffer = Render.create 10 1 in
  let element = Ui.text ~style:Style.(fg (Index 5) ++ bold) "Test" in
  Ui.render buffer element;

  (* Check that text is rendered *)
  let cell = Render.get buffer 0 0 in
  match cell.Render.chars with
  | ch :: _ when Uchar.to_char ch = 'T' -> (
      (* Style should have color index 5 and bold *)
      Alcotest.(check bool)
        "bold applied" true cell.Render.style.Render.Style.bold;
      match cell.Render.style.Render.Style.fg with
      | Some (Ansi.Index 5) -> ()
      | _ -> Alcotest.fail "color not applied")
  | _ -> Alcotest.fail "text not rendered"

let test_hbox_layout () =
  let buffer = Render.create 20 1 in
  let element = Ui.hbox [ Ui.text "A"; Ui.text "B"; Ui.text "C" ] in
  Ui.render buffer element;

  let output = buffer_to_string buffer in
  Alcotest.(check string) "hbox layout" "ABC                 " output;

  (* Test with gap *)
  let buffer = Render.create 20 1 in
  let element = Ui.hbox ~gap:2 [ Ui.text "A"; Ui.text "B"; Ui.text "C" ] in
  Ui.render buffer element;

  let output = buffer_to_string buffer in
  Alcotest.(check string) "hbox with gap" "A  B  C             " output

let test_vbox_layout () =
  let buffer = Render.create 10 3 in
  let element = Ui.vbox [ Ui.text "Line1"; Ui.text "Line2"; Ui.text "Line3" ] in
  Ui.render buffer element;

  let output = buffer_to_string buffer in
  let expected = "Line1     \nLine2     \nLine3     " in
  Alcotest.(check string) "vbox layout" expected output;

  (* Test with gap *)
  let buffer = Render.create 10 5 in
  let element = Ui.vbox ~gap:1 [ Ui.text "A"; Ui.text "B" ] in
  Ui.render buffer element;

  let output = buffer_to_string buffer in
  let expected = "A         \n          \nB         \n          \n          " in
  Alcotest.(check string) "vbox with gap" expected output

let test_padding () =
  let buffer = Render.create 10 5 in
  let element = Ui.hbox ~padding:(Ui.pad ~all:1 ()) [ Ui.text "Test" ] in
  Ui.render buffer element;

  let output = buffer_to_string buffer in
  (* With padding of 1 on all sides, "Test" should be offset by 1,1 *)
  let cell = Render.get buffer 1 1 in
  (match cell.Render.chars with
  | ch :: _ when Uchar.to_char ch = 'T' -> ()
  | _ -> Alcotest.failf "padding not applied correctly:\n%s" output);

  (* Test asymmetric padding *)
  let buffer = Render.create 10 5 in
  let element = Ui.hbox ~padding:(Ui.pad ~left:2 ~top:1 ()) [ Ui.text "X" ] in
  Ui.render buffer element;

  let cell = Render.get buffer 2 1 in
  match cell.Render.chars with
  | ch :: _ when Uchar.to_char ch = 'X' -> ()
  | _ -> Alcotest.fail "asymmetric padding not applied"

let test_borders () =
  let buffer = Render.create 10 5 in
  let element =
    Ui.hbox
      ~border:(Ui.border ~style:Solid ())
      ~width:10 ~height:5
      [ Ui.text "Hi" ]
  in
  Ui.render buffer element;

  (* Check corners *)
  let cell = Render.get buffer 0 0 in
  (match cell.Render.chars with
  | ch :: _ when Uchar.to_int ch = 0x250C -> () (* ┌ *)
  | _ -> Alcotest.fail "top-left corner missing");

  let cell = Render.get buffer 9 0 in
  (match cell.Render.chars with
  | ch :: _ when Uchar.to_int ch = 0x2510 -> () (* ┐ *)
  | _ -> Alcotest.fail "top-right corner missing");

  let cell = Render.get buffer 0 4 in
  (match cell.Render.chars with
  | ch :: _ when Uchar.to_int ch = 0x2514 -> () (* └ *)
  | _ -> Alcotest.fail "bottom-left corner missing");

  let cell = Render.get buffer 9 4 in
  (match cell.Render.chars with
  | ch :: _ when Uchar.to_int ch = 0x2518 -> () (* ┘ *)
  | _ -> Alcotest.fail "bottom-right corner missing");

  (* Check that content is inside border *)
  let cell = Render.get buffer 1 1 in
  match cell.Render.chars with
  | ch :: _ when Uchar.to_char ch = 'H' -> ()
  | _ -> Alcotest.fail "content not inside border"

let test_alignment () =
  (* Test horizontal alignment in vbox *)
  let buffer = Render.create 10 3 in
  let element = Ui.vbox ~align_items:Center ~width:10 [ Ui.text "Hi" ] in
  Ui.render buffer element;

  (* "Hi" should be centered (at position 4) *)
  let cell = Render.get buffer 4 0 in
  (match cell.Render.chars with
  | ch :: _ when Uchar.to_char ch = 'H' -> ()
  | _ -> Alcotest.fail "horizontal centering failed");

  (* Test End alignment *)
  let buffer = Render.create 10 1 in
  let element = Ui.vbox ~align_items:End ~width:10 [ Ui.text "End" ] in
  Ui.render buffer element;

  (* "End" should be at the right (position 7) *)
  let cell = Render.get buffer 7 0 in
  match cell.Render.chars with
  | ch :: _ when Uchar.to_char ch = 'E' -> ()
  | _ -> Alcotest.fail "end alignment failed"

let test_justify () =
  (* Test vertical justification in vbox *)
  let buffer = Render.create 5 10 in
  let element =
    Ui.vbox ~justify_content:Center ~height:10 [ Ui.text "A"; Ui.text "B" ]
  in
  Ui.render buffer element;

  (* Content should be vertically centered *)
  let cell = Render.get buffer 0 4 in
  match cell.Render.chars with
  | ch :: _ when Uchar.to_char ch = 'A' -> ()
  | _ -> Alcotest.fail "vertical centering failed"

let test_expand () =
  let buffer = Render.create 20 1 in
  let element = Ui.hbox [ Ui.text "A"; Ui.expand (Ui.space 1); Ui.text "B" ] in
  Ui.render buffer element;

  let output = buffer_to_string buffer in
  (* A should be at start, B should be at end *)
  let cell_a = Render.get buffer 0 0 in
  let cell_b = Render.get buffer 19 0 in
  match (cell_a.Render.chars, cell_b.Render.chars) with
  | ch_a :: _, ch_b :: _
    when Uchar.to_char ch_a = 'A' && Uchar.to_char ch_b = 'B' ->
      ()
  | _ -> Alcotest.failf "expand not working:\n%s" output

let test_fixed_dimensions () =
  let buffer = Render.create 20 10 in
  let element = Ui.hbox ~width:10 ~height:5 ~border:(Ui.border ()) [] in
  Ui.render buffer element;

  (* Check that box is exactly 10x5 *)
  (* Top border at y=0 from x=0 to x=9 *)
  let cell = Render.get buffer 9 0 in
  (match cell.Render.chars with
  | ch :: _ when Uchar.to_int ch = 0x2510 -> () (* ┐ *)
  | _ -> Alcotest.fail "width not respected");

  (* Bottom border at y=4 *)
  let cell = Render.get buffer 0 4 in
  (match cell.Render.chars with
  | ch :: _ when Uchar.to_int ch = 0x2514 -> () (* └ *)
  | _ -> Alcotest.fail "height not respected");

  (* Nothing beyond the box *)
  let cell = Render.get buffer 10 0 in
  match cell.Render.chars with
  | [] -> ()
  | ch :: _ when Uchar.to_char ch = ' ' -> ()
  | ch :: _ -> Alcotest.failf "content beyond width: %c" (Uchar.to_char ch)

let test_nested_layout () =
  let buffer = Render.create 20 5 in
  let element =
    Ui.vbox
      [
        Ui.text "Header";
        Ui.hbox ~border:(Ui.border ())
          [
            Ui.vbox [ Ui.text "A1"; Ui.text "A2" ];
            Ui.vbox [ Ui.text "B1"; Ui.text "B2" ];
          ];
      ]
  in
  Ui.render buffer element;

  (* Just check that it doesn't crash and renders something sensible *)
  let cell = Render.get buffer 0 0 in
  match cell.Render.chars with
  | ch :: _ when Uchar.to_char ch = 'H' -> () (* Header *)
  | _ -> Alcotest.fail "nested layout failed"

let test_space () =
  let buffer = Render.create 10 1 in
  let element = Ui.hbox [ Ui.text "A"; Ui.space 3; Ui.text "B" ] in
  Ui.render buffer element;

  let output = buffer_to_string buffer in
  Alcotest.(check string) "space works" "A   B     " output

let test_style_combination () =
  let _style = Style.(fg (Index 1) ++ bg (Index 2) ++ bold ++ italic) in

  (* Just check that we can combine styles without error *)
  (* We can't pattern match on Style.t as it's abstract *)
  ()
(* If it compiles, it works *)

let test_color_helpers () =
  (* Test gray *)
  let gray_color = Style.gray 10 in
  (match gray_color with
  | Style.Index n when n = 232 + 10 -> ()
  | _ -> Alcotest.fail "gray helper failed");

  (* Test rgb_hex *)
  let hex_color = Style.rgb_hex 0xFF00FF in
  match hex_color with
  | Style.RGB (255, 0, 255) -> ()
  | _ -> Alcotest.fail "rgb_hex helper failed"

let tests =
  [
    ("text rendering", `Quick, test_text_rendering);
    ("style application", `Quick, test_style_application);
    ("hbox layout", `Quick, test_hbox_layout);
    ("vbox layout", `Quick, test_vbox_layout);
    ("padding", `Quick, test_padding);
    ("borders", `Quick, test_borders);
    ("alignment", `Quick, test_alignment);
    ("justify", `Quick, test_justify);
    ("expand", `Quick, test_expand);
    ("fixed dimensions", `Quick, test_fixed_dimensions);
    ("nested layout", `Quick, test_nested_layout);
    ("space", `Quick, test_space);
    ("style combination", `Quick, test_style_combination);
    ("color helpers", `Quick, test_color_helpers);
  ]

let () = Alcotest.run "UI" [ ("rendering", tests) ]
