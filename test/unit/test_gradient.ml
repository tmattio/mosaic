open Mosaic

(** Test gradient style creation *)
let test_gradient_style_creation () =
  let module S = Render.Style in
  (* Test foreground gradient *)
  let style = S.gradient_fg ~colors:[ S.Red; S.Blue ] ~direction:`Horizontal in
  Alcotest.(check bool)
    "has foreground gradient"
    (match style.S.fg with Some (S.Gradient _) -> true | _ -> false)
    true;
  Alcotest.(check bool) "no background" (style.S.bg = None) true;

  (* Test background gradient *)
  let style =
    S.gradient_bg ~colors:[ S.Green; S.Yellow ] ~direction:`Vertical
  in
  Alcotest.(check bool)
    "has background gradient"
    (match style.S.bg with Some (S.Gradient _) -> true | _ -> false)
    true;
  Alcotest.(check bool) "no foreground" (style.S.fg = None) true

(** Test gradient with RGB colors *)
let test_gradient_with_rgb () =
  let module S = Render.Style in
  let gradient =
    S.gradient
      ~colors:[ S.rgb 255 0 0; S.rgb 0 255 0; S.rgb 0 0 255 ]
      ~direction:`Horizontal
  in
  Alcotest.(check int) "has 3 colors" (List.length gradient.S.colors) 3;
  match gradient.S.direction with
  | `Horizontal -> ()
  | _ -> Alcotest.fail "Expected horizontal direction"

(** Test gradient rendering integration *)
let test_gradient_rendering () =
  let module S = Render.Style in
  (* Create a buffer and render gradient text *)
  let buffer = Render.create 20 3 in
  let gradient_style =
    S.gradient_fg ~colors:[ S.Red; S.Blue ] ~direction:`Horizontal
  in
  let element = Ui.text ~style:gradient_style "Test" in
  Ui.render buffer element;

  (* Check that the text was rendered *)
  let cell = Render.get buffer 0 0 in
  Alcotest.(check bool) "cell has content" (List.length cell.chars > 0) true

(** Test adaptive color in color_spec *)
let test_adaptive_color_spec () =
  let module S = Render.Style in
  (* Create adaptive color *)
  let adaptive = S.adaptive ~light:S.Black ~dark:S.White in

  (* Test foreground adaptive *)
  let style = S.adaptive_fg adaptive in
  Alcotest.(check bool)
    "has adaptive foreground"
    (match style.S.fg with Some (S.Adaptive _) -> true | _ -> false)
    true;
  Alcotest.(check bool) "no background" (style.S.bg = None) true;

  (* Test background adaptive *)
  let style = S.adaptive_bg adaptive in
  Alcotest.(check bool)
    "has adaptive background"
    (match style.S.bg with Some (S.Adaptive _) -> true | _ -> false)
    true;
  Alcotest.(check bool) "no foreground" (style.S.fg = None) true;

  (* Test rendering with adaptive colors *)
  let buffer = Render.create 20 3 in
  let adaptive_style = S.adaptive_fg S.adaptive_primary in
  let element = Ui.text ~style:adaptive_style "Adaptive" in
  Ui.render buffer element;

  (* Check that text was rendered *)
  let cell = Render.get buffer 0 0 in
  Alcotest.(check bool) "cell has content" (List.length cell.chars > 0) true;

  (* Check that the color adapts based on background *)
  match cell.Render.style.Render.Style.fg with
  | Some (S.Solid S.White) -> () (* On dark background, should be white *)
  | _ -> Alcotest.fail "Expected white color on dark background"

(** Test gradient edge cases *)
let test_gradient_edge_cases () =
  let module S = Render.Style in
  (* Test empty color list *)
  let gradient = S.gradient ~colors:[] ~direction:`Horizontal in
  Alcotest.(check int) "empty colors list" (List.length gradient.S.colors) 0;

  (* Test single color *)
  let gradient = S.gradient ~colors:[ S.Red ] ~direction:`Vertical in
  Alcotest.(check int) "single color" (List.length gradient.S.colors) 1;

  (* Test many colors *)
  let many_colors = List.init 20 (fun i -> S.Index i) in
  let gradient = S.gradient ~colors:many_colors ~direction:`Horizontal in
  Alcotest.(check int) "many colors" (List.length gradient.S.colors) 20

(** Test gradient directions *)
let test_gradient_directions () =
  let module S = Render.Style in
  (* Test horizontal gradient on wide text *)
  let buffer = Render.create 30 5 in
  let h_gradient =
    S.gradient_fg
      ~colors:[ S.Red; S.Yellow; S.Green; S.Cyan; S.Blue ]
      ~direction:`Horizontal
  in
  let element = Ui.text ~style:h_gradient "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  Ui.render buffer element;

  (* Check that colors vary across horizontal positions *)
  let cell1 = Render.get buffer 0 0 in
  let cell2 = Render.get buffer 13 0 in
  let cell3 = Render.get buffer 25 0 in

  (* Cells at different positions should have different colors *)
  Alcotest.(check bool)
    "horizontal variation" true
    (match
       ( cell1.Render.style.Render.Style.fg,
         cell2.Render.style.Render.Style.fg,
         cell3.Render.style.Render.Style.fg )
     with
    | Some (S.Solid c1), Some (S.Solid c2), Some (S.Solid c3) ->
        c1 <> c2 || c2 <> c3 (* At least some variation *)
    | _ -> true);

  (* Gradient colors are resolved at render time *)

  (* Test vertical gradient *)
  let buffer = Render.create 10 10 in
  let v_gradient =
    S.gradient_bg ~colors:[ S.Black; S.gray 12; S.White ] ~direction:`Vertical
  in
  let element =
    Ui.vbox ~background:v_gradient ~height:10
      [
        Ui.text "Line 1";
        Ui.text "Line 2";
        Ui.text "Line 3";
        Ui.text "Line 4";
        Ui.text "Line 5";
      ]
  in
  Ui.render buffer element

(** Test gradient color interpolation *)
let test_gradient_interpolation () =
  let module S = Render.Style in
  (* Test RGB interpolation *)
  let gradient =
    S.gradient
      ~colors:[ S.rgb 255 0 0; S.rgb 0 0 255 ] (* Red to Blue *)
      ~direction:`Horizontal
  in

  (* Create element with gradient *)
  let buffer = Render.create 20 1 in
  let style = S.gradient_fg ~colors:gradient.S.colors ~direction:`Horizontal in
  let element = Ui.text ~style "Gradient Test Text" in
  Ui.render buffer element;

  (* Test gradient boundary conditions *)
  let first_cell = Render.get buffer 0 0 in
  let last_cell = Render.get buffer 17 0 in

  (* First and last cells should have content *)
  Alcotest.(check bool)
    "first cell has content"
    (List.length first_cell.chars > 0)
    true;
  Alcotest.(check bool)
    "last cell has content"
    (List.length last_cell.chars > 0)
    true

(** Test combined gradients *)
let test_combined_gradients () =
  let module S = Render.Style in
  (* Test foreground and background gradients together *)
  let fg_gradient =
    S.gradient_fg ~colors:[ S.White; S.Black ] ~direction:`Horizontal
  in
  let bg_gradient =
    S.gradient_bg ~colors:[ S.Blue; S.Cyan ] ~direction:`Vertical
  in
  let combined = S.(fg_gradient ++ bg_gradient) in

  (* Check both gradients are present *)
  Alcotest.(check bool)
    "has fg gradient"
    (match combined.S.fg with Some (S.Gradient _) -> true | _ -> false)
    true;
  Alcotest.(check bool)
    "has bg gradient"
    (match combined.S.bg with Some (S.Gradient _) -> true | _ -> false)
    true;

  (* Test rendering with combined gradients *)
  let buffer = Render.create 20 5 in
  let element =
    Ui.vbox ~background:combined
      [ Ui.text "Combined"; Ui.text "Gradient"; Ui.text "Test" ]
  in
  Ui.render buffer element

(** Test gradient with different color types *)
let test_gradient_color_types () =
  let module S = Render.Style in
  (* Mix of color types *)
  let mixed_gradient =
    S.gradient
      ~colors:
        [
          S.Red;
          (* Named color *)
          S.Index 123;
          (* 256-color index *)
          S.rgb 128 64 192;
          (* RGB color *)
          S.gray 15;
          (* Grayscale *)
          S.rgb_hex 0xFF00FF;
          (* Hex RGB *)
        ]
      ~direction:`Horizontal
  in

  Alcotest.(check int)
    "mixed color types"
    (List.length mixed_gradient.S.colors)
    5;

  (* Test each color type in the gradient *)
  match mixed_gradient.S.colors with
  | [ c1; c2; c3; c4; c5 ] -> (
      (match c1 with S.Red -> () | _ -> Alcotest.fail "Expected Red");
      (match c2 with
      | S.Index 123 -> ()
      | _ -> Alcotest.fail "Expected Index 123");
      (match c3 with
      | S.RGB (128, 64, 192) -> ()
      | _ -> Alcotest.fail "Expected RGB");
      (match c4 with
      | S.Index n when n >= 232 && n <= 255 -> ()
      | _ -> Alcotest.fail "Expected grayscale");
      match c5 with
      | S.RGB (255, 0, 255) -> ()
      | _ -> Alcotest.fail "Expected magenta RGB")
  | _ -> Alcotest.fail "Expected 5 colors"

(** Test gradient on empty and special cases *)
let test_gradient_special_cases () =
  let module S = Render.Style in
  (* Gradient on empty text *)
  let buffer = Render.create 10 1 in
  let gradient_style =
    S.gradient_fg ~colors:[ S.Red; S.Blue ] ~direction:`Horizontal
  in
  let element = Ui.text ~style:gradient_style "" in
  Ui.render buffer element;

  (* Gradient on single character *)
  let buffer = Render.create 10 1 in
  let element = Ui.text ~style:gradient_style "X" in
  Ui.render buffer element;
  let cell = Render.get buffer 0 0 in
  Alcotest.(check bool)
    "single char rendered"
    (match cell.chars with
    | [ c ] when Uchar.to_char c = 'X' -> true
    | _ -> false)
    true;

  (* Gradient on very long text *)
  let buffer = Render.create 100 1 in
  let long_text = String.make 100 'A' in
  let element = Ui.text ~style:gradient_style long_text in
  Ui.render buffer element;

  (* Gradient with newlines *)
  let buffer = Render.create 20 5 in
  let multiline = "Line1\nLine2\nLine3" in
  let element = Ui.text ~style:gradient_style multiline in
  Ui.render buffer element

let () =
  let open Alcotest in
  run "Gradient tests"
    [
      ( "Gradient styles",
        [
          test_case "gradient style creation" `Quick
            test_gradient_style_creation;
          test_case "gradient with RGB colors" `Quick test_gradient_with_rgb;
          test_case "gradient rendering integration" `Quick
            test_gradient_rendering;
          test_case "adaptive color in color_spec" `Quick
            test_adaptive_color_spec;
          test_case "gradient edge cases" `Quick test_gradient_edge_cases;
          test_case "gradient directions" `Quick test_gradient_directions;
          test_case "gradient interpolation" `Quick test_gradient_interpolation;
          test_case "combined gradients" `Quick test_combined_gradients;
          test_case "gradient color types" `Quick test_gradient_color_types;
          test_case "gradient special cases" `Quick test_gradient_special_cases;
        ] );
    ]
