(* Since we can't directly inspect cell attributes with Screen, 
   we'll test gradient functionality at a higher level *)

(** Test gradient style creation *)
let test_gradient_style_creation () =
  let module S = Ui.Style in
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
  let module S = Ui.Style in
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
  let module S = Ui.Style in
  (* Create a gradient style and render it *)
  let gradient_style =
    S.gradient_fg ~colors:[ S.Red; S.Blue ] ~direction:`Horizontal
  in
  let element = Ui.text ~style:gradient_style "Test" in
  let output = Test_utils.render_to_string ~width:20 ~height:3 element in
  (* Check that the text was rendered *)
  Alcotest.(check bool) "output is not empty" (String.length output > 0) true;
  Alcotest.(check bool)
    "contains Test text"
    (String.contains_from output 0 'T')
    true

(** Test adaptive color style creation *)
let test_adaptive_colors () =
  let module S = Ui.Style in
  (* Test adaptive color spec creation *)
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
  let adaptive_style = S.adaptive_fg S.adaptive_primary in
  let element = Ui.text ~style:adaptive_style "Adaptive" in
  let output = Test_utils.render_to_string ~width:20 ~height:3 element in
  Alcotest.(check bool) "output is not empty" (String.length output > 0) true

(** Test gradient edge cases *)
let test_gradient_edge_cases () =
  let module S = Ui.Style in
  (* Test empty color list - should raise exception *)
  Alcotest.check_raises "empty colors list raises exception"
    (Invalid_argument "Gradient must have at least one color") (fun () ->
      ignore (S.gradient ~colors:[] ~direction:`Horizontal));

  (* Test single color *)
  let gradient = S.gradient ~colors:[ S.Red ] ~direction:`Vertical in
  Alcotest.(check int) "single color" (List.length gradient.S.colors) 1;

  (* Test many colors *)
  let many_colors = List.init 20 (fun i -> S.Index i) in
  let gradient = S.gradient ~colors:many_colors ~direction:`Horizontal in
  Alcotest.(check int) "many colors" (List.length gradient.S.colors) 20

(** Test gradient directions *)
let test_gradient_directions () =
  let module S = Ui.Style in
  (* Test horizontal gradient on wide text *)
  let h_gradient =
    S.gradient_fg
      ~colors:[ S.Red; S.Yellow; S.Green; S.Cyan; S.Blue ]
      ~direction:`Horizontal
  in
  let element = Ui.text ~style:h_gradient "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let output = Test_utils.render_to_string ~width:30 ~height:5 element in
  Alcotest.(check bool)
    "horizontal gradient renders"
    (String.length output > 0)
    true;

  (* Test vertical gradient *)
  let v_gradient =
    S.gradient_bg ~colors:[ S.Black; S.gray 12; S.White ] ~direction:`Vertical
  in
  let element =
    Ui.vbox ~style:v_gradient ~height:(`Cells 10)
      [
        Ui.text "Line 1";
        Ui.text "Line 2";
        Ui.text "Line 3";
        Ui.text "Line 4";
        Ui.text "Line 5";
      ]
  in
  let output = Test_utils.render_to_string ~width:10 ~height:10 element in
  Alcotest.(check bool)
    "vertical gradient renders"
    (String.length output > 0)
    true

(** Test gradient color interpolation *)
let test_gradient_interpolation () =
  let module S = Ui.Style in
  (* Test RGB interpolation *)
  let gradient =
    S.gradient
      ~colors:[ S.rgb 255 0 0; S.rgb 0 0 255 ] (* Red to Blue *)
      ~direction:`Horizontal
  in

  (* Create element with gradient *)
  let style = S.gradient_fg ~colors:gradient.S.colors ~direction:`Horizontal in
  let element = Ui.text ~style "Gradient Test Text" in
  let output = Test_utils.render_to_string ~width:20 ~height:1 element in
  Alcotest.(check bool)
    "gradient interpolation renders"
    (String.length output > 0)
    true

(** Test combined gradients *)
let test_combined_gradients () =
  let module S = Ui.Style in
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

  (* Test rendering *)
  let element = Ui.text ~style:combined "Combined Gradients" in
  let output = Test_utils.render_to_string ~width:20 ~height:5 element in
  Alcotest.(check bool)
    "combined gradient renders"
    (String.length output > 0)
    true

(** Test gradient with boxes and containers *)
let test_gradient_with_containers () =
  let module S = Ui.Style in
  (* Test gradient in vbox *)
  let gradient =
    S.gradient_bg ~colors:[ S.Red; S.Yellow ] ~direction:`Vertical
  in
  let element =
    Ui.vbox ~style:gradient ~gap:(`Cells 1)
      [ Ui.text "Top"; Ui.text "Middle"; Ui.text "Bottom" ]
  in
  let output = Test_utils.render_to_string ~width:10 ~height:5 element in
  Alcotest.(check bool)
    "gradient in vbox renders"
    (String.length output > 0)
    true;

  (* Test gradient in hbox *)
  let h_gradient =
    S.gradient_bg ~colors:[ S.Blue; S.Green ] ~direction:`Horizontal
  in
  let element =
    Ui.hbox ~style:h_gradient ~gap:(`Cells 2)
      [ Ui.text "Left"; Ui.text "Center"; Ui.text "Right" ]
  in
  let output = Test_utils.render_to_string ~width:30 ~height:1 element in
  Alcotest.(check bool)
    "gradient in hbox renders"
    (String.length output > 0)
    true

(** Test style composition with gradients *)
let test_gradient_style_composition () =
  let module S = Ui.Style in
  (* Test gradient with other text attributes *)
  let gradient_style =
    S.gradient_fg ~colors:[ S.Red; S.Blue ] ~direction:`Horizontal
  in
  let combined = S.(gradient_style ++ bold ++ underline) in

  (* Check all attributes are present *)
  Alcotest.(check bool)
    "has gradient"
    (match combined.S.fg with Some (S.Gradient _) -> true | _ -> false)
    true;
  Alcotest.(check bool) "is bold" combined.S.bold true;
  Alcotest.(check bool) "is underlined" combined.S.underline true

(** Test gradient on multiline text *)
let test_gradient_multiline () =
  let module S = Ui.Style in
  let gradient = S.gradient_fg ~colors:[ S.Red; S.Blue ] ~direction:`Vertical in
  let element = Ui.text ~style:gradient "Line 1\nLine 2\nLine 3" in
  let output = Test_utils.render_to_string ~width:10 ~height:3 element in
  Alcotest.(check bool)
    "multiline gradient renders"
    (String.length output > 0)
    true

(** Test gradient memory/performance *)
let test_gradient_performance () =
  let module S = Ui.Style in
  (* Create a large gradient *)
  let colors = List.init 100 (fun i -> S.Index i) in
  let gradient = S.gradient ~colors ~direction:`Horizontal in

  (* Ensure it can be created without issues *)
  Alcotest.(check int)
    "large gradient created"
    (List.length gradient.S.colors)
    100;

  (* Render a large element with gradient *)
  let style = S.gradient_fg ~colors:gradient.S.colors ~direction:`Horizontal in
  let long_text = String.make 100 'X' in
  let element = Ui.text ~style long_text in
  let output = Test_utils.render_to_string ~width:100 ~height:1 element in
  Alcotest.(check bool) "large gradient renders" (String.length output > 0) true

(** Test gradient with padding and borders *)
let test_gradient_with_padding () =
  let module S = Ui.Style in
  let gradient =
    S.gradient_bg ~colors:[ S.Red; S.Blue ] ~direction:`Horizontal
  in
  let element =
    Ui.vbox ~style:gradient ~padding:(Ui.all 2) [ Ui.text "Padded" ]
  in
  let output = Test_utils.render_to_string ~width:20 ~height:5 element in
  Alcotest.(check bool)
    "gradient with padding renders"
    (String.length output > 0)
    true

(** Test gradient direction variations *)
let test_gradient_direction_variations () =
  let module S = Ui.Style in
  (* Test that both horizontal and vertical gradients work *)
  let h_gradient =
    S.gradient_fg ~colors:[ S.Red; S.Blue ] ~direction:`Horizontal
  in
  let v_gradient =
    S.gradient_fg ~colors:[ S.Red; S.Blue ] ~direction:`Vertical
  in
  let h_element = Ui.text ~style:h_gradient "Horizontal" in
  let v_element = Ui.text ~style:v_gradient "Vertical" in
  let h_output = Test_utils.render_to_string ~width:10 ~height:1 h_element in
  let v_output = Test_utils.render_to_string ~width:10 ~height:1 v_element in
  Alcotest.(check bool)
    "horizontal gradient renders"
    (String.length h_output > 0)
    true;
  Alcotest.(check bool)
    "vertical gradient renders"
    (String.length v_output > 0)
    true

(** Run all tests *)
let () =
  let test_suite =
    [
      ("gradient style creation", `Quick, test_gradient_style_creation);
      ("gradient with RGB", `Quick, test_gradient_with_rgb);
      ("gradient rendering", `Quick, test_gradient_rendering);
      ("adaptive colors", `Quick, test_adaptive_colors);
      ("gradient edge cases", `Quick, test_gradient_edge_cases);
      ("gradient directions", `Quick, test_gradient_directions);
      ("gradient interpolation", `Quick, test_gradient_interpolation);
      ("combined gradients", `Quick, test_combined_gradients);
      ("gradient with containers", `Quick, test_gradient_with_containers);
      ("gradient style composition", `Quick, test_gradient_style_composition);
      ("gradient multiline", `Quick, test_gradient_multiline);
      ("gradient performance", `Quick, test_gradient_performance);
      ("gradient with padding", `Quick, test_gradient_with_padding);
      ( "gradient direction variations",
        `Quick,
        test_gradient_direction_variations );
    ]
  in
  Alcotest.run "Gradient" [ ("Gradient tests", test_suite) ]
