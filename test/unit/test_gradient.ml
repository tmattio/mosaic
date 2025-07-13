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
        ] );
    ]
