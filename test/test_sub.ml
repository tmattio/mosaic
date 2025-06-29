(** Tests for the Sub module *)

open Mosaic

let test_none_sub () =
  (* We can't pattern match on abstract types *)
  let _ = Sub.none in
  ()

let test_keyboard_sub () =
  (* Test keyboard subscription creation *)
  let _ = Sub.keyboard (fun key -> `Key key) in
  ()

let test_keyboard_filter () =
  (* Test keyboard filter creation *)
  let _ = Sub.keyboard_filter (fun key ->
      if key.key = Enter then Some `Enter else None) in
  ()

let test_on_key () =
  (* Test simple key *)
  let _ = Sub.on_key Enter `EnterPressed in
  (* Test with modifiers *)
  let _ = Sub.on_key ~ctrl:true (Char (Uchar.of_char 'C')) `CtrlC in
  ()

let test_on_char () =
  let _ = Sub.on_char 'q' `Quit in
  ()

let test_mouse_sub () =
  let _ = Sub.mouse (fun evt -> `Mouse evt) in
  ()

let test_on_click () =
  let _ = Sub.on_click (fun x y btn -> `Click (x, y, btn)) in
  ()

let test_on_resize () =
  let _ = Sub.on_resize (fun w h -> `Resize (w, h)) in
  ()

let test_on_focus () =
  let _ = Sub.on_focus `GotFocus in
  ()

let test_on_blur () =
  let _ = Sub.on_blur `LostFocus in
  ()

let test_batch_sub () =
  (* Empty batch *)
  let _ = Sub.batch [] in
  (* Single sub *)
  let _ = Sub.batch [ Sub.on_key Enter `Enter ] in
  (* Multiple subs *)
  let _ = Sub.batch [ Sub.on_key Enter `Enter; Sub.on_focus `Focus ] in
  (* With nones *)
  let _ = Sub.batch [ Sub.none; Sub.on_key Enter `Enter; Sub.none ] in
  ()

let test_map_sub () =
  (* Map over keyboard sub *)
  let _ = Sub.map (fun `A -> `B) (Sub.on_key Enter `A) in
  (* Map over None *)
  let _ = Sub.map (fun x -> x) Sub.none in
  (* Map over Batch *)
  let _ = Sub.map (fun `A -> `B) (Sub.batch [ Sub.on_key Enter `A; Sub.on_focus `A ]) in
  ()

let test_collectors () =
  (* Test keyboard collector *)
  let sub = Sub.batch [ Sub.on_key Enter `Enter; Sub.on_key Tab `Tab; Sub.on_focus `Focus ] in
  let handlers = Sub.collect_keyboard [] sub in
  Alcotest.(check int) "keyboard handlers" 2 (List.length handlers);

  (* Test mouse collector *)
  let sub = Sub.batch [
    Sub.on_click (fun _ _ _ -> `Click);
    Sub.mouse (fun _ -> `Mouse);
    Sub.on_key Enter `Enter;
  ] in
  let handlers = Sub.collect_mouse [] sub in
  Alcotest.(check int) "mouse handlers" 2 (List.length handlers);

  (* Test window collector *)
  let sub = Sub.batch [ Sub.on_resize (fun _ _ -> `Resize); Sub.on_focus `Focus ] in
  let handlers = Sub.collect_window [] sub in
  Alcotest.(check int) "window handlers" 1 (List.length handlers);

  (* Test focus collector *)
  let sub = Sub.batch [ Sub.on_focus `Focus1; Sub.on_focus `Focus2; Sub.on_blur `Blur ] in
  let handlers = Sub.collect_focus [] sub in
  Alcotest.(check int) "focus handlers" 2 (List.length handlers);

  (* Test blur collector *)
  let handlers = Sub.collect_blur [] sub in
  Alcotest.(check int) "blur handlers" 1 (List.length handlers)

let tests =
  [
    ("none sub", `Quick, test_none_sub);
    ("keyboard sub", `Quick, test_keyboard_sub);
    ("keyboard filter", `Quick, test_keyboard_filter);
    ("on_key", `Quick, test_on_key);
    ("on_char", `Quick, test_on_char);
    ("mouse sub", `Quick, test_mouse_sub);
    ("on_click", `Quick, test_on_click);
    ("on_resize", `Quick, test_on_resize);
    ("on_focus", `Quick, test_on_focus);
    ("on_blur", `Quick, test_on_blur);
    ("batch sub", `Quick, test_batch_sub);
    ("map sub", `Quick, test_map_sub);
    ("collectors", `Quick, test_collectors);
  ]

let () = Alcotest.run "Sub" [ ("subscriptions", tests) ]