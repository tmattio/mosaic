open Mosaic_ui

let render_boxed ?(width = 10) ?(height = 1) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors:false ~width:(width + 2) ~height:(height + 2) content

let%expect_test "dots spinner first frame" =
  render_boxed ~width:2 ~height:1
    (spinner ~id:"s" ~preset:Spinner.Dots ~autoplay:false
       ~size:(size ~width:2 ~height:1) ());
  [%expect_exact
    {|
┌──┐
│⠋ │
└──┘
|}] [@@ocamlformat "disable"]

let%expect_test "line spinner first frame" =
  render_boxed ~width:2 ~height:1
    (spinner ~id:"s" ~preset:Spinner.Line ~autoplay:false
       ~size:(size ~width:2 ~height:1) ());
  [%expect_exact
    {|
┌──┐
│- │
└──┘
|}] [@@ocamlformat "disable"]

let%expect_test "circle spinner first frame" =
  render_boxed ~width:2 ~height:1
    (spinner ~id:"s" ~preset:Spinner.Circle ~autoplay:false
       ~size:(size ~width:2 ~height:1) ());
  [%expect_exact
    {|
┌──┐
│◐ │
└──┘
|}] [@@ocamlformat "disable"]

let%expect_test "bounce spinner first frame" =
  render_boxed ~width:2 ~height:1
    (spinner ~id:"s" ~preset:Spinner.Bounce ~autoplay:false
       ~size:(size ~width:2 ~height:1) ());
  [%expect_exact
    {|
┌──┐
│⠁ │
└──┘
|}] [@@ocamlformat "disable"]

let%expect_test "bar spinner first frame" =
  render_boxed ~width:8 ~height:1
    (spinner ~id:"s" ~preset:Spinner.Bar ~autoplay:false
       ~size:(size ~width:8 ~height:1) ());
  [%expect_exact
    {|
┌────────┐
│[    ]  │
└────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "arrow spinner first frame" =
  render_boxed ~width:2 ~height:1
    (spinner ~id:"s" ~preset:Spinner.Arrow ~autoplay:false
       ~size:(size ~width:2 ~height:1) ());
  [%expect_exact
    {|
┌──┐
│← │
└──┘
|}] [@@ocamlformat "disable"]

let%expect_test "custom frames spinner" =
  render_boxed ~width:2 ~height:1
    (spinner ~id:"s" ~frames:[| "X"; "O" |] ~autoplay:false
       ~size:(size ~width:2 ~height:1) ());
  [%expect_exact
    {|
┌──┐
│X │
└──┘
|}] [@@ocamlformat "disable"]
