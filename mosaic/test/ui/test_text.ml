open Mosaic_ui

let render_boxed ?(width = 20) ?(height = 6) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors:false ~width:(width + 2) ~height:(height + 2) content

let%expect_test "char wrap splits long words" =
  render_boxed ~width:12 ~height:2
    (text ~id:"t" ~size:(size ~width:12 ~height:2) ~wrap_mode:`Char
       ~content:"Supercalifragilistic" ());
  [%expect_exact
    {|
┌────────────┐
│Supercalifra│
│gilistic    │
└────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "word wrap breaks on spaces" =
  render_boxed ~width:12 ~height:2
    (text ~id:"t" ~size:(size ~width:12 ~height:2) ~wrap_mode:`Word
       ~content:"hello world wrap" ());
  [%expect_exact
    {|
┌────────────┐
│hello world │
│wrap        │
└────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "no wrap truncates" =
  render_boxed ~width:12 ~height:1
    (text ~id:"t" ~wrap_mode:`None ~content:"1234567890ABCDEF" ());
  [%expect_exact
    {|
┌────────────┐
│1234567890AB│
└────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "multiline content" =
  render_boxed ~width:10 ~height:3
    (text ~id:"t" ~content:"Line 1\nLine 2\nLine 3" ());
  [%expect_exact
    {|
┌──────────┐
│Line 1    │
│Line 2    │
│Line 3    │
└──────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "empty content renders blank" =
  render_boxed ~width:8 ~height:2
    (text ~id:"t" ~content:"" ());
  [%expect_exact
    {|
┌────────┐
│        │
│        │
└────────┘
|}] [@@ocamlformat "disable"]
