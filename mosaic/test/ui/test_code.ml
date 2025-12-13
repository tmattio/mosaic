open Mosaic_ui

let render_boxed ?(width = 30) ?(height = 6) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors:false ~width:(width + 2) ~height:(height + 2) content

let%expect_test "code empty content" =
  render_boxed ~width:20 ~height:3
    (code ~id:"c" ~size:(size ~width:20 ~height:3) "");
  [%expect_exact
    {|
┌────────────────────┐
│                    │
│                    │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "code single line" =
  render_boxed ~width:25 ~height:1
    (code ~id:"c" ~size:(size ~width:25 ~height:1) "let x = 42");
  [%expect_exact
    {|
┌─────────────────────────┐
│let x = 42               │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "code multiple lines" =
  render_boxed ~width:25 ~height:4
    (code ~id:"c" ~size:(size ~width:25 ~height:4)
       "let greet name =\n  print_endline name\n\nlet () = greet \"hi\"");
  [%expect_exact
    {|
┌─────────────────────────┐
│let greet name =         │
│  print_endline name     │
│                         │
│let () = greet "hi"      │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "code with word wrap" =
  render_boxed ~width:15 ~height:3
    (code ~id:"c" ~wrap_mode:`Word ~size:(size ~width:15 ~height:3)
       "let very_long_identifier = 123");
  [%expect_exact
    {|
┌───────────────┐
│let            │
│very_long_ident│
│ifier = 123    │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "code with char wrap" =
  render_boxed ~width:12 ~height:3
    (code ~id:"c" ~wrap_mode:`Char ~size:(size ~width:12 ~height:3)
       "abcdefghijklmnopqrstuvwxyz");
  [%expect_exact
    {|
┌────────────┐
│abcdefghijkl│
│mnopqrstuvwx│
│yz          │
└────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "code no wrap truncates" =
  render_boxed ~width:10 ~height:1
    (code ~id:"c" ~wrap_mode:`None ~size:(size ~width:10 ~height:1)
       "0123456789ABCDEF");
  [%expect_exact
    {|
┌──────────┐
│0123456789│
└──────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "code preserves indentation" =
  render_boxed ~width:20 ~height:4
    (code ~id:"c" ~size:(size ~width:20 ~height:4)
       "match x with\n| A -> 1\n| B -> 2");
  [%expect_exact
    {|
┌────────────────────┐
│match x with        │
│| A -> 1            │
│| B -> 2            │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "code json string without grammar (no conceal)" =
  render_boxed ~width:10 ~height:1
    (code ~id:"c" ~size:(size ~width:10 ~height:1) "\"X\"");
  [%expect_exact
    {|
┌──────────┐
│"X"       │
└──────────┘
|}] [@@ocamlformat "disable"]
