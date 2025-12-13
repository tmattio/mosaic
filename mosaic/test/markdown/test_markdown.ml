open Mosaic_ui
open Mosaic_markdown

let render_boxed ?(width = 40) ?(height = 10) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors:false ~width:(width + 2) ~height:(height + 2) content

let%expect_test "empty content" =
  render_boxed ~width:20 ~height:3 (markdown "");
  [%expect_exact
    {|
┌────────────────────┐
│                    │
│                    │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "simple paragraph" =
  render_boxed ~width:30 ~height:3
    (markdown ~width:30 "Hello, world!");
  [%expect_exact
    {|
┌──────────────────────────────┐
│Hello, world!                 │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "heading level 1" =
  render_boxed ~width:30 ~height:3
    (markdown ~width:30 "# Title");
  [%expect_exact
    {|
┌──────────────────────────────┐
│# Title                       │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "heading level 2" =
  render_boxed ~width:30 ~height:3
    (markdown ~width:30 "## Subtitle");
  [%expect_exact
    {|
┌──────────────────────────────┐
│## Subtitle                   │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "unordered list" =
  render_boxed ~width:30 ~height:5
    (markdown ~width:30 "- Item 1\n- Item 2\n- Item 3");
  [%expect_exact
    {|
┌──────────────────────────────┐
│• Item 1                      │
│• Item 2                      │
│• Item 3                      │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "ordered list" =
  render_boxed ~width:30 ~height:5
    (markdown ~width:30 "1. First\n2. Second\n3. Third");
  [%expect_exact
    {|
┌──────────────────────────────┐
│1. First                      │
│2. Second                     │
│3. Third                      │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "code block" =
  render_boxed ~width:30 ~height:6
    (markdown ~width:30 "```ocaml\nlet x = 1\n```");
  [%expect_exact
    {|
┌──────────────────────────────┐
│```ocaml                      │
│let x = 1                     │
│```                           │
│                              │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "blockquote" =
  render_boxed ~width:30 ~height:4
    (markdown ~width:30 "> A wise quote");
  [%expect_exact
    {|
┌──────────────────────────────┐
││ A wise quote                │
│                              │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "horizontal rule" =
  render_boxed ~width:30 ~height:5
    (markdown ~width:30 "Above\n\n---\n\nBelow");
  [%expect_exact
    {|
┌──────────────────────────────┐
│Above                         │
│──────────────────────────────│
│Below                         │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "multiple paragraphs" =
  render_boxed ~width:30 ~height:5
    (markdown ~width:30 "First paragraph.\n\nSecond paragraph.");
  [%expect_exact
    {|
┌──────────────────────────────┐
│First paragraph.              │
│                              │
│Second paragraph.             │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "word wrap in paragraph" =
  render_boxed ~width:20 ~height:4
    (markdown ~width:20 "This is a longer sentence that should wrap.");
  [%expect_exact
    {|
┌────────────────────┐
│This is a longer    │
│sentence that should│
│wrap.               │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "nested list" =
  render_boxed ~width:30 ~height:5
    (markdown ~width:30 "- Parent\n  - Child\n- Another");
  [%expect_exact
    {|
┌──────────────────────────────┐
│• Parent                      │
│  • Child                     │
│• Another                     │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]
