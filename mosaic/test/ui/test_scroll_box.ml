open Mosaic_ui

let render_boxed ?(width = 20) ?(height = 10) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors:false ~width:(width + 2) ~height:(height + 2) content

let%expect_test "scroll box with content fitting viewport" =
  render_boxed ~width:15 ~height:3
    (scroll_box ~id:"sb" ~scroll_y:true ~size:(size ~width:15 ~height:3)
       [ text ~id:"t" "Line 1\nLine 2\nLine 3" ]);
  [%expect_exact
    {|
┌───────────────┐
│Line 1         │
│Line 2         │
│Line 3         │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "scroll box with overflowing content" =
  render_boxed ~width:15 ~height:3
    (scroll_box ~id:"sb" ~scroll_y:true ~size:(size ~width:15 ~height:3)
       [ text ~id:"t" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5" ]);
  [%expect_exact
    {|
┌───────────────┐
│Line 1        █│
│Line 2        █│
│Line 3        █│
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "scroll box no scroll bars when disabled" =
  render_boxed ~width:15 ~height:3
    (scroll_box ~id:"sb" ~scroll_y:false ~size:(size ~width:15 ~height:3)
       [ text ~id:"t" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5" ]);
  [%expect_exact
    {|
┌───────────────┐
│Line 1         │
│Line 2         │
│Line 3         │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "scroll box horizontal content" =
  render_boxed ~width:10 ~height:2
    (scroll_box ~id:"sb" ~scroll_x:true ~size:(size ~width:10 ~height:2)
       [ text ~id:"t" ~wrap_mode:`None "This is a very long line" ]);
  [%expect_exact
    {|
┌──────────┐
│This is a │
│          │
└──────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "scroll box both directions" =
  render_boxed ~width:10 ~height:4
    (scroll_box ~id:"sb" ~scroll_x:true ~scroll_y:true
       ~size:(size ~width:10 ~height:4)
       [
         text ~id:"t"
           ~wrap_mode:`None "AAAAAAAAAAAAAAAA\nBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCC\nDDDDDDDDDDDDDDDD\nEEEEEEEEEEEEEEEE";
       ]);
  [%expect_exact
    {|
┌──────────┐
│AAAAAAAAA█│
│BBBBBBBBB█│
│CCCCCCCCC█│
│DDDDDDDDD█│
└──────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "scroll box empty content" =
  render_boxed ~width:12 ~height:4
    (scroll_box ~id:"sb" ~scroll_y:true ~size:(size ~width:12 ~height:4) []);
  [%expect_exact
    {|
┌────────────┐
│            │
│            │
│            │
│            │
└────────────┘
|}] [@@ocamlformat "disable"]

(* Interaction tests *)

(* Note: scroll_to/scroll_by during on_mount don't take effect because
   layout hasn't been computed yet. These tests verify the API is callable
   but the visual result shows initial position. For scroll behavior testing,
   use the TEA harness which supports proper frame sequencing. *)

let%expect_test "scroll_to during on_mount (layout not yet computed)" =
  render_boxed ~width:15 ~height:3
    (scroll_box ~id:"sb" ~scroll_y:true ~size:(size ~width:15 ~height:3)
       ~on_mount:(fun sb -> Scroll_box.scroll_to ~y:2 sb)
       [ text ~id:"t" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5" ]);
  (* Scroll position is queued but layout hasn't computed scroll limits yet *)
  [%expect_exact
    {|
┌───────────────┐
│Line 1        █│
│Line 2        █│
│Line 3        █│
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_sticky_scroll disables sticky behavior" =
  render_boxed ~width:15 ~height:3
    (scroll_box ~id:"sb" ~scroll_y:true ~sticky_scroll:true
       ~size:(size ~width:15 ~height:3)
       ~on_mount:(fun sb -> Scroll_box.set_sticky_scroll sb false)
       [ text ~id:"t" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5" ]);
  [%expect_exact
    {|
┌───────────────┐
│Line 1        █│
│Line 2        █│
│Line 3        █│
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_viewport_culling can be toggled" =
  render_boxed ~width:15 ~height:3
    (scroll_box ~id:"sb" ~scroll_y:true ~viewport_culling:false
       ~size:(size ~width:15 ~height:3)
       ~on_mount:(fun sb -> Scroll_box.set_viewport_culling sb true)
       [ text ~id:"t" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5" ]);
  [%expect_exact
    {|
┌───────────────┐
│Line 1        █│
│Line 2        █│
│Line 3        █│
└───────────────┘
|}] [@@ocamlformat "disable"]
