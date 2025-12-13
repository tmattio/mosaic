open Mosaic_ui

let render_boxed ?(width = 20) ?(height = 3) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors:false ~width:(width + 2) ~height:(height + 2) content

let%expect_test "horizontal slider at minimum" =
  render_boxed ~width:20 ~height:1
    (slider ~id:"s" ~orientation:`Horizontal ~min:0. ~max:100. ~value:0.
       ~viewport_size:10. ~size:(size ~width:20 ~height:1) ());
  [%expect_exact
    {|
┌────────────────────┐
│█▌                  │
└────────────────────┘
|}]

let%expect_test "horizontal slider at maximum" =
  render_boxed ~width:20 ~height:1
    (slider ~id:"s" ~orientation:`Horizontal ~min:0. ~max:100. ~value:100.
       ~viewport_size:10. ~size:(size ~width:20 ~height:1) ());
  [%expect_exact
    {|
┌────────────────────┐
│                  ▐█│
└────────────────────┘
|}]

let%expect_test "horizontal slider at midpoint" =
  render_boxed ~width:20 ~height:1
    (slider ~id:"s" ~orientation:`Horizontal ~min:0. ~max:100. ~value:50.
       ~viewport_size:10. ~size:(size ~width:20 ~height:1) ());
  [%expect_exact
    {|
┌────────────────────┐
│         ▐█         │
└────────────────────┘
|}]

let%expect_test "vertical slider at minimum" =
  render_boxed ~width:1 ~height:10
    (slider ~id:"s" ~orientation:`Vertical ~min:0. ~max:100. ~value:0.
       ~viewport_size:10. ~size:(size ~width:1 ~height:10) ());
  [%expect_exact {|
┌─┐
│▀│
│ │
│ │
│ │
│ │
│ │
│ │
│ │
│ │
│ │
└─┘
|}]

let%expect_test "vertical slider at maximum" =
  render_boxed ~width:1 ~height:10
    (slider ~id:"s" ~orientation:`Vertical ~min:0. ~max:100. ~value:100.
       ~viewport_size:10. ~size:(size ~width:1 ~height:10) ());
  [%expect_exact {|
┌─┐
│ │
│ │
│ │
│ │
│ │
│ │
│ │
│ │
│ │
│▄│
└─┘
|}]

(* Interaction tests *)

let%expect_test "set_value moves horizontal slider" =
  render_boxed ~width:20 ~height:1
    (slider ~id:"s" ~orientation:`Horizontal ~min:0. ~max:100. ~value:0.
       ~viewport_size:10.
       ~on_mount:(fun s -> Slider.set_value s 75.)
       ~size:(size ~width:20 ~height:1) ());
  [%expect_exact
    {|
┌────────────────────┐
│              █▌    │
└────────────────────┘
|}]

let%expect_test "set_value moves vertical slider" =
  render_boxed ~width:1 ~height:10
    (slider ~id:"s" ~orientation:`Vertical ~min:0. ~max:100. ~value:0.
       ~viewport_size:10.
       ~on_mount:(fun s -> Slider.set_value s 50.)
       ~size:(size ~width:1 ~height:10) ());
  [%expect_exact {|
┌─┐
│ │
│ │
│ │
│ │
│ │
│▀│
│ │
│ │
│ │
│ │
└─┘
|}]

let%expect_test "set_value clamps to range" =
  render_boxed ~width:20 ~height:1
    (slider ~id:"s" ~orientation:`Horizontal ~min:0. ~max:100. ~value:50.
       ~viewport_size:10.
       ~on_mount:(fun s -> Slider.set_value s 200.) (* exceeds max *)
       ~size:(size ~width:20 ~height:1) ());
  [%expect_exact
    {|
┌────────────────────┐
│                  ▐█│
└────────────────────┘
|}]

let%expect_test "set_range changes slider bounds" =
  render_boxed ~width:20 ~height:1
    (slider ~id:"s" ~orientation:`Horizontal ~min:0. ~max:100. ~value:50.
       ~viewport_size:10.
       ~on_mount:(fun s -> Slider.set_range s ~min:0. ~max:200.)
       ~size:(size ~width:20 ~height:1) ());
  (* value 50 is now at 25% of 0-200 range *)
  [%expect_exact
    {|
┌────────────────────┐
│     ▌              │
└────────────────────┘
|}]

let%expect_test "set_viewport_size changes thumb size" =
  render_boxed ~width:20 ~height:1
    (slider ~id:"s" ~orientation:`Horizontal ~min:0. ~max:100. ~value:0.
       ~viewport_size:10.
       ~on_mount:(fun s -> Slider.set_viewport_size s 50.)
         (* larger viewport = bigger thumb *)
       ~size:(size ~width:20 ~height:1) ());
  [%expect_exact
    {|
┌────────────────────┐
│██████▌             │
└────────────────────┘
|}]
