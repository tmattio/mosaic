open Mosaic_ui

let render_boxed ?(colors = false) ?(width = 20) ?(height = 6) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors ~width:(width + 2) ~height:(height + 2) content

let%expect_test "border disabled ignores custom sides" =
  (* border_sides is only effective when border:true; we set border:false
     explicitly to verify that border_sides is ignored when border is false *)
  let content =
    box ~id:"test" ~size:(size ~width:16 ~height:5) ~border:false
      ~border_sides:[ `Top; `Bottom ]
      [ text ~id:"text" ~content:"No border" () ]
  in
  render_boxed ~width:16 ~height:5 content;
  [%expect_exact
    {|
┌────────────────┐
│No border       │
│                │
│                │
│                │
│                │
└────────────────┘
|}]

let%expect_test "border clips content area" =
  let content =
    box ~id:"test" ~size:(size ~width:14 ~height:4) ~border:true
      [ text ~id:"text" ~content:"ABCDEFGHIJKLMN" ~wrap_mode:`None () ]
  in
  render_boxed ~width:14 ~height:4 content;
  [%expect_exact
    {|
┌──────────────┐
│┌────────────┐│
││ABCDEFGHIJKL││
││            ││
│└────────────┘│
└──────────────┘
|}]

let%expect_test "box with selective sides" =
  let content =
    box ~id:"test" ~size:(size ~width:18 ~height:4) ~border:true
      ~border_sides:[ `Top; `Bottom ]
      [ text ~id:"text" ~content:"Horizontal" ~wrap_mode:`None () ]
  in
  render_boxed ~width:18 ~height:4 content;
  [%expect_exact
    {|
┌──────────────────┐
│──────────────────│
│Horizontal        │
│                  │
│──────────────────│
└──────────────────┘
|}]

let style_box label border_style =
  box ~id:label ~size:(size ~width:14 ~height:3) ~border:true ~border_style
    [ text ~id:(label ^ "_text") ~content:label () ]

let%expect_test "box border styles" =
  let content =
    box ~id:"container" ~flex_direction:Column ~gap:(gap 1)
      ~align_items:Flex_start
      [
        style_box "Double" Border.double;
        style_box "Rounded" Border.rounded;
        style_box "Heavy" Border.heavy;
        style_box "Ascii" Border.ascii;
      ]
  in
  (* 4 boxes * 3 height + 3 gaps = 15 rows needed to show all styles *)
  render_boxed ~width:18 ~height:15 content;
  [%expect_exact
    {|
┌──────────────────┐
│╔════════════╗    │
│║Double      ║    │
│╚════════════╝    │
│                  │
│╭────────────╮    │
││Rounded     │    │
│╰────────────╯    │
│                  │
│┏━━━━━━━━━━━━┓    │
│┃Heavy       ┃    │
│┗━━━━━━━━━━━━┛    │
│                  │
│+------------+    │
│|Ascii       |    │
│+------------+    │
└──────────────────┘
|}]

let title_box title alignment =
  box ~id:title ~size:(size ~width:18 ~height:4) ~border:true ~title
    ~title_alignment:alignment
    [ text ~id:(title ^ "_text") ~content:(String.capitalize_ascii title) () ]

let%expect_test "box title alignments" =
  let content =
    box ~id:"container" ~flex_direction:Column ~gap:(gap 1)
      ~align_items:Flex_start
      [
        title_box "left" `Left;
        title_box "centered" `Center;
        title_box "right" `Right;
      ]
  in
  render_boxed ~width:22 ~height:14 content;
  [%expect_exact
    {|
┌──────────────────────┐
│┌─left───────────┐    │
││Left            │    │
││                │    │
│└────────────────┘    │
│                      │
│┌────centered────┐    │
││Centered        │    │
││                │    │
│└────────────────┘    │
│                      │
│┌──────────right─┐    │
││Right           │    │
││                │    │
│└────────────────┘    │
└──────────────────────┘
|}]

let%expect_test "title omitted without top border" =
  (* title is only displayed when top border is present; border_sides excludes top *)
  let content =
    box ~id:"test" ~size:(size ~width:16 ~height:5) ~border:true ~title:"Hidden"
      ~border_sides:[ `Left; `Right; `Bottom ]
      [ text ~id:"text" ~content:"No top border" () ]
  in
  render_boxed ~width:16 ~height:5 content;
  [%expect_exact
    {|
┌────────────────┐
││No top border ││
││              ││
││              ││
││              ││
│└──────────────┘│
└────────────────┘
|}]

let char_code c = Int32.of_int (Char.code c)

let funky_border =
  {
    Grid.Border.top_left = char_code '/';
    top_right = char_code '\\';
    bottom_left = char_code '\\';
    bottom_right = char_code '/';
    horizontal = char_code '~';
    vertical = char_code '!';
    top_t = char_code '^';
    bottom_t = char_code 'v';
    left_t = char_code '<';
    right_t = char_code '>';
    cross = char_code '+';
  }

let%expect_test "custom border characters" =
  let content =
    box ~id:"test" ~size:(size ~width:14 ~height:4) ~border:true
      ~custom_border_chars:funky_border
      [ text ~id:"text" ~content:"Custom" () ]
  in
  render_boxed ~width:14 ~height:4 content;
  [%expect_exact
    {|
┌──────────────┐
│/~~~~~~~~~~~~\│
│!Custom      !│
│!            !│
│\~~~~~~~~~~~~/│
└──────────────┘
|}]

let%expect_test "border style via props" =
  let content =
    box ~id:"test" ~size:(size ~width:14 ~height:4) ~border:true
      ~border_style:Border.double
      [ text ~id:"text" ~content:"Double" () ]
  in
  render_boxed ~width:14 ~height:4 content;
  [%expect_exact
    {|
┌──────────────┐
│╔════════════╗│
│║Double      ║│
│║            ║│
│╚════════════╝│
└──────────────┘
|}]

let%expect_test "nested boxes render without overlap" =
  (* inner box needs height:3 to have 1 row of content (height - 2 for borders) *)
  let inner =
    box ~id:"inner" ~size:(size ~width:8 ~height:3) ~border:true
      [ text ~id:"inner_text" ~content:"Inner" () ]
  in
  let content =
    box ~id:"outer_box" ~size:(size ~width:12 ~height:6) ~border:true [ inner ]
  in
  render_boxed ~width:12 ~height:6 content;
  [%expect_exact
    {|
┌────────────┐
│┌──────────┐│
││┌──────┐  ││
│││Inner │  ││
││└──────┘  ││
││          ││
│└──────────┘│
└────────────┘
|}]

let%expect_test "colored box renders orange background" =
  let orange = Ansi.Color.of_rgb 244 165 96 in
  let border_color = Ansi.Color.of_rgb 30 30 30 in
  let bold_dark =
    Ansi.Style.make ~bold:true ~fg:(Ansi.Color.of_rgb 16 16 16) ()
  in
  let dim_dark =
    Ansi.Style.make ~dim:true ~fg:(Ansi.Color.of_rgb 16 16 16) ()
  in
  let content =
    box ~id:"colored" ~size:(size ~width:12 ~height:5) ~border:true
      ~border_color ~background:orange
      [
        box ~id:"inner_box" ~flex_direction:Column ~align_items:Center
          ~justify_content:Center
          [
            text ~id:"ideas" ~content:"Ideas" ~text_style:bold_dark ();
            text ~id:"drag" ~content:"Drag me" ~text_style:dim_dark ();
          ];
      ]
  in
  render_boxed ~colors:true ~width:12 ~height:5 content;
  [%expect_exact
    {|
[0;38;2;255;255;255m┌────────────┐[0m
[0;38;2;255;255;255m│[0;38;2;30;30;30;48;2;244;165;96m┌──────────┐[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;30;30;30;48;2;244;165;96m│[0;38;2;255;255;255;48;2;244;165;96m          [0;38;2;30;30;30;48;2;244;165;96m│[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;30;30;30;48;2;244;165;96m│[0;38;2;255;255;255;48;2;244;165;96m [0;38;2;16;16;16;48;2;244;165;96;1mIdeas[0;38;2;255;255;255;48;2;244;165;96m    [0;38;2;30;30;30;48;2;244;165;96m│[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;30;30;30;48;2;244;165;96m│[0;38;2;16;16;16;48;2;244;165;96;2mDrag me[0;38;2;255;255;255;48;2;244;165;96m   [0;38;2;30;30;30;48;2;244;165;96m│[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;30;30;30;48;2;244;165;96m└──────────┘[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m└────────────┘[0m[0m
|}]
