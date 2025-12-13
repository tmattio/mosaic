open Mosaic_ui

let render_boxed ?(width = 20) ?(height = 8) element =
  let outer_width = width + 2 in
  let outer_height = height + 2 in
  let frame =
    box ~id:"outer" ~border:true
      ~size:(size ~width:outer_width ~height:outer_height)
      [ element ]
  in
  print_newline ();
  print ~colors:false ~width:outer_width ~height:outer_height frame

let%expect_test "canvas plots text at explicit coordinates" =
  render_boxed ~width:16 ~height:6
    (canvas ~id:"c" ~size:(size ~width:16 ~height:6)
       ~draw:(fun c ~width:_ ~height:_ ->
         Canvas.clear c;
         Canvas.plot c ~x:1 ~y:0 "Top";
         Canvas.plot c ~x:4 ~y:2 "Center";
         Canvas.plot c ~x:0 ~y:5 "Bottom")
       ());
  [%expect_exact
    {|
┌────────────────┐
│ Top            │
│                │
│    Center      │
│                │
│                │
│Bottom          │
└────────────────┘
|}]

let%expect_test "canvas grows when plotting beyond initial buffer" =
  render_boxed ~width:18 ~height:5
    (canvas ~id:"c" ~size:(size ~width:18 ~height:5) ~initial_width:2
       ~initial_height:1
       ~draw:(fun c ~width:_ ~height:_ ->
         Canvas.clear c;
         Canvas.plot c ~x:0 ~y:0 "A";
         Canvas.plot c ~x:12 ~y:2 "Far")
       ());
  [%expect_exact
    {|
┌──────────────────┐
│A                 │
│                  │
│            Far   │
│                  │
│                  │
└──────────────────┘
|}]

let%expect_test "canvas clear removes previous drawings" =
  render_boxed ~width:12 ~height:4
    (canvas ~id:"c" ~size:(size ~width:12 ~height:4)
       ~draw:(fun c ~width:_ ~height:_ ->
         Canvas.plot c ~x:0 ~y:0 "Old";
         Canvas.plot c ~x:6 ~y:3 "Data";
         Canvas.clear c;
         Canvas.plot c ~x:2 ~y:1 "New")
       ());
  [%expect_exact
    {|
┌────────────┐
│            │
│  New       │
│            │
│            │
└────────────┘
|}]

let%expect_test "canvas draw_box with double border draws frame" =
  render_boxed ~width:14 ~height:8
    (canvas ~id:"c" ~size:(size ~width:14 ~height:8)
       ~draw:(fun c ~width:_ ~height:_ ->
         Canvas.clear c;
         Canvas.draw_box c ~x:1 ~y:1 ~width:12 ~height:6
           ~border_style:Grid.Border.double ();
         Canvas.plot c ~x:4 ~y:3 "Hi")
       ());
  [%expect_exact
    {|
┌──────────────┐
│              │
│ ╔══════════╗ │
│ ║          ║ │
│ ║  Hi      ║ │
│ ║          ║ │
│ ║          ║ │
│ ╚══════════╝ │
│              │
└──────────────┘
|}]

let%expect_test "canvas draw_box can limit sides" =
  render_boxed ~width:18 ~height:6
    (canvas ~id:"c" ~size:(size ~width:18 ~height:6)
       ~draw:(fun c ~width:_ ~height:_ ->
         Canvas.clear c;
         Canvas.draw_box c ~x:1 ~y:1 ~width:16 ~height:4
           ~border_sides:[ `Top; `Bottom ] ();
         Canvas.plot c ~x:6 ~y:2 "Only edges")
       ());
  [%expect_exact
    {|
┌──────────────────┐
│                  │
│ ──────────────── │
│      Only edges  │
│                  │
│ ──────────────── │
│                  │
└──────────────────┘
|}]

let%expect_test "canvas draw_line renders basic directions" =
  render_boxed ~width:12 ~height:6
    (canvas ~id:"c" ~size:(size ~width:12 ~height:6)
       ~draw:(fun c ~width ~height ->
         Canvas.clear c;
         let max_x = width - 1 in
         let max_y = height - 1 in
         Canvas.draw_line c ~x1:0 ~y1:0 ~x2:max_x ~y2:0 ();
         Canvas.draw_line c ~x1:0 ~y1:0 ~x2:0 ~y2:max_y ();
         Canvas.draw_line c ~x1:0 ~y1:max_y ~x2:max_x ~y2:0 ())
       ());
  [%expect_exact
    {|
┌────────────┐
││─────────╱╱│
││       ╱╱  │
││     ╱╱    │
││   ╱╱      │
││ ╱╱        │
│╱╱          │
└────────────┘
|}]

let%expect_test "canvas draw_line with braille resolution" =
  render_boxed ~width:8 ~height:4
    (canvas ~id:"c" ~size:(size ~width:8 ~height:4)
       ~draw:(fun c ~width ~height ->
         Canvas.clear c;
         let x2 = (width * 2) - 1 in
         let y2 = (height * 4) - 1 in
         Canvas.draw_line c ~x1:0 ~y1:0 ~x2 ~y2 ~kind:`Braille ())
       ());
  [%expect_exact
    {|
┌────────┐
│⠑⢄      │
│  ⠑⢄    │
│    ⠑⢄  │
│      ⠑⢄│
└────────┘
|}]

let%expect_test "canvas set_intrinsic_size controls layout size" =
  print_newline ();
  print ~colors:false ~width:6 ~height:3
    (canvas ~id:"c" ~initial_width:1 ~initial_height:1
       ~draw:(fun c ~width:_ ~height:_ ->
         Canvas.set_intrinsic_size c ~width:6 ~height:3;
         Canvas.plot c ~x:5 ~y:2 "X")
       ());
  [%expect_exact {|
      
      
     X
|}]

let%expect_test "canvas draw_box with title centered alignment" =
  render_boxed ~width:18 ~height:6
    (canvas ~id:"c" ~size:(size ~width:18 ~height:6)
       ~draw:(fun c ~width:_ ~height:_ ->
         Canvas.clear c;
         Canvas.draw_box c ~x:1 ~y:1 ~width:16 ~height:4 ~title:"Centered"
           ~title_alignment:`Center ())
       ());
  [%expect_exact
    {|
┌──────────────────┐
│                  │
│ ┌───Centered───┐ │
│ │              │ │
│ │              │ │
│ └──────────────┘ │
│                  │
└──────────────────┘
|}]

let%expect_test "canvas clear_intrinsic_size returns to content-driven size" =
  print_newline ();
  print ~colors:false
    (box ~id:"row" ~flex_direction:Row
       [
         canvas ~id:"c" ~initial_width:1 ~initial_height:1
           ~draw:(fun c ~width:_ ~height:_ ->
             Canvas.set_intrinsic_size c ~width:5 ~height:1;
             Canvas.clear_intrinsic_size c;
             Canvas.plot c ~x:0 ~y:0 "A")
           ();
         text ~id:"t" ~content:"B" ();
       ]);
  [%expect_exact {|
AB
|}]

let%expect_test "canvas set_draw swaps callbacks immediately" =
  render_boxed ~width:12 ~height:4
    (canvas ~id:"c" ~size:(size ~width:12 ~height:4)
       ~draw:(fun c ~width:_ ~height:_ ->
         Canvas.clear c;
         Canvas.plot c ~x:0 ~y:0 "First";
         Canvas.set_draw c
           (Some
              (fun c ~width:_ ~height:_ ->
                Canvas.clear c;
                Canvas.plot c ~x:2 ~y:1 "Next")))
       ());
  [%expect_exact
    {|
┌────────────┐
│            │
│  Next      │
│            │
│            │
└────────────┘
|}]
