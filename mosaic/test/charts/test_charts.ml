open Mosaic_ui
open Mosaic_charts
module Style = Ansi.Style

let column ~id ?gap children = box ~id ~flex_direction:Column ?gap children

let render_node ~width ~height node =
  let frame =
    box ~id:"frame" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ node ]
  in
  print_newline ();
  print ~colors:false ~width:(width + 2) ~height:(height + 2) frame

let render_chart ~width ~height draw =
  let draw' canvas ~width ~height = ignore (draw canvas ~width ~height) in
  let canvas =
    canvas ~id:"canvas" ~initial_width:width ~initial_height:height ~draw:draw'
      ()
  in
  render_node ~width ~height canvas

let simple_points = [| (0.0, 0.0); (1.0, 2.0); (2.0, 1.0); (3.0, 3.0) |]

let sine_wave points =
  Array.init points (fun i ->
      let x = float_of_int i /. float_of_int (points - 1) *. 2.0 *. Float.pi in
      (x, sin x))

let gapped_series =
  [|
    (0.0, Some 1.0);
    (1.0, Some 2.0);
    (2.0, None);
    (3.0, None);
    (4.0, Some 1.5);
    (5.0, Some 2.5);
  |]

let bar_simple = [| ("Q1", 30.0); ("Q2", 45.0); ("Q3", 25.0); ("Q4", 50.0) |]
let horizontal_bars = [| ("A", 20.0); ("B", 30.0); ("C", 15.0) |]

let heat_points =
  Array.concat
    (List.map Array.of_list
       (List.init 5 (fun x ->
            List.init 5 (fun y ->
                ( float_of_int x,
                  float_of_int y,
                  sin (float_of_int x /. 2.0) *. cos (float_of_int y /. 2.0) )))))

let candle_data =
  [|
    Mark.{ time = 0.0; open_ = 100.0; high = 110.0; low = 95.0; close = 105.0 };
    Mark.{ time = 1.0; open_ = 105.0; high = 115.0; low = 100.0; close = 98.0 };
    Mark.{ time = 2.0; open_ = 98.0; high = 105.0; low = 92.0; close = 102.0 };
  |]

let%expect_test "line chart with axes" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.default ~y:Axis.default
      |> line ~x:fst ~y:snd simple_points
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:30 ~height:10 draw;
  [%expect_exact {|
┌──────────────────────────────┐
│  3 ─│                      ╱╱│
│2.5 ─│                    ╱╱  │
│  2 ─│       ╱╲╲╲╲     ╱╱╱    │
│1.5 ─│     ╱╱     ╲╲╲╱╱       │
│0.5 ─│  ╱╱╱                   │
│  0 ─│╱╱                      │
│     └────────────────────────│
│      │   │   │   │  │   │   ││
│      0  0.5  1  1.5 2  2.5  3│
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "line simple arc (no axes)" =
  let draw canvas ~width ~height =
    let data = [| (0., 0.); (9., 4.) |] in
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> line ~kind:`Wave ~x:fst ~y:snd data
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:10 ~height:5 draw;
  [%expect_exact
    {|
┌──────────┐
│        ╭─│
│      ╭─╯ │
│    ╭─╯   │
│  ╭─╯     │
│──╯       │
└──────────┘
|}]

let%expect_test "line simple horizontal (no axes)" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 9.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 4.)) ())
      |> line ~kind:`Line ~x:fst ~y:snd [| (0., 2.); (9., 2.) |]
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:10 ~height:5 draw;
  [%expect_exact
    {|
┌──────────┐
│          │
│          │
│──────────│
│          │
│          │
└──────────┘
|}]

let%expect_test "line chart with braille rendering" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.default ~y:Axis.default
      |> line ~kind:`Braille ~x:fst ~y:snd (sine_wave 20)
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:25 ~height:8 draw;
  [%expect_exact {|
┌─────────────────────────┐
│     1 ─│ ⢀⠐⠈⠉⠠⡀         │
│   0.5 ─│⡠⠃    ⠈⢠        │
│  -0.0 ─│        ⠢⡀    ⠠⠊│
│    -1 ─│         ⠈⠠⢀⣀⠔⠁ │
│        └────────────────│
│         │ │  │ │  │ │ │ │
│         0 1  2 3  4 5 6 │
│                         │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "line braille (no axes)" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 9.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 4.)) ())
      |> line ~kind:`Braille ~x:fst ~y:snd [| (0., 0.); (9., 4.) |]
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:10 ~height:5 draw;
  [%expect_exact
    {|
┌──────────┐
│        ⡠⠊│
│      ⡠⠊  │
│    ⡠⠊    │
│  ⡠⠊      │
│⡠⠊        │
└──────────┘
|}]

let%expect_test "line with gaps skips missing points" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.default ~y:Axis.default
      |> add (Mark.line_opt ~x:fst ~y:(fun (_, y) -> y) gapped_series)
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:30 ~height:8 draw;
  [%expect_exact {|
┌──────────────────────────────┐
│2.6 ─│                      ╱╱│
│2.2 ─│    ╱╱              ╱╱  │
│1.6 ─│  ╱╱              ╱╱    │
│1.2 ─│╱╱                      │
│     └────────────────────────│
│      │    │   │    │   │    ││
│      0    1   2    3   4    5│
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "scatter X glyph" =
  let draw canvas ~width ~height =
    let pts = [| (1., 1.); (3., 3.); (8., 2.) |] in
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 9.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 4.)) ())
      |> scatter ~glyph:"X" ~x:fst ~y:snd pts
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:10 ~height:5 draw;
  [%expect_exact
    {|
┌──────────┐
│          │
│   X      │
│        X │
│ X        │
│          │
└──────────┘
|}]

let%expect_test "scatter braille" =
  let draw canvas ~width ~height =
    let pts = [| (1., 1.); (3., 3.); (8., 2.) |] in
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 9.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 4.)) ())
      |> scatter ~kind:`Braille ~x:fst ~y:snd pts
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:10 ~height:5 draw;
  [%expect_exact
    {|
┌──────────┐
│          │
│   ⠂      │
│        ⠐ │
│ ⠄        │
│          │
└──────────┘
|}]

let%expect_test "circle (line)" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 11.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 7.)) ())
      |> add
           (Mark.circle ~kind:`Line ~cx:fst ~cy:snd
              ~r:(fun _ -> 3.)
              [| (6., 4.) |])
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:12 ~height:8 draw;
  [%expect_exact
    {|
┌────────────┐
│     ███    │
│    █   █   │
│   █     █  │
│   █     █  │
│   █     █  │
│    █   █   │
│     ███    │
│            │
└────────────┘
|}]

let%expect_test "circle braille" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 11.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 7.)) ())
      |> add
           (Mark.circle ~kind:`Braille ~cx:fst ~cy:snd
              ~r:(fun _ -> 3.)
              [| (6., 4.) |])
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:12 ~height:8 draw;
  [%expect_exact
    {|
┌────────────┐
│     ⠁⠈⠈    │
│    ⠁   ⠈   │
│   ⠂     ⠐  │
│   ⠂     ⠐  │
│   ⠄     ⠠  │
│    ⠄   ⠠   │
│     ⡀⢀⢀    │
│            │
└────────────┘
|}]

let%expect_test "vertical bar chart renders columns" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_frame { margins = (0, 1, 2, 0); inner_padding = 0 }
      |> with_x_scale (Scale.band ())
      |> with_axes ~x:Axis.default ~y:Axis.hidden
      |> bars_y ~x:fst ~y:snd bar_simple
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:25 ~height:10 draw;
  [%expect_exact {|
┌─────────────────────────┐
│      ▅▅▅▅       ████    │
│ ▃▃▃▃ ████       ████    │
│ ████ ████  ████ ████    │
│ ████ ████  ████ ████    │
│──────────────────────── │
│   │    │     │    │     │
│  Q1   Q2    Q3   Q4     │
│                         │
│                         │
│                         │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "horizontal bar chart" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_frame { margins = (0, 2, 0, 0); inner_padding = 0 }
      |> with_y_scale (Scale.band ())
      |> with_axes ~x:Axis.hidden ~y:Axis.default
      |> add (Mark.bars_x ~y:fst ~x:snd horizontal_bars)
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:30 ~height:8 draw;
  [%expect_exact {|
┌──────────────────────────────┐
│A ─│████████████████          │
│   │                          │
│B ─│████████████████████████  │
│   │                          │
│   │                          │
│C ─│████████████              │
│   │                          │
│   │                          │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let simple_bars = [| ("A", 2.); ("B", 5.); ("C", 3.); ("D", 6.) |]

let%expect_test "vertical bars (no axis)" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.band ())
      |> bars_y ~x:fst ~y:snd simple_bars
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:11 ~height:8 draw;
  [%expect_exact {|
┌───────────┐
│       █   │
│   ▅   █   │
│   █   █   │
│   █   █   │
│   █ █ █   │
│▅  █ █ █   │
│█  █ █ █   │
│█  █ █ █   │
└───────────┘
|}] [@@ocamlformat "disable"]

let horizontal_bars_2 = [| ("Alpha", 4.); ("Beta", 8.); ("Gamma", 3.) |]

let%expect_test "horizontal bars (no axis)" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_y_scale (Scale.band ())
      |> add (Mark.bars_x ~y:fst ~x:snd horizontal_bars_2)
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:16 ~height:6 draw;
  [%expect_exact {|
┌────────────────┐
│████████        │
│                │
│████████████████│
│██████          │
│                │
│                │
└────────────────┘
|}] [@@ocamlformat "disable"]

let stacked_bars_data =
  let st = Style.default in
  [|
    Mark.
      {
        category = "A";
        segments =
          [
            { value = 2.; style = st; label = None };
            { value = 2.; style = st; label = None };
          ];
      };
    Mark.
      {
        category = "B";
        segments =
          [
            { value = 3.; style = st; label = None };
            { value = 1.; style = st; label = None };
          ];
      };
    Mark.
      {
        category = "C";
        segments =
          [
            { value = 1.; style = st; label = None };
            { value = 4.; style = st; label = None };
          ];
      };
  |]

let%expect_test "stacked bars vertical (no axis)" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.band ())
      |> add (Mark.stacked_bars_y stacked_bars_data)
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:15 ~height:8 draw;
  [%expect_exact {|
┌───────────────┐
│         ██    │
│▃▃   ▃▃  ██    │
│██   ██  ██    │
│██   ██  ██    │
│██   ██  ██    │
│██   ██  ██    │
│██   ██  ██    │
│██   ██  ██    │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "stacked bars horizontal (no axis)" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_y_scale (Scale.band ())
      |> add (Mark.stacked_bars_x stacked_bars_data)
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:20 ~height:6 draw;
  [%expect_exact {|
┌────────────────────┐
│████████████████    │
│                    │
│████████████████    │
│████████████████████│
│                    │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "sparkline bars" =
  let draw canvas ~width ~height =
    Sparkline.draw_values ~kind:`Bars [ 1.0; 3.0; 2.0; 5.0; 4.0; 3.0; 2.0; 4.0 ]
      canvas ~width ~height
  in
  let node =
    column ~id:"sparkline"
      [ text ~id:"label" "CPU Usage:";
        canvas ~id:"spark" ~initial_width:15 ~initial_height:1 ~draw () ]
  in
  render_node ~width:20 ~height:3 node;
  [%expect_exact {|
┌────────────────────┐
│CPU Usage:          │
│       ▂▅▃█▆▅▃▆     │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "sparkline bars (no layout)" =
  let draw canvas ~width ~height =
    Sparkline.draw_values ~kind:`Bars
      [ 1.; 3.; 2.; 5.; 4.; 3.; 2.; 4. ]
      canvas ~width ~height
  in
  render_chart ~width:12 ~height:3 draw;
  [%expect_exact
    {|
┌────────────┐
│       █▃  ▃│
│     ▆▂██▆▂█│
│    ▅███████│
└────────────┘
|}]

let%expect_test "sparkline braille" =
  let draw canvas ~width ~height =
    let m = Sparkline.create ~capacity:10 () in
    Sparkline.push_all m [ 0.; 2.; 4.; 3.; 5.; 4.; 2.; 1. ];
    Sparkline.draw m ~kind:`Braille canvas ~width ~height
  in
  render_chart ~width:10 ~height:3 draw;
  [%expect_exact
    {|
┌──────────┐
│    ⣄⡸⢢   │
│   ⡜ ⠁ ⢣  │
│  ⡰⠁    ⠑ │
└──────────┘
|}]

let%expect_test "heatmap fills cells (shaded)" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> heatmap ~auto_value_range:true ~render:Mark.Shaded
           ~color_scale:
             [ Ansi.Color.Extended 232;
               Ansi.Color.Extended 236;
               Ansi.Color.Extended 240;
               Ansi.Color.Extended 244;
               Ansi.Color.Extended 248;
               Ansi.Color.Extended 252 ]
           ~x:(fun (x, _, _) -> x)
           ~y:(fun (_, y, _) -> y)
           ~value:(fun (_, _, v) -> v)
           heat_points
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:25 ~height:10 draw;
  [%expect_exact {|
┌─────────────────────────┐
│░░                       │
│░░░░                     │
│░░░░░░░░░░░░░░░░░░░░░░░░░│
│░░░░░░░░░░░░░░░░░░░░░░░░░│
│░░░░░░░░░░▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒│
│░░░░░░▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒│
│░░░░░▒▒▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓│
│░░░░░▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓│
│░░░░▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓│
│░░░░▒▒▒▒▒▓▓▓▓▓▓▓▓▓█▓▓▓▓▓▓│
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "candlestick draws bodies and wicks" =
  let draw canvas ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.default ~y:Axis.hidden
      |> candles candle_data
    in
    draw chart canvas ~width ~height
  in
  render_chart ~width:20 ~height:10 draw;
  [%expect_exact {|
┌────────────────────┐
│          │         │
││         │         │
│┃         ┃        ││
││         │        ┃│
││         │        ││
│                   ││
│────────────────────│
││    │    │   │    ││
│0   0.5   1  1.5   2│
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "legend arranges entries horizontally" =
  let draw canvas ~width ~height =
    Legend.draw ~direction:`Horizontal
      [ { label = "Revenue"; style = Style.make ~fg:Ansi.Color.green (); marker = "●" };
        { label = "Costs"; style = Style.make ~fg:Ansi.Color.red (); marker = "●" };
        { label = "Profit"; style = Style.make ~fg:Ansi.Color.blue (); marker = "●" } ]
      canvas ~width ~height
  in
  render_chart ~width:40 ~height:3 draw;
  [%expect_exact {|
┌────────────────────────────────────────┐
│● Revenue  ● Costs  ● Profit            │
│                                        │
│                                        │
└────────────────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "stacked legend renders vertical list" =
  let draw canvas ~width ~height =
    Legend.draw ~direction:`Vertical
      [ { label = "Alpha"; style = Style.make ~fg:Ansi.Color.yellow (); marker = "●" };
        { label = "Beta"; style = Style.make ~fg:Ansi.Color.cyan (); marker = "●" };
        { label = "Gamma"; style = Style.make ~fg:Ansi.Color.magenta (); marker = "●" } ]
      canvas ~width ~height
  in
  render_chart ~width:18 ~height:5 draw;
  [%expect_exact {|
┌──────────────────┐
│● Alpha           │
│● Beta            │
│● Gamma           │
│                  │
│                  │
└──────────────────┘
|}] [@@ocamlformat "disable"]
