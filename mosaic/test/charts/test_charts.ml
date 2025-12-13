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
    canvas ~id:"canvas" ~initial_width:width ~initial_height:height ~draw:draw' ()
  in
  render_node ~width ~height canvas

let simple_points = [ (0.0, 0.0); (1.0, 2.0); (2.0, 1.0); (3.0, 3.0) ]

let sine_wave points =
  List.init points (fun i ->
      let x = float_of_int i /. float_of_int (points - 1) *. 2.0 *. Float.pi in
      (x, sin x))

let gapped_series =
  [
    ( "Incomplete",
      [
        (0.0, Some 1.0);
        (1.0, Some 2.0);
        (2.0, None);
        (3.0, None);
        (4.0, Some 1.5);
        (5.0, Some 2.5);
      ] );
  ]

let time_series_points =
  let base_time = 1_700_000_000.0 in
  List.init 10 (fun i ->
      {
        time = base_time +. (float_of_int i *. 3600.0);
        value = 50.0 +. (20.0 *. sin (float_of_int i /. 4.0));
      })

let bar_simple =
  [
    {
      label = "Q1";
      segments = [ { value = 30.0; style = Style.default; label = None } ];
    };
    {
      label = "Q2";
      segments = [ { value = 45.0; style = Style.default; label = None } ];
    };
    {
      label = "Q3";
      segments = [ { value = 25.0; style = Style.default; label = None } ];
    };
    {
      label = "Q4";
      segments = [ { value = 50.0; style = Style.default; label = None } ];
    };
  ]

let horizontal_bars =
  [
    {
      label = "A";
      segments = [ { value = 20.0; style = Style.default; label = None } ];
    };
    {
      label = "B";
      segments = [ { value = 30.0; style = Style.default; label = None } ];
    };
    {
      label = "C";
      segments = [ { value = 15.0; style = Style.default; label = None } ];
    };
  ]

let heat_points =
  List.flatten
    (List.init 5 (fun x ->
         List.init 5 (fun y ->
             {
               x = float_of_int x;
               y = float_of_int y;
               value = sin (float_of_int x /. 2.0) *. cos (float_of_int y /. 2.0);
             })))

let candles =
  [
    { time = 0.0; open_ = 100.0; high = 110.0; low = 95.0; close = 105.0 };
    { time = 1.0; open_ = 105.0; high = 115.0; low = 100.0; close = 98.0 };
    { time = 2.0; open_ = 98.0; high = 105.0; low = 92.0; close = 102.0 };
  ]

let%expect_test "line chart with axes" =
  let draw =
    let open Plot in
    (make ~axes:true ()
    |> line ~x:fst ~y:snd simple_points
    |> draw)
  in
  render_chart ~width:30 ~height:10 draw;
  [%expect_exact {|
┌──────────────────────────────┐
││                            ╱│
││                          ╱╱ │
││          ╲╲            ╱╱   │
││        ╱╱  ╲╲╲       ╱╱     │
││       ╱       ╲╲╲  ╱╱       │
││     ╱╱           ╲╱         │
││    ╱                        │
││  ╱╱                         │
││ ╱                           │
│└─────────────────────────────│
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "line simple arc (no axes)" =
  let draw =
    let data = [ (0., 0.); (9., 4.) ] in
    let plot =
      Plot.(make ~axes:false () |> line ~kind:`Wave ~x:fst ~y:snd data)
    in
    Plot.draw plot
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
  let draw =
    let plot =
      Plot.(
        make ~axes:false ()
        |> x_domain (0., 9.)
        |> y_domain (0., 4.)
        |> line ~kind:`Line ~x:fst ~y:snd [ (0., 2.); (9., 2.) ])
    in
    Plot.draw plot
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
  let draw =
    let open Plot in
    (make ~axes:true ()
    |> line ~kind:`Braille ~x:fst ~y:snd (sine_wave 20)
    |> draw)
  in
  render_chart ~width:25 ~height:8 draw;
  [%expect_exact {|
┌─────────────────────────┐
││    ⠠⠈⠉⠉⠢⡀              │
││  ⢀⠐⠁    ⠈⢀             │
││ ⢠⠊       ⠈⢀            │
││ ⠃          ⢣          ⡠│
││             ⢣        ⠰⠁│
││              ⠱⡀    ⢀⠜  │
││               ⠈⠠⢀⣀⡠⠊   │
│└────────────────────────│
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "line braille (no axes)" =
  let draw =
    let open Plot in
    make ~axes:false ()
    |> x_domain (0., 9.)
    |> y_domain (0., 4.)
    |> line ~kind:`Braille ~x:fst ~y:snd [ (0., 0.); (9., 4.) ]
    |> draw
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
  let draw =
    let open Plot in
    (make ~axes:true ()
    |> line_opt ~x:fst ~y:(fun (_, y) -> y) (List.assoc "Incomplete" gapped_series)
    |> draw)
  in
  render_chart ~width:30 ~height:8 draw;
  [%expect_exact {|
┌──────────────────────────────┐
││                            ╱│
││                          ╱╱ │
││      ╱                  ╱   │
││     ╱                 ╱╱    │
││   ╱╱                 ╱      │
││  ╱                          │
││ ╱                           │
│└─────────────────────────────│
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "time series uses time for x axis" =
  let draw = time_series [ ("Temperature", time_series_points) ] in
  render_chart ~width:30 ~height:8 draw;
  [%expect_exact {|
┌──────────────────────────────┐
││            ╱────────────╲╲  │
││          ╱╱               ╲╲│
││        ╱╱                   │
││      ╱╱                     │
││    ╱╱                       │
││  ╱╱                         │
││ ╱                           │
│└─────────────────────────────│
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "scatter X glyph" =
  let draw =
    let pts = [ (1., 1.); (3., 3.); (8., 2.) ] in
    let open Plot in
    make ~axes:false ()
    |> x_domain (0., 9.)
    |> y_domain (0., 4.)
    |> scatter ~glyph:"X" ~x:fst ~y:snd pts
    |> draw
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
  let draw =
    let pts = [ (1., 1.); (3., 3.); (8., 2.) ] in
    let open Plot in
    make ~axes:false ()
    |> x_domain (0., 9.)
    |> y_domain (0., 4.)
    |> scatter ~kind:`Braille ~x:fst ~y:snd pts
    |> draw
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
  let draw =
    let open Plot in
    make ~axes:false ()
    |> x_domain (0., 11.)
    |> y_domain (0., 7.)
    |> circle ~kind:`Line ~cx:fst ~cy:snd ~r:(fun _ -> 3.) [ (6., 4.) ]
    |> draw
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
  let draw =
    let open Plot in
    make ~axes:false ()
    |> x_domain (0., 11.)
    |> y_domain (0., 7.)
    |> circle ~kind:`Braille ~cx:fst ~cy:snd ~r:(fun _ -> 3.) [ (6., 4.) ]
    |> draw
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
  let draw = bar ~orientation:`Vertical bar_simple in
  render_chart ~width:25 ~height:10 draw;
  [%expect_exact {|
┌─────────────────────────┐
│      ▂▂▂▂▂       █████  │
│      █████       █████  │
│      █████       █████  │
│▆▆▆▆▆ █████       █████  │
│█████ █████ █████ █████  │
│█████ █████ █████ █████  │
│█████ █████ █████ █████  │
│█████ █████ █████ █████  │
│─────────────────────────│
│Q1    Q2    Q3    Q4     │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "horizontal bar chart" =
  let draw = bar ~orientation:`Horizontal ~bar_width:1 horizontal_bars in
  render_chart ~width:30 ~height:8 draw;
  [%expect_exact {|
┌──────────────────────────────┐
│A│██████████████████▋         │
│ │                            │
│B│████████████████████████████│
│ │                            │
│C│██████████████              │
│ │                            │
│ │                            │
│ │                            │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "vertical bars (no axis)" =
  let draw =
    let bars : bar list =
      [
        {
          label = "A";
          segments = [ { value = 2.; style = Style.default; label = None } ];
        };
        {
          label = "B";
          segments = [ { value = 5.; style = Style.default; label = None } ];
        };
        {
          label = "C";
          segments = [ { value = 3.; style = Style.default; label = None } ];
        };
        {
          label = "D";
          segments = [ { value = 6.; style = Style.default; label = None } ];
        };
      ]
    in
    bar ~orientation:`Vertical ~show_axis:false bars
  in
  render_chart ~width:11 ~height:8 draw;
  [%expect_exact
    {|
┌───────────┐
│         ██│
│   ▅▅    ██│
│   ██    ██│
│   ██    ██│
│   ██ ██ ██│
│▅▅ ██ ██ ██│
│██ ██ ██ ██│
│██ ██ ██ ██│
└───────────┘
|}]

let%expect_test "horizontal bars (no axis)" =
  let draw =
    let bars : bar list =
      [
        {
          label = "Alpha";
          segments = [ { value = 4.; style = Style.default; label = None } ];
        };
        {
          label = "Beta";
          segments = [ { value = 8.; style = Style.default; label = None } ];
        };
        {
          label = "Gamma";
          segments = [ { value = 3.; style = Style.default; label = None } ];
        };
      ]
    in
    bar ~orientation:`Horizontal ~show_axis:false bars
  in
  render_chart ~width:16 ~height:6 draw;
  [%expect_exact
    {|
┌────────────────┐
│████████        │
│                │
│████████████████│
│                │
│██████          │
│                │
└────────────────┘
|}]

let%expect_test "stacked bars vertical (no axis)" =
  let draw =
    let st = Style.default in
    let bars : bar list =
      [
        {
          label = "A";
          segments =
            [
              { value = 2.; style = st; label = None };
              { value = 2.; style = st; label = None };
            ];
        };
        {
          label = "B";
          segments =
            [
              { value = 3.; style = st; label = None };
              { value = 1.; style = st; label = None };
            ];
        };
        {
          label = "C";
          segments =
            [
              { value = 1.; style = st; label = None };
              { value = 4.; style = st; label = None };
            ];
        };
      ]
    in
    bar ~orientation:`Vertical ~show_axis:false bars
  in
  render_chart ~width:15 ~height:8 draw;
  [%expect_exact
    {|
┌───────────────┐
│          ████ │
│▃▃▃▃ ▃▃▃▃ ████ │
│████ ████ ████ │
│████ ▆▆▆▆ ████ │
│▂▂▂▂ ████ ████ │
│████ ████ ████ │
│████ ████ ▅▅▅▅ │
│████ ████ ████ │
└───────────────┘
|}]

let%expect_test "stacked bars horizontal (no axis)" =
  let draw =
    let st = Style.default in
    let bars : bar list =
      [
        {
          label = "A";
          segments =
            [
              { value = 2.; style = st; label = None };
              { value = 2.; style = st; label = None };
            ];
        };
        {
          label = "B";
          segments =
            [
              { value = 3.; style = st; label = None };
              { value = 1.; style = st; label = None };
            ];
        };
        {
          label = "C";
          segments =
            [
              { value = 1.; style = st; label = None };
              { value = 4.; style = st; label = None };
            ];
        };
      ]
    in
    bar ~orientation:`Horizontal ~show_axis:false bars
  in
  render_chart ~width:20 ~height:6 draw;
  [%expect_exact
    {|
┌────────────────────┐
│████████████████    │
│                    │
│████████████████    │
│                    │
│████████████████████│
│                    │
└────────────────────┘
|}]

let%expect_test "sparkline bars" =
  let draw = sparkline ~kind:`Bars [ 1.0; 3.0; 2.0; 5.0; 4.0; 3.0; 2.0; 4.0 ] in
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
  let draw = sparkline ~kind:`Bars [ 1.; 3.; 2.; 5.; 4.; 3.; 2.; 4. ] in
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
  let draw =
    Sparkline.(
      let m = make ~width:10 ~height:3 () in
      push_all m [ 0.; 2.; 4.; 3.; 5.; 4.; 2.; 1. ];
      fun canvas ~width ~height -> draw m ~kind:`Braille canvas ~width ~height)
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

let%expect_test "heatmap fills cells" =
  let draw =
    heatmap ~shaded:true ~color_scale:
      [ Ansi.Color.Extended 232;
        Ansi.Color.Extended 236;
        Ansi.Color.Extended 240;
        Ansi.Color.Extended 244;
        Ansi.Color.Extended 248;
        Ansi.Color.Extended 252 ]
      heat_points
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
  let draw = candlestick candles in
  render_chart ~width:20 ~height:10 draw;
  [%expect_exact {|
┌────────────────────┐
││         │         │
││ │       │         │
││ │       │         │
││ ┃       ┃        ││
││ ┃       ┃        ┃│
││ ┃       ┃        ┃│
││ │                ││
││                  ││
││                  ││
│└───────────────────│
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "legend arranges entries horizontally" =
  let draw =
    legend
      [ ("Revenue", Style.make ~fg:Ansi.Color.green ());
        ("Costs", Style.make ~fg:Ansi.Color.red ());
        ("Profit", Style.make ~fg:Ansi.Color.blue ()) ]
  in
  render_chart ~width:40 ~height:3 draw;
  [%expect_exact {|
┌────────────────────────────────────────┐
│●●  Revenue  ●●  Costs  ●●  Profit      │
│                                        │
│                                        │
└────────────────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "stacked legend renders vertical list" =
  let draw =
    stacked_legend
      [ ("Alpha", Style.make ~fg:Ansi.Color.yellow ());
        ("Beta", Style.make ~fg:Ansi.Color.cyan ());
        ("Gamma", Style.make ~fg:Ansi.Color.magenta ()) ]
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
