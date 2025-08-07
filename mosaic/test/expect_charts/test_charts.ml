open Test_utils
open Mosaic_charts

(* Helper function to generate test data *)
let simple_line_data = 
  [ {x=0.0; y=0.0}; {x=1.0; y=2.0}; {x=2.0; y=1.0}; {x=3.0; y=3.0} ]

let sine_wave_data points =
  List.init points (fun i ->
    let x = float_of_int i /. float_of_int (points - 1) *. 2.0 *. Float.pi in
    { x; y = sin x })

let random_data points seed =
  Random.init seed;
  List.init points (fun i ->
    { x = float_of_int i; y = Random.float 10.0 })

let time_series_data points =
  let base_time = 1700000000.0 in (* Fixed timestamp for reproducible tests *)
  List.init points (fun i ->
    { time = base_time +. (float_of_int i *. 3600.0);
      value = 50.0 +. (20.0 *. sin (float_of_int i /. 4.0)) })

let bar_data_simple =
  [
    { label = "Q1"; segments = [{value=30.0; style=Ui.Style.(bg Red); label=None}] };
    { label = "Q2"; segments = [{value=45.0; style=Ui.Style.(bg Blue); label=None}] };
    { label = "Q3"; segments = [{value=25.0; style=Ui.Style.(bg Green); label=None}] };
    { label = "Q4"; segments = [{value=50.0; style=Ui.Style.(bg Yellow); label=None}] };
  ]

let bar_data_stacked =
  [
    { label = "Jan"; 
      segments = [
        {value=20.0; style=Ui.Style.(bg Red); label=Some "A"};
        {value=30.0; style=Ui.Style.(bg Blue); label=Some "B"}
      ] };
    { label = "Feb"; 
      segments = [
        {value=25.0; style=Ui.Style.(bg Red); label=Some "A"};
        {value=35.0; style=Ui.Style.(bg Blue); label=Some "B"}
      ] };
  ]

let ohlc_data =
  [
    { time = 0.0; open_ = 100.0; high = 110.0; low = 95.0; close = 105.0 };
    { time = 1.0; open_ = 105.0; high = 115.0; low = 100.0; close = 98.0 };
    { time = 2.0; open_ = 98.0; high = 105.0; low = 92.0; close = 102.0 };
  ]

let heat_data =
  List.flatten
    (List.init 5 (fun x ->
      List.init 5 (fun y ->
        { x = float_of_int x; 
          y = float_of_int y; 
          value = sin (float_of_int x /. 2.0) *. cos (float_of_int y /. 2.0) })))

let%expect_test "line chart - simple" =
  print_ui ~width:30 ~height:10
    (line ~width:(`Cells 30) ~height:(`Cells 10) ~show_axes:true
       [ ("Data", simple_line_data) ]);
  [%expect_exact {|
┌──────────────────────────────┐
││                        ╱╲   │
││                       ╱  ╲  │
││                      ╱    ╲ │
││                     ╱      ╲│
││           ╱╲       ╱         │
││          ╱  ╲     ╱          │
││         ╱    ╲   ╱           │
││        ╱      ╲ ╱            │
││       ╱        ╲             │
│└─────────────────────────────│
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "line chart - multiple series" =
  print_ui ~width:30 ~height:8
    (line ~width:(`Cells 30) ~height:(`Cells 8) ~show_axes:true
       ~series_styles:[Ui.Style.(fg Red); Ui.Style.(fg Blue)]
       [ ("Series 1", [{x=0.0; y=1.0}; {x=1.0; y=2.0}; {x=2.0; y=1.5}]);
         ("Series 2", [{x=0.0; y=2.0}; {x=1.0; y=1.0}; {x=2.0; y=2.5}]) ]);
  [%expect_exact {|
┌──────────────────────────────┐
││             ╱╲              │
││     ╲      ╱  ╲             │
││      ╲    ╱          ╱╲     │
││       ╲  ╱          ╱  ╲    │
││        ╲╱      ╱╲  ╱        │
││         ╲     ╱  ╲╱         │
││                              │
│└─────────────────────────────│
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "line chart - no axes" =
  print_ui ~width:20 ~height:6
    (line ~width:(`Cells 20) ~height:(`Cells 6) ~show_axes:false
       [ ("Data", simple_line_data) ]);
  [%expect_exact {|
┌────────────────────┐
│                 ╱╲ │
│                ╱  ╲│
│      ╱╲       ╱    │
│     ╱  ╲     ╱     │
│    ╱    ╲   ╱      │
│   ╱      ╲ ╱       │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "line chart - with Braille rendering" =
  print_ui ~width:25 ~height:8
    (line ~width:(`Cells 25) ~height:(`Cells 8) ~show_axes:true
       ~render_kind:Braille
       [ ("Sine", sine_wave_data 20) ]);
  [%expect_exact {|
┌─────────────────────────┐
││   ⣠⠤⠒⠒⠉⠉⠉⠉⠒⠒⠤⣄        │
││ ⢀⠔⠁         ⠈⠢⡀      │
││⢠⠊             ⠈⢆     │
││⠇               ⠸     │
││                  ⠱⡀   │
││                   ⠈⢆  │
││                     ⠑⢄│
│└────────────────────────│
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "line chart - scatter plot with Points" =
  print_ui ~width:25 ~height:8
    (line ~width:(`Cells 25) ~height:(`Cells 8) ~show_axes:true
       ~render_kind:(Points "●")
       [ ("Points", [{x=0.0; y=0.0}; {x=1.0; y=2.0}; {x=2.0; y=1.0}; {x=3.0; y=3.0}]) ]);
  [%expect_exact {|
┌─────────────────────────┐
││                    ●   │
││                        │
││       ●                │
││                        │
││              ●         │
││                        │
││  ●                     │
│└────────────────────────│
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "time series chart" =
  print_ui ~width:30 ~height:8
    (time_series ~width:(`Cells 30) ~height:(`Cells 8) ~show_axes:true
       [ ("Temperature", time_series_data 10) ]);
  [%expect_exact {|
┌──────────────────────────────┐
││     ╱╲                      │
││  ╱╲╱  ╲    ╱╲               │
││ ╱      ╲  ╱  ╲    ╱╲        │
││╱        ╲╱    ╲  ╱  ╲    ╱╲ │
││                ╲╱    ╲  ╱  ╲│
││                       ╲╱     │
││                              │
│└─────────────────────────────│
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "bar chart - vertical" =
  print_ui ~width:25 ~height:10
    (bar ~width:(`Cells 25) ~height:(`Cells 10) ~orientation:`Vertical
       bar_data_simple);
  [%expect_exact {|
┌─────────────────────────┐
│                         │
│                         │
│     ████                │
│     ████      ████      │
│████ ████      ████      │
│████ ████ ████ ████      │
│████ ████ ████ ████      │
│████ ████ ████ ████      │
│████ ████ ████ ████      │
│─────────────────────────│
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "bar chart - horizontal" =
  print_ui ~width:30 ~height:8
    (bar ~width:(`Cells 30) ~height:(`Cells 8) ~orientation:`Horizontal
       ~bar_width:1
       [ { label = "A"; segments = [{value=20.0; style=Ui.Style.(bg Blue); label=None}] };
         { label = "B"; segments = [{value=30.0; style=Ui.Style.(bg Red); label=None}] };
         { label = "C"; segments = [{value=15.0; style=Ui.Style.(bg Green); label=None}] }]);
  [%expect_exact {|
┌──────────────────────────────┐
││█████████████████            │
││                             │
││████████████████████████████ │
││                             │
││████████████                 │
││                             │
││                             │
││                             │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "bar chart - stacked" =
  print_ui ~width:20 ~height:10
    (bar ~width:(`Cells 20) ~height:(`Cells 10) ~orientation:`Vertical
       bar_data_stacked);
  [%expect_exact {|
┌────────────────────┐
│                    │
│      ████          │
│████  ████          │
│████  ████          │
│████  ████          │
│████  ████          │
│████  ████          │
│████  ████          │
│████  ████          │
│────────────────────│
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "sparkline - bars" =
  print_ui ~width:20 ~height:3
    (Ui.vbox [
      Ui.text "CPU Usage:";
      sparkline ~width:15 ~style:Ui.Style.(fg Green) ~render_kind:`Bars
        [1.0; 3.0; 2.0; 5.0; 4.0; 3.0; 2.0; 4.0]
    ]);
  [%expect_exact {|
┌────────────────────┐
│CPU Usage:          │
│▏▌▎█▋▌▎▋            │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "sparkline - line" =
  print_ui ~width:20 ~height:3
    (Ui.vbox [
      Ui.text "Memory:";
      sparkline ~width:10 ~style:Ui.Style.(fg Blue) ~render_kind:`Line
        [0.0; 2.0; 4.0; 3.0; 5.0; 4.0; 2.0; 1.0]
    ]);
  [%expect_exact {|
┌────────────────────┐
│Memory:             │
│__¯-¯¯-_            │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "sparkline - braille" =
  print_ui ~width:20 ~height:3
    (Ui.vbox [
      Ui.text "Network:";
      sparkline ~width:12 ~style:Ui.Style.(fg Red) ~render_kind:`Braille
        [1.0; 2.0; 1.5; 3.0; 2.5; 4.0; 3.5; 2.0; 1.0]
    ]);
  [%expect_exact {|
┌────────────────────┐
│Network:            │
│⠁⠃⠃⠇⠇⠟⠏⠃⠁           │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "heatmap - simple" =
  print_ui ~width:25 ~height:10
    (heatmap ~width:(`Cells 25) ~height:(`Cells 10) 
       ~color_scale:[
         Ui.Style.Index 232;  (* Black *)
         Ui.Style.Index 236;  (* Dark grey *)
         Ui.Style.Index 240;  (* Medium grey *)
         Ui.Style.Index 244;  (* Light grey *)
         Ui.Style.Index 248;  (* Very light grey *)
         Ui.Style.Index 252;  (* Almost white *)
       ]
       heat_data);
  [%expect_exact {|
┌─────────────────────────┐
││████████████████████████│
││████████████████████████│
││████████████████████████│
││████████████████████████│
││████████████████████████│
││████████████████████████│
││████████████████████████│
││████████████████████████│
││████████████████████████│
│└────────────────────────│
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "candlestick chart" =
  print_ui ~width:20 ~height:10
    (candlestick ~width:(`Cells 20) ~height:(`Cells 10) 
       ~bullish_style:Ui.Style.(fg Green ++ bold)
       ~bearish_style:Ui.Style.(fg Red ++ bold)
       ohlc_data);
  [%expect_exact {|
┌────────────────────┐
││      │            │
││    ┌─┴──┐         │
││    │████│    │    │
││    │████│  ┌─┴──┐ │
││    │████│  │████│ │
││    └─┬──┘  │████│ │
││      │     │████│ │
││            │████│ │
││            └─┬──┘ │
│└───────────────────│
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "Canvas element - basic drawing" =
  print_ui ~width:20 ~height:8
    (Ui.Canvas.create ~width:(`Cells 20) ~height:(`Cells 8) 
       (fun ~width ~height canvas ->
         (* Draw a border *)
         for x = 0 to width - 1 do
           Ui.Canvas.plot canvas ~x ~y:0 ~style:Ui.Style.(fg Blue) "─";
           Ui.Canvas.plot canvas ~x ~y:(height - 1) ~style:Ui.Style.(fg Blue) "─"
         done;
         for y = 0 to height - 1 do
           Ui.Canvas.plot canvas ~x:0 ~y ~style:Ui.Style.(fg Blue) "│";
           Ui.Canvas.plot canvas ~x:(width - 1) ~y ~style:Ui.Style.(fg Blue) "│"
         done;
         (* Corners *)
         Ui.Canvas.plot canvas ~x:0 ~y:0 ~style:Ui.Style.(fg Blue) "┌";
         Ui.Canvas.plot canvas ~x:(width - 1) ~y:0 ~style:Ui.Style.(fg Blue) "┐";
         Ui.Canvas.plot canvas ~x:0 ~y:(height - 1) ~style:Ui.Style.(fg Blue) "└";
         Ui.Canvas.plot canvas ~x:(width - 1) ~y:(height - 1) ~style:Ui.Style.(fg Blue) "┘";
         (* Content *)
         Ui.Canvas.plot canvas ~x:5 ~y:3 ~style:Ui.Style.(fg Red) "●";
         Ui.Canvas.plot canvas ~x:10 ~y:3 ~style:Ui.Style.(fg Green) "●";
         Ui.Canvas.plot canvas ~x:15 ~y:3 ~style:Ui.Style.(fg Yellow) "●"));
  [%expect_exact {|
┌────────────────────┐
│┌──────────────────┐│
││                  ││
││                  ││
││    ●    ●    ●   ││
││                  ││
││                  ││
││                  ││
│└──────────────────┘│
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "Canvas element - draw lines" =
  print_ui ~width:15 ~height:7
    (Ui.Canvas.create ~width:(`Cells 15) ~height:(`Cells 7)
       (fun ~width ~height canvas ->
         (* Draw diagonal lines *)
         Ui.Canvas.draw_line ~x1:0 ~y1:0 ~x2:(width - 1) ~y2:(height - 1)
           ~style:Ui.Style.(fg Red) canvas;
         Ui.Canvas.draw_line ~x1:0 ~y1:(height - 1) ~x2:(width - 1) ~y2:0
           ~style:Ui.Style.(fg Blue) canvas;
         (* Draw horizontal and vertical lines *)
         Ui.Canvas.draw_line ~x1:0 ~y1:(height / 2) ~x2:(width - 1) ~y2:(height / 2)
           ~style:Ui.Style.(fg Green) canvas;
         Ui.Canvas.draw_line ~x1:(width / 2) ~y1:0 ~x2:(width / 2) ~y2:(height - 1)
           ~style:Ui.Style.(fg Yellow) canvas));
  [%expect_exact {|
┌───────────────┐
│╲      │      ╱│
││╲     │     ╱ │
││ ╲    │    ╱  │
│───────────────│
││   ╱  │  ╲    │
││  ╱   │   ╲   │
│╱      │      ╲│
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "Canvas element - draw boxes" =
  print_ui ~width:20 ~height:8
    (Ui.Canvas.create ~width:(`Cells 20) ~height:(`Cells 8)
       (fun ~width:_ ~height:_ canvas ->
         (* Draw filled boxes *)
         Ui.Canvas.draw_box ~x:1 ~y:1 ~width:5 ~height:3
           ~style:Ui.Style.(bg Red) canvas;
         Ui.Canvas.draw_box ~x:8 ~y:2 ~width:4 ~height:4
           ~style:Ui.Style.(bg Blue) canvas;
         Ui.Canvas.draw_box ~x:14 ~y:1 ~width:5 ~height:2
           ~style:Ui.Style.(bg Green) canvas;
         (* Draw bordered box *)
         Ui.Canvas.draw_box ~x:2 ~y:5 ~width:8 ~height:2
           ~border:(Ui.Border.normal) canvas));
  [%expect_exact {|
┌────────────────────┐
│                    │
│                    │
│                    │
│                    │
│                    │
│                    │
│  ┌──────┐          │
│  └──────┘          │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "chart composition - dashboard layout" =
  print_ui ~width:50 ~height:20
    (Ui.vbox ~gap:(`Cells 1) [
      Ui.text ~style:Ui.Style.(fg Cyan ++ bold) "Dashboard";
      Ui.divider ();
      Ui.hbox ~gap:(`Cells 2) [
        Ui.vbox [
          Ui.text "Revenue:";
          line ~width:(`Cells 20) ~height:(`Cells 6) ~show_axes:false
            [ ("Q1-Q4", [{x=0.0; y=100.0}; {x=1.0; y=120.0}; {x=2.0; y=110.0}; {x=3.0; y=140.0}]) ]
        ];
        Ui.vbox [
          Ui.text "Status:";
          sparkline ~width:20 ~style:Ui.Style.(fg Green)
            [80.0; 85.0; 82.0; 90.0; 88.0; 92.0; 95.0]
        ];
      ];
      Ui.text "Sales by Region:";
      bar ~width:(`Cells 40) ~height:(`Cells 6) ~orientation:`Horizontal ~bar_width:1
        [ { label = "North"; segments = [{value=45.0; style=Ui.Style.(bg Blue); label=None}] };
          { label = "South"; segments = [{value=30.0; style=Ui.Style.(bg Red); label=None}] };
          { label = "East"; segments = [{value=35.0; style=Ui.Style.(bg Green); label=None}] };
          { label = "West"; segments = [{value=40.0; style=Ui.Style.(bg Yellow); label=None}] }]
    ]);
  [%expect_exact {|
┌──────────────────────────────────────────────────┐
│Dashboard                                         │
│                                                  │
│──────────────────────────────────────────────────│
│                                                  │
│Revenue:              Status:                     │
│                    ╱ ▋▊▊█▉██                     │
│         ╱╲        ╱                              │
│        ╱  ╲      ╱                               │
│    ╱╲ ╱    ╲    ╱                                │
│   ╱  ╲      ╲  ╱                                 │
│  ╱           ╲╱                                  │
│                                                  │
│Sales by Region:                                  │
││█████████████████████████████                    │
││████████████████████                             │
││████████████████████████                         │
││███████████████████████████                      │
││                                                 │
││                                                 │
└──────────────────────────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "chart with custom styling" =
  print_ui ~width:30 ~height:10
    (Ui.vbox [
      Ui.panel ~box_style:Ui.Border.Rounded ~title:"Performance"
        ~expand:false
        (line ~width:(`Cells 26) ~height:(`Cells 6) ~show_axes:true
           ~axis_style:Ui.Style.(fg (Index 240))
           ~series_styles:[
             Ui.Style.(fg Green ++ bold);
             Ui.Style.(fg Red ++ italic);
           ]
           [ ("Success", [{x=0.0; y=2.0}; {x=1.0; y=4.0}; {x=2.0; y=3.0}]);
             ("Errors", [{x=0.0; y=1.0}; {x=1.0; y=0.5}; {x=2.0; y=1.5}]) ])
    ]);
  [%expect_exact {|
┌──────────────────────────────┐
│╭────── Performance ─────────╮│
│││          ╱╲               ││
││                            ││
││ ──────── Performance ───────│
││          ╱╲                ││
│││      ╱╲╱  ╲               ││
│││     ╱                ╱    ││
│││╲   ╱        ╲       ╱     ││
│││ ╲ ╱          ╲     ╱      ││
││└─────────────────────────  ││
│╰────────────────────────────╯│
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "empty charts" =
  print_ui ~width:20 ~height:5
    (line ~width:(`Cells 20) ~height:(`Cells 5) [ ("Empty", []) ]);
  [%expect_exact {|
┌────────────────────┐
││                   │
││                   │
││                   │
││                   │
│└───────────────────│
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "single point chart" =
  print_ui ~width:15 ~height:5
    (line ~width:(`Cells 15) ~height:(`Cells 5) ~render_kind:(Points "*")
       [ ("Single", [{x=0.5; y=0.5}]) ]);
  [%expect_exact {|
┌───────────────┐
││              │
││              │
││       *      │
││              │
│└──────────────│
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "chart measure function" =
  let chart = line ~width:(`Cells 30) ~height:(`Cells 10) 
    [ ("Data", simple_line_data) ] in
  let w, h = Ui.measure chart in
  Printf.printf "Chart dimensions: %dx%d\n" w h;
  [%expect {| Chart dimensions: 30x10 |}] [@@ocamlformat "disable"]

let%expect_test "nested charts in layout" =
  print_ui ~width:40 ~height:15
    (Ui.grid ~col_gap:(`Cells 1) ~row_gap:(`Cells 1)
       ~template_columns:[
         Ui.Single (Ui.track_cells 19);
         Ui.Single (Ui.track_cells 19)
       ]
       ~template_rows:[
         Ui.Single (Ui.track_cells 7);
         Ui.Single (Ui.track_cells 7)
       ]
       [
         line ~width:(`Cells 19) ~height:(`Cells 7) ~show_axes:false
           [ ("TL", [{x=0.0; y=0.0}; {x=1.0; y=1.0}]) ];
         bar ~width:(`Cells 19) ~height:(`Cells 7) ~orientation:`Vertical
           [ { label = "TR"; segments = [{value=5.0; style=Ui.Style.(bg Blue); label=None}] }];
         sparkline ~width:19 ~style:Ui.Style.(fg Green)
           [1.0; 2.0; 3.0; 2.0; 4.0; 3.0; 5.0];
         heatmap ~width:(`Cells 19) ~height:(`Cells 7)
           ~color_scale:[Ui.Style.Index 232; Ui.Style.Index 255]
           [{x=0.0; y=0.0; value=0.0}; {x=1.0; y=1.0; value=1.0}]
       ]);
  [%expect_exact {|
┌────────────────────────────────────────┐
│                                        │
│            ╱╲╱╲╱╲╱                     │
│          ╱╱╱╱╱╱╱╱╱      ████████████   │
│        ╱╱╱╱╱╱╱╱╱╱╱      ████████████   │
│      ╱╱╱╱╱╱╱╱╱╱╱╱╱      ████████████   │
│    ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱      ████████████   │
│  ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱      ████████████   │
│╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱      ─────────────  │
│                                        │
│▏▎▌▎▊▌█              │█████████████████││
│                    ││█████████████████││
│                    ││█████████████████││
│                    ││█████████████████││
│                    ││█████████████████││
│                    │└──────────────────│
└────────────────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "chart with extreme values" =
  print_ui ~width:20 ~height:6
    (line ~width:(`Cells 20) ~height:(`Cells 6) ~show_axes:true
       [ ("Extreme", [{x=0.0; y=1000000.0}; {x=1.0; y= -1000000.0}]) ]);
  [%expect_exact {|
┌────────────────────┐
││╲                  │
││ ╲                 │
││  ╲                │
││   ╲               │
││    ╲              │
│└───────────────────│
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "chart with custom ranges" =
  print_ui ~width:20 ~height:6
    (line ~width:(`Cells 20) ~height:(`Cells 6) ~show_axes:true
       ~x_range:(0.0, 10.0) ~y_range:(0.0, 100.0)
       [ ("Scaled", [{x=1.0; y=20.0}; {x=2.0; y=40.0}; {x=3.0; y=30.0}]) ]);
  [%expect_exact {|
┌────────────────────┐
││                   │
││  ╱╲               │
││ ╱  ╲              │
││╱                  │
││                   │
│└───────────────────│
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "combined chart types in panel" =
  print_ui ~width:35 ~height:12
    (Ui.panel ~box_style:Ui.Border.Double ~title:"Analytics" ~expand:false
       (Ui.vbox ~gap:(`Cells 1) [
         Ui.hbox ~gap:(`Cells 2) [
           sparkline ~width:10 ~style:Ui.Style.(fg Red) [5.0; 3.0; 7.0; 2.0; 8.0];
           sparkline ~width:10 ~style:Ui.Style.(fg Blue) [2.0; 4.0; 3.0; 6.0; 5.0];
         ];
         Ui.divider ~char:"·" ();
         bar ~width:(`Cells 30) ~height:(`Cells 5) ~orientation:`Vertical
           [ { label = "Mon"; segments = [{value=3.0; style=Ui.Style.(bg Green); label=None}] };
             { label = "Tue"; segments = [{value=5.0; style=Ui.Style.(bg Green); label=None}] };
             { label = "Wed"; segments = [{value=4.0; style=Ui.Style.(bg Green); label=None}] }]
       ]));
  [%expect_exact {|
┌───────────────────────────────────┐
│╔════════ Analytics ═════════════╗│
│║▌▎▊▏█      ▎▊▎█▌                ║│
│║                                ║│
│║·································║│
│║                                ║│
│║      ████                      ║│
│║████  ████  ████                ║│
│║████  ████  ████                ║│
│║████  ████  ████                ║│
│║────────────────────────────────║│
│╚════════════════════════════════╝│
└───────────────────────────────────┘
|}] [@@ocamlformat "disable"]