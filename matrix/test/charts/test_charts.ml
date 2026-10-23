module Grid = Matrix_grid
open Matrix_charts
module Style = Ansi.Style

let render_boxed ~width ~height draw =
  let outer_width = width + 2 in
  let outer_height = height + 2 in
  let grid = Grid.create ~width:outer_width ~height:outer_height () in
  (* Draw outer border *)
  Grid.draw_box grid ~x:0 ~y:0 ~width:outer_width ~height:outer_height ();
  (* Draw inner content with scissor clipping *)
  let rect : Grid.region = { x = 1; y = 1; width; height } in
  Grid.clip grid rect (fun () ->
      let inner = Grid.create ~width ~height () in
      draw inner ~width ~height;
      Grid.blit_region ~src:inner ~dst:grid ~src_x:0 ~src_y:0 ~width ~height
        ~dst_x:1 ~dst_y:1);
  print_newline ();
  print_string (Grid.to_ansi ~reset:false grid)

let render_chart ~width ~height draw =
  render_boxed ~width ~height (fun grid ~width ~height ->
      ignore (draw grid ~width ~height))

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
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.default ~y:Axis.default
      |> line ~x:fst ~y:snd simple_points
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:30 ~height:10 draw;
  [%expect_exact {|
[0m┌──────────────────────────────┐[0m
[0m│  [38;5;246m3[39m [38;5;245m─┐[39m                      [38;5;6m─╱[39m│[0m
[0m│[38;5;246m2.5[39m [38;5;245m─┤[39m                    [38;5;6m─╱[39m  │[0m
[0m│  [38;5;246m2[39m [38;5;245m─┤[39m       [38;5;6m────╲[39m     [38;5;6m──╱[39m    │[0m
[0m│[38;5;246m1.5[39m [38;5;245m─┤[39m     [38;5;6m─╱[39m     [38;5;6m────╱[39m       │[0m
[0m│[38;5;246m0.5[39m [38;5;245m─┤[39m  [38;5;6m──╱[39m                   │[0m
[0m│  [38;5;246m0[39m [38;5;245m─┤[38;5;6m─╱[39m                      │[0m
[0m│     [38;5;245m└┬───┬───┬───┬──┬───┬───┐[39m│[0m
[0m│      [38;5;245m│[39m   [38;5;245m│[39m   [38;5;245m│[39m   [38;5;245m│[39m  [38;5;245m│[39m   [38;5;245m│[39m   [38;5;245m│[39m│[0m
[0m│      [38;5;246m0[39m  [38;5;246m0.5[39m  [38;5;246m1[39m  [38;5;246m1.5[39m [38;5;246m2[39m  [38;5;246m2.5[39m  [38;5;246m3[39m│[0m
[0m│                              │[0m
[0m└──────────────────────────────┘[0m|}] [@@ocamlformat "disable"]

let%expect_test "line simple arc (no axes)" =
  let draw grid ~width ~height =
    let data = [| (0., 0.); (9., 4.) |] in
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> line ~resolution:`Wave ~x:fst ~y:snd data
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:10 ~height:5 draw;
  [%expect_exact
    {|
[0m┌──────────┐[0m
[0m│        [38;5;6m╭─[39m│[0m
[0m│      [38;5;6m╭─╯[39m │[0m
[0m│    [38;5;6m╭─╯[39m   │[0m
[0m│  [38;5;6m╭─╯[39m     │[0m
[0m│[38;5;6m──╯[39m       │[0m
[0m└──────────┘[0m|}]

let%expect_test "line simple horizontal (no axes)" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 9.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 4.)) ())
      |> line ~x:fst ~y:snd [| (0., 2.); (9., 2.) |]
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:10 ~height:5 draw;
  [%expect_exact
    {|
[0m┌──────────┐[0m
[0m│          │[0m
[0m│          │[0m
[0m│[38;5;6m──────────[39m│[0m
[0m│          │[0m
[0m│          │[0m
[0m└──────────┘[0m|}]

let%expect_test "line chart with braille rendering" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.default ~y:Axis.default
      |> line ~resolution:`Braille2x4 ~x:fst ~y:snd (sine_wave 20)
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:25 ~height:8 draw;
  [%expect_exact {|
[0m┌─────────────────────────┐[0m
[0m│   [38;5;246m1[39m [38;5;245m─┐[39m  [38;5;6m⡠⠊⠉⠑⠢⡀[39m          │[0m
[0m│ [38;5;246m0.5[39m [38;5;245m─┤[38;5;6m⡠⠊[39m     [38;5;6m⠈⢢[39m         │[0m
[0m│[38;5;246m-0.0[39m [38;5;245m─┤[39m         [38;5;6m⠣⡀[39m     [38;5;6m⡠⠊[39m│[0m
[0m│  [38;5;246m-1[39m [38;5;245m─┤[39m          [38;5;6m⠈⠒⠤⣀⡠⠊[39m  │[0m
[0m│      [38;5;245m└┬──┬─┬──┬──┬──┬─┬─[39m│[0m
[0m│       [38;5;245m│[39m  [38;5;245m│[39m [38;5;245m│[39m  [38;5;245m│[39m  [38;5;245m│[39m  [38;5;245m│[39m [38;5;245m│[39m │[0m
[0m│       [38;5;246m0[39m  [38;5;246m1[39m [38;5;246m2[39m  [38;5;246m3[39m  [38;5;246m4[39m  [38;5;246m5[39m [38;5;246m6[39m │[0m
[0m│                         │[0m
[0m└─────────────────────────┘[0m|}] [@@ocamlformat "disable"]

let%expect_test "line braille (no axes)" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 9.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 4.)) ())
      |> line ~resolution:`Braille2x4 ~x:fst ~y:snd [| (0., 0.); (9., 4.) |]
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:10 ~height:5 draw;
  [%expect_exact
    {|
[0m┌──────────┐[0m
[0m│        [38;5;6m⡠⠊[39m│[0m
[0m│      [38;5;6m⡠⠊[39m  │[0m
[0m│    [38;5;6m⡠⠊[39m    │[0m
[0m│  [38;5;6m⡠⠊[39m      │[0m
[0m│[38;5;6m⡠⠊[39m        │[0m
[0m└──────────┘[0m|}]

let%expect_test "line with gaps skips missing points" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.default ~y:Axis.default
      |> line_gaps ~x:fst ~y:(fun (_, y) -> y) gapped_series
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:30 ~height:8 draw;
  [%expect_exact {|
[0m┌──────────────────────────────┐[0m
[0m│[38;5;246m2.6[39m [38;5;245m─┐[39m                      [38;5;6m─╱[39m│[0m
[0m│[38;5;246m2.2[39m [38;5;245m─┤[39m    [38;5;6m─╱[39m              [38;5;6m─╱[39m  │[0m
[0m│[38;5;246m1.6[39m [38;5;245m─┤[39m  [38;5;6m─╱[39m              [38;5;6m─╱[39m    │[0m
[0m│[38;5;246m1.2[39m [38;5;245m─┤[38;5;6m─╱[39m                      │[0m
[0m│     [38;5;245m└┬────┬───┬────┬───┬────┐[39m│[0m
[0m│      [38;5;245m│[39m    [38;5;245m│[39m   [38;5;245m│[39m    [38;5;245m│[39m   [38;5;245m│[39m    [38;5;245m│[39m│[0m
[0m│      [38;5;246m0[39m    [38;5;246m1[39m   [38;5;246m2[39m    [38;5;246m3[39m   [38;5;246m4[39m    [38;5;246m5[39m│[0m
[0m│                              │[0m
[0m└──────────────────────────────┘[0m|}] [@@ocamlformat "disable"]

let%expect_test "scatter X glyph" =
  let draw grid ~width ~height =
    let pts = [| (1., 1.); (3., 3.); (8., 2.) |] in
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 9.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 4.)) ())
      |> scatter ~glyph:"X" ~x:fst ~y:snd pts
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:10 ~height:5 draw;
  [%expect_exact
    {|
[0m┌──────────┐[0m
[0m│          │[0m
[0m│   [38;5;6mX[39m      │[0m
[0m│        [38;5;6mX[39m │[0m
[0m│ [38;5;6mX[39m        │[0m
[0m│          │[0m
[0m└──────────┘[0m|}]

let%expect_test "scatter braille" =
  let draw grid ~width ~height =
    let pts = [| (1., 1.); (3., 3.); (8., 2.) |] in
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 9.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 4.)) ())
      |> scatter ~mode:`Braille ~x:fst ~y:snd pts
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:10 ~height:5 draw;
  [%expect_exact
    {|
[0m┌──────────┐[0m
[0m│          │[0m
[0m│   [38;5;6m⠂[39m      │[0m
[0m│        [38;5;6m⠐[39m │[0m
[0m│ [38;5;6m⠄[39m        │[0m
[0m│          │[0m
[0m└──────────┘[0m|}]

let%expect_test "circle (line)" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 11.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 7.)) ())
      |> circle ~resolution:`Cell ~cx:fst ~cy:snd
           ~r:(fun _ -> 3.)
           [| (6., 4.) |]
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:12 ~height:8 draw;
  [%expect_exact
    {|
[0m┌────────────┐[0m
[0m│     [38;5;6m███[39m    │[0m
[0m│    [38;5;6m█[39m   [38;5;6m█[39m   │[0m
[0m│   [38;5;6m█[39m     [38;5;6m█[39m  │[0m
[0m│   [38;5;6m█[39m     [38;5;6m█[39m  │[0m
[0m│   [38;5;6m█[39m     [38;5;6m█[39m  │[0m
[0m│    [38;5;6m█[39m   [38;5;6m█[39m   │[0m
[0m│     [38;5;6m███[39m    │[0m
[0m│            │[0m
[0m└────────────┘[0m|}]

let%expect_test "circle braille" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 11.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 7.)) ())
      |> circle ~resolution:`Braille2x4 ~cx:fst ~cy:snd
           ~r:(fun _ -> 3.)
           [| (6., 4.) |]
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:12 ~height:8 draw;
  [%expect_exact
    {|
[0m┌────────────┐[0m
[0m│    [38;5;6m⢀⠔⠉⠑⢄[39m   │[0m
[0m│    [38;5;6m⡎[39m   [38;5;6m⠈⡆[39m  │[0m
[0m│   [38;5;6m⢸[39m     [38;5;6m⢸[39m  │[0m
[0m│   [38;5;6m⢸[39m     [38;5;6m⢸[39m  │[0m
[0m│   [38;5;6m⠸⡀[39m    [38;5;6m⡸[39m  │[0m
[0m│    [38;5;6m⢣[39m   [38;5;6m⢠⠃[39m  │[0m
[0m│     [38;5;6m⠑⠤⠔⠁[39m   │[0m
[0m│            │[0m
[0m└────────────┘[0m|}]

let%expect_test "circle sub-integer radius stays visible" =
  (* Radii are in data units: r=0.4 in a 0..1 domain must render as a circle
     of cells, not round to zero and vanish. *)
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 1.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 1.)) ())
      |> circle ~resolution:`Cell ~cx:fst ~cy:snd
           ~r:(fun _ -> 0.4)
           [| (0.5, 0.5) |]
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:12 ~height:8 draw;
  [%expect_exact
    {|
[0m┌────────────┐[0m
[0m│            │[0m
[0m│    [38;5;6m█████[39m   │[0m
[0m│   [38;5;6m█[39m     [38;5;6m█[39m  │[0m
[0m│  [38;5;6m█[39m       [38;5;6m█[39m │[0m
[0m│  [38;5;6m█[39m       [38;5;6m█[39m │[0m
[0m│  [38;5;6m█[39m       [38;5;6m█[39m │[0m
[0m│   [38;5;6m█[39m     [38;5;6m█[39m  │[0m
[0m│    [38;5;6m█████[39m   │[0m
[0m└────────────┘[0m|}]

let%expect_test "overlay text anchors" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.numeric ~domain:(`Domain (0., 9.)) ())
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 4.)) ())
    in
    let layout = draw chart grid ~width ~height in
    Overlay.text layout grid ~x:4.5 ~y:3. "beg";
    Overlay.text ~anchor:`Center layout grid ~x:4.5 ~y:2. "mid";
    Overlay.text ~anchor:`Right layout grid ~x:4.5 ~y:1. "end"
  in
  render_chart ~width:10 ~height:5 draw;
  [%expect_exact
    {|
[0m┌──────────┐[0m
[0m│          │[0m
[0m│     [38;5;246mbeg[39m  │[0m
[0m│    [38;5;246mmid[39m   │[0m
[0m│  [38;5;246mend[39m     │[0m
[0m│          │[0m
[0m└──────────┘[0m|}]

let%expect_test "ascii charset covers wave and area glyphs" =
  (* Charset.ascii must reach every glyph a chart emits: wave bends, area
     fills and stippled rules must not fall back to Unicode literals. *)
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_theme (Theme.with_charset Charset.ascii Theme.dark)
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> line ~resolution:`Wave ~x:fst ~y:snd [| (0., 0.); (9., 4.) |]
      |> add
           (Mark.area ~y_axis:`Y1
              ~x:(fun (x, _) -> x)
              ~y:(fun (_, y) -> y)
              [| (0., 1.); (9., 1.) |])
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:10 ~height:5 draw;
  [%expect_exact
    {|
[0m┌──────────┐[0m
[0m│        [38;5;6m+-[39m│[0m
[0m│      [38;5;6m+-+[39m │[0m
[0m│    [38;5;6m+-+[39m   │[0m
[0m│[38;5;5m#[39m [38;5;6m+-+[39m    [38;5;5m#[39m│[0m
[0m│[38;5;5m#[38;5;6m-+[39m      [38;5;5m#[39m│[0m
[0m└──────────┘[0m|}]

let%expect_test "vertical bar chart renders columns" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_frame (manual_frame ~margins:(0, 1, 2, 0) ())
      |> with_x_scale (Scale.band ())
      |> with_axes ~x:Axis.default ~y:Axis.hidden
      |> bar ~category:fst ~value:snd bar_simple
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:25 ~height:10 draw;
  [%expect_exact {|
[0m┌─────────────────────────┐[0m
[0m│      [38;5;6m▅▅▅▅[39m       [38;5;6m████[39m    │[0m
[0m│ [38;5;6m▃▃▃▃[39m [38;5;6m████[39m       [38;5;6m████[39m    │[0m
[0m│ [38;5;6m████[39m [38;5;6m████[39m  [38;5;6m████[39m [38;5;6m████[39m    │[0m
[0m│ [38;5;6m████[39m [38;5;6m████[39m  [38;5;6m████[39m [38;5;6m████[39m    │[0m
[0m│[38;5;245m───┬────┬─────┬────┬────[39m │[0m
[0m│   [38;5;245m│[39m    [38;5;245m│[39m     [38;5;245m│[39m    [38;5;245m│[39m     │[0m
[0m│  [38;5;246mQ1[39m   [38;5;246mQ2[39m    [38;5;246mQ3[39m   [38;5;246mQ4[39m     │[0m
[0m│                         │[0m
[0m│                         │[0m
[0m│                         │[0m
[0m└─────────────────────────┘[0m|}] [@@ocamlformat "disable"]

let%expect_test "horizontal bar chart" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_frame (manual_frame ~margins:(0, 2, 0, 0) ())
      |> with_y_scale (Scale.band ())
      |> with_axes ~x:Axis.hidden ~y:Axis.default
      |> bar ~direction:`Horizontal ~category:fst ~value:snd horizontal_bars
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:30 ~height:8 draw;
  [%expect_exact {|
[0m┌──────────────────────────────┐[0m
[0m│[38;5;246mA[39m [38;5;245m─┐[38;5;6m████████████████[39m          │[0m
[0m│   [38;5;245m│[39m                          │[0m
[0m│[38;5;246mB[39m [38;5;245m─┤[38;5;6m████████████████████████[39m  │[0m
[0m│   [38;5;245m│[39m                          │[0m
[0m│   [38;5;245m│[39m                          │[0m
[0m│[38;5;246mC[39m [38;5;245m─┤[38;5;6m████████████[39m              │[0m
[0m│   [38;5;245m│[39m                          │[0m
[0m│   [38;5;245m│[39m                          │[0m
[0m└──────────────────────────────┘[0m|}] [@@ocamlformat "disable"]

let simple_bars = [| ("A", 2.); ("B", 5.); ("C", 3.); ("D", 6.) |]

let%expect_test "vertical bars (no axis)" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.band ())
      |> bar ~category:fst ~value:snd simple_bars
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:11 ~height:8 draw;
  [%expect_exact {|
[0m┌───────────┐[0m
[0m│       [38;5;6m█[39m   │[0m
[0m│   [38;5;6m▅[39m   [38;5;6m█[39m   │[0m
[0m│   [38;5;6m█[39m   [38;5;6m█[39m   │[0m
[0m│   [38;5;6m█[39m   [38;5;6m█[39m   │[0m
[0m│   [38;5;6m█[39m [38;5;6m█[39m [38;5;6m█[39m   │[0m
[0m│[38;5;6m▅[39m  [38;5;6m█[39m [38;5;6m█[39m [38;5;6m█[39m   │[0m
[0m│[38;5;6m█[39m  [38;5;6m█[39m [38;5;6m█[39m [38;5;6m█[39m   │[0m
[0m│[38;5;6m█[39m  [38;5;6m█[39m [38;5;6m█[39m [38;5;6m█[39m   │[0m
[0m└───────────┘[0m|}] [@@ocamlformat "disable"]

let horizontal_bars_2 = [| ("Alpha", 4.); ("Beta", 8.); ("Gamma", 3.) |]

let%expect_test "horizontal bars (no axis)" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_y_scale (Scale.band ())
      |> bar ~direction:`Horizontal ~category:fst ~value:snd horizontal_bars_2
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:16 ~height:6 draw;
  [%expect_exact {|
[0m┌────────────────┐[0m
[0m│[38;5;6m████████[39m        │[0m
[0m│                │[0m
[0m│[38;5;6m████████████████[39m│[0m
[0m│[38;5;6m██████[39m          │[0m
[0m│                │[0m
[0m│                │[0m
[0m└────────────────┘[0m|}] [@@ocamlformat "disable"]

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
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_x_scale (Scale.band ())
      |> stacked_bar stacked_bars_data
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:15 ~height:8 draw;
  [%expect_exact {|
[0m┌───────────────┐[0m
[0m│         ██    │[0m
[0m│▃▃   ▃▃  ██    │[0m
[0m│██   ██  ██    │[0m
[0m│██   ██  ██    │[0m
[0m│██   ██  ██    │[0m
[0m│██   ██  ██    │[0m
[0m│██   ██  ██    │[0m
[0m│██   ██  ██    │[0m
[0m└───────────────┘[0m|}] [@@ocamlformat "disable"]

let%expect_test "stacked bars horizontal (no axis)" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> with_y_scale (Scale.band ())
      |> stacked_bar ~direction:`Horizontal stacked_bars_data
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:20 ~height:6 draw;
  [%expect_exact {|
[0m┌────────────────────┐[0m
[0m│████████████████    │[0m
[0m│                    │[0m
[0m│████████████████    │[0m
[0m│████████████████████│[0m
[0m│                    │[0m
[0m│                    │[0m
[0m└────────────────────┘[0m|}] [@@ocamlformat "disable"]

let%expect_test "sparkline bars" =
  (* Sparkline with label above *)
  let grid = Grid.create ~width:22 ~height:5 () in
  Grid.draw_box grid ~x:0 ~y:0 ~width:22 ~height:5 ();
  Grid.draw_text grid ~x:1 ~y:1 ~text:"CPU Usage:";
  Sparkline.draw_values ~kind:`Bars ~x:1 ~y:2
    [ 1.0; 3.0; 2.0; 5.0; 4.0; 3.0; 2.0; 4.0 ] grid ~width:15 ~height:1;
  print_newline ();
  print_string (Grid.to_ansi ~reset:false grid);
  [%expect_exact {|
[0m┌────────────────────┐[0m
[0m│CPU Usage:          │[0m
[0m│       ▂▅▃█▆▅▃▆     │[0m
[0m│                    │[0m
[0m└────────────────────┘[0m|}] [@@ocamlformat "disable"]

let%expect_test "sparkline bars (no layout)" =
  let draw grid ~width ~height =
    Sparkline.draw_values ~kind:`Bars
      [ 1.; 3.; 2.; 5.; 4.; 3.; 2.; 4. ]
      grid ~width ~height
  in
  render_chart ~width:12 ~height:3 draw;
  [%expect_exact
    {|
[0m┌────────────┐[0m
[0m│       █▃  ▃│[0m
[0m│     ▆▂██▆▂█│[0m
[0m│    ▅███████│[0m
[0m└────────────┘[0m|}]

let%expect_test "sparkline braille" =
  let draw grid ~width ~height =
    let m = Sparkline.create ~capacity:10 () in
    Sparkline.push_all m [ 0.; 2.; 4.; 3.; 5.; 4.; 2.; 1. ];
    Sparkline.draw m ~kind:`Braille grid ~width ~height
  in
  render_chart ~width:10 ~height:3 draw;
  [%expect_exact
    {|
[0m┌──────────┐[0m
[0m│    ⣄⡸⢢   │[0m
[0m│   ⡜ ⠁ ⢣  │[0m
[0m│  ⡰⠁    ⠑ │[0m
[0m└──────────┘[0m|}]

let%expect_test "heatmap fills cells (shaded)" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.hidden
      |> heatmap ~auto_value_range:true ~mode:Mark.Shaded
           ~color_scale:
             [| Ansi.Color.indexed 232;
                Ansi.Color.indexed 236;
                Ansi.Color.indexed 240;
                Ansi.Color.indexed 244;
                Ansi.Color.indexed 248;
                Ansi.Color.indexed 252 |]
           ~x:(fun (x, _, _) -> x)
           ~y:(fun (_, y, _) -> y)
           ~value:(fun (_, _, v) -> v)
           heat_points
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:25 ~height:10 draw;
  [%expect_exact {|
[0m┌─────────────────────────┐[0m
[0m│[38;5;236m░░    [38;5;232m                   [39m│[0m
[0m│[38;5;236m░░░░           [38;5;232m          [39m│[0m
[0m│[38;5;236m░░░░░░░░░░░░░░░░░░░░░░░░░[39m│[0m
[0m│[38;5;236m░░░░[38;5;240m░░░░░░░░░░░░░░░░░░░░░[39m│[0m
[0m│[38;5;236m░░[38;5;240m░░░░░░░░[38;5;244m▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒[39m│[0m
[0m│[38;5;236m░░[38;5;240m░░░░[38;5;244m▒▒▒▒▒▒▒[38;5;248m▒▒▒▒▒▒▒▒▒▒▒▒[39m│[0m
[0m│[38;5;236m░[38;5;240m░░░░[38;5;244m▒▒▒▒▒[38;5;248m▒▒▒▓▓▓▓▓[38;5;252m▓[38;5;248m▓▓▓▓▓▓[39m│[0m
[0m│[38;5;236m░[38;5;240m░░░░[38;5;244m▒▒▒[38;5;248m▒▒▒▓▓[38;5;252m▓▓▓▓▓▓▓▓▓▓▓▓[39m│[0m
[0m│[38;5;236m░[38;5;240m░░░[38;5;244m▒▒▒▒[38;5;248m▒▒▓▓[38;5;252m▓▓▓▓▓▓▓▓▓▓▓▓▓[39m│[0m
[0m│[38;5;236m░[38;5;240m░░░[38;5;244m▒▒▒[38;5;248m▒▒▓▓[38;5;252m▓▓▓▓▓▓▓█▓▓▓▓▓▓[39m│[0m
[0m└─────────────────────────┘[0m|}] [@@ocamlformat "disable"]

let%expect_test "candlestick draws bodies and wicks" =
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.default ~y:Axis.hidden
      |> candles candle_data
    in
    draw chart grid ~width ~height
  in
  render_chart ~width:20 ~height:10 draw;
  [%expect_exact {|
[0m┌────────────────────┐[0m
[0m│          [38;5;1m│[39m         │[0m
[0m│[38;5;2m│[39m         [38;5;1m│[39m         │[0m
[0m│[38;5;2m┃[39m         [38;5;1m┃[39m        [38;5;2m│[39m│[0m
[0m│[38;5;2m│[39m         [38;5;1m│[39m        [38;5;2m┃[39m│[0m
[0m│[38;5;2m│[39m         [38;5;1m│[39m        [38;5;2m│[39m│[0m
[0m│                   [38;5;2m│[39m│[0m
[0m│[38;5;245m┌────┬────┬───┬────┐[39m│[0m
[0m│[38;5;245m│[39m    [38;5;245m│[39m    [38;5;245m│[39m   [38;5;245m│[39m    [38;5;245m│[39m│[0m
[0m│[38;5;246m0[39m   [38;5;246m0.5[39m   [38;5;246m1[39m  [38;5;246m1.5[39m   [38;5;246m2[39m│[0m
[0m│                    │[0m
[0m└────────────────────┘[0m|}] [@@ocamlformat "disable"]

let%expect_test "zoomed y view keeps its axis labels" =
  (* The y gutter must be sized from the resolved view's ticks: zooming into
     a narrow range inside a wide domain needs wider labels than the domain. *)
  let draw grid ~width ~height =
    let chart =
      empty ()
      |> with_axes ~x:Axis.hidden ~y:Axis.default
      |> with_y_scale (Scale.numeric ~domain:(`Domain (0., 1000.)) ())
      |> line ~x:fst ~y:snd [| (0., 0.); (1., 1000.) |]
    in
    let view = View.(empty |> set_y (Some (window ~min:0.0011 ~max:0.0019))) in
    draw ~view chart grid ~width ~height
  in
  render_chart ~width:20 ~height:8 draw;
  [%expect_exact
    {|
[0m┌────────────────────┐[0m
[0m│        [38;5;245m│[38;5;6m│[39m          │[0m
[0m│[38;5;246m0.0018[39m [38;5;245m─┤[38;5;6m│[39m          │[0m
[0m│        [38;5;245m│[38;5;6m│[39m          │[0m
[0m│[38;5;246m0.0016[39m [38;5;245m─┤[38;5;6m│[39m          │[0m
[0m│[38;5;246m0.0014[39m [38;5;245m─┤[38;5;6m│[39m          │[0m
[0m│        [38;5;245m│[38;5;6m│[39m          │[0m
[0m│[38;5;246m0.0012[39m [38;5;245m─┤[38;5;6m│[39m          │[0m
[0m│ [38;5;246m0.001[39m [38;5;245m─┘[38;5;6m│[39m          │[0m
[0m└────────────────────┘[0m|}]

let%expect_test "hit test candles clamp to the wick span" =
  let draw grid ~width ~height =
    let chart =
      empty () |> with_axes ~x:Axis.hidden ~y:Axis.hidden |> candles candle_data
    in
    let layout = draw chart grid ~width ~height in
    let report policy name px py =
      match Layout.hit_test ~policy layout ~px ~py with
      | None -> Printf.printf "%s: none\n" name
      | Some h ->
          Printf.printf "%s: index %d distance %.0f\n" name h.Hit.index
            h.Hit.distance_px
    in
    (* Candle 2 tops out at row 4: a top-row query directly above it is four
       cells away vertically, not a distance-0 hit. *)
    report `Nearest_px "px above candle 2" 19 0;
    (* Only candle 1 spans the top row; the last candle in the list must not
       win with a bogus zero distance. *)
    report `Nearest_y "y top row" 0 0
  in
  render_chart ~width:20 ~height:10 draw;
  [%expect_exact
    {|px above candle 2: none
y top row: index 1 distance 0

[0m┌────────────────────┐[0m
[0m│          [38;5;1m│[39m         │[0m
[0m│          [38;5;1m│[39m         │[0m
[0m│[38;5;2m│[39m         [38;5;1m│[39m         │[0m
[0m│[38;5;2m│[39m         [38;5;1m│[39m         │[0m
[0m│[38;5;2m┃[39m         [38;5;1m┃[39m        [38;5;2m│[39m│[0m
[0m│[38;5;2m┃[39m         [38;5;1m┃[39m        [38;5;2m┃[39m│[0m
[0m│[38;5;2m│[39m         [38;5;1m│[39m        [38;5;2m┃[39m│[0m
[0m│[38;5;2m│[39m         [38;5;1m│[39m        [38;5;2m│[39m│[0m
[0m│[38;5;2m│[39m                  [38;5;2m│[39m│[0m
[0m│                   [38;5;2m│[39m│[0m
[0m└────────────────────┘[0m|}]

let%expect_test "legend arranges entries horizontally" =
  let draw grid ~width ~height =
    Legend.draw ~direction:`Horizontal
      [ { label = "Revenue"; style = Style.make ~fg:Ansi.Color.green (); marker = "●" };
        { label = "Costs"; style = Style.make ~fg:Ansi.Color.red (); marker = "●" };
        { label = "Profit"; style = Style.make ~fg:Ansi.Color.blue (); marker = "●" } ]
      grid ~width ~height
  in
  render_chart ~width:40 ~height:3 draw;
  [%expect_exact {|
[0m┌────────────────────────────────────────┐[0m
[0m│[38;5;2m● Revenue[39m  [38;5;1m● Costs[39m  [38;5;4m● Profit[39m            │[0m
[0m│                                        │[0m
[0m│                                        │[0m
[0m└────────────────────────────────────────┘[0m|}] [@@ocamlformat "disable"]

let%expect_test "stacked legend renders vertical list" =
  let draw grid ~width ~height =
    Legend.draw ~direction:`Vertical
      [ { label = "Alpha"; style = Style.make ~fg:Ansi.Color.yellow (); marker = "●" };
        { label = "Beta"; style = Style.make ~fg:Ansi.Color.cyan (); marker = "●" };
        { label = "Gamma"; style = Style.make ~fg:Ansi.Color.magenta (); marker = "●" } ]
      grid ~width ~height
  in
  render_chart ~width:18 ~height:5 draw;
  [%expect_exact {|
[0m┌──────────────────┐[0m
[0m│[38;5;3m●[39m [38;5;3mAlpha[39m           │[0m
[0m│[38;5;6m●[39m [38;5;6mBeta[39m            │[0m
[0m│[38;5;5m●[39m [38;5;5mGamma[39m           │[0m
[0m│                  │[0m
[0m│                  │[0m
[0m└──────────────────┘[0m|}] [@@ocamlformat "disable"]

let%expect_test "time labels before the epoch" =
  List.iter
    (fun v ->
      Printf.printf "%.1f %s %s\n" v
        (Label_format.mmdd_utc 0 v)
        (Label_format.hhmmss_utc 0 v))
    [ -1.; -0.5; -86400.; -31536000.; -2208988800.; 0.; 951782400.5 ];
  [%expect
    {|
    -1.0 12/31 23:59:59
    -0.5 12/31 23:59:59
    -86400.0 12/31 00:00:00
    -31536000.0 01/01 00:00:00
    -2208988800.0 01/01 00:00:00
    0.0 01/01 00:00:00
    951782400.5 02/29 00:00:00
    |}]

(* The text of [grid], one row per line between bars, with an empty or
   continuation cell as a space. *)
let print_cells grid =
  for y = 0 to Grid.height grid - 1 do
    let row = Buffer.create 64 in
    for x = 0 to Grid.width grid - 1 do
      match Grid.get_text grid (Grid.idx grid ~x ~y) with
      | "" -> Buffer.add_char row ' '
      | s -> Buffer.add_string row s
    done;
    Printf.printf "|%s|\n" (Buffer.contents row)
  done

(* A chart without axes whose plot is the whole 20 by 7 grid, with data
   coordinates equal to columns and rows counted from the bottom. *)
let bare_chart () =
  empty ()
  |> with_axes ~x:Axis.hidden ~y:Axis.hidden
  |> line ~style:(Style.make ()) ~x:fst ~y:snd [| (0., 0.); (19., 6.) |]

(* Right of the point the box clips by 2 cells, and moved inside the plot it
   covers the point. Below, it clips by 2 cells too and leaves it visible. *)
let%expect_test "a tooltip avoids covering its point" =
  let grid = Grid.create ~width:20 ~height:7 () in
  let layout = draw (bare_chart ()) grid ~width:20 ~height:7 in
  Grid.clear grid;
  Overlay.tooltip ~border:`None ~padding:0 layout grid ~x:6. ~y:3.
    [ "aaaaaaaaaaaaaa"; "b"; "c" ];
  Overlay.marker ~glyph:"X" layout grid ~x:6. ~y:3.;
  print_cells grid;
  [%expect
    {|
    |                    |
    |                    |
    |                    |
    |      X             |
    |aaaaaaaaaaaaaa      |
    |b                   |
    |c                   |
    |}]

(* A mark on the second y axis needs that axis's scale, which a chart may
   take after the mark, so the check comes when the chart is laid out. *)
let%expect_test "a `Y2 mark needs a second y scale" =
  let chart = empty () |> line ~y_axis:`Y2 ~x:fst ~y:snd simple_points in
  let grid = Grid.create ~width:20 ~height:7 () in
  (match draw chart grid ~width:20 ~height:7 with
  | _ -> print_endline "drawn"
  | exception Invalid_argument _ -> print_endline "Invalid_argument");
  (match layout chart ~width:20 ~height:7 with
  | _ -> print_endline "laid out"
  | exception Invalid_argument _ -> print_endline "Invalid_argument");
  let chart = with_y2_scale (Scale.numeric ()) chart in
  ignore (draw chart grid ~width:20 ~height:7 : Layout.t);
  print_endline "drawn with a second y scale";
  [%expect
    {|
    Invalid_argument
    Invalid_argument
    drawn with a second y scale
    |}]

(* The styles a chart gives its axis line and its gridlines come from its
   theme when it is drawn, whichever theme it was made with, while a style the
   program set stays. *)
let%expect_test "with_theme restyles what the program left unset" =
  let red = Style.make ~fg:Ansi.Color.red () in
  let green = Style.make ~fg:Ansi.Color.green () in
  let theme = { Theme.dark with axes = red; grid = green } in
  let fg grid ~x ~y = Grid.get_fg grid (Grid.idx grid ~x ~y) in
  let show name chart =
    let grid = Grid.create ~width:20 ~height:7 () in
    let l = draw chart grid ~width:20 ~height:7 in
    let r = Layout.plot_rect l in
    let axis = fg grid ~x:(r.x - 1) ~y:r.y in
    let gridline = fg grid ~x:(r.x + r.width - 1) ~y:(r.y + r.height - 1) in
    Printf.printf "%s: axis %b, gridline %b\n" name
      (Ansi.Color.equal axis Ansi.Color.red)
      (Ansi.Color.equal gridline Ansi.Color.green)
  in
  let chart ?theme () =
    empty ?theme ()
    |> with_grid (Gridlines.default |> Gridlines.with_style Style.default)
    |> line ~x:fst ~y:snd simple_points
  in
  show "made in the theme" (chart ~theme ());
  show "themed later" (with_theme theme (chart ()));
  let blue = Style.make ~fg:Ansi.Color.blue () in
  let own =
    chart ()
    |> with_axes ~y:(Axis.default |> Axis.with_style blue)
    |> with_theme theme
  in
  show "own axis style" own;
  [%expect
    {|
    made in the theme: axis true, gridline true
    themed later: axis true, gridline true
    own axis style: axis false, gridline true
    |}]

(* A chart drawn at an offset is the chart drawn at the origin, moved, with
   nothing drawn outside its region. *)
let%expect_test "draw at an offset moves the whole chart" =
  let chart =
    empty () |> with_title "T"
    |> with_frame (manual_frame ~margins:(1, 1, 1, 1) ())
    |> with_axes
         ~x:(Axis.default |> Axis.with_title "time")
         ~y:(Axis.default |> Axis.with_title "v" |> Axis.with_line `Frame)
    |> with_y2_scale (Scale.numeric ())
    |> with_y2_axis Axis.default
    |> line ~x:fst ~y:snd simple_points
    |> line ~y_axis:`Y2 ~x:fst ~y:(fun (_, y) -> y *. 100.) simple_points
  in
  let width = 30 and height = 12 in
  let at_origin = Grid.create ~width ~height () in
  ignore (draw chart at_origin ~width ~height : Layout.t);
  let dx = 7 and dy = 4 in
  let moved = Grid.create ~width:(width + 14) ~height:(height + 8) () in
  ignore (draw ~x:dx ~y:dy chart moved ~width ~height : Layout.t);
  let text g ~x ~y =
    match Grid.get_text g (Grid.idx g ~x ~y) with "" -> " " | s -> s
  in
  let differ = ref 0 in
  for y = 0 to Grid.height moved - 1 do
    for x = 0 to Grid.width moved - 1 do
      let inside = x >= dx && x < dx + width && y >= dy && y < dy + height in
      let expected =
        if inside then text at_origin ~x:(x - dx) ~y:(y - dy) else " "
      in
      if text moved ~x ~y <> expected then incr differ
    done
  done;
  Printf.printf "%d cells differ\n" !differ;
  [%expect {| 0 cells differ |}]

(* Axis lines are drawn with the charset's axis glyphs, and where they meet
   each other, or a tick mark, with its corners, tees and cross. *)
let%expect_test "axis lines and their junctions" =
  let frame =
    {
      Charset.tl = "a";
      tr = "b";
      bl = "c";
      br = "d";
      h = "-";
      v = "|";
      tee_up = "u";
      tee_down = "n";
      tee_left = "l";
      tee_right = "r";
      cross = "x";
    }
  in
  let charset =
    { Charset.ascii with frame; axis_h = "="; axis_v = "!"; tick_h = "-" }
  in
  let theme = Theme.with_charset charset Theme.dark in
  let chart =
    empty ~theme ()
    |> with_axes ~x:(Axis.default |> Axis.with_line `Frame) ~y:Axis.default
    |> with_y2_scale (Scale.numeric ())
    |> with_y2_axis Axis.default
    |> line ~style:(Style.make ()) ~glyph:"." ~x:fst ~y:snd simple_points
    |> line ~y_axis:`Y2 ~style:(Style.make ()) ~glyph:"." ~x:fst
         ~y:(fun (_, y) -> y *. 2.)
         simple_points
  in
  let grid = Grid.create ~width:24 ~height:8 () in
  ignore (draw chart grid ~width:24 ~height:8 : Layout.t);
  print_cells grid;
  [%expect
    {|
    |  3 -b             !a-6 |
    |2.5 -l    .        !r-5 |
    |1.5 -l         .   !r-3 |
    |0.5 -l.            !r-1 |
    |     cn=n=n==n=n=n=xd   |
    |      | | |  | | | |    |
    |      0   1 1.5 2.5     |
    |                        |
    |}]

let%expect_test "a manual frame draws the theme's border" =
  let theme = Theme.with_charset Charset.ascii Theme.dark in
  let chart =
    empty ~theme ()
    |> with_frame (manual_frame ~margins:(0, 1, 0, 1) ~border:true ())
    |> with_axes ~x:Axis.hidden ~y:Axis.hidden
    |> line ~style:(Style.make ()) ~glyph:"." ~x:fst ~y:snd simple_points
  in
  let grid = Grid.create ~width:12 ~height:5 () in
  ignore (draw chart grid ~width:12 ~height:5 : Layout.t);
  print_cells grid;
  let fg = Grid.get_fg grid (Grid.idx grid ~x:0 ~y:2) in
  Printf.printf "border in Theme.border: %b\n"
    (Ansi.Color.equal fg (Option.get theme.border.fg));
  [%expect {|
    |+----------+|
    ||        . ||
    ||   .  .   ||
    || .        ||
    |+----------+|
    border in Theme.border: true
    |}]
