open Ui
open Mosaic_charts

(** Sample data generation *)

let generate_sine_wave points amplitude frequency =
  List.init points (fun i ->
      let x = float_of_int i /. float_of_int points *. 4.0 *. Float.pi in
      { x; y = amplitude *. sin (frequency *. x) })

let generate_random_walk points start variance =
  let rec aux acc remaining current =
    if remaining = 0 then List.rev acc
    else
      let change = Random.float variance -. (variance /. 2.0) in
      let new_value = current +. change in
      let point = { x = float_of_int (points - remaining); y = new_value } in
      aux (point :: acc) (remaining - 1) new_value
  in
  aux [] points start

let generate_time_series_data points start_time interval =
  List.init points (fun i ->
      let time = start_time +. (float_of_int i *. interval) in
      let value =
        50.0 +. (20.0 *. sin (float_of_int i /. 10.0)) +. Random.float 10.0
      in
      { time; value })

let generate_ohlc_data points start_time interval start_price =
  let rec aux acc remaining current_price current_time =
    if remaining = 0 then List.rev acc
    else
      let volatility = 0.02 in
      let trend = if Random.bool () then 1.0 else -1.0 in
      let open_ = current_price in
      let high = current_price *. (1.0 +. Random.float volatility) in
      let low = current_price *. (1.0 -. Random.float volatility) in
      let close =
        current_price *. (1.0 +. (trend *. Random.float volatility))
      in
      let point = { time = current_time; open_; high; low; close } in
      aux (point :: acc) (remaining - 1) close (current_time +. interval)
  in
  aux [] points start_price start_time

let generate_bar_data labels =
  List.map
    (fun label ->
      let num_segments = 1 + Random.int 3 in
      let segments =
        List.init num_segments (fun i ->
            let colors =
              [|
                Style.Index 196;
                Style.Index 202;
                Style.Index 226;
                Style.Index 46;
              |]
            in
            {
              value = Random.float 50.0 +. 10.0;
              style = Style.bg colors.(i mod Array.length colors);
              label = Some (Printf.sprintf "Seg %d" (i + 1));
            })
      in
      { label; segments })
    labels

let generate_heatmap_data grid_size =
  List.flatten
    (List.init grid_size (fun x ->
         List.init grid_size (fun y ->
             let value =
               sin (float_of_int x /. 5.0) *. cos (float_of_int y /. 5.0)
             in
             { x = float_of_int x; y = float_of_int y; value })))

(** Demo functions for each chart type *)

let line_chart_demo () =
  let sine_data = generate_sine_wave 50 1.0 1.0 in
  let cosine_data = generate_sine_wave 50 0.8 1.5 in
  let random_data = generate_random_walk 50 0.0 0.3 in

  vbox ~gap:1
    [
      text ~style:Style.(fg Yellow ++ bold) "Line Chart Demo";
      divider ();
      text "Multiple series with different styles:";
      line ~width:(Px 60) ~height:(Px 15)
        ~series_styles:[ Style.(fg Red); Style.(fg Blue); Style.(fg Green) ]
        [
          ("Sine Wave", sine_data);
          ("Cosine Wave", cosine_data);
          ("Random Walk", random_data);
        ];
      divider ();
      text "Same data with Braille rendering:";
      line ~width:(Px 60) ~height:(Px 10) ~render_kind:Braille
        ~series_styles:[ Style.(fg Cyan) ]
        [ ("Braille Line", sine_data) ];
      divider ();
      text "Scatter plot with Points:";
      line ~width:(Px 60) ~height:(Px 10) ~render_kind:(Points "")
        ~series_styles:[ Style.(fg Magenta) ]
        [ ("Scatter", List.filter (fun _ -> Random.bool ()) sine_data) ];
    ]

let time_series_demo () =
  let now = Unix.time () in
  let hour = 3600.0 in
  let data1 = generate_time_series_data 24 (now -. (24.0 *. hour)) hour in
  let data2 = generate_time_series_data 24 (now -. (24.0 *. hour)) hour in

  vbox ~gap:1
    [
      text ~style:Style.(fg Yellow ++ bold) "Time Series Demo";
      divider ();
      text "24-hour data with multiple series:";
      time_series ~width:(Px 60) ~height:(Px 15)
        ~series_styles:[ Style.(fg Blue); Style.(fg Red) ]
        [
          ("Temperature", data1);
          ( "Humidity",
            List.map
              (fun (p : time_series_point) ->
                { p with value = (p.value *. 0.8) -. 10.0 })
              data2 );
        ];
    ]

let bar_chart_demo () =
  let categories = [ "Q1"; "Q2"; "Q3"; "Q4" ] in
  let bar_data = generate_bar_data categories in

  vbox ~gap:1
    [
      text ~style:Style.(fg Yellow ++ bold) "Bar Chart Demo";
      divider ();
      text "Vertical stacked bars:";
      bar ~width:(Px 60) ~height:(Px 12) ~orientation:`Vertical bar_data;
      divider ();
      text "Horizontal bars:";
      bar ~width:(Px 60) ~height:(Px 8) ~orientation:`Horizontal ~bar_width:2
        (List.map
           (fun b -> { b with segments = [ List.hd b.segments ] })
           bar_data);
    ]

let sparkline_demo () =
  let data1 =
    List.init 20 (fun i -> (sin (float_of_int i /. 3.0) *. 50.0) +. 50.0)
  in
  let data2 = List.init 20 (fun _ -> Random.float 100.0) in
  let data3 =
    List.init 30 (fun i -> (float_of_int i *. 3.0) +. Random.float 20.0)
  in

  vbox ~gap:1
    [
      text ~style:Style.(fg Yellow ++ bold) "Sparkline Demo";
      divider ();
      hbox ~gap:2
        [
          vbox [ text "CPU Usage:"; sparkline ~style:Style.(fg Green) data1 ];
          vbox
            [
              text "Memory:";
              sparkline ~style:Style.(fg Blue) ~render_kind:`Line data2;
            ];
          vbox
            [
              text "Network I/O:";
              sparkline ~style:Style.(fg Red) ~render_kind:`Braille data3;
            ];
        ];
      divider ();
      text "Inline sparklines in text:";
      hbox ~gap:1
        [
          text "Sales trend:";
          sparkline ~width:15 ~style:Style.(fg Green) data3;
          text "(+15% �)";
        ];
    ]

let heatmap_demo () =
  let heatmap_data = generate_heatmap_data 20 in

  vbox ~gap:1
    [
      text ~style:Style.(fg Yellow ++ bold) "Heatmap Demo";
      divider ();
      text "2D function visualization (sin(x) * cos(y)):";
      heatmap ~width:(Px 60) ~height:(Px 15)
        ~color_scale:
          [
            Style.Index 17;
            (* Dark blue *)
            Style.Index 19;
            (* Blue *)
            Style.Index 51;
            (* Cyan *)
            Style.Index 226;
            (* Yellow *)
            Style.Index 208;
            (* Orange *)
            Style.Index 196;
            (* Red *)
          ]
        heatmap_data;
    ]

let candlestick_demo () =
  let now = Unix.time () in
  let day = 86400.0 in
  let ohlc_data = generate_ohlc_data 20 (now -. (20.0 *. day)) day 100.0 in

  vbox ~gap:1
    [
      text ~style:Style.(fg Yellow ++ bold) "Candlestick Chart Demo";
      divider ();
      text "20-day OHLC data:";
      candlestick ~width:(Px 60) ~height:(Px 15)
        ~bullish_style:Style.(fg Green ++ bold)
        ~bearish_style:Style.(fg Red ++ bold)
        ohlc_data;
      divider ();
      text ~style:Style.(fg Green) "Green: Bullish (close > open)";
      text ~style:Style.(fg Red) "Red: Bearish (close < open)";
    ]

let combined_demo () =
  vbox ~gap:2
    [
      text ~style:Style.(fg Cyan ++ bold ++ underline) "=� Mosaic Charts Demo";
      text "A showcase of all available chart types";
      divider ~style:Style.(fg (Index 240)) ();
      (* Create a grid layout for charts *)
      vbox ~gap:2
        [
          (* Row 1: Line and Time Series *)
          hbox ~gap:2
            [
              panel ~box_style:Rounded ~title:"Line Chart" ~expand:false
                (line ~width:(Px 40) ~height:(Px 10)
                   ~series_styles:
                     [ Style.(fg (Index 39)); Style.(fg (Index 214)) ]
                   [
                     ("Data 1", generate_sine_wave 30 1.0 1.0);
                     ("Data 2", generate_sine_wave 30 0.7 2.0);
                   ]);
              panel ~box_style:Rounded ~title:"Time Series" ~expand:false
                (time_series ~width:(Px 40) ~height:(Px 10)
                   (let now = Unix.time () in
                    [
                      ( "Series",
                        generate_time_series_data 20 (now -. 7200.0) 360.0 );
                    ]));
            ];
          (* Row 2: Bar and Candlestick *)
          hbox ~gap:2
            [
              panel ~box_style:Rounded ~title:"Bar Chart" ~expand:false
                (bar ~width:(Px 40) ~height:(Px 10)
                   (generate_bar_data [ "A"; "B"; "C"; "D" ]));
              panel ~box_style:Rounded ~title:"Candlestick" ~expand:false
                (candlestick ~width:(Px 40) ~height:(Px 10)
                   (generate_ohlc_data 15 0.0 1.0 50.0));
            ];
          (* Row 3: Heatmap and Sparklines *)
          hbox ~gap:2
            [
              panel ~box_style:Rounded ~title:"Heatmap" ~expand:false
                (heatmap ~width:(Px 40) ~height:(Px 10)
                   ~color_scale:
                     [
                       Style.Index 232;
                       Style.Index 236;
                       Style.Index 240;
                       Style.Index 244;
                       Style.Index 248;
                       Style.Index 252;
                     ]
                   (generate_heatmap_data 15));
              panel ~box_style:Rounded ~title:"Sparklines" ~expand:false
                (vbox ~gap:1
                   [
                     hbox ~gap:2
                       [
                         text "CPU:";
                         sparkline ~width:30
                           ~style:Style.(fg Green)
                           (List.init 30 (fun i ->
                                50.0 +. (30.0 *. sin (float_of_int i /. 5.0))));
                       ];
                     hbox ~gap:2
                       [
                         text "MEM:";
                         sparkline ~width:30
                           ~style:Style.(fg Blue)
                           ~render_kind:`Line
                           (List.init 30 (fun _ -> Random.float 100.0));
                       ];
                     box ~margin:(Spacing.make ~top:5 ())
                       [
                         hbox ~gap:2
                           [
                             text "NET:";
                             sparkline ~width:30
                               ~style:Style.(fg Yellow)
                               ~render_kind:`Braille
                               (List.init 30 (fun i ->
                                    (float_of_int i *. 2.0) +. Random.float 10.0));
                           ];
                       ];
                   ]);
            ];
        ];
    ]

(** Main entry point *)

let () =
  Random.self_init ();

  (* Create a comprehensive demo showing all chart types *)
  let ui =
    vbox ~gap:2
      [
        text
          ~style:Style.(fg Cyan ++ bold ++ underline)
          "═══ Mosaic Charts Demo ═══";
        text "A showcase of all available chart types";
        divider ~style:Style.(fg (Index 240)) ();
        (* Show all demo sections *)
        line_chart_demo ();
        divider ~style:Style.(fg (Index 240)) ();
        time_series_demo ();
        divider ~style:Style.(fg (Index 240)) ();
        bar_chart_demo ();
        divider ~style:Style.(fg (Index 240)) ();
        sparkline_demo ();
        divider ~style:Style.(fg (Index 240)) ();
        heatmap_demo ();
        divider ~style:Style.(fg (Index 240)) ();
        candlestick_demo ();
        divider ~style:Style.(fg (Index 240)) ();
        (* Also show the combined grid view *)
        combined_demo ();
      ]
  in

  (* Print the entire demo at once *)
  print ~width:120 ~height:200 ui
