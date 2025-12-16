(** Interactive charts with tooltips and multiple chart types. *)

open Mosaic_tea
open Mosaic_charts
module Canvas = Mosaic_ui.Canvas
module Event = Mosaic_ui.Event

(* Chart types *)
type chart_type = Line_interactive | Scatter | Bar | Heatmap | Candlestick

let all_charts = [ Line_interactive; Scatter; Bar; Heatmap; Candlestick ]

let chart_name = function
  | Line_interactive -> "Line (Interactive)"
  | Scatter -> "Scatter"
  | Bar -> "Bar"
  | Heatmap -> "Heatmap"
  | Candlestick -> "Candlestick"

(* Model *)
type model = {
  chart : chart_type;
  hover : (int * int) option;
  x_view : (float * float) option;
  y_view : (float * float) option;
}

type msg =
  | Next_chart
  | Prev_chart
  | Set_hover of (int * int) option
  | Zoom_in
  | Zoom_out
  | Reset_zoom
  | Quit

let init () =
  ( { chart = Line_interactive; hover = None; x_view = None; y_view = None },
    Cmd.none )

let next_chart chart =
  match chart with
  | Line_interactive -> Scatter
  | Scatter -> Bar
  | Bar -> Heatmap
  | Heatmap -> Candlestick
  | Candlestick -> Line_interactive

let prev_chart chart =
  match chart with
  | Line_interactive -> Candlestick
  | Scatter -> Line_interactive
  | Bar -> Scatter
  | Heatmap -> Bar
  | Candlestick -> Heatmap

(* Sample data - defined before update so bounds can be used *)
let line_data =
  List.init 50 (fun i ->
      let x = float_of_int i in
      let y = (sin (x *. 0.2) *. 3.0) +. (cos (x *. 0.1) *. 2.0) +. 5.0 in
      (x, y))

(* Default bounds for line data - used for zoom *)
let line_x_bounds = Plot.bounds line_data ~f:fst
let line_y_bounds = Plot.bounds line_data ~f:snd

let update msg model =
  match msg with
  | Next_chart ->
      ( {
          chart = next_chart model.chart;
          hover = None;
          x_view = None;
          y_view = None;
        },
        Cmd.none )
  | Prev_chart ->
      ( {
          chart = prev_chart model.chart;
          hover = None;
          x_view = None;
          y_view = None;
        },
        Cmd.none )
  | Set_hover hover -> ({ model with hover }, Cmd.none)
  | Zoom_in ->
      let x_view =
        Plot.zoom (Option.value model.x_view ~default:line_x_bounds) ~factor:1.2
      in
      let y_view =
        Plot.zoom (Option.value model.y_view ~default:line_y_bounds) ~factor:1.2
      in
      ({ model with x_view = Some x_view; y_view = Some y_view }, Cmd.none)
  | Zoom_out ->
      let x_view =
        Plot.zoom (Option.value model.x_view ~default:line_x_bounds) ~factor:0.8
      in
      let y_view =
        Plot.zoom (Option.value model.y_view ~default:line_y_bounds) ~factor:0.8
      in
      ({ model with x_view = Some x_view; y_view = Some y_view }, Cmd.none)
  | Reset_zoom -> ({ model with x_view = None; y_view = None }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let scatter_data =
  List.init 100 (fun i ->
      let x = float_of_int (i mod 20) +. (Random.float 0.5 -. 0.25) in
      let y = float_of_int (i / 20) +. (Random.float 0.5 -. 0.25) in
      (x, y))

let bar_data =
  [
    ("Mon", 12.0);
    ("Tue", 19.0);
    ("Wed", 8.0);
    ("Thu", 15.0);
    ("Fri", 22.0);
    ("Sat", 10.0);
    ("Sun", 14.0);
  ]

let heatmap_data =
  List.concat
    (List.init 10 (fun y ->
         List.init 10 (fun x ->
             let value =
               sin (float_of_int x *. 0.5) *. cos (float_of_int y *. 0.5)
             in
             {
               x = float_of_int x;
               y = float_of_int y;
               value = (value +. 1.0) /. 2.0;
             })))

let candlestick_data =
  let base = 100.0 in
  List.init 20 (fun i ->
      let open_ =
        base +. (float_of_int i *. 0.5) +. (Random.float 5.0 -. 2.5)
      in
      let close = open_ +. (Random.float 6.0 -. 3.0) in
      let high = max open_ close +. Random.float 2.0 in
      let low = min open_ close -. Random.float 2.0 in
      { time = float_of_int i; open_; high; low; close })

(* Styles *)
let header_bg = Ansi.Color.of_rgb 40 60 80
let footer_bg = Ansi.Color.grayscale ~level:3
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let tooltip_style =
  Ansi.Style.make ~fg:Ansi.Color.white ~bg:(Ansi.Color.grayscale ~level:4) ()

let crosshair_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:10) ()
let marker_style = Ansi.Style.make ~fg:Ansi.Color.yellow ~bold:true ()

(* Chart drawing functions *)
let draw_line_chart model canvas ~width ~height =
  let base_plot =
    Plot.make
      ~margins:{ top = 1; right = 2; bottom = 2; left = 6 }
      ~axes:true ~grid:true ()
    |> Plot.axes
         ~style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:12) ())
         ~x_ticks:8 ~y_ticks:5
    |> Plot.grid
         ~style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:5) ())
         ~x:true ~y:true
    |> Plot.line ~kind:`Braille
         ~style:(Ansi.Style.make ~fg:Ansi.Color.cyan ())
         ~x:fst ~y:snd line_data
  in
  (* Apply viewport if zoomed *)
  let plot =
    match model.x_view with
    | Some xv -> Plot.x_view xv base_plot
    | None -> base_plot
  in
  let plot =
    match model.y_view with Some yv -> Plot.y_view yv plot | None -> plot
  in
  let t = Plot.draw plot canvas ~width ~height in
  (* Draw interactive overlay if hovering *)
  (match model.hover with
  | Some (px, py) -> (
      match t.px_to_data px py with
      | Some (dx, dy) -> (
          (* Find nearest data point for snapping using 2D distance *)
          let nearest =
            List.fold_left
              (fun best (x, y) ->
                let dist_x = x -. dx in
                let dist_y = y -. dy in
                let dist = sqrt ((dist_x *. dist_x) +. (dist_y *. dist_y)) in
                match best with
                | None -> Some (x, y, dist)
                | Some (_, _, d) when dist < d -> Some (x, y, dist)
                | _ -> best)
              None line_data
          in
          match nearest with
          | Some (snap_x, snap_y, _) ->
              Plot.draw_crosshair ~style:crosshair_style t canvas ~x:snap_x
                ~y:snap_y;
              Plot.draw_marker ~style:marker_style ~glyph:"*" t canvas ~x:snap_x
                ~y:snap_y;
              Plot.draw_tooltip ~style:tooltip_style t canvas ~x:snap_x
                ~y:snap_y
                [
                  Printf.sprintf " x: %.1f " snap_x;
                  Printf.sprintf " y: %.2f " snap_y;
                ]
          | None -> ())
      | None -> ())
  | None -> ());
  t

let draw_scatter_chart _model canvas ~width ~height =
  let plot =
    Plot.make
      ~margins:{ top = 1; right = 2; bottom = 2; left = 4 }
      ~axes:true ~grid:true ()
    |> Plot.axes
         ~style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:12) ())
         ~x_ticks:5 ~y_ticks:5
    |> Plot.grid ~style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:5) ())
    |> Plot.scatter ~kind:`Braille ~glyph:"."
         ~style:(Ansi.Style.make ~fg:Ansi.Color.green ())
         ~x:fst ~y:snd scatter_data
  in
  Plot.draw plot canvas ~width ~height

let draw_bar_chart _model canvas ~width ~height =
  let plot =
    Plot.make
      ~margins:{ top = 1; right = 1; bottom = 2; left = 4 }
      ~axes:true ()
    |> Plot.axes
         ~style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:12) ())
         ~y_ticks:5
    |> Plot.x_band (List.map fst bar_data)
    |> Plot.bar_y
         ~style:(Ansi.Style.make ~fg:Ansi.Color.blue ())
         ~x:fst ~y:snd bar_data
  in
  Plot.draw plot canvas ~width ~height

let draw_heatmap_chart _model canvas ~width ~height =
  let plot =
    Plot.make
      ~margins:{ top = 1; right = 1; bottom = 2; left = 3 }
      ~axes:true ()
    |> Plot.axes
         ~style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:12) ())
         ~x_ticks:5 ~y_ticks:5
    |> Plot.heatmap ~shaded:true ~auto_value_range:true
         ~x:(fun (p : heat_point) -> p.x)
         ~y:(fun (p : heat_point) -> p.y)
         ~value:(fun (p : heat_point) -> p.value)
         heatmap_data
  in
  Plot.draw plot canvas ~width ~height

let draw_candlestick_chart _model canvas ~width ~height =
  let plot =
    Plot.make
      ~margins:{ top = 1; right = 1; bottom = 2; left = 6 }
      ~axes:true ~grid:true ()
    |> Plot.axes
         ~style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:12) ())
         ~x_ticks:5 ~y_ticks:5
    |> Plot.grid ~style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:5) ())
    |> Plot.candles
         ~bullish:(Ansi.Style.make ~fg:Ansi.Color.green ())
         ~bearish:(Ansi.Style.make ~fg:Ansi.Color.red ())
         ~time:(fun p -> p.time)
         ~open_:(fun p -> p.open_)
         ~high:(fun p -> p.high)
         ~low:(fun p -> p.low)
         ~close:(fun p -> p.close)
         candlestick_data
  in
  Plot.draw plot canvas ~width ~height

let view model =
  let chart_index =
    let rec find i = function
      | [] -> 0
      | c :: _ when c = model.chart -> i
      | _ :: rest -> find (i + 1) rest
    in
    find 0 all_charts
  in
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header *)
      box ~padding:(padding 1) ~background:header_bg
        [
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            [
              text ~text_style:(Ansi.Style.make ~bold:true ()) "Charts Example";
              text ~text_style:muted
                (Printf.sprintf "[%d/%d] %s" (chart_index + 1)
                   (List.length all_charts) (chart_name model.chart));
            ];
        ];
      (* Chart area *)
      box ~flex_grow:1. ~padding:(padding 1)
        [
          (match model.chart with
          | Line_interactive ->
              canvas
                ~on_mouse:(fun ev ->
                  match Event.Mouse.kind ev with
                  | Move ->
                      Some
                        (Set_hover (Some (Event.Mouse.x ev, Event.Mouse.y ev)))
                  | Out -> Some (Set_hover None)
                  | Scroll -> (
                      match Event.Mouse.scroll_delta ev with
                      | Some (Scroll_up, _) -> Some Zoom_in
                      | Some (Scroll_down, _) -> Some Zoom_out
                      | _ -> None)
                  | _ -> None)
                ~draw:(fun canvas ~width ~height ->
                  let _ = draw_line_chart model canvas ~width ~height in
                  ())
                ~size:{ width = pct 100; height = pct 100 }
                ()
          | Scatter ->
              canvas
                ~draw:(fun canvas ~width ~height ->
                  ignore (draw_scatter_chart model canvas ~width ~height))
                ~size:{ width = pct 100; height = pct 100 }
                ()
          | Bar ->
              canvas
                ~draw:(fun canvas ~width ~height ->
                  ignore (draw_bar_chart model canvas ~width ~height))
                ~size:{ width = pct 100; height = pct 100 }
                ()
          | Heatmap ->
              canvas
                ~draw:(fun canvas ~width ~height ->
                  ignore (draw_heatmap_chart model canvas ~width ~height))
                ~size:{ width = pct 100; height = pct 100 }
                ()
          | Candlestick ->
              canvas
                ~draw:(fun canvas ~width ~height ->
                  ignore (draw_candlestick_chart model canvas ~width ~height))
                ~size:{ width = pct 100; height = pct 100 }
                ());
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [
          text ~text_style:hint
            (match model.chart with
            | Line_interactive ->
                "< > cycle  |  scroll/+- zoom  |  r reset  |  hover for \
                 tooltip  |  q quit"
            | _ -> "< > cycle charts  |  q quit");
        ];
    ]

let subscriptions model =
  Sub.on_key (fun ev ->
      let data = Event.Key.data ev in
      match data.key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | Char c when Uchar.equal c (Uchar.of_char 'n') -> Some Next_chart
      | Char c when Uchar.equal c (Uchar.of_char 'p') -> Some Prev_chart
      | Right -> Some Next_chart
      | Tab when not data.modifier.shift -> Some Next_chart
      | Left -> Some Prev_chart
      | Tab when data.modifier.shift -> Some Prev_chart
      (* Zoom controls for interactive chart *)
      | Char c
        when Uchar.equal c (Uchar.of_char '+') && model.chart = Line_interactive
        ->
          Some Zoom_in
      | Char c
        when Uchar.equal c (Uchar.of_char '=') && model.chart = Line_interactive
        ->
          Some Zoom_in
      | Char c
        when Uchar.equal c (Uchar.of_char '-') && model.chart = Line_interactive
        ->
          Some Zoom_out
      | Char c
        when Uchar.equal c (Uchar.of_char 'r') && model.chart = Line_interactive
        ->
          Some Reset_zoom
      | _ -> None)

let () =
  Random.init 42;
  run { init; update; view; subscriptions }
