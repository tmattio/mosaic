(** Interactive charts with tooltips and multiple chart types.

    Demonstrates the new Chart API:
    - Building charts with marks via {!add} or convenience functions
    - {!Layout.t} for coordinate transforms and hit testing
    - {!View} for zoom/pan state management
    - {!Overlay} for crosshairs, markers, and tooltips *)

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
  view : View.t;
  canvas_size : (int * int) option;
}

type msg =
  | Next_chart
  | Prev_chart
  | Set_hover of (int * int) option
  | Zoom_at of int * int * [ `In | `Out ]
  | Zoom_center of [ `In | `Out ]
  | Reset_zoom
  | Set_canvas_size of int * int
  | Quit

let init () =
  ( {
      chart = Line_interactive;
      hover = None;
      view = View.empty;
      canvas_size = None;
    },
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

(* Sample data - using arrays for the new API *)
let line_data =
  Array.init 50 (fun i ->
      let x = float_of_int i in
      let y = (sin (x *. 0.2) *. 3.0) +. (cos (x *. 0.1) *. 2.0) +. 5.0 in
      (x, y))

(* Styles used by update - defined here to be available before other data *)
let axis_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:12) ()
let grid_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:5) ()

(* Helper to build the line chart spec - shared between update and draw *)
let make_line_chart () =
  empty ()
  |> with_frame { margins = (1, 2, 2, 6); inner_padding = 0 }
  |> with_axes
       ~x:(Axis.default |> Axis.with_ticks 8 |> Axis.with_style axis_style)
       ~y:(Axis.default |> Axis.with_ticks 5 |> Axis.with_style axis_style)
  |> with_grid (Grid.default |> Grid.with_style grid_style)
  |> line ~kind:`Braille
       ~style:(Ansi.Style.make ~fg:Ansi.Color.cyan ())
       ~x:fst ~y:snd line_data

let update msg model =
  match msg with
  | Next_chart ->
      ( {
          model with
          chart = next_chart model.chart;
          hover = None;
          view = View.empty;
          (* Keep canvas_size - it's independent of chart type *)
        },
        Cmd.none )
  | Prev_chart ->
      ( {
          model with
          chart = prev_chart model.chart;
          hover = None;
          view = View.empty;
          (* Keep canvas_size - it's independent of chart type *)
        },
        Cmd.none )
  | Set_hover hover -> ({ model with hover }, Cmd.none)
  | Zoom_at (px, py, direction) -> (
      let factor = match direction with `In -> 1.3 | `Out -> 0.7 in
      match model.canvas_size with
      | Some (width, height) ->
          (* Compute layout using current canvas size *)
          let chart = make_line_chart () in
          let ly = layout ~view:model.view chart ~width ~height in
          if Layout.is_inside_plot ly ~px ~py then
            let view' =
              Layout.zoom_view_around_px ly ~view:model.view ~axis:`Both ~px ~py
                ~factor
            in
            let view' = Layout.clamp_view ly view' in
            ({ model with view = view' }, Cmd.none)
          else (model, Cmd.none)
      | None -> (model, Cmd.none))
  | Zoom_center direction -> (
      let factor = match direction with `In -> 1.3 | `Out -> 0.7 in
      match model.canvas_size with
      | Some (width, height) ->
          (* Compute layout using current canvas size *)
          let chart = make_line_chart () in
          let ly = layout ~view:model.view chart ~width ~height in
          let view' =
            Layout.zoom_view_around_center ly ~view:model.view ~axis:`Both
              ~factor
          in
          let view' = Layout.clamp_view ly view' in
          ({ model with view = view' }, Cmd.none)
      | None -> (model, Cmd.none))
  | Reset_zoom -> ({ model with view = View.empty }, Cmd.none)
  | Set_canvas_size (width, height) ->
      ({ model with canvas_size = Some (width, height) }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let scatter_data =
  Array.init 100 (fun i ->
      let x = float_of_int (i mod 20) +. (Random.float 0.5 -. 0.25) in
      let y = float_of_int (i / 20) +. (Random.float 0.5 -. 0.25) in
      (x, y))

let bar_data =
  [|
    ("Mon", 12.0);
    ("Tue", 19.0);
    ("Wed", 8.0);
    ("Thu", 15.0);
    ("Fri", 22.0);
    ("Sat", 10.0);
    ("Sun", 14.0);
  |]

let heatmap_data =
  Array.concat
    (List.map Array.of_list
       (List.init 10 (fun y ->
            List.init 10 (fun x ->
                let value =
                  sin (float_of_int x *. 0.5) *. cos (float_of_int y *. 0.5)
                in
                (float_of_int x, float_of_int y, (value +. 1.0) /. 2.0)))))

let candlestick_data =
  let base = 100.0 in
  Array.init 20 (fun i ->
      let open_ =
        base +. (float_of_int i *. 0.5) +. (Random.float 5.0 -. 2.5)
      in
      let close = open_ +. (Random.float 6.0 -. 3.0) in
      let high = max open_ close +. Random.float 2.0 in
      let low = min open_ close -. Random.float 2.0 in
      Mark.{ time = float_of_int i; open_; high; low; close })

(* Additional styles *)
let header_bg = Ansi.Color.of_rgb 40 60 80
let footer_bg = Ansi.Color.grayscale ~level:3
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let tooltip_style =
  Ansi.Style.make ~fg:Ansi.Color.white ~bg:(Ansi.Color.grayscale ~level:4) ()

let crosshair_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:10) ()
let marker_style = Ansi.Style.make ~fg:Ansi.Color.yellow ~bold:true ()

(* Helper to find nearest point in data *)
let nearest_xy data ~x ~y =
  if Array.length data = 0 then None
  else
    let best = ref None in
    let best_dist = ref Float.infinity in
    Array.iter
      (fun (px, py) ->
        let dist = ((px -. x) ** 2.) +. ((py -. y) ** 2.) in
        if dist < !best_dist then begin
          best_dist := dist;
          best := Some (px, py)
        end)
      data;
    !best

(* Chart drawing functions *)
let draw_line_chart model canvas ~width ~height =
  let chart =
    empty ()
    |> with_frame { margins = (1, 2, 2, 6); inner_padding = 0 }
    |> with_axes
         ~x:(Axis.default |> Axis.with_ticks 8 |> Axis.with_style axis_style)
         ~y:(Axis.default |> Axis.with_ticks 5 |> Axis.with_style axis_style)
    |> with_grid (Grid.default |> Grid.with_style grid_style)
    |> line ~kind:`Braille
         ~style:(Ansi.Style.make ~fg:Ansi.Color.cyan ())
         ~x:fst ~y:snd line_data
  in
  let layout = draw ~view:model.view chart canvas ~width ~height in
  (* Draw interactive overlay if hovering *)
  (match model.hover with
  | Some (px, py) -> (
      match Layout.data_of_px layout ~px ~py with
      | Some (dx, dy) -> (
          match nearest_xy line_data ~x:dx ~y:dy with
          | Some (snap_x, snap_y) ->
              Overlay.crosshair ~style:crosshair_style layout canvas ~x:snap_x
                ~y:snap_y;
              Overlay.marker ~style:marker_style ~glyph:"*" layout canvas
                ~x:snap_x ~y:snap_y;
              Overlay.tooltip ~style:tooltip_style layout canvas ~x:snap_x
                ~y:snap_y
                [
                  Printf.sprintf "x: %.1f" snap_x;
                  Printf.sprintf "y: %.2f" snap_y;
                ]
          | None -> ())
      | None -> ())
  | None -> ());
  layout

let draw_scatter_chart _model canvas ~width ~height =
  let chart =
    empty ()
    |> with_frame { margins = (1, 2, 2, 4); inner_padding = 0 }
    |> with_axes
         ~x:(Axis.default |> Axis.with_ticks 5 |> Axis.with_style axis_style)
         ~y:(Axis.default |> Axis.with_ticks 5 |> Axis.with_style axis_style)
    |> with_grid (Grid.default |> Grid.with_style grid_style)
    |> scatter ~kind:`Braille ~glyph:"."
         ~style:(Ansi.Style.make ~fg:Ansi.Color.green ())
         ~x:fst ~y:snd scatter_data
  in
  draw chart canvas ~width ~height

let draw_bar_chart _model canvas ~width ~height =
  let categories = Array.to_list (Array.map fst bar_data) in
  let chart =
    empty ()
    |> with_frame { margins = (1, 1, 2, 4); inner_padding = 0 }
    |> with_x_scale (Scale.band ~categories ())
    |> with_axes
         ~x:(Axis.default |> Axis.with_style axis_style)
         ~y:(Axis.default |> Axis.with_ticks 5 |> Axis.with_style axis_style)
    |> bars_y
         ~style:(Ansi.Style.make ~fg:Ansi.Color.blue ())
         ~x:fst ~y:snd bar_data
  in
  draw chart canvas ~width ~height

let draw_heatmap_chart _model canvas ~width ~height =
  let chart =
    empty ()
    |> with_frame { margins = (1, 1, 2, 3); inner_padding = 0 }
    |> with_axes
         ~x:(Axis.default |> Axis.with_ticks 5 |> Axis.with_style axis_style)
         ~y:(Axis.default |> Axis.with_ticks 5 |> Axis.with_style axis_style)
    |> heatmap ~auto_value_range:true
         ~x:(fun (x, _, _) -> x)
         ~y:(fun (_, y, _) -> y)
         ~value:(fun (_, _, v) -> v)
         heatmap_data
  in
  draw chart canvas ~width ~height

let draw_candlestick_chart _model canvas ~width ~height =
  let chart =
    empty ()
    |> with_frame { margins = (1, 1, 2, 6); inner_padding = 0 }
    |> with_axes
         ~x:(Axis.default |> Axis.with_ticks 5 |> Axis.with_style axis_style)
         ~y:(Axis.default |> Axis.with_ticks 5 |> Axis.with_style axis_style)
    |> with_grid (Grid.default |> Grid.with_style grid_style)
    |> candles
         ~bullish:(Ansi.Style.make ~fg:Ansi.Color.green ())
         ~bearish:(Ansi.Style.make ~fg:Ansi.Color.red ())
         candlestick_data
  in
  draw chart canvas ~width ~height

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
                  let px, py = (Event.Mouse.x ev, Event.Mouse.y ev) in
                  match Event.Mouse.kind ev with
                  | Move -> Some (Set_hover (Some (px, py)))
                  | Out -> Some (Set_hover None)
                  | Scroll -> (
                      match Event.Mouse.scroll_delta ev with
                      | Some (Scroll_up, _) -> Some (Zoom_at (px, py, `In))
                      | Some (Scroll_down, _) -> Some (Zoom_at (px, py, `Out))
                      | _ -> None)
                  | _ -> None)
                ~on_resize:(fun ~width ~height ->
                  Some (Set_canvas_size (width, height)))
                ~draw:(fun canvas ~width ~height ->
                  let layout = draw_line_chart model canvas ~width ~height in
                  ignore layout)
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
      (* Zoom controls for interactive chart - zoom around center with keyboard *)
      | Char c
        when Uchar.equal c (Uchar.of_char '+') && model.chart = Line_interactive
        ->
          Some (Zoom_center `In)
      | Char c
        when Uchar.equal c (Uchar.of_char '=') && model.chart = Line_interactive
        ->
          Some (Zoom_center `In)
      | Char c
        when Uchar.equal c (Uchar.of_char '-') && model.chart = Line_interactive
        ->
          Some (Zoom_center `Out)
      | Char c
        when Uchar.equal c (Uchar.of_char 'r') && model.chart = Line_interactive
        ->
          Some Reset_zoom
      | _ -> None)

let () =
  Random.init 42;
  run { init; update; view; subscriptions }
