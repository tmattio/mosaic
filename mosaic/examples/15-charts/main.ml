(** Interactive charts demo: zoom, pan, hover tooltips for multiple chart types.
*)

open Mosaic_tea
open Mosaic_charts
module Canvas = Mosaic_ui.Canvas
module Event = Mosaic_ui.Event

(* --- Chart types --- *)

type chart_type = Line | Scatter | Bar | Heatmap | Candlestick

let all_charts = [ Line; Scatter; Bar; Heatmap; Candlestick ]

let chart_name = function
  | Line -> "Line"
  | Scatter -> "Scatter"
  | Bar -> "Bar"
  | Heatmap -> "Heatmap"
  | Candlestick -> "Candlestick"

let chart_index (c : chart_type) : int =
  let rec find i = function
    | [] -> 0
    | x :: tl -> if x = c then i else find (i + 1) tl
  in
  find 0 all_charts

let chart_of_index (i : int) : chart_type =
  let n = List.length all_charts in
  let i = if n <= 0 then 0 else ((i mod n) + n) mod n in
  match List.nth_opt all_charts i with Some c -> c | None -> Line

let next_chart c = chart_of_index (chart_index c + 1)
let prev_chart c = chart_of_index (chart_index c - 1)

(* --- Sample data --- *)

let line_data =
  Array.init 80 (fun i ->
      let x = float_of_int i in
      let y = (sin (x *. 0.18) *. 3.0) +. (cos (x *. 0.07) *. 2.2) +. 6.0 in
      (x, y))

let scatter_data =
  Array.init 160 (fun i ->
      let x = float_of_int (i mod 20) +. (Random.float 0.6 -. 0.3) in
      let y = float_of_int (i / 20) +. (Random.float 0.6 -. 0.3) in
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
       (List.init 14 (fun y ->
            List.init 18 (fun x ->
                let xf = float_of_int x and yf = float_of_int y in
                let value =
                  (sin (xf *. 0.35) *. cos (yf *. 0.32))
                  +. (0.35 *. sin ((xf +. yf) *. 0.2))
                in
                let v01 = (value +. 2.0) /. 4.0 |> max 0.0 |> min 1.0 in
                (xf, yf, v01)))))

let candlestick_data =
  let base = 100.0 in
  Array.init 40 (fun i ->
      let open_ =
        base +. (float_of_int i *. 0.35) +. (Random.float 5.0 -. 2.5)
      in
      let close = open_ +. (Random.float 7.0 -. 3.5) in
      let high = max open_ close +. Random.float 2.4 in
      let low = min open_ close -. Random.float 2.4 in
      Mark.{ time = float_of_int i; open_; high; low; close })

(* --- UI styles (for header/footer only) --- *)

let header_bg = Ansi.Color.of_rgb 40 60 80
let footer_bg = Ansi.Color.grayscale ~level:3
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let emph = Ansi.Style.make ~bold:true ()

(* --- Interaction model --- *)

type theme_mode = [ `Dark | `Light ]
type drag = { start_px : int; start_py : int; start_view : View.t }

type model = {
  chart : chart_type;
  (* per-chart view persistence *)
  views : View.t array;
  canvas_size : (int * int) option;
  hover : (int * int) option;
  dragging : drag option;
  show_grid : bool;
  theme : theme_mode;
  show_help : bool;
  heatmap_render : Mark.heatmap_render;
}

type msg =
  | Next_chart
  | Prev_chart
  | Canvas_resized of int * int
  | Pointer_move of int * int
  | Pointer_leave
  | Pointer_down of int * int
  | Pointer_up
  | Scroll_zoom of int * int * [ `In | `Out ]
  | Zoom_center of [ `In | `Out ]
  | Pan_by_keys of int * int
  | Reset_view
  | Reset_all_views
  | Toggle_grid
  | Toggle_theme
  | Toggle_help
  | Cycle_heatmap_render
  | Quit

let init () =
  let n = List.length all_charts |> max 1 in
  ( {
      chart = Line;
      views = Array.init n (fun _ -> View.empty);
      canvas_size = None;
      hover = None;
      dragging = None;
      show_grid = true;
      theme = `Dark;
      show_help = false;
      heatmap_render = Mark.Cells;
    },
    Cmd.none )

let get_view (m : model) : View.t =
  let i = chart_index m.chart in
  if i >= 0 && i < Array.length m.views then m.views.(i) else View.empty

let set_view (m : model) (v : View.t) : model =
  let i = chart_index m.chart in
  if i < 0 || i >= Array.length m.views then m
  else
    let views = Array.copy m.views in
    views.(i) <- v;
    { m with views }

let reset_all_views (m : model) : model =
  let views = Array.map (fun _ -> View.empty) m.views in
  { m with views }

let theme_of_model (m : model) : Theme.t =
  match m.theme with `Dark -> Theme.dark | `Light -> Theme.light

let grid_of_model (m : model) : Grid.t =
  if not m.show_grid then Grid.hidden
  else
    (* Set style to Style.default so the theme can supply grid style. *)
    Grid.default |> Grid.with_style Ansi.Style.default

(* --- Chart specifications (pure) --- *)

let spec_for (m : model) (ct : chart_type) : Mosaic_charts.t =
  let theme = theme_of_model m in
  match ct with
  | Line ->
      empty ~theme ()
      |> with_frame { margins = (1, 2, 2, 6); inner_padding = 0 }
      |> with_axes
           ~x:(Axis.default |> Axis.with_ticks 8)
           ~y:(Axis.default |> Axis.with_ticks 6)
      |> with_grid (grid_of_model m)
      (* line + points makes hit-testing nicer *)
      |> line ~id:"line" ~kind:`Braille ~x:fst ~y:snd line_data
      |> scatter ~id:"points" ~glyph:"∙" ~kind:`Cell ~x:fst ~y:snd line_data
  | Scatter ->
      empty ~theme ()
      |> with_frame { margins = (1, 2, 2, 5); inner_padding = 0 }
      |> with_axes
           ~x:(Axis.default |> Axis.with_ticks 6)
           ~y:(Axis.default |> Axis.with_ticks 6)
      |> with_grid (grid_of_model m)
      |> scatter ~id:"scatter" ~kind:`Braille ~glyph:"·" ~x:fst ~y:snd
           scatter_data
  | Bar ->
      let categories = Array.to_list (Array.map fst bar_data) in
      empty ~theme ()
      |> with_frame { margins = (1, 2, 2, 6); inner_padding = 0 }
      |> with_x_scale (Scale.band ~categories ~padding:0.18 ())
      |> with_axes
           ~x:(Axis.default |> Axis.with_ticks 0)
             (* band labels are categories *)
           ~y:(Axis.default |> Axis.with_ticks 6)
      |> with_grid (grid_of_model m)
      |> bars_y ~id:"bars" ~x:fst ~y:snd bar_data
      |> rule_y 0.0
  | Heatmap ->
      empty ~theme ()
      |> with_frame { margins = (1, 2, 2, 5); inner_padding = 0 }
      |> with_axes
           ~x:(Axis.default |> Axis.with_ticks 6)
           ~y:(Axis.default |> Axis.with_ticks 6)
      |> with_grid (if m.show_grid then grid_of_model m else Grid.hidden)
      |> heatmap ~id:"heat" ~auto_value_range:true ~render:m.heatmap_render
           ~x:(fun (x, _, _) -> x)
           ~y:(fun (_, y, _) -> y)
           ~value:(fun (_, _, v) -> v)
           heatmap_data
  | Candlestick ->
      empty ~theme ()
      |> with_frame { margins = (1, 2, 2, 6); inner_padding = 0 }
      |> with_axes
           ~x:(Axis.default |> Axis.with_ticks 7)
           ~y:(Axis.default |> Axis.with_ticks 6)
      |> with_grid (grid_of_model m)
      |> candles ~id:"ohlc" candlestick_data

(* --- Layout helpers used in update (interaction) --- *)

let with_layout_for (m : model) ~(view : View.t) (f : Layout.t -> model * 'a) :
    (model * 'a) option =
  match m.canvas_size with
  | None -> None
  | Some (w, h) ->
      let chart = spec_for m m.chart in
      let layout = Mosaic_charts.layout ~view chart ~width:w ~height:h in
      Some (f layout)

let plot_center_px (layout : Layout.t) : int * int =
  let r = Layout.plot_rect layout in
  (r.x + (max 0 (r.width - 1) / 2), r.y + (max 0 (r.height - 1) / 2))

let clamp_view (layout : Layout.t) (v : View.t) : View.t =
  Layout.clamp_view layout v

(* --- Hit/tooltip formatting --- *)

let string_of_hit_kind : Hit.kind -> string = function
  | `Line -> "line"
  | `Scatter -> "scatter"
  | `Bars -> "bars"
  | `Stacked_bars -> "stacked bars"
  | `Heatmap -> "heatmap"
  | `Candles -> "candles"
  | `Circle -> "circle"

let hit_params (ct : chart_type) : int * Hit.policy =
  match ct with
  | Line -> (4, `Nearest_x)
  | Scatter -> (4, `Nearest_px)
  | Bar -> (2, `Nearest_px)
  | Heatmap -> (3, `Nearest_px)
  | Candlestick -> (2, `Nearest_x)

let x_center_for_category (layout : Layout.t) (category : string) : float =
  let fallback () =
    let w = Layout.x_view layout in
    (w.min +. w.max) /. 2.0
  in
  match Layout.px_of_x_category layout category with
  | None -> fallback ()
  | Some px -> (
      let r = Layout.plot_rect layout in
      let py = r.y + (max 0 (r.height - 1) / 2) in
      match Layout.data_of_px layout ~px ~py with
      | None -> fallback ()
      | Some (x, _) -> x)

let cursor_readout_lines (layout : Layout.t) ~(px : int) ~(py : int) :
    string list =
  match Layout.data_of_px layout ~px ~py with
  | None -> []
  | Some (x, y) ->
      let base = [ Printf.sprintf "x: %.3g" x; Printf.sprintf "y: %.3g" y ] in
      let extra =
        match Layout.x_category_of_px layout ~px with
        | None -> []
        | Some cat -> [ "category: " ^ cat ]
      in
      extra @ base

let hit_tooltip (layout : Layout.t) (hit : Hit.t) : float * float * string list
    =
  let header =
    let mark =
      match hit.mark_id with None -> "" | Some id -> Printf.sprintf " (%s)" id
    in
    Printf.sprintf "%s%s" (string_of_hit_kind hit.kind) mark
  in
  match hit.payload with
  | Hit.XY { x; y } ->
      ( x,
        y,
        [
          header;
          Printf.sprintf "x: %.3g" x;
          Printf.sprintf "y: %.3g" y;
          Printf.sprintf "index: %d" hit.index;
        ] )
  | Hit.Bar { category; value } ->
      let x = x_center_for_category layout category in
      let y = value in
      ( x,
        y,
        [ header; "category: " ^ category; Printf.sprintf "value: %.3g" value ]
      )
  | Hit.Stacked_bar { category; segment_index; value; total } ->
      let x = x_center_for_category layout category in
      let y = total in
      ( x,
        y,
        [
          header;
          "category: " ^ category;
          Printf.sprintf "segment: %d" segment_index;
          Printf.sprintf "value: %.3g (of %.3g)" value total;
        ] )
  | Hit.Heat { x; y; value } ->
      ( x,
        y,
        [
          header;
          Printf.sprintf "x: %.3g" x;
          Printf.sprintf "y: %.3g" y;
          Printf.sprintf "value: %.3g" value;
        ] )
  | Hit.OHLC { time; open_; high; low; close } ->
      let x = time and y = close in
      ( x,
        y,
        [
          header;
          Printf.sprintf "t: %.3g" time;
          Printf.sprintf "O: %.3g" open_;
          Printf.sprintf "H: %.3g" high;
          Printf.sprintf "L: %.3g" low;
          Printf.sprintf "C: %.3g" close;
        ] )

(* --- Update (interaction state machine) --- *)

let cycle_heatmap_render = function
  | Mark.Cells -> Mark.Dense_bilinear
  | Mark.Dense_bilinear -> Mark.Shaded
  | Mark.Shaded -> Mark.Cells

let update (msg : msg) (m : model) =
  match msg with
  | Next_chart ->
      ( { m with chart = next_chart m.chart; hover = None; dragging = None },
        Cmd.none )
  | Prev_chart ->
      ( { m with chart = prev_chart m.chart; hover = None; dragging = None },
        Cmd.none )
  | Canvas_resized (w, h) ->
      (* Store size; drop drag (canvas geometry changed) *)
      ({ m with canvas_size = Some (w, h); dragging = None }, Cmd.none)
  | Pointer_leave -> ({ m with hover = None; dragging = None }, Cmd.none)
  | Pointer_up -> ({ m with dragging = None }, Cmd.none)
  | Pointer_down (px, py) -> (
      let view = get_view m in
      match
        with_layout_for m ~view (fun layout ->
            if Layout.is_inside_plot layout ~px ~py then
              ( {
                  m with
                  hover = Some (px, py);
                  dragging =
                    Some { start_px = px; start_py = py; start_view = view };
                },
                () )
            else ({ m with hover = Some (px, py); dragging = None }, ()))
      with
      | None -> ({ m with hover = Some (px, py) }, Cmd.none)
      | Some (m', ()) -> (m', Cmd.none))
  | Pointer_move (px, py) -> (
      match m.dragging with
      | None -> ({ m with hover = Some (px, py) }, Cmd.none)
      | Some d -> (
          (* Pan relative to drag start. Use layout compiled at drag.start_view
             to keep the mapping stable even while the view changes. *)
          match
            with_layout_for m ~view:d.start_view (fun layout ->
                let dx = d.start_px - px in
                let dy = py - d.start_py in
                let view' =
                  Layout.pan_view_by_px layout ~view:d.start_view ~dx ~dy
                  |> clamp_view layout
                in
                let m' = set_view m view' in
                ({ m' with hover = Some (px, py); dragging = Some d }, ()))
          with
          | None -> ({ m with hover = Some (px, py) }, Cmd.none)
          | Some (m', ()) -> (m', Cmd.none)))
  | Scroll_zoom (px, py, dir) -> (
      let factor = match dir with `In -> 1.25 | `Out -> 0.8 in
      let view = get_view m in
      match
        with_layout_for m ~view (fun layout ->
            (* If cursor isn't inside plot, zoom around plot center. *)
            let px, py =
              if Layout.is_inside_plot layout ~px ~py then (px, py)
              else plot_center_px layout
            in
            let view' =
              Layout.zoom_view_around_px layout ~view ~axis:`Both ~px ~py
                ~factor
              |> clamp_view layout
            in
            (set_view m view', ()))
      with
      | None -> (m, Cmd.none)
      | Some (m', ()) -> (m', Cmd.none))
  | Zoom_center dir -> (
      let factor = match dir with `In -> 1.25 | `Out -> 0.8 in
      let view = get_view m in
      match
        with_layout_for m ~view (fun layout ->
            let px, py = plot_center_px layout in
            let view' =
              Layout.zoom_view_around_px layout ~view ~axis:`Both ~px ~py
                ~factor
              |> clamp_view layout
            in
            (set_view m view', ()))
      with
      | None -> (m, Cmd.none)
      | Some (m', ()) -> (m', Cmd.none))
  | Pan_by_keys (dx, dy) -> (
      let view = get_view m in
      match
        with_layout_for m ~view (fun layout ->
            let view' =
              Layout.pan_view_by_px layout ~view ~dx ~dy |> clamp_view layout
            in
            (set_view m view', ()))
      with
      | None -> (m, Cmd.none)
      | Some (m', ()) -> (m', Cmd.none))
  | Reset_view -> (set_view m View.empty, Cmd.none)
  | Reset_all_views -> (reset_all_views m, Cmd.none)
  | Toggle_grid -> ({ m with show_grid = not m.show_grid }, Cmd.none)
  | Toggle_theme ->
      let theme = match m.theme with `Dark -> `Light | `Light -> `Dark in
      ({ m with theme }, Cmd.none)
  | Toggle_help -> ({ m with show_help = not m.show_help }, Cmd.none)
  | Cycle_heatmap_render ->
      if m.chart <> Heatmap then (m, Cmd.none)
      else
        ( { m with heatmap_render = cycle_heatmap_render m.heatmap_render },
          Cmd.none )
  | Quit -> (m, Cmd.quit)

(* --- Drawing overlays (hover + help) --- *)

let draw_hover_overlay (m : model) (layout : Layout.t) (canvas : Canvas.t) =
  match m.hover with
  | None -> ()
  | Some (px, py) -> (
      if not (Layout.is_inside_plot layout ~px ~py) then ()
      else
        let radius, policy = hit_params m.chart in
        match Layout.hit_test layout ~px ~py ~radius ~policy with
        | Some hit ->
            let x, y, lines = hit_tooltip layout hit in
            Overlay.crosshair layout canvas ~x ~y;
            Overlay.marker layout canvas ~x ~y;
            Overlay.tooltip layout canvas ~x ~y lines
        | None -> (
            (* Free cursor readout when not near a mark *)
            match Layout.data_of_px layout ~px ~py with
            | None -> ()
            | Some (x, y) ->
                let lines = cursor_readout_lines layout ~px ~py in
                Overlay.crosshair layout canvas ~x ~y;
                Overlay.tooltip ~anchor:`Right layout canvas ~x ~y lines))

let draw_help_overlay (m : model) (layout : Layout.t) (canvas : Canvas.t) =
  if not m.show_help then ()
  else
    let r = Layout.plot_rect layout in
    let px = r.x + (max 0 (r.width - 1) / 2) in
    let py = r.y + (max 0 (r.height - 1) / 2) in
    match Layout.data_of_px layout ~px ~py with
    | None -> ()
    | Some (x, y) ->
        let heatmap_line =
          if m.chart = Heatmap then
            [ "m: cycle heatmap render (Cells / Dense / Shaded)" ]
          else []
        in
        let lines =
          [
            "Controls";
            "────────";
            "Mouse wheel: zoom at cursor";
            "Drag: pan";
            "+ / -: zoom at center";
            "Arrows: pan";
            "r: reset current view";
            "0: reset all views";
            "Tab / Shift+Tab or < / >: switch chart";
            "g: toggle grid";
            "t: toggle theme";
            "h: toggle this help";
            "q / Esc: quit";
          ]
          @ heatmap_line
        in
        Overlay.tooltip ~anchor:`Top layout canvas ~x ~y lines

let draw_chart (m : model) (canvas : Canvas.t) ~(width : int) ~(height : int) :
    Layout.t =
  let chart = spec_for m m.chart in
  let view = get_view m in
  let layout = Mosaic_charts.draw ~view chart canvas ~width ~height in
  draw_hover_overlay m layout canvas;
  draw_help_overlay m layout canvas;
  layout

(* --- View --- *)

let view (m : model) =
  let idx = chart_index m.chart in
  let n = List.length all_charts in
  let view_to_s = function
    | None -> "auto"
    | Some (w : View.window) -> Printf.sprintf "%.3g..%.3g" w.min w.max
  in
  let v = get_view m in
  let status =
    Printf.sprintf "theme:%s  grid:%s  view[x=%s y=%s]%s"
      (match m.theme with `Dark -> "dark" | `Light -> "light")
      (if m.show_grid then "on" else "off")
      (view_to_s v.x) (view_to_s v.y)
      (match m.dragging with None -> "" | Some _ -> "  (dragging)")
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
              text ~text_style:emph "Charts (interactive)";
              text ~text_style:muted
                (Printf.sprintf "[%d/%d] %s" (idx + 1) n (chart_name m.chart));
            ];
        ];
      (* Chart area *)
      box ~flex_grow:1. ~padding:(padding 1)
        [
          canvas
            ~on_resize:(fun ~width ~height ->
              Some (Canvas_resized (width, height)))
            ~on_mouse:(fun ev ->
              let px, py = (Event.Mouse.x ev, Event.Mouse.y ev) in
              match Event.Mouse.kind ev with
              | Move | Drag -> Some (Pointer_move (px, py))
              | Out -> Some Pointer_leave
              | Down -> Some (Pointer_down (px, py))
              | Up | Drag_end -> Some Pointer_up
              | Scroll -> (
                  match Event.Mouse.scroll_delta ev with
                  | Some (Scroll_up, _) -> Some (Scroll_zoom (px, py, `In))
                  | Some (Scroll_down, _) -> Some (Scroll_zoom (px, py, `Out))
                  | _ -> None)
              | _ -> None)
            ~draw:(fun canvas ~width ~height ->
              ignore (draw_chart m canvas ~width ~height))
            ~size:{ width = pct 100; height = pct 100 }
            ();
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg ~flex_direction:Column
        [
          text ~text_style:hint
            "Tab/Shift+Tab or </>: switch  |  wheel: zoom  |  drag: pan  |  \
             +/-: zoom center  |  r reset  |  0 reset all  |  g grid  |  t \
             theme  |  h help  |  q quit";
          text ~text_style:muted status;
        ];
    ]

(* --- Subscriptions --- *)

let subscriptions (_m : model) =
  Sub.on_key (fun ev ->
      let data = Event.Key.data ev in
      let step = if data.modifier.shift then 18 else 8 in
      match data.key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      (* Chart switching *)
      | Tab when not data.modifier.shift -> Some Next_chart
      | Tab when data.modifier.shift -> Some Prev_chart
      | Char c when Uchar.equal c (Uchar.of_char '>') -> Some Next_chart
      | Char c when Uchar.equal c (Uchar.of_char '<') -> Some Prev_chart
      | Char c when Uchar.equal c (Uchar.of_char 'n') -> Some Next_chart
      | Char c when Uchar.equal c (Uchar.of_char 'p') -> Some Prev_chart
      (* Zoom *)
      | Char c when Uchar.equal c (Uchar.of_char '+') -> Some (Zoom_center `In)
      | Char c when Uchar.equal c (Uchar.of_char '=') -> Some (Zoom_center `In)
      | Char c when Uchar.equal c (Uchar.of_char '-') -> Some (Zoom_center `Out)
      (* Pan (grab semantics) *)
      | Left -> Some (Pan_by_keys (step, 0)) (* move content left *)
      | Right -> Some (Pan_by_keys (-step, 0)) (* move content right *)
      | Up -> Some (Pan_by_keys (0, -step)) (* move content up *)
      | Down -> Some (Pan_by_keys (0, step)) (* move content down *)
      (* Reset *)
      | Char c when Uchar.equal c (Uchar.of_char 'r') -> Some Reset_view
      | Char c when Uchar.equal c (Uchar.of_char '0') -> Some Reset_all_views
      (* Toggles *)
      | Char c when Uchar.equal c (Uchar.of_char 'g') -> Some Toggle_grid
      | Char c when Uchar.equal c (Uchar.of_char 't') -> Some Toggle_theme
      | Char c when Uchar.equal c (Uchar.of_char 'h') -> Some Toggle_help
      (* Heatmap mode *)
      | Char c when Uchar.equal c (Uchar.of_char 'm') ->
          Some Cycle_heatmap_render
      | _ -> None)

let () =
  Random.init 42;
  run { init; update; view; subscriptions }
