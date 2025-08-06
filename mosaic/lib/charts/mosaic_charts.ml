open Ui

(** Common utilities *)

let size_to_int_with_default size default =
  match size with
  | Some (`Cells n) -> n
  | Some (`Pct _) | Some `Auto | Some (`Calc _) | None -> default

type point = { x : float; y : float }
type time_series_point = { time : float; value : float }

type ohlc_point = {
  time : float;
  open_ : float;
  high : float;
  low : float;
  close : float;
}

type bar_segment = { value : float; style : Style.t; label : string option }
type bar = { label : string; segments : bar_segment list }
type heat_point = { x : float; y : float; value : float }
type line_render_kind = Lines | Braille | Points of string
type sparkline_render_kind = [ `Bars | `Line | `Braille ]

let min_max lst =
  match lst with
  | [] -> invalid_arg "empty list"
  | hd :: tl ->
      List.fold_left
        (fun (min_v, max_v) v -> (min min_v v, max max_v v))
        (hd, hd) tl

let scale_value value min_v max_v target_min target_max =
  let epsilon = 1e-10 in
  if abs_float (max_v -. min_v) < epsilon then target_min
  else
    let scaled =
      target_min
      +. ((value -. min_v) /. (max_v -. min_v) *. (target_max -. target_min))
    in
    (* Ensure result is within bounds *)
    max target_min (min target_max scaled)

(** Braille line drawing helper *)
let braille_line x1 y1 x2 y2 canvas style =
  (* Braille characters use a 2x4 grid mapped to Unicode points U+2800-U+28FF *)
  let module BrailleBuffer = struct
    type t = { _width : int; _height : int; cells : (int * int, int) Hashtbl.t }

    let create width height =
      {
        _width = (width + 1) / 2;
        (* Braille cells are 2 pixels wide *)
        _height = (height + 3) / 4;
        (* Braille cells are 4 pixels tall *)
        cells = Hashtbl.create 100;
      }

    let set_pixel buffer x y =
      if x >= 0 && y >= 0 then
        let cell_x = x / 2 in
        let cell_y = y / 4 in
        let bit_x = x mod 2 in
        let bit_y = y mod 4 in
        (* Braille dot mapping:
           bit positions in cell:
           0 3
           1 4
           2 5
           6 7 *)
        let bit_pos =
          match (bit_x, bit_y) with
          | 0, 0 -> 0
          | 0, 1 -> 1
          | 0, 2 -> 2
          | 0, 3 -> 6
          | 1, 0 -> 3
          | 1, 1 -> 4
          | 1, 2 -> 5
          | 1, 3 -> 7
          | _ -> 0
        in
        let key = (cell_x, cell_y) in
        let current = try Hashtbl.find buffer.cells key with Not_found -> 0 in
        Hashtbl.replace buffer.cells key (current lor (1 lsl bit_pos))

    let render buffer canvas style =
      Hashtbl.iter
        (fun (x, y) bits ->
          let braille_code = 0x2800 + bits in
          let char = Uchar.of_int braille_code in
          let char_str =
            let buf = Buffer.create 4 in
            Uutf.Buffer.add_utf_8 buf char;
            Buffer.contents buf
          in
          canvas ~x ~y ?style:(Some style) char_str)
        buffer.cells
  end in
  (* Create a buffer for the line *)
  let max_x = max x1 x2 in
  let max_y = max y1 y2 in
  let buffer = BrailleBuffer.create (max_x + 1) (max_y + 1) in

  (* Draw line using Bresenham algorithm *)
  let dx = abs (x2 - x1) in
  let dy = abs (y2 - y1) in
  let sx = if x1 < x2 then 1 else -1 in
  let sy = if y1 < y2 then 1 else -1 in
  let err = ref (dx - dy) in
  let x = ref x1 in
  let y = ref y1 in

  let rec loop () =
    BrailleBuffer.set_pixel buffer !x !y;
    if !x = x2 && !y = y2 then ()
    else
      let e2 = 2 * !err in
      if e2 > -dy then (
        err := !err - dy;
        x := !x + sx);
      if e2 < dx then (
        err := !err + dx;
        y := !y + sy);
      loop ()
  in
  loop ();
  BrailleBuffer.render buffer canvas style

(** Style utilities *)
let calculate_gradient_color colors t =
  let num_colors = List.length colors in
  if num_colors = 0 then Style.Default
  else if num_colors = 1 then List.hd colors
  else
    let segment_size = 1. /. float_of_int (num_colors - 1) in
    let idx = int_of_float (t /. segment_size) in
    let idx = min idx (num_colors - 2) in
    List.nth colors idx

(** Line chart *)

let line ?width ?height ?x_range ?y_range ?(show_axes = true)
    ?(axis_style = Style.dim) ?(label_style = Style.dim) ?series_styles
    ?(render_kind = Lines) (data : (string * point list) list) =
  Canvas.create ?width ?height (fun canvas ->
      let w = size_to_int_with_default width 40 in
      let h = size_to_int_with_default height 10 in
      let series_count = List.length data in
      let styles =
        match series_styles with
        | Some s -> s
        | None ->
            List.init series_count (fun i ->
                Style.fg (Style.Index ((i * 30) + 1)))
      in
      (* Find global min/max *)
      let all_points = List.flatten (List.map snd data) in
      let x_min, x_max =
        Option.value x_range
          ~default:(min_max (List.map (fun (p : point) -> p.x) all_points))
      in
      let y_min, y_max =
        Option.value y_range
          ~default:(min_max (List.map (fun (p : point) -> p.y) all_points))
      in
      (* Adjust for axes *)
      let plot_w = if show_axes then w - 1 else w in
      let plot_h = if show_axes then h - 1 else h in
      let ox = if show_axes then 1 else 0 in
      let oy = if show_axes then h - 1 else h - 1 in
      (* Draw axes if needed *)
      if show_axes then (
        (* X axis *)
        Canvas.draw_line ~x1:0 ~y1:oy ~x2:(w - 1) ~y2:oy ~style:axis_style
          ~kind:`Line canvas;
        (* Y axis *)
        Canvas.draw_line ~x1:0 ~y1:0 ~x2:0 ~y2:(h - 1) ~style:axis_style
          ~kind:`Line canvas;
        (* Stub labels *)
        Canvas.plot canvas ~x:0 ~y:h ~style:label_style
          (Printf.sprintf "%.1f" y_min);
        Canvas.plot canvas ~x:(w / 2) ~y:h ~style:label_style "X");
      (* Plot each series *)
      List.iteri
        (fun i (_, (points : point list)) ->
          let style = List.nth styles i in
          let sorted_points =
            List.sort
              (fun (p1 : point) (p2 : point) -> compare p1.x p2.x)
              points
          in
          match render_kind with
          | Lines -> (
              let rec plot_lines (prev : point) = function
                | [] -> ()
                | (p : point) :: tl ->
                    let px =
                      int_of_float
                        (scale_value prev.x x_min x_max 0. (float (plot_w - 1)))
                      + ox
                    in
                    let py =
                      int_of_float
                        (scale_value prev.y y_max y_min 0. (float (plot_h - 1)))
                    in
                    (* Invert y *)
                    let x =
                      int_of_float
                        (scale_value p.x x_min x_max 0. (float (plot_w - 1)))
                      + ox
                    in
                    let y =
                      int_of_float
                        (scale_value p.y y_max y_min 0. (float (plot_h - 1)))
                    in
                    Canvas.draw_line ~x1:px ~y1:py ~x2:x ~y2:y ~style
                      ~kind:`Line canvas;
                    plot_lines p tl
              in
              match sorted_points with
              | [] | [ _ ] -> ()
              | hd :: tl -> plot_lines hd tl)
          | Braille -> (
              let rec plot_braille (prev : point) = function
                | [] -> ()
                | (p : point) :: tl ->
                    let px =
                      int_of_float
                        (scale_value prev.x x_min x_max 0. (float (plot_w - 1)))
                      + ox
                    in
                    let py =
                      int_of_float
                        (scale_value prev.y y_max y_min 0. (float (plot_h - 1)))
                    in
                    let x =
                      int_of_float
                        (scale_value p.x x_min x_max 0. (float (plot_w - 1)))
                      + ox
                    in
                    let y =
                      int_of_float
                        (scale_value p.y y_max y_min 0. (float (plot_h - 1)))
                    in
                    braille_line px py x y (Canvas.plot canvas) style;
                    plot_braille p tl
              in
              match sorted_points with
              | [] | [ _ ] -> ()
              | hd :: tl -> plot_braille hd tl)
          | Points char ->
              List.iter
                (fun (p : point) ->
                  let x =
                    int_of_float
                      (scale_value p.x x_min x_max 0. (float (plot_w - 1)))
                    + ox
                  in
                  let y =
                    int_of_float
                      (scale_value p.y y_max y_min 0. (float (plot_h - 1)))
                  in
                  Canvas.plot canvas ~x ~y ~style char)
                sorted_points)
        data)

(** Time series *)

let time_series ?width ?height ?time_range ?y_range ?(show_axes = true)
    ?axis_style ?label_style ?(x_label_format = "%Y-%m-%d") ?series_styles
    ?render_kind (data : (string * time_series_point list) list) =
  let _ = x_label_format in
  (* TODO: Implement proper time formatting *)
  (* Convert time series data to point data *)
  let point_data : (string * point list) list =
    List.map
      (fun ((name : string), (pts : time_series_point list)) ->
        ( name,
          List.map
            (fun (p : time_series_point) ->
              ({ x = p.time; y = p.value } : point))
            pts ))
      data
  in
  (* Create line chart with converted data *)
  line ?width ?height ?x_range:time_range ?y_range ~show_axes ?axis_style
    ?label_style ?series_styles ?render_kind point_data

(** Bar chart *)

let bar ?width ?height ?(orientation = `Vertical) ?max_value ?bar_width ?gap
    ?(show_axes = true) ?axis_style ?label_style (data : bar list) =
  let _ = label_style in
  (* TODO: Use for bar labels *)
  Canvas.create ?width ?height (fun canvas ->
      let w = size_to_int_with_default width 40 in
      let h = size_to_int_with_default height 10 in
      let bar_count = List.length data in
      let bw =
        Option.value bar_width
          ~default:
            (if orientation = `Vertical then max 1 ((w / bar_count) - 1) else 1)
      in
      let g = Option.value gap ~default:1 in
      let calc_bar_value (b : bar) =
        List.fold_left
          (fun acc (s : bar_segment) -> acc +. s.value)
          0. b.segments
      in
      let all_values = List.map calc_bar_value data in
      let mv =
        Option.value max_value
          ~default:(List.fold_left max 0. all_values +. 0.0001)
      in
      (* Avoid div0 *)
      if show_axes then
        if orientation = `Vertical then
          Canvas.draw_line ~x1:0 ~y1:(h - 1) ~x2:(w - 1) ~y2:(h - 1)
            ~style:(Option.value axis_style ~default:Style.dim)
            ~kind:`Line canvas
        else
          Canvas.draw_line ~x1:0 ~y1:0 ~x2:0 ~y2:(h - 1)
            ~style:(Option.value axis_style ~default:Style.dim)
            ~kind:`Line canvas;
      let pos = ref 0 in
      List.iter
        (fun (b : bar) ->
          (* Check if bar position is within canvas bounds *)
          if
            (orientation = `Vertical && !pos < w)
            || (orientation = `Horizontal && !pos < h)
          then (
            let stacked_pos = ref 0. in
            List.iter
              (fun (seg : bar_segment) ->
                let seg_size =
                  if orientation = `Vertical then
                    int_of_float (seg.value /. mv *. float (h - 1))
                  else int_of_float (seg.value /. mv *. float (w - 1))
                in
                let seg_size = max 0 seg_size in
                (* Avoid negative *)
                let x, y =
                  if orientation = `Vertical then
                    (!pos, h - 1 - int_of_float !stacked_pos - seg_size)
                  else (int_of_float !stacked_pos, !pos)
                in
                (* Ensure coordinates are within bounds *)
                let x = max 0 x in
                let y = max 0 y in
                let bw', bh' =
                  if orientation = `Vertical then
                    (min bw (w - x), min seg_size (h - y))
                  else (min seg_size (w - x), min bw (h - y))
                in
                if bw' > 0 && bh' > 0 && x >= 0 && y >= 0 then
                  Canvas.draw_box ~x ~y ~width:bw' ~height:bh' ~style:seg.style
                    canvas;
                stacked_pos := !stacked_pos +. float seg_size)
              b.segments;
            pos := !pos + bw + g))
        data)

(** Sparkline *)

let sparkline ?width ?range ?style ?(render_kind = `Bars) (data : float list) =
  let w = Option.value width ~default:(List.length data) in
  Canvas.create ~width:(`Cells w) ~height:(`Cells 1) (fun canvas ->
      let min_v, max_v = Option.value range ~default:(min_max data) in
      List.iteri
        (fun i v ->
          (* Ensure i is within bounds *)
          if i < w then
            let scaled = scale_value v min_v max_v 0. 8. |> int_of_float in
            let char =
              match render_kind with
              | `Bars ->
                  List.nth
                    [ " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" ]
                    (min scaled 8)
              | `Line ->
                  if scaled > 4 then "¯" else if scaled < 4 then "_" else "-"
              | `Braille ->
                  List.nth
                    [ " "; "⠁"; "⠃"; "⠇"; "⠏"; "⠟"; "⠿"; "⡿"; "⣿" ]
                    (min scaled 8)
            in
            Canvas.plot canvas ~x:i ~y:0
              ~style:(Option.value style ~default:Style.empty)
              char)
        data)

(** Heatmap *)

let heatmap ?width ?height ?x_range ?y_range ?value_range ?color_scale
    ?(show_axes = true) ?axis_style ?label_style (data : heat_point list) =
  let _ = label_style in
  (* TODO: Use for axis labels *)
  let default_scale = [ Style.Index 232; Style.Index 255 ] in
  (* Black to white *)
  let color_scale =
    match color_scale with None | Some [] -> default_scale | Some cs -> cs
  in
  Canvas.create ?width ?height (fun canvas ->
      let w = size_to_int_with_default width 40 in
      let h = size_to_int_with_default height 10 in
      let x_min, x_max =
        Option.value x_range ~default:(min_max (List.map (fun p -> p.x) data))
      in
      let y_min, y_max =
        Option.value y_range ~default:(min_max (List.map (fun p -> p.y) data))
      in
      let v_min, v_max =
        Option.value value_range
          ~default:(min_max (List.map (fun p -> p.value) data))
      in
      if show_axes then (
        Canvas.draw_line ~x1:0 ~y1:(h - 1) ~x2:(w - 1) ~y2:(h - 1)
          ~style:(Option.value axis_style ~default:Style.dim)
          ~kind:`Line canvas;
        Canvas.draw_line ~x1:0 ~y1:0 ~x2:0 ~y2:(h - 1)
          ~style:(Option.value axis_style ~default:Style.dim)
          ~kind:`Line canvas);
      (* Create a grid for interpolation *)
      let grid = Array.make_matrix h w Option.None in

      (* Place data points in grid *)
      List.iter
        (fun p ->
          let px =
            int_of_float
              (scale_value p.x x_min x_max 0. (float (max 0 (w - 1))))
          in
          let py =
            int_of_float
              (scale_value p.y y_max y_min 0. (float (max 0 (h - 1))))
          in
          if px >= 0 && px < w && py >= 0 && py < h then
            grid.(py).(px) <- Option.Some p.value)
        data;

      (* Fill grid with interpolated values *)
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          let value =
            match grid.(y).(x) with
            | Option.Some v -> v
            | Option.None ->
                (* Find nearest neighbors for interpolation *)
                let rec find_nearest max_dist =
                  if max_dist > max w h then v_min
                  else
                    let neighbors = ref [] in
                    for dy = -max_dist to max_dist do
                      for dx = -max_dist to max_dist do
                        if abs dy = max_dist || abs dx = max_dist then
                          let nx, ny = (x + dx, y + dy) in
                          if nx >= 0 && nx < w && ny >= 0 && ny < h then
                            match grid.(ny).(nx) with
                            | Option.Some v ->
                                let dist = float ((dx * dx) + (dy * dy)) in
                                neighbors := (v, dist) :: !neighbors
                            | Option.None -> ()
                      done
                    done;
                    if !neighbors = [] then find_nearest (max_dist + 1)
                    else
                      (* Weighted average by inverse distance *)
                      let total_weight = ref 0. in
                      let sum = ref 0. in
                      List.iter
                        (fun (v, d) ->
                          let weight = 1. /. (d +. 1.) in
                          total_weight := !total_weight +. weight;
                          sum := !sum +. (v *. weight))
                        !neighbors;
                      !sum /. !total_weight
                in
                find_nearest 1
          in
          let t = scale_value value v_min v_max 0. 1. in
          let color = calculate_gradient_color color_scale t in
          Canvas.draw_box ~x ~y ~width:1 ~height:1 ~style:(Style.bg color)
            canvas
        done
      done)

(** Candlestick *)

let candlestick ?width ?height ?time_range ?y_range ?(show_axes = true)
    ?axis_style ?label_style ?(bullish_style = Style.bg Green)
    ?(bearish_style = Style.bg Red) (data : ohlc_point list) =
  let _ = label_style in
  (* TODO: Use for axis labels *)
  Canvas.create ?width ?height (fun canvas ->
      let w = size_to_int_with_default width 40 in
      let h = size_to_int_with_default height 10 in
      let sorted = List.sort (fun p1 p2 -> compare p1.time p2.time) data in
      let _t_min, _t_max =
        Option.value time_range
          ~default:(min_max (List.map (fun p -> p.time) sorted))
      in
      (* TODO: Use time range for proper x-axis scaling instead of index-based positioning *)
      let all_y =
        List.flatten
          (List.map (fun p -> [ p.open_; p.high; p.low; p.close ]) sorted)
      in
      let y_min, y_max = Option.value y_range ~default:(min_max all_y) in
      let bar_w = max 1 ((w / List.length sorted) - 1) in
      if show_axes then (
        Canvas.draw_line ~x1:0 ~y1:(h - 1) ~x2:(w - 1) ~y2:(h - 1)
          ~style:(Option.value axis_style ~default:Style.dim)
          ~kind:`Line canvas;
        Canvas.draw_line ~x1:0 ~y1:0 ~x2:0 ~y2:(h - 1)
          ~style:(Option.value axis_style ~default:Style.dim)
          ~kind:`Line canvas);
      List.iteri
        (fun i p ->
          let px = i * (bar_w + 1) in
          let high_y =
            int_of_float
              (scale_value p.high y_max y_min 0. (float (max 0 (h - 1))))
          in
          let low_y =
            int_of_float
              (scale_value p.low y_max y_min 0. (float (max 0 (h - 1))))
          in
          let open_y =
            int_of_float
              (scale_value p.open_ y_max y_min 0. (float (max 0 (h - 1))))
          in
          let close_y =
            int_of_float
              (scale_value p.close y_max y_min 0. (float (max 0 (h - 1))))
          in
          let body_h = max 1 (abs (open_y - close_y) + 1) in
          (* Min height 1 for doji *)
          let body_y = min open_y close_y in
          let style =
            if p.close > p.open_ then bullish_style else bearish_style
          in
          (* Wick *)
          Canvas.draw_line
            ~x1:(px + (bar_w / 2))
            ~y1:high_y
            ~x2:(px + (bar_w / 2))
            ~y2:low_y ~style ~kind:`Line canvas;
          (* Body *)
          (* Add border for better visibility, especially for thin bars *)
          let border_color =
            if p.close > p.open_ then Style.Green else Style.Red
          in
          let candle_border =
            Ui.Border.make ~line_style:Solid ~color:border_color ()
          in
          Canvas.draw_box ~x:px ~y:body_y ~width:bar_w ~height:body_h ~style
            ~border:candle_border canvas)
        sorted)
