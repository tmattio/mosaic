open Ui

(** Common utilities *)

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
  | [] -> (0.0, 1.0) (* Return default range for empty list *)
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
    ?(axis_style = Style.dim) ?label_style ?series_styles ?(show_grid = false)
    ?(grid_style = Style.dim) ?(render_kind = Lines)
    (data : (string * point list) list) =
  (* Helper to format axis values *)
  let format_value v =
    if abs_float v >= 1000000.0 then Printf.sprintf "%.1e" v
    else if abs_float v >= 1000.0 then Printf.sprintf "%.0fk" (v /. 1000.0)
    else if abs_float v < 0.01 && v <> 0.0 then Printf.sprintf "%.1e" v
    else if abs_float (v -. floor v) < 0.01 then Printf.sprintf "%.0f" v
    else Printf.sprintf "%.1f" v
  in

  (* Calculate data ranges *)
  let all_points = List.flatten (List.map snd data) in
  let x_min, x_max =
    Option.value x_range
      ~default:(min_max (List.map (fun (p : point) -> p.x) all_points))
  in
  let y_min, y_max =
    Option.value y_range
      ~default:(min_max (List.map (fun (p : point) -> p.y) all_points))
  in

  (* Determine if we need space for labels *)
  let needs_y_labels = Option.is_some label_style && show_axes in
  let needs_x_labels = Option.is_some label_style && show_axes in

  (* Calculate label sizes *)
  let y_label_width =
    if needs_y_labels then
      let max_label = format_value y_max in
      let min_label = format_value y_min in
      max (String.length max_label) (String.length min_label) + 1
    else 0
  in

  let x_label_height = if needs_x_labels then 1 else 0 in

  (* Adjust canvas size for labels *)
  let chart_width =
    match width with
    | Some (`Cells w) -> Some (`Cells (max 1 (w - y_label_width)))
    | _ -> width
  in

  let chart_height =
    match height with
    | Some (`Cells h) -> Some (`Cells (max 1 (h - x_label_height)))
    | _ -> height
  in

  (* Create the main chart canvas *)
  let chart =
    Canvas.create ?width:chart_width ?height:chart_height
      (fun ~width:w ~height:h canvas ->
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
        (* Draw grid if requested - data will overwrite it *)
        if show_grid then
          for
            (* Fill entire plot area with grid dots *)
            y = 0 to plot_h - 1
          do
            for x = ox to ox + plot_w - 1 do
              (* Create a regular grid pattern - dot every 2 columns and every row *)
              if (x - ox) mod 2 = 0 then
                Canvas.plot canvas ~x ~y ~style:grid_style "·"
            done
          done;

        (* Draw axes if needed *)
        if show_axes then (
          (* X axis *)
          Canvas.draw_line ~x1:0 ~y1:oy ~x2:(w - 1) ~y2:oy ~style:axis_style
            ~kind:`Line canvas;
          (* Y axis *)
          Canvas.draw_line ~x1:0 ~y1:0 ~x2:0 ~y2:(h - 1) ~style:axis_style
            ~kind:`Line canvas;
          (* Draw corner character at the intersection *)
          Canvas.plot canvas ~x:0 ~y:oy ~style:axis_style "└");
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
                          (scale_value prev.x x_min x_max 0.
                             (float (plot_w - 1)))
                        + ox
                      in
                      let py =
                        int_of_float
                          (scale_value prev.y y_max y_min 0.
                             (float (plot_h - 1)))
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
                      (* Scale to braille resolution (2x wider, 4x taller) *)
                      let px =
                        int_of_float
                          (scale_value prev.x x_min x_max 0.
                             (float ((plot_w - 1) * 2)))
                        + (ox * 2)
                      in
                      let py =
                        int_of_float
                          (scale_value prev.y y_max y_min 0.
                             (float ((plot_h - 1) * 4)))
                      in
                      let x =
                        int_of_float
                          (scale_value p.x x_min x_max 0.
                             (float ((plot_w - 1) * 2)))
                        + (ox * 2)
                      in
                      let y =
                        int_of_float
                          (scale_value p.y y_max y_min 0.
                             (float ((plot_h - 1) * 4)))
                      in
                      Canvas.draw_line ~x1:px ~y1:py ~x2:x ~y2:y ~style
                        ~kind:`Braille canvas;
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
  in

  (* Now compose with labels if needed *)
  if needs_y_labels || needs_x_labels then
    let label_style = Option.value label_style ~default:Style.dim in

    (* Create Y-axis labels *)
    let y_labels =
      if needs_y_labels then
        let max_label = format_value y_max in
        let min_label = format_value y_min in
        (* Create a vbox with labels at top and bottom, using justify_content *)
        let label_col =
          Ui.vbox ~justify_content:`Space_between ~width:(`Cells y_label_width)
            [
              Ui.text ~style:label_style
                (Printf.sprintf "%*s" y_label_width max_label);
              Ui.text ~style:label_style
                (Printf.sprintf "%*s" y_label_width min_label);
            ]
        in
        Some label_col
      else None
    in

    (* Create X-axis labels *)
    let x_labels =
      if needs_x_labels then
        let min_label = format_value x_min in
        let max_label = format_value x_max in
        (* Create an hbox with labels at left and right *)
        let label_row =
          Ui.hbox ~justify_content:`Space_between
            [
              Ui.text ~style:label_style min_label;
              Ui.text ~style:label_style max_label;
            ]
        in
        Some label_row
      else None
    in

    (* Compose the full chart with labels *)
    let chart_with_y_labels =
      match y_labels with
      | Some labels -> Ui.hbox ~gap:(`Cells 0) [ labels; chart ]
      | None -> chart
    in

    match x_labels with
    | Some labels ->
        (* Add padding to align x-labels with chart *)
        let x_label_row =
          if needs_y_labels then
            Ui.hbox ~gap:(`Cells 0)
              [ Ui.text (String.make y_label_width ' '); labels ]
          else labels
        in
        Ui.vbox ~gap:(`Cells 0) [ chart_with_y_labels; x_label_row ]
    | None -> chart_with_y_labels
  else chart

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

let bar ?width ?height ?(orientation = `Vertical) ?max_value ?min_value
    ?bar_width ?gap ?(show_axes = true) ?axis_style ?label_style
    (data : bar list) =
  let _ = label_style in
  (* TODO: Use for bar labels *)
  Canvas.create ?width ?height (fun ~width:w ~height:h canvas ->
      let bar_count = List.length data in
      let bw =
        Option.value bar_width
          ~default:
            (if orientation = `Vertical then max 1 ((w / bar_count) - 1) else 1)
      in
      let g = Option.value gap ~default:1 in

      (* Handle negative values by finding min and max *)
      let all_bar_segments =
        List.flatten (List.map (fun b -> b.segments) data)
      in
      let all_segment_values =
        List.map (fun (s : bar_segment) -> s.value) all_bar_segments
      in
      let has_negative = List.exists (fun v -> v < 0.0) all_segment_values in

      let min_v =
        Option.value min_value
          ~default:
            (if has_negative then
               List.fold_left min 0. all_segment_values -. 0.0001
             else 0.0)
      in
      let max_v =
        Option.value max_value
          ~default:(List.fold_left max 0. all_segment_values +. 0.0001)
      in

      (* Calculate zero line position *)
      let zero_pos =
        if has_negative then
          if orientation = `Vertical then
            int_of_float (scale_value 0.0 min_v max_v 0. (float (h - 1)))
          else int_of_float (scale_value 0.0 min_v max_v 0. (float (w - 1)))
        else if orientation = `Vertical then h - 1
        else 0
      in

      (* Draw axes including zero line if needed *)
      (if show_axes then
         let axis_style = Option.value axis_style ~default:Style.dim in
         if orientation = `Vertical then
           if
             (* Bottom axis or zero line *)
             has_negative
           then (
             (* Draw full axis at bottom *)
             Canvas.draw_line ~x1:0 ~y1:(h - 1) ~x2:(w - 1) ~y2:(h - 1)
               ~style:axis_style ~kind:`Line canvas;
             (* Draw zero line *)
             Canvas.draw_line ~x1:0
               ~y1:(h - 1 - zero_pos)
               ~x2:(w - 1)
               ~y2:(h - 1 - zero_pos)
               ~style:Style.dim ~kind:`Line canvas)
           else
             Canvas.draw_line ~x1:0 ~y1:(h - 1) ~x2:(w - 1) ~y2:(h - 1)
               ~style:axis_style ~kind:`Line canvas
         else if
           (* Left axis or zero line *)
           has_negative
         then (
           Canvas.draw_line ~x1:0 ~y1:0 ~x2:0 ~y2:(h - 1) ~style:axis_style
             ~kind:`Line canvas;
           (* Draw zero line *)
           Canvas.draw_line ~x1:zero_pos ~y1:0 ~x2:zero_pos ~y2:(h - 1)
             ~style:Style.dim ~kind:`Line canvas)
         else (
           Canvas.draw_line ~x1:0 ~y1:0 ~x2:0 ~y2:(h - 1) ~style:axis_style
             ~kind:`Line canvas;
           (* Draw corner character at the intersection *)
           Canvas.plot canvas ~x:0 ~y:(h - 1) ~style:axis_style "└"));
      let pos = ref 0 in
      List.iter
        (fun (b : bar) ->
          (* Check if bar position is within canvas bounds *)
          if
            (orientation = `Vertical && !pos < w)
            || (orientation = `Horizontal && !pos < h)
          then (
            (* Note: stacked bars with negative values not supported yet *)
            let _stacked_pos = ref 0. in
            List.iter
              (fun (seg : bar_segment) ->
                (* Calculate bar position relative to zero line *)
                let is_negative = seg.value < 0.0 in
                let abs_value = abs_float seg.value in

                (* Calculate exact segment size with fractional part *)
                let seg_size_exact =
                  if orientation = `Vertical then
                    abs_value /. (max_v -. min_v) *. float (h - 1)
                  else abs_value /. (max_v -. min_v) *. float (w - 1)
                in
                let seg_size = int_of_float seg_size_exact in
                let seg_size = max 0 seg_size in

                (* Calculate fractional part for partial blocks *)
                let fraction = seg_size_exact -. float seg_size in

                (* Position bars relative to zero line *)
                let x, y =
                  if orientation = `Vertical then
                    if is_negative then (!pos, h - 1 - zero_pos)
                    else (!pos, h - 1 - zero_pos - seg_size)
                  else if is_negative then (zero_pos - seg_size, !pos)
                  else (zero_pos, !pos)
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

                (* Add partial block at the appropriate edge for fractional part *)
                if fraction > 0.125 && seg_size > 0 then
                  let vertical_blocks =
                    [| " "; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |]
                  in
                  let horizontal_blocks =
                    [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]
                  in
                  let frac_idx =
                    int_of_float (fraction *. 8.0) |> min 7 |> max 0
                  in

                  if orientation = `Vertical then (
                    if is_negative && y + bh' < h then
                      for
                        (* Draw partial block at bottom of negative bar *)
                        xi = x to min (x + bw' - 1) (w - 1)
                      do
                        Canvas.plot canvas ~x:xi ~y:(y + bh') ~style:seg.style
                          vertical_blocks.(8 - frac_idx)
                        (* Invert for bottom *)
                      done
                    else if (not is_negative) && y > 0 then
                      for
                        (* Draw partial block at top of positive bar *)
                        xi = x to min (x + bw' - 1) (w - 1)
                      do
                        Canvas.plot canvas ~x:xi ~y:(y - 1) ~style:seg.style
                          vertical_blocks.(frac_idx)
                      done)
                  else if orientation = `Horizontal then
                    if is_negative && x > 0 then
                      for
                        (* Draw partial block at left of negative bar *)
                        yi = y to min (y + bh' - 1) (h - 1)
                      do
                        Canvas.plot canvas ~x:(x - 1) ~y:yi ~style:seg.style
                          horizontal_blocks.(8 - frac_idx)
                        (* Invert for left *)
                      done
                    else if (not is_negative) && x + bw' < w then
                      for
                        (* Draw partial block at right of positive bar *)
                        yi = y to min (y + bh' - 1) (h - 1)
                      do
                        Canvas.plot canvas ~x:(x + bw') ~y:yi ~style:seg.style
                          horizontal_blocks.(frac_idx)
                      done
                (* Note: Removed stacked_pos update as it's not used with negative values *))
              b.segments;
            pos := !pos + bw + g))
        data)

(** Sparkline *)

let sparkline ?width ?range ?style ?(render_kind = `Bars) (data : float list) =
  let w = Option.value width ~default:(List.length data) in
  Canvas.create ~width:(`Cells w) ~height:(`Cells 1)
    (fun ~width:actual_w ~height:_ canvas ->
      let min_v, max_v = Option.value range ~default:(min_max data) in
      List.iteri
        (fun i v ->
          (* Ensure i is within bounds *)
          if i < actual_w then
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
  Canvas.create ?width ?height (fun ~width:w ~height:h canvas ->
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
        let style = Option.value axis_style ~default:Style.dim in
        Canvas.draw_line ~x1:0 ~y1:(h - 1) ~x2:(w - 1) ~y2:(h - 1) ~style
          ~kind:`Line canvas;
        Canvas.draw_line ~x1:0 ~y1:0 ~x2:0 ~y2:(h - 1) ~style ~kind:`Line canvas;
        (* Draw corner character at the intersection *)
        Canvas.plot canvas ~x:0 ~y:(h - 1) ~style "└");
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
          (* Use shading characters for better gradient visualization *)
          let shade_chars = [| " "; "░"; "▒"; "▓"; "█" |] in
          let shade_idx = int_of_float (t *. 4.0) |> min 4 |> max 0 in
          (* Use foreground color with shade characters for better gradients *)
          Canvas.plot canvas ~x ~y ~style:(Style.fg color)
            shade_chars.(shade_idx)
        done
      done)

(** Candlestick *)

let candlestick ?width ?height ?time_range ?y_range ?(show_axes = true)
    ?axis_style ?label_style ?(bullish_style = Style.bg Green)
    ?(bearish_style = Style.bg Red) (data : ohlc_point list) =
  let _ = label_style in
  (* TODO: Use for axis labels *)
  Canvas.create ?width ?height (fun ~width:w ~height:h canvas ->
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
      (* Adjust for axes *)
      let plot_w = if show_axes then w - 1 else w in
      let plot_h = if show_axes then h - 1 else h in
      let ox = if show_axes then 1 else 0 in

      let bar_w = max 3 ((plot_w / List.length sorted) - 1) in
      if show_axes then (
        let style = Option.value axis_style ~default:Style.dim in
        Canvas.draw_line ~x1:0 ~y1:(h - 1) ~x2:(w - 1) ~y2:(h - 1) ~style
          ~kind:`Line canvas;
        Canvas.draw_line ~x1:0 ~y1:0 ~x2:0 ~y2:(h - 1) ~style ~kind:`Line canvas;
        (* Draw corner character at the intersection *)
        Canvas.plot canvas ~x:0 ~y:(h - 1) ~style "└");
      List.iteri
        (fun i p ->
          let px = ox + (i * (bar_w + 1)) in
          let high_y =
            int_of_float
              (scale_value p.high y_max y_min 0. (float (max 0 (plot_h - 1))))
          in
          let low_y =
            int_of_float
              (scale_value p.low y_max y_min 0. (float (max 0 (plot_h - 1))))
          in
          let open_y =
            int_of_float
              (scale_value p.open_ y_max y_min 0. (float (max 0 (plot_h - 1))))
          in
          let close_y =
            int_of_float
              (scale_value p.close y_max y_min 0. (float (max 0 (plot_h - 1))))
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

(** Legend helper *)

let legend items =
  Ui.hbox ~gap:(`Cells 2)
    (List.map
       (fun (label, style) ->
         Ui.hbox ~gap:(`Cells 1)
           [
             (* Draw a marker with the provided style *)
             Ui.text ~style "●● ";
             Ui.text label;
           ])
       items)

(** Line chart with support for data gaps *)

let line_with_gaps ?width ?height ?x_range ?y_range ?(show_axes = true)
    ?axis_style ?label_style ?series_styles ?(show_grid = false)
    ?(grid_style = Style.dim) ?(render_kind = Lines)
    (data : (string * (float * float option) list) list) =
  (* Convert data with gaps to continuous segments *)
  let converted_data =
    List.map
      (fun (name, points_with_gaps) ->
        (* Split into continuous segments where values are Some *)
        let segments = ref [] in
        let current_segment = ref [] in

        List.iter
          (fun (x, y_opt) ->
            match y_opt with
            | Some y -> current_segment := { x; y } :: !current_segment
            | None ->
                if !current_segment <> [] then (
                  segments := List.rev !current_segment :: !segments;
                  current_segment := []))
          points_with_gaps;

        (* Add final segment if any *)
        if !current_segment <> [] then
          segments := List.rev !current_segment :: !segments;

        (* Return all segments as separate series for gaps *)
        List.mapi
          (fun i segment ->
            let segment_name = if i = 0 then name else "" in
            (segment_name, segment))
          (List.rev !segments))
      data
  in

  (* Flatten all series segments *)
  let all_series = List.flatten converted_data in

  (* Use regular line chart with the converted data *)
  line ?width ?height ?x_range ?y_range ~show_axes ?axis_style ?label_style
    ?series_styles ~show_grid ~grid_style ~render_kind all_series
