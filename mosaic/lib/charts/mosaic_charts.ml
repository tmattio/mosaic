module Style = Ansi.Style
module Canvas = Mosaic_ui.Canvas

(* Bring in the core plotting engine and streaming sparkline widget.
   If your files are named differently, update these aliases. *)
module Plot = Plot
module Sparkline = Sparkline

(* Small numeric helpers *)

let min_max lst =
  match lst with
  | [] -> (0., 1.)
  | hd :: tl ->
      List.fold_left
        (fun (mn, mx) v -> (Float.min mn v, Float.max mx v))
        (hd, hd) tl

let scale value ~domain:(dmin, dmax) ~range:(rmin, rmax) =
  let eps = 1e-12 in
  if Float.abs (dmax -. dmin) < eps then rmin
  else
    let t = (value -. dmin) /. (dmax -. dmin) in
    let v = rmin +. (t *. (rmax -. rmin)) in
    if v < rmin then rmin else if v > rmax then rmax else v

(* Stateless sparks + streaming Sparkline wrapper *)

type spark_kind = [ `Bars | `Line | `Braille ]

(* Convenience: one-shot sparkline.
   - For `Bars` / `Braille`, we use the stateful Sparkline widget under the hood.
   - For `Line`, we keep the original 1-row slope-based implementation. *)
let sparkline ?style ?(kind = `Bars) values =
 fun canvas ~width ~height ->
  match kind with
  | `Line -> (
      let st = Option.value ~default:Style.default style in
      match values with
      | [] | [ _ ] -> ()
      | _ ->
          let points = List.mapi (fun i v -> (float i, v)) values in
          let xs = List.map fst points and ys = List.map snd points in
          let xmin, xmax = min_max xs and ymin, ymax = min_max ys in
          let x_to_px v =
            int_of_float
              (scale v ~domain:(xmin, xmax)
                 ~range:(0., float (max 1 (width - 1))))
          in
          let y_to_px v =
            int_of_float
              (scale v ~domain:(ymax, ymin)
                 ~range:(0., float (max 1 (height - 1))))
          in
          let rec loop = function
            | (x1, y1) :: (x2, y2) :: tl ->
                Canvas.draw_line canvas ~x1:(x_to_px x1) ~y1:(y_to_px y1)
                  ~x2:(x_to_px x2) ~y2:(y_to_px y2) ~style:st ~kind:`Line ();
                loop ((x2, y2) :: tl)
            | _ -> ()
          in
          loop points)
  | (`Bars | `Braille) as k ->
      let t = Sparkline.make ~width ~height ?style () in
      Sparkline.push_all t values;
      Sparkline.draw t ~kind:k canvas ~width ~height

(* Simple line helper *)

let line ?(style = Style.default) points =
 fun canvas ~width ~height ->
  match points with
  | [] | [ _ ] -> ()
  | _ ->
      let xs = List.map fst points and ys = List.map snd points in
      let xmin, xmax = min_max xs and ymin, ymax = min_max ys in
      let x_to_px v =
        int_of_float
          (scale v ~domain:(xmin, xmax) ~range:(0., float (max 1 (width - 1))))
      in
      let y_to_px v =
        int_of_float
          (scale v ~domain:(ymax, ymin) ~range:(0., float (max 1 (height - 1))))
      in
      let rec loop = function
        | (x1, y1) :: (x2, y2) :: tl ->
            Canvas.draw_line canvas ~x1:(x_to_px x1) ~y1:(y_to_px y1)
              ~x2:(x_to_px x2) ~y2:(y_to_px y2) ~style ~kind:`Line ();
            loop ((x2, y2) :: tl)
        | _ -> ()
      in
      loop points

(* Time series helpers *)

type time_series_point = { time : float; value : float }

let time_series ?styles (data : (string * time_series_point list) list) =
 fun canvas ~width ~height ->
  let plot =
    let base = Plot.make ~axes:true () in
    let _, plot =
      List.fold_left
        (fun (i, p) (_name, pts) ->
          let style = Option.bind styles (fun ss -> List.nth_opt ss i) in
          let p =
            Plot.line ?style ~x:(fun p -> p.time) ~y:(fun p -> p.value) pts p
          in
          (i + 1, p))
        (0, base) data
    in
    plot
  in
  Plot.draw plot canvas ~width ~height

let line_with_gaps ?styles
    (series : (string * (float * float option) list) list) =
 fun canvas ~width ~height ->
  let base = Plot.make ~axes:true () in
  let _, plot =
    List.fold_left
      (fun (i, p) (_name, pts) ->
        let style = Option.bind styles (fun ss -> List.nth_opt ss i) in
        let p = Plot.line_opt ?style ~x:fst ~y:(fun (_, y) -> y) pts p in
        (i + 1, p))
      (0, base) series
  in
  Plot.draw plot canvas ~width ~height

(* Stacked bar charts *)

type bar_segment = {
  value : float;
  style : Ansi.Style.t;
  label : string option;
}

type bar = { label : string; segments : bar_segment list }

let bar ?(orientation = `Vertical) ?(gap = 1) ?bar_width ?min_value ?max_value
    ?(show_axis = true) ?(axis_style = Style.default)
    ?(label_style = Style.default) (bars : bar list) =
 fun canvas ~width ~height ->
  (* Prepare data with negative clamping to align with ntcharts semantics *)
  let data =
    List.map
      (fun b ->
        (b.label, List.map (fun s -> (max 0. s.value, s.style)) b.segments))
      bars
  in
  (* If no explicit domain, anchor at zero to mirror legacy visuals. *)
  let implied_min = match min_value with Some v -> v | None -> 0. in
  let implied_max =
    match max_value with
    | Some v -> v
    | None ->
        let agg =
          List.map
            (fun (_lbl, segs) ->
              List.fold_left (fun acc (v, _st) -> acc +. max 0. v) 0. segs)
            data
        in
        List.fold_left max 0. agg
  in
  match orientation with
  | `Vertical ->
      (* Reserve bottom rows for axis+labels if requested *)
      let margins =
        if show_axis then { Plot.top = 0; right = 0; bottom = 2; left = 0 }
        else { Plot.top = 0; right = 0; bottom = 0; left = 0 }
      in
      let plot =
        let p = Plot.make ~margins ~axes:false () in
        let p = Plot.y_domain (implied_min, implied_max) p in
        Plot.(p |> bars_y_stacked ~gap ?bar_width data)
      in
      Plot.draw plot canvas ~width ~height;
      let ox = margins.left in
      let plot_w = max 0 (width - margins.left - margins.right) in
      (* Draw axis and labels below *)
      if show_axis then (
        let axis_y = height - 2 in
        if axis_y >= 0 && axis_y < height then
          Canvas.draw_line canvas ~x1:ox ~y1:axis_y
            ~x2:(ox + plot_w - 1)
            ~y2:axis_y ~style:axis_style ~kind:`Line ();
        (* Labels: compute same bar placement as Bars_y_stacked *)
        let n = List.length data in
        if n > 0 && plot_w > 0 then
          let gapv = max 1 gap in
          let auto_w =
            let gaps = (n - 1) * gapv in
            max 1 ((plot_w - gaps) / n)
          in
          let bar_w = match bar_width with Some bw -> bw | None -> auto_w in
          List.iteri
            (fun i (label, _segs) ->
              let x0 = ox + (i * (bar_w + gapv)) in
              let lbl =
                let l = label in
                if String.length l > bar_w then String.sub l 0 bar_w else l
              in
              let y = height - 1 in
              if bar_w > 0 && y >= 0 && y < height then
                Canvas.plot canvas ~x:x0 ~y ~style:label_style lbl)
            data)
  | `Horizontal ->
      (* Compute left margin from max label length when showing axis *)
      let max_label_len =
        if show_axis then
          List.fold_left (fun acc (l, _) -> max acc (String.length l)) 0 data
        else 0
      in
      let left_margin = if show_axis then max_label_len + 1 else 0 in
      let margins =
        { Plot.top = 0; right = 0; bottom = 0; left = left_margin }
      in
      let plot =
        let p = Plot.make ~margins ~axes:false () in
        let p = Plot.x_domain (implied_min, implied_max) p in
        Plot.(p |> bars_x_stacked ~gap ?bar_width data)
      in
      Plot.draw plot canvas ~width ~height;
      let ox = margins.left in
      let plot_h = max 0 (height - margins.top - margins.bottom) in
      (* Draw axis and labels to the left *)
      if show_axis then
        let axis_x = ox - 1 in
        if axis_x >= 0 && axis_x < width then (
          let oy = margins.top in
          Canvas.draw_line canvas ~x1:axis_x ~y1:oy ~x2:axis_x
            ~y2:(oy + plot_h - 1)
            ~style:axis_style ~kind:`Line ();
          (* Labels: compute same bar placement as Bars_x_stacked *)
          let n = List.length data in
          if n > 0 && plot_h > 0 then
            let gapv = max 1 gap in
            let auto_h =
              let gaps = (n - 1) * gapv in
              max 1 ((plot_h - gaps) / n)
            in
            let bar_h = match bar_width with Some bw -> bw | None -> auto_h in
            let oy = margins.top in
            List.iteri
              (fun i (label, _segs) ->
                let y0 = oy + (i * (bar_h + gapv)) in
                let row = y0 + (bar_h / 2) in
                let visible = min max_label_len (String.length label) in
                let lbl =
                  if visible < String.length label then
                    String.sub label 0 visible
                  else label
                in
                if row >= 0 && row < height && max_label_len > 0 then
                  Canvas.plot canvas ~x:0 ~y:row ~style:label_style lbl)
              data)

(* Candlestick charts *)

type ohlc_point = {
  time : float;
  open_ : float;
  high : float;
  low : float;
  close : float;
}

let candlestick ?bullish ?bearish (data : ohlc_point list) =
 fun canvas ~width ~height ->
  let plot =
    Plot.(
      make ()
      |> candles ?bullish ?bearish
           ~time:(fun p -> p.time)
           ~open_:(fun p -> p.open_)
           ~high:(fun p -> p.high)
           ~low:(fun p -> p.low)
           ~close:(fun p -> p.close)
           data)
  in
  Plot.draw plot canvas ~width ~height

(* Heatmap helper *)

type heat_point = { x : float; y : float; value : float }

let heatmap ?color_scale ?value_range ?auto_value_range ?shaded ?agg
    (pts : heat_point list) =
 fun canvas ~width ~height ->
  (* When [shaded=true] and the input points form a regular grid, upsample the
     scalar field to one sample per output cell using bilinear interpolation.
     This makes the shaded heatmap fill the canvas instead of drawing isolated
     dots, which matches the intended “fills cells” semantics. *)
  let pts =
    match shaded with
    | Some true ->
        (* Collect unique sorted X/Y coordinates. *)
        let xs_tbl = Hashtbl.create 16 in
        let ys_tbl = Hashtbl.create 16 in
        List.iter
          (fun p ->
            Hashtbl.replace xs_tbl p.x ();
            Hashtbl.replace ys_tbl p.y ())
          pts;
        let xs =
          Hashtbl.to_seq_keys xs_tbl |> List.of_seq |> List.sort Float.compare
        in
        let ys =
          Hashtbl.to_seq_keys ys_tbl |> List.of_seq |> List.sort Float.compare
        in
        let nx = List.length xs and ny = List.length ys in
        if nx <= 1 || ny <= 1 then pts
        else
          let xs_arr = Array.of_list xs in
          let ys_arr = Array.of_list ys in
          (* Map input samples into a dense grid indexed by (ix,iy). *)
          let grid =
            Hashtbl.create (nx * ny)
            (* key: ix,iy  value: aggregated scalar *)
          in
          let find_index arr v =
            let len = Array.length arr in
            let rec loop i =
              if i + 1 >= len then i
              else if Float.compare arr.(i + 1) v > 0 then i
              else loop (i + 1)
            in
            loop 0
          in
          List.iter
            (fun p ->
              let ix = find_index xs_arr p.x in
              let iy = find_index ys_arr p.y in
              let key = (ix, iy) in
              match Hashtbl.find_opt grid key with
              | None -> Hashtbl.add grid key p.value
              | Some old -> (
                  match agg with
                  | Some `Max ->
                      Hashtbl.replace grid key (Float.max old p.value)
                  | Some `Avg ->
                      (* For now, treat [Avg] as simple running average over
                         duplicates; duplicates are rare in typical grids. *)
                      Hashtbl.replace grid key ((old +. p.value) /. 2.)
                  | _ -> Hashtbl.replace grid key p.value))
            pts;
          let lookup ix iy =
            match Hashtbl.find_opt grid (ix, iy) with Some v -> v | None -> 0.
          in
          let sample x y =
            let nx = Array.length xs_arr and ny = Array.length ys_arr in
            let clamp_idx i max_i =
              if i < 0 then 0 else if i > max_i then max_i else i
            in
            let find_interval arr v =
              let len = Array.length arr in
              if len = 1 then (0, 0, 0., 0.)
              else
                let rec loop i =
                  if i + 1 >= len then
                    (len - 2, len - 1, arr.(len - 2), arr.(len - 1))
                  else if Float.compare arr.(i + 1) v >= 0 then
                    (i, i + 1, arr.(i), arr.(i + 1))
                  else loop (i + 1)
                in
                loop 0
            in
            let ix0, ix1, x0, x1 = find_interval xs_arr x in
            let iy0, iy1, y0, y1 = find_interval ys_arr y in
            let ix0 = clamp_idx ix0 (nx - 1) and ix1 = clamp_idx ix1 (nx - 1) in
            let iy0 = clamp_idx iy0 (ny - 1) and iy1 = clamp_idx iy1 (ny - 1) in
            let tx =
              if Float.equal x0 x1 then 0.
              else (x -. x0) /. Float.max 1e-12 (x1 -. x0)
            in
            let ty =
              if Float.equal y0 y1 then 0.
              else (y -. y0) /. Float.max 1e-12 (y1 -. y0)
            in
            let v00 = lookup ix0 iy0 in
            let v10 = lookup ix1 iy0 in
            let v01 = lookup ix0 iy1 in
            let v11 = lookup ix1 iy1 in
            let v0 = v00 +. ((v10 -. v00) *. tx) in
            let v1 = v01 +. ((v11 -. v01) *. tx) in
            v0 +. ((v1 -. v0) *. ty)
          in
          let x_min = xs_arr.(0) and x_max = xs_arr.(nx - 1) in
          let y_min = ys_arr.(0) and y_max = ys_arr.(ny - 1) in
          let dense = ref [] in
          for py = 0 to height - 1 do
            let ty =
              if height <= 1 then 0.
              else float (height - 1 - py) /. float (height - 1)
            in
            let y = y_min +. (ty *. (y_max -. y_min)) in
            for px = 0 to width - 1 do
              let tx =
                if width <= 1 then 0. else float px /. float (width - 1)
              in
              let x = x_min +. (tx *. (x_max -. x_min)) in
              let value = sample x y in
              dense := { x; y; value } :: !dense
            done
          done;
          List.rev !dense
    | _ -> pts
  in
  let plot =
    Plot.(
      make ~axes:false ()
      |> heatmap ?color_scale ?value_range ?auto_value_range ?shaded ?agg
           ~x:(fun pt -> pt.x)
           ~y:(fun pt -> pt.y)
           ~value:(fun pt -> pt.value)
           pts)
  in
  Plot.draw plot canvas ~width ~height

(* Legends *)

let legend (items : (string * Ansi.Style.t) list) =
 fun canvas ~width ~height:_ ->
  let x = ref 0 in
  let y = 0 in
  let gap = 2 in
  let text_width s = Glyph.measure ~width_method:`Unicode s in
  List.iter
    (fun (label, style) ->
      let marker = "●●  " in
      if !x < width then (
        Canvas.plot canvas ~x:!x ~y ~style marker;
        let x' = !x + text_width marker in
        if x' < width then (
          Canvas.plot canvas ~x:x' ~y label;
          x := x' + text_width label + gap)))
    items

let stacked_legend (items : (string * Ansi.Style.t) list) =
 fun canvas ~width:_ ~height:_ ->
  let y = ref 0 in
  List.iter
    (fun (label, style) ->
      Canvas.plot canvas ~x:0 ~y:!y ~style "●";
      Canvas.plot canvas ~x:2 ~y:!y label;
      incr y)
    items

(* Convenience time label formatters (UTC) *)

let x_label_mmdd _ v =
  let tm = Unix.gmtime v in
  Printf.sprintf "%02d/%02d" (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

let x_label_hhmmss _ v =
  let tm = Unix.gmtime v in
  Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(* Re-export global heatmap defaults *)
let heatmap_default_scale = Plot.heatmap_default_scale
let set_heatmap_default_scale = Plot.set_heatmap_default_scale
