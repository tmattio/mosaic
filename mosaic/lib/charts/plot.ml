module Style = Ansi.Style
module Canvas = Mosaic_ui.Canvas

(* Utilities *)

module Util = struct
  let clamp01 v = if v < 0. then 0. else if v > 1. then 1. else v

  let min_max = function
    | [] -> (0., 1.)
    | hd :: tl ->
        List.fold_left
          (fun (mn, mx) v -> (Float.min mn v, Float.max mx v))
          (hd, hd) tl

  let scale ~domain:(dmin, dmax) ~range:(rmin, rmax) v =
    let eps = 1e-12 in
    if Float.abs (dmax -. dmin) < eps then rmin
    else
      let t = (v -. dmin) /. (dmax -. dmin) in
      let v' = rmin +. (t *. (rmax -. rmin)) in
      if v' < rmin then rmin else if v' > rmax then rmax else v'

  let scale_unclamped ~domain:(dmin, dmax) ~range:(rmin, rmax) v =
    let eps = 1e-12 in
    if Float.abs (dmax -. dmin) < eps then rmin
    else
      let t = (v -. dmin) /. (dmax -. dmin) in
      rmin +. (t *. (rmax -. rmin))

  let normalize ~domain:(vmin, vmax) v =
    if Float.equal vmin vmax then 0.
    else
      let denom = Float.max 1e-12 (vmax -. vmin) in
      let t = (v -. vmin) /. denom in
      if t < 0. then 0. else if t > 1. then 1. else t

  let invert ~domain:(dmin, dmax) ~range:(rmin, rmax) v =
    let eps = 1e-12 in
    if Float.abs (rmax -. rmin) < eps then dmin
    else
      let t = (v -. rmin) /. (rmax -. rmin) in
      dmin +. (t *. (dmax -. dmin))
end

(* Heatmap default colors *)

let heatmap_default_scale_ref =
  ref
    [
      Ansi.Color.Extended 232;
      Ansi.Color.Extended 236;
      Ansi.Color.Extended 240;
      Ansi.Color.Extended 244;
      Ansi.Color.Extended 248;
      Ansi.Color.Extended 252;
    ]

let heatmap_default_scale () = !heatmap_default_scale_ref
let set_heatmap_default_scale cs = heatmap_default_scale_ref := cs

(* Public types / configuration *)

type line_kind = [ `Line | `Braille | `Wave | `Points of string ]
type scatter_kind = [ `Cell | `Braille ]
type heatmap_agg = [ `Last | `Avg | `Max ]
type margins = { top : int; right : int; bottom : int; left : int }
type band = { cats : string list; padding : float }
type plot_rect = { x : int; y : int; width : int; height : int }

type transforms = {
  px_to_data : int -> int -> (float * float) option;
  data_to_px : float -> float -> int * int;
  plot_rect : plot_rect;
}

type series =
  | Line : {
      data : 'a list;
      x : 'a -> float;
      y : 'a -> float;
      style : Style.t option;
      kind : line_kind;
    }
      -> series
  | Line_opt : {
      data : 'a list;
      x : 'a -> float;
      y : 'a -> float option;
      style : Style.t option;
      kind : line_kind;
    }
      -> series
  | Scatter : {
      data : 'a list;
      x : 'a -> float;
      y : 'a -> float;
      style : Style.t option;
      glyph : string;
      kind : scatter_kind;
    }
      -> series
  | Bar_y : {
      data : 'a list;
      x : 'a -> string;
      y : 'a -> float;
      style : Style.t option;
    }
      -> series
  | Bar_x : {
      data : 'a list;
      y : 'a -> string;
      x : 'a -> float;
      style : Style.t option;
    }
      -> series
  | Bars_y_stacked of {
      data : (string * (float * Ansi.Color.t) list) list;
      gap : int;
      bar_width : int option;
    }
  | Bars_x_stacked of {
      data : (string * (float * Ansi.Color.t) list) list;
      gap : int;
      bar_width : int option;
    }
  | Rule_y of { v : float; style : Style.t option }
  | Rule_x of { v : float; style : Style.t option }
  | Heatmap : {
      data : 'a list;
      x : 'a -> float;
      y : 'a -> float;
      value : 'a -> float;
      color_scale : Ansi.Color.t list;
      value_range : (float * float) option;
      auto_value_range : bool;
      shaded : bool;
      agg : heatmap_agg;
    }
      -> series
  | Candles : {
      data : 'a list;
      time : 'a -> float;
      open_ : 'a -> float;
      high : 'a -> float;
      low : 'a -> float;
      close : 'a -> float;
      bullish : Style.t;
      bearish : Style.t;
    }
      -> series
  | Circle : {
      data : 'a list;
      cx : 'a -> float;
      cy : 'a -> float;
      r : 'a -> float;
      style : Style.t option;
      kind : [ `Line | `Braille ];
    }
      -> series
  | Shade_x of { x0 : float; x1 : float; style : Style.t }
  | Column_bg of { x : float; style : Style.t }

type domains = {
  x_numeric : (float * float) option;
  y_numeric : (float * float) option;
  x_band : band option;
  y_band : band option;
}

type grid_cfg = {
  style : Style.t;
  x_enabled : bool;
  y_enabled : bool;
  x_step : int option;
  y_step : int option;
}

type t = {
  margins : margins;
  axes : bool;
  axes_style : Style.t option;
  x_ticks : int option;
  y_ticks : int option;
  x_label : (int -> float -> string) option;
  y_label : (int -> float -> string) option;
  x_view : (float * float) option;
  y_view : (float * float) option;
  grid : grid_cfg option;
  palette : Ansi.Color.t list;
  domains : domains;
  series : series list;
}

(* Construction / configuration API *)

let default_palette =
  [
    Ansi.Color.Cyan;
    Ansi.Color.Magenta;
    Ansi.Color.Yellow;
    Ansi.Color.Green;
    Ansi.Color.Blue;
    Ansi.Color.Red;
    Ansi.Color.Extended 33;
    Ansi.Color.Extended 39;
    Ansi.Color.Extended 45;
  ]

let make ?(margins = { top = 0; right = 0; bottom = 0; left = 0 })
    ?(axes = true) ?(grid = false) () =
  let grid =
    if grid then
      Some
        {
          style = Style.default;
          x_enabled = true;
          y_enabled = true;
          x_step = None;
          y_step = None;
        }
    else None
  in
  {
    margins;
    axes;
    axes_style = None;
    x_ticks = None;
    y_ticks = None;
    x_label = None;
    y_label = None;
    x_view = None;
    y_view = None;
    grid;
    palette = default_palette;
    domains =
      { x_numeric = None; y_numeric = None; x_band = None; y_band = None };
    series = [];
  }

let palette colors t = { t with palette = colors }
let x_domain d t = { t with domains = { t.domains with x_numeric = Some d } }
let y_domain d t = { t with domains = { t.domains with y_numeric = Some d } }

let x_band ?(padding = 0.1) cats t =
  let padding = Util.clamp01 padding in
  let x_band = Some { cats; padding } in
  { t with domains = { t.domains with x_band } }

let y_band ?(padding = 0.1) cats t =
  let padding = Util.clamp01 padding in
  let y_band = Some { cats; padding } in
  { t with domains = { t.domains with y_band } }

let axes ?style ?x_ticks ?y_ticks ?x_label ?y_label t =
  { t with axes = true; axes_style = style; x_ticks; y_ticks; x_label; y_label }

let grid ?(style = Style.make ~dim:true ()) ?(x = true) ?(y = true) ?x_step
    ?y_step t =
  { t with grid = Some { style; x_enabled = x; y_enabled = y; x_step; y_step } }

let x_view d t = { t with x_view = Some d }
let y_view d t = { t with y_view = Some d }

let line ?style ?(kind = (`Line : line_kind)) ~x ~y data t =
  let s = Line { data; x; y; style; kind } in
  { t with series = t.series @ [ s ] }

let line_opt ?style ?(kind = (`Line : line_kind)) ~x ~y data t =
  let s = Line_opt { data; x; y; style; kind } in
  { t with series = t.series @ [ s ] }

let scatter ?style ?(glyph = "∙") ?(kind = (`Cell : scatter_kind)) ~x ~y data t
    =
  let s = Scatter { data; x; y; style; glyph; kind } in
  { t with series = t.series @ [ s ] }

let bar_y ?style ~x ~y data t =
  let s = Bar_y { data; x; y; style } in
  { t with series = t.series @ [ s ] }

let bar_x ?style ~y ~x data t =
  let s = Bar_x { data; y; x; style } in
  { t with series = t.series @ [ s ] }

let rule_y ?style v t = { t with series = t.series @ [ Rule_y { v; style } ] }
let rule_x ?style v t = { t with series = t.series @ [ Rule_x { v; style } ] }

let bars_y_stacked ?(gap = 1) ?bar_width data t =
  let color_of_style (st : Style.t) =
    match st with
    | { Style.fg = Some c; _ } -> c
    | { Style.bg = Some c; _ } -> c
    | _ -> Ansi.Color.default
  in
  let convert segs = List.map (fun (v, st) -> (v, color_of_style st)) segs in
  let data = List.map (fun (label, segs) -> (label, convert segs)) data in
  { t with series = t.series @ [ Bars_y_stacked { data; gap; bar_width } ] }

let bars_x_stacked ?(gap = 1) ?bar_width data t =
  let color_of_style (st : Style.t) =
    match st with
    | { Style.fg = Some c; _ } -> c
    | { Style.bg = Some c; _ } -> c
    | _ -> Ansi.Color.default
  in
  let convert segs = List.map (fun (v, st) -> (v, color_of_style st)) segs in
  let data = List.map (fun (label, segs) -> (label, convert segs)) data in
  { t with series = t.series @ [ Bars_x_stacked { data; gap; bar_width } ] }

let heatmap ?(color_scale = heatmap_default_scale ()) ?value_range
    ?(auto_value_range = true) ?(shaded = false) ?(agg = (`Last : heatmap_agg))
    ~x ~y ~value data t =
  let s =
    Heatmap
      {
        data;
        x;
        y;
        value;
        color_scale;
        value_range;
        auto_value_range;
        shaded;
        agg;
      }
  in
  { t with series = t.series @ [ s ] }

let candles ?(bullish = Style.fg Ansi.Color.Green Style.default)
    ?(bearish = Style.fg Ansi.Color.Red Style.default) ~time ~open_ ~high ~low
    ~close data t =
  let s = Candles { data; time; open_; high; low; close; bullish; bearish } in
  { t with series = t.series @ [ s ] }

let circle ?style ?(kind = `Line) ~cx ~cy ~r data t =
  let s = Circle { data; cx; cy; r; style; kind } in
  { t with series = t.series @ [ s ] }

let shade_x ?(style = Style.make ~dim:true ()) ~min ~max t =
  let x0, x1 = if min <= max then (min, max) else (max, min) in
  { t with series = t.series @ [ Shade_x { x0; x1; style } ] }

let column_background ?(style = Style.make ~dim:true ()) x t =
  { t with series = t.series @ [ Column_bg { x; style } ] }

(* Domain computation *)

let compute_numeric_domains t =
  let acc_x = ref [] in
  let acc_y = ref [] in
  List.iter
    (function
      | Line { data; x; y; _ } ->
          List.iter
            (fun a ->
              acc_x := x a :: !acc_x;
              acc_y := y a :: !acc_y)
            data
      | Line_opt { data; x; y; _ } ->
          List.iter
            (fun a ->
              acc_x := x a :: !acc_x;
              match y a with None -> () | Some vy -> acc_y := vy :: !acc_y)
            data
      | Scatter { data; x; y; _ } ->
          List.iter
            (fun a ->
              acc_x := x a :: !acc_x;
              acc_y := y a :: !acc_y)
            data
      | Bar_y { data; y; _ } -> List.iter (fun a -> acc_y := y a :: !acc_y) data
      | Bar_x { data; x; _ } -> List.iter (fun a -> acc_x := x a :: !acc_x) data
      | Bars_y_stacked { data; _ } ->
          List.iter
            (fun (_lbl, segs) ->
              List.iter (fun (v, _) -> acc_y := v :: !acc_y) segs)
            data
      | Bars_x_stacked { data; _ } ->
          List.iter
            (fun (_lbl, segs) ->
              List.iter (fun (v, _) -> acc_x := v :: !acc_x) segs)
            data
      | Heatmap { data; x; y; _ } ->
          List.iter
            (fun a ->
              acc_x := x a :: !acc_x;
              acc_y := y a :: !acc_y)
            data
      | Candles { data; time; open_; high; low; close; _ } ->
          List.iter
            (fun a ->
              acc_x := time a :: !acc_x;
              acc_y := open_ a :: !acc_y;
              acc_y := high a :: !acc_y;
              acc_y := low a :: !acc_y;
              acc_y := close a :: !acc_y)
            data
      | Rule_y { v; _ } -> acc_y := v :: !acc_y
      | Rule_x { v; _ } -> acc_x := v :: !acc_x
      | Shade_x { x0; x1; _ } -> acc_x := x0 :: x1 :: !acc_x
      | Column_bg { x; _ } -> acc_x := x :: !acc_x
      | Circle { data; cx; cy; r; _ } ->
          List.iter
            (fun a ->
              let cxx = cx a and cyy = cy a and rr = r a in
              acc_x := (cxx -. rr) :: (cxx +. rr) :: !acc_x;
              acc_y := (cyy -. rr) :: (cyy +. rr) :: !acc_y)
            data)
    t.series;
  let x_dom =
    match t.domains.x_numeric with Some d -> d | None -> Util.min_max !acc_x
  in
  let y_dom =
    match t.domains.y_numeric with Some d -> d | None -> Util.min_max !acc_y
  in
  (x_dom, y_dom)

(* Palette helpers *)

let palette_style palette idx style_opt =
  match style_opt with
  | Some s -> s
  | None -> (
      let color = List.nth_opt palette (idx mod max 1 (List.length palette)) in
      match color with
      | None -> Style.default
      | Some c -> Style.fg c Style.default)

(* Layout & coordinate system *)

type coords = {
  ox : int;
  oy : int;
  plot_w : int;
  plot_h : int;
  x_dom : float * float;
  y_dom : float * float;
  x_to_px : float -> int;
  y_to_px : float -> int;
  x_to_px_opt : float -> int option;
  y_to_px_opt : float -> int option;
  cell_allowed : int -> int -> bool;
}

let draw_axes canvas ~x ~y ~w ~h ~style =
  if w > 0 && h > 0 then (
    Canvas.draw_line canvas ~x1:x
      ~y1:(y + h - 1)
      ~x2:(x + w - 1)
      ~y2:(y + h - 1)
      ~style ~kind:`Line ();
    Canvas.draw_line canvas ~x1:x ~y1:y ~x2:x
      ~y2:(y + h - 1)
      ~style ~kind:`Line ();
    Canvas.plot canvas ~x ~y:(y + h - 1) ~style "└")

let draw_grid canvas ~x ~y ~w ~h ~style ~x_grid ~y_grid ~x_step ~y_step =
  if w > 0 && h > 0 then (
    let y_every = Option.value ~default:2 y_step in
    let x_every = Option.value ~default:4 x_step in
    if y_grid && y_every > 0 then
      for yy = y to y + h - 1 do
        if (yy - y) mod y_every = 0 then
          Canvas.draw_line canvas ~x1:x ~y1:yy
            ~x2:(x + w - 1)
            ~y2:yy ~style ~kind:`Line ()
      done;
    if x_grid && x_every > 0 then
      for xx = x to x + w - 1 do
        if (xx - x) mod x_every = 0 then
          Canvas.draw_line canvas ~x1:xx ~y1:y ~x2:xx
            ~y2:(y + h - 1)
            ~style ~kind:`Line ()
      done)

(* Band / categorical helpers *)
let band_index cats s =
  let rec aux i = function
    | [] -> None
    | c :: tl -> if String.equal c s then Some i else aux (i + 1) tl
  in
  aux 0 cats

let band_params cats padding extent =
  let n = max 1 (List.length cats) in
  let pad = padding *. float extent in
  let band_extent = float extent -. pad in
  let bw = if n = 0 then 0. else band_extent /. float n in
  (pad /. 2., bw)

(* Axis label drawing (reused) *)
let draw_numeric_labels t canvas (c : coords) ~width ~height =
  if not t.axes then ()
  else
    let label_style = Option.value t.axes_style ~default:Style.default in
    let { ox; oy; plot_w; plot_h; x_dom; y_dom; _ } = c in
    (* X numeric labels *)
    (match (t.x_ticks, t.x_label, t.domains.x_band) with
    | _, _, Some _ -> ()
    | Some n, Some fmt, _ when n > 0 && plot_h > 0 ->
        let total = max 1 n in
        let denom = max 1 (total - 1) in
        let row = if plot_h >= 2 then oy + plot_h - 2 else oy + plot_h - 1 in
        let last_end = ref (-1) in
        for k = 0 to total - 1 do
          let px =
            if total = 1 then 0
            else
              int_of_float
                (Float.round (float (plot_w - 1) *. (float k /. float denom)))
          in
          let v =
            let t' =
              if plot_w > 1 then float px /. float (plot_w - 1) else 0.
            in
            let xmin, xmax = x_dom in
            xmin +. (t' *. (xmax -. xmin))
          in
          let s = fmt k v in
          let col = ox + px in
          if row >= 0 && row < height && col >= 0 && col < width then (
            Canvas.plot canvas ~x:col ~y:row ~style:label_style "│";
            let w = String.length s in
            if col >= !last_end + 2 && col + w <= width then (
              Canvas.plot canvas ~x:col
                ~y:(max 0 (row - 1))
                ~style:label_style s;
              last_end := col + w))
        done
    | _ -> ());
    (* Y numeric labels *)
    match (t.y_ticks, t.y_label, t.domains.y_band) with
    | _, _, Some _ -> ()
    | Some n, Some fmt, _ when n > 0 && plot_h > 0 ->
        let total = max 1 n in
        let denom = max 1 (total - 1) in
        for k = 0 to total - 1 do
          let py =
            if total = 1 then 0
            else
              int_of_float
                (Float.round (float (plot_h - 1) *. (float k /. float denom)))
          in
          let v =
            let t' =
              if plot_h > 1 then float py /. float (plot_h - 1) else 0.
            in
            let ymin, ymax = y_dom in
            ymax +. (t' *. (ymin -. ymax))
          in
          let s = fmt k v in
          let row = oy + py in
          if row >= 0 && row < height then (
            if ox < width then
              Canvas.plot canvas ~x:ox ~y:row ~style:label_style "─";
            if ox + 1 < width then
              Canvas.plot canvas ~x:(ox + 1) ~y:row ~style:label_style s)
        done
    | _ -> ()

let draw_band_labels t canvas (c : coords) ~width =
  if not t.axes then ()
  else
    let label_style = Option.value t.axes_style ~default:Style.default in
    let { ox; oy; plot_w; plot_h; _ } = c in
    (* X band labels *)
    (match t.domains.x_band with
    | None -> ()
    | Some b ->
        let cats = b.cats in
        let offset, bw = band_params cats b.padding plot_w in
        let row = if plot_h >= 2 then oy + plot_h - 2 else oy + plot_h - 1 in
        let n = List.length cats in
        let last_end = ref (-1) in
        for i = 0 to n - 1 do
          let x0 = int_of_float (offset +. (float i *. bw)) in
          let band_w = int_of_float (Float.max 1. (bw -. 1.)) in
          let label = List.nth cats i in
          let len = String.length label in
          let visible = min band_w len in
          let start = ox + x0 + max 0 ((band_w - visible) / 2) in
          if start >= !last_end + 1 && start < width && visible > 0 then (
            Canvas.plot canvas ~x:start ~y:row ~style:label_style
              (String.sub label 0 visible);
            last_end := start + visible)
        done);
    (* Y band labels *)
    match t.domains.y_band with
    | None -> ()
    | Some b ->
        let cats = b.cats in
        let offset, bw = band_params cats b.padding plot_h in
        let col = ox in
        let n = List.length cats in
        for i = 0 to n - 1 do
          let y0 = int_of_float (offset +. (float i *. bw)) in
          let band_h = int_of_float (Float.max 1. (bw -. 1.)) in
          let row = oy + y0 + (band_h / 2) in
          if row < oy + plot_h then
            let label = List.nth cats i in
            Canvas.plot canvas ~x:col ~y:row ~style:label_style label
        done

(* Series drawing helpers *)

(* Wave helper reused by Line + Line_opt *)
let draw_wave_from_pts canvas (c : coords) ~style (pts : (int * int) list) =
  let { ox; oy; plot_w; _ } = c in
  let pts = List.sort (fun (x1, _) (x2, _) -> compare x1 x2) pts in
  let seq_y = Array.make plot_w (-1) in
  let set_col x y = if x >= 0 && x < plot_w then seq_y.(x) <- y in
  let draw_seg (x1, y1) (x2, y2) =
    if x1 = x2 then set_col x1 y2
    else
      let x_start = min x1 x2 and x_end = max x1 x2 in
      for x = x_start to x_end do
        let t = float (x - x1) /. float (x2 - x1) in
        let y =
          int_of_float (Float.round (float y1 +. (t *. float (y2 - y1))))
        in
        set_col x y
      done
  in
  let rec loop = function
    | p1 :: (p2 :: _ as tl) ->
        draw_seg p1 p2;
        loop tl
    | _ -> ()
  in
  loop pts;
  let prev_y = ref None in
  for i = 0 to plot_w - 1 do
    let y = seq_y.(i) in
    if y < 0 then prev_y := None
    else
      match !prev_y with
      | None ->
          let x = ox + i in
          let y_cur = oy + y in
          Canvas.plot canvas ~x ~y:y_cur ~style "─";
          prev_y := Some y
      | Some prev ->
          let x = ox + i in
          let y_prev = oy + prev in
          let y_cur = oy + y in
          if prev = y then Canvas.plot canvas ~x ~y:y_cur ~style "─"
          else if prev > y then (
            Canvas.plot canvas ~x ~y:y_cur ~style "╭";
            Canvas.plot canvas ~x ~y:y_prev ~style "╯";
            for yy = y_cur + 1 to y_prev - 1 do
              Canvas.plot canvas ~x ~y:yy ~style "│"
            done)
          else (
            Canvas.plot canvas ~x ~y:y_cur ~style "╰";
            Canvas.plot canvas ~x ~y:y_prev ~style "╮";
            for yy = y_prev + 1 to y_cur - 1 do
              Canvas.plot canvas ~x ~y:yy ~style "│"
            done);
          prev_y := Some y
  done

(* Scatter braille helper *)
let draw_scatter_braille canvas (c : coords) ~style ~x_dom ~y_dom data x y
    ~cell_allowed =
  let { ox; oy; plot_w; plot_h; _ } = c in
  let buffer : (int * int, int) Hashtbl.t = Hashtbl.create 64 in
  let add_dot x_sub y_sub =
    let cell_x = x_sub / 2 and cell_y = y_sub / 4 in
    if
      cell_x >= ox
      && cell_x < ox + plot_w
      && cell_y >= oy
      && cell_y < oy + plot_h
      && cell_allowed cell_x cell_y
    then
      let bit_x = x_sub mod 2 and bit_y = y_sub mod 4 in
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
      let cur = Option.value (Hashtbl.find_opt buffer key) ~default:0 in
      Hashtbl.replace buffer key (cur lor (1 lsl bit_pos))
  in
  List.iter
    (fun a ->
      let xa = x a and ya = y a in
      let min_x, max_x = x_dom in
      let min_y, max_y = y_dom in
      let g_width = plot_w * 2 in
      let g_height = plot_h * 4 in
      let dx = max_x -. min_x in
      let dy = max_y -. min_y in
      let sf_x =
        if dx > 0. then (xa -. min_x) *. float (g_width - 1) /. dx else 0.
      in
      let sf_y =
        if dy > 0. then (ya -. min_y) *. float (g_height - 1) /. dy else 0.
      in
      let x_grid = int_of_float (Float.round sf_x) in
      let y_grid = int_of_float (Float.round sf_y) in
      let x_sub = (ox * 2) + x_grid in
      let y_sub = (oy * 4) + (g_height - 1 - y_grid) in
      add_dot x_sub y_sub)
    data;
  let utf8_of_codepoint u =
    let b = Buffer.create 4 in
    let add_byte i = Buffer.add_char b (Char.chr i) in
    if u <= 0x7F then add_byte u
    else if u <= 0x7FF then (
      add_byte (0xC0 lor (u lsr 6));
      add_byte (0x80 lor (u land 0x3F)))
    else if u <= 0xFFFF then (
      add_byte (0xE0 lor (u lsr 12));
      add_byte (0x80 lor ((u lsr 6) land 0x3F));
      add_byte (0x80 lor (u land 0x3F)))
    else (
      add_byte (0xF0 lor (u lsr 18));
      add_byte (0x80 lor ((u lsr 12) land 0x3F));
      add_byte (0x80 lor ((u lsr 6) land 0x3F));
      add_byte (0x80 lor (u land 0x3F)));
    Buffer.contents b
  in
  Hashtbl.iter
    (fun (cell_x, cell_y) bits ->
      let code = 0x2800 + bits in
      let glyph = utf8_of_codepoint code in
      Canvas.plot canvas ~x:cell_x ~y:cell_y ~style glyph)
    buffer

(* Heatmap color helper *)
let heatmap_color_fun ~color_scale ~vmin ~vmax =
 fun v ->
  let t = Util.normalize ~domain:(vmin, vmax) v in
  let len = max 1 (List.length color_scale) in
  let raw_idx = int_of_float (t *. float len) in
  let idx =
    if raw_idx < 0 then 0 else if raw_idx >= len then len - 1 else raw_idx
  in
  let idx = max 0 (min (len - 1) idx) in
  Option.value (List.nth_opt color_scale idx) ~default:Ansi.Color.default

(* Main draw implementation *)

let draw_impl t (canvas : Canvas.t) ~width ~height =
  let m = t.margins in
  (* Legacy layout: 1 col for Y axis, 1 col padding, 1 row for X axis *)
  let axis_cols = if t.axes then 1 else 0 in
  let padding_after_axis = if t.axes && width > axis_cols + 1 then 1 else 0 in
  let tentative_ox = m.left + axis_cols + padding_after_axis in
  let ox = if tentative_ox >= width then max 0 (width - 1) else tentative_ox in
  let oy = m.top in
  let plot_w = max 1 (width - ox - m.right) in
  let plot_h = max 1 (height - (if t.axes then 1 else 0) - m.top - m.bottom) in

  (* Clear plot area *)
  Canvas.fill_rect canvas ~x:ox ~y:oy ~width:plot_w ~height:plot_h
    ~color:(Ansi.Color.of_rgba 0 0 0 255);

  (* Grid & axes *)
  (match t.grid with
  | Some { style; x_enabled; y_enabled; x_step; y_step } ->
      draw_grid canvas ~x:ox ~y:oy ~w:plot_w ~h:plot_h ~style ~x_grid:x_enabled
        ~y_grid:y_enabled ~x_step ~y_step
  | None -> ());
  (match (t.axes, t.axes_style) with
  | true, Some style ->
      draw_axes canvas ~x:m.left ~y:oy
        ~w:(width - m.left - m.right)
        ~h:(height - m.top - m.bottom)
        ~style
  | true, None ->
      draw_axes canvas ~x:m.left ~y:oy
        ~w:(width - m.left - m.right)
        ~h:(height - m.top - m.bottom)
        ~style:Style.default
  | _ -> ());

  (* Domains *)
  let dom_x_auto, dom_y_auto = compute_numeric_domains t in
  let x_dom = Option.value t.x_view ~default:dom_x_auto in
  let y_dom = Option.value t.y_view ~default:dom_y_auto in

  (* Label rows/cols reserved inside plot *)
  let has_x_numeric_labels =
    match (t.x_ticks, t.x_label, t.domains.x_band) with
    | Some n, Some _, None when n > 0 && plot_h > 0 -> true
    | _ -> false
  in
  let has_y_numeric_labels =
    match (t.y_ticks, t.y_label, t.domains.y_band) with
    | Some n, Some _, None when n > 0 && plot_h > 0 -> true
    | _ -> false
  in
  let x_label_row =
    if has_x_numeric_labels then
      if plot_h >= 2 then oy + plot_h - 2 else oy + plot_h - 1
    else -1
  in
  let y_label_col1 = if has_y_numeric_labels then ox else -1 in
  let y_label_col2 = if has_y_numeric_labels then ox + 1 else -1 in
  let cell_allowed x y =
    ((not has_x_numeric_labels) || y <> x_label_row)
    && ((not has_y_numeric_labels) || (x <> y_label_col1 && x <> y_label_col2))
  in

  (* Coordinate transforms *)
  let x_to_px v =
    ox
    + int_of_float (Util.scale ~domain:x_dom ~range:(0., float (plot_w - 1)) v)
  in
  let y_to_px v =
    oy
    + int_of_float
        (Util.scale
           ~domain:(snd y_dom, fst y_dom)
           ~range:(0., float (plot_h - 1))
           v)
  in
  let x_to_px_opt v =
    let xf =
      Util.scale_unclamped ~domain:x_dom ~range:(0., float (plot_w - 1)) v
    in
    let xi = ox + int_of_float (Float.round xf) in
    if xi < ox || xi >= ox + plot_w then None else Some xi
  in
  let y_to_px_opt v =
    let yf =
      Util.scale_unclamped
        ~domain:(snd y_dom, fst y_dom)
        ~range:(0., float (plot_h - 1))
        v
    in
    let yi = oy + int_of_float (Float.round yf) in
    if yi < oy || yi >= oy + plot_h then None else Some yi
  in

  let coords =
    {
      ox;
      oy;
      plot_w;
      plot_h;
      x_dom;
      y_dom;
      x_to_px;
      y_to_px;
      x_to_px_opt;
      y_to_px_opt;
      cell_allowed;
    }
  in

  (* Axis labels *)
  draw_numeric_labels t canvas coords ~width ~height;
  draw_band_labels t canvas coords ~width;

  (* Per-series drawer *)
  let palette = t.palette in

  let draw_line_series si ~data ~x ~y ~style ~kind =
    let st = palette_style palette si style in
    match kind with
    | `Line ->
        let rec loop = function
          | p1 :: p2 :: tl ->
              let x1 = x_to_px (x p1) in
              let y1 = y_to_px (y p1) in
              let x2 = x_to_px (x p2) in
              let y2 = y_to_px (y p2) in
              Canvas.draw_line canvas ~x1 ~y1 ~x2 ~y2 ~style:st ~kind:`Line ();
              loop (p2 :: tl)
          | _ -> ()
        in
        loop data
    | `Wave ->
        let to_px a =
          let xi =
            int_of_float
              (Util.scale ~domain:x_dom ~range:(0., float (plot_w - 1)) (x a))
          in
          let yi =
            int_of_float
              (Util.scale
                 ~domain:(snd y_dom, fst y_dom)
                 ~range:(0., float (plot_h - 1))
                 (y a))
          in
          (xi, yi)
        in
        let pts = List.map to_px data in
        draw_wave_from_pts canvas coords ~style:st pts
    | `Braille ->
        let xmin, xmax = x_dom in
        let ymin, ymax = y_dom in
        let dx = xmax -. xmin in
        let dy = ymax -. ymin in
        let grid_w = plot_w * 2 in
        let grid_h = plot_h * 4 in
        let xs = if dx > 0. then float (grid_w - 1) /. dx else 0. in
        let ys = if dy > 0. then float (grid_h - 1) /. dy else 0. in
        let map_point a =
          let xf = x a and yf = y a in
          let sx = (xf -. xmin) *. xs in
          let sy = (yf -. ymin) *. ys in
          let px = int_of_float (Float.round sx) in
          let py = grid_h - 1 - int_of_float (Float.round sy) in
          let px = (ox * 2) + px in
          let py = (oy * 4) + py in
          (px, py)
        in
        let rec loop = function
          | p1 :: p2 :: tl ->
              let x1, y1 = map_point p1 in
              let x2, y2 = map_point p2 in
              Canvas.draw_line canvas ~x1 ~y1 ~x2 ~y2 ~style:st ~kind:`Braille
                ();
              loop (p2 :: tl)
          | _ -> ()
        in
        loop data
    | `Points glyph ->
        List.iter
          (fun a ->
            let x' = x_to_px (x a) in
            let y' = y_to_px (y a) in
            Canvas.plot canvas ~x:x' ~y:y' ~style:st glyph)
          data
  in

  let draw_line_opt_series si ~data ~x ~y ~style ~kind =
    let st = palette_style palette si style in
    match kind with
    | `Points glyph ->
        List.iter
          (fun a ->
            match y a with
            | None -> ()
            | Some vy ->
                let x' = x_to_px (x a) in
                let y' = y_to_px vy in
                Canvas.plot canvas ~x:x' ~y:y' ~style:st glyph)
          data
    | (`Line | `Braille | `Wave) as k -> (
        let kind' =
          match k with `Line -> `Line | `Braille -> `Braille | `Wave -> `Line
        in
        match k with
        | `Wave ->
            let to_px a vy =
              let xi =
                int_of_float
                  (Util.scale ~domain:x_dom
                     ~range:(0., float (plot_w - 1))
                     (x a))
              in
              let yi =
                int_of_float
                  (Util.scale
                     ~domain:(snd y_dom, fst y_dom)
                     ~range:(0., float (plot_h - 1))
                     vy)
              in
              (xi, yi)
            in
            let rec split acc cur = function
              | [] -> List.rev (if cur = [] then acc else List.rev cur :: acc)
              | a :: tl -> (
                  match y a with
                  | None ->
                      let acc' =
                        if cur = [] then acc else List.rev cur :: acc
                      in
                      split acc' [] tl
                  | Some _ -> split acc (a :: cur) tl)
            in
            let segments = split [] [] data in
            List.iter
              (fun seg ->
                let pts =
                  List.filter_map
                    (fun a ->
                      match y a with
                      | None -> None
                      | Some vy -> Some (to_px a vy))
                    seg
                in
                draw_wave_from_pts canvas coords ~style:st pts)
              segments
        | `Line | `Braille ->
            let rec loop prev = function
              | [] -> ()
              | a :: tl -> (
                  match (prev, y a) with
                  | Some (px, py), Some vy ->
                      let x2 = x_to_px (x a) in
                      let y2 = y_to_px vy in
                      Canvas.draw_line canvas ~x1:px ~y1:py ~x2 ~y2 ~style:st
                        ~kind:kind' ();
                      loop (Some (x2, y2)) tl
                  | _, Some vy ->
                      let x2 = x_to_px (x a) in
                      let y2 = y_to_px vy in
                      loop (Some (x2, y2)) tl
                  | _ -> loop None tl)
            in
            loop None data)
  in

  let draw_scatter_series si ~data ~x ~y ~style ~glyph ~kind =
    let st = palette_style palette si style in
    match kind with
    | `Cell ->
        List.iter
          (fun a ->
            match (x_to_px_opt (x a), y_to_px_opt (y a)) with
            | Some x', Some y' when cell_allowed x' y' ->
                Canvas.plot canvas ~x:x' ~y:y' ~style:st glyph
            | _ -> ())
          data
    | `Braille ->
        draw_scatter_braille canvas coords ~style:st ~x_dom ~y_dom data x y
          ~cell_allowed
  in

  let draw_bar_y_series si ~data ~x ~y ~style =
    let _st = palette_style palette si style in
    let cats =
      match t.domains.x_band with
      | Some b -> b.cats
      | None ->
          let tbl = Hashtbl.create 32 in
          let ordered = ref [] in
          List.iter
            (fun a ->
              let c = x a in
              if not (Hashtbl.mem tbl c) then (
                Hashtbl.add tbl c ();
                ordered := !ordered @ [ c ]))
            data;
          !ordered
    in
    let padding =
      match t.domains.x_band with Some b -> b.padding | None -> 0.1
    in
    let offset, bw = band_params cats padding plot_w in
    let bar_w = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
    List.iter
      (fun a ->
        match band_index cats (x a) with
        | None -> ()
        | Some i ->
            let x0 = ox + int_of_float (offset +. (float i *. bw)) in
            let h = max 0 (y_to_px 0. - y_to_px (y a)) in
            let y0 = y_to_px (y a) in
            if h > 0 then
              for yy = 0 to h - 1 do
                for xx = 0 to bar_w - 1 do
                  Canvas.plot canvas ~x:(x0 + xx) ~y:(y0 + yy) "█"
                done
              done)
      data
  in

  let draw_bar_x_series si ~data ~y ~x ~style =
    let _st = palette_style palette si style in
    let cats =
      match t.domains.y_band with
      | Some b -> b.cats
      | None ->
          let tbl = Hashtbl.create 32 in
          let ordered = ref [] in
          List.iter
            (fun a ->
              let c = y a in
              if not (Hashtbl.mem tbl c) then (
                Hashtbl.add tbl c ();
                ordered := !ordered @ [ c ]))
            data;
          !ordered
    in
    let padding =
      match t.domains.y_band with Some b -> b.padding | None -> 0.1
    in
    let offset, bw = band_params cats padding plot_h in
    let bar_h = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
    let x0_px =
      int_of_float (Util.scale ~domain:x_dom ~range:(0., float plot_w) 0.)
    in
    List.iter
      (fun a ->
        match band_index cats (y a) with
        | None -> ()
        | Some i ->
            let y0 = oy + int_of_float (offset +. (float i *. bw)) in
            let x_px =
              int_of_float
                (Util.scale ~domain:x_dom ~range:(0., float plot_w) (x a))
            in
            let xlen = max 0 (x_px - x0_px) in
            if xlen > 0 then
              for xx = 0 to xlen - 1 do
                for yy = 0 to bar_h - 1 do
                  Canvas.plot canvas ~x:(ox + xx) ~y:(y0 + yy) "█"
                done
              done)
      data
  in

  let draw_heatmap_series ~data ~x ~y ~value ~color_scale ~value_range
      ~auto_value_range ~shaded ~agg =
    let values = List.map value data in
    let vmin, vmax =
      match value_range with
      | Some (a, b) -> (a, b)
      | None -> if auto_value_range then Util.min_max values else (0., 1.)
    in
    let color_of = heatmap_color_fun ~color_scale ~vmin ~vmax in
    if not shaded then (
      match agg with
      | `Last ->
          List.iter
            (fun a ->
              let cx = x_to_px (x a) in
              let cy = y_to_px (y a) in
              let c = color_of (value a) in
              Canvas.fill_rect canvas ~x:cx ~y:cy ~width:1 ~height:1 ~color:c)
            data
      | `Avg ->
          let tbl : (int * int, float * int) Hashtbl.t = Hashtbl.create 97 in
          List.iter
            (fun a ->
              let cx = x_to_px (x a) and cy = y_to_px (y a) in
              let v = value a in
              match Hashtbl.find_opt tbl (cx, cy) with
              | None -> Hashtbl.add tbl (cx, cy) (v, 1)
              | Some (s, n) -> Hashtbl.replace tbl (cx, cy) (s +. v, n + 1))
            data;
          Hashtbl.iter
            (fun (cx, cy) (s, n) ->
              let v = s /. float n in
              let c = color_of v in
              Canvas.fill_rect canvas ~x:cx ~y:cy ~width:1 ~height:1 ~color:c)
            tbl
      | `Max ->
          let tbl : (int * int, float) Hashtbl.t = Hashtbl.create 97 in
          List.iter
            (fun a ->
              let cx = x_to_px (x a) and cy = y_to_px (y a) in
              let v = value a in
              match Hashtbl.find_opt tbl (cx, cy) with
              | None -> Hashtbl.add tbl (cx, cy) v
              | Some cur -> Hashtbl.replace tbl (cx, cy) (Float.max cur v))
            data;
          Hashtbl.iter
            (fun (cx, cy) v ->
              let c = color_of v in
              Canvas.fill_rect canvas ~x:cx ~y:cy ~width:1 ~height:1 ~color:c)
            tbl)
    else
      let shade = [| " "; "░"; "▒"; "▓"; "█" |] in
      let shade_idx v =
        let t = Util.normalize ~domain:(vmin, vmax) v in
        max 0 (min 4 (int_of_float (t *. 4.)))
      in
      let draw_one cx cy v =
        let color = color_of v in
        let st = Style.fg color Style.default in
        Canvas.plot canvas ~x:cx ~y:cy ~style:st shade.(shade_idx v)
      in
      match agg with
      | `Last ->
          List.iter
            (fun a -> draw_one (x_to_px (x a)) (y_to_px (y a)) (value a))
            data
      | `Avg ->
          let tbl : (int * int, float * int) Hashtbl.t = Hashtbl.create 97 in
          List.iter
            (fun a ->
              let key = (x_to_px (x a), y_to_px (y a)) in
              let v = value a in
              match Hashtbl.find_opt tbl key with
              | None -> Hashtbl.add tbl key (v, 1)
              | Some (s, n) -> Hashtbl.replace tbl key (s +. v, n + 1))
            data;
          Hashtbl.iter
            (fun (cx, cy) (s, n) -> draw_one cx cy (s /. float n))
            tbl
      | `Max ->
          let tbl : (int * int, float) Hashtbl.t = Hashtbl.create 97 in
          List.iter
            (fun a ->
              let key = (x_to_px (x a), y_to_px (y a)) in
              let v = value a in
              match Hashtbl.find_opt tbl key with
              | None -> Hashtbl.add tbl key v
              | Some cur -> Hashtbl.replace tbl key (Float.max cur v))
            data;
          Hashtbl.iter (fun (cx, cy) v -> draw_one cx cy v) tbl
  in

  let draw_circle_series ~data ~cx ~cy ~r ~style ~kind =
    let st = Option.value style ~default:Style.default in
    let get_circle_points ~cx ~cy ~radius =
      if radius <= 0 then []
      else
        let points = ref [] in
        let t1 = ref (radius / 16) in
        let t2 = ref 0 in
        let x = ref radius in
        let y = ref 0 in
        let add dx dy = points := (cx + dx, cy + dy) :: !points in
        while !x >= !y do
          add !x !y;
          add !x (- !y);
          add (- !x) !y;
          add (- !x) (- !y);
          add !y !x;
          add !y (- !x);
          add (- !y) !x;
          add (- !y) (- !x);
          incr y;
          t1 := !t1 + !y;
          t2 := !t1 - !x;
          if !t2 >= 0 then (
            t1 := !t2;
            decr x)
        done;
        !points
    in
    match kind with
    | `Line ->
        (* Approximate circle using an integer mid-point algorithm, mirroring
           ntcharts' GetCirclePoints + DrawRuneCircle behaviour, then map the
           resulting domain coordinates through [x_to_px]/[y_to_px] and plot
           solid blocks. *)
        let draw_one a =
          let cxv = cx a and cyv = cy a and rv = r a in
          let cx_i = int_of_float (Float.round cxv) in
          let cy_i = int_of_float (Float.round cyv) in
          let radius = int_of_float (Float.round rv) in
          let points = get_circle_points ~cx:cx_i ~cy:cy_i ~radius in
          List.iter
            (fun (xd, yd) ->
              let x = x_to_px (float xd) in
              let y = y_to_px (float yd) in
              if cell_allowed x y then Canvas.plot canvas ~x ~y ~style:st "█")
            points
        in
        List.iter draw_one data
    | `Braille ->
        (* Draw circle into a braille "pixel" grid (2x4 per cell), similar to
           ntcharts' BrailleGrid + DrawBrailleCircle, then emit the resulting
           braille runes into the canvas. *)
        let set_dot buffer x y =
          let cell_x = x / 2 in
          let cell_y = y / 4 in
          let bit_x = x mod 2 in
          let bit_y = y mod 4 in
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
          let current = Option.value (Hashtbl.find_opt buffer key) ~default:0 in
          Hashtbl.replace buffer key (current lor (1 lsl bit_pos))
        in
        let draw_one a =
          let cxv = cx a and cyv = cy a and rv = r a in
          let cx_i = int_of_float (Float.round cxv) in
          let cy_i = int_of_float (Float.round cyv) in
          let radius = int_of_float (Float.round rv) in
          let points = get_circle_points ~cx:cx_i ~cy:cy_i ~radius in
          let buffer = Hashtbl.create 64 in
          List.iter
            (fun (xd, yd) ->
              let min_x, max_x = x_dom in
              let min_y, max_y = y_dom in
              let g_width = plot_w * 2 in
              let g_height = plot_h * 4 in
              let dx = max_x -. min_x in
              let dy = max_y -. min_y in
              let sf_x =
                if dx > 0. then (float xd -. min_x) *. float (g_width - 1) /. dx
                else 0.
              in
              let sf_y =
                if dy > 0. then
                  (float yd -. min_y) *. float (g_height - 1) /. dy
                else 0.
              in
              let x_grid = int_of_float (Float.round sf_x) in
              let y_grid = int_of_float (Float.round sf_y) in
              let x_px = (ox * 2) + x_grid in
              let y_px = (oy * 4) + (g_height - 1 - y_grid) in
              set_dot buffer x_px y_px)
            points;
          Hashtbl.iter
            (fun (cell_x, cell_y) bits ->
              let code = 0x2800 + bits in
              let uchar =
                match Uchar.of_int code with
                | exception Invalid_argument _ -> Uchar.of_int 0x2800
                | c -> c
              in
              let buf = Buffer.create 4 in
              Buffer.add_utf_8_uchar buf uchar;
              let glyph = Buffer.contents buf in
              if cell_allowed cell_x cell_y then
                Canvas.plot canvas ~x:cell_x ~y:cell_y ~style:st glyph)
            buffer
        in
        List.iter draw_one data
  in

  let draw_shade_x_series ~x0 ~x1 ~style =
    let c0 = x_to_px x0 and c1 = x_to_px x1 in
    let x_start = min c0 c1 and x_end = max c0 c1 in
    for xx = x_start to x_end do
      for yy = oy to oy + plot_h - 1 do
        Canvas.plot canvas ~x:xx ~y:yy ~style " "
      done
    done
  in

  let draw_column_bg_series ~x ~style =
    let col = x_to_px x in
    if col >= ox && col < ox + plot_w then
      for yy = oy to oy + plot_h - 1 do
        Canvas.plot canvas ~x:col ~y:yy ~style " "
      done
  in

  let draw_candles_series ~data ~time ~open_ ~high ~low ~close ~bullish ~bearish
      =
    let sorted = List.sort (fun a b -> compare (time a) (time b)) data in
    List.iter
      (fun a ->
        let cx = x_to_px (time a) in
        let o = open_ a and c = close a in
        let h = high a and l = low a in
        let st = if c >= o then bullish else bearish in
        let y_body_top = y_to_px (max o c) in
        let y_body_bot = y_to_px (min o c) in
        let y_wick_top = y_to_px h in
        let y_wick_bot = y_to_px l in
        if y_wick_top < y_body_top then
          for yy = y_wick_top to y_body_top - 1 do
            Canvas.plot canvas ~x:cx ~y:yy ~style:st "│"
          done;
        for yy = y_body_top to y_body_bot do
          Canvas.plot canvas ~x:cx ~y:yy ~style:st "┃"
        done;
        if y_body_bot < y_wick_bot then
          for yy = y_body_bot + 1 to y_wick_bot do
            Canvas.plot canvas ~x:cx ~y:yy ~style:st "│"
          done)
      sorted
  in

  let draw_bars_y_stacked_series ~data ~gap ~bar_width =
    let n = max 1 (List.length data) in
    let auto_w =
      let gaps = (n - 1) * max 1 gap in
      max 1 ((plot_w - gaps) / n)
    in
    let bar_w = Option.value bar_width ~default:auto_w in
    let ymin, ymax = y_dom in
    let span = ymax -. ymin in
    let span = if span <= 0. then 1. else span in
    let scale = float plot_h /. span in
    let bar_len = float plot_h in
    let is_lower_block = function
      | "▁" | "▂" | "▃" | "▄" | "▅" | "▆" | "▇" | "█" -> true
      | _ -> false
    in
    let lower_block_glyph frac =
      if frac <= 0. then None
      else if frac >= 1. then Some "█"
      else
        let eighth = 0.125 in
        let e = int_of_float (Float.floor (frac /. eighth)) in
        let rem = frac -. (float e *. eighth) in
        let e = if rem >= 0.0625 then e + 1 else e in
        match e with
        | 0 -> None
        | 1 -> Some "▁"
        | 2 -> Some "▂"
        | 3 -> Some "▃"
        | 4 -> Some "▄"
        | 5 -> Some "▅"
        | 6 -> Some "▆"
        | 7 -> Some "▇"
        | 8 -> Some "█"
        | _ -> None
    in
    let get_column_height cells =
      let rec loop h row =
        if row < 0 then h
        else
          match cells.(row) with
          | Some c when is_lower_block c -> loop (h + 1) (row - 1)
          | _ -> h
      in
      loop 0 (plot_h - 1)
    in
    let draw_column_rune cells row glyph =
      if glyph = "" || (not (is_lower_block glyph)) || row < 0 || row >= plot_h
      then ()
      else
        match cells.(row) with
        | Some c when is_lower_block c ->
            if glyph = "█" && c <> "█" then () else cells.(row) <- Some glyph
        | _ -> cells.(row) <- Some glyph
    in
    let draw_column cells v =
      if v <= 0. then ()
      else
        let v = Float.min v bar_len in
        let h = get_column_height cells in
        let n_float = Float.floor v in
        let frac = v -. n_float in
        let top = lower_block_glyph frac in
        let base_full = int_of_float n_float in
        let nh =
          let extra = match top with None -> 0 | Some _ -> 1 in
          min plot_h (base_full + extra)
        in
        if h = 0 || nh = h then (
          (* Replace entire column. *)
          for i = 0 to base_full - 1 do
            let row = plot_h - 1 - i in
            if row >= 0 && row < plot_h then cells.(row) <- Some "█"
          done;
          match top with
          | None -> ()
          | Some glyph ->
              let row = plot_h - 1 - base_full in
              draw_column_rune cells row glyph)
        else if nh < h then (
          (* Shorter column: replace bottom full blocks and new top rune. *)
          for i = 0 to base_full - 1 do
            let row = plot_h - 1 - i in
            if row >= 0 && row < plot_h then cells.(row) <- Some "█"
          done;
          match top with
          | None -> ()
          | Some glyph ->
              let row = plot_h - 1 - base_full in
              draw_column_rune cells row glyph)
        else
          (* Taller column: keep previous top, extend upwards, set new top. *)
          let oc = if h - 1 <= 0 then 0 else h - 1 in
          let row_oc = plot_h - 1 - oc in
          draw_column_rune cells row_oc "█";
          for i = h to base_full - 1 do
            let row = plot_h - 1 - i in
            if row >= 0 && row < plot_h then cells.(row) <- Some "█"
          done;
          match top with
          | None -> ()
          | Some glyph ->
              let row = plot_h - 1 - base_full in
              draw_column_rune cells row glyph
    in
    List.iteri
      (fun i (_label, segs) ->
        let x0 = ox + (i * (bar_w + max 1 gap)) in
        let scaled =
          List.map
            (fun (v, _) ->
              let v = max 0. (v -. ymin) in
              v *. scale)
            segs
        in
        let cells = Array.make plot_h None in
        let total = List.fold_left ( +. ) 0. scaled in
        let rev_scaled = List.rev scaled in
        let sum = ref total in
        List.iter
          (fun v_seg ->
            draw_column cells (Float.min !sum bar_len);
            sum := !sum -. v_seg)
          rev_scaled;
        let st =
          match segs with
          | [] -> Style.default
          | (_, color) :: _ -> Style.fg color Style.default
        in
        for row = 0 to plot_h - 1 do
          match cells.(row) with
          | None -> ()
          | Some glyph ->
              let y = oy + row in
              if y >= oy && y < oy + plot_h then
                for xx = 0 to bar_w - 1 do
                  Canvas.plot canvas ~x:(x0 + xx) ~y ~style:st glyph
                done
        done)
      data
  in

  let draw_bars_x_stacked_series ~data ~gap ~bar_width =
    let n = max 1 (List.length data) in
    let auto_h =
      let gaps = (n - 1) * max 1 gap in
      max 1 ((plot_h - gaps) / n)
    in
    let bar_h = Option.value bar_width ~default:auto_h in
    let xmin, xmax = x_dom in
    let span = xmax -. xmin in
    let span = if span <= 0. then 1. else span in
    let scale = float plot_w /. span in
    let left_block_glyph frac =
      if frac <= 0. then None
      else if frac >= 1. then Some "█"
      else
        let eighth = 0.125 in
        let e = int_of_float (Float.floor (frac /. eighth)) in
        let rem = frac -. (float e *. eighth) in
        let e = if rem >= 0.0625 then e + 1 else e in
        match e with
        | 0 -> None
        | 1 -> Some "▏"
        | 2 -> Some "▎"
        | 3 -> Some "▍"
        | 4 -> Some "▌"
        | 5 -> Some "▋"
        | 6 -> Some "▊"
        | 7 -> Some "▉"
        | 8 -> Some "█"
        | _ -> None
    in
    List.iteri
      (fun i (_label, segs) ->
        let y0 = oy + (i * (bar_h + max 1 gap)) in
        let total =
          List.fold_left
            (fun acc (v, _) ->
              let v = max 0. (v -. xmin) in
              acc +. v)
            0. segs
        in
        let w_units = total *. scale in
        let full_blocks = int_of_float (Float.floor w_units) in
        let frac = w_units -. float full_blocks in
        let st =
          match segs with
          | [] -> Style.default
          | (_, color) :: _ -> Style.fg color Style.default
        in
        let full_blocks = min full_blocks plot_w in
        for k = 0 to full_blocks - 1 do
          let x = ox + k in
          if x >= ox && x < ox + plot_w then
            for yy = 0 to bar_h - 1 do
              Canvas.plot canvas ~x ~y:(y0 + yy) ~style:st "█"
            done
        done;
        if full_blocks < plot_w then
          match left_block_glyph frac with
          | None -> ()
          | Some glyph ->
              let x = ox + full_blocks in
              if x >= ox && x < ox + plot_w then
                for yy = 0 to bar_h - 1 do
                  Canvas.plot canvas ~x ~y:(y0 + yy) ~style:st glyph
                done)
      data
  in

  (* Finally: iterate all series *)
  List.iteri
    (fun si -> function
      | Line { data; x; y; style; kind } ->
          draw_line_series si ~data ~x ~y ~style ~kind
      | Line_opt { data; x; y; style; kind } ->
          draw_line_opt_series si ~data ~x ~y ~style ~kind
      | Scatter { data; x; y; style; glyph; kind } ->
          draw_scatter_series si ~data ~x ~y ~style ~glyph ~kind
      | Bar_y { data; x; y; style } -> draw_bar_y_series si ~data ~x ~y ~style
      | Bar_x { data; y; x; style } -> draw_bar_x_series si ~data ~y ~x ~style
      | Heatmap
          {
            data;
            x;
            y;
            value;
            color_scale;
            value_range;
            auto_value_range;
            shaded;
            agg;
          } ->
          draw_heatmap_series ~data ~x ~y ~value ~color_scale ~value_range
            ~auto_value_range ~shaded ~agg
      | Circle { data; cx; cy; r; style; kind } ->
          draw_circle_series ~data ~cx ~cy ~r ~style ~kind
      | Shade_x { x0; x1; style } -> draw_shade_x_series ~x0 ~x1 ~style
      | Column_bg { x; style } -> draw_column_bg_series ~x ~style
      | Candles { data; time; open_; high; low; close; bullish; bearish } ->
          draw_candles_series ~data ~time ~open_ ~high ~low ~close ~bullish
            ~bearish
      | Bars_y_stacked { data; gap; bar_width } ->
          draw_bars_y_stacked_series ~data ~gap ~bar_width
      | Bars_x_stacked { data; gap; bar_width } ->
          draw_bars_x_stacked_series ~data ~gap ~bar_width
      | Rule_y { v; style } ->
          let st = Option.value style ~default:Style.default in
          let y = y_to_px v in
          Canvas.draw_line canvas ~x1:ox ~y1:y
            ~x2:(ox + plot_w - 1)
            ~y2:y ~style:st ~kind:`Line ()
      | Rule_x { v; style } ->
          let st = Option.value style ~default:Style.default in
          let x = x_to_px v in
          Canvas.draw_line canvas ~x1:x ~y1:oy ~x2:x
            ~y2:(oy + plot_h - 1)
            ~style:st ~kind:`Line ())
    t.series;

  (* Build and return transforms *)
  let px_to_data px py =
    let rel_x = px - ox in
    let rel_y = py - oy in
    if rel_x < 0 || rel_x >= plot_w || rel_y < 0 || rel_y >= plot_h then None
    else
      let data_x =
        Util.invert ~domain:x_dom ~range:(0., float (plot_w - 1)) (float rel_x)
      in
      (* Y is inverted: top of plot = max Y, bottom = min Y *)
      let data_y =
        Util.invert
          ~domain:(snd y_dom, fst y_dom)
          ~range:(0., float (plot_h - 1))
          (float rel_y)
      in
      Some (data_x, data_y)
  in
  let data_to_px data_x data_y = (x_to_px data_x, y_to_px data_y) in
  {
    px_to_data;
    data_to_px;
    plot_rect = { x = ox; y = oy; width = plot_w; height = plot_h };
  }

let draw t = fun canvas ~width ~height -> draw_impl t canvas ~width ~height
let draw_into t ~canvas ~width ~height = draw_impl t canvas ~width ~height

(* Pure domain helpers for zoom/pan *)

let zoom (min, max) ~factor =
  let center = (min +. max) /. 2.0 in
  let half_range = (max -. min) /. 2.0 /. factor in
  (center -. half_range, center +. half_range)

let zoom_around (min, max) ~center ~factor =
  let range = max -. min in
  let new_range = range /. factor in
  let ratio = (center -. min) /. range in
  let new_min = center -. (ratio *. new_range) in
  let new_max = new_min +. new_range in
  (new_min, new_max)

let pan (min, max) ~delta = (min +. delta, max +. delta)

let bounds data ~f =
  match data with
  | [] -> (0., 1.)
  | hd :: tl ->
      let v0 = f hd in
      List.fold_left
        (fun (mn, mx) a ->
          let v = f a in
          (Float.min mn v, Float.max mx v))
        (v0, v0) tl

(* Drawing helpers for interactive overlays *)

let draw_crosshair ?(style = Style.default) transforms (canvas : Canvas.t) ~x ~y
    =
  let px, py = transforms.data_to_px x y in
  let rect = transforms.plot_rect in
  (* Vertical line *)
  if px >= rect.x && px < rect.x + rect.width then
    Canvas.draw_line canvas ~x1:px ~y1:rect.y ~x2:px
      ~y2:(rect.y + rect.height - 1)
      ~style ~kind:`Line ();
  (* Horizontal line *)
  if py >= rect.y && py < rect.y + rect.height then
    Canvas.draw_line canvas ~x1:rect.x ~y1:py
      ~x2:(rect.x + rect.width - 1)
      ~y2:py ~style ~kind:`Line ()

let draw_marker ?(style = Style.default) ?(glyph = "●") transforms
    (canvas : Canvas.t) ~x ~y =
  let px, py = transforms.data_to_px x y in
  let rect = transforms.plot_rect in
  if
    px >= rect.x
    && px < rect.x + rect.width
    && py >= rect.y
    && py < rect.y + rect.height
  then Canvas.plot canvas ~x:px ~y:py ~style glyph

let draw_tooltip ?(style = Style.default) ?(anchor = `Right) transforms
    (canvas : Canvas.t) ~x ~y lines =
  let px, py = transforms.data_to_px x y in
  let rect = transforms.plot_rect in
  (* Only draw if within plot bounds *)
  if
    px >= rect.x
    && px < rect.x + rect.width
    && py >= rect.y
    && py < rect.y + rect.height
  then
    let max_len =
      List.fold_left (fun acc s -> max acc (String.length s)) 0 lines
    in
    let num_lines = List.length lines in
    (* Position tooltip to the right or left of the point *)
    let tooltip_x =
      match anchor with
      | `Right ->
          let x' = px + 2 in
          if x' + max_len >= rect.x + rect.width then px - max_len - 1 else x'
      | `Left ->
          let x' = px - max_len - 1 in
          if x' < rect.x then px + 2 else x'
    in
    (* Center vertically around the point *)
    let tooltip_y = py - (num_lines / 2) in
    let tooltip_y =
      max rect.y (min tooltip_y (rect.y + rect.height - num_lines))
    in
    List.iteri
      (fun i line ->
        Canvas.plot canvas ~x:tooltip_x ~y:(tooltip_y + i) ~style line)
      lines
