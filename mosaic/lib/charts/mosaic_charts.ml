module Style = Ansi.Style
module Color = Ansi.Color
module Canvas = Mosaic_ui.Canvas

(* *)

module Sparkline = Sparkline

(* --- small utilities --- *)

let clamp_int lo hi v = if v < lo then lo else if v > hi then hi else v
let clamp01 v = if v < 0. then 0. else if v > 1. then 1. else v

let safe_range (a : float) (b : float) =
  let eps = 1e-12 in
  if Float.abs (b -. a) < eps then (a -. 1.0, b +. 1.0) else (a, b)

let text_width (s : string) : int =
  (* matrix.glyph provides Glyph.measure; preserve Unicode correctness. *)
  Glyph.measure ~width_method:`Unicode ~tab_width:2 s

let utf8_of_uchar (u : Uchar.t) : string =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b u;
  Buffer.contents b

let braille_glyph_of_bits bits =
  let code = 0x2800 + bits in
  let u =
    match Uchar.of_int code with
    | exception Invalid_argument _ -> Uchar.of_int 0x2800
    | x -> x
  in
  utf8_of_uchar u

let lerp a b t = a +. ((b -. a) *. t)

(* Nice ticks algorithm: chooses step sizes from {1, 2, 5} × 10^k *)
let nice_number x round =
  let exp = Float.floor (Float.log10 x) in
  let f = x /. Float.pow 10. exp in
  let nf =
    if round then
      if f < 1.5 then 1. else if f < 3. then 2. else if f < 7. then 5. else 10.
    else if f <= 1. then 1.
    else if f <= 2. then 2.
    else if f <= 5. then 5.
    else 10.
  in
  nf *. Float.pow 10. exp

let nice_ticks ~min_val ~max_val ~target_ticks =
  if Float.abs (max_val -. min_val) < 1e-12 then [ min_val ]
  else
    let range = max_val -. min_val in
    let target_ticks = Float.max 2. (float target_ticks) in
    let step = nice_number (range /. (target_ticks -. 1.)) true in
    let start = Float.floor (min_val /. step) *. step in
    let stop = Float.ceil (max_val /. step) *. step in
    let rec gen acc v =
      if v > stop +. (step *. 0.5) then List.rev acc
      else if v >= min_val -. (step *. 0.5) && v <= max_val +. (step *. 0.5)
      then gen (v :: acc) (v +. step)
      else gen acc (v +. step)
    in
    let ticks = gen [] start in
    if ticks = [] then [ min_val; max_val ] else ticks

(* Cohen-Sutherland line clipping algorithm for geometric clipping *)
module Clip = struct
  (* Region codes for Cohen-Sutherland *)
  let inside = 0
  let left = 1
  let right = 2
  let bottom = 4
  let top = 8

  let compute_code ~xmin ~xmax ~ymin ~ymax x y =
    let code = ref inside in
    if x < xmin then code := !code lor left
    else if x > xmax then code := !code lor right;
    if y < ymin then code := !code lor bottom
    else if y > ymax then code := !code lor top;
    !code

  (* Clip a line segment to a rectangle. Returns None if completely outside,
     or Some (x1', y1', x2', y2') with clipped coordinates. *)
  let line_to_rect ~xmin ~xmax ~ymin ~ymax ~x1 ~y1 ~x2 ~y2 =
    let x1 = ref (float x1) and y1 = ref (float y1) in
    let x2 = ref (float x2) and y2 = ref (float y2) in
    let xmin = float xmin and xmax = float xmax in
    let ymin = float ymin and ymax = float ymax in
    let code1 = ref (compute_code ~xmin ~xmax ~ymin ~ymax !x1 !y1) in
    let code2 = ref (compute_code ~xmin ~xmax ~ymin ~ymax !x2 !y2) in
    let accept = ref false in
    let done_ = ref false in
    while not !done_ do
      if !code1 lor !code2 = 0 then (
        (* Both inside *)
        accept := true;
        done_ := true)
      else if !code1 land !code2 <> 0 then
        (* Both outside same region - trivially reject *)
        done_ := true
      else
        (* Needs clipping *)
        let code_out = if !code1 <> 0 then !code1 else !code2 in
        let x = ref 0. and y = ref 0. in
        let dx = !x2 -. !x1 and dy = !y2 -. !y1 in
        if code_out land top <> 0 then (
          x := !x1 +. (dx *. (ymax -. !y1) /. dy);
          y := ymax)
        else if code_out land bottom <> 0 then (
          x := !x1 +. (dx *. (ymin -. !y1) /. dy);
          y := ymin)
        else if code_out land right <> 0 then (
          y := !y1 +. (dy *. (xmax -. !x1) /. dx);
          x := xmax)
        else if code_out land left <> 0 then (
          y := !y1 +. (dy *. (xmin -. !x1) /. dx);
          x := xmin);
        if code_out = !code1 then (
          x1 := !x;
          y1 := !y;
          code1 := compute_code ~xmin ~xmax ~ymin ~ymax !x1 !y1)
        else (
          x2 := !x;
          y2 := !y;
          code2 := compute_code ~xmin ~xmax ~ymin ~ymax !x2 !y2)
    done;
    if !accept then
      Some
        ( int_of_float (Float.round !x1),
          int_of_float (Float.round !y1),
          int_of_float (Float.round !x2),
          int_of_float (Float.round !y2) )
    else None
end

(* Sub-cell precision for bar charts *)
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
    | _ -> Some "█"

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
    | _ -> Some "█"

(* --- internal Data helpers --- *)

module Data = struct
  let iter arr f = Array.iter f arr
  let fold arr ~init ~f = Array.fold_left f init arr
  let to_list arr = Array.to_list arr
end

(* --- Theme --- *)

module Theme = struct
  type t = {
    palette : Color.t list;
    background : Color.t option;
    axes : Style.t;
    grid : Style.t;
    labels : Style.t;
    tooltip : Style.t;
    tooltip_border : Style.t option;
    crosshair : Style.t;
    marker : Style.t;
  }

  let default_palette =
    [
      Color.Cyan;
      Color.Magenta;
      Color.Yellow;
      Color.Green;
      Color.Blue;
      Color.Red;
      Color.Extended 33;
      Color.Extended 39;
      Color.Extended 45;
    ]

  let dark =
    {
      palette = default_palette;
      background = None;
      axes = Style.make ~fg:(Color.grayscale ~level:13) ();
      grid = Style.make ~fg:(Color.grayscale ~level:5) ();
      labels = Style.make ~fg:(Color.grayscale ~level:14) ();
      tooltip = Style.make ~fg:Color.white ~bg:(Color.grayscale ~level:4) ();
      tooltip_border = Some (Style.make ~fg:(Color.grayscale ~level:10) ());
      crosshair = Style.make ~fg:(Color.grayscale ~level:10) ();
      marker = Style.make ~fg:Color.yellow ~bold:true ();
    }

  let light =
    {
      palette = default_palette;
      background = None;
      axes = Style.make ~fg:(Color.grayscale ~level:2) ();
      grid = Style.make ~fg:(Color.grayscale ~level:18) ();
      labels = Style.make ~fg:(Color.grayscale ~level:1) ();
      tooltip = Style.make ~fg:Color.black ~bg:(Color.grayscale ~level:20) ();
      tooltip_border = Some (Style.make ~fg:(Color.grayscale ~level:8) ());
      crosshair = Style.make ~fg:(Color.grayscale ~level:8) ();
      marker = Style.make ~fg:Color.red ~bold:true ();
    }

  let default = dark
end

(* --- Format --- *)

module Format = struct
  let float ?(precision = 3) () : int -> float -> string =
   fun _ v ->
    (* Use a general/compact float format by default. *)
    match precision with
    | 0 -> Printf.sprintf "%.0g" v
    | 1 -> Printf.sprintf "%.1g" v
    | 2 -> Printf.sprintf "%.2g" v
    | 3 -> Printf.sprintf "%.3g" v
    | 4 -> Printf.sprintf "%.4g" v
    | 5 -> Printf.sprintf "%.5g" v
    | 6 -> Printf.sprintf "%.6g" v
    | _ -> Printf.sprintf "%.*g" precision v

  let mmdd_utc _ v =
    let tm = Unix.gmtime v in
    Printf.sprintf "%02d/%02d" (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

  let hhmmss_utc _ v =
    let tm = Unix.gmtime v in
    Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec
end

(* --- Scale --- *)

module Scale = struct
  type numeric_domain = [ `Auto | `Domain of float * float ]

  type t =
    | Auto
    | Numeric of { domain : numeric_domain; clamp : bool }
    | Band of { categories : string list option; padding : float }

  let numeric ?(domain = `Auto) ?(clamp = true) () = Numeric { domain; clamp }

  let band ?categories ?(padding = 0.1) () =
    (* Clamp padding to [0, 0.95] to ensure bands have non-zero width *)
    let padding = Float.min 0.95 (clamp01 padding) in
    Band { categories; padding }
end

(* --- Axis --- *)

module Axis = struct
  type formatter = int -> float -> string

  type t = {
    show : bool;
    ticks : int;
    format : formatter;
    style : Style.t;
    tick_style : Style.t;
    label_style : Style.t;
    tick_length : int;
    label_padding : int;
  }

  let hidden =
    {
      show = false;
      ticks = 0;
      format = Format.float ();
      style = Style.default;
      tick_style = Style.default;
      label_style = Style.default;
      tick_length = 0;
      label_padding = 0;
    }

  let default =
    {
      show = true;
      ticks = 6;
      format = Format.float ();
      style = Style.default;
      tick_style = Style.default;
      label_style = Style.default;
      tick_length = 1;
      label_padding = 1;
    }

  let with_ticks ticks a = { a with ticks = max 0 ticks }
  let with_format format a = { a with format }
  let with_style style a = { a with style }
  let with_tick_style tick_style a = { a with tick_style }
  let with_label_style label_style a = { a with label_style }

  let with_tick_length tick_length a =
    { a with tick_length = max 0 tick_length }

  let with_label_padding label_padding a =
    { a with label_padding = max 0 label_padding }
end

(* --- Grid --- *)

module Grid = struct
  type t = {
    show : bool;
    x : bool;
    y : bool;
    style : Style.t;
    x_step : int option;
    y_step : int option;
  }

  let hidden =
    {
      show = false;
      x = true;
      y = true;
      style = Style.default;
      x_step = None;
      y_step = None;
    }

  let default =
    {
      show = true;
      x = true;
      y = true;
      style = Style.make ~dim:true ();
      x_step = None;
      y_step = None;
    }

  let with_style style g = { g with style }
  let with_x x g = { g with x }
  let with_y y g = { g with y }
  let with_x_step x_step g = { g with x_step }
  let with_y_step y_step g = { g with y_step }
end

(* --- View --- *)

module View = struct
  type window = { min : float; max : float }
  type t = { x : window option; y : window option }

  let empty = { x = None; y = None }
  let set_x x t = { t with x }
  let set_y y t = { t with y }

  let window ~min ~max =
    let min, max = if min <= max then (min, max) else (max, min) in
    let min, max = safe_range min max in
    { min; max }

  let zoom (w : window) ~factor =
    let factor = if factor <= 0. then 1.0 else factor in
    let center = (w.min +. w.max) /. 2.0 in
    let half = (w.max -. w.min) /. 2.0 /. factor in
    window ~min:(center -. half) ~max:(center +. half)

  let zoom_around (w : window) ~center ~factor =
    let factor = if factor <= 0. then 1.0 else factor in
    let range = w.max -. w.min in
    if Float.abs range < 1e-12 then w
    else
      let new_range = range /. factor in
      let ratio = (center -. w.min) /. range in
      let new_min = center -. (ratio *. new_range) in
      window ~min:new_min ~max:(new_min +. new_range)

  let pan (w : window) ~delta =
    window ~min:(w.min +. delta) ~max:(w.max +. delta)

  let clamp ~domain (w : window) =
    let dom = domain in
    let size = w.max -. w.min in
    if size <= 0. then dom
    else
      let min' = max dom.min (min w.min (dom.max -. size)) in
      window ~min:min' ~max:(min' +. size)
end

(* --- Marks --- *)

module Mark = struct
  type id = string
  type line_kind = [ `Line | `Braille | `Wave | `Points of string ]
  type scatter_kind = [ `Cell | `Braille ]
  type heatmap_agg = [ `Last | `Avg | `Max ]
  type heatmap_render = Cells | Dense_bilinear | Shaded
  type bar_segment = { value : float; style : Style.t; label : string option }
  type stacked_bar = { category : string; segments : bar_segment list }

  type ohlc = {
    time : float;
    open_ : float;
    high : float;
    low : float;
    close : float;
  }

  type t =
    | Line : {
        id : id option;
        style : Style.t option;
        kind : line_kind;
        x : 'a -> float;
        y : 'a -> float;
        data : 'a array;
      }
        -> t
    | Line_opt : {
        id : id option;
        style : Style.t option;
        kind : line_kind;
        x : 'a -> float;
        y : 'a -> float option;
        data : 'a array;
      }
        -> t
    | Scatter : {
        id : id option;
        style : Style.t option;
        glyph : string;
        kind : scatter_kind;
        x : 'a -> float;
        y : 'a -> float;
        data : 'a array;
      }
        -> t
    | Bars_y : {
        id : id option;
        style : Style.t option;
        x : 'a -> string;
        y : 'a -> float;
        data : 'a array;
      }
        -> t
    | Bars_x : {
        id : id option;
        style : Style.t option;
        y : 'a -> string;
        x : 'a -> float;
        data : 'a array;
      }
        -> t
    | Stacked_bars_y of {
        id : id option;
        gap : int;
        bar_width : int option;
        data : stacked_bar array;
      }
    | Stacked_bars_x of {
        id : id option;
        gap : int;
        bar_height : int option;
        data : stacked_bar array;
      }
    | Rule_y of { id : id option; style : Style.t option; y : float }
    | Rule_x of { id : id option; style : Style.t option; x : float }
    | Heatmap : {
        id : id option;
        color_scale : Color.t list;
        value_range : (float * float) option;
        auto_value_range : bool;
        agg : heatmap_agg;
        render : heatmap_render;
        x : 'a -> float;
        y : 'a -> float;
        value : 'a -> float;
        data : 'a array;
      }
        -> t
    | Candles of {
        id : id option;
        bullish : Style.t;
        bearish : Style.t;
        data : ohlc array;
      }
    | Circle : {
        id : id option;
        style : Style.t option;
        kind : [ `Line | `Braille ];
        cx : 'a -> float;
        cy : 'a -> float;
        r : 'a -> float;
        data : 'a array;
      }
        -> t
    | Shade_x of {
        id : id option;
        style : Style.t option;
        x0 : float;
        x1 : float;
      }
    | Column_background of { id : id option; style : Style.t option; x : float }

  let line ?id ?style ?(kind = (`Line : line_kind)) ~x ~y data =
    Line { id; style; kind; x; y; data }

  let line_opt ?id ?style ?(kind = (`Line : line_kind)) ~x ~y data =
    Line_opt { id; style; kind; x; y; data }

  let scatter ?id ?style ?(glyph = "∙") ?(kind = (`Cell : scatter_kind)) ~x ~y
      data =
    Scatter { id; style; glyph; kind; x; y; data }

  let bars_y ?id ?style ~x ~y data = Bars_y { id; style; x; y; data }
  let bars_x ?id ?style ~y ~x data = Bars_x { id; style; y; x; data }

  let stacked_bars_y ?id ?(gap = 1) ?bar_width data =
    Stacked_bars_y { id; gap = max 0 gap; bar_width; data }

  let stacked_bars_x ?id ?(gap = 1) ?bar_height data =
    Stacked_bars_x { id; gap = max 0 gap; bar_height; data }

  let rule_y ?id ?style y = Rule_y { id; style; y }
  let rule_x ?id ?style x = Rule_x { id; style; x }

  let heatmap ?id ?(color_scale = Theme.default.palette) ?value_range
      ?(auto_value_range = true) ?(agg = (`Last : heatmap_agg))
      ?(render = Cells) ~x ~y ~value data =
    Heatmap
      {
        id;
        color_scale;
        value_range;
        auto_value_range;
        agg;
        render;
        x;
        y;
        value;
        data;
      }

  let candles ?id ?bullish ?bearish data =
    let bullish =
      Option.value bullish ~default:(Style.fg Color.Green Style.default)
    in
    let bearish =
      Option.value bearish ~default:(Style.fg Color.Red Style.default)
    in
    Candles { id; bullish; bearish; data }

  let circle ?id ?style ?(kind = `Line) ~cx ~cy ~r data =
    Circle { id; style; kind; cx; cy; r; data }

  let shade_x ?id ?style ~min ~max () =
    let x0, x1 = if min <= max then (min, max) else (max, min) in
    Shade_x { id; style; x0; x1 }

  let column_background ?id ?style x = Column_background { id; style; x }
end

(* --- Hit --- *)

module Hit = struct
  type policy = [ `Nearest_px | `Nearest_x | `Nearest_y ]

  type kind =
    [ `Line | `Scatter | `Bars | `Stacked_bars | `Heatmap | `Candles | `Circle ]

  type payload =
    | XY of { x : float; y : float }
    | Bar of { category : string; value : float }
    | Stacked_bar of {
        category : string;
        segment_index : int;
        value : float;
        total : float;
      }
    | Heat of { x : float; y : float; value : float }
    | OHLC of {
        time : float;
        open_ : float;
        high : float;
        low : float;
        close : float;
      }

  type t = {
    mark_id : string option;
    kind : kind;
    index : int;
    px : int;
    py : int;
    distance_px : float;
    payload : payload;
  }
end

(* --- internal compiled scales --- *)

type axis_kind =
  | Numeric of { domain : View.window; view : View.window; clamp : bool }
  | Band of {
      categories : string list;
      padding : float;
      domain : View.window;
      view : View.window;
    }

type compiled_mark =
  | CLine : {
      id : string option;
      kind : Mark.line_kind;
      style : Style.t;
      x : 'a -> float;
      y : 'a -> float;
      data : 'a array;
    }
      -> compiled_mark
  | CLine_opt : {
      id : string option;
      kind : Mark.line_kind;
      style : Style.t;
      x : 'a -> float;
      y : 'a -> float option;
      data : 'a array;
    }
      -> compiled_mark
  | CScatter : {
      id : string option;
      kind : Mark.scatter_kind;
      glyph : string;
      style : Style.t;
      x : 'a -> float;
      y : 'a -> float;
      data : 'a array;
    }
      -> compiled_mark
  | CBars_y : {
      id : string option;
      style : Style.t;
      x : 'a -> string;
      y : 'a -> float;
      data : 'a array;
    }
      -> compiled_mark
  | CBars_x : {
      id : string option;
      style : Style.t;
      y : 'a -> string;
      x : 'a -> float;
      data : 'a array;
    }
      -> compiled_mark
  | CStacked_y of {
      id : string option;
      gap : int;
      bar_width : int option;
      data : Mark.stacked_bar array;
    }
  | CStacked_x of {
      id : string option;
      gap : int;
      bar_height : int option;
      data : Mark.stacked_bar array;
    }
  | CRule_y of { id : string option; style : Style.t; y : float }
  | CRule_x of { id : string option; style : Style.t; x : float }
  | CHeatmap : {
      id : string option;
      color_scale : Color.t list;
      value_range : (float * float) option;
      auto_value_range : bool;
      agg : Mark.heatmap_agg;
      render : Mark.heatmap_render;
      x : 'a -> float;
      y : 'a -> float;
      value : 'a -> float;
      data : 'a array;
    }
      -> compiled_mark
  | CCandles of {
      id : string option;
      bullish : Style.t;
      bearish : Style.t;
      data : Mark.ohlc array;
    }
  | CCircle : {
      id : string option;
      kind : [ `Line | `Braille ];
      style : Style.t;
      cx : 'a -> float;
      cy : 'a -> float;
      r : 'a -> float;
      data : 'a array;
    }
      -> compiled_mark
  | CShade_x of { id : string option; style : Style.t; x0 : float; x1 : float }
  | CColumn_bg of { id : string option; style : Style.t; x : float }

(* --- Layout --- *)

module Layout = struct
  type rect = { x : int; y : int; width : int; height : int }

  let rect_contains r ~x ~y =
    x >= r.x && x < r.x + r.width && y >= r.y && y < r.y + r.height

  type axis = [ `X | `Y | `Both ]

  type t = {
    width : int;
    height : int;
    plot : rect;
    theme : Theme.t;
    (* resolved scale info *)
    x_scale : axis_kind;
    y_scale : axis_kind;
    (* "full" domains (always exist) *)
    x_domain : View.window;
    y_domain : View.window;
    (* effective view windows used for scaling *)
    x_view : View.window;
    y_view : View.window;
    (* axis and grid config for drawing (used by Chart.draw) *)
    x_axis : Axis.t;
    y_axis : Axis.t;
    grid : Grid.t;
    frame_inner_padding : int;
    margin_left : int;
    y_axis_width : int;
    (* compiled marks (used by hit-testing) *)
    marks : compiled_mark list;
  }

  let size t = (t.width, t.height)
  let plot_rect t = t.plot
  let is_inside_plot t ~px ~py = rect_contains t.plot ~x:px ~y:py
  let x_domain t = t.x_domain
  let y_domain t = t.y_domain
  let x_view t = t.x_view
  let y_view t = t.y_view

  let data_of_px t ~px ~py =
    if not (rect_contains t.plot ~x:px ~y:py) then None
    else
      let rel_x = px - t.plot.x in
      let rel_y = py - t.plot.y in
      let w = max 1 (t.plot.width - 1) in
      let h = max 1 (t.plot.height - 1) in
      let tx = float rel_x /. float w in
      let ty = float rel_y /. float h in
      let x = lerp t.x_view.min t.x_view.max tx in
      (* y is inverted *)
      let y = lerp t.y_view.max t.y_view.min ty in
      Some (x, y)

  (* Internal: unclamped version for hit-testing - returns None if outside view *)
  let px_of_data_unclamped t ~x ~y =
    (* Handle single-cell plots *)
    if t.plot.width <= 1 && t.plot.height <= 1 then
      let tx =
        if Float.abs (t.x_view.max -. t.x_view.min) < 1e-12 then 0.
        else (x -. t.x_view.min) /. (t.x_view.max -. t.x_view.min)
      in
      let ty =
        if Float.abs (t.y_view.max -. t.y_view.min) < 1e-12 then 0.
        else (t.y_view.max -. y) /. (t.y_view.max -. t.y_view.min)
      in
      if tx < 0. || tx > 1. || ty < 0. || ty > 1. then None
      else Some (t.plot.x, t.plot.y)
    else
      let w = max 0 (t.plot.width - 1) in
      let h = max 0 (t.plot.height - 1) in
      let tx =
        if Float.abs (t.x_view.max -. t.x_view.min) < 1e-12 || w = 0 then 0.
        else (x -. t.x_view.min) /. (t.x_view.max -. t.x_view.min)
      in
      let ty =
        if Float.abs (t.y_view.max -. t.y_view.min) < 1e-12 || h = 0 then 0.
        else (t.y_view.max -. y) /. (t.y_view.max -. t.y_view.min)
      in
      (* Check if point is within view before returning *)
      if tx < 0. || tx > 1. || ty < 0. || ty > 1. then None
      else
        let px = t.plot.x + int_of_float (Float.round (tx *. float w)) in
        let py = t.plot.y + int_of_float (Float.round (ty *. float h)) in
        Some (px, py)

  let px_of_data t ~x ~y =
    (* Handle single-cell plots: always return origin *)
    if t.plot.width <= 1 && t.plot.height <= 1 then (t.plot.x, t.plot.y)
    else
      let w = max 0 (t.plot.width - 1) in
      let h = max 0 (t.plot.height - 1) in
      let tx =
        if Float.abs (t.x_view.max -. t.x_view.min) < 1e-12 || w = 0 then 0.
        else (x -. t.x_view.min) /. (t.x_view.max -. t.x_view.min)
      in
      let ty =
        if Float.abs (t.y_view.max -. t.y_view.min) < 1e-12 || h = 0 then 0.
        else (t.y_view.max -. y) /. (t.y_view.max -. t.y_view.min)
      in
      let tx = clamp01 tx in
      let ty = clamp01 ty in
      let px = t.plot.x + int_of_float (Float.round (tx *. float w)) in
      let py = t.plot.y + int_of_float (Float.round (ty *. float h)) in
      (px, py)

  (* band mapping helpers *)
  let band_params ~cats ~padding ~extent =
    let n = max 1 (List.length cats) in
    (* Clamp padding to [0, 0.95] to ensure bw > 0 and avoid division by zero *)
    let padding = Float.min 0.95 (clamp01 padding) in
    let pad = padding *. float extent in
    let band_extent = float extent -. pad in
    let bw = Float.max 1e-6 (band_extent /. float n) in
    (pad /. 2., bw)

  let band_index cats s =
    let rec aux i = function
      | [] -> None
      | c :: tl -> if String.equal c s then Some i else aux (i + 1) tl
    in
    aux 0 cats

  let x_category_of_px t ~px =
    match t.x_scale with
    | Band { categories; padding; _ } ->
        if t.plot.width <= 0 then None
        else if px < t.plot.x || px >= t.plot.x + t.plot.width then None
        else
          let offset, bw =
            band_params ~cats:categories ~padding ~extent:t.plot.width
          in
          let rel = float (px - t.plot.x) in
          let i = int_of_float ((rel -. offset) /. bw) in
          let i = clamp_int 0 (List.length categories - 1) i in
          List.nth_opt categories i
    | _ -> None

  let y_category_of_px t ~py =
    match t.y_scale with
    | Band { categories; padding; _ } ->
        if t.plot.height <= 0 then None
        else if py < t.plot.y || py >= t.plot.y + t.plot.height then None
        else
          let offset, bw =
            band_params ~cats:categories ~padding ~extent:t.plot.height
          in
          let rel = float (py - t.plot.y) in
          let i = int_of_float ((rel -. offset) /. bw) in
          let i = clamp_int 0 (List.length categories - 1) i in
          List.nth_opt categories i
    | _ -> None

  let px_of_x_category t cat =
    match t.x_scale with
    | Band { categories; padding; _ } -> (
        match band_index categories cat with
        | None -> None
        | Some i ->
            let offset, bw =
              band_params ~cats:categories ~padding ~extent:t.plot.width
            in
            let band_w = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
            let x0 = t.plot.x + int_of_float (offset +. (float i *. bw)) in
            Some (x0 + (band_w / 2)))
    | _ -> None

  let py_of_y_category t cat =
    match t.y_scale with
    | Band { categories; padding; _ } -> (
        match band_index categories cat with
        | None -> None
        | Some i ->
            let offset, bw =
              band_params ~cats:categories ~padding ~extent:t.plot.height
            in
            let band_h = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
            let y0 = t.plot.y + int_of_float (offset +. (float i *. bw)) in
            Some (y0 + (band_h / 2)))
    | _ -> None

  let clamp_view t (v : View.t) =
    let clamp_axis scale domain win_opt =
      match (scale, win_opt) with
      (* Only clamp if the scale's clamp flag is true *)
      | Numeric { clamp = true; _ }, Some w -> Some (View.clamp ~domain w)
      | Numeric { clamp = false; _ }, Some w -> Some w
      | Numeric _, None -> None
      | Band _, _ -> None
    in
    {
      View.x = clamp_axis t.x_scale t.x_domain v.View.x;
      y = clamp_axis t.y_scale t.y_domain v.View.y;
    }

  let zoom_view_around_px t ~view ~axis ~px ~py ~factor =
    let base = clamp_view t view in
    let center_x, center_y =
      match data_of_px t ~px ~py with
      | Some (x, y) -> (x, y)
      | None ->
          ( (t.x_view.min +. t.x_view.max) /. 2.0,
            (t.y_view.min +. t.y_view.max) /. 2.0 )
    in
    let zoom_x w = View.zoom_around w ~center:center_x ~factor in
    let zoom_y w = View.zoom_around w ~center:center_y ~factor in
    (* Check if zoom applies and if clamping is enabled for each axis *)
    let apply_x, clamp_x =
      match t.x_scale with
      | Numeric { clamp; _ } -> (true, clamp)
      | Band _ -> (false, false)
    in
    let apply_y, clamp_y =
      match t.y_scale with
      | Numeric { clamp; _ } -> (true, clamp)
      | Band _ -> (false, false)
    in
    let x =
      if apply_x && (axis = `X || axis = `Both) then
        let w = Option.value base.View.x ~default:t.x_view in
        let zoomed = zoom_x w in
        Some (if clamp_x then View.clamp ~domain:t.x_domain zoomed else zoomed)
      else base.View.x
    in
    let y =
      if apply_y && (axis = `Y || axis = `Both) then
        let w = Option.value base.View.y ~default:t.y_view in
        let zoomed = zoom_y w in
        Some (if clamp_y then View.clamp ~domain:t.y_domain zoomed else zoomed)
      else base.View.y
    in
    { View.x; y }

  let pan_view_by_px t ~view ~dx ~dy =
    let base = clamp_view t view in
    (* Check if pan applies and if clamping is enabled for each axis *)
    let apply_x, clamp_x =
      match t.x_scale with
      | Numeric { clamp; _ } -> (true, clamp)
      | Band _ -> (false, false)
    in
    let apply_y, clamp_y =
      match t.y_scale with
      | Numeric { clamp; _ } -> (true, clamp)
      | Band _ -> (false, false)
    in
    let pan_x =
      if not apply_x then None
      else
        let w = Option.value base.View.x ~default:t.x_view in
        let denom = float (max 1 (t.plot.width - 1)) in
        let delta = float dx *. (w.max -. w.min) /. denom in
        let panned = View.pan w ~delta in
        Some (if clamp_x then View.clamp ~domain:t.x_domain panned else panned)
    in
    let pan_y =
      if not apply_y then None
      else
        let w = Option.value base.View.y ~default:t.y_view in
        let denom = float (max 1 (t.plot.height - 1)) in
        let delta = float dy *. (w.max -. w.min) /. denom in
        let panned = View.pan w ~delta in
        Some (if clamp_y then View.clamp ~domain:t.y_domain panned else panned)
    in
    { View.x = pan_x; y = pan_y }

  let plot_center_px t =
    let px = t.plot.x + (t.plot.width / 2) in
    let py = t.plot.y + (t.plot.height / 2) in
    (px, py)

  let zoom_view_around_center t ~view ~axis ~factor =
    let px, py = plot_center_px t in
    zoom_view_around_px t ~view ~axis ~px ~py ~factor

  (* --- Hit-testing --- *)

  let hit_distance ~policy ~px ~py ~x ~y =
    let dx = float (px - x) in
    let dy = float (py - y) in
    match policy with
    | `Nearest_px -> sqrt ((dx *. dx) +. (dy *. dy))
    | `Nearest_x -> Float.abs dx
    | `Nearest_y -> Float.abs dy

  let within_radius ~radius d = d <= float radius

  let hit_test ?(radius = 3) ?(policy = (`Nearest_px : Hit.policy)) t ~px ~py =
    if not (rect_contains t.plot ~x:px ~y:py) then None
    else
      let best : Hit.t option ref = ref None in
      let consider (cand : Hit.t) =
        match !best with
        | None -> best := Some cand
        (* Use <= so that last-added mark wins ties *)
        | Some b when cand.distance_px <= b.distance_px -> best := Some cand
        | _ -> ()
      in
      (* Use unclamped version for hit-testing to filter out offscreen points *)
      let map_xy_unclamped x y = px_of_data_unclamped t ~x ~y in

      let check_points ~mark_id ~kind ~payload_of ~data_iter =
        data_iter (fun idx (x, y) ->
            (* Only consider points that are within the current view *)
            match map_xy_unclamped x y with
            | None -> () (* Point is outside view, skip it *)
            | Some (xpx, ypx) ->
                let d = hit_distance ~policy ~px ~py ~x:xpx ~y:ypx in
                if within_radius ~radius d then
                  consider
                    {
                      Hit.mark_id;
                      kind;
                      index = idx;
                      px = xpx;
                      py = ypx;
                      distance_px = d;
                      payload = payload_of x y;
                    })
      in

      let check_rects ~mark_id ~kind ~payload_of ~data_iter =
        data_iter (fun idx (x0, y0, x1, y1, payload) ->
            let x_min = min x0 x1 and x_max = max x0 x1 in
            let y_min = min y0 y1 and y_max = max y0 y1 in
            let inside =
              px >= x_min && px <= x_max && py >= y_min && py <= y_max
            in
            if inside then
              consider
                {
                  Hit.mark_id;
                  kind;
                  index = idx;
                  px;
                  py;
                  distance_px = 0.;
                  payload = payload_of payload;
                }
            else
              (* distance to rect for fallback *)
              let cx = clamp_int x_min x_max px in
              let cy = clamp_int y_min y_max py in
              let d = hit_distance ~policy ~px ~py ~x:cx ~y:cy in
              if within_radius ~radius d then
                consider
                  {
                    Hit.mark_id;
                    kind;
                    index = idx;
                    px = cx;
                    py = cy;
                    distance_px = d;
                    payload = payload_of payload;
                  })
      in

      List.iter
        (function
          | CLine { id; x; y; data; _ } ->
              check_points ~mark_id:id ~kind:`Line
                ~payload_of:(fun x y -> Hit.XY { x; y })
                ~data_iter:(fun f ->
                  let idx = ref 0 in
                  Data.iter data (fun a ->
                      let i = !idx in
                      incr idx;
                      f i (x a, y a)))
          | CLine_opt { id; x; y; data; _ } ->
              check_points ~mark_id:id ~kind:`Line
                ~payload_of:(fun x y -> Hit.XY { x; y })
                ~data_iter:(fun f ->
                  let idx = ref 0 in
                  Data.iter data (fun a ->
                      let i = !idx in
                      incr idx;
                      match y a with None -> () | Some yy -> f i (x a, yy)))
          | CScatter { id; x; y; data; _ } ->
              check_points ~mark_id:id ~kind:`Scatter
                ~payload_of:(fun x y -> Hit.XY { x; y })
                ~data_iter:(fun f ->
                  let idx = ref 0 in
                  Data.iter data (fun a ->
                      let i = !idx in
                      incr idx;
                      f i (x a, y a)))
          | CBars_y { id; x; y; data; _ } -> (
              match t.x_scale with
              | Band { categories; padding; _ } ->
                  let offset, bw =
                    band_params ~cats:categories ~padding ~extent:t.plot.width
                  in
                  let band_w = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
                  check_rects ~mark_id:id ~kind:`Bars
                    ~payload_of:(fun (cat, value) ->
                      Hit.Bar { category = cat; value })
                    ~data_iter:(fun f ->
                      let idx = ref 0 in
                      Data.iter data (fun a ->
                          let i = !idx in
                          incr idx;
                          let cat = x a in
                          match band_index categories cat with
                          | None -> ()
                          | Some bi ->
                              let x0 =
                                t.plot.x
                                + int_of_float (offset +. (float bi *. bw))
                              in
                              let x1 = x0 + band_w - 1 in
                              let v = y a in
                              let y0px =
                                snd (px_of_data t ~x:t.x_view.min ~y:v)
                              in
                              let ybase =
                                snd (px_of_data t ~x:t.x_view.min ~y:0.)
                              in
                              let y0 = min y0px ybase in
                              let y1 = max y0px ybase in
                              f i (x0, y0, x1, y1, (cat, v))))
              | _ -> ())
          | CBars_x { id; y; x; data; _ } -> (
              match t.y_scale with
              | Band { categories; padding; _ } ->
                  let offset, bw =
                    band_params ~cats:categories ~padding ~extent:t.plot.height
                  in
                  let band_h = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
                  check_rects ~mark_id:id ~kind:`Bars
                    ~payload_of:(fun (cat, value) ->
                      Hit.Bar { category = cat; value })
                    ~data_iter:(fun f ->
                      let idx = ref 0 in
                      Data.iter data (fun a ->
                          let i = !idx in
                          incr idx;
                          let cat = y a in
                          match band_index categories cat with
                          | None -> ()
                          | Some bi ->
                              let y0 =
                                t.plot.y
                                + int_of_float (offset +. (float bi *. bw))
                              in
                              let y1 = y0 + band_h - 1 in
                              let v = x a in
                              let x0px, _ =
                                px_of_data t ~x:0. ~y:t.y_view.min
                              in
                              let x1px, _ = px_of_data t ~x:v ~y:t.y_view.min in
                              let x0 = min x0px x1px in
                              let x1 = max x0px x1px in
                              f i (x0, y0, x1, y1, (cat, v))))
              | _ -> ())
          | CStacked_y { id; data; _ } -> (
              match t.x_scale with
              | Band { categories; padding; _ } ->
                  let offset, bw =
                    band_params ~cats:categories ~padding ~extent:t.plot.width
                  in
                  let band_w = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
                  check_rects ~mark_id:id ~kind:`Stacked_bars
                    ~payload_of:(fun (cat, seg_i, v, total) ->
                      Hit.Stacked_bar
                        {
                          category = cat;
                          segment_index = seg_i;
                          value = v;
                          total;
                        })
                    ~data_iter:(fun f ->
                      let idx = ref 0 in
                      Data.iter data (fun bar ->
                          let i = !idx in
                          incr idx;
                          match band_index categories bar.Mark.category with
                          | None -> ()
                          | Some bi ->
                              let x0 =
                                t.plot.x
                                + int_of_float (offset +. (float bi *. bw))
                              in
                              let x1 = x0 + band_w - 1 in
                              let total =
                                List.fold_left
                                  (fun acc s -> acc +. max 0. s.Mark.value)
                                  0. bar.segments
                              in
                              let cum = ref 0. in
                              List.iteri
                                (fun si seg ->
                                  let v = max 0. seg.Mark.value in
                                  let y0v = !cum in
                                  cum := !cum +. v;
                                  let y1v = !cum in
                                  let y0px =
                                    snd (px_of_data t ~x:t.x_view.min ~y:y0v)
                                  in
                                  let y1px =
                                    snd (px_of_data t ~x:t.x_view.min ~y:y1v)
                                  in
                                  let y0 = min y0px y1px in
                                  let y1 = max y0px y1px in
                                  f i
                                    ( x0,
                                      y0,
                                      x1,
                                      y1,
                                      (bar.category, si, v, total) ))
                                bar.segments))
              | _ -> ())
          | CStacked_x { id; data; _ } -> (
              match t.y_scale with
              | Band { categories; padding; _ } ->
                  let offset, bw =
                    band_params ~cats:categories ~padding ~extent:t.plot.height
                  in
                  let band_h = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
                  check_rects ~mark_id:id ~kind:`Stacked_bars
                    ~payload_of:(fun (cat, seg_i, v, total) ->
                      Hit.Stacked_bar
                        {
                          category = cat;
                          segment_index = seg_i;
                          value = v;
                          total;
                        })
                    ~data_iter:(fun f ->
                      let idx = ref 0 in
                      Data.iter data (fun bar ->
                          let i = !idx in
                          incr idx;
                          match band_index categories bar.Mark.category with
                          | None -> ()
                          | Some bi ->
                              let y0 =
                                t.plot.y
                                + int_of_float (offset +. (float bi *. bw))
                              in
                              let y1 = y0 + band_h - 1 in
                              let total =
                                List.fold_left
                                  (fun acc s -> acc +. max 0. s.Mark.value)
                                  0. bar.segments
                              in
                              let cum = ref 0. in
                              List.iteri
                                (fun si seg ->
                                  let v = max 0. seg.Mark.value in
                                  let x0v = !cum in
                                  cum := !cum +. v;
                                  let x1v = !cum in
                                  let x0px, _ =
                                    px_of_data t ~x:x0v ~y:t.y_view.min
                                  in
                                  let x1px, _ =
                                    px_of_data t ~x:x1v ~y:t.y_view.min
                                  in
                                  let x0 = min x0px x1px in
                                  let x1 = max x0px x1px in
                                  f i
                                    ( x0,
                                      y0,
                                      x1,
                                      y1,
                                      (bar.category, si, v, total) ))
                                bar.segments))
              | _ -> ())
          | CHeatmap { id; x; y; value = value_fn; data; _ } ->
              (* nearest-point heat hit - now passes value through *)
              let idx = ref 0 in
              Data.iter data (fun a ->
                  let i = !idx in
                  incr idx;
                  let xv = x a in
                  let yv = y a in
                  let v = value_fn a in
                  (* Only consider points within current view *)
                  match map_xy_unclamped xv yv with
                  | None -> () (* Point outside view, skip *)
                  | Some (xpx, ypx) ->
                      let d = hit_distance ~policy ~px ~py ~x:xpx ~y:ypx in
                      if within_radius ~radius d then
                        consider
                          {
                            Hit.mark_id = id;
                            kind = `Heatmap;
                            index = i;
                            px = xpx;
                            py = ypx;
                            distance_px = d;
                            payload = Hit.Heat { x = xv; y = yv; value = v };
                          })
          | CCandles { id; data; _ } ->
              let idx = ref 0 in
              Data.iter data (fun o ->
                  let i = !idx in
                  incr idx;
                  (* Check if candle's x position is within view *)
                  match map_xy_unclamped o.Mark.time t.y_view.min with
                  | None -> () (* Candle outside view, skip *)
                  | Some (cx, _) ->
                      let d = hit_distance ~policy ~px ~py ~x:cx ~y:py in
                      if within_radius ~radius d then
                        consider
                          {
                            Hit.mark_id = id;
                            kind = `Candles;
                            index = i;
                            px = cx;
                            py;
                            distance_px = d;
                            payload =
                              Hit.OHLC
                                {
                                  time = o.time;
                                  open_ = o.open_;
                                  high = o.high;
                                  low = o.low;
                                  close = o.close;
                                };
                          })
          | CCircle { id; cx; cy; data; _ } ->
              check_points ~mark_id:id ~kind:`Circle
                ~payload_of:(fun x y -> Hit.XY { x; y })
                ~data_iter:(fun f ->
                  let idx = ref 0 in
                  Data.iter data (fun a ->
                      let i = !idx in
                      incr idx;
                      f i (cx a, cy a)))
          | _ -> ())
        t.marks;
      !best
end

(* --- Overlay --- *)

module Overlay = struct
  type tooltip_anchor = [ `Auto | `Left | `Right | `Top | `Bottom ]

  let crosshair ?style (layout : Layout.t) (canvas : Canvas.t) ~x ~y =
    let style = Option.value style ~default:layout.theme.crosshair in
    let px, py = Layout.px_of_data layout ~x ~y in
    let r = Layout.plot_rect layout in
    if px >= r.x && px < r.x + r.width then
      Canvas.draw_line canvas ~x1:px ~y1:r.y ~x2:px
        ~y2:(r.y + r.height - 1)
        ~style ~kind:`Line ();
    if py >= r.y && py < r.y + r.height then
      Canvas.draw_line canvas ~x1:r.x ~y1:py
        ~x2:(r.x + r.width - 1)
        ~y2:py ~style ~kind:`Line ()

  let marker ?style ?(glyph = "●") (layout : Layout.t) (canvas : Canvas.t) ~x ~y
      =
    let style = Option.value style ~default:layout.theme.marker in
    let px, py = Layout.px_of_data layout ~x ~y in
    let r = Layout.plot_rect layout in
    if Layout.rect_contains r ~x:px ~y:py then
      Canvas.plot canvas ~x:px ~y:py ~style glyph

  let tooltip ?style ?border ?(padding = 1) ?(anchor = `Auto)
      (layout : Layout.t) (canvas : Canvas.t) ~x ~y (lines : string list) =
    let style = Option.value style ~default:layout.theme.tooltip in
    let border =
      match border with Some b -> Some b | None -> layout.theme.tooltip_border
    in
    let padding = max 0 padding in
    let px, py = Layout.px_of_data layout ~x ~y in
    let rect = Layout.plot_rect layout in
    if not (Layout.rect_contains rect ~x:px ~y:py) then ()
    else
      let lines =
        (* pad all lines to equal width for stable background fill *)
        let w = List.fold_left (fun acc s -> max acc (text_width s)) 0 lines in
        List.map
          (fun s ->
            let sw = text_width s in
            if sw >= w then s else s ^ String.make (w - sw) ' ')
          lines
      in
      let content_w =
        List.fold_left (fun acc s -> max acc (text_width s)) 0 lines
      in
      let content_h = List.length lines in
      let box_w =
        content_w + (2 * padding) + match border with Some _ -> 2 | None -> 0
      in
      let box_h =
        content_h + (2 * padding) + match border with Some _ -> 2 | None -> 0
      in
      if box_w <= 0 || box_h <= 0 then ()
      else
        (* Compute proposed position for each anchor *)
        let propose_for anchor =
          match anchor with
          | `Right -> (px + 2, py - (box_h / 2))
          | `Left -> (px - box_w - 2, py - (box_h / 2))
          | `Top -> (px - (box_w / 2), py - box_h - 2)
          | `Bottom -> (px - (box_w / 2), py + 2)
        in
        (* Calculate how much a box would be clipped at given position *)
        let clip_amount (x0, y0) =
          let left_clip = max 0 (rect.x - x0) in
          let right_clip = max 0 (x0 + box_w - (rect.x + rect.width)) in
          let top_clip = max 0 (rect.y - y0) in
          let bottom_clip = max 0 (y0 + box_h - (rect.y + rect.height)) in
          left_clip + right_clip + top_clip + bottom_clip
        in
        (* Check if position would overlap the data point *)
        let overlaps_point (x0, y0) =
          px >= x0 && px < x0 + box_w && py >= y0 && py < y0 + box_h
        in
        let x0, y0 =
          match anchor with
          | (`Left | `Right | `Top | `Bottom) as a -> propose_for a
          | `Auto ->
              (* Try anchors in preference order: Right, Left, Top, Bottom *)
              let candidates = [ `Right; `Left; `Top; `Bottom ] in
              let best = ref (propose_for `Right) in
              let best_score = ref max_int in
              List.iter
                (fun a ->
                  let pos = propose_for a in
                  let clip = clip_amount pos in
                  (* Penalize overlapping the data point *)
                  let overlap_penalty =
                    if overlaps_point pos then 1000 else 0
                  in
                  let score = clip + overlap_penalty in
                  if score < !best_score then (
                    best_score := score;
                    best := pos))
                candidates;
              !best
        in
        let x0 = clamp_int rect.x (rect.x + rect.width - box_w) x0 in
        let y0 = clamp_int rect.y (rect.y + rect.height - box_h) y0 in

        (* background fill *)
        for yy = 0 to box_h - 1 do
          for xx = 0 to box_w - 1 do
            let x = x0 + xx and y = y0 + yy in
            if Layout.rect_contains rect ~x ~y then
              Canvas.plot canvas ~x ~y ~style " "
          done
        done;

        (* border *)
        (match border with
        | None -> ()
        | Some bst ->
            if box_w >= 2 && box_h >= 2 then (
              let top = y0 and bot = y0 + box_h - 1 in
              let left = x0 and right = x0 + box_w - 1 in
              Canvas.plot canvas ~x:left ~y:top ~style:bst "┌";
              Canvas.plot canvas ~x:right ~y:top ~style:bst "┐";
              Canvas.plot canvas ~x:left ~y:bot ~style:bst "└";
              Canvas.plot canvas ~x:right ~y:bot ~style:bst "┘";
              if box_w > 2 then
                Canvas.draw_line canvas ~x1:(left + 1) ~y1:top ~x2:(right - 1)
                  ~y2:top ~style:bst ~kind:`Line ();
              if box_w > 2 then
                Canvas.draw_line canvas ~x1:(left + 1) ~y1:bot ~x2:(right - 1)
                  ~y2:bot ~style:bst ~kind:`Line ();
              if box_h > 2 then
                Canvas.draw_line canvas ~x1:left ~y1:(top + 1) ~x2:left
                  ~y2:(bot - 1) ~style:bst ~kind:`Line ();
              if box_h > 2 then
                Canvas.draw_line canvas ~x1:right ~y1:(top + 1) ~x2:right
                  ~y2:(bot - 1) ~style:bst ~kind:`Line ()));
        (* content *)
        let inner_x =
          x0 + (match border with Some _ -> 1 | None -> 0) + padding
        in
        let inner_y =
          y0 + (match border with Some _ -> 1 | None -> 0) + padding
        in
        List.iteri
          (fun i line ->
            let y = inner_y + i in
            if y >= rect.y && y < rect.y + rect.height then
              Canvas.plot canvas ~x:inner_x ~y ~style line)
          lines
end

(* --- Legend --- *)

module Legend = struct
  type item = { label : string; style : Style.t; marker : string }

  let draw ?(direction = `Vertical) ?(gap = 0) items (canvas : Canvas.t) ~width
      ~height:_ =
    match direction with
    | `Horizontal ->
        let x = ref 0 in
        let y = 0 in
        let gap = max 0 (if gap = 0 then 2 else gap) in
        List.iter
          (fun { label; style; marker } ->
            let block = marker ^ " " in
            let w_block = text_width block in
            let w_label = text_width label in
            if !x < width then (
              Canvas.plot canvas ~x:!x ~y ~style block;
              let x' = !x + w_block in
              if x' < width then Canvas.plot canvas ~x:x' ~y label;
              x := x' + w_label + gap))
          items
    | `Vertical ->
        let y = ref 0 in
        let gap = max 0 gap in
        List.iter
          (fun { label; style; marker } ->
            Canvas.plot canvas ~x:0 ~y:!y ~style marker;
            Canvas.plot canvas ~x:(text_width marker + 1) ~y:!y label;
            y := !y + 1 + gap)
          items
end

(* *)

type frame = { margins : int * int * int * int; inner_padding : int }

type t = {
  theme : Theme.t;
  frame : frame;
  x_scale : Scale.t;
  y_scale : Scale.t;
  x_axis : Axis.t;
  y_axis : Axis.t;
  grid : Grid.t;
  marks_rev : Mark.t list; (* Stored in reverse order for O(1) add *)
}

(* Get marks in correct order (reverses the internal list) *)
let marks t = List.rev t.marks_rev
let default_frame = { margins = (0, 0, 0, 0); inner_padding = 0 }

let apply_theme_defaults (theme : Theme.t) (a : Axis.t) =
  {
    a with
    style = (if a.style = Style.default then theme.axes else a.style);
    tick_style =
      (if a.tick_style = Style.default then theme.axes else a.tick_style);
    label_style =
      (if a.label_style = Style.default then theme.labels else a.label_style);
  }

let apply_grid_theme (theme : Theme.t) (g : Grid.t) =
  { g with style = (if g.style = Style.default then theme.grid else g.style) }

let empty ?(theme = Theme.default) () =
  {
    theme;
    frame = default_frame;
    x_scale = Scale.Auto;
    y_scale = Scale.Auto;
    x_axis = apply_theme_defaults theme Axis.default;
    y_axis = apply_theme_defaults theme Axis.default;
    grid = apply_grid_theme theme Grid.hidden;
    marks_rev = [];
  }

let with_theme theme t =
  {
    t with
    theme;
    x_axis = apply_theme_defaults theme t.x_axis;
    y_axis = apply_theme_defaults theme t.y_axis;
    grid = apply_grid_theme theme t.grid;
  }

let with_frame frame t =
  { t with frame = { frame with inner_padding = max 0 frame.inner_padding } }

let with_x_scale x_scale t = { t with x_scale }
let with_y_scale y_scale t = { t with y_scale }

let with_axes ?x ?y t =
  let x_axis =
    match x with None -> t.x_axis | Some a -> apply_theme_defaults t.theme a
  in
  let y_axis =
    match y with None -> t.y_axis | Some a -> apply_theme_defaults t.theme a
  in
  { t with x_axis; y_axis }

let with_grid g t = { t with grid = apply_grid_theme t.theme g }

(* O(1) mark addition by prepending to reversed list *)
let add m t = { t with marks_rev = m :: t.marks_rev }

let line ?id ?style ?kind ~x ~y data t =
  add (Mark.line ?id ?style ?kind ~x ~y data) t

let line_opt ?id ?style ?kind ~x ~y data t =
  add (Mark.line_opt ?id ?style ?kind ~x ~y data) t

let scatter ?id ?style ?glyph ?kind ~x ~y data t =
  add (Mark.scatter ?id ?style ?glyph ?kind ~x ~y data) t

let bars_y ?id ?style ~x ~y data t = add (Mark.bars_y ?id ?style ~x ~y data) t
let bars_x ?id ?style ~y ~x data t = add (Mark.bars_x ?id ?style ~y ~x data) t

let stacked_bars_y ?id ?gap ?bar_width data t =
  add (Mark.stacked_bars_y ?id ?gap ?bar_width data) t

let stacked_bars_x ?id ?gap ?bar_height data t =
  add (Mark.stacked_bars_x ?id ?gap ?bar_height data) t

let rule_y ?id ?style y t = add (Mark.rule_y ?id ?style y) t
let rule_x ?id ?style x t = add (Mark.rule_x ?id ?style x) t

let heatmap ?id ?color_scale ?value_range ?auto_value_range ?agg ?render ~x ~y
    ~value data t =
  add
    (Mark.heatmap ?id ?color_scale ?value_range ?auto_value_range ?agg ?render
       ~x ~y ~value data)
    t

let candles ?id ?bullish ?bearish data t =
  add (Mark.candles ?id ?bullish ?bearish data) t

let circle ?id ?style ?kind ~cx ~cy ~r data t =
  add (Mark.circle ?id ?style ?kind ~cx ~cy ~r data) t

let shade_x ?id ?style ~min ~max () t =
  add (Mark.shade_x ?id ?style ~min ~max ()) t

let column_background ?id ?style x t =
  add (Mark.column_background ?id ?style x) t

(* --- compilation (layout) --- *)

type dom_acc = {
  mutable has : bool;
  mutable minv : float;
  mutable maxv : float;
}

let dom_acc () = { has = false; minv = 0.; maxv = 0. }

let dom_add acc v =
  if not acc.has then (
    acc.has <- true;
    acc.minv <- v;
    acc.maxv <- v)
  else (
    acc.minv <- Float.min acc.minv v;
    acc.maxv <- Float.max acc.maxv v)

let dom_finish acc =
  if not acc.has then View.window ~min:0. ~max:1.
  else
    let mn, mx = safe_range acc.minv acc.maxv in
    View.window ~min:mn ~max:mx

let infer_band_categories_x (marks : Mark.t list) : string list =
  let seen = Hashtbl.create 64 in
  let q : string Queue.t = Queue.create () in
  let add_cat c =
    if not (Hashtbl.mem seen c) then (
      Hashtbl.add seen c ();
      Queue.add c q)
  in
  List.iter
    (function
      | Mark.Bars_y { x; data; _ } -> Data.iter data (fun a -> add_cat (x a))
      | Mark.Stacked_bars_y { data; _ } ->
          Data.iter data (fun b -> add_cat b.Mark.category)
      | _ -> ())
    marks;
  (* queue -> list *)
  let acc = ref [] in
  Queue.iter (fun x -> acc := x :: !acc) q;
  List.rev !acc

let infer_band_categories_y (marks : Mark.t list) : string list =
  let seen = Hashtbl.create 64 in
  let q : string Queue.t = Queue.create () in
  let add_cat c =
    if not (Hashtbl.mem seen c) then (
      Hashtbl.add seen c ();
      Queue.add c q)
  in
  List.iter
    (function
      | Mark.Bars_x { y; data; _ } -> Data.iter data (fun a -> add_cat (y a))
      | Mark.Stacked_bars_x { data; _ } ->
          Data.iter data (fun b -> add_cat b.Mark.category)
      | _ -> ())
    marks;
  let acc = ref [] in
  Queue.iter (fun x -> acc := x :: !acc) q;
  List.rev !acc

let infer_axis_kind_x (scale : Scale.t) marks =
  match scale with
  | Scale.Band { categories; padding } ->
      let cats =
        match categories with
        | Some c -> c
        | None -> infer_band_categories_x marks
      in
      `Band (cats, padding)
  | Scale.Numeric { clamp; domain = _ } -> `Numeric clamp
  | Scale.Auto ->
      (* if any x-band marks exist, choose band; otherwise numeric *)
      let has_band =
        List.exists
          (function
            | Mark.Bars_y _ | Mark.Stacked_bars_y _ -> true | _ -> false)
          marks
      in
      if has_band then `Band (infer_band_categories_x marks, 0.1)
      else `Numeric true

let infer_axis_kind_y (scale : Scale.t) marks =
  match scale with
  | Scale.Band { categories; padding } ->
      let cats =
        match categories with
        | Some c -> c
        | None -> infer_band_categories_y marks
      in
      `Band (cats, padding)
  | Scale.Numeric { clamp; domain = _ } -> `Numeric clamp
  | Scale.Auto ->
      let has_band =
        List.exists
          (function
            | Mark.Bars_x _ | Mark.Stacked_bars_x _ -> true | _ -> false)
          marks
      in
      if has_band then `Band (infer_band_categories_y marks, 0.1)
      else `Numeric true

let resolve_numeric_domains (t : t)
    (x_kind : [ `Band of string list * float | `Numeric of bool ])
    (y_kind : [ `Band of string list * float | `Numeric of bool ]) =
  let xacc = dom_acc () in
  let yacc = dom_acc () in

  (* include baseline 0 for bars/stacked bars on numeric axis *)
  let include_zero_y () = dom_add yacc 0. in
  let include_zero_x () = dom_add xacc 0. in

  List.iter
    (function
      | Mark.Line { x; y; data; _ } ->
          Data.iter data (fun a ->
              dom_add xacc (x a);
              dom_add yacc (y a))
      | Mark.Line_opt { x; y; data; _ } ->
          Data.iter data (fun a ->
              dom_add xacc (x a);
              match y a with None -> () | Some yy -> dom_add yacc yy)
      | Mark.Scatter { x; y; data; _ } ->
          Data.iter data (fun a ->
              dom_add xacc (x a);
              dom_add yacc (y a))
      | Mark.Bars_y { y; data; _ } ->
          include_zero_y ();
          Data.iter data (fun a -> dom_add yacc (y a))
      | Mark.Bars_x { x; data; _ } ->
          include_zero_x ();
          Data.iter data (fun a -> dom_add xacc (x a))
      | Mark.Stacked_bars_y { data; _ } ->
          include_zero_y ();
          Data.iter data (fun b ->
              let total =
                List.fold_left
                  (fun acc s -> acc +. max 0. s.Mark.value)
                  0. b.segments
              in
              dom_add yacc total)
      | Mark.Stacked_bars_x { data; _ } ->
          include_zero_x ();
          Data.iter data (fun b ->
              let total =
                List.fold_left
                  (fun acc s -> acc +. max 0. s.Mark.value)
                  0. b.segments
              in
              dom_add xacc total)
      | Mark.Rule_x { x; _ } -> dom_add xacc x
      | Mark.Rule_y { y; _ } -> dom_add yacc y
      | Mark.Heatmap { x; y; data; _ } ->
          Data.iter data (fun a ->
              dom_add xacc (x a);
              dom_add yacc (y a))
      | Mark.Candles { data; _ } ->
          Data.iter data (fun o ->
              dom_add xacc o.time;
              dom_add yacc o.open_;
              dom_add yacc o.close;
              dom_add yacc o.high;
              dom_add yacc o.low)
      | Mark.Circle { cx; cy; r; data; _ } ->
          Data.iter data (fun a ->
              let cxv = cx a and cyv = cy a and rv = max 0. (r a) in
              dom_add xacc (cxv -. rv);
              dom_add xacc (cxv +. rv);
              dom_add yacc (cyv -. rv);
              dom_add yacc (cyv +. rv))
      | Mark.Shade_x { x0; x1; _ } ->
          dom_add xacc x0;
          dom_add xacc x1
      | Mark.Column_background { x; _ } -> dom_add xacc x)
    (marks t);

  (* If axis is Band, numeric domain becomes [0,n] internal. *)
  let x_domain =
    match x_kind with
    | `Band (cats, _) ->
        let n = max 1 (List.length cats) in
        View.window ~min:0. ~max:(float n)
    | `Numeric _ -> dom_finish xacc
  in
  let y_domain =
    match y_kind with
    | `Band (cats, _) ->
        let n = max 1 (List.length cats) in
        View.window ~min:0. ~max:(float n)
    | `Numeric _ -> dom_finish yacc
  in
  (x_domain, y_domain)

let apply_domain_override (scale : Scale.t) (dom : View.window) : View.window =
  match scale with
  | Scale.Numeric { domain = `Domain (a, b); _ } -> View.window ~min:a ~max:b
  | _ -> dom

let resolve_view (dom : View.window) (view_opt : View.window option) :
    View.window =
  match view_opt with None -> dom | Some v -> View.clamp ~domain:dom v

let resolve_style_from_palette (theme : Theme.t) (i : int) : Style.t =
  let color =
    match
      List.nth_opt theme.palette (i mod max 1 (List.length theme.palette))
    with
    | None -> Color.default
    | Some c -> c
  in
  Style.fg color Style.default

let compile_marks (theme : Theme.t) (marks : Mark.t list) : compiled_mark list =
  let idx = ref 0 in
  let next_default () =
    let i = !idx in
    incr idx;
    resolve_style_from_palette theme i
  in
  let default_shade_style () =
    (* subtle default shading: bg must be set to be visible *)
    Style.make ~bg:(Color.grayscale ~level:2) ()
  in
  List.map
    (function
      | Mark.Line { id; style; kind; x; y; data } ->
          let st = Option.value style ~default:(next_default ()) in
          CLine { id; kind; style = st; x; y; data }
      | Mark.Line_opt { id; style; kind; x; y; data } ->
          let st = Option.value style ~default:(next_default ()) in
          CLine_opt { id; kind; style = st; x; y; data }
      | Mark.Scatter { id; style; glyph; kind; x; y; data } ->
          let st = Option.value style ~default:(next_default ()) in
          CScatter { id; kind; glyph; style = st; x; y; data }
      | Mark.Bars_y { id; style; x; y; data } ->
          let st = Option.value style ~default:(next_default ()) in
          CBars_y { id; style = st; x; y; data }
      | Mark.Bars_x { id; style; y; x; data } ->
          let st = Option.value style ~default:(next_default ()) in
          CBars_x { id; style = st; y; x; data }
      | Mark.Stacked_bars_y { id; gap; bar_width; data } ->
          CStacked_y { id; gap; bar_width; data }
      | Mark.Stacked_bars_x { id; gap; bar_height; data } ->
          CStacked_x { id; gap; bar_height; data }
      | Mark.Rule_y { id; style; y } ->
          let st = Option.value style ~default:(next_default ()) in
          CRule_y { id; style = st; y }
      | Mark.Rule_x { id; style; x } ->
          let st = Option.value style ~default:(next_default ()) in
          CRule_x { id; style = st; x }
      | Mark.Heatmap
          {
            id;
            color_scale;
            value_range;
            auto_value_range;
            agg;
            render;
            x;
            y;
            value;
            data;
          } ->
          CHeatmap
            {
              id;
              color_scale;
              value_range;
              auto_value_range;
              agg;
              render;
              x;
              y;
              value;
              data;
            }
      | Mark.Candles { id; bullish; bearish; data } ->
          CCandles { id; bullish; bearish; data }
      | Mark.Circle { id; style; kind; cx; cy; r; data } ->
          let st = Option.value style ~default:(next_default ()) in
          CCircle { id; kind; style = st; cx; cy; r; data }
      | Mark.Shade_x { id; style; x0; x1 } ->
          let st = Option.value style ~default:(default_shade_style ()) in
          CShade_x { id; style = st; x0; x1 }
      | Mark.Column_background { id; style; x } ->
          let st = Option.value style ~default:(default_shade_style ()) in
          CColumn_bg { id; style = st; x })
    marks

let axis_label_width_numeric (axis : Axis.t) (w : View.window) : int =
  if not axis.show then 0
  else
    let ticks = max 2 axis.ticks in
    let acc = ref 0 in
    for i = 0 to ticks - 1 do
      let t = float i /. float (ticks - 1) in
      let v = lerp w.min w.max t in
      let s = axis.format i v in
      acc := max !acc (text_width s)
    done;
    !acc

let axis_label_width_band (axis : Axis.t) (cats : string list) : int =
  if not axis.show then 0
  else List.fold_left (fun acc s -> max acc (text_width s)) 0 cats

let compute_layout ?(view = View.empty) (t : t) ~width ~height : Layout.t =
  let width = max 1 width and height = max 1 height in
  let mt, mr, mb, ml = t.frame.margins in
  let mt = max 0 mt and mr = max 0 mr and mb = max 0 mb and ml = max 0 ml in
  let ip = max 0 t.frame.inner_padding in

  let t_marks = marks t in
  let x_kind = infer_axis_kind_x t.x_scale t_marks in
  let y_kind = infer_axis_kind_y t.y_scale t_marks in

  let x_domain0, y_domain0 = resolve_numeric_domains t x_kind y_kind in
  let x_domain = apply_domain_override t.x_scale x_domain0 in
  let y_domain = apply_domain_override t.y_scale y_domain0 in

  let x_scale_res =
    match x_kind with
    | `Band (cats, padding) ->
        let dom = View.window ~min:0. ~max:(float (max 1 (List.length cats))) in
        Band { categories = cats; padding; domain = dom; view = dom }
    | `Numeric clamp ->
        let dom = x_domain in
        let v = resolve_view dom view.View.x in
        Numeric { domain = dom; view = v; clamp }
  in

  let y_scale_res =
    match y_kind with
    | `Band (cats, padding) ->
        let dom = View.window ~min:0. ~max:(float (max 1 (List.length cats))) in
        Band { categories = cats; padding; domain = dom; view = dom }
    | `Numeric clamp ->
        let dom = y_domain in
        let v = resolve_view dom view.View.y in
        Numeric { domain = dom; view = v; clamp }
  in

  let x_view =
    match x_scale_res with
    | Numeric { view; _ } -> view
    | Band { view; _ } -> view
  in
  let y_view =
    match y_scale_res with
    | Numeric { view; _ } -> view
    | Band { view; _ } -> view
  in

  (* axis reserved sizes - use domain (not view) for stable layout during zoom *)
  let y_axis_width =
    if not t.y_axis.show then 0
    else
      let label_w =
        match y_scale_res with
        | Numeric { domain; _ } -> axis_label_width_numeric t.y_axis domain
        | Band { categories; _ } -> axis_label_width_band t.y_axis categories
      in
      (* label area + padding + tick_length + axis line *)
      label_w + t.y_axis.label_padding + t.y_axis.tick_length + 1
  in
  let x_axis_height =
    if not t.x_axis.show then 0
    else
      (* axis line + tick_length + label_padding + label row *)
      1 + t.x_axis.tick_length + t.x_axis.label_padding + 1
  in

  let plot_x = ml + y_axis_width + ip in
  let plot_y = mt + ip in
  let plot_w = max 1 (width - plot_x - mr - ip) in
  let plot_h = max 1 (height - plot_y - (mb + x_axis_height + ip)) in
  let plot : Layout.rect =
    { x = plot_x; y = plot_y; width = plot_w; height = plot_h }
  in

  let compiled_marks = compile_marks t.theme t_marks in

  {
    Layout.width;
    height;
    plot;
    theme = t.theme;
    x_scale = x_scale_res;
    y_scale = y_scale_res;
    x_domain;
    y_domain;
    x_view;
    y_view;
    x_axis = t.x_axis;
    y_axis = t.y_axis;
    grid = t.grid;
    frame_inner_padding = ip;
    margin_left = ml;
    y_axis_width;
    marks = compiled_marks;
  }

let layout ?view t ~width ~height = compute_layout ?view t ~width ~height

(* --- drawing helpers --- *)

let scale_to_px ~minv ~maxv ~extent ~origin ~clamp v =
  if Float.abs (maxv -. minv) < 1e-12 then origin
  else if extent <= 1 then origin (* Single cell: always return origin *)
  else
    let t = (v -. minv) /. (maxv -. minv) in
    let t = if clamp then clamp01 t else t in
    origin + int_of_float (Float.round (t *. float (extent - 1)))

let y_to_px ~minv ~maxv ~extent ~origin ~clamp v =
  (* invert y *)
  let py = scale_to_px ~minv:maxv ~maxv:minv ~extent ~origin ~clamp v in
  py

let x_to_px ~minv ~maxv ~extent ~origin ~clamp v =
  scale_to_px ~minv ~maxv ~extent ~origin ~clamp v

let draw_grid (layout : Layout.t) (canvas : Canvas.t) =
  let g = layout.grid in
  if not g.show then ()
  else
    let r = layout.plot in
    let style = g.style in
    let draw_every_x step =
      if step > 0 then
        for x = r.x to r.x + r.width - 1 do
          if (x - r.x) mod step = 0 then
            Canvas.draw_line canvas ~x1:x ~y1:r.y ~x2:x
              ~y2:(r.y + r.height - 1)
              ~style ~kind:`Line ()
        done
    in
    let draw_every_y step =
      if step > 0 then
        for y = r.y to r.y + r.height - 1 do
          if (y - r.y) mod step = 0 then
            Canvas.draw_line canvas ~x1:r.x ~y1:y
              ~x2:(r.x + r.width - 1)
              ~y2:y ~style ~kind:`Line ()
        done
    in
    (* explicit step wins, else use nice ticks if numeric. *)
    (match g.x_step with
    | Some s -> if g.x then draw_every_x s
    | None -> (
        if g.x then
          match layout.x_scale with
          | Numeric _ ->
              let tick_values =
                nice_ticks ~min_val:layout.x_view.min ~max_val:layout.x_view.max
                  ~target_ticks:layout.x_axis.ticks
              in
              List.iter
                (fun v ->
                  let px =
                    x_to_px ~minv:layout.x_view.min ~maxv:layout.x_view.max
                      ~extent:r.width ~origin:r.x ~clamp:true v
                  in
                  Canvas.draw_line canvas ~x1:px ~y1:r.y ~x2:px
                    ~y2:(r.y + r.height - 1)
                    ~style ~kind:`Line ())
                tick_values
          | Band { categories; padding; _ } ->
              let offset, bw =
                Layout.band_params ~cats:categories ~padding ~extent:r.width
              in
              let n = List.length categories in
              for i = 0 to n do
                let x0 =
                  r.x + int_of_float (Float.round (offset +. (float i *. bw)))
                in
                Canvas.draw_line canvas ~x1:x0 ~y1:r.y ~x2:x0
                  ~y2:(r.y + r.height - 1)
                  ~style ~kind:`Line ()
              done));

    match g.y_step with
    | Some s -> if g.y then draw_every_y s
    | None -> (
        if g.y then
          match layout.y_scale with
          | Numeric _ ->
              let tick_values =
                nice_ticks ~min_val:layout.y_view.min ~max_val:layout.y_view.max
                  ~target_ticks:layout.y_axis.ticks
              in
              List.iter
                (fun v ->
                  let py =
                    y_to_px ~minv:layout.y_view.min ~maxv:layout.y_view.max
                      ~extent:r.height ~origin:r.y ~clamp:true v
                  in
                  Canvas.draw_line canvas ~x1:r.x ~y1:py
                    ~x2:(r.x + r.width - 1)
                    ~y2:py ~style ~kind:`Line ())
                tick_values
          | Band { categories; padding; _ } ->
              let offset, bw =
                Layout.band_params ~cats:categories ~padding ~extent:r.height
              in
              let n = List.length categories in
              for i = 0 to n do
                let y0 =
                  r.y + int_of_float (Float.round (offset +. (float i *. bw)))
                in
                Canvas.draw_line canvas ~x1:r.x ~y1:y0
                  ~x2:(r.x + r.width - 1)
                  ~y2:y0 ~style ~kind:`Line ()
              done)

let draw_axes (layout : Layout.t) (canvas : Canvas.t) =
  let r = layout.plot in
  let ip = layout.frame_inner_padding in

  (* y axis *)
  if layout.y_axis.show then (
    let ax = layout.margin_left + layout.y_axis_width - 1 in
    let st = layout.y_axis.style in
    Canvas.draw_line canvas ~x1:ax ~y1:r.y ~x2:ax
      ~y2:(r.y + r.height - 1)
      ~style:st ~kind:`Line ();

    (* ticks + labels - use nice ticks algorithm *)
    (match layout.y_scale with
    | Numeric _ ->
        let tick_values =
          nice_ticks ~min_val:layout.y_view.min ~max_val:layout.y_view.max
            ~target_ticks:layout.y_axis.ticks
        in
        List.iteri
          (fun i v ->
            let py =
              y_to_px ~minv:layout.y_view.min ~maxv:layout.y_view.max
                ~extent:r.height ~origin:r.y ~clamp:true v
            in
            (* tick *)
            for k = 1 to layout.y_axis.tick_length do
              let x = ax - k in
              if x >= 0 then
                Canvas.plot canvas ~x ~y:py ~style:layout.y_axis.tick_style "─"
            done;
            (* label *)
            let label = layout.y_axis.format i v in
            let lw = text_width label in
            let label_x =
              ax - layout.y_axis.tick_length - layout.y_axis.label_padding - lw
            in
            if label_x >= 0 then
              Canvas.plot canvas ~x:label_x ~y:py
                ~style:layout.y_axis.label_style label)
          tick_values
    | Band { categories; padding; _ } ->
        let offset, bw =
          Layout.band_params ~cats:categories ~padding ~extent:r.height
        in
        List.iteri
          (fun i cat ->
            let band_h = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
            let y0 = r.y + int_of_float (offset +. (float i *. bw)) in
            let py = y0 + (band_h / 2) in
            for k = 1 to layout.y_axis.tick_length do
              let x = ax - k in
              if x >= 0 then
                Canvas.plot canvas ~x ~y:py ~style:layout.y_axis.tick_style "─"
            done;
            let lw = text_width cat in
            let label_x =
              ax - layout.y_axis.tick_length - layout.y_axis.label_padding - lw
            in
            if label_x >= 0 then
              Canvas.plot canvas ~x:label_x ~y:py
                ~style:layout.y_axis.label_style cat)
          categories);

    (* y axis meets plot padding: draw a corner marker if x axis is on *)
    if layout.x_axis.show then
      let y_corner = r.y + r.height + ip in
      if y_corner >= 0 && y_corner < layout.height then
        Canvas.plot canvas ~x:ax ~y:y_corner ~style:st "└");

  (* x axis *)
  if layout.x_axis.show then (
    let ay = r.y + r.height + ip in
    let st = layout.x_axis.style in
    Canvas.draw_line canvas ~x1:r.x ~y1:ay
      ~x2:(r.x + r.width - 1)
      ~y2:ay ~style:st ~kind:`Line ();

    (* Use nice ticks algorithm for X axis with label collision avoidance *)
    match layout.x_scale with
    | Numeric _ ->
        let tick_values =
          nice_ticks ~min_val:layout.x_view.min ~max_val:layout.x_view.max
            ~target_ticks:layout.x_axis.ticks
        in
        (* Track the rightmost edge of the last rendered label *)
        let last_label_right = ref min_int in
        List.iteri
          (fun i v ->
            let px =
              x_to_px ~minv:layout.x_view.min ~maxv:layout.x_view.max
                ~extent:r.width ~origin:r.x ~clamp:true v
            in
            (* tick - always draw *)
            for k = 1 to layout.x_axis.tick_length do
              let y = ay + k in
              if y >= 0 && y < layout.height then
                Canvas.plot canvas ~x:px ~y ~style:layout.x_axis.tick_style "│"
            done;
            (* label - skip if it would overlap with previous label *)
            let label = layout.x_axis.format i v in
            let lw = text_width label in
            let label_y =
              ay + layout.x_axis.tick_length + layout.x_axis.label_padding
            in
            let label_x = px - (lw / 2) in
            (* Check for collision: need at least 1 char gap between labels *)
            if
              label_y >= 0 && label_y < layout.height
              && label_x > !last_label_right
            then (
              Canvas.plot canvas ~x:(max 0 label_x) ~y:label_y
                ~style:layout.x_axis.label_style label;
              last_label_right := label_x + lw))
          tick_values
    | Band { categories; padding; _ } ->
        let offset, bw =
          Layout.band_params ~cats:categories ~padding ~extent:r.width
        in
        (* Track the rightmost edge of the last rendered label *)
        let last_label_right = ref min_int in
        List.iteri
          (fun i cat ->
            let band_w = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
            let x0 = r.x + int_of_float (offset +. (float i *. bw)) in
            let px = x0 + (band_w / 2) in
            (* tick - always draw *)
            for k = 1 to layout.x_axis.tick_length do
              let y = ay + k in
              if y >= 0 && y < layout.height then
                Canvas.plot canvas ~x:px ~y ~style:layout.x_axis.tick_style "│"
            done;
            (* label - skip if it would overlap with previous label *)
            let lw = text_width cat in
            let label_y =
              ay + layout.x_axis.tick_length + layout.x_axis.label_padding
            in
            let label_x = px - (lw / 2) in
            if
              label_y >= 0 && label_y < layout.height
              && label_x > !last_label_right
            then (
              Canvas.plot canvas ~x:(max 0 label_x) ~y:label_y
                ~style:layout.x_axis.label_style cat;
              last_label_right := label_x + lw))
          categories)

(* wave helper (based on the old implementation) *)
let draw_wave_from_px_points canvas (r : Layout.rect) ~style
    (pts : (int * int) list) =
  let pts = List.sort (fun (x1, _) (x2, _) -> compare x1 x2) pts in
  let seq_y = Array.make r.width (-1) in
  let set_col x y =
    let ix = x - r.x in
    if ix >= 0 && ix < r.width then seq_y.(ix) <- y
  in
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
  for i = 0 to r.width - 1 do
    let y = seq_y.(i) in
    if y < 0 then prev_y := None
    else
      match !prev_y with
      | None ->
          Canvas.plot canvas ~x:(r.x + i) ~y ~style "─";
          prev_y := Some y
      | Some py ->
          let x = r.x + i in
          if py = y then Canvas.plot canvas ~x ~y ~style "─"
          else if py > y then (
            Canvas.plot canvas ~x ~y ~style "╭";
            Canvas.plot canvas ~x ~y:py ~style "╯";
            for yy = y + 1 to py - 1 do
              Canvas.plot canvas ~x ~y:yy ~style "│"
            done)
          else (
            Canvas.plot canvas ~x ~y ~style "╰";
            Canvas.plot canvas ~x ~y:py ~style "╮";
            for yy = py + 1 to y - 1 do
              Canvas.plot canvas ~x ~y:yy ~style "│"
            done);
          prev_y := Some y
  done

let heatmap_color_fun ~color_scale ~vmin ~vmax =
  let len = max 1 (List.length color_scale) in
  fun v ->
    let t =
      if Float.equal vmin vmax then 0.
      else clamp01 ((v -. vmin) /. Float.max 1e-12 (vmax -. vmin))
    in
    let raw = int_of_float (t *. float len) in
    let idx = clamp_int 0 (len - 1) raw in
    Option.value (List.nth_opt color_scale idx) ~default:Color.default

let draw_marks (layout : Layout.t) (canvas : Canvas.t) =
  let r = layout.plot in

  (* numeric mapping funcs (even if axis band, used for overlays etc.)
     We provide both clamped (for points/scatter) and unclamped (for line clipping) versions. *)
  let x_to_px_cell =
    match layout.x_scale with
    | Numeric { view; _ } ->
        fun x ->
          x_to_px ~minv:view.min ~maxv:view.max ~extent:r.width ~origin:r.x
            ~clamp:true x
    | Band _ ->
        fun x ->
          x_to_px ~minv:layout.x_view.min ~maxv:layout.x_view.max
            ~extent:r.width ~origin:r.x ~clamp:true x
  in
  (* Unclamped version for geometric clipping of lines *)
  let x_to_px_unclamped =
    match layout.x_scale with
    | Numeric { view; _ } ->
        fun x ->
          x_to_px ~minv:view.min ~maxv:view.max ~extent:r.width ~origin:r.x
            ~clamp:false x
    | Band _ ->
        fun x ->
          x_to_px ~minv:layout.x_view.min ~maxv:layout.x_view.max
            ~extent:r.width ~origin:r.x ~clamp:false x
  in
  let y_to_px_cell =
    match layout.y_scale with
    | Numeric { view; _ } ->
        fun y ->
          y_to_px ~minv:view.min ~maxv:view.max ~extent:r.height ~origin:r.y
            ~clamp:true y
    | Band _ ->
        fun y ->
          y_to_px ~minv:layout.y_view.min ~maxv:layout.y_view.max
            ~extent:r.height ~origin:r.y ~clamp:true y
  in
  (* Unclamped version for geometric clipping of lines *)
  let y_to_px_unclamped =
    match layout.y_scale with
    | Numeric { view; _ } ->
        fun y ->
          y_to_px ~minv:view.min ~maxv:view.max ~extent:r.height ~origin:r.y
            ~clamp:false y
    | Band _ ->
        fun y ->
          y_to_px ~minv:layout.y_view.min ~maxv:layout.y_view.max
            ~extent:r.height ~origin:r.y ~clamp:false y
  in

  (* helpers for band axis bar placement *)
  let x_band =
    match layout.x_scale with
    | Band { categories; padding; _ } ->
        let offset, bw =
          Layout.band_params ~cats:categories ~padding ~extent:r.width
        in
        (* Bar width: use band width minus 1 for visual gap, minimum 1 *)
        let band_w = max 1 (int_of_float bw - 1) in
        Some (categories, offset, bw, band_w)
    | _ -> None
  in
  let y_band =
    match layout.y_scale with
    | Band { categories; padding; _ } ->
        let offset, bw =
          Layout.band_params ~cats:categories ~padding ~extent:r.height
        in
        (* Bar height: use band width minus 1 for visual gap, minimum 1 *)
        let band_h = max 1 (int_of_float bw - 1) in
        Some (categories, offset, bw, band_h)
    | _ -> None
  in

  let draw_shade_x style x0 x1 =
    let px0 = x_to_px_cell x0 and px1 = x_to_px_cell x1 in
    let a = min px0 px1 and b = max px0 px1 in
    for x = a to b do
      if x >= r.x && x < r.x + r.width then
        for y = r.y to r.y + r.height - 1 do
          Canvas.plot canvas ~x ~y ~style " "
        done
    done
  in

  let draw_column_bg style x =
    let px = x_to_px_cell x in
    if px >= r.x && px < r.x + r.width then
      for y = r.y to r.y + r.height - 1 do
        Canvas.plot canvas ~x:px ~y ~style " "
      done
  in

  let draw_line_segment style ~kind (x1, y1) (x2, y2) =
    match kind with
    | `Line -> Canvas.draw_line canvas ~x1 ~y1 ~x2 ~y2 ~style ~kind:`Line ()
    | `Braille ->
        (* map already in braille coordinates; caller must provide those *)
        Canvas.draw_line canvas ~x1 ~y1 ~x2 ~y2 ~style ~kind:`Braille ()
  in

  let draw_line_series ~style ~kind ~x ~y data =
    match kind with
    | `Points glyph ->
        Data.iter data (fun a ->
            let px = x_to_px_cell (x a) in
            let py = y_to_px_cell (y a) in
            if Layout.rect_contains r ~x:px ~y:py then
              Canvas.plot canvas ~x:px ~y:py ~style glyph)
    | `Wave ->
        let pts =
          Data.fold data ~init:[] ~f:(fun acc a ->
              let px = x_to_px_cell (x a) in
              let py = y_to_px_cell (y a) in
              (px, py) :: acc)
          |> List.rev
        in
        draw_wave_from_px_points canvas r ~style pts
    | `Line ->
        (* Use unclamped coordinates and geometric clipping for proper line rendering *)
        let prev = ref None in
        let xmin = r.x and xmax = r.x + r.width - 1 in
        let ymin = r.y and ymax = r.y + r.height - 1 in
        Data.iter data (fun a ->
            let px = x_to_px_unclamped (x a) in
            let py = y_to_px_unclamped (y a) in
            (match !prev with
            | None -> ()
            | Some (px0, py0) -> (
                (* Clip line segment to plot rectangle *)
                match
                  Clip.line_to_rect ~xmin ~xmax ~ymin ~ymax ~x1:px0 ~y1:py0
                    ~x2:px ~y2:py
                with
                | Some (x1, y1, x2, y2) ->
                    Canvas.draw_line canvas ~x1 ~y1 ~x2 ~y2 ~style ~kind:`Line
                      ()
                | None -> (* Line completely outside plot *) ()));
            prev := Some (px, py))
    | `Braille ->
        (* project into 2x4 braille subgrid inside plot *)
        let gx = r.width * 2 in
        let gy = r.height * 4 in
        let xmin = layout.x_view.min and xmax = layout.x_view.max in
        let ymin = layout.y_view.min and ymax = layout.y_view.max in
        let dx = xmax -. xmin and dy = ymax -. ymin in
        let prev = ref None in
        Data.iter data (fun a ->
            let x' = x a and y' = y a in
            let sx =
              if dx <= 0. then 0. else (x' -. xmin) *. float (gx - 1) /. dx
            in
            let sy =
              if dy <= 0. then 0. else (y' -. ymin) *. float (gy - 1) /. dy
            in
            let px = (r.x * 2) + int_of_float (Float.round sx) in
            let py = (r.y * 4) + (gy - 1 - int_of_float (Float.round sy)) in
            (match !prev with
            | None -> ()
            | Some (px0, py0) ->
                draw_line_segment style ~kind:`Braille (px0, py0) (px, py));
            prev := Some (px, py))
  in

  let draw_line_opt_series ~style ~kind ~x ~y data =
    match kind with
    | `Points glyph ->
        Data.iter data (fun a ->
            match y a with
            | None -> ()
            | Some yy ->
                let px = x_to_px_cell (x a) in
                let py = y_to_px_cell yy in
                if Layout.rect_contains r ~x:px ~y:py then
                  Canvas.plot canvas ~x:px ~y:py ~style glyph)
    | `Wave ->
        (* wave: fall back to simple line segments with geometric clipping *)
        let prev = ref None in
        let xmin = r.x and xmax = r.x + r.width - 1 in
        let ymin = r.y and ymax = r.y + r.height - 1 in
        Data.iter data (fun a ->
            match y a with
            | None -> prev := None
            | Some yy ->
                let px = x_to_px_unclamped (x a) in
                let py = y_to_px_unclamped yy in
                (match !prev with
                | None -> ()
                | Some (px0, py0) -> (
                    match
                      Clip.line_to_rect ~xmin ~xmax ~ymin ~ymax ~x1:px0 ~y1:py0
                        ~x2:px ~y2:py
                    with
                    | Some (x1, y1, x2, y2) ->
                        Canvas.draw_line canvas ~x1 ~y1 ~x2 ~y2 ~style
                          ~kind:`Line ()
                    | None -> ()));
                prev := Some (px, py))
    | `Line ->
        (* Use unclamped coordinates and geometric clipping *)
        let prev = ref None in
        let xmin = r.x and xmax = r.x + r.width - 1 in
        let ymin = r.y and ymax = r.y + r.height - 1 in
        Data.iter data (fun a ->
            match y a with
            | None -> prev := None
            | Some yy ->
                let px = x_to_px_unclamped (x a) in
                let py = y_to_px_unclamped yy in
                (match !prev with
                | None -> ()
                | Some (px0, py0) -> (
                    match
                      Clip.line_to_rect ~xmin ~xmax ~ymin ~ymax ~x1:px0 ~y1:py0
                        ~x2:px ~y2:py
                    with
                    | Some (x1, y1, x2, y2) ->
                        Canvas.draw_line canvas ~x1 ~y1 ~x2 ~y2 ~style
                          ~kind:`Line ()
                    | None -> ()));
                prev := Some (px, py))
    | `Braille ->
        (* Project into 2x4 braille subgrid inside plot, same as draw_line_series *)
        let gx = r.width * 2 in
        let gy = r.height * 4 in
        let xmin = layout.x_view.min and xmax = layout.x_view.max in
        let ymin = layout.y_view.min and ymax = layout.y_view.max in
        let dx = xmax -. xmin and dy = ymax -. ymin in
        let prev = ref None in
        Data.iter data (fun a ->
            match y a with
            | None -> prev := None
            | Some yy ->
                let x' = x a in
                let sx =
                  if dx <= 0. then 0. else (x' -. xmin) *. float (gx - 1) /. dx
                in
                let sy =
                  if dy <= 0. then 0. else (yy -. ymin) *. float (gy - 1) /. dy
                in
                let px = (r.x * 2) + int_of_float (Float.round sx) in
                let py = (r.y * 4) + (gy - 1 - int_of_float (Float.round sy)) in
                (match !prev with
                | None -> ()
                | Some (px0, py0) ->
                    draw_line_segment style ~kind:`Braille (px0, py0) (px, py));
                prev := Some (px, py))
  in

  let draw_scatter_series ~style ~glyph ~kind ~x ~y data =
    match kind with
    | `Cell ->
        Data.iter data (fun a ->
            let px = x_to_px_cell (x a) in
            let py = y_to_px_cell (y a) in
            if Layout.rect_contains r ~x:px ~y:py then
              Canvas.plot canvas ~x:px ~y:py ~style glyph)
    | `Braille ->
        let gx = r.width * 2 in
        let gy = r.height * 4 in
        let xmin = layout.x_view.min and xmax = layout.x_view.max in
        let ymin = layout.y_view.min and ymax = layout.y_view.max in
        let dx = xmax -. xmin and dy = ymax -. ymin in
        let dots : (int * int, int) Hashtbl.t = Hashtbl.create 128 in
        let set_dot x_sub y_sub =
          let cell_x = x_sub / 2 in
          let cell_y = y_sub / 4 in
          if Layout.rect_contains r ~x:cell_x ~y:cell_y then
            let bit_x = x_sub land 1 in
            let bit_y = y_sub mod 4 in
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
            let cur = Option.value (Hashtbl.find_opt dots key) ~default:0 in
            Hashtbl.replace dots key (cur lor (1 lsl bit_pos))
        in
        Data.iter data (fun a ->
            let xa = x a and ya = y a in
            let sx =
              if dx <= 0. then 0. else (xa -. xmin) *. float (gx - 1) /. dx
            in
            let sy =
              if dy <= 0. then 0. else (ya -. ymin) *. float (gy - 1) /. dy
            in
            let xg = int_of_float (Float.round sx) in
            let yg = int_of_float (Float.round sy) in
            let x_sub = (r.x * 2) + xg in
            let y_sub = (r.y * 4) + (gy - 1 - yg) in
            set_dot x_sub y_sub);
        Hashtbl.iter
          (fun (cx, cy) bits ->
            let glyph = braille_glyph_of_bits bits in
            Canvas.plot canvas ~x:cx ~y:cy ~style glyph)
          dots
  in

  let draw_bars_y ~style ~x ~y data =
    match x_band with
    | None -> ()
    | Some (cats, offset, bw, band_w) ->
        (* Calculate sub-cell precision scale based on y domain *)
        let y_view = layout.y_view in
        let y_span = y_view.max -. y_view.min in
        let y_span = if y_span <= 0. then 1. else y_span in
        let scale = float r.height /. y_span in
        (* Baseline is at y=0 in data space, clamped to view *)
        let baseline_data = 0. in
        let baseline_clamped =
          Float.max y_view.min (Float.min y_view.max baseline_data)
        in
        Data.iter data (fun a ->
            let cat = x a in
            match Layout.band_index cats cat with
            | None -> ()
            | Some i -> (
                let x0 = r.x + int_of_float (offset +. (float i *. bw)) in
                let v = y a in
                (* Calculate bar extent in floating point cells from bottom *)
                let y0v = Float.min baseline_clamped v in
                let y1v = Float.max baseline_clamped v in
                let y0_f = (y0v -. y_view.min) *. scale in
                let y1_f = (y1v -. y_view.min) *. scale in
                let y0_cells = int_of_float (Float.floor y0_f) in
                let y1_cells = int_of_float (Float.floor y1_f) in
                let y1_frac = y1_f -. float y1_cells in
                let top_glyph = lower_block_glyph y1_frac in
                (* Draw full block cells from bottom of bar to top *)
                for k = y0_cells to y1_cells - 1 do
                  let yy = r.y + r.height - 1 - k in
                  if yy >= r.y && yy < r.y + r.height then
                    for xx = 0 to band_w - 1 do
                      let px = x0 + xx in
                      if px >= r.x && px < r.x + r.width then
                        Canvas.plot canvas ~x:px ~y:yy ~style "█"
                    done
                done;
                (* Draw fractional top cell if any *)
                match top_glyph with
                | None -> ()
                | Some glyph ->
                    let yy = r.y + r.height - 1 - y1_cells in
                    if yy >= r.y && yy < r.y + r.height then
                      for xx = 0 to band_w - 1 do
                        let px = x0 + xx in
                        if px >= r.x && px < r.x + r.width then
                          Canvas.plot canvas ~x:px ~y:yy ~style glyph
                      done))
  in

  let draw_bars_x ~style ~y ~x data =
    match y_band with
    | None -> ()
    | Some (cats, offset, bw, band_h) ->
        (* Calculate sub-cell precision scale based on x domain *)
        let x_view = layout.x_view in
        let x_span = x_view.max -. x_view.min in
        let x_span = if x_span <= 0. then 1. else x_span in
        let scale = float r.width /. x_span in
        (* Baseline is at x=0 in data space, clamped to view *)
        let baseline_data = 0. in
        let baseline_clamped =
          Float.max x_view.min (Float.min x_view.max baseline_data)
        in
        Data.iter data (fun a ->
            let cat = y a in
            match Layout.band_index cats cat with
            | None -> ()
            | Some i -> (
                let y0 = r.y + int_of_float (offset +. (float i *. bw)) in
                let v = x a in
                (* Calculate bar extent in floating point cells from left *)
                let x0v = Float.min baseline_clamped v in
                let x1v = Float.max baseline_clamped v in
                let x0_f = (x0v -. x_view.min) *. scale in
                let x1_f = (x1v -. x_view.min) *. scale in
                let x0_cells = int_of_float (Float.floor x0_f) in
                let x1_cells = int_of_float (Float.floor x1_f) in
                let x1_frac = x1_f -. float x1_cells in
                let right_glyph = left_block_glyph x1_frac in
                (* Draw full block cells from left of bar to right *)
                for k = x0_cells to x1_cells - 1 do
                  let xx = r.x + k in
                  if xx >= r.x && xx < r.x + r.width then
                    for yy = 0 to band_h - 1 do
                      let py = y0 + yy in
                      if py >= r.y && py < r.y + r.height then
                        Canvas.plot canvas ~x:xx ~y:py ~style "█"
                    done
                done;
                (* Draw fractional right cell if any *)
                match right_glyph with
                | None -> ()
                | Some glyph ->
                    let xx = r.x + x1_cells in
                    if xx >= r.x && xx < r.x + r.width then
                      for yy = 0 to band_h - 1 do
                        let py = y0 + yy in
                        if py >= r.y && py < r.y + r.height then
                          Canvas.plot canvas ~x:xx ~y:py ~style glyph
                      done))
  in

  let draw_stacked_y ~gap ~bar_width data =
    match x_band with
    | None -> ()
    | Some (cats, offset, bw, band_w_auto) ->
        (* Use band_params for consistent placement with hit-testing *)
        let bw_cells =
          match bar_width with
          | Some w -> w
          | None ->
              (* Apply gap reduction to band width *)
              let gap = max 0 gap in
              max 1 (int_of_float bw - gap - 1)
        in
        let bw_cells = min bw_cells band_w_auto in
        (* Calculate sub-cell precision scale based on y domain *)
        let y_view = layout.y_view in
        let y_span = y_view.max -. y_view.min in
        let y_span = if y_span <= 0. then 1. else y_span in
        let scale = float r.height /. y_span in
        Data.iter data (fun b ->
            match Layout.band_index cats b.Mark.category with
            | None -> ()
            | Some i ->
                (* Use same band_params calculation as hit-testing *)
                let x0 = r.x + int_of_float (offset +. (float i *. bw)) in
                let cum = ref 0. in
                List.iter
                  (fun seg ->
                    let v = max 0. seg.Mark.value in
                    let y0v = !cum in
                    cum := !cum +. v;
                    let y1v = !cum in
                    (* Calculate bar heights in floating point for sub-cell precision *)
                    let y0_f = (y0v -. y_view.min) *. scale in
                    let y1_f = (y1v -. y_view.min) *. scale in
                    let y0_cells = int_of_float (Float.floor y0_f) in
                    let y1_cells = int_of_float (Float.floor y1_f) in
                    let y1_frac = y1_f -. float y1_cells in
                    let top_glyph = lower_block_glyph y1_frac in
                    (* Draw full block cells from bottom of segment to top *)
                    for k = y0_cells to y1_cells - 1 do
                      let yy = r.y + r.height - 1 - k in
                      if yy >= r.y && yy < r.y + r.height then
                        for xx = 0 to bw_cells - 1 do
                          let px = x0 + xx in
                          if px >= r.x && px < r.x + r.width then
                            Canvas.plot canvas ~x:px ~y:yy ~style:seg.style "█"
                        done
                    done;
                    (* Draw fractional top cell if any *)
                    match top_glyph with
                    | None -> ()
                    | Some glyph ->
                        let yy = r.y + r.height - 1 - y1_cells in
                        if yy >= r.y && yy < r.y + r.height then
                          for xx = 0 to bw_cells - 1 do
                            let px = x0 + xx in
                            if px >= r.x && px < r.x + r.width then
                              Canvas.plot canvas ~x:px ~y:yy ~style:seg.style
                                glyph
                          done)
                  b.segments)
  in

  let draw_stacked_x ~gap ~bar_height data =
    match y_band with
    | None -> ()
    | Some (cats, offset, bw, band_h_auto) ->
        (* Use band_params for consistent placement with hit-testing *)
        let bh_cells =
          match bar_height with
          | Some h -> h
          | None ->
              (* Apply gap reduction to band height *)
              let gap = max 0 gap in
              max 1 (int_of_float bw - gap - 1)
        in
        let bh_cells = min bh_cells band_h_auto in
        (* Calculate sub-cell precision scale based on x domain *)
        let x_view = layout.x_view in
        let x_span = x_view.max -. x_view.min in
        let x_span = if x_span <= 0. then 1. else x_span in
        let scale = float r.width /. x_span in
        Data.iter data (fun b ->
            match Layout.band_index cats b.Mark.category with
            | None -> ()
            | Some i ->
                (* Use same band_params calculation as hit-testing *)
                let y0 = r.y + int_of_float (offset +. (float i *. bw)) in
                let cum = ref 0. in
                List.iter
                  (fun seg ->
                    let v = max 0. seg.Mark.value in
                    let x0v = !cum in
                    cum := !cum +. v;
                    let x1v = !cum in
                    (* Calculate bar widths in floating point for sub-cell precision *)
                    let x0_f = (x0v -. x_view.min) *. scale in
                    let x1_f = (x1v -. x_view.min) *. scale in
                    let x0_cells = int_of_float (Float.floor x0_f) in
                    let x1_cells = int_of_float (Float.floor x1_f) in
                    let x1_frac = x1_f -. float x1_cells in
                    let right_glyph = left_block_glyph x1_frac in
                    (* Draw full block cells from left of segment to right *)
                    for k = x0_cells to x1_cells - 1 do
                      let xx = r.x + k in
                      if xx >= r.x && xx < r.x + r.width then
                        for yy = 0 to bh_cells - 1 do
                          let py = y0 + yy in
                          if py >= r.y && py < r.y + r.height then
                            Canvas.plot canvas ~x:xx ~y:py ~style:seg.style "█"
                        done
                    done;
                    (* Draw fractional right cell if any *)
                    match right_glyph with
                    | None -> ()
                    | Some glyph ->
                        let xx = r.x + x1_cells in
                        if xx >= r.x && xx < r.x + r.width then
                          for yy = 0 to bh_cells - 1 do
                            let py = y0 + yy in
                            if py >= r.y && py < r.y + r.height then
                              Canvas.plot canvas ~x:xx ~y:py ~style:seg.style
                                glyph
                          done)
                  b.segments)
  in

  let draw_rule_x ~style x =
    let px = x_to_px_cell x in
    if px >= r.x && px < r.x + r.width then
      Canvas.draw_line canvas ~x1:px ~y1:r.y ~x2:px
        ~y2:(r.y + r.height - 1)
        ~style ~kind:`Line ()
  in

  let draw_rule_y ~style y =
    let py = y_to_px_cell y in
    if py >= r.y && py < r.y + r.height then
      Canvas.draw_line canvas ~x1:r.x ~y1:py
        ~x2:(r.x + r.width - 1)
        ~y2:py ~style ~kind:`Line ()
  in

  let draw_candles ~bullish ~bearish data =
    (* draw in time order *)
    let arr =
      Data.to_list data
      |> List.sort (fun a b -> compare a.Mark.time b.Mark.time)
    in
    List.iter
      (fun (o : Mark.ohlc) ->
        let cx = x_to_px_cell o.time in
        let st = if o.close >= o.open_ then bullish else bearish in
        let y_body_top = y_to_px_cell (max o.open_ o.close) in
        let y_body_bot = y_to_px_cell (min o.open_ o.close) in
        let y_wick_top = y_to_px_cell o.high in
        let y_wick_bot = y_to_px_cell o.low in
        if cx >= r.x && cx < r.x + r.width then (
          for yy = min y_wick_top y_body_top to max y_wick_top y_body_top do
            if yy >= r.y && yy < r.y + r.height then
              Canvas.plot canvas ~x:cx ~y:yy ~style:st "│"
          done;
          for yy = min y_body_top y_body_bot to max y_body_top y_body_bot do
            if yy >= r.y && yy < r.y + r.height then
              Canvas.plot canvas ~x:cx ~y:yy ~style:st "┃"
          done;
          for yy = min y_body_bot y_wick_bot to max y_body_bot y_wick_bot do
            if yy >= r.y && yy < r.y + r.height then
              Canvas.plot canvas ~x:cx ~y:yy ~style:st "│"
          done))
      arr
  in

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

  let draw_circle ~style ~kind ~cx ~cy ~radius_fn data =
    match kind with
    | `Line ->
        Data.iter data (fun a ->
            let cxv = cx a and cyv = cy a and rv = radius_fn a in
            let cx_i = int_of_float (Float.round cxv) in
            let cy_i = int_of_float (Float.round cyv) in
            let radius = int_of_float (Float.round (max 0. rv)) in
            let pts = get_circle_points ~cx:cx_i ~cy:cy_i ~radius in
            List.iter
              (fun (xd, yd) ->
                let px, py =
                  Layout.px_of_data layout ~x:(float xd) ~y:(float yd)
                in
                if Layout.rect_contains r ~x:px ~y:py then
                  Canvas.plot canvas ~x:px ~y:py ~style "█")
              pts)
    | `Braille ->
        (* accumulate braille dots inside plot *)
        let gx = r.width * 2 in
        let gy = r.height * 4 in
        let xmin = layout.x_view.min and xmax = layout.x_view.max in
        let ymin = layout.y_view.min and ymax = layout.y_view.max in
        let dx = xmax -. xmin and dy = ymax -. ymin in
        let dots : (int * int, int) Hashtbl.t = Hashtbl.create 128 in
        let set_dot x_sub y_sub =
          let cell_x = x_sub / 2 in
          let cell_y = y_sub / 4 in
          if Layout.rect_contains r ~x:cell_x ~y:cell_y then
            let bit_x = x_sub land 1 in
            let bit_y = y_sub mod 4 in
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
            let cur = Option.value (Hashtbl.find_opt dots key) ~default:0 in
            Hashtbl.replace dots key (cur lor (1 lsl bit_pos))
        in
        Data.iter data (fun a ->
            let cxv = cx a and cyv = cy a and rv = radius_fn a in
            let cx_i = int_of_float (Float.round cxv) in
            let cy_i = int_of_float (Float.round cyv) in
            let radius = int_of_float (Float.round (max 0. rv)) in
            let pts = get_circle_points ~cx:cx_i ~cy:cy_i ~radius in
            List.iter
              (fun (xd, yd) ->
                let x' = float xd and y' = float yd in
                let sx =
                  if dx <= 0. then 0. else (x' -. xmin) *. float (gx - 1) /. dx
                in
                let sy =
                  if dy <= 0. then 0. else (y' -. ymin) *. float (gy - 1) /. dy
                in
                let xg = int_of_float (Float.round sx) in
                let yg = int_of_float (Float.round sy) in
                let x_sub = (r.x * 2) + xg in
                let y_sub = (r.y * 4) + (gy - 1 - yg) in
                set_dot x_sub y_sub)
              pts);
        Hashtbl.iter
          (fun (cx, cy) bits ->
            let glyph = braille_glyph_of_bits bits in
            Canvas.plot canvas ~x:cx ~y:cy ~style glyph)
          dots
  in

  let rec draw_heatmap ~color_scale ~value_range ~auto_value_range ~agg ~render
      ~x ~y ~value data =
    let values =
      Data.fold data ~init:[] ~f:(fun acc a -> value a :: acc) |> List.rev
    in
    let vmin, vmax =
      match value_range with
      | Some (a, b) -> (a, b)
      | None -> (
          if not auto_value_range then (0., 1.)
          else
            match values with
            | [] -> (0., 1.)
            | hd :: tl ->
                List.fold_left
                  (fun (mn, mx) v -> (Float.min mn v, Float.max mx v))
                  (hd, hd) tl)
    in
    let vmin, vmax = safe_range vmin vmax in
    let color_of = heatmap_color_fun ~color_scale ~vmin ~vmax in
    let style_of_color c = Style.make ~bg:c () in

    let draw_cell px py v =
      if Layout.rect_contains r ~x:px ~y:py then
        Canvas.plot canvas ~x:px ~y:py ~style:(style_of_color (color_of v)) " "
    in

    match render with
    | Mark.Cells -> (
        (* cell aggregation based on agg *)
        let tbl : (int * int, float * int) Hashtbl.t = Hashtbl.create 256 in
        let tbl_max : (int * int, float) Hashtbl.t = Hashtbl.create 256 in
        Data.iter data (fun a ->
            let px = x_to_px_cell (x a) in
            let py = y_to_px_cell (y a) in
            let v = value a in
            if Layout.rect_contains r ~x:px ~y:py then
              match agg with
              | `Last -> Hashtbl.replace tbl_max (px, py) v
              | `Max ->
                  let cur =
                    Option.value (Hashtbl.find_opt tbl_max (px, py)) ~default:v
                  in
                  Hashtbl.replace tbl_max (px, py) (Float.max cur v)
              | `Avg ->
                  let s, n =
                    Option.value (Hashtbl.find_opt tbl (px, py)) ~default:(0., 0)
                  in
                  Hashtbl.replace tbl (px, py) (s +. v, n + 1));
        match agg with
        | `Avg ->
            Hashtbl.iter
              (fun (px, py) (s, n) ->
                if n > 0 then draw_cell px py (s /. float n))
              tbl
        | `Last | `Max ->
            Hashtbl.iter (fun (px, py) v -> draw_cell px py v) tbl_max)
    | Mark.Dense_bilinear ->
        (* bilinear upsample across plot rect using unique X/Y from inputs *)
        (* collect unique xs/ys *)
        let xs_tbl = Hashtbl.create 64 and ys_tbl = Hashtbl.create 64 in
        Data.iter data (fun a ->
            Hashtbl.replace xs_tbl (x a) ();
            Hashtbl.replace ys_tbl (y a) ());
        let xs =
          Hashtbl.to_seq_keys xs_tbl |> List.of_seq |> List.sort Float.compare
        in
        let ys =
          Hashtbl.to_seq_keys ys_tbl |> List.of_seq |> List.sort Float.compare
        in
        let nx = List.length xs and ny = List.length ys in
        if nx <= 1 || ny <= 1 then
          (* fallback *)
          draw_heatmap ~color_scale ~value_range ~auto_value_range ~agg
            ~render:Mark.Cells ~x ~y ~value data
        else
          let xs = Array.of_list xs and ys = Array.of_list ys in
          let grid : (int * int, float * int) Hashtbl.t =
            Hashtbl.create (nx * ny)
          in
          let grid_max : (int * int, float) Hashtbl.t =
            Hashtbl.create (nx * ny)
          in
          let find_interval arr v =
            let len = Array.length arr in
            if len = 1 then (0, 0, arr.(0), arr.(0))
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
          let find_index_floor arr v =
            let len = Array.length arr in
            let rec loop i =
              if i + 1 >= len then i
              else if Float.compare arr.(i + 1) v > 0 then i
              else loop (i + 1)
            in
            loop 0
          in
          (* populate grid *)
          Data.iter data (fun a ->
              let xi = find_index_floor xs (x a) in
              let yi = find_index_floor ys (y a) in
              let key = (xi, yi) in
              let v = value a in
              match agg with
              | `Last -> Hashtbl.replace grid_max key v
              | `Max ->
                  let cur =
                    Option.value (Hashtbl.find_opt grid_max key) ~default:v
                  in
                  Hashtbl.replace grid_max key (Float.max cur v)
              | `Avg ->
                  let s, n =
                    Option.value (Hashtbl.find_opt grid key) ~default:(0., 0)
                  in
                  Hashtbl.replace grid key (s +. v, n + 1));
          let lookup ix iy =
            match agg with
            | `Avg -> (
                match Hashtbl.find_opt grid (ix, iy) with
                | None -> 0.
                | Some (s, n) -> if n <= 0 then 0. else s /. float n)
            | `Last | `Max ->
                Option.value (Hashtbl.find_opt grid_max (ix, iy)) ~default:0.
          in
          let sample xq yq =
            let ix0, ix1, x0, x1 = find_interval xs xq in
            let iy0, iy1, y0, y1 = find_interval ys yq in
            let tx =
              if Float.equal x0 x1 then 0.
              else (xq -. x0) /. Float.max 1e-12 (x1 -. x0)
            in
            let ty =
              if Float.equal y0 y1 then 0.
              else (yq -. y0) /. Float.max 1e-12 (y1 -. y0)
            in
            let v00 = lookup ix0 iy0 in
            let v10 = lookup ix1 iy0 in
            let v01 = lookup ix0 iy1 in
            let v11 = lookup ix1 iy1 in
            let v0 = v00 +. ((v10 -. v00) *. tx) in
            let v1 = v01 +. ((v11 -. v01) *. tx) in
            v0 +. ((v1 -. v0) *. ty)
          in
          for py = r.y to r.y + r.height - 1 do
            for px = r.x to r.x + r.width - 1 do
              match Layout.data_of_px layout ~px ~py with
              | None -> ()
              | Some (dx, dy) ->
                  let v = sample dx dy in
                  draw_cell px py v
            done
          done
    | Mark.Shaded ->
        (* Shaded mode: bilinear upsample with text characters instead of colors *)
        let shade_chars = [| " "; "░"; "▒"; "▓"; "█" |] in
        let shade_idx v =
          let t = (v -. vmin) /. Float.max 1e-12 (vmax -. vmin) in
          let t = clamp01 t in
          max 0 (min 4 (int_of_float (t *. 4.)))
        in
        let draw_shaded_cell px py v =
          if Layout.rect_contains r ~x:px ~y:py then
            let color = color_of v in
            let st = Style.make ~fg:color () in
            Canvas.plot canvas ~x:px ~y:py ~style:st shade_chars.(shade_idx v)
        in
        (* collect unique xs/ys for bilinear interpolation *)
        let xs_tbl = Hashtbl.create 64 and ys_tbl = Hashtbl.create 64 in
        Data.iter data (fun a ->
            Hashtbl.replace xs_tbl (x a) ();
            Hashtbl.replace ys_tbl (y a) ());
        let xs =
          Hashtbl.to_seq_keys xs_tbl |> List.of_seq |> List.sort Float.compare
        in
        let ys =
          Hashtbl.to_seq_keys ys_tbl |> List.of_seq |> List.sort Float.compare
        in
        let nx = List.length xs and ny = List.length ys in
        if nx <= 1 || ny <= 1 then (
          (* sparse fallback - just plot individual cells *)
          let tbl_max : (int * int, float) Hashtbl.t = Hashtbl.create 256 in
          Data.iter data (fun a ->
              let px = x_to_px_cell (x a) in
              let py = y_to_px_cell (y a) in
              let v = value a in
              if Layout.rect_contains r ~x:px ~y:py then
                Hashtbl.replace tbl_max (px, py) v);
          Hashtbl.iter (fun (px, py) v -> draw_shaded_cell px py v) tbl_max)
        else
          let xs = Array.of_list xs and ys = Array.of_list ys in
          let grid : (int * int, float * int) Hashtbl.t =
            Hashtbl.create (nx * ny)
          in
          let grid_max : (int * int, float) Hashtbl.t =
            Hashtbl.create (nx * ny)
          in
          let find_interval arr v =
            let len = Array.length arr in
            if len = 1 then (0, 0, arr.(0), arr.(0))
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
          let find_index_floor arr v =
            let len = Array.length arr in
            let rec loop i =
              if i + 1 >= len then i
              else if Float.compare arr.(i + 1) v > 0 then i
              else loop (i + 1)
            in
            loop 0
          in
          (* populate grid *)
          Data.iter data (fun a ->
              let xi = find_index_floor xs (x a) in
              let yi = find_index_floor ys (y a) in
              let key = (xi, yi) in
              let v = value a in
              match agg with
              | `Last -> Hashtbl.replace grid_max key v
              | `Max ->
                  let cur =
                    Option.value (Hashtbl.find_opt grid_max key) ~default:v
                  in
                  Hashtbl.replace grid_max key (Float.max cur v)
              | `Avg ->
                  let s, n =
                    Option.value (Hashtbl.find_opt grid key) ~default:(0., 0)
                  in
                  Hashtbl.replace grid key (s +. v, n + 1));
          let lookup ix iy =
            match agg with
            | `Avg -> (
                match Hashtbl.find_opt grid (ix, iy) with
                | None -> 0.
                | Some (s, n) -> if n <= 0 then 0. else s /. float n)
            | `Last | `Max ->
                Option.value (Hashtbl.find_opt grid_max (ix, iy)) ~default:0.
          in
          let sample xq yq =
            let ix0, ix1, x0, x1 = find_interval xs xq in
            let iy0, iy1, y0, y1 = find_interval ys yq in
            let tx =
              if Float.equal x0 x1 then 0.
              else (xq -. x0) /. Float.max 1e-12 (x1 -. x0)
            in
            let ty =
              if Float.equal y0 y1 then 0.
              else (yq -. y0) /. Float.max 1e-12 (y1 -. y0)
            in
            let v00 = lookup ix0 iy0 in
            let v10 = lookup ix1 iy0 in
            let v01 = lookup ix0 iy1 in
            let v11 = lookup ix1 iy1 in
            let v0 = v00 +. ((v10 -. v00) *. tx) in
            let v1 = v01 +. ((v11 -. v01) *. tx) in
            v0 +. ((v1 -. v0) *. ty)
          in
          for py = r.y to r.y + r.height - 1 do
            for px = r.x to r.x + r.width - 1 do
              match Layout.data_of_px layout ~px ~py with
              | None -> ()
              | Some (dx, dy) ->
                  let v = sample dx dy in
                  draw_shaded_cell px py v
            done
          done
  in

  (* Iterate marks in the order given (layering) *)
  List.iter
    (function
      | CShade_x { style; x0; x1; _ } -> draw_shade_x style x0 x1
      | CColumn_bg { style; x; _ } -> draw_column_bg style x
      | CLine { style; kind; x; y; data; _ } ->
          draw_line_series ~style ~kind ~x ~y data
      | CLine_opt { style; kind; x; y; data; _ } ->
          draw_line_opt_series ~style ~kind ~x ~y data
      | CScatter { style; glyph; kind; x; y; data; _ } ->
          draw_scatter_series ~style ~glyph ~kind ~x ~y data
      | CBars_y { style; x; y; data; _ } -> draw_bars_y ~style ~x ~y data
      | CBars_x { style; y; x; data; _ } -> draw_bars_x ~style ~y ~x data
      | CStacked_y { gap; bar_width; data; _ } ->
          draw_stacked_y ~gap ~bar_width data
      | CStacked_x { gap; bar_height; data; _ } ->
          draw_stacked_x ~gap ~bar_height data
      | CRule_x { style; x; _ } -> draw_rule_x ~style x
      | CRule_y { style; y; _ } -> draw_rule_y ~style y
      | CHeatmap
          {
            color_scale;
            value_range;
            auto_value_range;
            agg;
            render;
            x;
            y;
            value;
            data;
            _;
          } ->
          draw_heatmap ~color_scale ~value_range ~auto_value_range ~agg ~render
            ~x ~y ~value data
      | CCandles { bullish; bearish; data; _ } ->
          draw_candles ~bullish ~bearish data
      | CCircle { style; kind; cx; cy; r; data; _ } ->
          draw_circle ~style ~kind ~cx ~cy ~radius_fn:r data)
    layout.marks

let draw ?view (t : t) (canvas : Canvas.t) ~width ~height : Layout.t =
  let layout = compute_layout ?view t ~width ~height in
  Canvas.clear canvas;
  (* background fill (entire canvas) if requested *)
  (match t.theme.background with
  | None -> ()
  | Some bg -> Canvas.fill_rect canvas ~x:0 ~y:0 ~width ~height ~color:bg);

  (* clear plot area if background exists? If no background, we still want a clean plot region. *)
  (match t.theme.background with
  | None -> ()
  | Some bg ->
      Canvas.fill_rect canvas ~x:layout.plot.x ~y:layout.plot.y
        ~width:layout.plot.width ~height:layout.plot.height ~color:bg);

  draw_grid layout canvas;
  draw_marks layout canvas;
  draw_axes layout canvas;
  layout
