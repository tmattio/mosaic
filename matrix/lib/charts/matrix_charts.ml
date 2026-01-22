module Style = Ansi.Style
module Color = Ansi.Color
module G = Grid

(* *)

module Sparkline = Sparkline

(* --- Charset --- *)

module Charset = struct
  type line_pattern = [ `Solid | `Dashed | `Dotted ]

  type frame = {
    tl : string;
    tr : string;
    bl : string;
    br : string;
    h : string;
    v : string;
    tee_up : string;
    tee_down : string;
    tee_left : string;
    tee_right : string;
    cross : string;
  }

  type t = {
    frame : frame;
    axis_h : string;
    axis_v : string;
    tick_h : string;
    tick_v : string;
    grid_h_solid : string;
    grid_v_solid : string;
    grid_h_dashed : string;
    grid_v_dashed : string;
    grid_h_dotted : string;
    grid_v_dotted : string;
    point_default : string;
    point_heavy : string;
    bar_fill : string;
    shade_levels : string array;
    tooltip_frame : frame;
    diag_up : string;
    diag_down : string;
  }

  let ascii_frame =
    {
      tl = "+";
      tr = "+";
      bl = "+";
      br = "+";
      h = "-";
      v = "|";
      tee_up = "+";
      tee_down = "+";
      tee_left = "+";
      tee_right = "+";
      cross = "+";
    }

  let light_frame =
    {
      tl = "┌";
      tr = "┐";
      bl = "└";
      br = "┘";
      h = "─";
      v = "│";
      tee_up = "┴";
      tee_down = "┬";
      tee_left = "┤";
      tee_right = "├";
      cross = "┼";
    }

  let heavy_frame =
    {
      tl = "┏";
      tr = "┓";
      bl = "┗";
      br = "┛";
      h = "━";
      v = "┃";
      tee_up = "┻";
      tee_down = "┳";
      tee_left = "┫";
      tee_right = "┣";
      cross = "╋";
    }

  let rounded_frame =
    {
      tl = "╭";
      tr = "╮";
      bl = "╰";
      br = "╯";
      h = "─";
      v = "│";
      tee_up = "┴";
      tee_down = "┬";
      tee_left = "┤";
      tee_right = "├";
      cross = "┼";
    }

  let ascii =
    {
      frame = ascii_frame;
      axis_h = "-";
      axis_v = "|";
      tick_h = "-";
      tick_v = "|";
      grid_h_solid = "-";
      grid_v_solid = "|";
      grid_h_dashed = "-";
      grid_v_dashed = "|";
      grid_h_dotted = ".";
      grid_v_dotted = ":";
      point_default = "*";
      point_heavy = "O";
      bar_fill = "#";
      shade_levels = [| " "; "."; "o"; "O"; "#" |];
      tooltip_frame = ascii_frame;
      diag_up = "/";
      diag_down = "\\";
    }

  let unicode_light =
    {
      frame = light_frame;
      axis_h = "─";
      axis_v = "│";
      tick_h = "─";
      tick_v = "│";
      grid_h_solid = "─";
      grid_v_solid = "│";
      grid_h_dashed = "┄";
      grid_v_dashed = "┆";
      grid_h_dotted = "┈";
      grid_v_dotted = "┊";
      point_default = "∙";
      point_heavy = "●";
      bar_fill = "█";
      shade_levels = [| " "; "░"; "▒"; "▓"; "█" |];
      tooltip_frame = rounded_frame;
      diag_up = "╱";
      diag_down = "╲";
    }

  let unicode_heavy =
    {
      frame = heavy_frame;
      axis_h = "━";
      axis_v = "┃";
      tick_h = "━";
      tick_v = "┃";
      grid_h_solid = "━";
      grid_v_solid = "┃";
      grid_h_dashed = "┅";
      grid_v_dashed = "┇";
      grid_h_dotted = "┉";
      grid_v_dotted = "┋";
      point_default = "∙";
      point_heavy = "●";
      bar_fill = "█";
      shade_levels = [| " "; "░"; "▒"; "▓"; "█" |];
      tooltip_frame = heavy_frame;
      diag_up = "╱";
      diag_down = "╲";
    }

  let unicode_rounded =
    {
      frame = rounded_frame;
      axis_h = "─";
      axis_v = "│";
      tick_h = "─";
      tick_v = "│";
      grid_h_solid = "─";
      grid_v_solid = "│";
      grid_h_dashed = "┄";
      grid_v_dashed = "┆";
      grid_h_dotted = "┈";
      grid_v_dotted = "┊";
      point_default = "∙";
      point_heavy = "●";
      bar_fill = "█";
      shade_levels = [| " "; "░"; "▒"; "▓"; "█" |];
      tooltip_frame = rounded_frame;
      diag_up = "╱";
      diag_down = "╲";
    }

  let default = unicode_light
end

(* --- Raster --- *)

module Raster = struct
  type resolution = [ `Cell | `Wave | `Block2x2 | `Braille2x4 ]
end

(* --- small utilities --- *)

let clamp_int lo hi v = if v < lo then lo else if v > hi then hi else v
let clamp01 v = if v < 0. then 0. else if v > 1. then 1. else v

(* Safe range for domain inference: when the inferred domain is degenerate (min
   == max), expand it by an absolute amount. This is appropriate for domain
   inference where we don't know the scale of the data. *)
let safe_domain_range (a : float) (b : float) =
  let eps = 1e-12 in
  if Float.abs (b -. a) < eps then (a -. 1.0, b +. 1.0) else (a, b)

(* Safe range for view windows: when the view range is degenerate, expand it
   relative to the magnitude of the values. This prevents large absolute
   expansions when working with small-scale data. Guarantees the resulting span
   is always >= epsilon (1e-12). *)
let safe_view_range (a : float) (b : float) =
  let epsilon = 1e-12 in
  let span = Float.abs (b -. a) in
  if span >= epsilon then (a, b)
  else
    let scale = Float.max (Float.abs a) (Float.abs b) in
    (* expansion must be at least epsilon/2 so that 2*expansion >= epsilon *)
    let expansion = Float.max (epsilon /. 2.) (1e-6 *. scale) in
    (a -. expansion, b +. expansion)

(* Legacy alias for backwards compatibility *)
let safe_range = safe_domain_range

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

(* Block2x2 quadrant glyphs: 4 bits = [top-left, top-right, bottom-left,
   bottom-right] Bit layout: bit 0 = top-left, bit 1 = top-right, bit 2 =
   bottom-left, bit 3 = bottom-right *)
let quadrant_glyphs =
  [|
    " ";
    (* 0000 *)
    "▘";
    (* 0001 - top-left *)
    "▝";
    (* 0010 - top-right *)
    "▀";
    (* 0011 - top half *)
    "▖";
    (* 0100 - bottom-left *)
    "▌";
    (* 0101 - left half *)
    "▞";
    (* 0110 - diagonal: top-right + bottom-left *)
    "▛";
    (* 0111 - all but bottom-right *)
    "▗";
    (* 1000 - bottom-right *)
    "▚";
    (* 1001 - diagonal: top-left + bottom-right *)
    "▐";
    (* 1010 - right half *)
    "▜";
    (* 1011 - all but bottom-left *)
    "▄";
    (* 1100 - bottom half *)
    "▙";
    (* 1101 - all but top-right *)
    "▟";
    (* 1110 - all but top-left *)
    "█";
    (* 1111 - full block *)
  |]

let quadrant_glyph_of_bits bits = quadrant_glyphs.(bits land 0xF)
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

(* Log scale transformation helper *)
let log_transform ~base v =
  if v <= 0. then Float.neg_infinity else Float.log v /. Float.log base

(* Generate nice tick values for log scale *)
let log_ticks ~base ~min_val ~max_val ~target_ticks =
  if min_val <= 0. || max_val <= 0. then [ min_val; max_val ]
  else
    let log_min = Float.floor (log_transform ~base min_val) in
    let log_max = Float.ceil (log_transform ~base max_val) in
    let num_decades = log_max -. log_min in
    (* Generate major ticks at powers of base *)
    let rec gen_major acc exp =
      if exp > log_max +. 0.5 then List.rev acc
      else
        let v = Float.pow base exp in
        if v >= min_val *. 0.999 && v <= max_val *. 1.001 then
          gen_major (v :: acc) (exp +. 1.)
        else gen_major acc (exp +. 1.)
    in
    let major = gen_major [] log_min in
    (* If we have very few ticks and room for more, add minor ticks *)
    if List.length major < target_ticks && num_decades <= 3. then
      (* Add 2 and 5 between powers for base 10 *)
      let minors =
        if base = 10. then
          let rec gen_minor acc exp =
            if exp >= log_max then acc
            else
              let base_val = Float.pow base exp in
              let m2 = base_val *. 2. in
              let m5 = base_val *. 5. in
              let acc =
                if m2 >= min_val && m2 <= max_val then m2 :: acc else acc
              in
              let acc =
                if m5 >= min_val && m5 <= max_val then m5 :: acc else acc
              in
              gen_minor acc (exp +. 1.)
          in
          gen_minor [] log_min
        else []
      in
      List.sort Float.compare (major @ minors)
    else if major = [] then [ min_val; max_val ]
    else major

let draw_text ?style ?tab_width grid ~x ~y text =
  G.draw_text ?style ?tab_width grid ~x ~y ~text

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

  (* Clip a line segment to a rectangle. Returns None if completely outside, or
     Some (x1', y1', x2', y2') with clipped coordinates. *)
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
        (* Guard against divide-by-zero for horizontal/vertical lines. For
           horizontal lines (dy=0), top/bottom clips are impossible since
           code_out can only have left/right bits set. Similarly for vertical
           lines (dx=0), left/right clips are impossible. However, we guard
           anyway for safety in case of floating point edge cases. *)
        if code_out land top <> 0 then (
          x := if dy = 0. then !x1 else !x1 +. (dx *. (ymax -. !y1) /. dy);
          y := ymax)
        else if code_out land bottom <> 0 then (
          x := if dy = 0. then !x1 else !x1 +. (dx *. (ymin -. !y1) /. dy);
          y := ymin)
        else if code_out land right <> 0 then (
          y := if dx = 0. then !y1 else !y1 +. (dy *. (xmax -. !x1) /. dx);
          x := xmax)
        else if code_out land left <> 0 then (
          y := if dx = 0. then !y1 else !y1 +. (dy *. (xmin -. !x1) /. dx);
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

(* Upper block glyphs fill from the top down (▔▀ etc.) Note: Unicode only has ▀
   (upper half) and ▔ (upper eighth). For intermediate values, we use ▀ with
   proportional coverage. *)
let upper_block_glyph frac =
  if frac <= 0. then None
  else if frac >= 1. then Some "█"
  else
    let eighth = 0.125 in
    let e = int_of_float (Float.floor (frac /. eighth)) in
    let rem = frac -. (float e *. eighth) in
    let e = if rem >= 0.0625 then e + 1 else e in
    match e with
    | 0 -> None
    | 1 -> Some "▔"
    | 2 -> Some "▔" (* No 2/8 upper block in Unicode, use 1/8 *)
    | 3 -> Some "▀" (* Use half block for 3/8 *)
    | 4 -> Some "▀"
    | 5 -> Some "▀" (* No 5/8 upper block, use half *)
    | 6 -> Some "▀" (* No 6/8 upper block, use half *)
    | 7 -> Some "█" (* Close to full *)
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

(* Right block glyphs fill from the right side (▕▐ etc.) Note: Unicode only has
   ▐ (right half) and ▕ (right eighth). *)
let right_block_glyph frac =
  if frac <= 0. then None
  else if frac >= 1. then Some "█"
  else
    let eighth = 0.125 in
    let e = int_of_float (Float.floor (frac /. eighth)) in
    let rem = frac -. (float e *. eighth) in
    let e = if rem >= 0.0625 then e + 1 else e in
    match e with
    | 0 -> None
    | 1 -> Some "▕"
    | 2 -> Some "▕" (* No 2/8 right block in Unicode *)
    | 3 -> Some "▐" (* Use half block *)
    | 4 -> Some "▐"
    | 5 -> Some "▐"
    | 6 -> Some "▐"
    | 7 -> Some "█"
    | _ -> Some "█"

(* --- internal Data helpers --- *)

module Data = struct
  let iter arr f = Array.iter f arr
end

(* --- Theme --- *)

module Theme = struct
  type t = {
    palette : Color.t array;
    background : Color.t option;
    axes : Style.t;
    border : Style.t;
    grid : Style.t;
    grid_minor : Style.t;
    labels : Style.t;
    tooltip : Style.t;
    tooltip_border : Style.t option;
    crosshair : Style.t;
    marker : Style.t;
    charset : Charset.t;
  }

  let default_palette =
    [|
      Color.Cyan;
      Color.Magenta;
      Color.Yellow;
      Color.Green;
      Color.Blue;
      Color.Red;
      Color.Extended 33;
      Color.Extended 39;
      Color.Extended 45;
    |]

  let dark =
    {
      palette = default_palette;
      background = None;
      axes = Style.make ~fg:(Color.grayscale ~level:13) ();
      border = Style.make ~fg:(Color.grayscale ~level:10) ();
      grid = Style.make ~fg:(Color.grayscale ~level:5) ();
      grid_minor = Style.make ~fg:(Color.grayscale ~level:3) ();
      labels = Style.make ~fg:(Color.grayscale ~level:14) ();
      tooltip = Style.make ~fg:Color.white ~bg:(Color.grayscale ~level:4) ();
      tooltip_border = Some (Style.make ~fg:(Color.grayscale ~level:10) ());
      crosshair = Style.make ~fg:(Color.grayscale ~level:10) ();
      marker = Style.make ~fg:Color.yellow ~bold:true ();
      charset = Charset.unicode_light;
    }

  let light =
    {
      palette = default_palette;
      background = None;
      axes = Style.make ~fg:(Color.grayscale ~level:2) ();
      border = Style.make ~fg:(Color.grayscale ~level:5) ();
      grid = Style.make ~fg:(Color.grayscale ~level:18) ();
      grid_minor = Style.make ~fg:(Color.grayscale ~level:20) ();
      labels = Style.make ~fg:(Color.grayscale ~level:1) ();
      tooltip = Style.make ~fg:Color.black ~bg:(Color.grayscale ~level:20) ();
      tooltip_border = Some (Style.make ~fg:(Color.grayscale ~level:8) ());
      crosshair = Style.make ~fg:(Color.grayscale ~level:8) ();
      marker = Style.make ~fg:Color.red ~bold:true ();
      charset = Charset.unicode_light;
    }

  let default = dark
  let with_charset charset t = { t with charset }
end

(* --- Label_format --- *)

(* Renamed from Format to avoid confusion with Stdlib.Format *)
module Label_format = struct
  let float ?(precision = 3) () : int -> float -> string =
   fun _ v -> Printf.sprintf "%.*g" precision v

  (* Days in each month for non-leap and leap years *)
  let days_in_month ~leap m =
    match m with
    | 1 -> 31
    | 2 -> if leap then 29 else 28
    | 3 -> 31
    | 4 -> 30
    | 5 -> 31
    | 6 -> 30
    | 7 -> 31
    | 8 -> 31
    | 9 -> 30
    | 10 -> 31
    | 11 -> 30
    | 12 -> 31
    | _ -> 30

  let is_leap_year y = y mod 4 = 0 && (y mod 100 <> 0 || y mod 400 = 0)

  let timestamp_to_date v =
    let secs = int_of_float v in
    let days = secs / 86400 in
    let rem = secs mod 86400 in
    let hour = rem / 3600 in
    let min = rem mod 3600 / 60 in
    let sec = rem mod 60 in
    (* Days since 1970-01-01 *)
    let rec find_year days year =
      let days_in_year = if is_leap_year year then 366 else 365 in
      if days < days_in_year then (year, days)
      else find_year (days - days_in_year) (year + 1)
    in
    let year, day_of_year = find_year days 1970 in
    let leap = is_leap_year year in
    let rec find_month day month =
      let dim = days_in_month ~leap month in
      if day < dim then (month, day + 1) else find_month (day - dim) (month + 1)
    in
    let month, day = find_month day_of_year 1 in
    (year, month, day, hour, min, sec)

  let mmdd_utc _ v =
    let _, month, day, _, _, _ = timestamp_to_date v in
    Printf.sprintf "%02d/%02d" month day

  let hhmmss_utc _ v =
    let _, _, _, hour, min, sec = timestamp_to_date v in
    Printf.sprintf "%02d:%02d:%02d" hour min sec
end

(* --- Transform --- *)

module Transform = struct
  let ema alpha data =
    let n = Array.length data in
    if n = 0 then [||]
    else
      let result = Array.make n (0., 0.) in
      let x0, y0 = data.(0) in
      result.(0) <- (x0, y0);
      let prev_y = ref y0 in
      for i = 1 to n - 1 do
        let x, y = data.(i) in
        let smoothed = (alpha *. y) +. ((1. -. alpha) *. !prev_y) in
        result.(i) <- (x, smoothed);
        prev_y := smoothed
      done;
      result

  let sma window data =
    let n = Array.length data in
    if n = 0 || window <= 0 then [||]
    else
      let window = min window n in
      let result = Array.make n (0., 0.) in
      let sum = ref 0. in
      (* Initialize sum with first window elements *)
      for i = 0 to window - 1 do
        let _, y = data.(i) in
        sum := !sum +. y
      done;
      (* First valid point *)
      let x0, _ = data.(window - 1) in
      result.(window - 1) <- (x0, !sum /. float window);
      (* Slide the window *)
      for i = window to n - 1 do
        let _, y_old = data.(i - window) in
        let x, y_new = data.(i) in
        sum := !sum -. y_old +. y_new;
        result.(i) <- (x, !sum /. float window)
      done;
      (* Fill initial points with partial averages *)
      let partial_sum = ref 0. in
      for i = 0 to window - 2 do
        let x, y = data.(i) in
        partial_sum := !partial_sum +. y;
        result.(i) <- (x, !partial_sum /. float (i + 1))
      done;
      result

  let gaussian sigma data =
    let n = Array.length data in
    if n = 0 || sigma <= 0. then data
    else
      (* Kernel radius: 3 sigma covers 99.7% of the distribution *)
      let radius = int_of_float (Float.ceil (3. *. sigma)) in
      let kernel_size = (2 * radius) + 1 in
      (* Precompute Gaussian kernel *)
      let kernel = Array.make kernel_size 0. in
      let kernel_sum = ref 0. in
      for i = 0 to kernel_size - 1 do
        let d = float (i - radius) in
        let w = Float.exp (-.(d *. d) /. (2. *. sigma *. sigma)) in
        kernel.(i) <- w;
        kernel_sum := !kernel_sum +. w
      done;
      (* Normalize kernel *)
      for i = 0 to kernel_size - 1 do
        kernel.(i) <- kernel.(i) /. !kernel_sum
      done;
      (* Apply convolution *)
      let result = Array.make n (0., 0.) in
      for i = 0 to n - 1 do
        let x, _ = data.(i) in
        let sum = ref 0. in
        let weight_sum = ref 0. in
        for k = 0 to kernel_size - 1 do
          let j = i + k - radius in
          if j >= 0 && j < n then (
            let _, y = data.(j) in
            sum := !sum +. (kernel.(k) *. y);
            weight_sum := !weight_sum +. kernel.(k))
        done;
        let smoothed = if !weight_sum > 0. then !sum /. !weight_sum else 0. in
        result.(i) <- (x, smoothed)
      done;
      result
end

(* --- Scale --- *)

module Scale = struct
  type numeric_domain = [ `Auto | `Domain of float * float ]

  type t =
    | Auto
    | Numeric of { domain : numeric_domain; clamp : bool }
    | Log of { base : float; domain : numeric_domain; clamp : bool }
    | Band of { categories : string list option; padding : float }

  let numeric ?(domain = `Auto) ?(clamp = true) () = Numeric { domain; clamp }

  let log ?(base = 10.) ?(domain = `Auto) ?(clamp = true) () =
    let base = if base <= 1. then 10. else base in
    Log { base; domain; clamp }

  let band ?categories ?(padding = 0.1) () =
    (* Clamp padding to [0, 0.95] to ensure bands have non-zero width *)
    let padding = Float.min 0.95 (clamp01 padding) in
    Band { categories; padding }
end

(* --- Axis --- *)

module Axis = struct
  type formatter = int -> float -> string
  type line = [ `None | `Axis_only | `Frame ]
  type title = { text : string; style : Style.t option }

  type t = {
    show : bool;
    line : line;
    ticks : int;
    format : formatter;
    style : Style.t option; (* None = use theme.axes *)
    tick_style : Style.t option; (* None = use theme.axes *)
    label_style : Style.t option; (* None = use theme.labels *)
    tick_length : int;
    label_padding : int;
    title : title option;
  }

  let hidden =
    {
      show = false;
      line = `None;
      ticks = 0;
      format = Label_format.float ();
      style = None;
      tick_style = None;
      label_style = None;
      tick_length = 0;
      label_padding = 0;
      title = None;
    }

  let default =
    {
      show = true;
      line = `Axis_only;
      ticks = 6;
      format = Label_format.float ();
      style = None;
      tick_style = None;
      label_style = None;
      tick_length = 1;
      label_padding = 1;
      title = None;
    }

  let with_ticks ticks a = { a with ticks = max 0 ticks }
  let with_format format a = { a with format }
  let with_style style a = { a with style = Some style }
  let with_tick_style tick_style a = { a with tick_style = Some tick_style }
  let with_label_style label_style a = { a with label_style = Some label_style }

  let with_tick_length tick_length a =
    { a with tick_length = max 0 tick_length }

  let with_label_padding label_padding a =
    { a with label_padding = max 0 label_padding }

  let with_line line a = { a with line }
  let with_title ?style text a = { a with title = Some { text; style } }
end

(* --- Gridlines --- *)

module Gridlines = struct
  type t = {
    show : bool;
    x : bool;
    y : bool;
    style : Style.t;
    pattern : Charset.line_pattern;
    x_step : int option;
    y_step : int option;
    minor : int option;
    minor_style : Style.t option;
  }

  let hidden =
    {
      show = false;
      x = true;
      y = true;
      style = Style.default;
      pattern = `Dotted;
      x_step = None;
      y_step = None;
      minor = None;
      minor_style = None;
    }

  let default =
    {
      show = true;
      x = true;
      y = true;
      style = Style.make ~dim:true ();
      pattern = `Dotted;
      x_step = None;
      y_step = None;
      minor = None;
      minor_style = None;
    }

  let with_style style g = { g with style }
  let with_pattern pattern g = { g with pattern }
  let with_x x g = { g with x }
  let with_y y g = { g with y }
  let with_x_step x_step g = { g with x_step }
  let with_y_step y_step g = { g with y_step }
  let with_minor minor g = { g with minor }
  let with_minor_style minor_style g = { g with minor_style }
end

(* --- View --- *)

module View = struct
  type window = { min : float; max : float }
  type t = { x : window option; y : window option; y2 : window option }

  let empty = { x = None; y = None; y2 = None }
  let set_x x t = { t with x }
  let set_y y t = { t with y }
  let set_y2 y2 t = { t with y2 }

  let window ~min ~max =
    let min, max = if min <= max then (min, max) else (max, min) in
    let min, max = safe_view_range min max in
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
    let size = w.max -. w.min in
    let dom_size = domain.max -. domain.min in
    if size <= 0. || dom_size <= 0. then domain
    else if size >= dom_size then domain
    else
      let min' = Float.max domain.min (Float.min w.min (domain.max -. size)) in
      { min = min'; max = min' +. size }
end

(* --- Marks --- *)

module Mark = struct
  type id = string
  type scatter_mode = [ `Cell | `Braille | `Density ]
  type heatmap_agg = [ `Last | `Avg | `Max ]

  type heatmap_mode =
    | Cells_fg
    | Cells_bg
    | Halfblock_fg_bg
    | Shaded
    | Dense_bilinear

  (* TODO: bar_mode is defined but not yet implemented in rendering. Currently
     all bars render as half-block sub-cell precision. *)
  type bar_mode = [ `Cell | `Half_block ]
  type candle_body = [ `Filled | `Hollow ]
  type candle_width = [ `One | `Two ]
  type area_baseline = [ `Zero | `Value of float ]
  type bin_method = Bins of int | Width of float | Edges of float array
  type histogram_normalize = [ `Count | `Density | `Probability ]
  type bar_segment = { value : float; style : Style.t; label : string option }
  type stacked_bar = { category : string; segments : bar_segment list }

  type ohlc = {
    time : float;
    open_ : float;
    high : float;
    low : float;
    close : float;
  }

  type y_axis_selector = [ `Y1 | `Y2 ]
  (** Selector for which Y-axis a mark should use.
      - [`Y1]: Primary Y-axis (left side, default).
      - [`Y2]: Secondary Y-axis (right side). *)

  type t =
    | Line : {
        id : id option;
        label : string option;
        style : Style.t option;
        resolution : Raster.resolution;
        y_axis : y_axis_selector;
        pattern : Charset.line_pattern;
        glyph : string option;
        x : 'a -> float;
        y : 'a -> float;
        data : 'a array;
      }
        -> t
    | Line_opt : {
        id : id option;
        label : string option;
        style : Style.t option;
        resolution : Raster.resolution;
        y_axis : y_axis_selector;
        pattern : Charset.line_pattern;
        glyph : string option;
        x : 'a -> float;
        y : 'a -> float option;
        data : 'a array;
      }
        -> t
    | Scatter : {
        id : id option;
        label : string option;
        style : Style.t option;
        glyph : string option;
        mode : scatter_mode;
        y_axis : y_axis_selector;
        x : 'a -> float;
        y : 'a -> float;
        data : 'a array;
      }
        -> t
    | Bars_y : {
        id : id option;
        label : string option;
        style : Style.t option;
        mode : bar_mode;
        x : 'a -> string;
        y : 'a -> float;
        data : 'a array;
      }
        -> t
    | Bars_x : {
        id : id option;
        label : string option;
        style : Style.t option;
        mode : bar_mode;
        y : 'a -> string;
        x : 'a -> float;
        data : 'a array;
      }
        -> t
    | Stacked_bars_y of {
        id : id option;
        gap : int;
        bar_width : int option;
        mode : bar_mode;
        data : stacked_bar array;
      }
    | Stacked_bars_x of {
        id : id option;
        gap : int;
        bar_height : int option;
        mode : bar_mode;
        data : stacked_bar array;
      }
    | Rule_y of {
        id : id option;
        style : Style.t option;
        pattern : Charset.line_pattern;
        y_axis : y_axis_selector;
        y : float;
      }
    | Rule_x of {
        id : id option;
        style : Style.t option;
        pattern : Charset.line_pattern;
        x : float;
      }
    | Heatmap : {
        id : id option;
        color_scale : Color.t array;
        value_range : (float * float) option;
        auto_value_range : bool;
        agg : heatmap_agg;
        mode : heatmap_mode;
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
        width : candle_width;
        body : candle_body;
        y_axis : y_axis_selector;
        data : ohlc array;
      }
    | Circle : {
        id : id option;
        style : Style.t option;
        resolution : Raster.resolution;
        y_axis : y_axis_selector;
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
    | Area : {
        id : id option;
        label : string option;
        style : Style.t option;
        baseline : area_baseline;
        resolution : Raster.resolution;
        y_axis : y_axis_selector;
        x : 'a -> float;
        y : 'a -> float;
        data : 'a array;
      }
        -> t
    | Fill_between : {
        id : id option;
        label : string option;
        style : Style.t option;
        resolution : Raster.resolution;
        y_axis : y_axis_selector;
        x : 'a -> float;
        y_low : 'a -> float;
        y_high : 'a -> float;
        data : 'a array;
      }
        -> t
    | Histogram : {
        id : id option;
        label : string option;
        style : Style.t option;
        bins : bin_method;
        normalize : histogram_normalize;
        x : 'a -> float;
        data : 'a array;
      }
        -> t

  let line ?id ?label ?style ?(resolution = `Cell) ?(pattern = `Solid) ?glyph
      ?(y_axis = `Y1) ~x ~y data =
    Line { id; label; style; resolution; y_axis; pattern; glyph; x; y; data }

  let line_opt ?id ?label ?style ?(resolution = `Cell) ?(pattern = `Solid)
      ?glyph ?(y_axis = `Y1) ~x ~y data =
    Line_opt
      { id; label; style; resolution; y_axis; pattern; glyph; x; y; data }

  let scatter ?id ?label ?style ?glyph ?(mode = (`Cell : scatter_mode))
      ?(y_axis = `Y1) ~x ~y data =
    Scatter { id; label; style; glyph; mode; y_axis; x; y; data }

  let bars_y ?id ?label ?style ?(mode = (`Half_block : bar_mode)) ~x ~y data =
    Bars_y { id; label; style; mode; x; y; data }

  let bars_x ?id ?label ?style ?(mode = (`Half_block : bar_mode)) ~y ~x data =
    Bars_x { id; label; style; mode; y; x; data }

  let stacked_bars_y ?id ?(gap = 1) ?bar_width
      ?(mode = (`Half_block : bar_mode)) data =
    Stacked_bars_y { id; gap = max 0 gap; bar_width; mode; data }

  let stacked_bars_x ?id ?(gap = 1) ?bar_height
      ?(mode = (`Half_block : bar_mode)) data =
    Stacked_bars_x { id; gap = max 0 gap; bar_height; mode; data }

  let rule_y ?id ?style ?(pattern = `Solid) ?(y_axis = `Y1) y =
    Rule_y { id; style; pattern; y_axis; y }

  let rule_x ?id ?style ?(pattern = `Solid) x = Rule_x { id; style; pattern; x }

  let heatmap ?id ?(color_scale = Theme.default.palette) ?value_range
      ?(auto_value_range = true) ?(agg = (`Last : heatmap_agg))
      ?(mode = Cells_fg) ~x ~y ~value data =
    Heatmap
      {
        id;
        color_scale;
        value_range;
        auto_value_range;
        agg;
        mode;
        x;
        y;
        value;
        data;
      }

  let candles ?id ?bullish ?bearish ?(width = (`One : candle_width))
      ?(body = (`Filled : candle_body)) ?(y_axis = `Y1) data =
    let bullish =
      Option.value bullish ~default:(Style.fg Color.Green Style.default)
    in
    let bearish =
      Option.value bearish ~default:(Style.fg Color.Red Style.default)
    in
    Candles { id; bullish; bearish; width; body; y_axis; data }

  let circle ?id ?style ?(resolution = (`Cell : Raster.resolution))
      ?(y_axis = `Y1) ~cx ~cy ~r data =
    Circle { id; style; resolution; y_axis; cx; cy; r; data }

  let shade_x ?id ?style ~min ~max () =
    let x0, x1 = if min <= max then (min, max) else (max, min) in
    Shade_x { id; style; x0; x1 }

  let column_background ?id ?style x = Column_background { id; style; x }

  let area ?id ?label ?style ?(baseline = `Zero) ?(resolution = `Cell)
      ?(y_axis = `Y1) ~x ~y data =
    Area { id; label; style; baseline; resolution; y_axis; x; y; data }

  let fill_between ?id ?label ?style ?(resolution = `Cell) ?(y_axis = `Y1) ~x
      ~y_low ~y_high data =
    Fill_between
      { id; label; style; resolution; y_axis; x; y_low; y_high; data }

  let histogram ?id ?label ?style ?(bins = Bins 10) ?(normalize = `Count) ~x
      data =
    Histogram { id; label; style; bins; normalize; x; data }
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
  | Log of {
      base : float;
      domain : View.window;
      view : View.window;
      clamp : bool;
    }
  | Band of {
      categories : string list;
      categories_arr : string array; (* O(1) index-to-category *)
      index_of : (string, int) Hashtbl.t; (* O(1) category-to-index *)
      padding : float;
      domain : View.window;
      view : View.window;
    }

(* Helper to create Band with precomputed structures *)
let make_band ~categories ~padding ~domain ~view =
  let categories_arr = Array.of_list categories in
  let index_of = Hashtbl.create (List.length categories) in
  List.iteri (fun i cat -> Hashtbl.add index_of cat i) categories;
  Band { categories; categories_arr; index_of; padding; domain; view }

type compiled_mark =
  | CLine : {
      id : string option;
      label : string option;
      resolution : Raster.resolution;
      pattern : Charset.line_pattern;
      glyph : string option;
      style : Style.t;
      y_axis : Mark.y_axis_selector;
      x : 'a -> float;
      y : 'a -> float;
      data : 'a array;
    }
      -> compiled_mark
  | CLine_opt : {
      id : string option;
      label : string option;
      resolution : Raster.resolution;
      pattern : Charset.line_pattern;
      glyph : string option;
      style : Style.t;
      y_axis : Mark.y_axis_selector;
      x : 'a -> float;
      y : 'a -> float option;
      data : 'a array;
    }
      -> compiled_mark
  | CScatter : {
      id : string option;
      label : string option;
      mode : Mark.scatter_mode;
      glyph : string;
      style : Style.t;
      y_axis : Mark.y_axis_selector;
      x : 'a -> float;
      y : 'a -> float;
      data : 'a array;
    }
      -> compiled_mark
  | CBars_y : {
      id : string option;
      label : string option;
      mode : Mark.bar_mode;
      style : Style.t;
      x : 'a -> string;
      y : 'a -> float;
      data : 'a array;
    }
      -> compiled_mark
  | CBars_x : {
      id : string option;
      label : string option;
      mode : Mark.bar_mode;
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
      mode : Mark.bar_mode;
      data : Mark.stacked_bar array;
    }
  | CStacked_x of {
      id : string option;
      gap : int;
      bar_height : int option;
      mode : Mark.bar_mode;
      data : Mark.stacked_bar array;
    }
  | CRule_y of {
      id : string option;
      style : Style.t;
      pattern : Charset.line_pattern;
      y_axis : Mark.y_axis_selector;
      y : float;
    }
  | CRule_x of {
      id : string option;
      style : Style.t;
      pattern : Charset.line_pattern;
      x : float;
    }
  | CHeatmap : {
      id : string option;
      color_scale : Color.t array;
      value_range : (float * float) option;
      auto_value_range : bool;
      agg : Mark.heatmap_agg;
      mode : Mark.heatmap_mode;
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
      width : Mark.candle_width;
      body : Mark.candle_body;
      y_axis : Mark.y_axis_selector;
      data : Mark.ohlc array;
    }
  | CCircle : {
      id : string option;
      resolution : Raster.resolution;
      style : Style.t;
      y_axis : Mark.y_axis_selector;
      cx : 'a -> float;
      cy : 'a -> float;
      r : 'a -> float;
      data : 'a array;
    }
      -> compiled_mark
  | CShade_x of { id : string option; style : Style.t; x0 : float; x1 : float }
  | CColumn_bg of { id : string option; style : Style.t; x : float }
  | CArea : {
      id : string option;
      label : string option;
      style : Style.t;
      baseline : Mark.area_baseline;
      resolution : Raster.resolution;
      y_axis : Mark.y_axis_selector;
      x : 'a -> float;
      y : 'a -> float;
      data : 'a array;
    }
      -> compiled_mark
  | CFill_between : {
      id : string option;
      label : string option;
      style : Style.t;
      resolution : Raster.resolution;
      y_axis : Mark.y_axis_selector;
      x : 'a -> float;
      y_low : 'a -> float;
      y_high : 'a -> float;
      data : 'a array;
    }
      -> compiled_mark
  | CHistogram of {
      id : string option;
      label : string option;
      style : Style.t;
      bin_edges : float array;
      bin_values : float array;
    }

(* --- Layout --- *)

module Layout = struct
  type rect = { x : int; y : int; width : int; height : int }

  let rect_contains r ~x ~y =
    x >= r.x && x < r.x + r.width && y >= r.y && y < r.y + r.height

  type axis = [ `X | `Y | `Both ]
  type title_info = { text : string; style : Style.t }

  type t = {
    width : int;
    height : int;
    plot : rect;
    theme : Theme.t;
    (* resolved scale info *)
    x_scale : axis_kind;
    y_scale : axis_kind;
    y2_scale : axis_kind option; (* Secondary Y-axis scale *)
    (* "full" domains (always exist) *)
    x_domain : View.window;
    y_domain : View.window;
    y2_domain : View.window option; (* Secondary Y-axis domain *)
    (* effective view windows used for scaling *)
    x_view : View.window;
    y_view : View.window;
    y2_view : View.window option; (* Secondary Y-axis view *)
    (* precomputed tick values for axes (computed once during layout) *)
    x_ticks : float list;
    y_ticks : float list;
    y2_ticks : float list option;
    (* axis and grid config for drawing (used by Chart.draw) *)
    x_axis : Axis.t;
    y_axis : Axis.t;
    y2_axis : Axis.t option; (* Secondary Y-axis config *)
    grid : Gridlines.t;
    frame_inner_padding : int;
    margin_left : int;
    y_axis_width : int;
    y_axis_title_width : int; (* Space reserved for y-axis title *)
    y2_axis_width : int; (* Right margin for secondary Y-axis *)
    (* compiled marks (used by hit-testing) *)
    marks : compiled_mark list;
    (* title *)
    title : title_info option;
  }

  let size t = (t.width, t.height)
  let plot_rect t = t.plot
  let is_inside_plot t ~px ~py = rect_contains t.plot ~x:px ~y:py
  let x_domain t = t.x_domain
  let y_domain t = t.y_domain
  let y2_domain t = t.y2_domain
  let x_view t = t.x_view
  let y_view t = t.y_view
  let y2_view t = t.y2_view
  let y_axis_title_width t = t.y_axis_title_width
  let y2_axis_width t = t.y2_axis_width

  (* Check if Y2 axis is enabled *)
  let has_y2 t = Option.is_some t.y2_scale && Option.is_some t.y2_axis

  (* Scale-aware inverse transform: convert normalized t value [0,1] to data
     value *)
  let inverse_x_transform t tx =
    match t.x_scale with
    | Numeric _ | Band _ -> lerp t.x_view.min t.x_view.max tx
    | Log { base; view; _ } ->
        (* Inverse of log scale: t maps to log space, then exponentiate *)
        let safe_min = Float.max 1e-10 view.min in
        let safe_max = Float.max 1e-10 view.max in
        let log_min = log_transform ~base safe_min in
        let log_max = log_transform ~base safe_max in
        let log_v = lerp log_min log_max tx in
        Float.pow base log_v

  let inverse_y_transform t ty =
    match t.y_scale with
    | Numeric _ | Band _ ->
        (* y is inverted: ty=0 is top (max), ty=1 is bottom (min) *)
        lerp t.y_view.max t.y_view.min ty
    | Log { base; view; _ } ->
        let safe_min = Float.max 1e-10 view.min in
        let safe_max = Float.max 1e-10 view.max in
        let log_min = log_transform ~base safe_min in
        let log_max = log_transform ~base safe_max in
        (* y is inverted: ty=0 is top (max), ty=1 is bottom (min) *)
        let log_v = lerp log_max log_min ty in
        Float.pow base log_v

  (* Scale-aware forward transform: convert data value to normalized t [0,1] *)
  let forward_x_transform t x =
    match t.x_scale with
    | Numeric _ | Band _ ->
        if Float.abs (t.x_view.max -. t.x_view.min) < 1e-12 then 0.
        else (x -. t.x_view.min) /. (t.x_view.max -. t.x_view.min)
    | Log { base; view; _ } ->
        let safe_min = Float.max 1e-10 view.min in
        let safe_max = Float.max 1e-10 view.max in
        let safe_x = Float.max 1e-10 x in
        let log_min = log_transform ~base safe_min in
        let log_max = log_transform ~base safe_max in
        if Float.abs (log_max -. log_min) < 1e-12 then 0.
        else (log_transform ~base safe_x -. log_min) /. (log_max -. log_min)

  let forward_y_transform t y =
    match t.y_scale with
    | Numeric _ | Band _ ->
        if Float.abs (t.y_view.max -. t.y_view.min) < 1e-12 then 0.
        else (t.y_view.max -. y) /. (t.y_view.max -. t.y_view.min)
    | Log { base; view; _ } ->
        let safe_min = Float.max 1e-10 view.min in
        let safe_max = Float.max 1e-10 view.max in
        let safe_y = Float.max 1e-10 y in
        let log_min = log_transform ~base safe_min in
        let log_max = log_transform ~base safe_max in
        if Float.abs (log_max -. log_min) < 1e-12 then 0.
        else (log_max -. log_transform ~base safe_y) /. (log_max -. log_min)

  (* Secondary Y-axis transform (Y2) - falls back to Y1 if Y2 not configured *)
  let forward_y2_transform t y =
    match (t.y2_scale, t.y2_view) with
    | Some (Numeric _ | Band _), Some view ->
        if Float.abs (view.max -. view.min) < 1e-12 then 0.
        else (view.max -. y) /. (view.max -. view.min)
    | Some (Log { base; view; _ }), _ ->
        let safe_min = Float.max 1e-10 view.min in
        let safe_max = Float.max 1e-10 view.max in
        let safe_y = Float.max 1e-10 y in
        let log_min = log_transform ~base safe_min in
        let log_max = log_transform ~base safe_max in
        if Float.abs (log_max -. log_min) < 1e-12 then 0.
        else (log_max -. log_transform ~base safe_y) /. (log_max -. log_min)
    | _ -> forward_y_transform t y (* Fallback to Y1 *)

  let data_of_px t ~px ~py =
    if not (rect_contains t.plot ~x:px ~y:py) then None
    else
      let rel_x = px - t.plot.x in
      let rel_y = py - t.plot.y in
      let w = max 1 (t.plot.width - 1) in
      let h = max 1 (t.plot.height - 1) in
      let tx = float rel_x /. float w in
      let ty = float rel_y /. float h in
      let x = inverse_x_transform t tx in
      let y = inverse_y_transform t ty in
      Some (x, y)

  (* Internal: unclamped version for hit-testing - returns None if outside view.
     Respects y_axis selector for marks that use Y2 axis. *)
  let px_of_data_unclamped ?(y_axis : Mark.y_axis_selector = `Y1) t ~x ~y =
    let forward_y =
      match y_axis with
      | `Y1 -> forward_y_transform t y
      | `Y2 -> forward_y2_transform t y
    in
    (* Handle single-cell plots *)
    if t.plot.width <= 1 && t.plot.height <= 1 then
      let tx = forward_x_transform t x in
      let ty = forward_y in
      if tx < 0. || tx > 1. || ty < 0. || ty > 1. then None
      else Some (t.plot.x, t.plot.y)
    else
      let w = max 0 (t.plot.width - 1) in
      let h = max 0 (t.plot.height - 1) in
      let tx = forward_x_transform t x in
      let ty = forward_y in
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
      let tx = clamp01 (forward_x_transform t x) in
      let ty = clamp01 (forward_y_transform t y) in
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

  (* Convert band index to pixel position (start of band). Uses Float.round for
     consistency across gridlines and bars. *)
  let band_start_px ~origin ~offset ~bw ~index =
    origin + int_of_float (Float.round (offset +. (float index *. bw)))

  (* Compute the band width in pixels (excluding 1px for inter-band gap).
     Ensures minimum of 1 pixel. Uses Float.max 1. to ensure non-negative before
     subtracting, then applies outer max 1 for safety. *)
  let band_size_px ~bw = max 1 (int_of_float (Float.max 1. (bw -. 1.)))

  (* O(1) category-to-index lookup using precomputed hashtable *)
  let band_index_fast index_of s = Hashtbl.find_opt index_of s

  (* O(1) index-to-category lookup using precomputed array *)
  let band_category_fast categories_arr i =
    if i >= 0 && i < Array.length categories_arr then Some categories_arr.(i)
    else None

  let x_category_of_px t ~px =
    match t.x_scale with
    | Band { categories_arr; padding; categories; _ } ->
        if t.plot.width <= 0 then None
        else if px < t.plot.x || px >= t.plot.x + t.plot.width then None
        else
          let offset, bw =
            band_params ~cats:categories ~padding ~extent:t.plot.width
          in
          let rel = float (px - t.plot.x) in
          let i = int_of_float ((rel -. offset) /. bw) in
          let i = clamp_int 0 (Array.length categories_arr - 1) i in
          band_category_fast categories_arr i
    | _ -> None

  let y_category_of_px t ~py =
    match t.y_scale with
    | Band { categories_arr; padding; categories; _ } ->
        if t.plot.height <= 0 then None
        else if py < t.plot.y || py >= t.plot.y + t.plot.height then None
        else
          let offset, bw =
            band_params ~cats:categories ~padding ~extent:t.plot.height
          in
          let rel = float (py - t.plot.y) in
          let i = int_of_float ((rel -. offset) /. bw) in
          let i = clamp_int 0 (Array.length categories_arr - 1) i in
          band_category_fast categories_arr i
    | _ -> None

  let px_of_x_category t cat =
    match t.x_scale with
    | Band { index_of; padding; categories; _ } -> (
        match band_index_fast index_of cat with
        | None -> None
        | Some i ->
            let offset, bw =
              band_params ~cats:categories ~padding ~extent:t.plot.width
            in
            let band_w = band_size_px ~bw in
            let x0 = band_start_px ~origin:t.plot.x ~offset ~bw ~index:i in
            Some (x0 + (band_w / 2)))
    | _ -> None

  let py_of_y_category t cat =
    match t.y_scale with
    | Band { index_of; padding; categories; _ } -> (
        match band_index_fast index_of cat with
        | None -> None
        | Some i ->
            let offset, bw =
              band_params ~cats:categories ~padding ~extent:t.plot.height
            in
            let band_h = band_size_px ~bw in
            let y0 = band_start_px ~origin:t.plot.y ~offset ~bw ~index:i in
            Some (y0 + (band_h / 2)))
    | _ -> None

  let clamp_view t (v : View.t) =
    let clamp_axis scale_opt domain_opt win_opt =
      match (scale_opt, domain_opt, win_opt) with
      (* Only clamp if the scale's clamp flag is true and domain is available *)
      | Some (Numeric { clamp = true; _ }), Some domain, Some w
      | Some (Log { clamp = true; _ }), Some domain, Some w ->
          Some (View.clamp ~domain w)
      (* clamp=true but no domain: pass through without clamping *)
      | Some (Numeric { clamp = true; _ }), None, Some w
      | Some (Log { clamp = true; _ }), None, Some w ->
          Some w
      | Some (Numeric { clamp = false; _ }), _, Some w
      | Some (Log { clamp = false; _ }), _, Some w ->
          Some w
      | Some (Numeric _), _, None | Some (Log _), _, None -> None
      | Some (Band _), _, _ | None, _, _ -> None
    in
    {
      View.x = clamp_axis (Some t.x_scale) (Some t.x_domain) v.View.x;
      y = clamp_axis (Some t.y_scale) (Some t.y_domain) v.View.y;
      y2 = clamp_axis t.y2_scale t.y2_domain v.View.y2;
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
      | Numeric { clamp; _ } | Log { clamp; _ } -> (true, clamp)
      | Band _ -> (false, false)
    in
    let apply_y, clamp_y =
      match t.y_scale with
      | Numeric { clamp; _ } | Log { clamp; _ } -> (true, clamp)
      | Band _ -> (false, false)
    in
    (* Y2 axis zoom support *)
    let apply_y2, clamp_y2, y2_domain =
      match t.y2_scale with
      | Some (Numeric { clamp; _ }) | Some (Log { clamp; _ }) ->
          (true, clamp, t.y2_domain)
      | Some (Band _) | None -> (false, false, None)
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
    (* Y2 zooms together with Y when axis = `Y or `Both *)
    let y2 =
      match (apply_y2, axis, base.View.y2, t.y2_view) with
      | true, (`Y | `Both), Some w, _ ->
          let center_y2 =
            match t.y2_view with
            | Some v -> (v.min +. v.max) /. 2.0
            | None -> center_y
          in
          let zoomed = View.zoom_around w ~center:center_y2 ~factor in
          let clamped =
            match (clamp_y2, y2_domain) with
            | true, Some domain -> View.clamp ~domain zoomed
            | _ -> zoomed
          in
          Some clamped
      | true, (`Y | `Both), None, Some default_view ->
          let center_y2 = (default_view.min +. default_view.max) /. 2.0 in
          let zoomed =
            View.zoom_around default_view ~center:center_y2 ~factor
          in
          let clamped =
            match (clamp_y2, y2_domain) with
            | true, Some domain -> View.clamp ~domain zoomed
            | _ -> zoomed
          in
          Some clamped
      | _ -> base.View.y2
    in
    { View.x; y; y2 }

  let pan_view_by_px t ~view ~dx ~dy =
    let base = clamp_view t view in
    (* Check if pan applies and if clamping is enabled for each axis *)
    let apply_x, clamp_x =
      match t.x_scale with
      | Numeric { clamp; _ } | Log { clamp; _ } -> (true, clamp)
      | Band _ -> (false, false)
    in
    let apply_y, clamp_y =
      match t.y_scale with
      | Numeric { clamp; _ } | Log { clamp; _ } -> (true, clamp)
      | Band _ -> (false, false)
    in
    (* Y2 axis pan support *)
    let apply_y2, clamp_y2, y2_domain =
      match t.y2_scale with
      | Some (Numeric { clamp; _ }) | Some (Log { clamp; _ }) ->
          (true, clamp, t.y2_domain)
      | Some (Band _) | None -> (false, false, None)
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
    (* Y2 pans together with Y *)
    let pan_y2 =
      if not apply_y2 then base.View.y2
      else
        match (base.View.y2, t.y2_view) with
        | Some w, _ ->
            let denom = float (max 1 (t.plot.height - 1)) in
            let delta = float dy *. (w.max -. w.min) /. denom in
            let panned = View.pan w ~delta in
            let clamped =
              match (clamp_y2, y2_domain) with
              | true, Some domain -> View.clamp ~domain panned
              | _ -> panned
            in
            Some clamped
        | None, Some default_view ->
            let denom = float (max 1 (t.plot.height - 1)) in
            let delta =
              float dy *. (default_view.max -. default_view.min) /. denom
            in
            let panned = View.pan default_view ~delta in
            let clamped =
              match (clamp_y2, y2_domain) with
              | true, Some domain -> View.clamp ~domain panned
              | _ -> panned
            in
            Some clamped
        | None, None -> None
    in
    { View.x = pan_x; y = pan_y; y2 = pan_y2 }

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
      (* Use unclamped version for hit-testing to filter out offscreen points.
         Now respects y_axis selector for Y2 axis support. *)
      let map_xy_unclamped ~y_axis x y = px_of_data_unclamped ~y_axis t ~x ~y in

      let check_points ~y_axis ~mark_id ~kind ~payload_of ~data_iter =
        data_iter (fun idx (x, y) ->
            (* Only consider points that are within the current view *)
            match map_xy_unclamped ~y_axis x y with
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
          | CLine { id; x; y; y_axis; data; _ } ->
              check_points ~y_axis ~mark_id:id ~kind:`Line
                ~payload_of:(fun x y -> Hit.XY { x; y })
                ~data_iter:(fun f ->
                  let idx = ref 0 in
                  Data.iter data (fun a ->
                      let i = !idx in
                      incr idx;
                      f i (x a, y a)))
          | CLine_opt { id; x; y; y_axis; data; _ } ->
              check_points ~y_axis ~mark_id:id ~kind:`Line
                ~payload_of:(fun x y -> Hit.XY { x; y })
                ~data_iter:(fun f ->
                  let idx = ref 0 in
                  Data.iter data (fun a ->
                      let i = !idx in
                      incr idx;
                      match y a with None -> () | Some yy -> f i (x a, yy)))
          | CScatter { id; x; y; y_axis; data; _ } ->
              check_points ~y_axis ~mark_id:id ~kind:`Scatter
                ~payload_of:(fun x y -> Hit.XY { x; y })
                ~data_iter:(fun f ->
                  let idx = ref 0 in
                  Data.iter data (fun a ->
                      let i = !idx in
                      incr idx;
                      f i (x a, y a)))
          | CBars_y { id; x; y; data; _ } -> (
              match t.x_scale with
              | Band { categories; index_of; padding; _ } ->
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
                          match band_index_fast index_of cat with
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
              | Band { categories; index_of; padding; _ } ->
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
                          match band_index_fast index_of cat with
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
              | Band { categories; index_of; padding; _ } ->
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
                          match band_index_fast index_of bar.Mark.category with
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
              | Band { categories; index_of; padding; _ } ->
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
                          match band_index_fast index_of bar.Mark.category with
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
              (* nearest-point heat hit - now passes value through. CHeatmap
                 always uses Y1 axis. *)
              let idx = ref 0 in
              Data.iter data (fun a ->
                  let i = !idx in
                  incr idx;
                  let xv = x a in
                  let yv = y a in
                  let v = value_fn a in
                  (* Only consider points within current view *)
                  match map_xy_unclamped ~y_axis:`Y1 xv yv with
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
          | CCandles { id; y_axis; data; _ } ->
              let idx = ref 0 in
              Data.iter data (fun o ->
                  let i = !idx in
                  incr idx;
                  (* Check if candle's x position is within view *)
                  match map_xy_unclamped ~y_axis o.Mark.time t.y_view.min with
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
          | CCircle { id; y_axis; cx; cy; data; _ } ->
              check_points ~y_axis ~mark_id:id ~kind:`Circle
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
  type tooltip_border = [ `Theme | `None | `Style of Ansi.Style.t ]
  type h_anchor = [ `Left | `Center | `Right ]
  type v_anchor = [ `Top | `Middle | `Bottom ]
  type arrow_head = [ `None | `Arrow | `Dot ]

  let crosshair ?style ?(pattern = `Solid) (layout : Layout.t) (grid : G.t) ~x
      ~y =
    let style = Option.value style ~default:layout.theme.crosshair in
    let px, py = Layout.px_of_data layout ~x ~y in
    let r = Layout.plot_rect layout in
    let charset = layout.theme.charset in
    (if px >= r.x && px < r.x + r.width then
       let glyph =
         match pattern with
         | `Solid -> charset.grid_v_solid
         | `Dashed -> charset.grid_v_dashed
         | `Dotted -> charset.grid_v_dotted
       in
       for yy = r.y to r.y + r.height - 1 do
         draw_text grid ~x:px ~y:yy ~style glyph
       done);
    if py >= r.y && py < r.y + r.height then
      let glyph =
        match pattern with
        | `Solid -> charset.grid_h_solid
        | `Dashed -> charset.grid_h_dashed
        | `Dotted -> charset.grid_h_dotted
      in
      for xx = r.x to r.x + r.width - 1 do
        draw_text grid ~x:xx ~y:py ~style glyph
      done

  let marker ?style ?(glyph = "●") (layout : Layout.t) (grid : G.t) ~x ~y =
    let style = Option.value style ~default:layout.theme.marker in
    let px, py = Layout.px_of_data layout ~x ~y in
    let r = Layout.plot_rect layout in
    if Layout.rect_contains r ~x:px ~y:py then
      draw_text grid ~x:px ~y:py ~style glyph

  let tooltip ?style ?(border = `Theme) ?(padding = 1) ?(anchor = `Auto)
      (layout : Layout.t) (grid : G.t) ~x ~y (lines : string list) =
    let style = Option.value style ~default:layout.theme.tooltip in
    let border =
      match border with
      | `Theme -> layout.theme.tooltip_border
      | `None -> None
      | `Style s -> Some s
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
              draw_text grid ~x ~y ~style " "
          done
        done;

        (* border *)
        (match border with
        | None -> ()
        | Some bst ->
            let tf = layout.theme.charset.tooltip_frame in
            if box_w >= 2 && box_h >= 2 then (
              let top = y0 and bot = y0 + box_h - 1 in
              let left = x0 and right = x0 + box_w - 1 in
              draw_text grid ~x:left ~y:top ~style:bst tf.tl;
              draw_text grid ~x:right ~y:top ~style:bst tf.tr;
              draw_text grid ~x:left ~y:bot ~style:bst tf.bl;
              draw_text grid ~x:right ~y:bot ~style:bst tf.br;
              for xx = left + 1 to right - 1 do
                draw_text grid ~x:xx ~y:top ~style:bst tf.h;
                draw_text grid ~x:xx ~y:bot ~style:bst tf.h
              done;
              for yy = top + 1 to bot - 1 do
                draw_text grid ~x:left ~y:yy ~style:bst tf.v;
                draw_text grid ~x:right ~y:yy ~style:bst tf.v
              done));
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
              draw_text grid ~x:inner_x ~y ~style line)
          lines

  let text ?style ?(anchor = `Left) ?(v_anchor = `Middle) (layout : Layout.t)
      (grid : G.t) ~x ~y label =
    let style = Option.value style ~default:layout.theme.labels in
    let px, py = Layout.px_of_data layout ~x ~y in
    let rect = Layout.plot_rect layout in
    let w = text_width label in
    (* Horizontal anchor offset *)
    let px =
      match anchor with
      | `Left -> px
      | `Center -> px - (w / 2)
      | `Right -> px - w
    in
    (* Vertical anchor offset - terminal has no sub-cell vertical positioning *)
    let py = match v_anchor with `Top -> py | `Middle -> py | `Bottom -> py in
    if py >= rect.y && py < rect.y + rect.height then
      draw_text grid ~x:px ~y:py ~style label

  let arrow ?style ?(head = `Arrow) (layout : Layout.t) (grid : G.t) ~x1 ~y1 ~x2
      ~y2 =
    let style = Option.value style ~default:layout.theme.labels in
    let ch = layout.theme.charset in
    let line_glyphs : G.line_glyphs =
      {
        G.h = ch.frame.h;
        v = ch.frame.v;
        diag_up = ch.diag_up;
        diag_down = ch.diag_down;
      }
    in
    let px1, py1 = Layout.px_of_data layout ~x:x1 ~y:y1 in
    let px2, py2 = Layout.px_of_data layout ~x:x2 ~y:y2 in
    let rect = Layout.plot_rect layout in
    (* Draw line from (px1,py1) to (px2,py2) *)
    G.draw_line grid ~x1:px1 ~y1:py1 ~x2:px2 ~y2:py2 ~style ~glyphs:line_glyphs
      ~kind:`Line ();
    (* Draw arrow head at endpoint if within plot *)
    if Layout.rect_contains rect ~x:px2 ~y:py2 then
      match head with
      | `None -> ()
      | `Dot -> draw_text grid ~x:px2 ~y:py2 ~style "●"
      | `Arrow ->
          (* Choose arrow glyph based on direction *)
          let dx = px2 - px1 in
          let dy = py2 - py1 in
          let glyph =
            if abs dx > abs dy then if dx > 0 then "→" else "←"
            else if dy > 0 then "↓"
            else "↑"
          in
          draw_text grid ~x:px2 ~y:py2 ~style glyph
end

(* --- Legend --- *)

module Legend = struct
  type item = { label : string; style : Style.t; marker : string }

  let draw ?(direction = `Vertical) ?(gap = 0) items (grid : G.t) ~width
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
              draw_text grid ~x:!x ~y ~style block;
              let x' = !x + w_block in
              if x' < width then draw_text grid ~x:x' ~y label;
              x := x' + w_label + gap))
          items
    | `Vertical ->
        let y = ref 0 in
        let gap = max 0 gap in
        List.iter
          (fun { label; style; marker } ->
            draw_text grid ~x:0 ~y:!y ~style marker;
            draw_text grid ~x:(text_width marker + 1) ~y:!y label;
            y := !y + 1 + gap)
          items

  let items_of_layout (layout : Layout.t) : item list =
    let extract_item ~label ~style ~marker =
      match label with
      | Some l -> Some { label = l; style; marker }
      | None -> None
    in
    let charset = layout.theme.charset in
    List.filter_map
      (function
        | CLine { label; style; glyph; _ } ->
            let marker =
              match glyph with Some g -> g | None -> charset.frame.h
            in
            extract_item ~label ~style ~marker
        | CLine_opt { label; style; glyph; _ } ->
            let marker =
              match glyph with Some g -> g | None -> charset.frame.h
            in
            extract_item ~label ~style ~marker
        | CScatter { label; style; glyph; _ } ->
            extract_item ~label ~style ~marker:glyph
        | CBars_y { label; style; _ } ->
            extract_item ~label ~style ~marker:charset.bar_fill
        | CBars_x { label; style; _ } ->
            extract_item ~label ~style ~marker:charset.bar_fill
        | CArea { label; style; _ } -> extract_item ~label ~style ~marker:"▒"
        | CFill_between { label; style; _ } ->
            extract_item ~label ~style ~marker:"▒"
        | CHistogram { label; style; _ } ->
            extract_item ~label ~style ~marker:charset.bar_fill
        | _ -> None)
      layout.marks
end

(* *)

type frame_config = { margins : int * int * int * int; inner_padding : int }
type frame = Auto | Manual of frame_config
type title = { text : string; style : Style.t option }

type t = {
  theme : Theme.t;
  frame : frame;
  x_scale : Scale.t;
  y_scale : Scale.t;
  y2_scale : Scale.t option; (* Secondary Y-axis scale *)
  x_axis : Axis.t;
  y_axis : Axis.t;
  y2_axis : Axis.t option; (* Secondary Y-axis configuration *)
  grid : Gridlines.t;
  marks_rev : Mark.t list; (* Stored in reverse order for O(1) add *)
  title : title option;
}

(* Get marks in correct order (reverses the internal list) *)
let marks t = List.rev t.marks_rev
let default_frame = Auto

let manual_frame ?(margins = (0, 0, 0, 0)) ?(inner_padding = 0) () =
  Manual { margins; inner_padding }

let apply_theme_defaults (theme : Theme.t) (a : Axis.t) =
  {
    a with
    style = Some (Option.value a.style ~default:theme.axes);
    tick_style = Some (Option.value a.tick_style ~default:theme.axes);
    label_style = Some (Option.value a.label_style ~default:theme.labels);
  }

let apply_grid_theme (theme : Theme.t) (g : Gridlines.t) =
  { g with style = (if g.style = Style.default then theme.grid else g.style) }

let empty ?(theme = Theme.default) () =
  {
    theme;
    frame = default_frame;
    x_scale = Scale.Auto;
    y_scale = Scale.Auto;
    y2_scale = None;
    x_axis = apply_theme_defaults theme Axis.default;
    y_axis = apply_theme_defaults theme Axis.default;
    y2_axis = None;
    grid = apply_grid_theme theme Gridlines.hidden;
    marks_rev = [];
    title = None;
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
  let frame =
    match frame with
    | Auto -> Auto
    | Manual cfg -> Manual { cfg with inner_padding = max 0 cfg.inner_padding }
  in
  { t with frame }

let with_title ?style text t = { t with title = Some { text; style } }
let with_x_scale x_scale t = { t with x_scale }
let with_y_scale y_scale t = { t with y_scale }
let with_y2_scale y2_scale t = { t with y2_scale = Some y2_scale }

let with_axes ?x ?y t =
  let x_axis =
    match x with None -> t.x_axis | Some a -> apply_theme_defaults t.theme a
  in
  let y_axis =
    match y with None -> t.y_axis | Some a -> apply_theme_defaults t.theme a
  in
  { t with x_axis; y_axis }

let with_y2_axis y2_axis t =
  { t with y2_axis = Some (apply_theme_defaults t.theme y2_axis) }

let with_grid g t = { t with grid = apply_grid_theme t.theme g }

(* O(1) mark addition by prepending to reversed list *)
let add m t = { t with marks_rev = m :: t.marks_rev }

let line ?id ?label ?style ?resolution ?pattern ?glyph ?y_axis ~x ~y data t =
  add
    (Mark.line ?id ?label ?style ?resolution ?pattern ?glyph ?y_axis ~x ~y data)
    t

let line_opt ?id ?label ?style ?resolution ?pattern ?glyph ?y_axis ~x ~y data t
    =
  add
    (Mark.line_opt ?id ?label ?style ?resolution ?pattern ?glyph ?y_axis ~x ~y
       data)
    t

let scatter ?id ?label ?style ?glyph ?mode ?y_axis ~x ~y data t =
  add (Mark.scatter ?id ?label ?style ?glyph ?mode ?y_axis ~x ~y data) t

let bars_y ?id ?label ?style ?mode ~x ~y data t =
  add (Mark.bars_y ?id ?label ?style ?mode ~x ~y data) t

let bars_x ?id ?label ?style ?mode ~y ~x data t =
  add (Mark.bars_x ?id ?label ?style ?mode ~y ~x data) t

let stacked_bars_y ?id ?gap ?bar_width ?mode data t =
  add (Mark.stacked_bars_y ?id ?gap ?bar_width ?mode data) t

let stacked_bars_x ?id ?gap ?bar_height ?mode data t =
  add (Mark.stacked_bars_x ?id ?gap ?bar_height ?mode data) t

let rule_y ?id ?style ?pattern ?y_axis y t =
  add (Mark.rule_y ?id ?style ?pattern ?y_axis y) t

let rule_x ?id ?style ?pattern x t = add (Mark.rule_x ?id ?style ?pattern x) t

let heatmap ?id ?color_scale ?value_range ?auto_value_range ?agg ?mode ~x ~y
    ~value data t =
  add
    (Mark.heatmap ?id ?color_scale ?value_range ?auto_value_range ?agg ?mode ~x
       ~y ~value data)
    t

let candles ?id ?bullish ?bearish ?width ?body ?y_axis data t =
  add (Mark.candles ?id ?bullish ?bearish ?width ?body ?y_axis data) t

let circle ?id ?style ?resolution ?y_axis ~cx ~cy ~r data t =
  add (Mark.circle ?id ?style ?resolution ?y_axis ~cx ~cy ~r data) t

let shade_x ?id ?style ~min ~max () t =
  add (Mark.shade_x ?id ?style ~min ~max ()) t

let column_background ?id ?style x t =
  add (Mark.column_background ?id ?style x) t

let area ?id ?label ?style ?baseline ?resolution ?y_axis ~x ~y data t =
  add (Mark.area ?id ?label ?style ?baseline ?resolution ?y_axis ~x ~y data) t

let fill_between ?id ?label ?style ?resolution ?y_axis ~x ~y_low ~y_high data t
    =
  add
    (Mark.fill_between ?id ?label ?style ?resolution ?y_axis ~x ~y_low ~y_high
       data)
    t

let histogram ?id ?label ?style ?bins ?normalize ~x data t =
  add (Mark.histogram ?id ?label ?style ?bins ?normalize ~x data) t

(* --- compilation (layout) --- *)

type dom_acc = {
  mutable has : bool;
  mutable minv : float;
  mutable maxv : float;
}

let dom_acc () = { has = false; minv = 0.; maxv = 0. }

let dom_add acc v =
  (* Skip non-finite values (NaN, Infinity) to prevent domain corruption *)
  if not (Float.is_finite v) then ()
  else if not acc.has then (
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
  | Scale.Log { base; clamp; domain = _ } -> `Log (base, clamp)
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
  | Scale.Log { base; clamp; domain = _ } -> `Log (base, clamp)
  | Scale.Auto ->
      let has_band =
        List.exists
          (function
            | Mark.Bars_x _ | Mark.Stacked_bars_x _ -> true | _ -> false)
          marks
      in
      if has_band then `Band (infer_band_categories_y marks, 0.1)
      else `Numeric true

type axis_kind_poly =
  [ `Band of string list * float | `Numeric of bool | `Log of float * bool ]

(* Histogram binning helpers - defined here so they can be used in domain
   inference *)
let compute_histogram_bins ~(bins : Mark.bin_method)
    ~(normalize : Mark.histogram_normalize) ~(x : 'a -> float) (data : 'a array)
    : float array * float array =
  let n = Array.length data in
  if n = 0 then ([||], [||])
  else
    (* Extract values and compute range *)
    let values = Array.map x data in
    Array.sort Float.compare values;
    let vmin = values.(0) in
    let vmax = values.(n - 1) in
    let range = vmax -. vmin in
    let range = if range <= 0. then 1. else range in
    (* Compute bin edges *)
    let edges =
      match bins with
      | Mark.Bins num_bins ->
          let num_bins = max 1 num_bins in
          let width = range /. float num_bins in
          Array.init (num_bins + 1) (fun i -> vmin +. (float i *. width))
      | Mark.Width w ->
          let w = if w <= 0. then range /. 10. else w in
          let num_bins = max 1 (int_of_float (Float.ceil (range /. w))) in
          Array.init (num_bins + 1) (fun i -> vmin +. (float i *. w))
      | Mark.Edges e ->
          (* Edges array must have at least 2 elements to define one bin. If
             invalid, fall back to auto-computed edges. *)
          if Array.length e >= 2 then e
          else
            let num_bins = 10 in
            let width = range /. float num_bins in
            Array.init (num_bins + 1) (fun i -> vmin +. (float i *. width))
    in
    let num_bins = Array.length edges - 1 in
    (* Count values in each bin *)
    let counts = Array.make num_bins 0 in
    Array.iter
      (fun v ->
        (* Find bin using binary search *)
        let rec find lo hi =
          if lo >= hi then lo
          else
            let mid = (lo + hi) / 2 in
            if mid + 1 < Array.length edges && v >= edges.(mid + 1) then
              find (mid + 1) hi
            else find lo mid
        in
        let bin = min (num_bins - 1) (find 0 (num_bins - 1)) in
        counts.(bin) <- counts.(bin) + 1)
      values;
    (* Normalize counts *)
    let bin_values =
      match normalize with
      | `Count -> Array.map float counts
      | `Probability ->
          let total = float n in
          Array.map (fun c -> float c /. total) counts
      | `Density ->
          let total = float n in
          Array.mapi
            (fun i c ->
              let bin_width =
                if i + 1 < Array.length edges then edges.(i + 1) -. edges.(i)
                else 1.
              in
              float c /. (total *. bin_width))
            counts
    in
    (edges, bin_values)

let resolve_numeric_domains (t : t) (x_kind : axis_kind_poly)
    (y_kind : axis_kind_poly) =
  let xacc = dom_acc () in
  let y1acc = dom_acc () in
  let y2acc = dom_acc () in

  (* Helper: add to correct Y accumulator based on y_axis selector *)
  let add_y (y_axis : Mark.y_axis_selector) v =
    match y_axis with `Y1 -> dom_add y1acc v | `Y2 -> dom_add y2acc v
  in

  (* include baseline 0 for bars/stacked bars on numeric axis *)
  let include_zero_y1 () = dom_add y1acc 0. in
  let include_zero_x () = dom_add xacc 0. in

  List.iter
    (function
      | Mark.Line { x; y; y_axis; data; _ } ->
          Data.iter data (fun a ->
              dom_add xacc (x a);
              add_y y_axis (y a))
      | Mark.Line_opt { x; y; y_axis; data; _ } ->
          Data.iter data (fun a ->
              dom_add xacc (x a);
              match y a with None -> () | Some yy -> add_y y_axis yy)
      | Mark.Scatter { x; y; y_axis; data; _ } ->
          Data.iter data (fun a ->
              dom_add xacc (x a);
              add_y y_axis (y a))
      | Mark.Bars_y { y; data; _ } ->
          include_zero_y1 ();
          Data.iter data (fun a -> dom_add y1acc (y a))
      | Mark.Bars_x { x; data; _ } ->
          include_zero_x ();
          Data.iter data (fun a -> dom_add xacc (x a))
      | Mark.Stacked_bars_y { data; _ } ->
          include_zero_y1 ();
          Data.iter data (fun b ->
              let total =
                List.fold_left
                  (fun acc s -> acc +. max 0. s.Mark.value)
                  0. b.segments
              in
              dom_add y1acc total)
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
      | Mark.Rule_y { y; y_axis; _ } -> add_y y_axis y
      | Mark.Heatmap { x; y; data; _ } ->
          Data.iter data (fun a ->
              dom_add xacc (x a);
              dom_add y1acc (y a))
      | Mark.Candles { y_axis; data; _ } ->
          Data.iter data (fun o ->
              dom_add xacc o.time;
              add_y y_axis o.open_;
              add_y y_axis o.close;
              add_y y_axis o.high;
              add_y y_axis o.low)
      | Mark.Circle { cx; cy; r; y_axis; data; _ } ->
          Data.iter data (fun a ->
              let cxv = cx a and cyv = cy a and rv = max 0. (r a) in
              dom_add xacc (cxv -. rv);
              dom_add xacc (cxv +. rv);
              add_y y_axis (cyv -. rv);
              add_y y_axis (cyv +. rv))
      | Mark.Shade_x { x0; x1; _ } ->
          dom_add xacc x0;
          dom_add xacc x1
      | Mark.Column_background { x; _ } -> dom_add xacc x
      | Mark.Area { x; y; baseline; y_axis; data; _ } ->
          (match baseline with
          | `Zero -> add_y y_axis 0.
          | `Value v -> add_y y_axis v);
          Data.iter data (fun a ->
              dom_add xacc (x a);
              add_y y_axis (y a))
      | Mark.Fill_between { x; y_low; y_high; y_axis; data; _ } ->
          Data.iter data (fun a ->
              dom_add xacc (x a);
              add_y y_axis (y_low a);
              add_y y_axis (y_high a))
      | Mark.Histogram { bins; normalize; x; data; _ } ->
          (* Include data range on x-axis *)
          Data.iter data (fun a -> dom_add xacc (x a));
          (* Compute histogram bins to determine y-axis domain *)
          include_zero_y1 ();
          let _bin_edges, bin_values =
            compute_histogram_bins ~bins ~normalize ~x data
          in
          (* Add max bin value to y domain *)
          if Array.length bin_values > 0 then
            let max_bin = Array.fold_left Float.max bin_values.(0) bin_values in
            dom_add y1acc max_bin)
    (marks t);

  (* If axis is Band, numeric domain becomes [0,n] internal. *)
  let x_domain =
    match x_kind with
    | `Band (cats, _) ->
        let n = max 1 (List.length cats) in
        View.window ~min:0. ~max:(float n)
    | `Numeric _ | `Log _ -> dom_finish xacc
  in
  let y1_domain =
    match y_kind with
    | `Band (cats, _) ->
        let n = max 1 (List.length cats) in
        View.window ~min:0. ~max:(float n)
    | `Numeric _ | `Log _ -> dom_finish y1acc
  in
  let y2_domain = dom_finish y2acc in
  (x_domain, y1_domain, y2_domain)

let apply_domain_override (scale : Scale.t) (dom : View.window) : View.window =
  match scale with
  | Scale.Numeric { domain = `Domain (a, b); _ }
  | Scale.Log { domain = `Domain (a, b); _ } ->
      View.window ~min:a ~max:b
  | _ -> dom

let resolve_view ~clamp (dom : View.window) (view_opt : View.window option) :
    View.window =
  match view_opt with
  | None -> dom
  | Some v -> if clamp then View.clamp ~domain:dom v else v

(* Validate and adjust log scale domain. If the domain contains non-positive
   values, adjust to use a safe minimum (1e-10 or the smallest positive value).
   Returns the adjusted (min, max) pair. *)
let validate_log_domain (w : View.window) : View.window =
  let safe_min = 1e-10 in
  let min' = if w.min <= 0. then safe_min else w.min in
  let max' = if w.max <= 0. then safe_min *. 10. else w.max in
  (* Ensure min < max *)
  let min', max' = if min' >= max' then (min', min' *. 10.) else (min', max') in
  { View.min = min'; max = max' }

let resolve_style_from_palette (theme : Theme.t) (i : int) : Style.t =
  let len = Array.length theme.palette in
  let color = if len = 0 then Color.default else theme.palette.(i mod len) in
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
      | Mark.Line
          { id; label; style; resolution; y_axis; pattern; glyph; x; y; data }
        ->
          let st = Option.value style ~default:(next_default ()) in
          CLine
            {
              id;
              label;
              resolution;
              pattern;
              glyph;
              style = st;
              y_axis;
              x;
              y;
              data;
            }
      | Mark.Line_opt
          { id; label; style; resolution; y_axis; pattern; glyph; x; y; data }
        ->
          let st = Option.value style ~default:(next_default ()) in
          CLine_opt
            {
              id;
              label;
              resolution;
              pattern;
              glyph;
              style = st;
              y_axis;
              x;
              y;
              data;
            }
      | Mark.Scatter { id; label; style; glyph; mode; y_axis; x; y; data } ->
          let st = Option.value style ~default:(next_default ()) in
          let glyph = Option.value glyph ~default:theme.charset.point_default in
          CScatter { id; label; mode; glyph; style = st; y_axis; x; y; data }
      | Mark.Bars_y { id; label; style; mode; x; y; data } ->
          let st = Option.value style ~default:(next_default ()) in
          CBars_y { id; label; mode; style = st; x; y; data }
      | Mark.Bars_x { id; label; style; mode; y; x; data } ->
          let st = Option.value style ~default:(next_default ()) in
          CBars_x { id; label; mode; style = st; y; x; data }
      | Mark.Stacked_bars_y { id; gap; bar_width; mode; data } ->
          CStacked_y { id; gap; bar_width; mode; data }
      | Mark.Stacked_bars_x { id; gap; bar_height; mode; data } ->
          CStacked_x { id; gap; bar_height; mode; data }
      | Mark.Rule_y { id; style; pattern; y_axis; y } ->
          let st = Option.value style ~default:(next_default ()) in
          CRule_y { id; style = st; pattern; y_axis; y }
      | Mark.Rule_x { id; style; pattern; x } ->
          let st = Option.value style ~default:(next_default ()) in
          CRule_x { id; style = st; pattern; x }
      | Mark.Heatmap
          {
            id;
            color_scale;
            value_range;
            auto_value_range;
            agg;
            mode;
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
              mode;
              x;
              y;
              value;
              data;
            }
      | Mark.Candles { id; bullish; bearish; width; body; y_axis; data } ->
          (* Pre-sort candles by time during compilation for O(1) draw access *)
          let sorted_data =
            Array.copy data |> fun arr ->
            Array.sort (fun a b -> compare a.Mark.time b.Mark.time) arr;
            arr
          in
          CCandles
            { id; bullish; bearish; width; body; y_axis; data = sorted_data }
      | Mark.Circle { id; style; resolution; y_axis; cx; cy; r; data } ->
          let st = Option.value style ~default:(next_default ()) in
          CCircle { id; resolution; style = st; y_axis; cx; cy; r; data }
      | Mark.Shade_x { id; style; x0; x1 } ->
          let st = Option.value style ~default:(default_shade_style ()) in
          CShade_x { id; style = st; x0; x1 }
      | Mark.Column_background { id; style; x } ->
          let st = Option.value style ~default:(default_shade_style ()) in
          CColumn_bg { id; style = st; x }
      | Mark.Area { id; label; style; baseline; resolution; y_axis; x; y; data }
        ->
          let st = Option.value style ~default:(next_default ()) in
          CArea
            { id; label; style = st; baseline; resolution; y_axis; x; y; data }
      | Mark.Fill_between
          { id; label; style; resolution; y_axis; x; y_low; y_high; data } ->
          let st = Option.value style ~default:(next_default ()) in
          CFill_between
            {
              id;
              label;
              style = st;
              resolution;
              y_axis;
              x;
              y_low;
              y_high;
              data;
            }
      | Mark.Histogram { id; label; style; bins; normalize; x; data } ->
          let st = Option.value style ~default:(next_default ()) in
          let bin_edges, bin_values =
            compute_histogram_bins ~bins ~normalize ~x data
          in
          CHistogram { id; label; style = st; bin_edges; bin_values })
    marks

(* Compute axis label width using the actual tick values that will be displayed.
   This ensures consistent layout regardless of scale type (linear vs log). *)
let axis_label_width_from_ticks (axis : Axis.t) (tick_values : float list) : int
    =
  if not axis.show then 0
  else
    List.fold_left
      (fun (acc, i) v ->
        let s = axis.format i v in
        (max acc (text_width s), i + 1))
      (0, 0) tick_values
    |> fst

let axis_label_width_numeric (axis : Axis.t) (w : View.window) : int =
  if not axis.show then 0
  else
    let tick_values =
      nice_ticks ~min_val:w.min ~max_val:w.max ~target_ticks:axis.ticks
    in
    axis_label_width_from_ticks axis tick_values

let axis_label_width_log (axis : Axis.t) ~base (w : View.window) : int =
  if not axis.show then 0
  else
    let tick_values =
      log_ticks ~base ~min_val:w.min ~max_val:w.max ~target_ticks:axis.ticks
    in
    axis_label_width_from_ticks axis tick_values

let axis_label_width_band (axis : Axis.t) (cats : string list) : int =
  if not axis.show then 0
  else List.fold_left (fun acc s -> max acc (text_width s)) 0 cats

let compute_layout ?(view = View.empty) ?(x = 0) ?(y = 0) (t : t) ~width ~height
    : Layout.t =
  let width = max 1 width and height = max 1 height in
  (* Resolve frame config - Auto mode computes margins from axis requirements *)
  let mt, mr, mb, ml, ip =
    match t.frame with
    | Manual { margins = mt, mr, mb, ml; inner_padding } ->
        (max 0 mt, max 0 mr, max 0 mb, max 0 ml, max 0 inner_padding)
    | Auto ->
        (* In Auto mode: left margin accommodates y-axis, bottom accommodates
           x-axis *)
        (0, 0, 0, 0, 0)
  in

  let t_marks = marks t in
  let x_kind = infer_axis_kind_x t.x_scale t_marks in
  let y_kind = infer_axis_kind_y t.y_scale t_marks in

  let x_domain0, y1_domain0, y2_domain0 =
    resolve_numeric_domains t x_kind y_kind
  in
  let x_domain = apply_domain_override t.x_scale x_domain0 in
  let y_domain = apply_domain_override t.y_scale y1_domain0 in

  let x_scale_res =
    match x_kind with
    | `Band (cats, padding) ->
        let dom = View.window ~min:0. ~max:(float (max 1 (List.length cats))) in
        make_band ~categories:cats ~padding ~domain:dom ~view:dom
    | `Numeric clamp ->
        let dom = x_domain in
        let v = resolve_view ~clamp dom view.View.x in
        Numeric { domain = dom; view = v; clamp }
    | `Log (base, clamp) ->
        (* Validate log domain - adjust non-positive values to safe minimum *)
        let dom = validate_log_domain x_domain in
        let v = resolve_view ~clamp dom view.View.x in
        let v = validate_log_domain v in
        Log { base; domain = dom; view = v; clamp }
  in

  let y_scale_res =
    match y_kind with
    | `Band (cats, padding) ->
        let dom = View.window ~min:0. ~max:(float (max 1 (List.length cats))) in
        make_band ~categories:cats ~padding ~domain:dom ~view:dom
    | `Numeric clamp ->
        let dom = y_domain in
        let v = resolve_view ~clamp dom view.View.y in
        Numeric { domain = dom; view = v; clamp }
    | `Log (base, clamp) ->
        (* Validate log domain - adjust non-positive values to safe minimum *)
        let dom = validate_log_domain y_domain in
        let v = resolve_view ~clamp dom view.View.y in
        let v = validate_log_domain v in
        Log { base; domain = dom; view = v; clamp }
  in

  (* Y2 scale resolution - only if y2_scale is configured *)
  let y2_scale_res, y2_domain_res, y2_view_res =
    match t.y2_scale with
    | None -> (None, None, None)
    | Some scale ->
        let y2_kind = infer_axis_kind_y scale t_marks in
        let y2_dom =
          match y2_kind with
          | `Band _ -> y2_domain0 (* fallback to computed Y2 domain *)
          | `Numeric _ | `Log _ -> apply_domain_override scale y2_domain0
        in
        let res =
          match y2_kind with
          | `Band (cats, padding) ->
              let dom =
                View.window ~min:0. ~max:(float (max 1 (List.length cats)))
              in
              make_band ~categories:cats ~padding ~domain:dom ~view:dom
          | `Numeric clamp ->
              (* Use y2 view if provided, otherwise use domain (not y view!) *)
              let v = resolve_view ~clamp y2_dom view.View.y2 in
              Numeric { domain = y2_dom; view = v; clamp }
          | `Log (base, clamp) ->
              (* Validate log domain - adjust non-positive values to safe
                 minimum *)
              let y2_dom = validate_log_domain y2_dom in
              (* Use y2 view if provided, otherwise use domain (not y view!) *)
              let v = resolve_view ~clamp y2_dom view.View.y2 in
              let v = validate_log_domain v in
              Log { base; domain = y2_dom; view = v; clamp }
        in
        let view_win =
          match res with
          | Numeric { view; _ } | Log { view; _ } | Band { view; _ } -> view
        in
        (Some res, Some y2_dom, Some view_win)
  in

  let x_view =
    match x_scale_res with
    | Numeric { view; _ } | Log { view; _ } -> view
    | Band { view; _ } -> view
  in
  let y_view =
    match y_scale_res with
    | Numeric { view; _ } | Log { view; _ } -> view
    | Band { view; _ } -> view
  in

  (* axis reserved sizes - use domain (not view) for stable layout during
     zoom *)
  let y_axis_width =
    if not t.y_axis.show then 0
    else
      let label_w =
        match y_scale_res with
        | Numeric { domain; _ } -> axis_label_width_numeric t.y_axis domain
        | Log { base; domain; _ } -> axis_label_width_log t.y_axis ~base domain
        | Band { categories; _ } -> axis_label_width_band t.y_axis categories
      in
      (* label area + padding + tick_length + axis line *)
      label_w + t.y_axis.label_padding + t.y_axis.tick_length + 1
  in
  let x_axis_height =
    if not t.x_axis.show then 0
    else
      (* axis line + tick_length + label_padding + label row + optional title
         row *)
      let base = 1 + t.x_axis.tick_length + t.x_axis.label_padding + 1 in
      let title_space = match t.x_axis.title with None -> 0 | Some _ -> 1 in
      base + title_space
  in
  (* Y-axis title space - reserve columns to the left if y-axis has a title
     (title + gap) *)
  let y_axis_title_width =
    match t.y_axis.title with None -> 0 | Some _ -> 3
  in

  (* Y2 axis width - computed if y2_axis is configured *)
  let y2_axis_width =
    match (t.y2_axis, y2_scale_res) with
    | Some y2_ax, Some y2_sc when y2_ax.Axis.show ->
        let label_w =
          match y2_sc with
          | Numeric { domain; _ } -> axis_label_width_numeric y2_ax domain
          | Log { base; domain; _ } -> axis_label_width_log y2_ax ~base domain
          | Band { categories; _ } -> axis_label_width_band y2_ax categories
        in
        label_w + y2_ax.label_padding + y2_ax.tick_length + 1
    | _ -> 0
  in

  let plot_x_rel = ml + y_axis_title_width + y_axis_width + ip in
  let plot_y_rel = mt + ip in
  let plot_w = max 1 (width - plot_x_rel - mr - ip - y2_axis_width) in
  let plot_h = max 1 (height - plot_y_rel - (mb + x_axis_height + ip)) in
  let plot_x = x + plot_x_rel in
  let plot_y = y + plot_y_rel in
  let plot : Layout.rect =
    { x = plot_x; y = plot_y; width = plot_w; height = plot_h }
  in

  let compiled_marks = compile_marks t.theme t_marks in

  (* Resolve title info *)
  let title =
    match t.title with
    | None -> None
    | Some { text; style } ->
        let st = Option.value style ~default:t.theme.labels in
        Some Layout.{ text; style = st }
  in

  (* Precompute tick values for axes (computed once, used in draw_grid and
     draw_axes) *)
  let x_ticks =
    match x_scale_res with
    | Numeric _ ->
        nice_ticks ~min_val:x_view.min ~max_val:x_view.max
          ~target_ticks:t.x_axis.ticks
    | Log { base; view; _ } ->
        log_ticks ~base ~min_val:view.min ~max_val:view.max
          ~target_ticks:t.x_axis.ticks
    | Band _ -> [] (* Band scales use categories, not numeric ticks *)
  in
  let y_ticks =
    match y_scale_res with
    | Numeric _ ->
        nice_ticks ~min_val:y_view.min ~max_val:y_view.max
          ~target_ticks:t.y_axis.ticks
    | Log { base; view; _ } ->
        log_ticks ~base ~min_val:view.min ~max_val:view.max
          ~target_ticks:t.y_axis.ticks
    | Band _ -> []
  in
  let y2_ticks =
    match (y2_scale_res, t.y2_axis) with
    | Some (Numeric _), Some y2_ax ->
        let y2v = Option.get y2_view_res in
        Some
          (nice_ticks ~min_val:y2v.min ~max_val:y2v.max
             ~target_ticks:y2_ax.ticks)
    | Some (Log { base; view; _ }), Some y2_ax ->
        Some
          (log_ticks ~base ~min_val:view.min ~max_val:view.max
             ~target_ticks:y2_ax.ticks)
    | _ -> None
  in

  {
    Layout.width;
    height;
    plot;
    theme = t.theme;
    x_scale = x_scale_res;
    y_scale = y_scale_res;
    y2_scale = y2_scale_res;
    x_domain;
    y_domain;
    y2_domain = y2_domain_res;
    x_view;
    y_view;
    y2_view = y2_view_res;
    x_ticks;
    y_ticks;
    y2_ticks;
    x_axis = t.x_axis;
    y_axis = t.y_axis;
    y2_axis = t.y2_axis;
    grid = t.grid;
    frame_inner_padding = ip;
    margin_left = ml;
    y_axis_width;
    y_axis_title_width;
    y2_axis_width;
    (* Computed above based on y2_axis *)
    marks = compiled_marks;
    title;
  }

let layout ?view ?x ?y t ~width ~height =
  compute_layout ?view ?x ?y t ~width ~height

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

(* Log scale pixel mapping helpers - use log_transform defined earlier *)
let log_scale_to_px ~base ~minv ~maxv ~extent ~origin ~clamp v =
  (* Apply safe minimum for log scale - values <= 0 are mapped to the safe
     min *)
  let safe_minv = Float.max 1e-10 minv in
  let safe_maxv = Float.max 1e-10 maxv in
  if Float.abs (safe_maxv -. safe_minv) < 1e-12 then origin
  else if extent <= 1 then origin
  else
    let log_min = log_transform ~base safe_minv in
    let log_max = log_transform ~base safe_maxv in
    let safe_v = Float.max 1e-10 v in
    let log_v = log_transform ~base safe_v in
    let t = (log_v -. log_min) /. (log_max -. log_min) in
    let t = if clamp then clamp01 t else t in
    origin + int_of_float (Float.round (t *. float (extent - 1)))

let y_to_px_log ~base ~minv ~maxv ~extent ~origin ~clamp v =
  (* invert y for log scale *)
  log_scale_to_px ~base ~minv:maxv ~maxv:minv ~extent ~origin ~clamp v

let x_to_px_log ~base ~minv ~maxv ~extent ~origin ~clamp v =
  log_scale_to_px ~base ~minv ~maxv ~extent ~origin ~clamp v

let draw_grid (layout : Layout.t) (grid : G.t) =
  let g = layout.grid in
  if not g.show then ()
  else
    let r = layout.plot in
    let style = g.style in
    let minor_style =
      match g.minor_style with Some s -> s | None -> layout.theme.grid_minor
    in
    let ch = layout.theme.charset in
    (* Get the appropriate glyph based on pattern *)
    let v_glyph =
      match g.pattern with
      | `Solid -> ch.grid_v_solid
      | `Dashed -> ch.grid_v_dashed
      | `Dotted -> ch.grid_v_dotted
    in
    let h_glyph =
      match g.pattern with
      | `Solid -> ch.grid_h_solid
      | `Dashed -> ch.grid_h_dashed
      | `Dotted -> ch.grid_h_dotted
    in
    (* Draw a vertical grid line at x using the pattern glyph *)
    let draw_v_line ?(minor = false) x =
      let st = if minor then minor_style else style in
      for y = r.y to r.y + r.height - 1 do
        draw_text grid ~x ~y ~style:st v_glyph
      done
    in
    (* Draw a horizontal grid line at y using the pattern glyph *)
    let draw_h_line ?(minor = false) y =
      let st = if minor then minor_style else style in
      for x = r.x to r.x + r.width - 1 do
        draw_text grid ~x ~y ~style:st h_glyph
      done
    in
    let draw_every_x step =
      if step > 0 then
        for x = r.x to r.x + r.width - 1 do
          if (x - r.x) mod step = 0 then draw_v_line x
        done
    in
    let draw_every_y step =
      if step > 0 then
        for y = r.y to r.y + r.height - 1 do
          if (y - r.y) mod step = 0 then draw_h_line y
        done
    in
    (* Draw minor gridlines between consecutive pixel positions *)
    let draw_minor_v ~n px0 px1 =
      if n > 1 then
        let span = px1 - px0 in
        for i = 1 to n - 1 do
          let px = px0 + (span * i / n) in
          if px > px0 && px < px1 && px >= r.x && px < r.x + r.width then
            draw_v_line ~minor:true px
        done
    in
    let draw_minor_h ~n py0 py1 =
      if n > 1 then
        let span = py1 - py0 in
        for i = 1 to n - 1 do
          let py = py0 + (span * i / n) in
          if py > py0 && py < py1 && py >= r.y && py < r.y + r.height then
            draw_h_line ~minor:true py
        done
    in
    (* explicit step wins, else use precomputed ticks if numeric. *)
    (match g.x_step with
    | Some s -> if g.x then draw_every_x s
    | None -> (
        if g.x then
          match layout.x_scale with
          | Numeric _ ->
              (* Use precomputed ticks from layout *)
              let prev_px = ref None in
              List.iter
                (fun v ->
                  let px =
                    x_to_px ~minv:layout.x_view.min ~maxv:layout.x_view.max
                      ~extent:r.width ~origin:r.x ~clamp:true v
                  in
                  (match (g.minor, !prev_px) with
                  | Some n, Some px0 -> draw_minor_v ~n px0 px
                  | _ -> ());
                  prev_px := Some px;
                  draw_v_line px)
                layout.x_ticks
          | Log { base; view; _ } ->
              (* Use precomputed ticks from layout *)
              let prev_px = ref None in
              List.iter
                (fun v ->
                  let px =
                    x_to_px_log ~base ~minv:view.min ~maxv:view.max
                      ~extent:r.width ~origin:r.x ~clamp:true v
                  in
                  (match (g.minor, !prev_px) with
                  | Some n, Some px0 -> draw_minor_v ~n px0 px
                  | _ -> ());
                  prev_px := Some px;
                  draw_v_line px)
                layout.x_ticks
          | Band { categories; padding; _ } ->
              let offset, bw =
                Layout.band_params ~cats:categories ~padding ~extent:r.width
              in
              let n = List.length categories in
              for i = 0 to n do
                let x0 =
                  r.x + int_of_float (Float.round (offset +. (float i *. bw)))
                in
                (* Clamp to plot bounds to avoid drawing outside *)
                if x0 >= r.x && x0 < r.x + r.width then draw_v_line x0
              done));

    match g.y_step with
    | Some s -> if g.y then draw_every_y s
    | None -> (
        if g.y then
          match layout.y_scale with
          | Numeric _ ->
              (* Use precomputed ticks from layout *)
              let prev_py = ref None in
              List.iter
                (fun v ->
                  let py =
                    y_to_px ~minv:layout.y_view.min ~maxv:layout.y_view.max
                      ~extent:r.height ~origin:r.y ~clamp:true v
                  in
                  (match (g.minor, !prev_py) with
                  | Some n, Some py0 -> draw_minor_h ~n py0 py
                  | _ -> ());
                  prev_py := Some py;
                  draw_h_line py)
                layout.y_ticks
          | Log { base; view; _ } ->
              (* Use precomputed ticks from layout *)
              let prev_py = ref None in
              List.iter
                (fun v ->
                  let py =
                    y_to_px_log ~base ~minv:view.min ~maxv:view.max
                      ~extent:r.height ~origin:r.y ~clamp:true v
                  in
                  (match (g.minor, !prev_py) with
                  | Some n, Some py0 -> draw_minor_h ~n py0 py
                  | _ -> ());
                  prev_py := Some py;
                  draw_h_line py)
                layout.y_ticks
          | Band { categories; padding; _ } ->
              let offset, bw =
                Layout.band_params ~cats:categories ~padding ~extent:r.height
              in
              let n = List.length categories in
              for i = 0 to n do
                let y0 =
                  r.y + int_of_float (Float.round (offset +. (float i *. bw)))
                in
                (* Clamp to plot bounds to avoid drawing outside *)
                if y0 >= r.y && y0 < r.y + r.height then draw_h_line y0
              done)

let draw_axes (layout : Layout.t) (grid : G.t) =
  let r = layout.plot in
  let ip = layout.frame_inner_padding in
  let ch = layout.theme.charset in
  let line_glyphs : G.line_glyphs =
    {
      G.h = ch.frame.h;
      v = ch.frame.v;
      diag_up = ch.diag_up;
      diag_down = ch.diag_down;
    }
  in

  (* Helper to draw frame lines based on Axis.line setting *)
  let draw_y_axis_line ~ax ~st =
    match layout.y_axis.line with
    | `None -> ()
    | `Axis_only ->
        G.draw_line grid ~x1:ax ~y1:r.y ~x2:ax
          ~y2:(r.y + r.height - 1)
          ~style:st ~glyphs:line_glyphs ~kind:`Line ()
    | `Frame ->
        (* Draw left side of frame and top/bottom edges *)
        G.draw_line grid ~x1:ax ~y1:r.y ~x2:ax
          ~y2:(r.y + r.height - 1)
          ~style:st ~glyphs:line_glyphs ~kind:`Line ();
        (* Draw top edge *)
        G.draw_line grid ~x1:ax ~y1:r.y
          ~x2:(r.x + r.width - 1)
          ~y2:r.y ~style:st ~glyphs:line_glyphs ~kind:`Line ();
        (* Draw bottom edge (if x axis is hidden) *)
        if not layout.x_axis.show then
          G.draw_line grid ~x1:ax
            ~y1:(r.y + r.height - 1)
            ~x2:(r.x + r.width - 1)
            ~y2:(r.y + r.height - 1)
            ~style:st ~glyphs:line_glyphs ~kind:`Line ()
  in

  let draw_x_axis_line ~ay ~st =
    match layout.x_axis.line with
    | `None -> ()
    | `Axis_only ->
        G.draw_line grid ~x1:r.x ~y1:ay
          ~x2:(r.x + r.width - 1)
          ~y2:ay ~style:st ~glyphs:line_glyphs ~kind:`Line ()
    | `Frame ->
        (* Draw bottom edge and left/right if y axis is hidden *)
        G.draw_line grid ~x1:r.x ~y1:ay
          ~x2:(r.x + r.width - 1)
          ~y2:ay ~style:st ~glyphs:line_glyphs ~kind:`Line ();
        (* Draw right edge *)
        G.draw_line grid
          ~x1:(r.x + r.width - 1)
          ~y1:r.y
          ~x2:(r.x + r.width - 1)
          ~y2:ay ~style:st ~glyphs:line_glyphs ~kind:`Line ();
        (* Draw left edge (if y axis is hidden) *)
        if not layout.y_axis.show then
          G.draw_line grid ~x1:r.x ~y1:r.y ~x2:r.x ~y2:ay ~style:st
            ~glyphs:line_glyphs ~kind:`Line ()
  in

  (* y axis *)
  if layout.y_axis.show then (
    let ax =
      layout.margin_left + layout.y_axis_title_width + layout.y_axis_width - 1
    in
    let st = Option.get layout.y_axis.style in
    let tick_st = Option.get layout.y_axis.tick_style in
    let label_st = Option.get layout.y_axis.label_style in
    draw_y_axis_line ~ax ~st;

    (* ticks + labels - use precomputed ticks from layout *)
    (match layout.y_scale with
    | Numeric _ ->
        (* Use precomputed ticks from layout *)
        List.iteri
          (fun i v ->
            let py =
              y_to_px ~minv:layout.y_view.min ~maxv:layout.y_view.max
                ~extent:r.height ~origin:r.y ~clamp:true v
            in
            (* tick *)
            for k = 1 to layout.y_axis.tick_length do
              let x = ax - k in
              if x >= 0 then draw_text grid ~x ~y:py ~style:tick_st ch.tick_h
            done;
            (* label *)
            let label = layout.y_axis.format i v in
            let lw = text_width label in
            let label_x =
              ax - layout.y_axis.tick_length - layout.y_axis.label_padding - lw
            in
            if label_x >= 0 then
              draw_text grid ~x:label_x ~y:py ~style:label_st label)
          layout.y_ticks
    | Log { base; view; _ } ->
        (* Use precomputed ticks from layout *)
        List.iteri
          (fun i v ->
            let py =
              y_to_px_log ~base ~minv:view.min ~maxv:view.max ~extent:r.height
                ~origin:r.y ~clamp:true v
            in
            (* tick *)
            for k = 1 to layout.y_axis.tick_length do
              let x = ax - k in
              if x >= 0 then draw_text grid ~x ~y:py ~style:tick_st ch.tick_h
            done;
            (* label *)
            let label = layout.y_axis.format i v in
            let lw = text_width label in
            let label_x =
              ax - layout.y_axis.tick_length - layout.y_axis.label_padding - lw
            in
            if label_x >= 0 then
              draw_text grid ~x:label_x ~y:py ~style:label_st label)
          layout.y_ticks
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
              if x >= 0 then draw_text grid ~x ~y:py ~style:tick_st ch.tick_h
            done;
            let lw = text_width cat in
            let label_x =
              ax - layout.y_axis.tick_length - layout.y_axis.label_padding - lw
            in
            if label_x >= 0 then
              draw_text grid ~x:label_x ~y:py ~style:label_st cat)
          categories);

    (* y axis meets plot padding: draw a corner marker if x axis is on *)
    if layout.x_axis.show then
      let y_corner = r.y + r.height + ip in
      if y_corner >= 0 && y_corner < layout.height then
        draw_text grid ~x:ax ~y:y_corner ~style:st ch.frame.bl);

  (* x axis *)
  if layout.x_axis.show then (
    let ay = r.y + r.height + ip in
    let st = Option.get layout.x_axis.style in
    let tick_st = Option.get layout.x_axis.tick_style in
    let label_st = Option.get layout.x_axis.label_style in
    draw_x_axis_line ~ay ~st;

    (* Use precomputed ticks for X axis with label collision avoidance *)
    match layout.x_scale with
    | Numeric _ ->
        (* Use precomputed ticks from layout *)
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
                draw_text grid ~x:px ~y ~style:tick_st ch.tick_v
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
              draw_text grid ~x:(max 0 label_x) ~y:label_y ~style:label_st label;
              last_label_right := label_x + lw))
          layout.x_ticks
    | Log { base; view; _ } ->
        (* Use precomputed ticks from layout *)
        (* Track the rightmost edge of the last rendered label *)
        let last_label_right = ref min_int in
        List.iteri
          (fun i v ->
            let px =
              x_to_px_log ~base ~minv:view.min ~maxv:view.max ~extent:r.width
                ~origin:r.x ~clamp:true v
            in
            (* tick - always draw *)
            for k = 1 to layout.x_axis.tick_length do
              let y = ay + k in
              if y >= 0 && y < layout.height then
                draw_text grid ~x:px ~y ~style:tick_st ch.tick_v
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
              draw_text grid ~x:(max 0 label_x) ~y:label_y ~style:label_st label;
              last_label_right := label_x + lw))
          layout.x_ticks
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
                draw_text grid ~x:px ~y ~style:tick_st ch.tick_v
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
              draw_text grid ~x:(max 0 label_x) ~y:label_y ~style:label_st cat;
              last_label_right := label_x + lw))
          categories);

  (* Y2 axis - secondary Y-axis on the right side *)
  (match (layout.y2_axis, layout.y2_scale, layout.y2_view) with
  | Some y2_ax, Some y2_sc, Some y2_view when y2_ax.Axis.show ->
      (* Y2 axis line is drawn at the right edge of the plot + inner padding *)
      let ax2 = r.x + r.width + ip in
      let st = Option.get y2_ax.style in
      let tick_st = Option.get y2_ax.tick_style in
      let label_st = Option.get y2_ax.label_style in
      (* Respect y2 axis line setting *)
      (match y2_ax.line with
      | `None -> ()
      | `Axis_only ->
          G.draw_line grid ~x1:ax2 ~y1:r.y ~x2:ax2
            ~y2:(r.y + r.height - 1)
            ~style:st ~glyphs:line_glyphs ~kind:`Line ()
      | `Frame ->
          (* Draw right side of frame *)
          G.draw_line grid ~x1:ax2 ~y1:r.y ~x2:ax2
            ~y2:(r.y + r.height - 1)
            ~style:st ~glyphs:line_glyphs ~kind:`Line ();
          (* Draw top edge if y axis is hidden *)
          if not layout.y_axis.show then
            G.draw_line grid ~x1:r.x ~y1:r.y ~x2:ax2 ~y2:r.y ~style:st
              ~glyphs:line_glyphs ~kind:`Line ();
          (* Draw bottom edge if x axis is hidden *)
          if not layout.x_axis.show then
            G.draw_line grid ~x1:r.x
              ~y1:(r.y + r.height - 1)
              ~x2:ax2
              ~y2:(r.y + r.height - 1)
              ~style:st ~glyphs:line_glyphs ~kind:`Line ());

      (* ticks + labels on the right side - use precomputed ticks *)
      let y2_tick_values = Option.value layout.y2_ticks ~default:[] in
      (match y2_sc with
      | Numeric _ ->
          (* Use precomputed ticks from layout *)
          List.iteri
            (fun i v ->
              let py =
                y_to_px ~minv:y2_view.min ~maxv:y2_view.max ~extent:r.height
                  ~origin:r.y ~clamp:true v
              in
              (* tick - extends to the right *)
              for k = 1 to y2_ax.tick_length do
                let x = ax2 + k in
                if x < layout.width then
                  draw_text grid ~x ~y:py ~style:tick_st ch.tick_h
              done;
              (* label - to the right of ticks *)
              let label = y2_ax.format i v in
              let label_x = ax2 + y2_ax.tick_length + y2_ax.label_padding in
              if label_x < layout.width then
                draw_text grid ~x:label_x ~y:py ~style:label_st label)
            y2_tick_values
      | Log { base; view; _ } ->
          (* Use precomputed ticks from layout *)
          List.iteri
            (fun i v ->
              let py =
                y_to_px_log ~base ~minv:view.min ~maxv:view.max ~extent:r.height
                  ~origin:r.y ~clamp:true v
              in
              (* tick - extends to the right *)
              for k = 1 to y2_ax.tick_length do
                let x = ax2 + k in
                if x < layout.width then
                  draw_text grid ~x ~y:py ~style:tick_st ch.tick_h
              done;
              (* label - to the right of ticks *)
              let label = y2_ax.format i v in
              let label_x = ax2 + y2_ax.tick_length + y2_ax.label_padding in
              if label_x < layout.width then
                draw_text grid ~x:label_x ~y:py ~style:label_st label)
            y2_tick_values
      | Band { categories; padding; _ } ->
          let offset, bw =
            Layout.band_params ~cats:categories ~padding ~extent:r.height
          in
          List.iteri
            (fun i cat ->
              let band_h = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
              let y0 = r.y + int_of_float (offset +. (float i *. bw)) in
              let py = y0 + (band_h / 2) in
              for k = 1 to y2_ax.tick_length do
                let x = ax2 + k in
                if x < layout.width then
                  draw_text grid ~x ~y:py ~style:tick_st ch.tick_h
              done;
              let label_x = ax2 + y2_ax.tick_length + y2_ax.label_padding in
              if label_x < layout.width then
                draw_text grid ~x:label_x ~y:py ~style:label_st cat)
            categories);

      (* Y2 axis meets x-axis: draw a corner marker *)
      if layout.x_axis.show then
        let y_corner = r.y + r.height + ip in
        if y_corner >= 0 && y_corner < layout.height then
          draw_text grid ~x:ax2 ~y:y_corner ~style:st ch.frame.br
  | _ -> ());

  (* Draw axis titles *)
  (* Y-axis title: draw vertically (one char per row) to the left of labels *)
  (match layout.y_axis.title with
  | None -> ()
  | Some { text; style } ->
      let default_st = Option.get layout.y_axis.label_style in
      let title_style = Option.value ~default:default_st style in
      (* Use Unicode-aware iteration for proper grapheme cluster handling *)
      let graphemes = ref [] in
      Glyph.iter_graphemes
        (fun ~offset ~len ->
          graphemes := String.sub text offset len :: !graphemes)
        text;
      let chars = List.rev !graphemes in
      let title_len = List.length chars in
      (* Center title vertically along the y-axis *)
      let title_y_start = r.y + ((r.height - title_len) / 2) in
      (* Position in the reserved title column (at margin_left position) *)
      let title_x = layout.margin_left in
      (* Draw each grapheme cluster vertically *)
      List.iteri
        (fun i char_str ->
          let y = title_y_start + i in
          if y >= r.y && y < r.y + r.height then
            draw_text grid ~x:title_x ~y ~style:title_style char_str)
        chars);

  (* X-axis title: draw horizontally centered below x-axis labels *)
  match layout.x_axis.title with
  | None -> ()
  | Some { text; style } ->
      let default_st = Option.get layout.x_axis.label_style in
      let title_style = Option.value ~default:default_st style in
      let title_w = text_width text in
      let title_x = r.x + ((r.width - title_w) / 2) in
      (* Position below the tick labels: axis + tick_length + label_padding + 1
         (label row) + 1 *)
      let title_y =
        r.y + r.height + ip + layout.x_axis.tick_length
        + layout.x_axis.label_padding + 2
      in
      if title_y < layout.height then
        draw_text grid ~x:(max 0 title_x) ~y:title_y ~style:title_style text

let heatmap_color_fun ~color_scale ~vmin ~vmax =
  let len = Array.length color_scale in
  if len = 0 then fun _ -> Color.default
  else fun v ->
    let t =
      if Float.equal vmin vmax then 0.
      else clamp01 ((v -. vmin) /. Float.max 1e-12 (vmax -. vmin))
    in
    let raw = int_of_float (t *. float len) in
    let idx = clamp_int 0 (len - 1) raw in
    color_scale.(idx)

let heatmap_color_idx ~len ~vmin ~vmax v =
  let t =
    if Float.equal vmin vmax then 0.
    else clamp01 ((v -. vmin) /. Float.max 1e-12 (vmax -. vmin))
  in
  let raw = int_of_float (t *. float len) in
  clamp_int 0 (len - 1) raw

let draw_marks (layout : Layout.t) (grid : G.t) =
  let r = layout.plot in

  (* numeric mapping funcs (even if axis band, used for overlays etc.) We
     provide both clamped (for points/scatter) and unclamped (for line clipping)
     versions. *)
  let x_to_px_cell =
    match layout.x_scale with
    | Numeric { view; _ } ->
        fun x ->
          x_to_px ~minv:view.min ~maxv:view.max ~extent:r.width ~origin:r.x
            ~clamp:true x
    | Log { base; view; _ } ->
        fun x ->
          x_to_px_log ~base ~minv:view.min ~maxv:view.max ~extent:r.width
            ~origin:r.x ~clamp:true x
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
    | Log { base; view; _ } ->
        fun x ->
          x_to_px_log ~base ~minv:view.min ~maxv:view.max ~extent:r.width
            ~origin:r.x ~clamp:false x
    | Band _ ->
        fun x ->
          x_to_px ~minv:layout.x_view.min ~maxv:layout.x_view.max
            ~extent:r.width ~origin:r.x ~clamp:false x
  in
  let y1_to_px_cell =
    match layout.y_scale with
    | Numeric { view; _ } ->
        fun y ->
          y_to_px ~minv:view.min ~maxv:view.max ~extent:r.height ~origin:r.y
            ~clamp:true y
    | Log { base; view; _ } ->
        fun y ->
          y_to_px_log ~base ~minv:view.min ~maxv:view.max ~extent:r.height
            ~origin:r.y ~clamp:true y
    | Band _ ->
        fun y ->
          y_to_px ~minv:layout.y_view.min ~maxv:layout.y_view.max
            ~extent:r.height ~origin:r.y ~clamp:true y
  in
  (* Unclamped version for geometric clipping of lines *)
  let y1_to_px_unclamped =
    match layout.y_scale with
    | Numeric { view; _ } ->
        fun y ->
          y_to_px ~minv:view.min ~maxv:view.max ~extent:r.height ~origin:r.y
            ~clamp:false y
    | Log { base; view; _ } ->
        fun y ->
          y_to_px_log ~base ~minv:view.min ~maxv:view.max ~extent:r.height
            ~origin:r.y ~clamp:false y
    | Band _ ->
        fun y ->
          y_to_px ~minv:layout.y_view.min ~maxv:layout.y_view.max
            ~extent:r.height ~origin:r.y ~clamp:false y
  in
  (* Y2 axis versions - fallback to Y1 if Y2 not configured *)
  let y2_to_px_cell =
    match (layout.y2_scale, layout.y2_view) with
    | Some (Numeric { view; _ }), _ ->
        fun y ->
          y_to_px ~minv:view.min ~maxv:view.max ~extent:r.height ~origin:r.y
            ~clamp:true y
    | Some (Log { base; view; _ }), _ ->
        fun y ->
          y_to_px_log ~base ~minv:view.min ~maxv:view.max ~extent:r.height
            ~origin:r.y ~clamp:true y
    | Some (Band _), Some y2_view ->
        fun y ->
          y_to_px ~minv:y2_view.min ~maxv:y2_view.max ~extent:r.height
            ~origin:r.y ~clamp:true y
    | _ -> y1_to_px_cell (* fallback *)
  in
  let y2_to_px_unclamped =
    match (layout.y2_scale, layout.y2_view) with
    | Some (Numeric { view; _ }), _ ->
        fun y ->
          y_to_px ~minv:view.min ~maxv:view.max ~extent:r.height ~origin:r.y
            ~clamp:false y
    | Some (Log { base; view; _ }), _ ->
        fun y ->
          y_to_px_log ~base ~minv:view.min ~maxv:view.max ~extent:r.height
            ~origin:r.y ~clamp:false y
    | Some (Band _), Some y2_view ->
        fun y ->
          y_to_px ~minv:y2_view.min ~maxv:y2_view.max ~extent:r.height
            ~origin:r.y ~clamp:false y
    | _ -> y1_to_px_unclamped (* fallback *)
  in
  (* Select y mapping based on y_axis selector *)
  let y_to_px_cell = y1_to_px_cell in
  let select_y_to_px_cell = function
    | `Y1 -> y1_to_px_cell
    | `Y2 -> y2_to_px_cell
  in
  let select_y_to_px_unclamped = function
    | `Y1 -> y1_to_px_unclamped
    | `Y2 -> y2_to_px_unclamped
  in
  let select_y_view = function
    | `Y1 -> layout.y_view
    | `Y2 -> Option.value layout.y2_view ~default:layout.y_view
  in

  (* helpers for band axis bar placement *)
  let x_band =
    match layout.x_scale with
    | Band { categories; index_of; padding; _ } ->
        let offset, bw =
          Layout.band_params ~cats:categories ~padding ~extent:r.width
        in
        (* Bar width: use band width minus 1 for visual gap, minimum 1 *)
        let band_w = max 1 (int_of_float bw - 1) in
        Some (categories, index_of, offset, bw, band_w)
    | _ -> None
  in
  let y_band =
    match layout.y_scale with
    | Band { categories; index_of; padding; _ } ->
        let offset, bw =
          Layout.band_params ~cats:categories ~padding ~extent:r.height
        in
        (* Bar height: use band width minus 1 for visual gap, minimum 1 *)
        let band_h = max 1 (int_of_float bw - 1) in
        Some (categories, index_of, offset, bw, band_h)
    | _ -> None
  in

  let draw_shade_x style x0 x1 =
    let px0 = x_to_px_cell x0 and px1 = x_to_px_cell x1 in
    let a = min px0 px1 and b = max px0 px1 in
    for x = a to b do
      if x >= r.x && x < r.x + r.width then
        for y = r.y to r.y + r.height - 1 do
          draw_text grid ~x ~y ~style " "
        done
    done
  in

  let draw_column_bg style x =
    let px = x_to_px_cell x in
    if px >= r.x && px < r.x + r.width then
      for y = r.y to r.y + r.height - 1 do
        draw_text grid ~x:px ~y ~style " "
      done
  in

  (* Convert charset to Grid.line_glyphs for theme-aware line rendering *)
  let line_glyphs : G.line_glyphs =
    let charset = layout.theme.charset in
    {
      G.h = charset.frame.h;
      v = charset.frame.v;
      diag_up = charset.diag_up;
      diag_down = charset.diag_down;
    }
  in

  (* Array-based dot buffer for block2x2 rendering. Uses a flat array indexed by
     (cx - r.x) + (cy - r.y) * r.width. Much faster than Hashtbl for
     accumulating dots in tight loops. *)
  let block2x2_dots = Array.make (r.width * r.height) 0 in

  let block2x2_set_dot px py =
    let cell_x = px / 2 in
    let cell_y = py / 2 in
    if
      cell_x >= r.x
      && cell_x < r.x + r.width
      && cell_y >= r.y
      && cell_y < r.y + r.height
    then
      let sub_x = px mod 2 in
      let sub_y = py mod 2 in
      (* Bit layout: bit 0=top-left, bit 1=top-right, bit 2=bottom-left, bit
         3=bottom-right *)
      let bit = (sub_y * 2) + sub_x in
      let idx = cell_x - r.x + ((cell_y - r.y) * r.width) in
      Array.unsafe_set block2x2_dots idx
        (Array.unsafe_get block2x2_dots idx lor (1 lsl bit))
  in

  let block2x2_clear () = Array.fill block2x2_dots 0 (r.width * r.height) 0 in

  let render_block2x2 style =
    for cy = r.y to r.y + r.height - 1 do
      for cx = r.x to r.x + r.width - 1 do
        let idx = cx - r.x + ((cy - r.y) * r.width) in
        let bits = Array.unsafe_get block2x2_dots idx in
        if bits <> 0 then
          let glyph = quadrant_glyph_of_bits bits in
          draw_text grid ~x:cx ~y:cy ~style glyph
      done
    done
  in

  (* Array-based dot buffer for braille rendering (2x4 subgrid per cell). Uses a
     flat array indexed by (cx - r.x) + (cy - r.y) * r.width. *)
  let braille_dots = Array.make (r.width * r.height) 0 in

  let braille_set_dot x_sub y_sub =
    let cell_x = x_sub / 2 in
    let cell_y = y_sub / 4 in
    if
      cell_x >= r.x
      && cell_x < r.x + r.width
      && cell_y >= r.y
      && cell_y < r.y + r.height
    then
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
      let idx = cell_x - r.x + ((cell_y - r.y) * r.width) in
      Array.unsafe_set braille_dots idx
        (Array.unsafe_get braille_dots idx lor (1 lsl bit_pos))
  in

  let braille_clear () = Array.fill braille_dots 0 (r.width * r.height) 0 in

  let render_braille style =
    for cy = r.y to r.y + r.height - 1 do
      for cx = r.x to r.x + r.width - 1 do
        let idx = cx - r.x + ((cy - r.y) * r.width) in
        let bits = Array.unsafe_get braille_dots idx in
        if bits <> 0 then
          let glyph = braille_glyph_of_bits bits in
          draw_text grid ~x:cx ~y:cy ~style glyph
      done
    done
  in

  (* Block2x2 rendering: accumulate bits using array, then render glyphs *)
  let draw_block2x2_line (x1, y1) (x2, y2) =
    (* Bresenham's line algorithm for 2x2 subgrid coordinates *)
    let dx = abs (x2 - x1) and dy = abs (y2 - y1) in
    let sx = if x1 < x2 then 1 else -1 in
    let sy = if y1 < y2 then 1 else -1 in
    let err = ref (dx - dy) in
    let x = ref x1 and y = ref y1 in
    while not (!x = x2 && !y = y2) do
      block2x2_set_dot !x !y;
      let e2 = 2 * !err in
      if e2 > -dy then (
        err := !err - dy;
        x := !x + sx);
      if e2 < dx then (
        err := !err + dx;
        y := !y + sy)
    done;
    block2x2_set_dot x2 y2
  in

  (* Stipple mask for line patterns: returns true if step should be drawn *)
  let stipple_should_draw ~pattern ~step =
    match pattern with
    | `Solid -> true
    | `Dashed ->
        (* Draw 3, skip 2 pattern *)
        let cycle = step mod 5 in
        cycle < 3
    | `Dotted ->
        (* Draw 1, skip 1 pattern *)
        step mod 2 = 0
  in

  (* Draw a stippled line segment using Bresenham's algorithm *)
  let draw_stippled_line ~style ~pattern ~step_counter (x1, y1) (x2, y2) =
    let dx = abs (x2 - x1) and dy = abs (y2 - y1) in
    let sx = if x1 < x2 then 1 else -1 in
    let sy = if y1 < y2 then 1 else -1 in
    let err = ref (dx - dy) in
    let px = ref x1 and py = ref y1 in
    let glyph = if dx > dy then "─" else if dy > dx then "│" else "·" in
    while not (!px = x2 && !py = y2) do
      if stipple_should_draw ~pattern ~step:!step_counter then
        if Layout.rect_contains r ~x:!px ~y:!py then
          draw_text grid ~x:!px ~y:!py ~style glyph;
      incr step_counter;
      let e2 = 2 * !err in
      if e2 > -dy then (
        err := !err - dy;
        px := !px + sx);
      if e2 < dx then (
        err := !err + dx;
        py := !py + sy)
    done;
    if stipple_should_draw ~pattern ~step:!step_counter then
      if Layout.rect_contains r ~x:x2 ~y:y2 then
        draw_text grid ~x:x2 ~y:y2 ~style glyph;
    incr step_counter
  in

  (* Unified line drawing function that handles optional y values (gaps) *)
  let draw_line_series_impl ~y_axis ~style ~pattern ~kind ~x ~y data =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let y_to_px_unclamped' = select_y_to_px_unclamped y_axis in
    let y_view = select_y_view y_axis in
    match kind with
    | `Points glyph ->
        Data.iter data (fun a ->
            match y a with
            | None -> ()
            | Some yy ->
                let px = x_to_px_cell (x a) in
                let py = y_to_px_cell' yy in
                if Layout.rect_contains r ~x:px ~y:py then
                  draw_text grid ~x:px ~y:py ~style glyph)
    | `Wave ->
        (* Wave: render with box-drawing characters for smooth curves. Collect
           points, interpolate per-column, then render with ╭╮╯╰│─ *)
        let seq_y = Array.make r.width (-1) in
        let set_col x yval =
          let ix = x - r.x in
          if ix >= 0 && ix < r.width then seq_y.(ix) <- yval
        in
        let draw_seg (x1, y1) (x2, y2) =
          if x1 = x2 then set_col x1 y2
          else
            let x_start = min x1 x2 and x_end = max x1 x2 in
            for xc = x_start to x_end do
              let t = float (xc - x1) /. float (x2 - x1) in
              let yc =
                int_of_float (Float.round (float y1 +. (t *. float (y2 - y1))))
              in
              set_col xc yc
            done
        in
        let prev = ref None in
        Data.iter data (fun a ->
            match y a with
            | None -> prev := None
            | Some yy ->
                let px = x_to_px_cell (x a) in
                let py = y_to_px_cell' yy in
                (match !prev with
                | None -> ()
                | Some (px0, py0) -> draw_seg (px0, py0) (px, py));
                prev := Some (px, py));
        (* Render the interpolated wave *)
        let prev_y = ref None in
        for i = 0 to r.width - 1 do
          let yval = seq_y.(i) in
          if yval < 0 then prev_y := None
          else if yval >= r.y && yval < r.y + r.height then
            match !prev_y with
            | None ->
                draw_text grid ~x:(r.x + i) ~y:yval ~style "─";
                prev_y := Some yval
            | Some py ->
                let xc = r.x + i in
                if py = yval then draw_text grid ~x:xc ~y:yval ~style "─"
                else if py > yval then (
                  draw_text grid ~x:xc ~y:yval ~style "╭";
                  if py >= r.y && py < r.y + r.height then
                    draw_text grid ~x:xc ~y:py ~style "╯";
                  for yy = yval + 1 to py - 1 do
                    if yy >= r.y && yy < r.y + r.height then
                      draw_text grid ~x:xc ~y:yy ~style "│"
                  done)
                else (
                  draw_text grid ~x:xc ~y:yval ~style "╰";
                  if py >= r.y && py < r.y + r.height then
                    draw_text grid ~x:xc ~y:py ~style "╮";
                  for yy = py + 1 to yval - 1 do
                    if yy >= r.y && yy < r.y + r.height then
                      draw_text grid ~x:xc ~y:yy ~style "│"
                  done);
                prev_y := Some yval
        done
    | `Line ->
        (* Use unclamped coordinates and geometric clipping *)
        let prev = ref None in
        let xmin = r.x and xmax = r.x + r.width - 1 in
        let ymin = r.y and ymax = r.y + r.height - 1 in
        let step_counter = ref 0 in
        Data.iter data (fun a ->
            match y a with
            | None ->
                step_counter := 0;
                prev := None
            | Some yy ->
                let px = x_to_px_unclamped (x a) in
                let py = y_to_px_unclamped' yy in
                (match !prev with
                | None -> ()
                | Some (px0, py0) -> (
                    match
                      Clip.line_to_rect ~xmin ~xmax ~ymin ~ymax ~x1:px0 ~y1:py0
                        ~x2:px ~y2:py
                    with
                    | Some (x1, y1, x2, y2) -> (
                        match pattern with
                        | `Solid ->
                            G.draw_line grid ~x1 ~y1 ~x2 ~y2 ~style
                              ~glyphs:line_glyphs ~kind:`Line ()
                        | `Dashed | `Dotted ->
                            draw_stippled_line ~style ~pattern ~step_counter
                              (x1, y1) (x2, y2))
                    | None -> ()));
                prev := Some (px, py))
    | `Block2x2 ->
        (* Project into 2x2 subgrid per cell *)
        let gx = r.width * 2 in
        let gy = r.height * 2 in
        let xmin = layout.x_view.min and xmax = layout.x_view.max in
        let ymin = y_view.min and ymax = y_view.max in
        let dx = xmax -. xmin and dy = ymax -. ymin in
        (* Clip bounds in block2x2 coordinates *)
        let clip_xmin = r.x * 2 in
        let clip_xmax = ((r.x + r.width) * 2) - 1 in
        let clip_ymin = r.y * 2 in
        let clip_ymax = ((r.y + r.height) * 2) - 1 in
        block2x2_clear ();
        let prev = ref None in
        Data.iter data (fun a ->
            match y a with
            | None ->
                (* Render accumulated dots before gap, then reset *)
                render_block2x2 style;
                block2x2_clear ();
                prev := None
            | Some yy ->
                let x' = x a in
                let sx =
                  if dx <= 0. then 0. else (x' -. xmin) *. float (gx - 1) /. dx
                in
                let sy =
                  if dy <= 0. then 0. else (yy -. ymin) *. float (gy - 1) /. dy
                in
                let px = (r.x * 2) + int_of_float (Float.round sx) in
                let py = (r.y * 2) + (gy - 1 - int_of_float (Float.round sy)) in
                (match !prev with
                | None -> ()
                | Some (px0, py0) -> (
                    (* Clip line segment to plot rectangle in block2x2
                       coordinates *)
                    match
                      Clip.line_to_rect ~xmin:clip_xmin ~xmax:clip_xmax
                        ~ymin:clip_ymin ~ymax:clip_ymax ~x1:px0 ~y1:py0 ~x2:px
                        ~y2:py
                    with
                    | Some (x1, y1, x2, y2) ->
                        draw_block2x2_line (x1, y1) (x2, y2)
                    | None -> ()));
                prev := Some (px, py));
        render_block2x2 style
    | `Braille ->
        (* Project into 2x4 braille subgrid inside plot *)
        let gx = r.width * 2 in
        let gy = r.height * 4 in
        let xmin = layout.x_view.min and xmax = layout.x_view.max in
        let ymin = y_view.min and ymax = y_view.max in
        let dx = xmax -. xmin and dy = ymax -. ymin in
        (* Clip bounds in braille coordinates *)
        let clip_xmin = r.x * 2 in
        let clip_xmax = ((r.x + r.width) * 2) - 1 in
        let clip_ymin = r.y * 4 in
        let clip_ymax = ((r.y + r.height) * 4) - 1 in
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
                | Some (px0, py0) -> (
                    match
                      Clip.line_to_rect ~xmin:clip_xmin ~xmax:clip_xmax
                        ~ymin:clip_ymin ~ymax:clip_ymax ~x1:px0 ~y1:py0 ~x2:px
                        ~y2:py
                    with
                    | Some (x1, y1, x2, y2) ->
                        G.draw_line grid ~x1 ~y1 ~x2 ~y2 ~style ~kind:`Braille
                          ()
                    | None -> ()));
                prev := Some (px, py))
  in

  (* Wrapper for Line: wraps non-optional y with Some *)
  let draw_line_series ~y_axis ~style ~pattern ~kind ~x ~y data =
    draw_line_series_impl ~y_axis ~style ~pattern ~kind ~x
      ~y:(fun a -> Some (y a))
      data
  in

  (* Wrapper for Line_opt: y already returns option *)
  let draw_line_opt_series ~y_axis ~style ~pattern ~kind ~x ~y data =
    draw_line_series_impl ~y_axis ~style ~pattern ~kind ~x ~y data
  in

  let draw_scatter_series ~y_axis ~style ~glyph ~kind ~x ~y data =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let y_view = select_y_view y_axis in
    match kind with
    | `Cell ->
        Data.iter data (fun a ->
            let px = x_to_px_cell (x a) in
            let py = y_to_px_cell' (y a) in
            if Layout.rect_contains r ~x:px ~y:py then
              draw_text grid ~x:px ~y:py ~style glyph)
    | `Density ->
        (* Density mode: count points per cell and render with shade_levels. Use
           flat array for O(1) access instead of Hashtbl. *)
        let w = r.width and h = r.height in
        if w > 0 && h > 0 then (
          let counts = Array.make (w * h) 0 in
          let max_count = ref 0 in
          Data.iter data (fun a ->
              let px = x_to_px_cell (x a) in
              let py = y_to_px_cell' (y a) in
              if Layout.rect_contains r ~x:px ~y:py then (
                let idx = ((py - r.y) * w) + (px - r.x) in
                let new_count = counts.(idx) + 1 in
                counts.(idx) <- new_count;
                max_count := max !max_count new_count));
          let shade_chars = layout.theme.charset.shade_levels in
          let num_levels = Array.length shade_chars in
          let max_c = max 1 !max_count in
          for py = r.y to r.y + h - 1 do
            for px = r.x to r.x + w - 1 do
              let idx = ((py - r.y) * w) + (px - r.x) in
              let count = counts.(idx) in
              if count > 0 then
                let t = float count /. float max_c in
                let level_idx =
                  max 0
                    (min (num_levels - 1)
                       (int_of_float (t *. float (num_levels - 1))))
                in
                draw_text grid ~x:px ~y:py ~style shade_chars.(level_idx)
            done
          done)
    | `Braille ->
        let gx = r.width * 2 in
        let gy = r.height * 4 in
        let xmin = layout.x_view.min and xmax = layout.x_view.max in
        let ymin = y_view.min and ymax = y_view.max in
        let dx = xmax -. xmin and dy = ymax -. ymin in
        braille_clear ();
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
            braille_set_dot x_sub y_sub);
        render_braille style
  in

  (* Area fill rendering: fills the region from baseline to y values *)
  let draw_area ~y_axis ~style ~baseline ~resolution ~x ~y data =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let y_view = select_y_view y_axis in
    let n = Array.length data in
    if n = 0 then ()
    else
      let baseline_y = match baseline with `Zero -> 0. | `Value v -> v in
      match resolution with
      | `Cell | `Wave | `Block2x2 ->
          (* Cell-based area: for each x column, fill from baseline to y *)
          for i = 0 to n - 1 do
            let datum = data.(i) in
            let xv = x datum in
            let yv = y datum in
            let px = x_to_px_cell xv in
            if px >= r.x && px < r.x + r.width then
              let py_data = y_to_px_cell' yv in
              let py_base = y_to_px_cell' baseline_y in
              let py_top = min py_data py_base in
              let py_bot = max py_data py_base in
              for py = py_top to py_bot do
                if py >= r.y && py < r.y + r.height then
                  draw_text grid ~x:px ~y:py ~style "█"
              done
          done
      | `Braille2x4 ->
          (* Braille-based area fill with sub-cell precision *)
          let gx = r.width * 2 in
          let gy = r.height * 4 in
          let xmin = layout.x_view.min and xmax = layout.x_view.max in
          let ymin = y_view.min and ymax = y_view.max in
          let dx = xmax -. xmin and dy = ymax -. ymin in
          braille_clear ();
          let base_sub =
            let sy =
              if dy <= 0. then 0.
              else (baseline_y -. ymin) *. float (gy - 1) /. dy
            in
            (r.y * 4) + (gy - 1 - int_of_float (Float.round sy))
          in
          for i = 0 to n - 1 do
            let datum = data.(i) in
            let xv = x datum in
            let yv = y datum in
            let sx =
              if dx <= 0. then 0. else (xv -. xmin) *. float (gx - 1) /. dx
            in
            let sy =
              if dy <= 0. then 0. else (yv -. ymin) *. float (gy - 1) /. dy
            in
            let x_sub = (r.x * 2) + int_of_float (Float.round sx) in
            let y_sub = (r.y * 4) + (gy - 1 - int_of_float (Float.round sy)) in
            let y_top = min y_sub base_sub in
            let y_bot = max y_sub base_sub in
            (* Fill column from top to bottom *)
            for ys = y_top to y_bot do
              braille_set_dot x_sub ys
            done
          done;
          render_braille style
  in

  (* Fill between two y values *)
  let draw_fill_between ~y_axis ~style ~resolution ~x ~y_low ~y_high data =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let y_view = select_y_view y_axis in
    let n = Array.length data in
    if n = 0 then ()
    else
      match resolution with
      | `Cell | `Wave | `Block2x2 ->
          (* Cell-based fill: for each x column, fill from y_low to y_high *)
          for i = 0 to n - 1 do
            let datum = data.(i) in
            let xv = x datum in
            let yv_low = y_low datum in
            let yv_high = y_high datum in
            let px = x_to_px_cell xv in
            if px >= r.x && px < r.x + r.width then
              let py_low = y_to_px_cell' yv_low in
              let py_high = y_to_px_cell' yv_high in
              let py_top = min py_low py_high in
              let py_bot = max py_low py_high in
              for py = py_top to py_bot do
                if py >= r.y && py < r.y + r.height then
                  draw_text grid ~x:px ~y:py ~style "▒"
              done
          done
      | `Braille2x4 ->
          (* Braille-based fill between with sub-cell precision *)
          let gx = r.width * 2 in
          let gy = r.height * 4 in
          let xmin = layout.x_view.min and xmax = layout.x_view.max in
          let ymin = y_view.min and ymax = y_view.max in
          let dx = xmax -. xmin and dy = ymax -. ymin in
          braille_clear ();
          for i = 0 to n - 1 do
            let datum = data.(i) in
            let xv = x datum in
            let yv_low = y_low datum in
            let yv_high = y_high datum in
            let sx =
              if dx <= 0. then 0. else (xv -. xmin) *. float (gx - 1) /. dx
            in
            let sy_low =
              if dy <= 0. then 0. else (yv_low -. ymin) *. float (gy - 1) /. dy
            in
            let sy_high =
              if dy <= 0. then 0. else (yv_high -. ymin) *. float (gy - 1) /. dy
            in
            let x_sub = (r.x * 2) + int_of_float (Float.round sx) in
            let y_sub_low =
              (r.y * 4) + (gy - 1 - int_of_float (Float.round sy_low))
            in
            let y_sub_high =
              (r.y * 4) + (gy - 1 - int_of_float (Float.round sy_high))
            in
            let y_top = min y_sub_low y_sub_high in
            let y_bot = max y_sub_low y_sub_high in
            for ys = y_top to y_bot do
              braille_set_dot x_sub ys
            done
          done;
          render_braille style
  in

  let draw_bars_y ~style ~mode ~x ~y data =
    match x_band with
    | None -> ()
    | Some (_cats, index_of, offset, bw, band_w) ->
        let y_view = layout.y_view in
        let y_span = y_view.max -. y_view.min in
        let y_span = if y_span <= 0. then 1. else y_span in
        let scale = float r.height /. y_span in
        let baseline_data = 0. in
        let baseline_clamped =
          Float.max y_view.min (Float.min y_view.max baseline_data)
        in
        Data.iter data (fun a ->
            let cat = x a in
            match Layout.band_index_fast index_of cat with
            | None -> ()
            | Some i -> (
                let x0 = r.x + int_of_float (offset +. (float i *. bw)) in
                let v = y a in
                let y0v = Float.min baseline_clamped v in
                let y1v = Float.max baseline_clamped v in
                let y0_f = (y0v -. y_view.min) *. scale in
                let y1_f = (y1v -. y_view.min) *. scale in
                match mode with
                | `Cell ->
                    (* Simple full-cell rendering *)
                    let lo_cell = int_of_float (Float.round y0_f) in
                    let hi_cell = int_of_float (Float.round y1_f) in
                    for k = lo_cell to hi_cell - 1 do
                      let yy = r.y + r.height - 1 - k in
                      if yy >= r.y && yy < r.y + r.height then
                        for xx = 0 to band_w - 1 do
                          let px = x0 + xx in
                          if px >= r.x && px < r.x + r.width then
                            draw_text grid ~x:px ~y:yy ~style "█"
                        done
                    done
                | `Half_block -> (
                    (* Sub-cell precision using half-block characters *)
                    let lo_cell = int_of_float (Float.floor y0_f) in
                    let hi_cell = int_of_float (Float.floor y1_f) in
                    let lo_frac = y0_f -. float lo_cell in
                    let hi_frac = y1_f -. float hi_cell in
                    let bot_glyph =
                      if lo_frac > 1e-6 then upper_block_glyph (1. -. lo_frac)
                      else None
                    in
                    let top_glyph = lower_block_glyph hi_frac in
                    (match bot_glyph with
                    | None -> ()
                    | Some glyph ->
                        let yy = r.y + r.height - 1 - lo_cell in
                        if yy >= r.y && yy < r.y + r.height then
                          for xx = 0 to band_w - 1 do
                            let px = x0 + xx in
                            if px >= r.x && px < r.x + r.width then
                              draw_text grid ~x:px ~y:yy ~style glyph
                          done);
                    let start_cell =
                      if lo_frac > 1e-6 then lo_cell + 1 else lo_cell
                    in
                    for k = start_cell to hi_cell - 1 do
                      let yy = r.y + r.height - 1 - k in
                      if yy >= r.y && yy < r.y + r.height then
                        for xx = 0 to band_w - 1 do
                          let px = x0 + xx in
                          if px >= r.x && px < r.x + r.width then
                            draw_text grid ~x:px ~y:yy ~style "█"
                        done
                    done;
                    match top_glyph with
                    | None -> ()
                    | Some glyph ->
                        let yy = r.y + r.height - 1 - hi_cell in
                        if yy >= r.y && yy < r.y + r.height then
                          for xx = 0 to band_w - 1 do
                            let px = x0 + xx in
                            if px >= r.x && px < r.x + r.width then
                              draw_text grid ~x:px ~y:yy ~style glyph
                          done)))
  in

  let draw_bars_x ~style ~mode ~y ~x data =
    match y_band with
    | None -> ()
    | Some (_cats, index_of, offset, bw, band_h) ->
        let x_view = layout.x_view in
        let x_span = x_view.max -. x_view.min in
        let x_span = if x_span <= 0. then 1. else x_span in
        let scale = float r.width /. x_span in
        let baseline_data = 0. in
        let baseline_clamped =
          Float.max x_view.min (Float.min x_view.max baseline_data)
        in
        Data.iter data (fun a ->
            let cat = y a in
            match Layout.band_index_fast index_of cat with
            | None -> ()
            | Some i -> (
                let y0 = r.y + int_of_float (offset +. (float i *. bw)) in
                let v = x a in
                let x0v = Float.min baseline_clamped v in
                let x1v = Float.max baseline_clamped v in
                let x0_f = (x0v -. x_view.min) *. scale in
                let x1_f = (x1v -. x_view.min) *. scale in
                match mode with
                | `Cell ->
                    (* Simple full-cell rendering *)
                    let lo_cell = int_of_float (Float.round x0_f) in
                    let hi_cell = int_of_float (Float.round x1_f) in
                    for k = lo_cell to hi_cell - 1 do
                      let xx = r.x + k in
                      if xx >= r.x && xx < r.x + r.width then
                        for yy = 0 to band_h - 1 do
                          let py = y0 + yy in
                          if py >= r.y && py < r.y + r.height then
                            draw_text grid ~x:xx ~y:py ~style "█"
                        done
                    done
                | `Half_block -> (
                    (* Sub-cell precision using half-block characters *)
                    let lo_cell = int_of_float (Float.floor x0_f) in
                    let hi_cell = int_of_float (Float.floor x1_f) in
                    let lo_frac = x0_f -. float lo_cell in
                    let hi_frac = x1_f -. float hi_cell in
                    let left_glyph =
                      if lo_frac > 1e-6 then right_block_glyph (1. -. lo_frac)
                      else None
                    in
                    let right_glyph = left_block_glyph hi_frac in
                    (match left_glyph with
                    | None -> ()
                    | Some glyph ->
                        let xx = r.x + lo_cell in
                        if xx >= r.x && xx < r.x + r.width then
                          for yy = 0 to band_h - 1 do
                            let py = y0 + yy in
                            if py >= r.y && py < r.y + r.height then
                              draw_text grid ~x:xx ~y:py ~style glyph
                          done);
                    let start_cell =
                      if lo_frac > 1e-6 then lo_cell + 1 else lo_cell
                    in
                    for k = start_cell to hi_cell - 1 do
                      let xx = r.x + k in
                      if xx >= r.x && xx < r.x + r.width then
                        for yy = 0 to band_h - 1 do
                          let py = y0 + yy in
                          if py >= r.y && py < r.y + r.height then
                            draw_text grid ~x:xx ~y:py ~style "█"
                        done
                    done;
                    match right_glyph with
                    | None -> ()
                    | Some glyph ->
                        let xx = r.x + hi_cell in
                        if xx >= r.x && xx < r.x + r.width then
                          for yy = 0 to band_h - 1 do
                            let py = y0 + yy in
                            if py >= r.y && py < r.y + r.height then
                              draw_text grid ~x:xx ~y:py ~style glyph
                          done)))
  in

  let draw_stacked_y ~gap ~bar_width ~mode data =
    match x_band with
    | None -> ()
    | Some (_cats, index_of, offset, bw, band_w_auto) ->
        let bw_cells =
          match bar_width with
          | Some w -> w
          | None ->
              let gap = max 0 gap in
              max 1 (int_of_float bw - gap - 1)
        in
        let bw_cells = min bw_cells band_w_auto in
        let y_view = layout.y_view in
        let y_span = y_view.max -. y_view.min in
        let y_span = if y_span <= 0. then 1. else y_span in
        let scale = float r.height /. y_span in
        Data.iter data (fun b ->
            match Layout.band_index_fast index_of b.Mark.category with
            | None -> ()
            | Some i ->
                let x0 = r.x + int_of_float (offset +. (float i *. bw)) in
                let cum = ref 0. in
                List.iter
                  (fun seg ->
                    let v = max 0. seg.Mark.value in
                    let y0v = !cum in
                    cum := !cum +. v;
                    let y1v = !cum in
                    let y0_f = (y0v -. y_view.min) *. scale in
                    let y1_f = (y1v -. y_view.min) *. scale in
                    match mode with
                    | `Cell ->
                        let y0_cells = int_of_float (Float.round y0_f) in
                        let y1_cells = int_of_float (Float.round y1_f) in
                        for k = y0_cells to y1_cells - 1 do
                          let yy = r.y + r.height - 1 - k in
                          if yy >= r.y && yy < r.y + r.height then
                            for xx = 0 to bw_cells - 1 do
                              let px = x0 + xx in
                              if px >= r.x && px < r.x + r.width then
                                draw_text grid ~x:px ~y:yy ~style:seg.style "█"
                            done
                        done
                    | `Half_block -> (
                        let y0_cells = int_of_float (Float.floor y0_f) in
                        let y1_cells = int_of_float (Float.floor y1_f) in
                        let y1_frac = y1_f -. float y1_cells in
                        let top_glyph = lower_block_glyph y1_frac in
                        for k = y0_cells to y1_cells - 1 do
                          let yy = r.y + r.height - 1 - k in
                          if yy >= r.y && yy < r.y + r.height then
                            for xx = 0 to bw_cells - 1 do
                              let px = x0 + xx in
                              if px >= r.x && px < r.x + r.width then
                                draw_text grid ~x:px ~y:yy ~style:seg.style "█"
                            done
                        done;
                        match top_glyph with
                        | None -> ()
                        | Some glyph ->
                            let yy = r.y + r.height - 1 - y1_cells in
                            if yy >= r.y && yy < r.y + r.height then
                              for xx = 0 to bw_cells - 1 do
                                let px = x0 + xx in
                                if px >= r.x && px < r.x + r.width then
                                  draw_text grid ~x:px ~y:yy ~style:seg.style
                                    glyph
                              done))
                  b.segments)
  in

  let draw_stacked_x ~gap ~bar_height ~mode data =
    match y_band with
    | None -> ()
    | Some (_cats, index_of, offset, bw, band_h_auto) ->
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
            match Layout.band_index_fast index_of b.Mark.category with
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
                    (* Calculate bar widths in floating point for sub-cell
                       precision *)
                    let x0_f = (x0v -. x_view.min) *. scale in
                    let x1_f = (x1v -. x_view.min) *. scale in
                    match mode with
                    | `Cell ->
                        let x0_cells = int_of_float (Float.round x0_f) in
                        let x1_cells = int_of_float (Float.round x1_f) in
                        for k = x0_cells to x1_cells - 1 do
                          let xx = r.x + k in
                          if xx >= r.x && xx < r.x + r.width then
                            for yy = 0 to bh_cells - 1 do
                              let py = y0 + yy in
                              if py >= r.y && py < r.y + r.height then
                                draw_text grid ~x:xx ~y:py ~style:seg.style "█"
                            done
                        done
                    | `Half_block -> (
                        let x0_cells = int_of_float (Float.floor x0_f) in
                        let x1_cells = int_of_float (Float.floor x1_f) in
                        let x1_frac = x1_f -. float x1_cells in
                        let right_glyph = left_block_glyph x1_frac in
                        (* Draw full block cells from left of segment to
                           right *)
                        for k = x0_cells to x1_cells - 1 do
                          let xx = r.x + k in
                          if xx >= r.x && xx < r.x + r.width then
                            for yy = 0 to bh_cells - 1 do
                              let py = y0 + yy in
                              if py >= r.y && py < r.y + r.height then
                                draw_text grid ~x:xx ~y:py ~style:seg.style "█"
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
                                  draw_text grid ~x:xx ~y:py ~style:seg.style
                                    glyph
                              done))
                  b.segments)
  in

  let charset = layout.theme.charset in

  let draw_rule_x ~style ~pattern x =
    let px = x_to_px_cell x in
    if px >= r.x && px < r.x + r.width then
      let glyph =
        match pattern with
        | `Solid -> charset.grid_v_solid
        | `Dashed -> charset.grid_v_dashed
        | `Dotted -> charset.grid_v_dotted
      in
      for y = r.y to r.y + r.height - 1 do
        draw_text grid ~x:px ~y ~style glyph
      done
  in

  let draw_rule_y ~y_axis ~style ~pattern y =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let py = y_to_px_cell' y in
    if py >= r.y && py < r.y + r.height then
      let glyph =
        match pattern with
        | `Solid -> charset.grid_h_solid
        | `Dashed -> charset.grid_h_dashed
        | `Dotted -> charset.grid_h_dotted
      in
      for x = r.x to r.x + r.width - 1 do
        draw_text grid ~x ~y:py ~style glyph
      done
  in

  let draw_candles ~y_axis ~bullish ~bearish ~width ~body data =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    (* Data is pre-sorted by time during compilation *)
    (* Determine candle rendering characters based on width and body options *)
    let body_char, body_filled_char =
      match width with `One -> ("┃", "┃") | `Two -> ("▐▌", "██")
    in
    let wick_char = match width with `One -> "│" | `Two -> " │" in
    Data.iter data (fun (o : Mark.ohlc) ->
        let cx = x_to_px_cell o.time in
        let st = if o.close >= o.open_ then bullish else bearish in
        let is_bullish = o.close >= o.open_ in
        let y_body_top = y_to_px_cell' (max o.open_ o.close) in
        let y_body_bot = y_to_px_cell' (min o.open_ o.close) in
        let y_wick_top = y_to_px_cell' o.high in
        let y_wick_bot = y_to_px_cell' o.low in
        (* Select body rendering based on body style *)
        let body_str =
          match body with
          | `Filled -> body_filled_char
          | `Hollow -> if is_bullish then body_char else body_filled_char
        in
        if cx >= r.x && cx < r.x + r.width then (
          (* Upper wick *)
          for yy = min y_wick_top y_body_top to max y_wick_top y_body_top do
            if yy >= r.y && yy < r.y + r.height then
              draw_text grid ~x:cx ~y:yy ~style:st wick_char
          done;
          (* Body *)
          for yy = min y_body_top y_body_bot to max y_body_top y_body_bot do
            if yy >= r.y && yy < r.y + r.height then
              draw_text grid ~x:cx ~y:yy ~style:st body_str
          done;
          (* Lower wick *)
          for yy = min y_body_bot y_wick_bot to max y_body_bot y_wick_bot do
            if yy >= r.y && yy < r.y + r.height then
              draw_text grid ~x:cx ~y:yy ~style:st wick_char
          done))
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

  let draw_circle ~y_axis ~style ~kind ~cx ~cy ~radius_fn data =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let y_view = select_y_view y_axis in
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
                let px = x_to_px_cell (float xd) in
                let py = y_to_px_cell' (float yd) in
                if Layout.rect_contains r ~x:px ~y:py then
                  draw_text grid ~x:px ~y:py ~style "█")
              pts)
    | `Braille ->
        (* accumulate braille dots inside plot *)
        let gx = r.width * 2 in
        let gy = r.height * 4 in
        let xmin = layout.x_view.min and xmax = layout.x_view.max in
        let ymin = y_view.min and ymax = y_view.max in
        let dx = xmax -. xmin and dy = ymax -. ymin in
        braille_clear ();
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
                braille_set_dot x_sub y_sub)
              pts);
        render_braille style
  in

  let rec draw_heatmap ~color_scale ~value_range ~auto_value_range ~agg ~render
      ~x ~y ~value data =
    (* Compute min/max directly without building intermediate list *)
    let vmin, vmax =
      match value_range with
      | Some (a, b) -> (a, b)
      | None ->
          if not auto_value_range then (0., 1.)
          else if Array.length data = 0 then (0., 1.)
          else
            let mn = ref (value data.(0)) in
            let mx = ref !mn in
            for i = 1 to Array.length data - 1 do
              let v = value data.(i) in
              if v < !mn then mn := v;
              if v > !mx then mx := v
            done;
            (!mn, !mx)
    in
    let vmin, vmax = safe_range vmin vmax in
    let color_of = heatmap_color_fun ~color_scale ~vmin ~vmax in
    let ncolors = Array.length color_scale in
    (* Pre-build style caches to avoid allocation in inner loops *)
    let bg_styles = Array.map (fun c -> Style.make ~bg:c ()) color_scale in
    let fg_styles = Array.map (fun c -> Style.make ~fg:c ()) color_scale in
    let color_idx v = heatmap_color_idx ~len:ncolors ~vmin ~vmax v in

    let draw_cell px py v =
      if Layout.rect_contains r ~x:px ~y:py then
        draw_text grid ~x:px ~y:py ~style:bg_styles.(color_idx v) " "
    in

    match render with
    | Mark.Cells_bg ->
        (* Cells_bg: colored background with space glyph *)
        (* Use flat arrays instead of Hashtbl for O(1) access *)
        let size = r.width * r.height in
        let idx px py = px - r.x + ((py - r.y) * r.width) in
        let values_arr = Array.make size 0. in
        let counts_arr = Array.make size 0 in
        Data.iter data (fun a ->
            let px = x_to_px_cell (x a) in
            let py = y_to_px_cell (y a) in
            let v = value a in
            if Layout.rect_contains r ~x:px ~y:py then
              let i = idx px py in
              match agg with
              | `Last ->
                  values_arr.(i) <- v;
                  counts_arr.(i) <- 1
              | `Max ->
                  if counts_arr.(i) = 0 then (
                    values_arr.(i) <- v;
                    counts_arr.(i) <- 1)
                  else values_arr.(i) <- Float.max values_arr.(i) v
              | `Avg ->
                  values_arr.(i) <- values_arr.(i) +. v;
                  counts_arr.(i) <- counts_arr.(i) + 1);
        for py = r.y to r.y + r.height - 1 do
          for px = r.x to r.x + r.width - 1 do
            let i = idx px py in
            let n = counts_arr.(i) in
            if n > 0 then
              let v =
                match agg with
                | `Avg -> values_arr.(i) /. float n
                | _ -> values_arr.(i)
              in
              draw_cell px py v
          done
        done
    | Mark.Cells_fg ->
        (* Cells_fg: colored foreground glyph (█) *)
        let draw_cell_fg px py v =
          if Layout.rect_contains r ~x:px ~y:py then
            draw_text grid ~x:px ~y:py
              ~style:fg_styles.(color_idx v)
              charset.bar_fill
        in
        (* Use flat arrays instead of Hashtbl for O(1) access *)
        let size = r.width * r.height in
        let idx px py = px - r.x + ((py - r.y) * r.width) in
        let values_arr = Array.make size 0. in
        let counts_arr = Array.make size 0 in
        Data.iter data (fun a ->
            let px = x_to_px_cell (x a) in
            let py = y_to_px_cell (y a) in
            let v = value a in
            if Layout.rect_contains r ~x:px ~y:py then
              let i = idx px py in
              match agg with
              | `Last ->
                  values_arr.(i) <- v;
                  counts_arr.(i) <- 1
              | `Max ->
                  if counts_arr.(i) = 0 then (
                    values_arr.(i) <- v;
                    counts_arr.(i) <- 1)
                  else values_arr.(i) <- Float.max values_arr.(i) v
              | `Avg ->
                  values_arr.(i) <- values_arr.(i) +. v;
                  counts_arr.(i) <- counts_arr.(i) + 1);
        for py = r.y to r.y + r.height - 1 do
          for px = r.x to r.x + r.width - 1 do
            let i = idx px py in
            let n = counts_arr.(i) in
            if n > 0 then
              let v =
                match agg with
                | `Avg -> values_arr.(i) /. float n
                | _ -> values_arr.(i)
              in
              draw_cell_fg px py v
          done
        done
    | Mark.Halfblock_fg_bg ->
        (* Halfblock_fg_bg: Two values per row using ▀ with fg/bg colors.
           Each terminal cell represents two data rows (top half and bottom half),
           doubling vertical resolution. *)
        (* Use flat arrays for 2x vertical resolution *)
        let half_height = r.height * 2 in
        let size = r.width * half_height in
        let idx px py_half = px - r.x + (py_half * r.width) in
        let values_arr = Array.make size 0. in
        let counts_arr = Array.make size 0 in
        (* Map data to half-cell coordinates *)
        Data.iter data (fun a ->
            let px = x_to_px_cell (x a) in
            (* Map to double-resolution y coordinate *)
            let y_data = y a in
            let py_float =
              let minv = layout.y_view.min and maxv = layout.y_view.max in
              let extent = half_height in
              if Float.abs (maxv -. minv) < 1e-12 then float (extent / 2)
              else
                let t = (y_data -. minv) /. (maxv -. minv) in
                (* y inverted: top=min, bottom=max *)
                float (extent - 1) *. (1. -. t)
            in
            let py_half = int_of_float (Float.round py_float) in
            let py_half = max 0 (min (half_height - 1) py_half) in
            let v = value a in
            if px >= r.x && px < r.x + r.width then
              let i = idx px py_half in
              match agg with
              | `Last ->
                  values_arr.(i) <- v;
                  counts_arr.(i) <- 1
              | `Max ->
                  if counts_arr.(i) = 0 then (
                    values_arr.(i) <- v;
                    counts_arr.(i) <- 1)
                  else values_arr.(i) <- Float.max values_arr.(i) v
              | `Avg ->
                  values_arr.(i) <- values_arr.(i) +. v;
                  counts_arr.(i) <- counts_arr.(i) + 1);
        let get_value px py_half =
          let i = idx px py_half in
          let n = counts_arr.(i) in
          if n = 0 then None
          else
            Some
              (match agg with
              | `Avg -> values_arr.(i) /. float n
              | _ -> values_arr.(i))
        in
        (* Render cell-by-cell: each cell covers py_half = [py*2, py*2+1] *)
        for py = r.y to r.y + r.height - 1 do
          for px = r.x to r.x + r.width - 1 do
            let py_rel = py - r.y in
            let py_half_top = py_rel * 2 in
            let py_half_bot = (py_rel * 2) + 1 in
            let v_top = get_value px py_half_top in
            let v_bot = get_value px py_half_bot in
            match (v_top, v_bot) with
            | None, None -> ()
            | Some vt, None ->
                (* Only top half has data *)
                draw_text grid ~x:px ~y:py ~style:fg_styles.(color_idx vt) "▀"
            | None, Some vb ->
                (* Only bottom half has data *)
                draw_text grid ~x:px ~y:py ~style:fg_styles.(color_idx vb) "▄"
            | Some vt, Some vb ->
                (* Both halves have data: top=fg, bottom=bg, glyph=▀ *)
                let st = Style.make ~fg:(color_of vt) ~bg:(color_of vb) () in
                draw_text grid ~x:px ~y:py ~style:st "▀"
          done
        done
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
            ~render:Mark.Cells_bg ~x ~y ~value data
        else
          let xs = Array.of_list xs and ys = Array.of_list ys in
          (* Use flat arrays instead of Hashtbl for O(1) access *)
          let grid_size = nx * ny in
          let idx ix iy = ix + (iy * nx) in
          let values_arr = Array.make grid_size 0. in
          let counts_arr = Array.make grid_size 0 in
          (* Binary search to find the interval [arr.(i), arr.(i+1)] containing
             v *)
          let find_interval arr v =
            let len = Array.length arr in
            if len = 1 then (0, 0, arr.(0), arr.(0))
            else if len = 2 then (0, 1, arr.(0), arr.(1))
            else
              (* Binary search for largest i where arr.(i) <= v *)
              let rec bsearch lo hi =
                if lo >= hi then lo
                else
                  let mid = lo + ((hi - lo + 1) / 2) in
                  if Float.compare arr.(mid) v <= 0 then bsearch mid hi
                  else bsearch lo (mid - 1)
              in
              let i = bsearch 0 (len - 2) in
              (i, i + 1, arr.(i), arr.(i + 1))
          in
          (* Binary search to find floor index: largest i where arr.(i) <= v *)
          let find_index_floor arr v =
            let len = Array.length arr in
            if len = 1 then 0
            else
              let rec bsearch lo hi =
                if lo >= hi then lo
                else
                  let mid = lo + ((hi - lo + 1) / 2) in
                  if Float.compare arr.(mid) v <= 0 then bsearch mid hi
                  else bsearch lo (mid - 1)
              in
              bsearch 0 (len - 1)
          in
          (* populate grid arrays *)
          Data.iter data (fun a ->
              let xi = find_index_floor xs (x a) in
              let yi = find_index_floor ys (y a) in
              let i = idx xi yi in
              let v = value a in
              match agg with
              | `Last ->
                  values_arr.(i) <- v;
                  counts_arr.(i) <- 1
              | `Max ->
                  if counts_arr.(i) = 0 then (
                    values_arr.(i) <- v;
                    counts_arr.(i) <- 1)
                  else values_arr.(i) <- Float.max values_arr.(i) v
              | `Avg ->
                  values_arr.(i) <- values_arr.(i) +. v;
                  counts_arr.(i) <- counts_arr.(i) + 1);
          let lookup ix iy =
            let i = idx ix iy in
            match agg with
            | `Avg ->
                let n = counts_arr.(i) in
                if n <= 0 then 0. else values_arr.(i) /. float n
            | `Last | `Max -> values_arr.(i)
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
        (* Shaded mode: bilinear upsample with text characters instead of
           colors *)
        let shade_chars = charset.shade_levels in
        let num_levels = Array.length shade_chars in
        let shade_idx v =
          let t = (v -. vmin) /. Float.max 1e-12 (vmax -. vmin) in
          let t = clamp01 t in
          max 0
            (min (num_levels - 1) (int_of_float (t *. float (num_levels - 1))))
        in
        let draw_shaded_cell px py v =
          if Layout.rect_contains r ~x:px ~y:py then
            draw_text grid ~x:px ~y:py
              ~style:fg_styles.(color_idx v)
              shade_chars.(shade_idx v)
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
          (* sparse fallback - just plot individual cells using flat array *)
          let size = r.width * r.height in
          let idx px py = px - r.x + ((py - r.y) * r.width) in
          let values_arr = Array.make size 0. in
          let has_value = Array.make size false in
          Data.iter data (fun a ->
              let px = x_to_px_cell (x a) in
              let py = y_to_px_cell (y a) in
              let v = value a in
              if Layout.rect_contains r ~x:px ~y:py then (
                let i = idx px py in
                values_arr.(i) <- v;
                has_value.(i) <- true));
          for py = r.y to r.y + r.height - 1 do
            for px = r.x to r.x + r.width - 1 do
              let i = idx px py in
              if has_value.(i) then draw_shaded_cell px py values_arr.(i)
            done
          done)
        else
          let xs = Array.of_list xs and ys = Array.of_list ys in
          (* Use flat arrays instead of Hashtbl for O(1) access *)
          let grid_size = nx * ny in
          let idx ix iy = ix + (iy * nx) in
          let values_arr = Array.make grid_size 0. in
          let counts_arr = Array.make grid_size 0 in
          (* Binary search to find the interval [arr.(i), arr.(i+1)] containing
             v *)
          let find_interval arr v =
            let len = Array.length arr in
            if len = 1 then (0, 0, arr.(0), arr.(0))
            else if len = 2 then (0, 1, arr.(0), arr.(1))
            else
              (* Binary search for largest i where arr.(i) <= v *)
              let rec bsearch lo hi =
                if lo >= hi then lo
                else
                  let mid = lo + ((hi - lo + 1) / 2) in
                  if Float.compare arr.(mid) v <= 0 then bsearch mid hi
                  else bsearch lo (mid - 1)
              in
              let i = bsearch 0 (len - 2) in
              (i, i + 1, arr.(i), arr.(i + 1))
          in
          (* Binary search to find floor index: largest i where arr.(i) <= v *)
          let find_index_floor arr v =
            let len = Array.length arr in
            if len = 1 then 0
            else
              let rec bsearch lo hi =
                if lo >= hi then lo
                else
                  let mid = lo + ((hi - lo + 1) / 2) in
                  if Float.compare arr.(mid) v <= 0 then bsearch mid hi
                  else bsearch lo (mid - 1)
              in
              bsearch 0 (len - 1)
          in
          (* populate grid arrays *)
          Data.iter data (fun a ->
              let xi = find_index_floor xs (x a) in
              let yi = find_index_floor ys (y a) in
              let i = idx xi yi in
              let v = value a in
              match agg with
              | `Last ->
                  values_arr.(i) <- v;
                  counts_arr.(i) <- 1
              | `Max ->
                  if counts_arr.(i) = 0 then (
                    values_arr.(i) <- v;
                    counts_arr.(i) <- 1)
                  else values_arr.(i) <- Float.max values_arr.(i) v
              | `Avg ->
                  values_arr.(i) <- values_arr.(i) +. v;
                  counts_arr.(i) <- counts_arr.(i) + 1);
          let lookup ix iy =
            let i = idx ix iy in
            match agg with
            | `Avg ->
                let n = counts_arr.(i) in
                if n <= 0 then 0. else values_arr.(i) /. float n
            | `Last | `Max -> values_arr.(i)
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
      | CLine { style; resolution; pattern; glyph; x; y; y_axis; data; _ } ->
          let kind =
            match resolution with
            | `Cell -> ( match glyph with Some g -> `Points g | None -> `Line)
            | `Wave -> `Wave
            | `Block2x2 -> `Block2x2
            | `Braille2x4 -> `Braille
          in
          draw_line_series ~y_axis ~style ~pattern ~kind ~x ~y data
      | CLine_opt { style; resolution; pattern; glyph; x; y; y_axis; data; _ }
        ->
          let kind =
            match resolution with
            | `Cell -> ( match glyph with Some g -> `Points g | None -> `Line)
            | `Wave -> `Wave
            | `Block2x2 -> `Block2x2
            | `Braille2x4 -> `Braille
          in
          draw_line_opt_series ~y_axis ~style ~pattern ~kind ~x ~y data
      | CScatter { style; glyph; mode; x; y; y_axis; data; _ } ->
          draw_scatter_series ~y_axis ~style ~glyph ~kind:mode ~x ~y data
      | CBars_y { style; mode; x; y; data; _ } ->
          draw_bars_y ~style ~mode ~x ~y data
      | CBars_x { style; mode; y; x; data; _ } ->
          draw_bars_x ~style ~mode ~y ~x data
      | CStacked_y { gap; bar_width; mode; data; _ } ->
          draw_stacked_y ~gap ~bar_width ~mode data
      | CStacked_x { gap; bar_height; mode; data; _ } ->
          draw_stacked_x ~gap ~bar_height ~mode data
      | CRule_x { style; pattern; x; _ } -> draw_rule_x ~style ~pattern x
      | CRule_y { style; pattern; y; y_axis; _ } ->
          draw_rule_y ~y_axis ~style ~pattern y
      | CHeatmap
          {
            color_scale;
            value_range;
            auto_value_range;
            agg;
            mode;
            x;
            y;
            value;
            data;
            _;
          } ->
          draw_heatmap ~color_scale ~value_range ~auto_value_range ~agg
            ~render:mode ~x ~y ~value data
      | CCandles { bullish; bearish; width; body; y_axis; data; _ } ->
          draw_candles ~y_axis ~bullish ~bearish ~width ~body data
      | CCircle { style; resolution; cx; cy; r; y_axis; data; _ } ->
          let kind =
            match resolution with
            | `Cell | `Wave | `Block2x2 -> `Line
            | `Braille2x4 -> `Braille
          in
          draw_circle ~y_axis ~style ~kind ~cx ~cy ~radius_fn:r data
      | CArea { style; baseline; resolution; x; y; y_axis; data; _ } ->
          draw_area ~y_axis ~style ~baseline ~resolution ~x ~y data
      | CFill_between { style; resolution; x; y_low; y_high; y_axis; data; _ }
        ->
          draw_fill_between ~y_axis ~style ~resolution ~x ~y_low ~y_high data
      | CHistogram { style; bin_edges; bin_values; _ } ->
          (* Render histogram as bars - each bar spans [edge_i, edge_{i+1}) *)
          let num_bins = Array.length bin_values in
          if num_bins > 0 && Array.length bin_edges > num_bins then
            for i = 0 to num_bins - 1 do
              let x0 = bin_edges.(i) in
              let x1 = bin_edges.(i + 1) in
              let y_val = bin_values.(i) in
              (* Convert to pixel coordinates *)
              let px0 = x_to_px_cell x0 in
              let px1 = x_to_px_cell x1 in
              let py_base = y_to_px_cell 0. in
              let py_top = y_to_px_cell y_val in
              let py_min = min py_base py_top in
              let py_max = max py_base py_top in
              (* Draw bar from px0 to px1-1 *)
              for px = max r.x px0 to min (r.x + r.width - 1) (px1 - 1) do
                for py = py_min to py_max do
                  if Layout.rect_contains r ~x:px ~y:py then
                    draw_text grid ~x:px ~y:py ~style "█"
                done
              done
            done)
    layout.marks

let draw ?view ?(x = 0) ?(y = 0) (t : t) (grid : G.t) ~width ~height : Layout.t
    =
  let layout = compute_layout ?view ~x ~y t ~width ~height in
  (* Always clear the entire chart area to prevent stale axis labels/ticks from
     persisting between frames. Use background color if set, otherwise
     Color.Default clears cells to blank (space with alpha=0). *)
  let bg = Option.value t.theme.background ~default:Color.Default in
  G.fill_rect grid ~x ~y ~width ~height ~color:bg;

  draw_grid layout grid;
  draw_marks layout grid;
  draw_axes layout grid;

  (* Draw chart title centered above plot *)
  (match layout.title with
  | None -> ()
  | Some { text; style } ->
      let text_w = text_width text in
      let center_x = layout.plot.x + (layout.plot.width / 2) - (text_w / 2) in
      let title_y = max y (layout.plot.y - 1) in
      G.draw_text grid ~x:center_x ~y:title_y ~text ~style);

  layout
