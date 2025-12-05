module Style = Ansi.Style
module Canvas = Mosaic_ui.Canvas

(* Rendering modes supported by the streaming sparkline. *)
type kind = [ `Bars | `Braille ]

type t = {
  auto_max : bool;
  style : Style.t;
  mutable width : int;
  mutable height : int;
  mutable max_value : float;
  mutable buf : float array;
  mutable head : int; (* next insertion index *)
  mutable len : int; (* number of valid elements, <= capacity *)
}

let clamp_nonneg f = if f < 0. then 0. else f
let capacity t = Array.length t.buf
let set_max t m = t.max_value <- (if m > 0. then m else 1.)

let push t v =
  let v = clamp_nonneg v in
  if t.auto_max && v > t.max_value then set_max t v;
  let cap = capacity t in
  Array.unsafe_set t.buf t.head v;
  t.head <- (t.head + 1) mod cap;
  if t.len < cap then t.len <- t.len + 1

let make ~width ~height ?(style = Style.default) ?(auto_max = true) ?max_value
    ?data () =
  let w = max 1 width and h = max 1 height in
  let maxv = match max_value with Some m when m > 0. -> m | _ -> 1. in
  let t =
    {
      auto_max;
      style;
      width = w;
      height = h;
      max_value = maxv;
      buf = Array.make w 0.;
      head = 0;
      len = 0;
    }
  in
  (match data with None -> () | Some vs -> List.iter (fun v -> push t v) vs);
  t

let clear t =
  t.head <- 0;
  t.len <- 0

let push_all t vs = List.iter (push t) vs

let resize t ~width ~height =
  let new_w = max 1 width and new_h = max 1 height in
  let cap_old = capacity t in
  (* Only grow the underlying buffer; never shrink, so short-lived
     layout passes with a smaller width do not drop historical samples. *)
  if new_w > cap_old then (
    let keep = min t.len new_w in
    let new_buf = Array.make new_w 0. in
    (* copy last [keep] elements in chronological order into new_buf *)
    let start_old = (t.head - t.len + cap_old) mod cap_old in
    let start_idx = t.len - keep in
    for i = 0 to keep - 1 do
      let src = (start_old + start_idx + i) mod cap_old in
      Array.unsafe_set new_buf i (Array.unsafe_get t.buf src)
    done;
    t.buf <- new_buf;
    t.head <- keep mod new_w;
    t.len <- keep);
  (* Track the latest logical draw size. *)
  t.width <- new_w;
  t.height <- new_h

(* Iterate over visible values in chronological order, passing (i, v),
   where [i] is 0 for the oldest sample and increases. *)
let iter_visible ?max_points t f =
  let cap = capacity t in
  let keep =
    match max_points with
    | None -> min t.len cap
    | Some m -> min t.len (min cap m)
  in
  if keep = 0 then 0
  else
    let start = (t.head - keep + cap) mod cap in
    for i = 0 to keep - 1 do
      let idx = (start + i) mod cap in
      f i (Array.unsafe_get t.buf idx)
    done;
    keep

let fill_background canvas ~width ~height style =
  match style.Style.bg with
  | None -> ()
  | Some bg -> Canvas.fill_rect canvas ~x:0 ~y:0 ~width ~height ~color:bg

let draw_bars t canvas ~width ~height ~columns_only =
  (* Match ntcharts' Canvas.Clear() on each draw to avoid stale glyphs
     when the canvas has been resized or reused. *)
  Canvas.clear canvas;
  if not columns_only then fill_background canvas ~width ~height t.style;
  let keep = iter_visible ~max_points:width t (fun _ _ -> ()) in
  if keep = 0 then ()
  else
    let start_x = width - keep in
    let bottom = height - 1 in
    let scale = if t.max_value <= 0. then 1. else float height /. t.max_value in
    (* Match ntcharts' LowerBlockElementFromFloat64 rounding:
       - map [0,1) to the nearest 1/8th lower-block element
       - values < 1/16 round to empty, >= 15/16 round to full block. *)
    let lower_block_char f =
      if f >= 1. then "█"
      else if f <= 0. then ""
      else
        let e = int_of_float (f /. 0.125) in
        let n = f -. (float e *. 0.125) in
        let e = if n >= 0.0625 then e + 1 else e in
        match e with
        | 0 -> ""
        | 1 -> "▁"
        | 2 -> "▂"
        | 3 -> "▃"
        | 4 -> "▄"
        | 5 -> "▅"
        | 6 -> "▆"
        | 7 -> "▇"
        | _ -> "█"
    in
    (* second pass: actually draw the columns *)
    ignore
      (iter_visible ~max_points:width t (fun i v ->
           let x = start_x + i in
           let sv = v *. scale in
           if sv > 0. then (
             let n = Float.floor sv in
             let full = int_of_float n in
             let frac = sv -. n in
             let top_rune = lower_block_char frac in
             (* full blocks from bottom up *)
             for j = 0 to full - 1 do
               let y = bottom - j in
               if y >= 0 && y < height then
                 Canvas.plot canvas ~x ~y ~style:t.style "█"
             done;
             (* top partial block, if any *)
             if top_rune <> "" then
               let y = bottom - full in
               if y >= 0 && y < height then
                 Canvas.plot canvas ~x ~y ~style:t.style top_rune)))

let draw_braille t canvas ~width ~height ~columns_only =
  Canvas.clear canvas;
  if not columns_only then fill_background canvas ~width ~height t.style;
  let keep = iter_visible ~max_points:width t (fun _ _ -> ()) in
  if keep = 0 then ()
  else
    let start_x = width - keep in
    let scale =
      if t.max_value <= 0. then 1. else float_of_int height /. t.max_value
    in
    (* Match ntcharts' BrailleGrid + GetLinePoints pipeline by:
       - projecting logical points into a 2x4 braille grid
       - running the same Bresenham variant as graph.GetLinePoints
       - mapping fine-grained dots into Unicode braille cells. *)
    let grid_w = width * 2 in
    let grid_h = height * 4 in
    let grid_wm1 = grid_w - 1 in
    let grid_hm1 = grid_h - 1 in
    let dx_f = float_of_int width in
    let dy_f = float_of_int height in
    (* Project a sample index and value into braille pixel coordinates. *)
    let to_px i v =
      let x_logical = float_of_int (start_x + i) in
      let x_scaled =
        if dx_f <= 0. then 0. else x_logical *. float_of_int grid_wm1 /. dx_f
      in
      let x_px = int_of_float (Float.round x_scaled) in
      let y_val = v *. scale in
      let y_scaled =
        if dy_f <= 0. then 0. else y_val *. float_of_int grid_hm1 /. dy_f
      in
      let y_px = grid_hm1 - int_of_float (Float.round y_scaled) in
      (x_px, y_px)
    in
    (* Accumulate braille dot bits per cell, mirroring Canvas.draw_line's layout. *)
    let dots = Hashtbl.create 64 in
    let set_dot x y =
      if x >= 0 && y >= 0 && x < grid_w && y < grid_h then
        let cell_x = x / 2 in
        let cell_y = y / 4 in
        if cell_x >= 0 && cell_x < width && cell_y >= 0 && cell_y < height then
          let bit_x = x land 1 in
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
          let current = Option.value (Hashtbl.find_opt dots key) ~default:0 in
          Hashtbl.replace dots key (current lor (1 lsl bit_pos))
    in
    let abs_int x = if x < 0 then -x else x in
    (* Port of ntcharts/graph.GetLinePoints specialised to call [set_dot]. *)
    let draw_segment (x1, y1) (x2, y2) =
      let get_line_points_low (x1, y1) (x2, y2) =
        let dx = x2 - x1 in
        let dy = y2 - y1 in
        let yi = ref 1 in
        let dy' = ref dy in
        if !dy' < 0 then (
          yi := -1;
          dy' := - !dy');
        let d = ref ((2 * !dy') - dx) in
        let y = ref y1 in
        let start = ref x1 in
        let stop = ref x2 in
        if !start > !stop then (
          start := x2;
          stop := x1);
        for x = !start to !stop do
          set_dot x !y;
          if !d > 0 then (
            y := !y + !yi;
            d := !d + (2 * (!dy' - dx)))
          else d := !d + (2 * !dy')
        done
      in
      let get_line_points_high (x1, y1) (x2, y2) =
        let dx = x2 - x1 in
        let dy = y2 - y1 in
        let xi = ref 1 in
        let dx' = ref dx in
        if !dx' < 0 then (
          xi := -1;
          dx' := - !dx');
        let d = ref ((2 * !dx') - dy) in
        let x = ref x1 in
        let start = ref y1 in
        let stop = ref y2 in
        if !start > !stop then (
          start := y2;
          stop := y1);
        for y = !start to !stop do
          set_dot !x y;
          if !d > 0 then (
            x := !x + !xi;
            d := !d + (2 * (!dx' - dy)))
          else d := !d + (2 * !dx')
        done
      in
      if abs_int (y2 - y1) < abs_int (x2 - x1) then
        if x1 > x2 then get_line_points_low (x2, y2) (x1, y1)
        else get_line_points_low (x1, y1) (x2, y2)
      else if y1 > y2 then get_line_points_high (x2, y2) (x1, y1)
      else get_line_points_high (x1, y1) (x2, y2)
    in
    (* Walk samples, drawing segments in the braille grid. *)
    let prev = ref None in
    ignore
      (iter_visible ~max_points:width t (fun i v ->
           let pt = to_px i v in
           match !prev with
           | None ->
               let x, y = pt in
               set_dot x y;
               prev := Some pt
           | Some prev_pt ->
               draw_segment prev_pt pt;
               prev := Some pt));
    (* Flush accumulated braille cells to the canvas. *)
    Hashtbl.iter
      (fun (cell_x, cell_y) bits ->
        let code = 0x2800 + bits in
        let uchar =
          match Uchar.of_int code with
          | exception Invalid_argument _ -> Uchar.of_int 0x2800
          | c -> c
        in
        let buf = Buffer.create 4 in
        Uutf.Buffer.add_utf_8 buf uchar;
        let glyph = Buffer.contents buf in
        Canvas.plot canvas ~x:cell_x ~y:cell_y ~style:t.style glyph)
      dots

let draw t ~kind ?(columns_only = false) canvas ~width ~height =
  (* Grow internal capacity as needed but avoid shrinking below the
     sparkline's initial logical size, so transient small layout passes
     don't drop historical samples. *)
  let width = max width t.width in
  let height = max height t.height in
  resize t ~width ~height;
  match kind with
  | `Bars -> draw_bars t canvas ~width ~height ~columns_only
  | `Braille -> draw_braille t canvas ~width ~height ~columns_only
