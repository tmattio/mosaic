module Style = Ansi.Style
module Color = Ansi.Color

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

type kind = [ `Bars | `Braille ]

type t = {
  auto_max : bool;
  style : Style.t;
  mutable max_value : float;
  buf : float array;
  mutable head : int;
  mutable len : int;
}

let capacity t = Array.length t.buf
let clamp_nonneg v = if v < 0. then 0. else v
let set_max t m = t.max_value <- (if m > 0. then m else 1.)

let create ?(style = Style.default) ?(auto_max = true) ?max_value ~capacity () =
  let cap = max 1 capacity in
  let maxv = match max_value with Some m when m > 0. -> m | _ -> 1. in
  {
    auto_max;
    style;
    max_value = maxv;
    buf = Array.make cap 0.;
    head = 0;
    len = 0;
  }

let clear t =
  t.head <- 0;
  t.len <- 0

let push t v =
  let v = clamp_nonneg v in
  if t.auto_max && v > t.max_value then set_max t v;
  let cap = capacity t in
  Array.unsafe_set t.buf t.head v;
  t.head <- (t.head + 1) mod cap;
  if t.len < cap then t.len <- t.len + 1

let push_all t vs = List.iter (push t) vs

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

let fill_background canvas ~x ~y ~width ~height style =
  match style.Style.bg with
  | None -> ()
  | Some bg -> Grid.fill_rect canvas ~x ~y ~width ~height ~color:bg

let draw_bars t canvas ~x:ox ~y:oy ~width ~height ~columns_only =
  if not columns_only then
    fill_background canvas ~x:ox ~y:oy ~width ~height t.style;
  let keep = iter_visible ~max_points:width t (fun _ _ -> ()) in
  if keep = 0 then ()
  else
    let start_x = width - keep in
    let bottom = height - 1 in
    let scale = if t.max_value <= 0. then 1. else float height /. t.max_value in
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
    ignore
      (iter_visible ~max_points:width t (fun i v ->
           let x = ox + start_x + i in
           let sv = v *. scale in
           if sv > 0. then (
             let n = Float.floor sv in
             let full = int_of_float n in
             let frac = sv -. n in
             for j = 0 to full - 1 do
               let y = oy + bottom - j in
               if y >= oy && y < oy + height then
                 Grid.draw_text ~style:t.style canvas ~x ~y ~text:"█"
             done;
             let top = lower_block_char frac in
             if top <> "" then
               let y = oy + bottom - full in
               if y >= oy && y < oy + height then
                 Grid.draw_text ~style:t.style canvas ~x ~y ~text:top)))

let draw_braille t canvas ~x:ox ~y:oy ~width ~height ~columns_only =
  if not columns_only then
    fill_background canvas ~x:ox ~y:oy ~width ~height t.style;
  let keep = iter_visible ~max_points:width t (fun _ _ -> ()) in
  if keep = 0 then ()
  else
    let start_x = width - keep in
    let scale = if t.max_value <= 0. then 1. else float height /. t.max_value in
    let grid_w = width * 2 in
    let grid_h = height * 4 in
    let grid_wm1 = grid_w - 1 in
    let grid_hm1 = grid_h - 1 in
    let dots : (int * int, int) Hashtbl.t = Hashtbl.create 64 in
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
          let cur = Option.value (Hashtbl.find_opt dots key) ~default:0 in
          Hashtbl.replace dots key (cur lor (1 lsl bit_pos))
    in
    let to_px i v =
      let x_logical = float (start_x + i) in
      let x_px =
        int_of_float
          (Float.round (x_logical *. float grid_wm1 /. float (max 1 width)))
      in
      let y_val = v *. scale in
      let y_px =
        grid_hm1
        - int_of_float
            (Float.round (y_val *. float grid_hm1 /. float (max 1 height)))
      in
      (x_px, y_px)
    in
    let abs_int x = if x < 0 then -x else x in
    let draw_segment (x1, y1) (x2, y2) =
      let low (x1, y1) (x2, y2) =
        let dx = x2 - x1 in
        let dy = y2 - y1 in
        let yi = ref 1 in
        let dy' = ref dy in
        if !dy' < 0 then (
          yi := -1;
          dy' := - !dy');
        let d = ref ((2 * !dy') - dx) in
        let y = ref y1 in
        let start = ref x1 and stop = ref x2 in
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
      let high (x1, y1) (x2, y2) =
        let dx = x2 - x1 in
        let dy = y2 - y1 in
        let xi = ref 1 in
        let dx' = ref dx in
        if !dx' < 0 then (
          xi := -1;
          dx' := - !dx');
        let d = ref ((2 * !dx') - dy) in
        let x = ref x1 in
        let start = ref y1 and stop = ref y2 in
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
        if x1 > x2 then low (x2, y2) (x1, y1) else low (x1, y1) (x2, y2)
      else if y1 > y2 then high (x2, y2) (x1, y1)
      else high (x1, y1) (x2, y2)
    in
    let prev = ref None in
    ignore
      (iter_visible ~max_points:width t (fun i v ->
           let pt = to_px i v in
           match !prev with
           | None ->
               let x, y = pt in
               set_dot x y;
               prev := Some pt
           | Some p ->
               draw_segment p pt;
               prev := Some pt));
    Hashtbl.iter
      (fun (cx, cy) bits ->
        Grid.draw_text ~style:t.style canvas ~x:(ox + cx) ~y:(oy + cy)
          ~text:(braille_glyph_of_bits bits))
      dots

let draw t ~kind ?(columns_only = false) ?(x = 0) ?(y = 0) canvas ~width ~height
    =
  let width = max 1 width and height = max 1 height in
  match kind with
  | `Bars -> draw_bars t canvas ~x ~y ~width ~height ~columns_only
  | `Braille -> draw_braille t canvas ~x ~y ~width ~height ~columns_only

let draw_values ?(style = Style.default) ~kind ?(x = 0) ?(y = 0) values canvas
    ~width ~height =
  let t = create ~style ~capacity:(max 1 width) () in
  push_all t values;
  draw t ~kind ~x ~y canvas ~width ~height
