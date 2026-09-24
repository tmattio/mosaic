type color_depth = [ `Ansi16 | `Ansi256 | `Truecolor ]

type t = {
  mutable fg_color : int;
  mutable bg_color : int;
  mutable attrs : int;
  mutable link : string; (* "" = no link *)
  mutable link_open : bool;
  mutable color_depth : color_depth;
}

let create () =
  {
    fg_color = -1;
    bg_color = -1;
    attrs = -1;
    link = "";
    link_open = false;
    color_depth = `Truecolor;
  }

let set_color_depth t depth =
  if t.color_depth <> depth then (
    t.color_depth <- depth;
    t.fg_color <- -1;
    t.bg_color <- -1;
    t.attrs <- -1)

let reset t =
  t.fg_color <- -1;
  t.bg_color <- -1;
  t.attrs <- -1;
  t.link <- "";
  t.link_open <- false

let update_link t w link =
  if not (String.equal link t.link) then (
    if t.link_open then Escape.hyperlink_close w;
    if link <> "" then (
      Escape.hyperlink_open w link;
      t.link <- link;
      t.link_open <- true)
    else (
      t.link <- "";
      t.link_open <- false))

let ansi16_rgb =
  [|
    0x000000;
    0x800000;
    0x008000;
    0x808000;
    0x000080;
    0x800080;
    0x008080;
    0xc0c0c0;
    0x808080;
    0xff0000;
    0x00ff00;
    0xffff00;
    0x0000ff;
    0xff00ff;
    0x00ffff;
    0xffffff;
  |]

let cube_level = [| 0; 95; 135; 175; 215; 255 |]
let[@inline] rgb24 r g b = (r lsl 16) lor (g lsl 8) lor b

let[@inline] palette_rgb24 idx =
  if idx < 16 then Array.unsafe_get ansi16_rgb idx
  else if idx < 232 then
    let n = idx - 16 in
    rgb24
      (Array.unsafe_get cube_level (n / 36))
      (Array.unsafe_get cube_level (n / 6 mod 6))
      (Array.unsafe_get cube_level (n mod 6))
  else
    let gray = 8 + ((idx - 232) * 10) in
    rgb24 gray gray gray

let[@inline] color_distance r g b candidate =
  let dr = r - ((candidate lsr 16) land 0xFF) in
  let dg = g - ((candidate lsr 8) land 0xFF) in
  let db = b - (candidate land 0xFF) in
  (dr * dr) + (dg * dg) + (db * db)

let nearest_palette_index limit color =
  let r = Color.Packed.red color in
  let g = Color.Packed.green color in
  let b = Color.Packed.blue color in
  let rec loop i best best_dist =
    if i >= limit then best
    else
      let dist = color_distance r g b (palette_rgb24 i) in
      if dist < best_dist then loop (i + 1) i dist
      else loop (i + 1) best best_dist
  in
  loop 0 0 max_int

let emit_sgr_code w first code =
  if not first then Escape.sgr_sep w;
  Escape.sgr_code w code;
  false

let emit_indexed_sgr w first ~bg idx =
  let first = emit_sgr_code w first (if bg then 48 else 38) in
  let first = emit_sgr_code w first 5 in
  emit_sgr_code w first idx

let emit_ansi16_sgr w first ~bg idx =
  if idx < 8 then emit_sgr_code w first ((if bg then 40 else 30) + idx)
  else emit_sgr_code w first ((if bg then 100 else 90) + idx - 8)

let emit_rgb_sgr w first ~bg color =
  let first = emit_sgr_code w first (if bg then 48 else 38) in
  let first = emit_sgr_code w first 2 in
  let first = emit_sgr_code w first (Color.Packed.red color) in
  let first = emit_sgr_code w first (Color.Packed.green color) in
  emit_sgr_code w first (Color.Packed.blue color)

let emit_default_sgr w first ~bg = emit_sgr_code w first (if bg then 49 else 39)

let emit_color_sgr t w first ~bg ~initial color =
  match Color.Packed.intent color with
  | Color.Default -> if initial then first else emit_default_sgr w first ~bg
  | Color.Indexed idx -> (
      match t.color_depth with
      | `Truecolor | `Ansi256 -> emit_indexed_sgr w first ~bg idx
      | `Ansi16 -> emit_ansi16_sgr w first ~bg (nearest_palette_index 16 color))
  | Color.Rgb -> (
      if Color.Packed.alpha color = 0 then
        if initial then first else emit_default_sgr w first ~bg
      else
        match t.color_depth with
        | `Truecolor -> emit_rgb_sgr w first ~bg color
        | `Ansi256 ->
            emit_indexed_sgr w first ~bg (nearest_palette_index 256 color)
        | `Ansi16 ->
            emit_ansi16_sgr w first ~bg (nearest_palette_index 16 color))

let emit_full_update t w ~fg ~bg ~attrs =
  Escape.sgr_open w;
  Escape.sgr_code w 0;
  let first = emit_color_sgr t w false ~bg:false ~initial:true fg in
  let first = emit_color_sgr t w first ~bg:true ~initial:true bg in
  ignore (Sgr_attrs.emit_enabled w first attrs);
  Escape.sgr_close w

let emit_diff_update t w ~fg ~bg ~attrs =
  Escape.sgr_open w;
  let first =
    if fg <> t.fg_color then emit_color_sgr t w true ~bg:false ~initial:false fg
    else true
  in
  let first =
    if bg <> t.bg_color then emit_color_sgr t w first ~bg:true ~initial:false bg
    else first
  in
  let first =
    if attrs = t.attrs then first
    else
      let removed = t.attrs land lnot attrs in
      let first = Sgr_attrs.emit_disabled w first removed in
      Sgr_attrs.emit_enabled w first (Sgr_attrs.added ~prev:t.attrs ~attrs)
  in
  ignore first;
  Escape.sgr_close w

let update t w ~fg ~bg ~attrs ~link =
  update_link t w link;
  let fg = Color.Packed.encode fg in
  let bg = Color.Packed.encode bg in
  if fg <> t.fg_color || bg <> t.bg_color || attrs <> t.attrs then (
    if t.fg_color < 0 || t.bg_color < 0 || t.attrs < 0 then
      emit_full_update t w ~fg ~bg ~attrs
    else if fg <> t.fg_color && bg <> t.bg_color && attrs <> t.attrs then
      emit_full_update t w ~fg ~bg ~attrs
    else emit_diff_update t w ~fg ~bg ~attrs;
    t.fg_color <- fg;
    t.bg_color <- bg;
    t.attrs <- attrs)

let close_link t w =
  if t.link_open then (
    Escape.hyperlink_close w;
    t.link <- "";
    t.link_open <- false)
