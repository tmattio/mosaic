type color_depth = Color.depth

type t = {
  mutable fg_color : int;
  mutable bg_color : int;
  mutable attrs : int;
  mutable link : string; (* "" = no link *)
  mutable link_open : bool;
  mutable color_depth : color_depth;
  (* The writer and the separator state of the codes [push] writes. *)
  mutable out : Writer.t;
  mutable first : bool;
  push : int -> unit;
}

let emit_sgr_code w first code =
  if not first then Escape.sgr_sep w;
  Escape.sgr_code w code;
  false

let create () =
  let rec t =
    {
      fg_color = -1;
      bg_color = -1;
      attrs = -1;
      link = "";
      link_open = false;
      color_depth = `Truecolor;
      out = Writer.create 0;
      first = true;
      push = (fun code -> t.first <- emit_sgr_code t.out t.first code);
    }
  in
  t

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

(* A color's codes are Color.emit_sgr_codes's at the tracker's depth, pushed
   through [t.push], made once with the tracker, so a write builds no
   closure. [initial] leaves out a default color after [CSI 0]. *)
let emit_color_sgr t w first ~bg ~initial color =
  let c = Color.to_depth t.color_depth (Color.Packed.decode color) in
  let default =
    match Color.intent c with
    | Color.Default -> true
    | Color.Rgb -> Color.alpha c = 0.
    | Color.Indexed _ -> false
  in
  if initial && default then first
  else (
    t.out <- w;
    t.first <- first;
    Color.emit_sgr_codes ~depth:t.color_depth ~bg t.push c;
    t.first)

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
