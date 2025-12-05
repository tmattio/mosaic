module Esc = Escape

type t = {
  fg : Color.t option;
  bg : Color.t option;
  attrs : Attr.t;
  link : string option;
}

let default = { fg = None; bg = None; attrs = Attr.empty; link = None }

let error =
  { fg = Some Color.bright_red; bg = None; attrs = Attr.empty; link = None }

let success =
  { fg = Some Color.bright_green; bg = None; attrs = Attr.empty; link = None }

let warning =
  { fg = Some Color.bright_yellow; bg = None; attrs = Attr.empty; link = None }

let info =
  { fg = Some Color.bright_blue; bg = None; attrs = Attr.empty; link = None }

let make ?fg ?bg ?(bold = false) ?(dim = false) ?(italic = false)
    ?(underline = false) ?(blink = false) ?(inverse = false) ?(hidden = false)
    ?(strikethrough = false) ?(overline = false) ?(double_underline = false)
    ?(framed = false) ?(encircled = false) ?link () =
  let attrs =
    Attr.combine ~bold ~dim ~italic ~underline ~blink ~inverse ~hidden
      ~strikethrough ~overline ~double_underline ()
  in
  let attrs = if framed then Attr.add Attr.Framed attrs else attrs in
  let attrs = if encircled then Attr.add Attr.Encircled attrs else attrs in
  { fg; bg; attrs; link }

(* Modifiers *)

let fg fg t = { t with fg = Some fg }
let bg bg t = { t with bg = Some bg }
let with_no_fg t = { t with fg = None }
let with_no_bg t = { t with bg = None }
let with_attrs attrs t = { t with attrs }
let add_attr flag t = { t with attrs = Attr.add flag t.attrs }
let remove_attr flag t = { t with attrs = Attr.remove flag t.attrs }
let overlay_attrs t attrs = { t with attrs = Attr.union t.attrs attrs }

let with_bold enabled t =
  { t with attrs = Attr.with_flag Attr.Bold enabled t.attrs }

let with_dim enabled t =
  { t with attrs = Attr.with_flag Attr.Dim enabled t.attrs }

let with_italic enabled t =
  { t with attrs = Attr.with_flag Attr.Italic enabled t.attrs }

let with_underline enabled t =
  { t with attrs = Attr.with_flag Attr.Underline enabled t.attrs }

let with_double_underline enabled t =
  { t with attrs = Attr.with_flag Attr.Double_underline enabled t.attrs }

let with_blink enabled t =
  { t with attrs = Attr.with_flag Attr.Blink enabled t.attrs }

let with_inverse enabled t =
  { t with attrs = Attr.with_flag Attr.Inverse enabled t.attrs }

let with_hidden enabled t =
  { t with attrs = Attr.with_flag Attr.Hidden enabled t.attrs }

let with_strikethrough enabled t =
  { t with attrs = Attr.with_flag Attr.Strikethrough enabled t.attrs }

let with_overline enabled t =
  { t with attrs = Attr.with_flag Attr.Overline enabled t.attrs }

let with_framed enabled t =
  { t with attrs = Attr.with_flag Attr.Framed enabled t.attrs }

let with_encircled enabled t =
  { t with attrs = Attr.with_flag Attr.Encircled enabled t.attrs }

let hyperlink url t = { t with link = Some url }
let link t = t.link
let unlink t = { t with link = None }

(* Composition *)

let overlay_option ~over ~base =
  match over with Some v -> Some v | None -> base

let merge ~base ~overlay =
  {
    fg = overlay_option ~over:overlay.fg ~base:base.fg;
    bg = overlay_option ~over:overlay.bg ~base:base.bg;
    attrs = Attr.union base.attrs overlay.attrs;
    link = overlay_option ~over:overlay.link ~base:base.link;
  }

let ( ++ ) base overlay = merge ~base ~overlay

let resolve styles =
  List.fold_left
    (fun acc style -> merge ~base:acc ~overlay:style)
    default styles

(* Comparison *)

let equal a b =
  a.attrs = b.attrs && a.link = b.link
  && Option.equal Color.equal a.fg b.fg
  && Option.equal Color.equal a.bg b.bg

let compare a b =
  (* Compare attributes first (fast int comparison) *)
  let c = Attr.compare a.attrs b.attrs in
  if c <> 0 then c
  else
    (* Compare foreground packed (fast int64 comparison) *)
    let compare_color_opt c1 c2 =
      match (c1, c2) with
      | None, None -> 0
      | None, Some _ -> -1
      | Some _, None -> 1
      | Some ca, Some cb -> Int64.compare (Color.pack ca) (Color.pack cb)
    in
    let c = compare_color_opt a.fg b.fg in
    if c <> 0 then c
    else
      (* Compare background packed *)
      let c = compare_color_opt a.bg b.bg in
      if c <> 0 then c
      else (* Fallback to link string comparison *)
        Option.compare String.compare a.link b.link

let hash t = Hashtbl.hash (t.fg, t.bg, t.attrs, t.link)

(* Emission *)

let attr_enable_code = function
  | Attr.Bold -> 1
  | Attr.Dim -> 2
  | Attr.Italic -> 3
  | Attr.Underline -> 4
  | Attr.Double_underline -> 21
  | Attr.Blink -> 5
  | Attr.Inverse -> 7
  | Attr.Hidden -> 8
  | Attr.Strikethrough -> 9
  | Attr.Overline -> 53
  | Attr.Framed -> 51
  | Attr.Encircled -> 52

let to_sgr_codes ?prev t =
  let prev = Option.value prev ~default in
  if equal prev t then []
  else
    let rev_codes = ref [] in
    let push code = rev_codes := code :: !rev_codes in
    (* Always include reset code 0 for deterministic state *)
    push 0;
    (match t.fg with
    | None -> ()
    | Some color -> Color.to_sgr_codes ~bg:false color |> List.iter push);
    (match t.bg with
    | None -> ()
    | Some color -> Color.to_sgr_codes ~bg:true color |> List.iter push);
    Attr.iter (fun flag -> push (attr_enable_code flag)) t.attrs;
    List.rev !rev_codes

let emit ?prev t (w : Esc.writer) =
  let prev = Option.value prev ~default in
  if equal prev t then ()
  else
    Esc.sgr_direct
      (fun push ->
        push 0;
        (match t.fg with
        | None -> ()
        | Some color -> Color.emit_sgr_codes_push ~bg:false push color);
        (match t.bg with
        | None -> ()
        | Some color -> Color.emit_sgr_codes_push ~bg:true push color);
        Attr.iter (fun flag -> push (attr_enable_code flag)) t.attrs)
      w

let emit_raw ~prev_fg_r ~prev_fg_g ~prev_fg_b ~prev_fg_a ~prev_bg_r ~prev_bg_g
    ~prev_bg_b ~prev_bg_a ~prev_attrs ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r ~bg_g ~bg_b
    ~bg_a ~attrs (w : Esc.writer) =
  let[@inline] floats_equal a b = Float.abs (a -. b) <= 0.00001 in
  let[@inline] float_to_u8 f =
    if Float.is_nan f then 0
    else if f <= 1.0 && f >= 0.0 then
      Int.min 255 (Int.max 0 (int_of_float ((f *. 255.0) +. 0.5)))
    else Int.min 255 (Int.max 0 (int_of_float (f +. 0.5)))
  in
  (* Compare floats to detect changes *)
  let fg_changed =
    not
      (floats_equal fg_r prev_fg_r
      && floats_equal fg_g prev_fg_g
      && floats_equal fg_b prev_fg_b
      && floats_equal fg_a prev_fg_a)
  in
  let bg_visible = bg_a > 0.0001 in
  let prev_bg_visible = prev_bg_a > 0.0001 in
  let bg_changed =
    bg_visible <> prev_bg_visible
    || bg_visible
       && not
            (floats_equal bg_r prev_bg_r
            && floats_equal bg_g prev_bg_g
            && floats_equal bg_b prev_bg_b)
  in
  let attrs_changed = attrs <> prev_attrs in

  if not (fg_changed || bg_changed || attrs_changed) then ()
  else
    Esc.sgr_direct
      (fun push ->
        (* Start with reset to clear previous state *)
        push 0;

        if fg_changed then (
          push 38;
          push 2;
          push (float_to_u8 fg_r);
          push (float_to_u8 fg_g);
          push (float_to_u8 fg_b));

        if bg_visible then (
          push 48;
          push 2;
          push (float_to_u8 bg_r);
          push (float_to_u8 bg_g);
          push (float_to_u8 bg_b));

        (* Unpack attributes from int bitmask *)
        if attrs <> 0 then
          let attrs_t = Attr.unpack attrs in
          Attr.iter (fun flag -> push (attr_enable_code flag)) attrs_t)
      w

let sgr_sequence ?prev t =
  match to_sgr_codes ?prev t with
  | [] -> ""
  | codes -> Esc.to_string (fun w -> Esc.sgr codes w)

let styled ?(reset = false) t str =
  if equal t default then str
  else
    Esc.to_string (fun w ->
        emit t w;
        Esc.literal str w;
        if reset then Esc.reset w)

let pp fmt t =
  let parts = ref [] in
  (match t.fg with
  | None -> ()
  | Some color -> parts := ("fg=" ^ Color.to_hex color) :: !parts);
  (match t.bg with
  | None -> ()
  | Some color -> parts := ("bg=" ^ Color.to_hex color) :: !parts);
  if not (Attr.is_empty t.attrs) then
    parts := Format.asprintf "attrs=%a" Attr.pp t.attrs :: !parts;
  (match t.link with
  | None -> ()
  | Some url -> parts := ("link=" ^ url) :: !parts);
  Format.fprintf fmt "Style{%s}" (String.concat ", " (List.rev !parts))
