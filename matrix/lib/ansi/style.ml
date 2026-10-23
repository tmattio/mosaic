type t = {
  fg : Color.t option;
  bg : Color.t option;
  attrs : Attr.t;
  link : string option;
}

let default = { fg = None; bg = None; attrs = Attr.empty; link = None }

let make ?fg ?bg ?(bold = false) ?(dim = false) ?(italic = false)
    ?(underline = false) ?(blink = false) ?(inverse = false) ?(hidden = false)
    ?(strikethrough = false) ?(overline = false) ?(double_underline = false)
    ?(framed = false) ?(encircled = false) ?link () =
  let attrs =
    Attr.combine ~bold ~dim ~italic ~underline ~blink ~inverse ~hidden
      ~strikethrough ~overline ~double_underline ~framed ~encircled ()
  in
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

(* Field-wise monomorphic comparisons: [attrs] and [link] under polymorphic
   [=] go through the generic runtime compare, and [equal] is one of the
   hottest calls of a render pass (per-cell style diffing). *)
let equal a b =
  Attr.equal a.attrs b.attrs
  && Option.equal Color.equal a.fg b.fg
  && Option.equal Color.equal a.bg b.bg
  && Option.equal String.equal a.link b.link

(* Check if visual properties (colors, attrs) are equal, ignoring link. Used by
   emit to avoid emitting empty SGR sequences when only link differs. *)
let visual_equal a b =
  Attr.equal a.attrs b.attrs
  && Option.equal Color.equal a.fg b.fg
  && Option.equal Color.equal a.bg b.bg

let compare a b =
  let c = Attr.compare a.attrs b.attrs in
  if c <> 0 then c
  else
    let compare_color_opt c1 c2 =
      match (c1, c2) with
      | None, None -> 0
      | None, Some _ -> -1
      | Some _, None -> 1
      | Some ca, Some cb -> Color.compare ca cb
    in
    let c = compare_color_opt a.fg b.fg in
    if c <> 0 then c
    else
      let c = compare_color_opt a.bg b.bg in
      if c <> 0 then c else Option.compare String.compare a.link b.link

let hash t =
  let h = 17 in
  let h = (h * 31) + match t.fg with None -> 0 | Some c -> Color.hash c in
  let h = (h * 31) + match t.bg with None -> 0 | Some c -> Color.hash c in
  let h = (h * 31) + Attr.pack t.attrs in
  let h = (h * 31) + match t.link with None -> 0 | Some s -> Hashtbl.hash s in
  h land max_int

(* Emission *)

(* Common helper for SGR code emission. Emits the minimal set of codes to
   transition from [prev] to [t] by calling [push] for each code. *)
let emit_sgr_diff ~prev t push =
  let prev_attrs = Attr.pack prev.attrs and attrs = Attr.pack t.attrs in
  let attrs_to_disable = prev_attrs land lnot attrs in
  let attrs_to_enable = Sgr_attrs.added ~prev:prev_attrs ~attrs in
  Sgr_attrs.iter_disabled push attrs_to_disable;
  (* Handle foreground color changes *)
  let fg_changed = not (Option.equal Color.equal prev.fg t.fg) in
  (if fg_changed then
     match t.fg with
     | None -> push 39
     | Some color -> Color.emit_sgr_codes ~bg:false push color);
  (* Handle background color changes *)
  let bg_changed = not (Option.equal Color.equal prev.bg t.bg) in
  (if bg_changed then
     match t.bg with
     | None -> push 49
     | Some color -> Color.emit_sgr_codes ~bg:true push color);
  (* Emit enable codes for added attributes *)
  Sgr_attrs.iter_enabled push attrs_to_enable

(* A transition compares colors and attributes alone: links are written by
   the hyperlink layer. *)
let to_sgr_codes ?prev t =
  let prev = Option.value prev ~default in
  if visual_equal prev t then []
  else if visual_equal t default then [ 0 ]
  else
    let rev_codes = ref [] in
    emit_sgr_diff ~prev t (fun code -> rev_codes := code :: !rev_codes);
    List.rev !rev_codes

let emit ?prev t (w : Writer.t) =
  let prev = Option.value prev ~default in
  if visual_equal prev t then ()
  else if visual_equal t default then Escape.sgr [ 0 ] w
  else Escape.sgr_direct (emit_sgr_diff ~prev t) w

let sgr_sequence ?prev t =
  match to_sgr_codes ?prev t with
  | [] -> ""
  | codes -> Escape.to_string (fun w -> Escape.sgr codes w)

let styled ?(reset = false) t str =
  if equal t default then str
  else
    Escape.to_string (fun w ->
        emit t w;
        Escape.literal str w;
        if reset then Escape.reset w)

(* Reading SGR *)

let set_color code color t =
  if code = 38 then { t with fg = Some color }
  else if code = 48 then { t with bg = Some color }
  else t (* 58, the underline color, is not represented *)

let apply_sgr p t =
  let module P = Parser.Params in
  let n = P.count p in
  let field i j = Option.value (P.field p i j) ~default:0 in
  let rec go i t =
    if i >= n then t
    else
      let code = P.ps p i ~default:0 in
      if P.fields p i > 1 && code <> 4 && code <> 38 && code <> 48 && code <> 58
      then
        (* Sub-parameters mean what the function defines (ECMA-48 §5.4.2):
           SGR defines them for the colours (T.416) and the underline style
           only, so with any other code the parameter has no effect. *)
        go (i + 1) t
      else
        match code with
        | 0 -> go (i + 1) { default with link = t.link }
        | (38 | 48 | 58) when P.fields p i > 1 -> (
            (* T.416: 38:5:n, 38:2:r:g:b and 38:2:cs:r:g:b *)
            let fields = P.fields p i in
            match field i 1 with
            | 5 when fields >= 3 ->
                go (i + 1)
                  (set_color code (Color.of_palette_index (field i 2)) t)
            | 2 when fields = 5 ->
                let color = Color.of_rgb (field i 2) (field i 3) (field i 4) in
                go (i + 1) (set_color code color t)
            | 2 when fields >= 6 ->
                let color = Color.of_rgb (field i 3) (field i 4) (field i 5) in
                go (i + 1) (set_color code color t)
            | _ -> go (i + 1) t)
        | 38 | 48 | 58 -> (
            (* xterm: 38;5;n and 38;2;r;g;b *)
            let arg k = P.ps p (i + k) ~default:0 in
            match arg 1 with
            | 5 when i + 2 < n ->
                go (i + 3) (set_color code (Color.of_palette_index (arg 2)) t)
            | 2 when i + 4 < n ->
                let color = Color.of_rgb (arg 2) (arg 3) (arg 4) in
                go (i + 5) (set_color code color t)
            | 2 | 5 -> t (* the parameters left are an incomplete color *)
            | _ -> go (i + 1) t)
        | 39 -> go (i + 1) { t with fg = Some Color.default_fg }
        | 49 -> go (i + 1) { t with bg = Some Color.default_bg }
        | _ when code >= 30 && code <= 37 ->
            go (i + 1) { t with fg = Some (Color.of_palette_index (code - 30)) }
        | _ when code >= 40 && code <= 47 ->
            go (i + 1) { t with bg = Some (Color.of_palette_index (code - 40)) }
        | _ when code >= 90 && code <= 97 ->
            go (i + 1) { t with fg = Some (Color.of_palette_index (code - 82)) }
        | _ when code >= 100 && code <= 107 ->
            go (i + 1) { t with bg = Some (Color.of_palette_index (code - 92)) }
        | _ ->
            (* 4:0 is 24 and 4:2 is 21; the other underline styles are 4, and
             rapid blink is blink. *)
            let code =
              if code = 4 && P.fields p i > 1 then
                match field i 1 with 0 -> 24 | 2 -> 21 | _ -> 4
              else if code = 6 then 5
              else code
            in
            let on = Sgr_attrs.enabled_by code in
            let off = Sgr_attrs.disabled_by code in
            if on lor off = 0 then go (i + 1) t
            else
              let attrs = Attr.pack t.attrs land lnot off lor on in
              go (i + 1) { t with attrs = Attr.unpack attrs }
  in
  if n = 0 then { default with link = t.link } else go 0 t

let pp fmt t =
  Format.fprintf fmt "Style{";
  let first = ref true in
  let sep () = if !first then first := false else Format.fprintf fmt ", " in
  Option.iter
    (fun c ->
      sep ();
      Format.fprintf fmt "fg=%s" (Color.to_hex c))
    t.fg;
  Option.iter
    (fun c ->
      sep ();
      Format.fprintf fmt "bg=%s" (Color.to_hex c))
    t.bg;
  if not (Attr.is_empty t.attrs) then (
    sep ();
    Format.fprintf fmt "attrs=%a" Attr.pp t.attrs);
  Option.iter
    (fun url ->
      sep ();
      Format.fprintf fmt "link=%s" url)
    t.link;
  Format.fprintf fmt "}"
