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

let equal a b =
  a.attrs = b.attrs && a.link = b.link
  && Option.equal Color.equal a.fg b.fg
  && Option.equal Color.equal a.bg b.bg

(* Check if visual properties (colors, attrs) are equal, ignoring link. Used by
   emit to avoid emitting empty SGR sequences when only link differs. *)
let visual_equal a b =
  a.attrs = b.attrs
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

(* Check if disabling these attributes would also disable attributes we want to
   keep. Bold/Dim share code 22, Underline/Double_underline share 24,
   Framed/Encircled share 54. Returns attributes that need to be re-enabled
   after the disable codes are emitted. *)
let attrs_to_reenable ~to_disable ~to_keep =
  let check_shared_pair a b acc =
    if Attr.mem a to_disable || Attr.mem b to_disable then
      let acc = if Attr.mem a to_keep then Attr.add a acc else acc in
      if Attr.mem b to_keep then Attr.add b acc else acc
    else acc
  in
  Attr.empty
  |> check_shared_pair Attr.Bold Attr.Dim
  |> check_shared_pair Attr.Underline Attr.Double_underline
  |> check_shared_pair Attr.Framed Attr.Encircled

(* Common helper for SGR code emission. Emits the minimal set of codes to
   transition from [prev] to [t] by calling [push] for each code. *)
let emit_sgr_diff ~prev t push =
  let attrs_to_disable = Attr.diff prev.attrs t.attrs in
  let attrs_to_enable = Attr.diff t.attrs prev.attrs in
  let to_reenable =
    attrs_to_reenable ~to_disable:attrs_to_disable ~to_keep:t.attrs
  in
  let attrs_to_enable = Attr.union attrs_to_enable to_reenable in
  (* Emit disable codes for removed attributes *)
  Attr.iter_sgr_disable_codes push attrs_to_disable;
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
  Attr.iter_sgr_codes push attrs_to_enable

let to_sgr_codes ?prev t =
  let prev = Option.value prev ~default in
  if equal prev t then []
  else if equal t default then [ 0 ]
  else
    let rev_codes = ref [] in
    emit_sgr_diff ~prev t (fun code -> rev_codes := code :: !rev_codes);
    List.rev !rev_codes

let emit ?prev t (w : Escape.writer) =
  let prev = Option.value prev ~default in
  (* Check visual properties only - link changes are handled by segment layer *)
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
