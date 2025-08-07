type adaptive_color = { light : Ansi.color; dark : Ansi.color }

(* Gradient type for linear color interpolation *)
type gradient = {
  colors : Ansi.color list;
  direction : [ `Horizontal | `Vertical ];
}

(* Color specification: solid, adaptive, or gradient *)
type color_spec =
  | Solid of Ansi.color
  | Adaptive of adaptive_color
  | Gradient of gradient

type t = {
  fg : color_spec option;
  bg : color_spec option;
  bold : bool;
  dim : bool;
  italic : bool;
  underline : bool;
  double_underline : bool;
  blink : bool;
  reverse : bool;
  strikethrough : bool;
  overline : bool;
  conceal : bool;
  framed : bool;
  encircled : bool;
  uri : string option;
}

let default =
  {
    fg = None;
    bg = None;
    bold = false;
    dim = false;
    italic = false;
    underline = false;
    double_underline = false;
    blink = false;
    reverse = false;
    strikethrough = false;
    overline = false;
    conceal = false;
    framed = false;
    encircled = false;
    uri = None;
  }

(* Attribute type for building styles from lists *)
type attr =
  | Fg of Ansi.color
  | Bg of Ansi.color
  | Fg_gradient of gradient
  | Bg_gradient of gradient
  | Bold
  | Dim
  | Italic
  | Underline
  | Double_underline
  | Blink
  | Reverse
  | Strikethrough
  | Overline
  | Conceal
  | Framed
  | Encircled
  | Link of string

let empty = default
let fg color = { empty with fg = Some (Solid color) }
let bg color = { empty with bg = Some (Solid color) }

(* Gradient constructors *)
let gradient ~colors ~direction =
  if colors = [] then
    raise (Invalid_argument "Gradient must have at least one color")
  else { colors; direction }

let gradient_fg ~colors ~direction =
  if colors = [] then
    raise (Invalid_argument "Gradient must have at least one color")
  else { empty with fg = Some (Gradient { colors; direction }) }

let gradient_bg ~colors ~direction =
  if colors = [] then
    raise (Invalid_argument "Gradient must have at least one color")
  else { empty with bg = Some (Gradient { colors; direction }) }

let bold = { empty with bold = true }
let dim = { empty with dim = true }
let italic = { empty with italic = true }
let underline = { empty with underline = true }
let double_underline = { empty with double_underline = true }
let blink = { empty with blink = true }
let reverse = { empty with reverse = true }
let strikethrough = { empty with strikethrough = true }
let overline = { empty with overline = true }
let conceal = { empty with conceal = true }
let framed = { empty with framed = true }
let encircled = { empty with encircled = true }
let link uri = { empty with uri = Some uri }

(* Create a style from a list of attributes *)
let of_list attrs =
  List.fold_left
    (fun style attr ->
      match attr with
      | Fg color -> { style with fg = Some (Solid color) }
      | Bg color -> { style with bg = Some (Solid color) }
      | Fg_gradient g -> { style with fg = Some (Gradient g) }
      | Bg_gradient g -> { style with bg = Some (Gradient g) }
      | Bold -> { style with bold = true }
      | Dim -> { style with dim = true }
      | Italic -> { style with italic = true }
      | Underline -> { style with underline = true }
      | Double_underline -> { style with double_underline = true }
      | Blink -> { style with blink = true }
      | Reverse -> { style with reverse = true }
      | Strikethrough -> { style with strikethrough = true }
      | Overline -> { style with overline = true }
      | Conceal -> { style with conceal = true }
      | Framed -> { style with framed = true }
      | Encircled -> { style with encircled = true }
      | Link uri -> { style with uri = Some uri })
    empty attrs

let merge (a : t) (b : t) : t =
  {
    fg = (match b.fg with Some _ -> b.fg | None -> a.fg);
    bg = (match b.bg with Some _ -> b.bg | None -> a.bg);
    bold = a.bold || b.bold;
    dim = a.dim || b.dim;
    italic = a.italic || b.italic;
    underline = a.underline || b.underline;
    double_underline = a.double_underline || b.double_underline;
    blink = a.blink || b.blink;
    reverse = a.reverse || b.reverse;
    strikethrough = a.strikethrough || b.strikethrough;
    overline = a.overline || b.overline;
    conceal = a.conceal || b.conceal;
    framed = a.framed || b.framed;
    encircled = a.encircled || b.encircled;
    uri = (match b.uri with Some _ -> b.uri | None -> a.uri);
  }

let ( ++ ) a b = merge a b
let ansi256 n = Ansi.Index n
let rgb r g b = Ansi.RGB (r, g, b)

(* Color type export *)
type color = Ansi.color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default
  | Bright_black
  | Bright_red
  | Bright_green
  | Bright_yellow
  | Bright_blue
  | Bright_magenta
  | Bright_cyan
  | Bright_white
  | Index of int (* 256-color palette (0-255) *)
  | RGB of int * int * int (* 24-bit color (0-255 each) *)
  | RGBA of int * int * int * int (* 24-bit color with alpha (0-255 each) *)

(* Color helpers *)
let gray n = Index (232 + min 23 (max 0 n))

let rgb_hex hex =
  let r = (hex lsr 16) land 0xFF in
  let g = (hex lsr 8) land 0xFF in
  let b = hex land 0xFF in
  RGB (r, g, b)

let adaptive ~light ~dark = { light; dark }

(* Terminal background state is now managed by the Terminal module *)
(* Adaptive colors should be resolved at render time based on the terminal's state *)

let adaptive_fg color = { empty with fg = Some (Adaptive color) }
let adaptive_bg color = { empty with bg = Some (Adaptive color) }

(* Common adaptive colors *)
let adaptive_primary = { light = Black; dark = White }
let adaptive_secondary = { light = gray 8; dark = gray 15 }
let adaptive_accent = { light = Blue; dark = Bright_blue }
let adaptive_error = { light = Red; dark = Bright_red }
let adaptive_warning = { light = Yellow; dark = Bright_yellow }
let adaptive_success = { light = Green; dark = Bright_green }

(* Color interpolation for gradients *)

(* Standard RGB values for ANSI colors *)
let levels = [| 0; 95; 135; 175; 215; 255 |]

let basic_rgb = function
  | Black -> (0, 0, 0)
  | Red -> (128, 0, 0)
  | Green -> (0, 128, 0)
  | Yellow -> (128, 128, 0)
  | Blue -> (0, 0, 128)
  | Magenta -> (128, 0, 128)
  | Cyan -> (0, 128, 128)
  | White -> (192, 192, 192)
  | Default -> (192, 192, 192)
  | Bright_black -> (128, 128, 128)
  | Bright_red -> (255, 0, 0)
  | Bright_green -> (0, 255, 0)
  | Bright_yellow -> (255, 255, 0)
  | Bright_blue -> (0, 0, 255)
  | Bright_magenta -> (255, 0, 255)
  | Bright_cyan -> (0, 255, 255)
  | Bright_white -> (255, 255, 255)
  | Index i ->
      if i < 16 then
        match i with
        | 0 -> (0, 0, 0)
        | 1 -> (128, 0, 0)
        | 2 -> (0, 128, 0)
        | 3 -> (128, 128, 0)
        | 4 -> (0, 0, 128)
        | 5 -> (128, 0, 128)
        | 6 -> (0, 128, 128)
        | 7 -> (192, 192, 192)
        | 8 -> (128, 128, 128)
        | 9 -> (255, 0, 0)
        | 10 -> (0, 255, 0)
        | 11 -> (255, 255, 0)
        | 12 -> (0, 0, 255)
        | 13 -> (255, 0, 255)
        | 14 -> (0, 255, 255)
        | 15 -> (255, 255, 255)
        | _ -> (0, 0, 0)
      else if i < 232 then
        let i = i - 16 in
        let r = levels.(i / 36) in
        let g = levels.(i / 6 mod 6) in
        let b = levels.(i mod 6) in
        (r, g, b)
      else
        let v = 8 + ((i - 232) * 10) in
        (v, v, v)
  | RGB (r, g, b) -> (r, g, b)
  | RGBA (r, g, b, _) -> (r, g, b)
(* Ignore alpha for RGB conversion *)

let to_rgb = basic_rgb

(* Linear interpolation between two values *)
let lerp start_val end_val t =
  int_of_float
    (float_of_int start_val +. (t *. float_of_int (end_val - start_val)) +. 0.5)

(* Interpolate between two RGB colors *)
let interpolate_rgb (r1, g1, b1) (r2, g2, b2) t =
  let t = max 0.0 (min 1.0 t) in
  RGB (lerp r1 r2 t, lerp g1 g2 t, lerp b1 b2 t)

(* Gradient cache for performance - cache key is (colors_hash, t_rounded) *)
module Gradient_cache = struct
  type key = int * int (* colors hash * t rounded to 0.001 precision *)

  let cache : (key, Ansi.color) Hashtbl.t = Hashtbl.create 256

  let hash_colors colors =
    (* Simple hash of color list for cache key *)
    List.fold_left
      (fun acc c ->
        let c_hash =
          match c with
          | Default -> 0
          | Black -> 1
          | Red -> 2
          | Green -> 3
          | Yellow -> 4
          | Blue -> 5
          | Magenta -> 6
          | Cyan -> 7
          | White -> 8
          | Bright_black -> 9
          | Bright_red -> 10
          | Bright_green -> 11
          | Bright_yellow -> 12
          | Bright_blue -> 13
          | Bright_magenta -> 14
          | Bright_cyan -> 15
          | Bright_white -> 16
          | RGB (r, g, b) -> 17 + (r * 1000000) + (g * 1000) + b
          | RGBA (r, g, b, a) ->
              18 + (r * 1000000000) + (g * 1000000) + (b * 1000) + a
          | Index i -> 19 + i
        in
        ((acc * 31) + c_hash) land max_int)
      0 colors

  let round_t t = int_of_float (t *. 1000.0) (* Round to 0.001 precision *)

  let get colors t compute =
    let key = (hash_colors colors, round_t t) in
    match Hashtbl.find_opt cache key with
    | Some color -> color
    | None ->
        let color = compute () in
        (* Limit cache size to prevent unbounded growth *)
        if Hashtbl.length cache < 1024 then Hashtbl.add cache key color;
        color
end

(* Calculate the color at position t (0.0 to 1.0) along a gradient *)
let calculate_gradient_color colors t =
  let t = max 0.0 (min 1.0 t) in
  match colors with
  | [] ->
      Default (* Return Default color for empty gradient instead of raising *)
  | [ c ] -> c
  | _ ->
      Gradient_cache.get colors t (fun () ->
          let num_segments = float_of_int (List.length colors - 1) in
          let segment_index =
            min (int_of_float (t *. num_segments)) (List.length colors - 2)
          in
          let segment_start_t = float_of_int segment_index /. num_segments in
          let c1 = List.nth colors segment_index in
          let c2 = List.nth colors (segment_index + 1) in
          let segment_t = (t -. segment_start_t) /. (1. /. num_segments) in
          interpolate_rgb (to_rgb c1) (to_rgb c2) segment_t)

let to_attrs ~dark style =
  let attrs = ref [] in
  if style.encircled then attrs := `Encircled :: !attrs;
  if style.framed then attrs := `Framed :: !attrs;
  if style.conceal then attrs := `Conceal :: !attrs;
  if style.overline then attrs := `Overline :: !attrs;
  if style.strikethrough then attrs := `Strikethrough :: !attrs;
  if style.reverse then attrs := `Reverse :: !attrs;
  if style.blink then attrs := `Blink :: !attrs;
  if style.double_underline then attrs := `Double_underline :: !attrs
  else if style.underline then attrs := `Underline :: !attrs;
  if style.italic then attrs := `Italic :: !attrs;
  if style.dim then attrs := `Dim :: !attrs;
  if style.bold then attrs := `Bold :: !attrs;
  (* Extract concrete colors from color_spec *)
  (match style.bg with
  | Some (Solid c) -> attrs := `Bg c :: !attrs
  | Some (Adaptive adaptive) ->
      let c = if dark then adaptive.dark else adaptive.light in
      attrs := `Bg c :: !attrs
  | Some (Gradient _) -> () (* Gradients should be resolved before this *)
  | None -> ());
  (match style.fg with
  | Some (Solid c) -> attrs := `Fg c :: !attrs
  | Some (Adaptive adaptive) ->
      let c = if dark then adaptive.dark else adaptive.light in
      attrs := `Fg c :: !attrs
  | Some (Gradient _) -> () (* Gradients should be resolved before this *)
  | None -> ());
  !attrs

let to_sgr ~dark style = Ansi.sgr (to_attrs ~dark style)

(* Resolve a color_spec to a concrete color based on position *)
let resolve_color spec ~dark ~x ~y ~width ~height =
  match spec with
  | None -> None
  | Some (Solid color) -> Some color
  | Some (Adaptive adaptive) ->
      let selected = if dark then adaptive.dark else adaptive.light in
      Some selected
  | Some (Gradient gradient) ->
      let t =
        match gradient.direction with
        | `Horizontal ->
            if width <= 1 then 0.0
            else float_of_int x /. float_of_int (width - 1)
        | `Vertical ->
            if height <= 1 then 0.0
            else float_of_int y /. float_of_int (height - 1)
      in
      Some (calculate_gradient_color gradient.colors t)

(* Predefined styles *)
let error = fg Red ++ bold
let warning = fg Yellow ++ underline
let success = fg Green ++ italic
let info = fg Blue
let muted = dim

(* Utility functions *)
let equal a b =
  a.fg = b.fg && a.bg = b.bg && a.bold = b.bold && a.dim = b.dim
  && a.italic = b.italic && a.underline = b.underline
  && a.double_underline = b.double_underline
  && a.blink = b.blink && a.reverse = b.reverse
  && a.strikethrough = b.strikethrough
  && a.overline = b.overline && a.conceal = b.conceal && a.framed = b.framed
  && a.encircled = b.encircled && a.uri = b.uri

let pp_color fmt = function
  | Ansi.Black -> Format.fprintf fmt "Black"
  | Red -> Format.fprintf fmt "Red"
  | Green -> Format.fprintf fmt "Green"
  | Yellow -> Format.fprintf fmt "Yellow"
  | Blue -> Format.fprintf fmt "Blue"
  | Magenta -> Format.fprintf fmt "Magenta"
  | Cyan -> Format.fprintf fmt "Cyan"
  | White -> Format.fprintf fmt "White"
  | Default -> Format.fprintf fmt "Default"
  | Bright_black -> Format.fprintf fmt "Bright_black"
  | Bright_red -> Format.fprintf fmt "Bright_red"
  | Bright_green -> Format.fprintf fmt "Bright_green"
  | Bright_yellow -> Format.fprintf fmt "Bright_yellow"
  | Bright_blue -> Format.fprintf fmt "Bright_blue"
  | Bright_magenta -> Format.fprintf fmt "Bright_magenta"
  | Bright_cyan -> Format.fprintf fmt "Bright_cyan"
  | Bright_white -> Format.fprintf fmt "Bright_white"
  | Index i -> Format.fprintf fmt "Index(%d)" i
  | RGB (r, g, b) -> Format.fprintf fmt "RGB(%d,%d,%d)" r g b
  | RGBA (r, g, b, a) -> Format.fprintf fmt "RGBA(%d,%d,%d,%d)" r g b a

let pp_color_spec fmt = function
  | None -> Format.fprintf fmt "None"
  | Some (Solid c) -> Format.fprintf fmt "Solid(%a)" pp_color c
  | Some (Adaptive { light; dark }) ->
      Format.fprintf fmt "Adaptive(light=%a,dark=%a)" pp_color light pp_color
        dark
  | Some (Gradient { colors; direction }) ->
      let dir_str =
        match direction with `Horizontal -> "H" | `Vertical -> "V"
      in
      Format.fprintf fmt "Gradient(%s,%d colors)" dir_str (List.length colors)

let pp fmt style =
  Format.fprintf fmt "@[<v>Style{@[<v 2>@,";
  if style.fg <> None then Format.fprintf fmt "fg=%a@," pp_color_spec style.fg;
  if style.bg <> None then Format.fprintf fmt "bg=%a@," pp_color_spec style.bg;
  if style.bold then Format.fprintf fmt "bold@,";
  if style.dim then Format.fprintf fmt "dim@,";
  if style.italic then Format.fprintf fmt "italic@,";
  if style.underline then Format.fprintf fmt "underline@,";
  if style.double_underline then Format.fprintf fmt "double_underline@,";
  if style.blink then Format.fprintf fmt "blink@,";
  if style.reverse then Format.fprintf fmt "reverse@,";
  if style.strikethrough then Format.fprintf fmt "strikethrough@,";
  if style.overline then Format.fprintf fmt "overline@,";
  if style.conceal then Format.fprintf fmt "conceal@,";
  if style.framed then Format.fprintf fmt "framed@,";
  if style.encircled then Format.fprintf fmt "encircled@,";
  Option.iter (Format.fprintf fmt "uri=%S@,") style.uri;
  Format.fprintf fmt "@]@,}@]"

let resolve (style : t) ~dark ~(pos : int * int) ~(bounds : int * int) :
    Ansi.Style.t =
  let x, y = pos in
  let width, height = bounds in

  (* Start with default style *)
  let cell_style = ref Ansi.Style.default in

  (* Resolve foreground color *)
  (match resolve_color style.fg ~dark ~x ~y ~width ~height with
  | Some color -> cell_style := Ansi.Style.with_fg color !cell_style
  | None -> ());

  (* Resolve background color *)
  (match resolve_color style.bg ~dark ~x ~y ~width ~height with
  | Some color -> cell_style := Ansi.Style.with_bg color !cell_style
  | None -> ());

  (* Apply text attributes *)
  if style.bold then cell_style := Ansi.Style.with_bold true !cell_style;
  if style.dim then cell_style := Ansi.Style.with_dim true !cell_style;
  if style.italic then cell_style := Ansi.Style.with_italic true !cell_style;
  if style.underline then
    cell_style := Ansi.Style.with_underline true !cell_style;
  if style.double_underline then
    cell_style := Ansi.Style.with_double_underline true !cell_style;
  if style.blink then cell_style := Ansi.Style.with_blink true !cell_style;
  if style.reverse then cell_style := Ansi.Style.with_reversed true !cell_style;
  if style.strikethrough then
    cell_style := Ansi.Style.with_strikethrough true !cell_style;
  if style.overline then cell_style := Ansi.Style.with_overline true !cell_style;

  (* Apply hyperlink if present *)
  (* TODO: hyperlink support not yet implemented in Ansi.Style
  (match style.uri with
  | Some uri -> cell_style := Ansi.Style.with_link (Some uri) !cell_style
  | None -> ());
  *)
  !cell_style
