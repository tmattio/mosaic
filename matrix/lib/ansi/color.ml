type rgba = { r : float; g : float; b : float; a : float }

type t =
  | Default
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Bright_black
  | Bright_red
  | Bright_green
  | Bright_yellow
  | Bright_blue
  | Bright_magenta
  | Bright_cyan
  | Bright_white
  | Extended of int
  | Rgb of { r : int; g : int; b : int }
  | Rgba of { r : int; g : int; b : int; a : int }

let clamp_byte v = max 0 (min 255 v)
let clamp_extended idx = max 0 (min 255 idx)
let clamp_channel_f v = max 0. (min 1. v)
let float_of_byte v = float_of_int v /. 255.
let byte_of_float v = int_of_float (Float.round (clamp_channel_f v *. 255.))

(* CSS-like palette (RGB in [0,255]) *)
let ansi_16_rgb =
  [|
    (0, 0, 0);
    (* Black *)
    (255, 0, 0);
    (* Red *)
    (0, 128, 0);
    (* Green *)
    (255, 255, 0);
    (* Yellow *)
    (0, 0, 255);
    (* Blue *)
    (255, 0, 255);
    (* Magenta *)
    (0, 255, 255);
    (* Cyan *)
    (255, 255, 255);
    (* White *)
    (102, 102, 102);
    (* Bright_black / gray *)
    (255, 102, 102);
    (* Bright_red *)
    (102, 255, 102);
    (* Bright_green *)
    (255, 255, 102);
    (* Bright_yellow *)
    (102, 102, 255);
    (* Bright_blue *)
    (255, 102, 255);
    (* Bright_magenta *)
    (102, 255, 255);
    (* Bright_cyan *)
    (255, 255, 255);
    (* Bright_white *)
  |]

let cube_level = [| 0; 95; 135; 175; 215; 255 |]

let palette_index = function
  | Default -> -1 (* Special case *)
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7
  | Bright_black -> 8
  | Bright_red -> 9
  | Bright_green -> 10
  | Bright_yellow -> 11
  | Bright_blue -> 12
  | Bright_magenta -> 13
  | Bright_cyan -> 14
  | Bright_white -> 15
  | Extended idx -> clamp_extended idx
  | Rgb _ | Rgba _ -> -1 (* Not palette *)

let palette_rgb_int idx =
  let idx = clamp_byte idx in
  if idx < 16 then ansi_16_rgb.(idx)
  else if idx < 232 then
    let n = idx - 16 in
    let r = cube_level.(n / 36) in
    let g = cube_level.(n / 6 mod 6) in
    let b = cube_level.(n mod 6) in
    (r, g, b)
  else
    let gray = 8 + ((idx - 232) * 10) in
    (gray, gray, gray)

let to_rgb color =
  match color with
  | Default -> (0, 0, 0)
  | Rgb { r; g; b } -> (r, g, b)
  | Rgba { r; g; b; _ } -> (r, g, b)
  | _ ->
      let idx = palette_index color in
      if idx >= 0 then palette_rgb_int idx else (0, 0, 0)

let to_rgba color =
  match color with
  | Default ->
      (* Use alpha=0 as a sentinel to indicate "use terminal default color".
         This allows Default to be distinguished from explicit Black (0,0,0,255).
         Renderers should detect alpha=0 and substitute their configured default colors. *)
      (0, 0, 0, 0)
  | _ ->
      let r, g, b = to_rgb color in
      let a = match color with Rgba { a; _ } -> a | _ -> 255 in
      (r, g, b, a)

let to_rgba_f color =
  let r, g, b, a = to_rgba color in
  (float_of_byte r, float_of_byte g, float_of_byte b, float_of_byte a)

let default = Default
let black = Black
let red = Red
let green = Green
let yellow = Yellow
let blue = Blue
let magenta = Magenta
let cyan = Cyan
let white = White
let bright_black = Bright_black
let bright_red = Bright_red
let bright_green = Bright_green
let bright_yellow = Bright_yellow
let bright_blue = Bright_blue
let bright_magenta = Bright_magenta
let bright_cyan = Bright_cyan
let bright_white = Bright_white

let grayscale ~level =
  let level = max 0 (min 23 level) in
  Extended (216 + level)

let of_rgb r g b = Rgb { r = clamp_byte r; g = clamp_byte g; b = clamp_byte b }

let of_rgba r g b a =
  Rgba
    { r = clamp_byte r; g = clamp_byte g; b = clamp_byte b; a = clamp_byte a }

let of_palette_index idx =
  let idx = clamp_byte idx in
  if idx < 16 then
    match idx with
    | 0 -> Black
    | 1 -> Red
    | 2 -> Green
    | 3 -> Yellow
    | 4 -> Blue
    | 5 -> Magenta
    | 6 -> Cyan
    | 7 -> White
    | 8 -> Bright_black
    | 9 -> Bright_red
    | 10 -> Bright_green
    | 11 -> Bright_yellow
    | 12 -> Bright_blue
    | 13 -> Bright_magenta
    | 14 -> Bright_cyan
    | _ -> Bright_white
  else Extended idx

let of_hsl ~h ~s ~l ?a () =
  let h = Float.rem h 360. in
  let h = if h < 0. then h +. 360. else h in
  let s = clamp_channel_f s in
  let l = clamp_channel_f l in
  let a = Option.value a ~default:1.0 |> clamp_channel_f in
  let c = (1. -. abs_float ((2. *. l) -. 1.)) *. s in
  let x = c *. (1. -. abs_float (Float.rem h 2. -. 1.)) in
  let m = l -. (c /. 2.) in
  let r', g', b' =
    if h < 60. then (c, x, 0.)
    else if h < 120. then (x, c, 0.)
    else if h < 180. then (0., c, x)
    else if h < 240. then (0., x, c)
    else if h < 300. then (x, 0., c)
    else (c, 0., x)
  in
  let r = byte_of_float (r' +. m) in
  let g = byte_of_float (g' +. m) in
  let b = byte_of_float (b' +. m) in
  let alpha = byte_of_float a in
  if alpha = 255 then of_rgb r g b else of_rgba r g b alpha

let to_hsl color =
  let rf, gf, bf, af = to_rgba_f color in
  let max_val = max rf (max gf bf) in
  let min_val = min rf (min gf bf) in
  let l = (max_val +. min_val) /. 2. in
  if max_val = min_val then (0., 0., l, af)
  else
    let d = max_val -. min_val in
    let s =
      if l > 0.5 then d /. (2. -. max_val -. min_val)
      else d /. (max_val +. min_val)
    in
    let h =
      if max_val = rf then ((gf -. bf) /. d) +. if gf < bf then 6. else 0.
      else if max_val = gf then ((bf -. rf) /. d) +. 2.
      else ((rf -. gf) /. d) +. 4.
    in
    let h = h *. 60. in
    (h, s, l, af)

let equal a b =
  let ra, ga, ba, aa = to_rgba a in
  let rb, gb, bb, ab = to_rgba b in
  ra = rb && ga = gb && ba = bb && aa = ab

let hash color =
  let r, g, b, a = to_rgba color in
  Hashtbl.hash (r, g, b, a)

let alpha color =
  let _, _, _, a = to_rgba color in
  float_of_byte a

let blend ?(mode = `Perceptual) ~src ~dst () =
  let sr, sg, sb, sa_f = to_rgba_f src in
  let dr, dg, db, da_f = to_rgba_f dst in
  let sa = clamp_channel_f sa_f in
  if sa >= 0.999 then
    let r = byte_of_float sr in
    let g = byte_of_float sg in
    let b = byte_of_float sb in
    Rgb { r; g; b }
  else if sa <= Float.epsilon then dst
  else
    match mode with
    | `Linear ->
        let blended_channel sc dc = (sa *. sc) +. ((1. -. sa) *. dc) in
        let r = byte_of_float (blended_channel sr dr) in
        let g = byte_of_float (blended_channel sg dg) in
        let b = byte_of_float (blended_channel sb db) in
        let a = byte_of_float (sa +. da_f -. (sa *. da_f)) in
        if a = 255 then Rgb { r; g; b } else Rgba { r; g; b; a }
    | `Perceptual ->
        let perceptual_alpha alpha =
          if alpha >= 0.8 then
            let norm = (alpha -. 0.8) *. 5. in
            0.8 +. (Float.pow norm 0.2 *. 0.2)
          else Float.pow alpha 0.9
        in
        let sa_adj = perceptual_alpha sa in
        let blended_channel sc dc = (sa_adj *. sc) +. ((1. -. sa_adj) *. dc) in
        let r = byte_of_float (blended_channel sr dr) in
        let g = byte_of_float (blended_channel sg dg) in
        let b = byte_of_float (blended_channel sb db) in
        let a = byte_of_float (sa +. da_f -. (sa *. da_f)) in
        if a = 255 then Rgb { r; g; b } else Rgba { r; g; b; a }

let squared_distance (r1, g1, b1) (r2, g2, b2) =
  let dr = r1 - r2 in
  let dg = g1 - g2 in
  let db = b1 - b2 in
  (dr * dr) + (dg * dg) + (db * db)

let detected_level =
  lazy
    (match Sys.getenv_opt "COLORTERM" with
    | Some "truecolor" -> `Truecolor
    | _ -> (
        match Sys.getenv_opt "TERM" with
        | Some term
          when String.contains term '2' && String.contains term '5'
               && String.contains term '6' ->
            `Ansi256
        | _ -> `Ansi16))

let detect_level () = Lazy.force detected_level

let downgrade ?level color =
  let effective_level = Option.value level ~default:(detect_level ()) in
  match effective_level with
  | `Truecolor -> color
  | `Ansi256 | `Ansi16 ->
      let target_size = if effective_level = `Ansi256 then 256 else 16 in
      let r, g, b = to_rgb color in
      let min_dist = ref max_int in
      let nearest = ref 0 in
      for i = 0 to target_size - 1 do
        let pr, pg, pb = palette_rgb_int i in
        let dist = squared_distance (r, g, b) (pr, pg, pb) in
        if dist < !min_dist then (
          min_dist := dist;
          nearest := i)
      done;
      of_palette_index !nearest

let to_sgr_codes ~bg color =
  match color with
  | Default -> [ (if bg then 49 else 39) ]
  | _ ->
      (* For all explicit colors, emit truecolor 38;2/48;2 sequences *)
      let r, g, b, a = to_rgba color in
      if bg && a = 0 then
        (* Transparent background (alpha=0) should use terminal default background *)
        [ 49 ]
      else
        let base = if bg then 48 else 38 in
        [ base; 2; r; g; b ]

(* Zero-allocation integer writing to buffer *)
let rec add_int_digits buf n =
  if n >= 10 then (
    add_int_digits buf (n / 10);
    Buffer.add_char buf (Char.chr (48 + (n mod 10))))
  else Buffer.add_char buf (Char.chr (48 + n))

let add_int buf n =
  if n < 0 then (
    Buffer.add_char buf '-';
    add_int_digits buf (-n))
  else if n = 0 then Buffer.add_char buf '0'
  else add_int_digits buf n

(* Zero-allocation SGR code emission - writes directly to buffer *)
let emit_sgr_codes ~bg buf color =
  match color with
  | Default -> Buffer.add_string buf (if bg then "49" else "39")
  | _ ->
      (* For all explicit colors, emit truecolor 38;2/48;2 sequences *)
      let r, g, b, a = to_rgba color in
      if bg && a = 0 then
        (* Transparent background (alpha=0) should use terminal default background *)
        Buffer.add_string buf "49"
      else
        let base = if bg then 48 else 38 in
        add_int buf base;
        Buffer.add_char buf ';';
        Buffer.add_string buf "2";
        Buffer.add_char buf ';';
        add_int buf r;
        Buffer.add_char buf ';';
        add_int buf g;
        Buffer.add_char buf ';';
        add_int buf b

(* Emit SGR codes via push callback - zero allocation *)
let emit_sgr_codes_push ~bg push color =
  match color with
  | Default ->
      let code = if bg then 49 else 39 in
      push code
  | _ ->
      (* For all explicit colors, emit truecolor 38;2/48;2 sequences *)
      let r, g, b, a = to_rgba color in
      if bg && a = 0 then
        (* Transparent background (alpha=0) should use terminal default background *)
        push 49
      else
        let base = if bg then 48 else 38 in
        push base;
        push 2;
        push r;
        push g;
        push b

let invert color =
  let r, g, b = to_rgb color in
  of_rgb (255 - r) (255 - g) (255 - b)

module Packed = struct
  let tag_default = 0L
  let tag_basic = Int64.shift_left 1L 61
  let tag_extended = Int64.shift_left 2L 61
  let tag_rgb = Int64.shift_left 3L 61
  let tag_rgba = Int64.shift_left 4L 61
  let tag_mask = Int64.shift_left 7L 61 (* top 3 bits reserved for tags *)
  let data_mask = Int64.sub (Int64.shift_left 1L 61) 1L

  let encode color =
    match color with
    | Default -> tag_default
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    | Bright_black | Bright_red | Bright_green | Bright_yellow | Bright_blue
    | Bright_magenta | Bright_cyan | Bright_white ->
        let idx = palette_index color in
        Int64.logor tag_basic (Int64.of_int idx)
    | Extended idx ->
        Int64.logor tag_extended (Int64.of_int (clamp_extended idx))
    | Rgb { r; g; b } ->
        let data = (r lsl 16) lor (g lsl 8) lor b in
        Int64.logor tag_rgb (Int64.of_int data)
    | Rgba { r; g; b; a } ->
        let data = (r lsl 24) lor (g lsl 16) lor (b lsl 8) lor a in
        Int64.logor tag_rgba (Int64.of_int data)

  let decode packed =
    let tag = Int64.logand packed tag_mask in
    let data = Int64.logand packed data_mask |> Int64.to_int in
    match Int64.to_int (Int64.shift_right_logical tag 61) with
    | 0 -> Default
    | 1 -> of_palette_index data (* Basics map to 0-15 *)
    | 2 -> Extended data
    | 3 ->
        let r = (data lsr 16) land 0xFF in
        let g = (data lsr 8) land 0xFF in
        let b = data land 0xFF in
        Rgb { r; g; b }
    | 4 ->
        let r = (data lsr 24) land 0xFF in
        let g = (data lsr 16) land 0xFF in
        let b = (data lsr 8) land 0xFF in
        let a = data land 0xFF in
        Rgba { r; g; b; a }
    | _ -> Default
end

let pack = Packed.encode
let unpack = Packed.decode

let string_of_color = function
  | Default -> "Default"
  | Black -> "Black"
  | Red -> "Red"
  | Green -> "Green"
  | Yellow -> "Yellow"
  | Blue -> "Blue"
  | Magenta -> "Magenta"
  | Cyan -> "Cyan"
  | White -> "White"
  | Bright_black -> "Bright_black"
  | Bright_red -> "Bright_red"
  | Bright_green -> "Bright_green"
  | Bright_yellow -> "Bright_yellow"
  | Bright_blue -> "Bright_blue"
  | Bright_magenta -> "Bright_magenta"
  | Bright_cyan -> "Bright_cyan"
  | Bright_white -> "Bright_white"
  | Extended idx -> Printf.sprintf "Extended(%d)" idx
  | Rgb { r; g; b } -> Printf.sprintf "Rgb(%d,%d,%d)" r g b
  | Rgba { r; g; b; a } -> Printf.sprintf "Rgba(%d,%d,%d,%d)" r g b a

let pp fmt color = Format.pp_print_string fmt (string_of_color color)

let hex_value c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (10 + Char.code c - Char.code 'a')
  | 'A' .. 'F' -> Some (10 + Char.code c - Char.code 'A')
  | _ -> None

let parse_hex_component s start len =
  let rec aux acc idx remaining =
    if remaining = 0 then Some acc
    else
      match hex_value s.[idx] with
      | None -> None
      | Some v -> aux ((acc lsl 4) lor v) (idx + 1) (remaining - 1)
  in
  aux 0 start len

let expand_short_hex s =
  let len = String.length s in
  let buf = Bytes.create (len * 2) in
  for i = 0 to len - 1 do
    let c = s.[i] in
    Bytes.set buf (i * 2) c;
    Bytes.set buf ((i * 2) + 1) c
  done;
  Bytes.unsafe_to_string buf

let sanitize_hex s =
  let s =
    if String.length s > 0 && s.[0] = '#' then
      String.sub s 1 (String.length s - 1)
    else s
  in
  match String.length s with
  | 3 | 4 -> expand_short_hex s
  | 6 | 8 -> s
  | _ -> ""

let of_hex hex =
  let s = sanitize_hex hex in
  let len = String.length s in
  if len = 0 then None
  else if len = 6 then
    match
      ( parse_hex_component s 0 2,
        parse_hex_component s 2 2,
        parse_hex_component s 4 2 )
    with
    | Some r, Some g, Some b -> Some (of_rgb r g b)
    | _ -> None
  else if len = 8 then
    match
      ( parse_hex_component s 0 2,
        parse_hex_component s 2 2,
        parse_hex_component s 4 2,
        parse_hex_component s 6 2 )
    with
    | Some r, Some g, Some b, Some a -> Some (of_rgba r g b a)
    | _ -> None
  else None

let of_hex_exn hex =
  match of_hex hex with
  | Some color -> color
  | None -> invalid_arg "Color.of_hex_exn: invalid hex string"

let to_hex color =
  let r, g, b = to_rgb color in
  Printf.sprintf "#%02x%02x%02x" r g b
