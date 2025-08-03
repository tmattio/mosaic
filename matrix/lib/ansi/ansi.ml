include Internal
module Parser = Parser
module Style = Style

(* Re-export types from Style to match the interface *)
type color = Style.color =
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
  | Index of int
  | RGB of int * int * int
  | RGBA of int * int * int * int

(* Pretty-printing *)

let pp_attr ppf = function
  | `Reset -> Fmt.string ppf "`Reset"
  | `Fg color -> Fmt.pf ppf "`Fg %a" Style.pp_color color
  | `Bg color -> Fmt.pf ppf "`Bg %a" Style.pp_color color
  | `No_bold -> Fmt.string ppf "`No_bold"
  | `No_dim -> Fmt.string ppf "`No_dim"
  | `No_italic -> Fmt.string ppf "`No_italic"
  | `No_underline -> Fmt.string ppf "`No_underline"
  | `No_blink -> Fmt.string ppf "`No_blink"
  | `No_reverse -> Fmt.string ppf "`No_reverse"
  | `No_conceal -> Fmt.string ppf "`No_conceal"
  | `No_strikethrough -> Fmt.string ppf "`No_strikethrough"
  | `No_overline -> Fmt.string ppf "`No_overline"
  | `No_framed -> Fmt.string ppf "`No_framed"
  | `No_encircled -> Fmt.string ppf "`No_encircled"
  | `Bold -> Fmt.string ppf "`Bold"
  | `Dim -> Fmt.string ppf "`Dim"
  | `Italic -> Fmt.string ppf "`Italic"
  | `Underline -> Fmt.string ppf "`Underline"
  | `Double_underline -> Fmt.string ppf "`Double_underline"
  | `Blink -> Fmt.string ppf "`Blink"
  | `Reverse -> Fmt.string ppf "`Reverse"
  | `Conceal -> Fmt.string ppf "`Conceal"
  | `Strikethrough -> Fmt.string ppf "`Strikethrough"
  | `Overline -> Fmt.string ppf "`Overline"
  | `Framed -> Fmt.string ppf "`Framed"
  | `Encircled -> Fmt.string ppf "`Encircled"

let pp_attrs = Fmt.(list ~sep:(any "; ") pp_attr)

(* Equality functions *)

let equal_color c1 c2 =
  match (c1, c2) with
  | Black, Black
  | Red, Red
  | Green, Green
  | Yellow, Yellow
  | Blue, Blue
  | Magenta, Magenta
  | Cyan, Cyan
  | White, White
  | Default, Default
  | Bright_black, Bright_black
  | Bright_red, Bright_red
  | Bright_green, Bright_green
  | Bright_yellow, Bright_yellow
  | Bright_blue, Bright_blue
  | Bright_magenta, Bright_magenta
  | Bright_cyan, Bright_cyan
  | Bright_white, Bright_white ->
      true
  | Index i1, Index i2 -> i1 = i2
  | RGB (r1, g1, b1), RGB (r2, g2, b2) -> r1 = r2 && g1 = g2 && b1 = b2
  | _ -> false

let equal_attr a1 a2 =
  match (a1, a2) with
  | `Reset, `Reset -> true
  | `Fg c1, `Fg c2 -> equal_color c1 c2
  | `Bg c1, `Bg c2 -> equal_color c1 c2
  | `No_bold, `No_bold
  | `No_dim, `No_dim
  | `No_italic, `No_italic
  | `No_underline, `No_underline
  | `No_blink, `No_blink
  | `No_reverse, `No_reverse
  | `No_conceal, `No_conceal
  | `No_strikethrough, `No_strikethrough
  | `No_overline, `No_overline
  | `No_framed, `No_framed
  | `No_encircled, `No_encircled ->
      true
  | `Bold, `Bold
  | `Dim, `Dim
  | `Italic, `Italic
  | `Underline, `Underline
  | `Double_underline, `Double_underline
  | `Blink, `Blink
  | `Reverse, `Reverse
  | `Conceal, `Conceal
  | `Strikethrough, `Strikethrough
  | `Overline, `Overline
  | `Framed, `Framed
  | `Encircled, `Encircled ->
      true
  | _ -> false
