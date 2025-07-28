open Ansi

type style = {
  bold : bool;
  faint : bool; (* Corresponds to Ansi.style.`Dim` *)
  italic : bool;
  underline : bool;
  double_underline : bool;
  fg : Ansi.color;
  bg : Ansi.color;
  reversed : bool;
  link : string option;
  strikethrough : bool;
  overline : bool;
  blink : bool;
}

type attr = style
type t = { glyph : string; width : int; attrs : attr }

let default_style =
  {
    bold = false;
    faint = false;
    italic = false;
    underline = false;
    double_underline = false;
    fg = Default;
    bg = Default;
    reversed = false;
    link = None;
    strikethrough = false;
    overline = false;
    blink = false;
  }

let empty = None

let apply_sgr_attr state (attr : Ansi.attr) =
  match attr with
  | `Reset -> default_style
  | `Bold -> { state with bold = true }
  | `Dim -> { state with faint = true }
  | `Italic -> { state with italic = true }
  | `Underline -> { state with underline = true }
  | `Double_underline -> { state with double_underline = true }
  | `Reverse -> { state with reversed = true }
  | `Fg fg -> { state with fg }
  | `Bg bg -> { state with bg }
  | `Blink -> { state with blink = true }
  | `Strikethrough -> { state with strikethrough = true }
  | `Overline -> { state with overline = true }
  | `Conceal | `Framed | `Encircled -> state (* Not visually implemented *)
