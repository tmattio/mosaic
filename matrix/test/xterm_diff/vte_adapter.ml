module Grid = Matrix_grid

type t = Vte.t

let create ~rows ~cols =
  Vte.create ~scrollback:0 ~default_fg:Ansi.Color.default_fg
    ~default_bg:Ansi.Color.default_bg ~rows ~cols ()

let feed t s =
  Vte.feed_string t s;
  Vte.take_replies t

let color c =
  match Ansi.Color.intent c with
  | Ansi.Color.Default -> Screen.Default
  | Indexed i -> Index i
  | Rgb ->
      let r, g, b, a = Ansi.Color.to_rgba c in
      if a = 0 then Default else Rgb (r, g, b)

let attrs packed =
  let a = Ansi.Attr.unpack packed in
  List.fold_left
    (fun acc (flag, bit) -> if Ansi.Attr.mem flag a then acc lor bit else acc)
    0
    Ansi.Attr.
      [
        (Bold, Screen.bold);
        (Dim, Screen.faint);
        (Italic, Screen.italic);
        (Underline, Screen.underline);
        (Double_underline, Screen.dbl_under);
        (Blink, Screen.blink);
        (Inverse, Screen.inverse);
        (Hidden, Screen.invisible);
        (Strikethrough, Screen.strike);
      ]

let screen t =
  let g = Vte.grid t in
  let rows = Grid.height g and cols = Grid.width g in
  let cell idx =
    let cont = Grid.is_continuation g idx in
    let text = Grid.get_text g idx in
    {
      Screen.text = (if cont then "" else if text = "" then " " else text);
      cont;
      attrs = attrs (Grid.get_attrs g idx);
      fg = color (Grid.get_fg g idx);
      bg = color (Grid.get_bg g idx);
      known = true;
    }
  in
  {
    Screen.lines =
      Array.init rows (fun y ->
          Array.init cols (fun x -> cell (Grid.idx g ~x ~y)));
    line_attrs = Array.make rows '5';
  }
