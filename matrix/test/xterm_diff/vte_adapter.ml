module Grid = Matrix_grid

type t = Vte.t

let create ?(width_method = `Unicode) ~rows ~cols () =
  Vte.create ~scrollback:0 ~width_method ~default_fg:Ansi.Color.default_fg
    ~default_bg:Ansi.Color.default_bg ~rows ~cols ()

(* The replies' bytes, compared with xterm's. *)
let feed t s = Matrix_input.Reply.encode (Vte.feed_string t s)

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

let cell_at g idx =
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

let cell t ~row ~col =
  let g = Vte.grid t in
  cell_at g (Grid.idx g ~x:col ~y:row)

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

type cursor = {
  row : int;
  col : int;
  last_column : bool;
  auto_wrap : bool;
  reverse_wrap : bool;
  insert : bool;
  rows : int;
  cols : int;
  alternate : bool;
}

let cursor t =
  let row, col = Vte.cursor_pos t in
  {
    row;
    col;
    last_column = Vte.last_column t;
    auto_wrap = (Vte.modes t).auto_wrap;
    reverse_wrap = (Vte.modes t).reverse_wrap;
    insert = (Vte.modes t).insert;
    rows = Vte.rows t;
    cols = Vte.cols t;
    alternate = (Vte.modes t).alternate_screen;
  }

let protected t ~row ~col =
  let g = Vte.grid t in
  Grid.is_protected g (Grid.idx g ~x:col ~y:row)

let tag ?protected t ~text cells =
  let g = Vte.grid t in
  List.iter
    (fun (row, col) ->
      (* With the cell's protection unless told, so that selective erases
         treat it as they treat the cell. *)
      let protected =
        match protected with
        | Some p -> p
        | None -> Grid.is_protected g (Grid.idx g ~x:col ~y:row)
      in
      Grid.set_grapheme g ~x:col ~y:row ~width:1 ~fg:Ansi.Color.default_fg
        ~bg:Ansi.Color.default_bg ~attrs:Ansi.Attr.empty ~protected text ~pos:0
        ~len:(String.length text))
    cells

let tagged t ~text =
  let g = Vte.grid t in
  List.concat
    (List.init (Grid.height g) (fun y ->
         List.filter_map
           (fun x ->
             if
               String.starts_with ~prefix:text
                 (Grid.get_text g (Grid.idx g ~x ~y))
             then Some (y, x)
             else None)
           (List.init (Grid.width g) Fun.id)))

let modes t = Vte.modes t
