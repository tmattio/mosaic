open Windtrap
open Matrix

let idx grid x y = (y * Grid.width grid) + x

let read_bg grid x y =
  let i = idx grid x y in
  Ansi.Color.to_rgba (Grid.get_bg grid i)

let text_width_method_is_applied_during_render () =
  let grid = Grid.create ~width:6 ~height:1 ~width_method:`Unicode () in
  let img = Image.text ~width_method:`Wcwidth "👩\u{200D}🚀" in

  Image.draw img grid;

  let start0 = Grid.get_cell grid (Grid.idx grid ~x:0 ~y:0) in
  let start2 = Grid.get_cell grid (Grid.idx grid ~x:2 ~y:0) in
  is_true ~msg:"first grapheme start written" (start0 <> Grid.Cell.space);
  is_true ~msg:"second grapheme start written at x=2" (start2 <> Grid.Cell.space);
  is_true ~msg:"render restores grid width method"
    (Grid.width_method grid = `Unicode)

let box_without_fill_preserves_background () =
  let grid = Grid.create ~width:4 ~height:3 () in
  let bg = Ansi.Color.of_rgb 5 15 25 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:4 ~height:3 ~color:bg;
  let border_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let img = Image.box ~border_style ~width:4 ~height:3 () in
  Image.draw img grid;
  let er, eg, eb, ea = Ansi.Color.to_rgba bg in
  equal ~msg:"border cell keeps existing bg"
    (pair int (pair int (pair int int)))
    (er, (eg, (eb, ea)))
    (let r, g, b, a = read_bg grid 0 0 in
     (r, (g, (b, a))))

let cell_written grid x y =
  Grid.get_cell grid (Grid.idx grid ~x ~y) <> Grid.Cell.space

let hsnap_defaults_to_center () =
  let grid = Grid.create ~width:3 ~height:1 () in
  let img = Image.hsnap 3 (Image.text "x") in
  Image.draw img grid;
  is_false ~msg:"left cell is padding" (cell_written grid 0 0);
  is_true ~msg:"content centered at x=1" (cell_written grid 1 0);
  is_false ~msg:"right cell is padding" (cell_written grid 2 0)

let vsnap_defaults_to_middle () =
  let grid = Grid.create ~width:1 ~height:3 () in
  let img = Image.vsnap 3 (Image.text "x") in
  Image.draw img grid;
  is_false ~msg:"top cell is padding" (cell_written grid 0 0);
  is_true ~msg:"content centered at y=1" (cell_written grid 0 1);
  is_false ~msg:"bottom cell is padding" (cell_written grid 0 2)

let fill_defaults_to_black () =
  let grid = Grid.create ~width:2 ~height:1 () in
  let img = Image.fill ~width:1 ~height:1 () in
  Image.draw img grid;
  let _, _, _, alpha = read_bg grid 0 0 in
  is_true ~msg:"filled cell has an opaque background" (alpha > 0);
  let _, _, _, untouched_alpha = read_bg grid 1 0 in
  equal ~msg:"unfilled cell keeps the terminal default" int 0 untouched_alpha

let () =
  Windtrap.run "matrix.image"
    [
      group "render"
        [
          test "text width_method is applied during render"
            text_width_method_is_applied_during_render;
          test "box without fill preserves background"
            box_without_fill_preserves_background;
        ];
      group "defaults"
        [
          test "hsnap defaults to `Center" hsnap_defaults_to_center;
          test "vsnap defaults to `Middle" vsnap_defaults_to_middle;
          test "fill defaults to black" fill_defaults_to_black;
        ];
    ]
