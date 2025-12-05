open Toffee

let or_fail = function
  | Ok v -> v
  | Error e ->
      prerr_endline (Error.to_string e);
      Stdlib.exit 1

let () =
  let tree = new_tree () in
  let open Geometry in
  let open Style in
  let root_style =
    Style.make ~display:Display.Grid
      ~size:Size.{ width = Dimension.px 800.; height = Dimension.px 600. }
      ~grid_template_columns:
        [
          Grid_template_component.length 250.;
          Grid_template_component.fr 1.;
          Grid_template_component.length 250.;
        ]
      ~grid_template_rows:
        [
          Grid_template_component.length 150.;
          Grid_template_component.fr 1.;
          Grid_template_component.length 150.;
        ]
      ()
  in

  let line index =
    Line.{ start = Grid_placement.line index; end_ = Grid_placement.auto }
  in
  let span count =
    Line.{ start = Grid_placement.span count; end_ = Grid_placement.auto }
  in

  let header_style = Style.make ~grid_row:(line 1) ~grid_column:(span 3) () in
  let left_sidebar_style =
    Style.make ~grid_row:(line 2) ~grid_column:(line 1) ()
  in
  let content_style = Style.make ~grid_row:(line 2) ~grid_column:(line 2) () in
  let right_sidebar_style =
    Style.make ~grid_row:(line 2) ~grid_column:(line 3) ()
  in
  let footer_style = Style.make ~grid_row:(line 3) ~grid_column:(span 3) () in

  let header = new_leaf tree header_style |> or_fail in
  let left_sidebar = new_leaf tree left_sidebar_style |> or_fail in
  let content_area = new_leaf tree content_style |> or_fail in
  let right_sidebar = new_leaf tree right_sidebar_style |> or_fail in
  let footer = new_leaf tree footer_style |> or_fail in

  let root =
    new_with_children tree root_style
      [| header; left_sidebar; content_area; right_sidebar; footer |]
    |> or_fail
  in

  let available =
    Size.
      {
        width = Available_space.of_length 800.;
        height = Available_space.of_length 600.;
      }
  in
  compute_layout tree root available |> or_fail;
  print_tree tree root
