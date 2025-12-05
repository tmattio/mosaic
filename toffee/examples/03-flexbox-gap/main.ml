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
  let child_style =
    Style.make
      ~size:Size.{ width = Dimension.px 20.; height = Dimension.px 20. }
      ()
  in
  let child0 = new_leaf tree child_style |> or_fail in
  let child1 = new_leaf tree child_style |> or_fail in
  let child2 = new_leaf tree child_style |> or_fail in

  let gap =
    Size.{ width = Length_percentage.px 10.; height = Length_percentage.zero }
  in
  let root_style = Style.make ~gap () in
  let root =
    new_with_children tree root_style [| child0; child1; child2 |] |> or_fail
  in

  let max_content =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  compute_layout tree root max_content |> or_fail;
  print_tree tree root
