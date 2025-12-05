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
      ~size:Size.{ width = Dimension.pct 50.; height = Dimension.auto }
      ()
  in
  let child = new_leaf tree child_style |> or_fail in

  let node_style =
    Style.make
      ~size:Size.{ width = Dimension.px 100.; height = Dimension.px 100. }
      ~justify_content:Align_content.Center ()
  in
  let node = new_with_children tree node_style [| child |] |> or_fail in

  let definite_100 =
    Size.
      {
        width = Available_space.of_length 100.;
        height = Available_space.of_length 100.;
      }
  in
  Printf.printf "Compute layout with 100x100 viewport:\n%!";
  compute_layout tree node definite_100 |> or_fail;
  let node_layout = layout tree node |> or_fail in
  let child_layout = layout tree child |> or_fail in
  Printf.printf "node: %s\n%!" (Layout.to_string node_layout);
  Printf.printf "child: %s\n%!" (Layout.to_string child_layout);

  let max_content =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  Printf.printf "\nCompute layout with max-content viewport:\n%!";
  compute_layout tree node max_content |> or_fail;
  let node_layout = layout tree node |> or_fail in
  let child_layout = layout tree child |> or_fail in
  Printf.printf "node: %s\n%!" (Layout.to_string node_layout);
  Printf.printf "child: %s\n%!" (Layout.to_string child_layout)
