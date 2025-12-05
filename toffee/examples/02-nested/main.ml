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
  let leaf_style =
    Style.make
      ~size:Size.{ width = Dimension.px 5.; height = Dimension.px 5. }
      ()
  in
  let child_t1 = new_leaf tree leaf_style |> or_fail in
  let child_t2 = new_leaf tree leaf_style |> or_fail in

  let half_height =
    Size.{ width = Dimension.pct 50.; height = Dimension.pct 100. }
  in
  let div_style = Style.make ~size:half_height () in
  let div1 = new_with_children tree div_style [| child_t1 |] |> or_fail in
  let div2 = new_with_children tree div_style [| child_t2 |] |> or_fail in

  let container_style =
    Style.make
      ~size:Size.{ width = Dimension.pct 100.; height = Dimension.pct 100. }
      ()
  in
  let container =
    new_with_children tree container_style [| div1; div2 |] |> or_fail
  in

  let available =
    Size.
      {
        width = Available_space.of_length 100.;
        height = Available_space.of_length 100.;
      }
  in
  compute_layout tree container available |> or_fail;

  let print_node label node_id =
    let layout = layout tree node_id |> or_fail in
    Printf.printf "%s: %s\n%!" label (Layout.to_string layout)
  in

  print_endline "Computed layouts:";
  print_node "container" container;
  print_node "div1" div1;
  print_node "div2" div2;
  print_node "child1" child_t1;
  print_node "child2" child_t2
