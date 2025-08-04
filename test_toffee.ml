open Toffee

let test_basic_layout () =
  (* Create a new tree *)
  let tree = new_tree () in

  (* Create some nodes *)
  let root =
    match new_leaf tree Style.default with
    | Ok id -> id
    | Error e -> failwith (Error.to_string e)
  in

  let child1 =
    let style =
      Style.(
        default
        |> set_size
             Size.
               { width = Dimension.length 100.; height = Dimension.length 50. })
    in
    match new_leaf tree style with
    | Ok id -> id
    | Error e -> failwith (Error.to_string e)
  in

  let child2 =
    let style = Style.(default |> set_flex_grow 1.0) in
    match new_leaf tree style with
    | Ok id -> id
    | Error e -> failwith (Error.to_string e)
  in

  (* Build the tree structure *)
  (match add_child tree root child1 with
  | Ok () -> ()
  | Error e -> failwith (Error.to_string e));

  (match add_child tree root child2 with
  | Ok () -> ()
  | Error e -> failwith (Error.to_string e));

  (* Compute layout *)
  let available_space =
    Size.
      {
        width = Available_space.from_points 500.;
        height = Available_space.from_points 300.;
      }
  in

  (match compute_layout tree root available_space with
  | Ok () -> ()
  | Error e -> failwith (Error.to_string e));

  (* Get the computed layout *)
  match layout tree root with
  | Ok layout ->
      Printf.printf "Root size: %fx%f\n" layout.size.width layout.size.height
  | Error e -> failwith (Error.to_string e)

let () = test_basic_layout ()
