(* Basic tests for the toffee layout library - Updated API *)

open Toffee
open Geometry
open Windtrap

let test_helpers =
  (* Helper to check float equality with epsilon *)
  let float_eq ?(epsilon = 0.001) a b = Float.abs (a -. b) < epsilon in

  (* Helper to print layout for debugging *)
  let print_layout layout =
    Printf.printf "Layout { location: (%.2f, %.2f), size: (%.2f x %.2f) }\n"
      (Layout.location layout).x (Layout.location layout).y
      (Layout.size layout).width (Layout.size layout).height
  in

  (float_eq, print_layout)

let test_create_tree () =
  let tree = new_tree () in
  match new_leaf tree Style.default with
  | Ok _node -> is_true ~msg:"Node created" true
  | Error e -> failf "Failed to create node: %s" (Error.to_string e)

let test_simple_leaf_layout () =
  let tree = new_tree () in
  let style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 100.0;
             height = Style.Dimension.length 50.0;
           }
  in
  match new_leaf tree style with
  | Error e -> failf "Failed to create node: %s" (Error.to_string e)
  | Ok node -> (
      let result =
        compute_layout tree node
          Size.
            {
              width = Available_space.of_float 500.0;
              height = Available_space.of_float 500.0;
            }
      in

      match result with
      | Ok () -> (
          let layout_result = layout tree node in
          match layout_result with
          | Ok layout ->
              let float_eq, print_layout = test_helpers in
              print_layout layout;
              let size = Layout.size layout in
              is_true ~msg:"Width is 100" (float_eq size.width 100.0);
              is_true ~msg:"Height is 50" (float_eq size.height 50.0)
          | Error _ -> fail "Failed to get layout")
      | Error _ -> fail "Failed to compute layout")

let test_flexbox_row () =
  let tree = new_tree () in

  (* Create container with flex row *)
  let container_style =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_flex_direction Style.Flex_direction.Row
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 300.0;
             height = Style.Dimension.length 100.0;
           }
  in

  (* Create child nodes *)
  let child_style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 50.0;
             height = Style.Dimension.length 50.0;
           }
  in

  match new_leaf tree child_style with
  | Error e -> failf "Failed to create child1: %s" (Error.to_string e)
  | Ok child1 -> (
      match new_leaf tree child_style with
      | Error e -> failf "Failed to create child2: %s" (Error.to_string e)
      | Ok child2 -> (
          match new_with_children tree container_style [| child1; child2 |] with
          | Error e ->
              failf "Failed to create container: %s" (Error.to_string e)
          | Ok container -> (
              let result =
                compute_layout tree container
                  Size.
                    {
                      width = Available_space.of_float 500.0;
                      height = Available_space.of_float 500.0;
                    }
              in

              match result with
              | Ok () -> (
                  (* Check container layout *)
                  (match layout tree container with
                  | Ok layout ->
                      let float_eq, _ = test_helpers in
                      let size = Layout.size layout in
                      Printf.printf "Container layout: size=(%.1f x %.1f)\n"
                        size.width size.height;
                      is_true ~msg:"Container width" (float_eq size.width 300.0);
                      is_true ~msg:"Container height"
                        (float_eq size.height 100.0)
                  | Error _ -> fail "Failed to get container layout");

                  (* Check child layouts *)
                  (match layout tree child1 with
                  | Ok layout ->
                      let float_eq, _ = test_helpers in
                      let location = Layout.location layout in
                      let size = Layout.size layout in
                      Printf.printf
                        "Child1 layout: location=(%.1f, %.1f), size=(%.1f x \
                         %.1f)\n"
                        location.x location.y size.width size.height;
                      is_true ~msg:"Child1 x position" (float_eq location.x 0.0);
                      is_true ~msg:"Child1 width" (float_eq size.width 50.0)
                  | Error _ -> fail "Failed to get child1 layout");

                  match layout tree child2 with
                  | Ok layout ->
                      let float_eq, _ = test_helpers in
                      let location = Layout.location layout in
                      let size = Layout.size layout in
                      Printf.printf
                        "Child2 layout: location=(%.1f, %.1f), size=(%.1f x \
                         %.1f)\n"
                        location.x location.y size.width size.height;
                      is_true ~msg:"Child2 x position"
                        (float_eq location.x 50.0);
                      is_true ~msg:"Child2 width" (float_eq size.width 50.0)
                  | Error _ -> fail "Failed to get child2 layout")
              | Error _ -> fail "Failed to compute layout")))

let test_flexbox_column () =
  let tree = new_tree () in

  (* Create container with flex column *)
  let container_style =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_flex_direction Style.Flex_direction.Column
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 100.0;
             height = Style.Dimension.length 300.0;
           }
  in

  (* Create child nodes *)
  let child_style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 50.0;
             height = Style.Dimension.length 50.0;
           }
  in

  match new_leaf tree child_style with
  | Error e -> failf "Failed to create child1: %s" (Error.to_string e)
  | Ok child1 -> (
      match new_leaf tree child_style with
      | Error e -> failf "Failed to create child2: %s" (Error.to_string e)
      | Ok child2 -> (
          match new_with_children tree container_style [| child1; child2 |] with
          | Error e ->
              failf "Failed to create container: %s" (Error.to_string e)
          | Ok container -> (
              let result =
                compute_layout tree container
                  Size.
                    {
                      width = Available_space.of_float 500.0;
                      height = Available_space.of_float 500.0;
                    }
              in

              match result with
              | Ok () -> (
                  (* Check container layout *)
                  (match layout tree container with
                  | Ok layout ->
                      let size = Layout.size layout in
                      Printf.printf
                        "Column container layout: size=(%.1f x %.1f)\n"
                        size.width size.height
                  | Error _ -> ());

                  (* Check child layouts *)
                  (match layout tree child1 with
                  | Ok layout ->
                      let float_eq, _ = test_helpers in
                      let location = Layout.location layout in
                      let size = Layout.size layout in
                      Printf.printf
                        "Column child1 layout: location=(%.1f, %.1f), \
                         size=(%.1f x %.1f)\n"
                        location.x location.y size.width size.height;
                      is_true ~msg:"Child1 y position" (float_eq location.y 0.0)
                  | Error _ -> fail "Failed to get child1 layout");

                  match layout tree child2 with
                  | Ok layout ->
                      let float_eq, _ = test_helpers in
                      let location = Layout.location layout in
                      let size = Layout.size layout in
                      Printf.printf
                        "Column child2 layout: location=(%.1f, %.1f), \
                         size=(%.1f x %.1f)\n"
                        location.x location.y size.width size.height;
                      is_true ~msg:"Child2 y position"
                        (float_eq location.y 50.0)
                  | Error _ -> fail "Failed to get child2 layout")
              | Error _ -> fail "Failed to compute layout")))

let test_padding () =
  let tree = new_tree () in

  let style =
    Style.(
      default
      |> set_size
           Size.
             { width = Dimension.length 100.0; height = Dimension.length 100.0 }
      |> set_padding (Rect.all (Length_percentage.length 10.0)))
  in

  match new_leaf tree style with
  | Error e -> failf "Failed to create node: %s" (Error.to_string e)
  | Ok node -> (
      let result =
        compute_layout tree node
          Size.
            {
              width = Available_space.of_float 500.0;
              height = Available_space.of_float 500.0;
            }
      in

      match result with
      | Ok () -> (
          match layout tree node with
          | Ok layout ->
              let float_eq, _ = test_helpers in
              let size = Layout.size layout in
              (* With padding, the size should remain 100x100 for content-box
                 sizing *)
              is_true ~msg:"Width with padding" (float_eq size.width 100.0);
              is_true ~msg:"Height with padding" (float_eq size.height 100.0)
          | Error _ -> fail "Failed to get layout")
      | Error _ -> fail "Failed to compute layout")

let test_margin () =
  let tree = new_tree () in

  (* Create parent container *)
  let parent_style =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 200.0;
             height = Style.Dimension.length 200.0;
           }
  in

  (* Create child with margin *)
  let child_style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 50.0;
             height = Style.Dimension.length 50.0;
           }
    |> Style.set_margin (Rect.all (Style.Length_percentage_auto.length 10.0))
  in

  match new_leaf tree child_style with
  | Error e -> failf "Failed to create child: %s" (Error.to_string e)
  | Ok child -> (
      match new_with_children tree parent_style [| child |] with
      | Error e -> failf "Failed to create parent: %s" (Error.to_string e)
      | Ok parent -> (
          let result =
            compute_layout tree parent
              Size.
                {
                  width = Available_space.of_float 500.0;
                  height = Available_space.of_float 500.0;
                }
          in

          match result with
          | Ok () -> (
              match layout tree child with
              | Ok layout ->
                  let float_eq, print_layout = test_helpers in
                  print_layout layout;
                  let location = Layout.location layout in
                  (* Child should be positioned at (10, 10) due to margin *)
                  is_true ~msg:"Child x with margin" (float_eq location.x 10.0);
                  is_true ~msg:"Child y with margin" (float_eq location.y 10.0)
              | Error _ -> fail "Failed to get child layout")
          | Error _ -> fail "Failed to compute layout"))

let test_flex_grow () =
  let tree = new_tree () in

  let container_style =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_flex_direction Style.Flex_direction.Row
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 200.0;
             height = Style.Dimension.length 100.0;
           }
  in

  (* Create child with flex_grow *)
  let child_style =
    Style.default |> Style.set_flex_grow 1.0
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.auto;
             height = Style.Dimension.length 50.0;
           }
  in

  Printf.printf "Creating child with flex_grow=%.1f\n" 1.0;
  match new_leaf tree child_style with
  | Error e -> failf "Failed to create child: %s" (Error.to_string e)
  | Ok child -> (
      match new_with_children tree container_style [| child |] with
      | Error e -> failf "Failed to create container: %s" (Error.to_string e)
      | Ok container -> (
          let result =
            compute_layout tree container
              Size.
                {
                  width = Available_space.of_float 500.0;
                  height = Available_space.of_float 500.0;
                }
          in

          match result with
          | Ok () -> (
              match layout tree child with
              | Ok layout ->
                  let float_eq, print_layout = test_helpers in
                  print_layout layout;
                  let size = Layout.size layout in
                  Printf.printf
                    "Flex grow test: child size=(%.1f x %.1f), expecting \
                     width=200.0\n"
                    size.width size.height;
                  (* Child should grow to fill container width *)
                  is_true ~msg:"Child width with flex_grow"
                    (float_eq size.width 200.0)
              | Error _ -> fail "Failed to get child layout")
          | Error _ -> fail "Failed to compute layout"))

let test_aspect_ratio () =
  let tree = new_tree () in

  let style =
    Style.default
    |> Style.set_aspect_ratio (Some 2.0) (* width / height = 2 *)
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 100.0;
             height = Style.Dimension.auto;
           }
  in

  match new_leaf tree style with
  | Error e -> failf "Failed to create node: %s" (Error.to_string e)
  | Ok node -> (
      let result =
        compute_layout tree node
          Size.
            {
              width = Available_space.of_float 500.0;
              height = Available_space.of_float 500.0;
            }
      in

      match result with
      | Ok () -> (
          match layout tree node with
          | Ok layout ->
              let float_eq, print_layout = test_helpers in
              print_layout layout;
              let size = Layout.size layout in
              (* Height should be width / aspect_ratio = 100 / 2 = 50 *)
              is_true ~msg:"Width" (float_eq size.width 100.0);
              is_true ~msg:"Height from aspect ratio"
                (float_eq size.height 50.0)
          | Error _ -> fail "Failed to get layout")
      | Error _ -> fail "Failed to compute layout")

(* Manual ports of selected taffy tests *)

type measure_ctx = { mutable count : int }

let or_fail msg = function
  | Ok v -> v
  | Error e -> failf "%s: %s" msg (Error.to_string e)

let compute_or_fail tree node available msg =
  match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "%s: %s" msg (Error.to_string e)

let test_measure_count_flexbox () =
  let tree = new_tree () in
  let ctx = { count = 0 } in
  let leaf =
    new_leaf_with_context tree Style.default ctx
    |> or_fail "Failed to create measured leaf"
  in
  let rec nest depth node =
    if depth = 0 then node
    else
      new_with_children tree Style.default [| node |]
      |> or_fail "Failed to create parent"
      |> nest (depth - 1)
  in
  let root = nest 100 leaf in
  let measure_function known_dimensions _available_space _node context _style =
    (match context with
    | Some data -> data.count <- data.count + 1
    | None -> ());
    match known_dimensions with
    | Size.{ width = Some width; height = Some height } ->
        Size.{ width; height }
    | _ -> Size.{ width = 50.0; height = 50.0 }
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  (match compute_layout_with_measure tree root available measure_function with
  | Ok () -> ()
  | Error e ->
      failf "Failed to compute layout with measure: %s" (Error.to_string e));
  match get_node_context tree leaf with
  | None -> fail "Missing node context after layout"
  | Some data -> equal ~msg:"Measure function call count" int 4 data.count

let make_size width height =
  Size.
    {
      width = Style.Dimension.length width;
      height = Style.Dimension.length height;
    }

let definite available = Available_space.of_float available

let test_min_overrides_max () =
  let tree = new_tree () in
  let style =
    Style.default
    |> Style.set_size (make_size 50.0 50.0)
    |> Style.set_min_size (make_size 100.0 100.0)
    |> Style.set_max_size (make_size 10.0 10.0)
  in
  let node = new_leaf tree style |> or_fail "Failed to create node" in
  let available = Size.{ width = definite 100.0; height = definite 100.0 } in
  (match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree node) in
  equal ~msg:"Width clamped to min" (float 0.001) 100.0
    (Layout.size layout).width;
  equal ~msg:"Height clamped to min" (float 0.001) 100.0
    (Layout.size layout).height

let test_max_overrides_size () =
  let tree = new_tree () in
  let style =
    Style.default
    |> Style.set_size (make_size 50.0 50.0)
    |> Style.set_max_size (make_size 10.0 10.0)
  in
  let node = new_leaf tree style |> or_fail "Failed to create node" in
  let available = Size.{ width = definite 100.0; height = definite 100.0 } in
  (match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree node) in
  equal ~msg:"Width clamped to max" (float 0.001) 10.0
    (Layout.size layout).width;
  equal ~msg:"Height clamped to max" (float 0.001) 10.0
    (Layout.size layout).height

let test_min_overrides_size () =
  let tree = new_tree () in
  let style =
    Style.default
    |> Style.set_size (make_size 50.0 50.0)
    |> Style.set_min_size (make_size 100.0 100.0)
  in
  let node = new_leaf tree style |> or_fail "Failed to create node" in
  let available = Size.{ width = definite 100.0; height = definite 100.0 } in
  (match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree node) in
  equal ~msg:"Width from min" (float 0.001) 100.0 (Layout.size layout).width;
  equal ~msg:"Height from min" (float 0.001) 100.0 (Layout.size layout).height

let test_root_with_percentage_size () =
  let tree = new_tree () in
  let style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.percent 1.0;
             height = Style.Dimension.percent 1.0;
           }
  in
  let node = new_leaf tree style |> or_fail "Failed to create node" in
  let available = Size.{ width = definite 100.0; height = definite 200.0 } in
  (match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree node) in
  equal ~msg:"Percent width uses available space" (float 0.001) 100.0
    (Layout.size layout).width;
  equal ~msg:"Percent height uses available space" (float 0.001) 200.0
    (Layout.size layout).height

let test_root_with_no_size () =
  let tree = new_tree () in
  let node = new_leaf tree Style.default |> or_fail "Failed to create node" in
  let available = Size.{ width = definite 100.0; height = definite 100.0 } in
  (match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree node) in
  equal ~msg:"Default width is zero" (float 0.001) 0.0
    (Layout.size layout).width;
  equal ~msg:"Default height is zero" (float 0.001) 0.0
    (Layout.size layout).height

let test_root_with_larger_size () =
  let tree = new_tree () in
  let style = Style.default |> Style.set_size (make_size 200.0 200.0) in
  let node = new_leaf tree style |> or_fail "Failed to create node" in
  let available = Size.{ width = definite 100.0; height = definite 100.0 } in
  (match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree node) in
  equal ~msg:"Width keeps explicit size" (float 0.001) 200.0
    (Layout.size layout).width;
  equal ~msg:"Height keeps explicit size" (float 0.001) 200.0
    (Layout.size layout).height

let test_root_padding_and_border_larger_than_definite_size () =
  let tree = new_tree () in
  let child = new_leaf tree Style.default |> or_fail "Failed to create child" in
  let padding = Rect.all (Style.Length_percentage.length 10.0) in
  let border = Rect.all (Style.Length_percentage.length 10.0) in
  let style =
    Style.default
    |> Style.set_size (make_size 10.0 10.0)
    |> Style.set_padding padding |> Style.set_border border
  in
  let root =
    new_with_children tree style [| child |] |> or_fail "Failed to create root"
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  (match compute_layout tree root available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree root) in
  equal ~msg:"Width accounts for padding and border" (float 0.001) 40.0
    (Layout.size layout).width;
  equal ~msg:"Height accounts for padding and border" (float 0.001) 40.0
    (Layout.size layout).height

let test_rounding_doesnt_leave_gaps () =
  let tree = new_tree () in
  let square = Style.default |> Style.set_size (make_size 100.3 100.3) in
  let child_a = new_leaf tree square |> or_fail "Failed to create child" in
  let child_b = new_leaf tree square |> or_fail "Failed to create child" in
  let root_style =
    Style.default
    |> Style.set_size (make_size 963.3333 1000.0)
    |> Style.set_justify_content (Some Style.Align_content.Center)
  in
  let root =
    new_with_children tree root_style [| child_a; child_b |]
    |> or_fail "Failed to create root"
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  (match compute_layout tree root available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout_a = Result.get_ok (layout tree child_a) in
  let layout_b = Result.get_ok (layout tree child_b) in
  let end_of_a = (Layout.location layout_a).x +. (Layout.size layout_a).width in
  equal ~msg:"Children abut after rounding" (float 0.001) end_of_a
    (Layout.location layout_b).x

let test_toggle_root_display_none () =
  let tree = new_tree () in
  let hidden =
    Style.default
    |> Style.set_display Style.Display.None
    |> Style.set_size (make_size 100.0 100.0)
  in
  let flex =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_size (make_size 100.0 100.0)
  in
  let node = new_leaf tree hidden |> or_fail "Failed to create node" in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  compute_or_fail tree node available "Compute hidden layout";
  let layout_none = Result.get_ok (layout tree node) in
  equal ~msg:"Hidden width" (float 0.001) 0.0 (Layout.size layout_none).width;
  equal ~msg:"Hidden height" (float 0.001) 0.0 (Layout.size layout_none).height;
  set_style tree node flex |> or_fail "set_style flex";
  compute_or_fail tree node available "Compute flex layout";
  let layout_flex = Result.get_ok (layout tree node) in
  equal ~msg:"Flex width" (float 0.001) 100.0 (Layout.size layout_flex).width;
  equal ~msg:"Flex height" (float 0.001) 100.0 (Layout.size layout_flex).height;
  set_style tree node hidden |> or_fail "set_style hidden";
  compute_or_fail tree node available "Compute hidden layout again";
  let layout_hidden = Result.get_ok (layout tree node) in
  equal ~msg:"Hidden width again" (float 0.001) 0.0
    (Layout.size layout_hidden).width;
  equal ~msg:"Hidden height again" (float 0.001) 0.0
    (Layout.size layout_hidden).height

let test_toggle_root_display_none_with_children () =
  let tree = new_tree () in
  let child_style = Style.default |> Style.set_size (make_size 800.0 100.0) in
  let child = new_leaf tree child_style |> or_fail "Failed to create child" in
  let parent_style = Style.default |> Style.set_size (make_size 800.0 100.0) in
  let parent =
    new_with_children tree parent_style [| child |]
    |> or_fail "Failed to create parent"
  in
  let root =
    new_with_children tree Style.default [| parent |] |> or_fail "Root"
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  compute_or_fail tree root available "Compute layout visible root";
  let layout_child = Result.get_ok (layout tree child) in
  equal ~msg:"Child width" (float 0.001) 800.0 (Layout.size layout_child).width;
  equal ~msg:"Child height" (float 0.001) 100.0
    (Layout.size layout_child).height;
  let hidden_root = Style.set_display Style.Display.None Style.default in
  set_style tree root hidden_root |> or_fail "set_style hidden root";
  compute_or_fail tree root available "Compute layout hidden root";
  let hidden_child = Result.get_ok (layout tree child) in
  equal ~msg:"Hidden child width" (float 0.001) 0.0
    (Layout.size hidden_child).width;
  equal ~msg:"Hidden child height" (float 0.001) 0.0
    (Layout.size hidden_child).height;
  set_style tree root Style.default |> or_fail "set_style default root";
  compute_or_fail tree root available "Compute layout restored root";
  let restored_child = Result.get_ok (layout tree child) in
  equal ~msg:"Restored child width" (float 0.001) 800.0
    (Layout.size restored_child).width;
  equal ~msg:"Restored child height" (float 0.001) 100.0
    (Layout.size restored_child).height

let test_toggle_flex_child_display_none () =
  let tree = new_tree () in
  let hidden =
    Style.default
    |> Style.set_display Style.Display.None
    |> Style.set_size (make_size 100.0 100.0)
  in
  let flex =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_size (make_size 100.0 100.0)
  in
  let node = new_leaf tree hidden |> or_fail "Failed to create child" in
  let root =
    new_with_children tree flex [| node |] |> or_fail "Failed to create root"
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  compute_or_fail tree root available "Compute layout hidden child";
  let layout_hidden = Result.get_ok (layout tree node) in
  equal ~msg:"Hidden width" (float 0.001) 0.0 (Layout.size layout_hidden).width;
  equal ~msg:"Hidden height" (float 0.001) 0.0
    (Layout.size layout_hidden).height;
  set_style tree node flex |> or_fail "set_style flex child";
  compute_or_fail tree root available "Compute layout flex child";
  let layout_flex = Result.get_ok (layout tree node) in
  equal ~msg:"Flex width" (float 0.001) 100.0 (Layout.size layout_flex).width;
  equal ~msg:"Flex height" (float 0.001) 100.0 (Layout.size layout_flex).height;
  set_style tree node hidden |> or_fail "set_style hidden child";
  compute_or_fail tree root available "Compute layout child hidden again";
  let layout_hidden_again = Result.get_ok (layout tree node) in
  equal ~msg:"Hidden width again" (float 0.001) 0.0
    (Layout.size layout_hidden_again).width;
  equal ~msg:"Hidden height again" (float 0.001) 0.0
    (Layout.size layout_hidden_again).height

let test_toggle_flex_container_display_none () =
  let tree = new_tree () in
  let hidden =
    Style.default
    |> Style.set_display Style.Display.None
    |> Style.set_size (make_size 100.0 100.0)
  in
  let flex =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_size (make_size 100.0 100.0)
  in
  let child = new_leaf tree hidden |> or_fail "Failed to create child" in
  let root =
    new_with_children tree hidden [| child |] |> or_fail "Failed to create root"
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  compute_or_fail tree root available "Compute layout hidden container";
  let layout_hidden = Result.get_ok (layout tree root) in
  equal ~msg:"Root hidden width" (float 0.001) 0.0
    (Layout.size layout_hidden).width;
  equal ~msg:"Root hidden height" (float 0.001) 0.0
    (Layout.size layout_hidden).height;
  set_style tree root flex |> or_fail "set_style flex container";
  compute_or_fail tree root available "Compute layout flex container";
  let layout_flex = Result.get_ok (layout tree root) in
  equal ~msg:"Root flex width" (float 0.001) 100.0
    (Layout.size layout_flex).width;
  equal ~msg:"Root flex height" (float 0.001) 100.0
    (Layout.size layout_flex).height;
  set_style tree root hidden |> or_fail "set_style hidden container";
  compute_or_fail tree root available "Compute layout hidden container again";
  let layout_hidden_again = Result.get_ok (layout tree root) in
  equal ~msg:"Root hidden width again" (float 0.001) 0.0
    (Layout.size layout_hidden_again).width;
  equal ~msg:"Root hidden height again" (float 0.001) 0.0
    (Layout.size layout_hidden_again).height

let test_toggle_grid_child_display_none () =
  let tree = new_tree () in
  let hidden =
    Style.default
    |> Style.set_display Style.Display.None
    |> Style.set_size (make_size 100.0 100.0)
  in
  let grid =
    Style.default
    |> Style.set_display Style.Display.Grid
    |> Style.set_size (make_size 100.0 100.0)
  in
  let node = new_leaf tree hidden |> or_fail "Failed to create child" in
  let root =
    new_with_children tree grid [| node |] |> or_fail "Failed to create root"
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  compute_or_fail tree root available "Compute layout hidden grid child";
  let layout_hidden = Result.get_ok (layout tree node) in
  equal ~msg:"Hidden width" (float 0.001) 0.0 (Layout.size layout_hidden).width;
  equal ~msg:"Hidden height" (float 0.001) 0.0
    (Layout.size layout_hidden).height;
  set_style tree node grid |> or_fail "set_style grid child";
  compute_or_fail tree root available "Compute layout grid child";
  let layout_grid = Result.get_ok (layout tree node) in
  equal ~msg:"Grid width" (float 0.001) 100.0 (Layout.size layout_grid).width;
  equal ~msg:"Grid height" (float 0.001) 100.0 (Layout.size layout_grid).height;
  set_style tree node hidden |> or_fail "set_style hidden grid child";
  compute_or_fail tree root available "Compute layout grid child hidden again";
  let layout_hidden_again = Result.get_ok (layout tree node) in
  equal ~msg:"Hidden width again" (float 0.001) 0.0
    (Layout.size layout_hidden_again).width;
  equal ~msg:"Hidden height again" (float 0.001) 0.0
    (Layout.size layout_hidden_again).height

let test_relayout_stable () =
  let tree = new_tree () in
  let node1 =
    Style.default
    |> Style.set_size (make_size 8.0 80.0)
    |> new_leaf tree |> or_fail "node1"
  in
  let node0_style =
    Style.default
    |> Style.set_align_self (Some Style.Align_items.Center)
    |> Style.set_size
         Size.{ width = Style.Dimension.auto; height = Style.Dimension.auto }
  in
  let node0 =
    new_with_children tree node0_style [| node1 |] |> or_fail "node0"
  in
  let root_style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.percent 1.0;
             height = Style.Dimension.percent 1.0;
           }
  in
  let root = new_with_children tree root_style [| node0 |] |> or_fail "root" in
  let available =
    Size.
      {
        width = Available_space.of_float 100.0;
        height = Available_space.of_float 100.0;
      }
  in
  compute_or_fail tree root available "Compute layout relayout";
  let initial = Result.get_ok (layout tree root) in
  let initial0 = Result.get_ok (layout tree node0) in
  let initial1 = Result.get_ok (layout tree node1) in
  for _ = 1 to 5 do
    compute_or_fail tree root available "Recompute layout relayout";
    equal ~msg:"Root x stable" (float 0.001) (Layout.location initial).x
      (Layout.location (Result.get_ok (layout tree root))).x;
    equal ~msg:"Root y stable" (float 0.001) (Layout.location initial).y
      (Layout.location (Result.get_ok (layout tree root))).y;
    equal ~msg:"Node0 x stable" (float 0.001) (Layout.location initial0).x
      (Layout.location (Result.get_ok (layout tree node0))).x;
    equal ~msg:"Node0 y stable" (float 0.001) (Layout.location initial0).y
      (Layout.location (Result.get_ok (layout tree node0))).y;
    equal ~msg:"Node1 x stable" (float 0.001) (Layout.location initial1).x
      (Layout.location (Result.get_ok (layout tree node1))).x;
    equal ~msg:"Node1 y stable" (float 0.001) (Layout.location initial1).y
      (Layout.location (Result.get_ok (layout tree node1))).y
  done

(* Test suite *)
let () =
  run "Toffee Tests"
    [
      group "basic"
        [
          test "Create tree and node" test_create_tree;
          test "Simple leaf layout" test_simple_leaf_layout;
        ];
      group "flexbox"
        [
          test "Flexbox row" test_flexbox_row;
          test "Flexbox column" test_flexbox_column;
          test "Flex grow" test_flex_grow;
        ];
      group "spacing" [ test "Padding" test_padding; test "Margin" test_margin ];
      group "sizing" [ test "Aspect ratio" test_aspect_ratio ];
      group "caching"
        [ test "Measure count flexbox" test_measure_count_flexbox ];
      group "min_max_overrides"
        [
          test "Min overrides max" test_min_overrides_max;
          test "Max overrides size" test_max_overrides_size;
          test "Min overrides size" test_min_overrides_size;
        ];
      group "root_constraints"
        [
          test "Root with percentage size" test_root_with_percentage_size;
          test "Root with no size" test_root_with_no_size;
          test "Root with larger size" test_root_with_larger_size;
          test "Root padding and border larger than size"
            test_root_padding_and_border_larger_than_definite_size;
        ];
      group "rounding"
        [ test "Rounding without gaps" test_rounding_doesnt_leave_gaps ];
      group "display_none"
        [
          test "Toggle root display none" test_toggle_root_display_none;
          test "Toggle root display none with children"
            test_toggle_root_display_none_with_children;
          test "Toggle flex child display none"
            test_toggle_flex_child_display_none;
          test "Toggle flex container display none"
            test_toggle_flex_container_display_none;
          test "Toggle grid child display none"
            test_toggle_grid_child_display_none;
        ];
      group "relayout" [ test "Relayout is stable" test_relayout_stable ];
    ]
