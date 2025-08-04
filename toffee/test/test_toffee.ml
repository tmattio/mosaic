(* Basic tests for the toffee layout library - Updated API *)

open Toffee
open Geometry

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
  | Ok _node -> Alcotest.(check bool) "Node created" true true
  | Error e -> Alcotest.failf "Failed to create node: %s" (Error.to_string e)

let test_simple_leaf_layout () =
  let tree = new_tree () in
  let style =
    Style.set_size Style.default
      Size.
        {
          width = Style.Dimension.length 100.0;
          height = Style.Dimension.length 50.0;
        }
  in
  match new_leaf tree style with
  | Error e -> Alcotest.failf "Failed to create node: %s" (Error.to_string e)
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
              Alcotest.(check bool)
                "Width is 100"
                (float_eq size.width 100.0)
                true;
              Alcotest.(check bool)
                "Height is 50"
                (float_eq size.height 50.0)
                true
          | Error _ -> Alcotest.fail "Failed to get layout")
      | Error _ -> Alcotest.fail "Failed to compute layout")

let test_flexbox_row () =
  let tree = new_tree () in

  (* Create container with flex row *)
  let container_style =
    Style.set_size
      (Style.set_flex_direction
         (Style.set_display Style.default Style.Display.Flex)
         Style.Flex_direction.Row)
      Size.
        {
          width = Style.Dimension.length 300.0;
          height = Style.Dimension.length 100.0;
        }
  in

  (* Create child nodes *)
  let child_style =
    Style.set_size Style.default
      Size.
        {
          width = Style.Dimension.length 50.0;
          height = Style.Dimension.length 50.0;
        }
  in

  match new_leaf tree child_style with
  | Error e -> Alcotest.failf "Failed to create child1: %s" (Error.to_string e)
  | Ok child1 -> (
      match new_leaf tree child_style with
      | Error e ->
          Alcotest.failf "Failed to create child2: %s" (Error.to_string e)
      | Ok child2 -> (
          match new_with_children tree container_style [| child1; child2 |] with
          | Error e ->
              Alcotest.failf "Failed to create container: %s"
                (Error.to_string e)
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
                      Alcotest.(check bool)
                        "Container width"
                        (float_eq size.width 300.0)
                        true;
                      Alcotest.(check bool)
                        "Container height"
                        (float_eq size.height 100.0)
                        true
                  | Error _ -> Alcotest.fail "Failed to get container layout");

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
                      Alcotest.(check bool)
                        "Child1 x position" (float_eq location.x 0.0) true;
                      Alcotest.(check bool)
                        "Child1 width" (float_eq size.width 50.0) true
                  | Error _ -> Alcotest.fail "Failed to get child1 layout");

                  match layout tree child2 with
                  | Ok layout ->
                      let float_eq, _ = test_helpers in
                      let location = Layout.location layout in
                      let size = Layout.size layout in
                      Printf.printf
                        "Child2 layout: location=(%.1f, %.1f), size=(%.1f x \
                         %.1f)\n"
                        location.x location.y size.width size.height;
                      Alcotest.(check bool)
                        "Child2 x position" (float_eq location.x 50.0) true;
                      Alcotest.(check bool)
                        "Child2 width" (float_eq size.width 50.0) true
                  | Error _ -> Alcotest.fail "Failed to get child2 layout")
              | Error _ -> Alcotest.fail "Failed to compute layout")))

let test_flexbox_column () =
  let tree = new_tree () in

  (* Create container with flex column *)
  let container_style =
    Style.set_size
      (Style.set_flex_direction
         (Style.set_display Style.default Style.Display.Flex)
         Style.Flex_direction.Column)
      Size.
        {
          width = Style.Dimension.length 100.0;
          height = Style.Dimension.length 300.0;
        }
  in

  (* Create child nodes *)
  let child_style =
    Style.set_size Style.default
      Size.
        {
          width = Style.Dimension.length 50.0;
          height = Style.Dimension.length 50.0;
        }
  in

  match new_leaf tree child_style with
  | Error e -> Alcotest.failf "Failed to create child1: %s" (Error.to_string e)
  | Ok child1 -> (
      match new_leaf tree child_style with
      | Error e ->
          Alcotest.failf "Failed to create child2: %s" (Error.to_string e)
      | Ok child2 -> (
          match new_with_children tree container_style [| child1; child2 |] with
          | Error e ->
              Alcotest.failf "Failed to create container: %s"
                (Error.to_string e)
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
                      Alcotest.(check bool)
                        "Child1 y position" (float_eq location.y 0.0) true
                  | Error _ -> Alcotest.fail "Failed to get child1 layout");

                  match layout tree child2 with
                  | Ok layout ->
                      let float_eq, _ = test_helpers in
                      let location = Layout.location layout in
                      let size = Layout.size layout in
                      Printf.printf
                        "Column child2 layout: location=(%.1f, %.1f), \
                         size=(%.1f x %.1f)\n"
                        location.x location.y size.width size.height;
                      Alcotest.(check bool)
                        "Child2 y position" (float_eq location.y 50.0) true
                  | Error _ -> Alcotest.fail "Failed to get child2 layout")
              | Error _ -> Alcotest.fail "Failed to compute layout")))

let test_padding () =
  let tree = new_tree () in

  let style =
    Style.(
      set_padding
        (set_size default
           Size.
             { width = Dimension.length 100.0; height = Dimension.length 100.0 })
        (Rect.all (Length_percentage.length 10.0)))
  in

  match new_leaf tree style with
  | Error e -> Alcotest.failf "Failed to create node: %s" (Error.to_string e)
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
              (* With padding, the size should remain 100x100 for content-box sizing *)
              Alcotest.(check bool)
                "Width with padding"
                (float_eq size.width 100.0)
                true;
              Alcotest.(check bool)
                "Height with padding"
                (float_eq size.height 100.0)
                true
          | Error _ -> Alcotest.fail "Failed to get layout")
      | Error _ -> Alcotest.fail "Failed to compute layout")

let test_margin () =
  let tree = new_tree () in

  (* Create parent container *)
  let parent_style =
    Style.set_size
      (Style.set_display Style.default Style.Display.Flex)
      Size.
        {
          width = Style.Dimension.length 200.0;
          height = Style.Dimension.length 200.0;
        }
  in

  (* Create child with margin *)
  let child_style =
    Style.set_margin
      (Style.set_size Style.default
         Size.
           {
             width = Style.Dimension.length 50.0;
             height = Style.Dimension.length 50.0;
           })
      (Rect.all (Style.Length_percentage_auto.length 10.0))
  in

  match new_leaf tree child_style with
  | Error e -> Alcotest.failf "Failed to create child: %s" (Error.to_string e)
  | Ok child -> (
      match new_with_children tree parent_style [| child |] with
      | Error e ->
          Alcotest.failf "Failed to create parent: %s" (Error.to_string e)
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
                  Alcotest.(check bool)
                    "Child x with margin" (float_eq location.x 10.0) true;
                  Alcotest.(check bool)
                    "Child y with margin" (float_eq location.y 10.0) true
              | Error _ -> Alcotest.fail "Failed to get child layout")
          | Error _ -> Alcotest.fail "Failed to compute layout"))

let test_flex_grow () =
  let tree = new_tree () in

  let container_style =
    Style.set_size
      (Style.set_flex_direction
         (Style.set_display Style.default Style.Display.Flex)
         Style.Flex_direction.Row)
      Size.
        {
          width = Style.Dimension.length 200.0;
          height = Style.Dimension.length 100.0;
        }
  in

  (* Create child with flex_grow *)
  let child_style =
    Style.set_size
      (Style.set_flex_grow Style.default 1.0)
      Size.
        { width = Style.Dimension.auto; height = Style.Dimension.length 50.0 }
  in

  Printf.printf "Creating child with flex_grow=%.1f\n" 1.0;
  match new_leaf tree child_style with
  | Error e -> Alcotest.failf "Failed to create child: %s" (Error.to_string e)
  | Ok child -> (
      match new_with_children tree container_style [| child |] with
      | Error e ->
          Alcotest.failf "Failed to create container: %s" (Error.to_string e)
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
                  Alcotest.(check bool)
                    "Child width with flex_grow"
                    (float_eq size.width 200.0)
                    true
              | Error _ -> Alcotest.fail "Failed to get child layout")
          | Error _ -> Alcotest.fail "Failed to compute layout"))

let test_aspect_ratio () =
  let tree = new_tree () in

  let style =
    Style.set_size
      (Style.set_aspect_ratio Style.default (Some 2.0)) (* width / height = 2 *)
      Size.
        { width = Style.Dimension.length 100.0; height = Style.Dimension.auto }
  in

  match new_leaf tree style with
  | Error e -> Alcotest.failf "Failed to create node: %s" (Error.to_string e)
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
              Alcotest.(check bool) "Width" (float_eq size.width 100.0) true;
              Alcotest.(check bool)
                "Height from aspect ratio"
                (float_eq size.height 50.0)
                true
          | Error _ -> Alcotest.fail "Failed to get layout")
      | Error _ -> Alcotest.fail "Failed to compute layout")

(* Test suite *)
let () =
  let open Alcotest in
  run "Toffee Tests"
    [
      ( "basic",
        [
          test_case "Create tree and node" `Quick test_create_tree;
          test_case "Simple leaf layout" `Quick test_simple_leaf_layout;
        ] );
      ( "flexbox",
        [
          test_case "Flexbox row" `Quick test_flexbox_row;
          test_case "Flexbox column" `Quick test_flexbox_column;
          test_case "Flex grow" `Quick test_flex_grow;
        ] );
      ( "spacing",
        [
          test_case "Padding" `Quick test_padding;
          test_case "Margin" `Quick test_margin;
        ] );
      ("sizing", [ test_case "Aspect ratio" `Quick test_aspect_ratio ]);
    ]
