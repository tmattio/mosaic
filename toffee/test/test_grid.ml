(* Additional grid-focused unit tests ported from taffy's non-generated suite *)

open Toffee
open Geometry
open Tree

let line = Style.Grid.Placement.line
let span = Style.Grid.Placement.span
let auto = Style.Grid.Placement.auto

let make_grid_child ~col_start ~col_end ~row_start ~row_end =
  Style.make
    ~grid_column:{ start = col_start; end_ = col_end }
    ~grid_row:{ start = row_start; end_ = row_end }
    ()

let empty_line_names track_count = List.init (track_count + 1) (fun _ -> [])

let repeat count tracks =
  Style.Grid.Template_component.repeat
    (Style.Grid_repetition.make ~count ~tracks
       ~line_names:(empty_line_names (List.length tracks)))

let resolve_calc _ basis = basis

let test_compute_grid_size_estimate_explicit_children () =
  let child_styles =
    [
      make_grid_child ~col_start:(line 1) ~col_end:(span 2) ~row_start:(line 2)
        ~row_end:auto;
      make_grid_child ~col_start:(line (-4)) ~col_end:auto
        ~row_start:(line (-2)) ~row_end:auto;
    ]
  in
  let cols, rows =
    Compute_grid.Implicit_grid.compute_grid_size_estimate ~explicit_col_count:6
      ~explicit_row_count:8 ~child_styles_iter:(List.to_seq child_styles)
  in
  Alcotest.(check int)
    "Inline negative implicit" 0
    (Compute_grid.Grid_track_counts.negative_implicit cols);
  Alcotest.(check int)
    "Inline explicit" 6
    (Compute_grid.Grid_track_counts.explicit cols);
  Alcotest.(check int)
    "Inline positive implicit" 0
    (Compute_grid.Grid_track_counts.positive_implicit cols);
  Alcotest.(check int)
    "Block negative implicit" 0
    (Compute_grid.Grid_track_counts.negative_implicit rows);
  Alcotest.(check int)
    "Block explicit" 8
    (Compute_grid.Grid_track_counts.explicit rows);
  Alcotest.(check int)
    "Block positive implicit" 0
    (Compute_grid.Grid_track_counts.positive_implicit rows)

let test_compute_grid_size_estimate_negative_implicit () =
  let child_styles =
    [
      make_grid_child ~col_start:(line (-6)) ~col_end:(span 2)
        ~row_start:(line (-8)) ~row_end:auto;
      make_grid_child ~col_start:(line 4) ~col_end:auto ~row_start:(line 3)
        ~row_end:auto;
    ]
  in
  let cols, rows =
    Compute_grid.Implicit_grid.compute_grid_size_estimate ~explicit_col_count:4
      ~explicit_row_count:4 ~child_styles_iter:(List.to_seq child_styles)
  in
  Alcotest.(check int)
    "Inline negative implicit" 1
    (Compute_grid.Grid_track_counts.negative_implicit cols);
  Alcotest.(check int)
    "Inline explicit" 4
    (Compute_grid.Grid_track_counts.explicit cols);
  Alcotest.(check int)
    "Inline positive implicit" 0
    (Compute_grid.Grid_track_counts.positive_implicit cols);
  Alcotest.(check int)
    "Block negative implicit" 3
    (Compute_grid.Grid_track_counts.negative_implicit rows);
  Alcotest.(check int)
    "Block explicit" 4
    (Compute_grid.Grid_track_counts.explicit rows);
  Alcotest.(check int)
    "Block positive implicit" 0
    (Compute_grid.Grid_track_counts.positive_implicit rows)

let test_explicit_grid_sizing_auto_fill_exact_fit () =
  let grid_style =
    Style.make ~display:Style.Display.Grid
      ~size:
        {
          width = Style.Dimension.length 120.0;
          height = Style.Dimension.length 80.0;
        }
      ~grid_template_columns:
        [
          repeat Style.Repetition_count.auto_fill
            [ Style.Grid.Track_sizing_function.length 40.0 ];
        ]
      ~grid_template_rows:
        [
          repeat Style.Repetition_count.auto_fill
            [ Style.Grid.Track_sizing_function.length 20.0 ];
        ]
      ()
  in
  let preferred_size =
    Style.size grid_style |> Size.map Style.Dimension.to_option
  in
  let auto_cols, col_count =
    Compute_grid.Explicit_grid.compute_explicit_grid_size_in_axis
      ~style:grid_style ~auto_fit_container_size:preferred_size.width
      ~auto_fit_strategy:
        Compute_grid.Explicit_grid.Max_repetitions_that_do_not_overflow
      ~resolve_calc_value:resolve_calc ~axis:Absolute_axis.Horizontal
  in
  let auto_rows, row_count =
    Compute_grid.Explicit_grid.compute_explicit_grid_size_in_axis
      ~style:grid_style ~auto_fit_container_size:preferred_size.height
      ~auto_fit_strategy:
        Compute_grid.Explicit_grid.Max_repetitions_that_do_not_overflow
      ~resolve_calc_value:resolve_calc ~axis:Absolute_axis.Vertical
  in
  Alcotest.(check int) "Column count" 3 col_count;
  Alcotest.(check int) "Row count" 4 row_count;
  Alcotest.(check int) "Auto column reps" 3 auto_cols;
  Alcotest.(check int) "Auto row reps" 4 auto_rows

let test_explicit_grid_sizing_auto_fill_with_gaps () =
  let grid_style =
    Style.make ~display:Style.Display.Grid
      ~size:
        {
          width = Style.Dimension.length 140.0;
          height = Style.Dimension.length 100.0;
        }
      ~grid_template_columns:
        [
          repeat Style.Repetition_count.auto_fill
            [ Style.Grid.Track_sizing_function.length 40.0 ];
        ]
      ~grid_template_rows:
        [
          repeat Style.Repetition_count.auto_fill
            [ Style.Grid.Track_sizing_function.length 20.0 ];
        ]
      ~gap:
        {
          width = Style.Length_percentage.length 20.0;
          height = Style.Length_percentage.length 20.0;
        }
      ()
  in
  let preferred_size =
    Style.size grid_style |> Size.map Style.Dimension.to_option
  in
  let auto_cols, col_count =
    Compute_grid.Explicit_grid.compute_explicit_grid_size_in_axis
      ~style:grid_style ~auto_fit_container_size:preferred_size.width
      ~auto_fit_strategy:
        Compute_grid.Explicit_grid.Max_repetitions_that_do_not_overflow
      ~resolve_calc_value:resolve_calc ~axis:Absolute_axis.Horizontal
  in
  let auto_rows, row_count =
    Compute_grid.Explicit_grid.compute_explicit_grid_size_in_axis
      ~style:grid_style ~auto_fit_container_size:preferred_size.height
      ~auto_fit_strategy:
        Compute_grid.Explicit_grid.Max_repetitions_that_do_not_overflow
      ~resolve_calc_value:resolve_calc ~axis:Absolute_axis.Vertical
  in
  Alcotest.(check int) "Column count with gap" 2 col_count;
  Alcotest.(check int) "Row count with gap" 3 row_count;
  Alcotest.(check int) "Auto column reps with gap" 2 auto_cols;
  Alcotest.(check int) "Auto row reps with gap" 3 auto_rows

(* Placement algorithm tests *)

let make_counts ~neg ~explicit ~pos =
  Compute_grid.Grid_track_counts.make ~negative_implicit:neg ~explicit
    ~positive_implicit:pos

let make_grid_style
    ((col_start, col_end, row_start, row_end) :
      Style.Grid.Placement.t
      * Style.Grid.Placement.t
      * Style.Grid.Placement.t
      * Style.Grid.Placement.t) =
  Style.make ~display:Style.Display.Grid
    ~grid_column:Geometry.Line.{ start = col_start; end_ = col_end }
    ~grid_row:Geometry.Line.{ start = row_start; end_ = row_end }
    ()

let expected_to_tuple (line : int Geometry.Line.t) = (line.start, line.end_)

let placement_test_runner ~flow ~explicit_cols ~explicit_rows children
    expected_col_counts expected_row_counts =
  let child_styles_iter =
    List.to_seq children |> Seq.map (fun (_, style, _) -> style)
  in
  let estimated_cols, estimated_rows =
    Compute_grid.Implicit_grid.compute_grid_size_estimate
      ~explicit_col_count:explicit_cols ~explicit_row_count:explicit_rows
      ~child_styles_iter
  in
  let cell_occupancy =
    Compute_grid.Cell_occupancy.with_track_counts estimated_cols estimated_rows
  in
  let items = ref [] in
  let name_resolver = Compute_grid.Named.create Style.default 0 0 in
  Compute_grid.Named.set_explicit_column_count name_resolver explicit_cols;
  Compute_grid.Named.set_explicit_row_count name_resolver explicit_rows;

  let child_ids_styles =
    List.map (fun (id, style, _) -> (Tree.Node_id.make id, style)) children
  in
  let parent_node = Tree.Node_id.make (-1) in
  let children_array = Array.of_list (List.map fst child_ids_styles) in
  let style_table = Hashtbl.create 16 in
  List.iter
    (fun (id, style) -> Hashtbl.add style_table id style)
    child_ids_styles;

  let tree =
    (module struct
      type t = unit

      let child_ids _ _ = Array.to_seq children_array
      let child_count _ _ = Array.length children_array
      let get_child_id _ _ idx = children_array.(idx)
      let get_core_container_style _ node = Hashtbl.find style_table node
      let set_unrounded_layout _ _ _ = ()
      let compute_child_layout _ _ _ = Layout_output.hidden
      let resolve_calc_value _ _ v = v
    end : Tree.LAYOUT_PARTIAL_TREE
      with type t = unit)
  in

  Compute_grid.Placement.place_grid_items tree
    ~cell_occupancy_matrix:cell_occupancy ~items ~tree:() ~parent_node
    ~grid_auto_flow:flow ~align_items:Style.Align_items.Start
    ~justify_items:Style.Align_items.Start ~named_line_resolver:name_resolver;

  (* Compare placements *)
  let sorted_items =
    List.sort
      (fun a b ->
        Int.compare
          (Tree.Node_id.index a.Compute_grid.Grid_item.node)
          (Tree.Node_id.index b.Compute_grid.Grid_item.node))
      !items
  in
  let sorted_children =
    List.sort (fun (id_a, _, _) (id_b, _, _) -> Int.compare id_a id_b) children
  in
  List.iter2
    (fun (_, _, (exp_col_start, exp_col_end, exp_row_start, exp_row_end)) item
       ->
      Alcotest.(check (pair int int))
        "Column placement"
        (exp_col_start, exp_col_end)
        (expected_to_tuple item.Compute_grid.Grid_item.column);
      Alcotest.(check (pair int int))
        "Row placement"
        (exp_row_start, exp_row_end)
        (expected_to_tuple item.Compute_grid.Grid_item.row))
    sorted_children sorted_items;

  let actual_rows =
    Compute_grid.Cell_occupancy.track_counts cell_occupancy
      Geometry.Absolute_axis.Vertical
  in
  let actual_cols =
    Compute_grid.Cell_occupancy.track_counts cell_occupancy
      Geometry.Absolute_axis.Horizontal
  in
  Alcotest.(check int)
    "Row negative implicit"
    (Compute_grid.Grid_track_counts.negative_implicit expected_row_counts)
    (Compute_grid.Grid_track_counts.negative_implicit actual_rows);
  Alcotest.(check int)
    "Row explicit"
    (Compute_grid.Grid_track_counts.explicit expected_row_counts)
    (Compute_grid.Grid_track_counts.explicit actual_rows);
  Alcotest.(check int)
    "Row positive implicit"
    (Compute_grid.Grid_track_counts.positive_implicit expected_row_counts)
    (Compute_grid.Grid_track_counts.positive_implicit actual_rows);
  Alcotest.(check int)
    "Col negative implicit"
    (Compute_grid.Grid_track_counts.negative_implicit expected_col_counts)
    (Compute_grid.Grid_track_counts.negative_implicit actual_cols);
  Alcotest.(check int)
    "Col explicit"
    (Compute_grid.Grid_track_counts.explicit expected_col_counts)
    (Compute_grid.Grid_track_counts.explicit actual_cols);
  Alcotest.(check int)
    "Col positive implicit"
    (Compute_grid.Grid_track_counts.positive_implicit expected_col_counts)
    (Compute_grid.Grid_track_counts.positive_implicit actual_cols)

let placement_tests =
  let flow_row = Style.Grid_auto_flow.Row in
  let flow_col = Style.Grid_auto_flow.Column in
  let flow_dense = Style.Grid_auto_flow.Row_dense in
  [
    ( "Only fixed placement",
      fun () ->
        let children =
          [
            (1, make_grid_style (line 1, auto, line 1, auto), (0, 1, 0, 1));
            ( 2,
              make_grid_style (line (-4), auto, line (-3), auto),
              (-1, 0, 0, 1) );
            ( 3,
              make_grid_style (line (-3), auto, line (-4), auto),
              (0, 1, -1, 0) );
            (4, make_grid_style (line 3, span 2, line 5, auto), (2, 4, 4, 5));
          ]
        in
        let expected_cols = make_counts ~neg:1 ~explicit:2 ~pos:2 in
        let expected_rows = make_counts ~neg:1 ~explicit:2 ~pos:3 in
        placement_test_runner ~flow:flow_row ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Placement spanning origin",
      fun () ->
        let children =
          [
            ( 1,
              make_grid_style (line (-1), line (-1), line (-1), line (-1)),
              (2, 3, 2, 3) );
            ( 2,
              make_grid_style (line (-1), span 2, line (-1), span 2),
              (2, 4, 2, 4) );
            ( 3,
              make_grid_style (line (-4), line (-4), line (-4), line (-4)),
              (-1, 0, -1, 0) );
            ( 4,
              make_grid_style (line (-4), span 2, line (-4), span 2),
              (-1, 1, -1, 1) );
          ]
        in
        let expected_cols = make_counts ~neg:1 ~explicit:2 ~pos:2 in
        let expected_rows = make_counts ~neg:1 ~explicit:2 ~pos:2 in
        placement_test_runner ~flow:flow_row ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Only auto placement row flow",
      fun () ->
        let auto_child = make_grid_style (auto, auto, auto, auto) in
        let mk id placement =
          let col_start, col_end, row_start, row_end = placement in
          (id, auto_child, (col_start, col_end, row_start, row_end))
        in
        let children =
          [
            mk 1 (0, 1, 0, 1);
            mk 2 (1, 2, 0, 1);
            mk 3 (0, 1, 1, 2);
            mk 4 (1, 2, 1, 2);
            mk 5 (0, 1, 2, 3);
            mk 6 (1, 2, 2, 3);
            mk 7 (0, 1, 3, 4);
            mk 8 (1, 2, 3, 4);
          ]
        in
        let expected_cols = make_counts ~neg:0 ~explicit:2 ~pos:0 in
        let expected_rows = make_counts ~neg:0 ~explicit:2 ~pos:2 in
        placement_test_runner ~flow:flow_row ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Only auto placement column flow",
      fun () ->
        let auto_child = make_grid_style (auto, auto, auto, auto) in
        let mk id placement =
          let col_start, col_end, row_start, row_end = placement in
          (id, auto_child, (col_start, col_end, row_start, row_end))
        in
        let children =
          [
            mk 1 (0, 1, 0, 1);
            mk 2 (0, 1, 1, 2);
            mk 3 (1, 2, 0, 1);
            mk 4 (1, 2, 1, 2);
            mk 5 (2, 3, 0, 1);
            mk 6 (2, 3, 1, 2);
            mk 7 (3, 4, 0, 1);
            mk 8 (3, 4, 1, 2);
          ]
        in
        let expected_cols = make_counts ~neg:0 ~explicit:2 ~pos:2 in
        let expected_rows = make_counts ~neg:0 ~explicit:2 ~pos:0 in
        placement_test_runner ~flow:flow_col ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Oversized item",
      fun () ->
        let children =
          [ (1, make_grid_style (span 5, auto, auto, auto), (0, 5, 0, 1)) ]
        in
        let expected_cols = make_counts ~neg:0 ~explicit:2 ~pos:3 in
        let expected_rows = make_counts ~neg:0 ~explicit:2 ~pos:0 in
        placement_test_runner ~flow:flow_row ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Fixed in secondary axis",
      fun () ->
        let children =
          [
            (1, make_grid_style (span 2, auto, line 1, auto), (0, 2, 0, 1));
            (2, make_grid_style (auto, auto, line 2, auto), (0, 1, 1, 2));
            (3, make_grid_style (auto, auto, line 1, auto), (2, 3, 0, 1));
            (4, make_grid_style (auto, auto, line 4, auto), (0, 1, 3, 4));
          ]
        in
        let expected_cols = make_counts ~neg:0 ~explicit:2 ~pos:1 in
        let expected_rows = make_counts ~neg:0 ~explicit:2 ~pos:2 in
        placement_test_runner ~flow:flow_row ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Definite in secondary axis with negative",
      fun () ->
        let children =
          [
            (2, make_grid_style (auto, auto, line 2, auto), (0, 1, 1, 2));
            (1, make_grid_style (line (-4), auto, line 2, auto), (-1, 0, 1, 2));
            (3, make_grid_style (auto, auto, line 1, auto), (-1, 0, 0, 1));
          ]
        in
        let expected_cols = make_counts ~neg:1 ~explicit:2 ~pos:0 in
        let expected_rows = make_counts ~neg:0 ~explicit:2 ~pos:0 in
        placement_test_runner ~flow:flow_row ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Dense packing algorithm",
      fun () ->
        let children =
          [
            (1, make_grid_style (line 2, auto, line 1, auto), (1, 2, 0, 1));
            (2, make_grid_style (span 2, auto, auto, auto), (2, 4, 0, 1));
            (3, make_grid_style (auto, auto, auto, auto), (0, 1, 0, 1));
          ]
        in
        let expected_cols = make_counts ~neg:0 ~explicit:4 ~pos:0 in
        let expected_rows = make_counts ~neg:0 ~explicit:4 ~pos:0 in
        placement_test_runner ~flow:flow_dense ~explicit_cols:4 ~explicit_rows:4
          children expected_cols expected_rows );
    ( "Sparse packing algorithm",
      fun () ->
        let children =
          [
            (1, make_grid_style (auto, span 3, auto, auto), (0, 3, 0, 1));
            (2, make_grid_style (auto, span 3, auto, auto), (0, 3, 1, 2));
            (3, make_grid_style (auto, span 1, auto, auto), (3, 4, 1, 2));
          ]
        in
        let expected_cols = make_counts ~neg:0 ~explicit:4 ~pos:0 in
        let expected_rows = make_counts ~neg:0 ~explicit:4 ~pos:0 in
        placement_test_runner ~flow:flow_row ~explicit_cols:4 ~explicit_rows:4
          children expected_cols expected_rows );
    ( "Auto placement in negative tracks",
      fun () ->
        let children =
          [
            (1, make_grid_style (line (-5), auto, line 1, auto), (-2, -1, 0, 1));
            (2, make_grid_style (auto, auto, line 2, auto), (-2, -1, 1, 2));
            (3, make_grid_style (auto, auto, auto, auto), (-1, 0, 0, 1));
          ]
        in
        let expected_cols = make_counts ~neg:2 ~explicit:2 ~pos:0 in
        let expected_rows = make_counts ~neg:0 ~explicit:2 ~pos:0 in
        placement_test_runner ~flow:flow_dense ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
  ]

let () =
  let open Alcotest in
  run "Toffee Grid Tests"
    [
      ( "implicit_grid",
        [
          test_case "Size estimate explicit children" `Quick
            test_compute_grid_size_estimate_explicit_children;
          test_case "Size estimate negative implicit" `Quick
            test_compute_grid_size_estimate_negative_implicit;
        ] );
      ( "explicit_grid",
        [
          test_case "Auto-fill exact fit" `Quick
            test_explicit_grid_sizing_auto_fill_exact_fit;
          test_case "Auto-fill with gaps" `Quick
            test_explicit_grid_sizing_auto_fill_with_gaps;
        ] );
      ( "placement",
        List.map (fun (name, fn) -> test_case name `Quick fn) placement_tests );
    ]
