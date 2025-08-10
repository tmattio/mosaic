open Ui
open Ubench

let benchmarks =
  [
    create_group "Text"
      [
        create "long_clip" (fun () ->
            render_string ~width:80 (text ~wrap:`Clip (String.make 5000 'a')));
        create "long_truncate" (fun () ->
            render_string ~width:80
              (text ~wrap:`Truncate (String.make 5000 'a')));
        create "long_wrap" (fun () ->
            render_string ~width:80 (text ~wrap:`Wrap (String.make 5000 'a')));
        create "multi_line" (fun () ->
            render_string ~width:80
              (text
                 (String.concat "\n"
                    (List.init 500 (fun _ -> String.make 80 'a')))));
      ];
    create_group "Box"
      [
        create "tall_vbox" (fun () ->
            render_string ~width:80
              (vbox (List.init 200 (fun i -> text (string_of_int i)))));
        create "wide_hbox" (fun () ->
            render_string ~width:200
              (hbox (List.init 100 (fun i -> text (string_of_int i)))));
        create "deep_nest" (fun () ->
            let rec build d =
              if d = 0 then text "leaf"
              else box ~border:Border.normal [ build (d - 1) ]
            in
            render_string ~width:80 (build 30));
        create "zbox_stack" (fun () ->
            render_string ~width:80
              (zbox (List.init 10 (fun _ -> text (String.make 100 'a')))));
      ];
    create_group "Table"
      [
        create "large_rows" (fun () ->
            let rows =
              List.init 500 (fun _ -> List.init 5 (fun _ -> "cell data"))
            in
            render_string ~width:80 (table ~rows ()));
        create "wide_columns" (fun () ->
            let rows =
              List.init 50 (fun _ -> List.init 20 (fun _ -> "cell data"))
            in
            render_string ~width:200 (table ~rows ()));
        create "with_borders" (fun () ->
            let rows = List.init 200 (fun _ -> List.init 5 (fun _ -> "data")) in

            render_string ~width:80 (table ~rows ~box_style:Table.Double ()));
      ];
    create_group "Tree"
      [
        create "deep" (fun () ->
            let rec build d =
              let leaf =
                {
                  Tree.label = text "leaf";
                  expanded = false;
                  children = [];
                  guide_style = None;
                }
              in
              if d = 0 then leaf
              else
                {
                  Tree.label = text "node";
                  expanded = true;
                  children = [ build (d - 1) ];
                  guide_style = None;
                }
            in
            render_string ~width:80 (tree (build 50)));
        create "wide" (fun () ->
            let rec build d b =
              let leaf =
                {
                  Tree.label = text "leaf";
                  expanded = false;
                  children = [];
                  guide_style = None;
                }
              in
              if d = 0 then leaf
              else
                {
                  Tree.label = text "node";
                  expanded = true;
                  children = List.init b (fun _ -> build (d - 1) b);
                  guide_style = None;
                }
            in
            render_string ~width:80 (tree (build 3 5)));
        create "nested" (fun () ->
            let rec build d b =
              let leaf =
                {
                  Tree.label = text "leaf";
                  expanded = false;
                  children = [];
                  guide_style = None;
                }
              in
              if d = 0 then leaf
              else
                {
                  Tree.label = text "node";
                  expanded = true;
                  children = List.init b (fun _ -> build (d - 1) b);
                  guide_style = None;
                }
            in
            render_string ~width:80 (tree (build 4 4)));
      ];
    create_group "Canvas"
      [
        create "dense_lines" (fun () ->
            render_string ~width:30 ~height:30
              (Canvas.create ~width:(`Cells 30) ~height:(`Cells 30)
                 (fun ~width ~height canvas ->
                   for _ = 0 to 20 do
                     Canvas.draw_line ~x1:0 ~y1:0 ~x2:width ~y2:height
                       ~kind:`Line canvas;
                     Canvas.draw_line ~x1:0 ~y1:height ~x2:width ~y2:0
                       ~kind:`Braille canvas
                   done)));
        create "filled_boxes" (fun () ->
            render_string ~width:20 ~height:20
              (Canvas.create ~width:(`Cells 20) ~height:(`Cells 20)
                 (fun ~width ~height canvas ->
                   for _ = 0 to 5 do
                     Canvas.draw_box ~x:0 ~y:0 ~width ~height canvas
                   done)));
      ];
    create_group "Complex"
      [
        create "dashboard" (fun () ->
            let tbl =
              table ~title:(Some "Data")
                ~rows:(List.init 50 (fun _ -> List.init 4 (fun _ -> "val")))
                ()
            in
            let chrt =
              Canvas.create ~width:(`Cells 40) ~height:(`Cells 20)
                (fun ~width ~height canvas ->
                  Canvas.draw_box ~x:0 ~y:0 ~width ~height
                    ~border:Border.rounded canvas;
                  for i = 0 to 20 do
                    Canvas.draw_line ~x1:0 ~y1:i ~x2:width ~y2:(height - i)
                      canvas
                  done)
            in
            let ui =
              hbox
                [
                  panel tbl;
                  panel chrt;
                  progress_bar ~width:20 ~completed:0.5 ();
                  spinner Spinner.Line_spin;
                ]
            in
            render_string ~width:80 ui);
      ];
    create_group "Interaction"
      [
        create "hit_test" (fun () ->
            let ui =
              vbox
                (List.init 100 (fun i ->
                     with_key (Key.of_int i)
                       (box ~border:Border.normal [ text (string_of_int i) ])))
            in
            let snap = Layout_snapshot.create () in
            let screen = Screen.create ~rows:100 ~cols:80 () in
            render ~snapshot:snap screen ui;
            for x = 0 to 39 do
              for y = 0 to 49 do
                ignore (Layout_snapshot.hit_test snap ~x ~y)
              done
            done);
      ];
  ]

let () = Ubench.run_cli benchmarks
