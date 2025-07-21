open Mosaic

let () =
  (* Example 1: Simple table with headers *)
  print_endline "Example 1: Simple table with headers";
  let simple =
    Ui.table ~headers:[ "Name"; "Age"; "City" ]
      ~rows:
        [
          [ "Alice"; "30"; "New York" ];
          [ "Bob"; "25"; "San Francisco" ];
          [ "Charlie"; "35"; "Chicago" ];
        ]
      ()
  in
  Ui.print simple;
  print_newline ();

  (* Example 2: Table with custom column configuration *)
  print_endline "Example 2: Table with custom column configuration";
  let columns =
    [
      (let c = Ui.Table.default_column ~header:"Product" in
       { c with justify = `Left; min_width = Some 15 });
      (let c = Ui.Table.default_column ~header:"Price" in
       { c with justify = `Right; style = Ui.Style.(fg Green) });
      (let c = Ui.Table.default_column ~header:"Stock" in
       { c with justify = `Center; style = Ui.Style.(fg Cyan) });
    ]
  in
  let custom =
    Ui.Table.table ~columns
      ~rows:
        [
          [ "Widget A"; "$19.99"; "150" ];
          [ "Gadget B"; "$49.99"; "75" ];
          [ "Tool C"; "$129.99"; "12" ];
        ]
      ()
  in
  Ui.print custom;
  print_newline ();

  (* Example 3: Table with different box styles *)
  print_endline "Example 3: Table with different box styles";
  let box_styles =
    [
      ("Rounded", Ui.Table.Rounded);
      ("Double", Ui.Table.Double);
      ("ASCII", Ui.Table.Ascii);
      ("Minimal", Ui.Table.Minimal);
    ]
  in
  List.iter
    (fun (name, style) ->
      print_endline ("Box style: " ^ name);
      let styled =
        Ui.Table.table
          ~columns:
            [
              Ui.Table.default_column ~header:"Col 1";
              Ui.Table.default_column ~header:"Col 2";
            ]
          ~rows:[ [ "A"; "B" ]; [ "C"; "D" ] ]
          ~box_style:style ()
      in
      Ui.print styled;
      print_newline ())
    box_styles;

  (* Example 4: Table with title and caption *)
  print_endline "Example 4: Table with title and caption";
  let with_title =
    Ui.Table.table ~title:(Some "Sales Report")
      ~caption:(Some "Q4 2023 Results")
      ~columns:
        [
          Ui.Table.default_column ~header:"Month";
          Ui.Table.default_column ~header:"Revenue";
          Ui.Table.default_column ~header:"Growth";
        ]
      ~rows:
        [
          [ "October"; "$45,000"; "+12%" ];
          [ "November"; "$52,000"; "+15%" ];
          [ "December"; "$61,000"; "+17%" ];
        ]
      ~title_style:Ui.Style.(fg Bright_blue ++ bold)
      ~caption_style:Ui.Style.(fg Bright_black ++ italic)
      ()
  in
  Ui.print with_title;
  print_newline ();

  (* Example 5: Table with padding and row styles *)
  print_endline "Example 5: Table with padding and alternating row styles";
  let padded =
    Ui.Table.table
      ~columns:
        [
          Ui.Table.default_column ~header:"Item";
          Ui.Table.default_column ~header:"Description";
          Ui.Table.default_column ~header:"Status";
        ]
      ~rows:
        [
          [ "Task 1"; "Complete documentation"; "Done" ];
          [ "Task 2"; "Review pull requests"; "In Progress" ];
          [ "Task 3"; "Deploy to production"; "Pending" ];
          [ "Task 4"; "Update dependencies"; "Done" ];
        ]
      ~padding:(2, 3, 2, 3)
      ~row_styles:[ Ui.Style.empty; Ui.Style.(bg (Index 236)) ]
      ()
  in
  Ui.print padded;
  print_newline ();

  (* Example 6: Table with show_lines and leading *)
  print_endline "Example 6: Table with lines between rows and spacing";
  let with_lines =
    Ui.Table.table
      ~columns:
        [
          Ui.Table.default_column ~header:"Department";
          Ui.Table.default_column ~header:"Manager";
          Ui.Table.default_column ~header:"Employees";
        ]
      ~rows:
        [
          [ "Engineering"; "Alice Smith"; "45" ];
          [ "Marketing"; "Bob Johnson"; "23" ];
          [ "Sales"; "Carol White"; "67" ];
        ]
      ~show_lines:true ~leading:1 ~box_style:Ui.Table.Heavy ()
  in
  Ui.print with_lines;
  print_newline ();

  (* Example 7: Table with footer *)
  print_endline "Example 7: Table with footer";
  let cols_with_footer =
    [
      (let c = Ui.Table.default_column ~header:"Product" in
       { c with footer = Some "Total:" });
      (let c = Ui.Table.default_column ~header:"Units Sold" in
       { c with footer = Some "245"; justify = `Right });
      (let c = Ui.Table.default_column ~header:"Revenue" in
       { c with footer = Some "$12,450"; justify = `Right });
    ]
  in
  let with_footer =
    Ui.Table.table ~columns:cols_with_footer
      ~rows:
        [
          [ "Widget A"; "100"; "$5,000" ];
          [ "Gadget B"; "75"; "$3,750" ];
          [ "Tool C"; "70"; "$3,700" ];
        ]
      ~show_footer:true
      ~footer_style:Ui.Style.(bold ++ fg Yellow)
      ()
  in
  Ui.print with_footer;
  print_newline ();

  (* Example 8: Table with text overflow handling *)
  print_endline "Example 8: Table with text overflow handling";
  let overflow_cols =
    [
      (let c = Ui.Table.default_column ~header:"Ellipsis" in
       { c with max_width = Some 10; overflow = `Ellipsis });
      (let c = Ui.Table.default_column ~header:"Crop" in
       { c with max_width = Some 10; overflow = `Crop });
      (let c = Ui.Table.default_column ~header:"Fold" in
       { c with max_width = Some 10; overflow = `Fold });
    ]
  in
  let overflow =
    Ui.Table.table ~columns:overflow_cols
      ~rows:
        [
          [
            "This is a very long text";
            "This is a very long text";
            "This is a very long text";
          ];
          [ "Short"; "Short"; "Short" ];
        ]
      ()
  in
  Ui.print overflow;
  print_newline ();

  (* Example 9: Table with collapse_padding *)
  print_endline "Example 9: Table with collapse_padding";
  let collapsed =
    Ui.Table.table
      ~columns:
        [
          Ui.Table.default_column ~header:"A";
          Ui.Table.default_column ~header:"B";
          Ui.Table.default_column ~header:"C";
        ]
      ~rows:[ [ "1"; "2"; "3" ]; [ "4"; "5"; "6" ] ]
      ~padding:(1, 2, 1, 2) ~collapse_padding:true ()
  in
  Ui.print collapsed;
  print_newline ();

  (* Example 10: Table with min_width constraint *)
  print_endline "Example 10: Table with min_width constraint";
  let min_width_table =
    Ui.Table.table
      ~columns:
        [
          Ui.Table.default_column ~header:"ID";
          Ui.Table.default_column ~header:"Val";
        ]
      ~rows:[ [ "1"; "A" ]; [ "2"; "B" ] ]
      ~min_width:(Some 40) ()
  in
  Ui.print min_width_table;
  print_newline ();

  (* Example 11: Grid table (no borders) *)
  print_endline "Example 11: Grid table (no borders)";
  let grid =
    Ui.Table.grid_table
      ~columns:
        [
          Ui.Table.default_column ~header:"";
          Ui.Table.default_column ~header:"";
          Ui.Table.default_column ~header:"";
        ]
      ~rows:
        [
          [ "Name:"; "John Doe"; "" ];
          [ "Email:"; "john@example.com"; "" ];
          [ "Phone:"; "+1-234-567-8900"; "" ];
        ]
  in
  Ui.print grid;
  print_newline ();

  (* Example 12: Complex table with all features *)
  print_endline "Example 12: Complex table with multiple features";
  let complex_cols =
    [
      (let c = Ui.Table.default_column ~header:"Employee" in
       {
         c with
         min_width = Some 20;
         header_style = Ui.Style.(fg Blue ++ bold);
         footer = Some "Summary";
       });
      (let c = Ui.Table.default_column ~header:"Department" in
       { c with justify = `Center; style = Ui.Style.(fg Cyan) });
      (let c = Ui.Table.default_column ~header:"Salary" in
       {
         c with
         justify = `Right;
         style = Ui.Style.(fg Green);
         footer = Some "$385,000";
       });
      (let c = Ui.Table.default_column ~header:"Performance" in
       {
         c with
         justify = `Center;
         vertical = `Middle;
         footer = Some "Avg: 8.5";
       });
    ]
  in
  let complex =
    Ui.Table.table ~title:(Some "Annual Employee Review") ~columns:complex_cols
      ~rows:
        [
          [ "Alice Johnson"; "Engineering"; "$120,000"; "9.5" ];
          [ "Bob Smith"; "Marketing"; "$85,000"; "8.0" ];
          [ "Charlie Brown"; "Sales"; "$95,000"; "8.5" ];
          [ "Diana Prince"; "HR"; "$85,000"; "8.0" ];
        ]
      ~box_style:Ui.Table.DoubleEdge ~show_footer:true ~show_lines:true
      ~padding:(1, 2, 1, 2) ~expand:true ~width:(Some 80)
      ~border_style:Ui.Style.(fg Blue)
      ~header_style:Ui.Style.(bg (Index 236))
      ~footer_style:Ui.Style.(bg (Index 238) ++ bold)
      ~title_style:Ui.Style.(fg Bright_white ++ bold ++ underline)
      ()
  in
  Ui.print complex
