open Test_utils

let%expect_test "simple table with headers" =
  print_ui ~width:30 ~height:8
    (Ui.table 
      ~box_style:Ui.Table.NoBox
      ~columns:[
        Ui.Table.default_column ~header:"Name";
        Ui.Table.default_column ~header:"Age";
        Ui.Table.default_column ~header:"City";
      ]
      ~rows:[
        ["Alice"; "30"; "NYC"];
        ["Bob"; "25"; "SF"];
      ]
      ());
  [%expect_exact {|
┌──────────────────────────────┐
│ Name   Age  City             │
│ Alice  30   NYC              │
│ Bob    25   SF               │
│                              │
│                              │
│                              │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with ascii box style" =
  print_ui ~width:20 ~height:5
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[["1"; "2"]]
      ~box_style:Ui.Table.Ascii
      ());
  [%expect_exact {|
┌────────────────────┐
│+---+---+           │
│| A | B |           │
│+---+---+           │
│| 1 | 2 |           │
│+---+---+           │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with double box style" =
  print_ui ~width:20 ~height:5
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[["1"; "2"]]
      ~box_style:Ui.Table.Double
      ());
  [%expect_exact {|
┌────────────────────┐
│╔═══╦═══╗           │
│║ A ║ B ║           │
│╠═══╬═══╣           │
│║ 1 ║ 2 ║           │
│╚═══╩═══╝           │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with minimal box style" =
  print_ui ~width:20 ~height:5
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[["1"; "2"]]
      ~box_style:Ui.Table.Minimal
      ());
  [%expect_exact {|
┌────────────────────┐
│                    │
│  A   B             │
│                    │
│  1   2             │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with custom column alignment" =
  print_ui ~width:40 ~height:8
    (Ui.Table.table
      ~columns:[
        (let c = Ui.Table.default_column ~header:"Left" in
         { c with justify = `Left });
        (let c = Ui.Table.default_column ~header:"Center" in
         { c with justify = `Center });
        (let c = Ui.Table.default_column ~header:"Right" in
         { c with justify = `Right });
      ]
      ~rows:[
        ["Left"; "Center"; "Right"];
        ["L"; "C"; "R"];
      ]
      ());
  [%expect_exact {|
┌────────────────────────────────────────┐
│┏━━━━━━┳━━━━━━━━┳━━━━━━━┓               │
││ Left   Center │ Right │               │
│├──────┼────────┼───────┤               │
││ Left   Center │ Right │               │
││ L        C    │     R │               │
│└──────┴────────┴───────┘               │
│                                        │
│                                        │
└────────────────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with title and caption" =
  print_ui ~width:30 ~height:10
    (Ui.Table.table
      ~title:(Some "Sales Report")
      ~caption:(Some "Q4 2023")
      ~columns:[
        Ui.Table.default_column ~header:"Month";
        Ui.Table.default_column ~header:"Total";
      ]
      ~rows:[
        ["Oct"; "$45K"];
        ["Nov"; "$52K"];
      ]
      ~title_style:Ui.Style.(fg Blue ++ bold)
      ~caption_style:Ui.Style.(fg (Index 8))
      ());
  [%expect_exact {|
┌──────────────────────────────┐
│   Sales Report               │
│┏━━━━━━━┳━━━━━━━┓             │
││ Month │ Total │             │
│├───────┼───────┤             │
││ Oct   │ $45K  │             │
││ Nov   │ $52K  │             │
│└───────┴───────┘             │
│     Q4 2023                  │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with padding" =
  print_ui ~width:30 ~height:8
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[["1"; "2"]]
      ~padding:(2, 3, 2, 3)
      ());
  [%expect_exact {|
┌──────────────────────────────┐
│┏━━━━━━━┳━━━━━━━┓             │
││       │       │             │
││       │       │             │
││   A   │   B   │             │
││       │       │             │
││       │       │             │
│├───────┼───────┤             │
││       │       │             │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with footer" =
  print_ui ~width:35 ~height:10
    (Ui.Table.table
      ~columns:[
        (let c = Ui.Table.default_column ~header:"Item" in
         { c with footer = Some "Total:" });
        (let c = Ui.Table.default_column ~header:"Count" in
         { c with footer = Some "5"; justify = `Right });
      ]
      ~rows:[
        ["Apples"; "2"];
        ["Oranges"; "3"];
      ]
      ~show_footer:true
      ~footer_style:Ui.Style.(bold)
      ());
  [%expect_exact {|
┌───────────────────────────────────┐
│┏━━━━━━━━━┳━━━━━━━┓                │
││ Item    │ Count │                │
│├─────────┼───────┤                │
││ Apples  │     2 │                │
││ Oranges │     3 │                │
│├─────────┼───────┤                │
││ Total:  │ 5     │                │
│└─────────┴───────┘                │
│                                   │
│                                   │
└───────────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with lines between rows" =
  print_ui ~width:25 ~height:10
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[
        ["1"; "2"];
        ["3"; "4"];
        ["5"; "6"];
      ]
      ~show_lines:true
      ());
  [%expect_exact {|
┌─────────────────────────┐
│┏━━━┳━━━┓                │
││ A │ B │                │
│├───┼───┤                │
││ 1 │ 2 │                │
│├───┼───┤                │
││ 3 │ 4 │                │
│├───┼───┤                │
││ 5 │ 6 │                │
│└───┴───┘                │
│                         │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with alternating row styles" =
  print_ui ~width:25 ~height:8
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"Name";
        Ui.Table.default_column ~header:"ID";
      ]
      ~rows:[
        ["Alice"; "001"];
        ["Bob"; "002"];
        ["Carol"; "003"];
      ]
      ~row_styles:[
        Ui.Style.empty;
        Ui.Style.(bg (Index 236));
      ]
      ());
  [%expect_exact {|
┌─────────────────────────┐
│┏━━━━━━━┳━━━━━┓          │
││ Name  │ ID  │          │
│├───────┼─────┤          │
││ Alice │ 001 │          │
││ Bob   │ 002 │          │
││ Carol │ 003 │          │
│└───────┴─────┘          │
│                         │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with text overflow" =
  print_ui ~width:30 ~height:10
    (Ui.Table.table
      ~columns:[
        (let c = Ui.Table.default_column ~header:"Ellipsis" in
         { c with max_width = Some 8; overflow = `Ellipsis });
        (let c = Ui.Table.default_column ~header:"Crop" in
         { c with max_width = Some 6; overflow = `Crop });
        (let c = Ui.Table.default_column ~header:"Fold" in
         { c with max_width = Some 6; overflow = `Fold });
      ]
      ~rows:[
        ["Very long text"; "Long text"; "More text"];
        ["Short"; "OK"; "Good"];
      ]
      ());
  [%expect_exact {|
┌──────────────────────────────┐
│┏━━━━━━━━┳━━━━━━┳━━━━━━┓      │
││ Ell...   Crop │ Fold │      │
│├────────┼──────┼──────┤      │
││ Ver...   Long │ More │      │
││               │ text │      │
││ Short    OK   │ Good │      │
│└────────┴──────┴──────┘      │
│                              │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with leading (row spacing)" =
  print_ui ~width:20 ~height:12
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[
        ["1"; "2"];
        ["3"; "4"];
      ]
      ~leading:1
      ());
  [%expect_exact {|
┌────────────────────┐
│┏━━━┳━━━┓           │
││ A │ B │           │
│├───┼───┤           │
││ 1 │ 2 │           │
││   │   │           │
││ 3 │ 4 │           │
│└───┴───┘           │
│                    │
│                    │
│                    │
│                    │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with min_width constraint" =
  print_ui ~width:50 ~height:6
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[["1"; "2"]]
      ~min_width:(Some 30)
      ());
  [%expect_exact {|
┌──────────────────────────────────────────────────┐
│           ┏━━━┳━━━┓                              │
│           │ A │ B │                              │
│           ├───┼───┤                              │
│           │ 1 │ 2 │                              │
│           └───┴───┘                              │
│                                                  │
└──────────────────────────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with vertical alignment" =
  print_ui ~width:25 ~height:8
    (Ui.Table.table
      ~columns:[
        (let c = Ui.Table.default_column ~header:"Top" in
         { c with vertical = `Top });
        (let c = Ui.Table.default_column ~header:"Middle" in
         { c with vertical = `Middle });
        (let c = Ui.Table.default_column ~header:"Bottom" in
         { c with vertical = `Bottom });
      ]
      ~rows:[
        ["T"; "M"; "B"];
      ]
      ());
  [%expect_exact {|
┌─────────────────────────┐
│┏━━━━━┳━━━━━━━━┳━━━━━━━━┓│
││ Top   Middle │ Bottom ││
│├─────┼────────┼────────┤│
││ T     M      │ B      ││
│└─────┴────────┴────────┘│
│                         │
│                         │
│                         │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "grid table (no borders)" =
  print_ui ~width:30 ~height:6
    (Ui.Table.grid_table
      ~columns:[
        Ui.Table.default_column ~header:"";
        Ui.Table.default_column ~header:"";
      ]
      ~rows:[
        ["Name:"; "John Doe"];
        ["Email:"; "john@example.com"];
      ]);
  [%expect_exact {|
┌──────────────────────────────┐
│Name:  John Doe               │
│Email: john@example.com       │
│                              │
│                              │
│                              │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with custom column widths" =
  print_ui ~width:35 ~height:8
    (Ui.Table.table
      ~columns:[
        (let c = Ui.Table.default_column ~header:"Fixed" in
         { c with min_width = Some 10; max_width = Some 10 });
        (let c = Ui.Table.default_column ~header:"Flexible" in
         { c with min_width = Some 5 });
      ]
      ~rows:[
        ["ABC"; "This is flexible content"];
        ["XYZ"; "Short"];
      ]
      ());
  [%expect_exact {|
┌───────────────────────────────────┐
│┏━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━│
││ Fixed │ Flexible                 │
│├───────┼──────────────────────────│
││ ABC   │ This is flexible content │
││ XYZ   │ Short                    │
│└───────┴──────────────────────────│
│                                   │
│                                   │
└───────────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with border style" =
  print_ui ~width:25 ~height:6
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[["1"; "2"]]
      ~border_style:Ui.Style.(fg Blue)
      ());
  [%expect_exact {|
┌─────────────────────────┐
│┏━━━┳━━━┓                │
││ A │ B │                │
│├───┼───┤                │
││ 1 │ 2 │                │
│└───┴───┘                │
│                         │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with header style" =
  print_ui ~width:25 ~height:6
    (Ui.Table.table
      ~columns:[
        (let c = Ui.Table.default_column ~header:"Name" in
         { c with header_style = Ui.Style.(fg Yellow ++ bold) });
        (let c = Ui.Table.default_column ~header:"Value" in
         { c with header_style = Ui.Style.(fg Green ++ bold) });
      ]
      ~rows:[["Item1"; "100"]]
      ());
  [%expect_exact {|
┌─────────────────────────┐
│┏━━━━━━━┳━━━━━━━┓        │
││ Name  │ Value │        │
│├───────┼───────┤        │
││ Item1 │ 100   │        │
│└───────┴───────┘        │
│                         │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with expand" =
  print_ui ~width:40 ~height:6
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[["1"; "2"]]
      ~expand:true
      ());
  [%expect_exact {|
┌────────────────────────────────────────┐
│┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━│
││ A                                     │
│├───────────────────────────────────────│
││ 1                                     │
│└───────────────────────────────────────│
│                                        │
└────────────────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with collapse_padding" =
  print_ui ~width:25 ~height:6
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
        Ui.Table.default_column ~header:"C";
      ]
      ~rows:[["1"; "2"; "3"]]
      ~padding:(1, 2, 1, 2)
      ~collapse_padding:true
      ());
  [%expect_exact {|
┌─────────────────────────┐
│┏━━━━━┳━━━┳━━━┓          │
││         │   │          │
││  A   B  │C  │          │
││         │   │          │
│├─────┼───┼───┤          │
││  1   2  │3  │          │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table edge case - empty table" =
  print_ui ~width:20 ~height:5
    (Ui.Table.table
      ~columns:[]
      ~rows:[]
      ());
  [%expect_exact {|
┌────────────────────┐
│                    │
│                    │
│                    │
│                    │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table edge case - headers only" =
  print_ui ~width:20 ~height:5
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[]
      ());
  [%expect_exact {|
┌────────────────────┐
│┏━━━┳━━━┓           │
││ A │ B │           │
│├───┼───┤           │
│└───┴───┘           │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with heavy box style" =
  print_ui ~width:20 ~height:6
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[["1"; "2"]]
      ~box_style:Ui.Table.Heavy
      ());
  [%expect_exact {|
┌────────────────────┐
│┏━━━┳━━━┓           │
│┃ A ┃ B ┃           │
│┣━━━╋━━━┫           │
│┃ 1 ┃ 2 ┃           │
│┗━━━┻━━━┛           │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with double edge box style" =
  print_ui ~width:20 ~height:6
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[["1"; "2"]]
      ~box_style:Ui.Table.DoubleEdge
      ());
  [%expect_exact {|
┌────────────────────┐
│╔═══╤═══╗           │
│║ A ║ B ║           │
││───┼───│           │
│║ 1 ║ 2 ║           │
│╚═══╧═══╝           │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with rounded box style" =
  print_ui ~width:20 ~height:6
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"A";
        Ui.Table.default_column ~header:"B";
      ]
      ~rows:[["1"; "2"]]
      ~box_style:Ui.Table.Rounded
      ());
  [%expect_exact {|
┌────────────────────┐
│╭───┬───╮           │
││ A │ B │           │
││───┼───│           │
││ 1 │ 2 │           │
│╰───┴───╯           │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with width constraint" =
  print_ui ~width:50 ~height:6
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"Name";
        Ui.Table.default_column ~header:"Value";
      ]
      ~rows:[["Item"; "100"]]
      ~width:(Some 25)
      ());
  [%expect_exact {|
┌──────────────────────────────────────────────────┐
│┏━━━━━━┳━━━━━━━┓                                  │
││ Name │ Value │                                  │
│├──────┼───────┤                                  │
││ Item │ 100   │                                  │
│└──────┴───────┘                                  │
│                                                  │
└──────────────────────────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with column styles" =
  print_ui ~width:30 ~height:6
    (Ui.Table.table
      ~columns:[
        (let c = Ui.Table.default_column ~header:"Product" in
         { c with style = Ui.Style.(fg Green) });
        (let c = Ui.Table.default_column ~header:"Price" in
         { c with style = Ui.Style.(fg Yellow ++ bold) });
      ]
      ~rows:[
        ["Widget"; "$19.99"];
        ["Gadget"; "$49.99"];
      ]
      ());
  [%expect_exact {|
┌──────────────────────────────┐
│┏━━━━━━━━━┳━━━━━━━━┓          │
││ Product │ Price  │          │
│├─────────┼────────┤          │
││ Widget  │ $19.99 │          │
││ Gadget  │ $49.99 │          │
│└─────────┴────────┘          │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with complex column configuration" =
  print_ui ~width:40 ~height:8
    (Ui.Table.table
      ~columns:[
        (let c = Ui.Table.default_column ~header:"Product" in
         { c with 
           justify = `Left; 
           min_width = Some 15;
           style = Ui.Style.(fg Cyan) 
         });
        (let c = Ui.Table.default_column ~header:"Price" in
         { c with 
           justify = `Right; 
           style = Ui.Style.(fg Green);
           header_style = Ui.Style.(fg Green ++ bold)
         });
      ]
      ~rows:[
        ["Widget A"; "$19.99"];
        ["Gadget B"; "$49.99"];
      ]
      ());
  [%expect_exact {|
┌────────────────────────────────────────┐
│┏━━━━━━━━━━┳━━━━━━━━┓                   │
││ Product  │ Price  │                   │
│├──────────┼────────┤                   │
││ Widget A │ $19.99 │                   │
││ Gadget B │ $49.99 │                   │
│└──────────┴────────┘                   │
│                                        │
│                                        │
└────────────────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with row styles wraparound" =
  print_ui ~width:25 ~height:10
    (Ui.Table.table
      ~columns:[
        Ui.Table.default_column ~header:"ID";
        Ui.Table.default_column ~header:"Name";
      ]
      ~rows:[
        ["1"; "Alice"];
        ["2"; "Bob"];
        ["3"; "Carol"];
        ["4"; "Dave"];
        ["5"; "Eve"];
      ]
      ~row_styles:[
        Ui.Style.empty;
        Ui.Style.(bg (Index 236));
      ]
      ());
  [%expect_exact {|
┌─────────────────────────┐
│┏━━━━┳━━━━━━━┓           │
││ ID │ Name  │           │
│├────┼───────┤           │
││ 1  │ Alice │           │
││ 2  │ Bob   │           │
││ 3  │ Carol │           │
││ 4  │ Dave  │           │
││ 5  │ Eve   │           │
│└────┴───────┘           │
│                         │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "table with complex features combined" =
  print_ui ~width:50 ~height:12
    (Ui.Table.table
      ~title:(Some "Complex Table")
      ~columns:[
        (let c = Ui.Table.default_column ~header:"Name" in
         { c with 
           min_width = Some 10; 
           header_style = Ui.Style.(fg Blue ++ bold);
           footer = Some "Total" 
         });
        (let c = Ui.Table.default_column ~header:"Value" in
         { c with 
           justify = `Right;
           style = Ui.Style.(fg Green);
           footer = Some "$150"
         });
      ]
      ~rows:[
        ["Item A"; "$50"];
        ["Item B"; "$100"];
      ]
      ~box_style:Ui.Table.DoubleEdge
      ~show_footer:true
      ~show_lines:true
      ~padding:(1, 2, 1, 2)
      ~border_style:Ui.Style.(fg Blue)
      ~title_style:Ui.Style.(fg Bright_white ++ bold)
      ~footer_style:Ui.Style.(fg Yellow ++ bold)
      ());
  [%expect_exact {|
┌──────────────────────────────────────────────────┐
│     Complex Table                                │
│╔══════════╤═════════╗                            │
│║          ║         ║                            │
│║  Name    ║  Value  ║                            │
│║          ║         ║                            │
││──────────┼─────────│                            │
│║          ║         ║                            │
│║  Item A  ║    $50  ║                            │
│║          ║         ║                            │
││──────────┼─────────│                            │
│║          ║         ║                            │
│║  Item B  ║   $100  ║                            │
└──────────────────────────────────────────────────┘
|}] [@@ocamlformat "disable"]
