open Mosaic_ui

let render_boxed ?(width = 30) ?(height = 8) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors:false ~width:(width + 2) ~height:(height + 2) content

let cell ?style text = Table.cell ?style text
let row ?style values = Table.row ?style (List.map cell values)

let column ?footer ?style ?header_style ?footer_style ?justify ?vertical
    ?overflow ?width ?min_width ?max_width ?ratio ?no_wrap header =
  Table.column ~header:(cell header) ?footer ?style ?header_style ?footer_style
    ?justify ?vertical ?overflow ?width ?min_width ?max_width ?ratio ?no_wrap
    header

let%expect_test "simple table with headers" =
  render_boxed ~width:30 ~height:8
    (table ~id:"t"
       ~columns:[ column "Name"; column "Age"; column "City" ]
       ~rows:[ row [ "Alice"; "30"; "NYC" ]; row [ "Bob"; "25"; "SF" ] ]
       ~box_style:Table.No_box ());
  [%expect_exact
    {|
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
|}]

let%expect_test "table with ascii box style" =
  render_boxed ~width:20 ~height:5
    (table ~id:"t"
       ~columns:[ column "A"; column "B" ]
       ~rows:[ row [ "1"; "2" ] ]
       ~box_style:Table.Ascii ());
  [%expect_exact
    {|
┌────────────────────┐
│+---+---+           │
│| A | B |           │
│+---+---+           │
│| 1 | 2 |           │
│+---+---+           │
└────────────────────┘
|}]

let%expect_test "table with double box style" =
  render_boxed ~width:20 ~height:5
    (table ~id:"t"
       ~columns:[ column "A"; column "B" ]
       ~rows:[ row [ "1"; "2" ] ]
       ~box_style:Table.Double ());
  [%expect_exact
    {|
┌────────────────────┐
│╔═══╦═══╗           │
│║ A ║ B ║           │
│╠═══╬═══╣           │
│║ 1 ║ 2 ║           │
│╚═══╩═══╝           │
└────────────────────┘
|}]

let%expect_test "table with minimal box style" =
  render_boxed ~width:20 ~height:5
    (table ~id:"t"
       ~columns:[ column "A"; column "B" ]
       ~rows:[ row [ "1"; "2" ] ]
       ~box_style:Table.Minimal ());
  [%expect_exact
    {|
┌────────────────────┐
│─── ───             │
│  A   B             │
│                    │
│  1   2             │
│                    │
└────────────────────┘
|}]

let%expect_test "table with custom column alignment" =
  let columns =
    [
      column ~justify:`Left "Left";
      column ~justify:`Center "Center";
      column ~justify:`Right "Right";
    ]
  in
  render_boxed ~width:40 ~height:8
    (table ~id:"t" ~columns
       ~rows:[ row [ "Left"; "Center"; "Right" ]; row [ "L"; "C"; "R" ] ]
       ());
  [%expect_exact
    {|
┌────────────────────────────────────────┐
│┏━━━━━━┳━━━━━━━━┳━━━━━━━┓               │
││ Left │ Center │ Right │               │
│├──────┼────────┼───────┤               │
││ Left │ Center │ Right │               │
││ L    │   C    │     R │               │
│└──────┴────────┴───────┘               │
│                                        │
│                                        │
└────────────────────────────────────────┘
|}]

let%expect_test "table with title and caption" =
  let title_style = Ansi.Style.make ~fg:Ansi.Color.blue ~bold:true () in
  let caption_style =
    Ansi.Style.make ~fg:(Ansi.Color.of_rgba 128 128 128 255) ()
  in
  render_boxed ~width:30 ~height:10
    (table ~id:"t" ~title:(cell "Sales Report") ~caption:(cell "Q4 2023")
       ~columns:[ column "Month"; column "Total" ]
       ~rows:[ row [ "Oct"; "$45K" ]; row [ "Nov"; "$52K" ] ]
       ~title_style ~caption_style ());
  [%expect_exact
    {|
┌──────────────────────────────┐
│  Sales Report                │
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
|}]

let%expect_test "table with padding" =
  (* Increased height to show body row with padding applied *)
  render_boxed ~width:30 ~height:14
    (table ~id:"t"
       ~columns:[ column "A"; column "B" ]
       ~rows:[ row [ "1"; "2" ] ]
       ~table_padding:(2, 3, 2, 3) ());
  [%expect_exact
    {|
┌──────────────────────────────┐
│┏━━━━━━━┳━━━━━━━┓             │
││       │       │             │
││       │       │             │
││   A   │   B   │             │
││       │       │             │
││       │       │             │
│├───────┼───────┤             │
││       │       │             │
││       │       │             │
││   1   │   2   │             │
││       │       │             │
││       │       │             │
│└───────┴───────┘             │
│                              │
└──────────────────────────────┘
|}]

let%expect_test "table with footer" =
  let footer_style = Ansi.Style.make ~bold:true () in
  let columns =
    [
      column ~footer:(cell "Total:") "Item";
      column ~footer:(cell "5") ~justify:`Right "Count";
    ]
  in
  render_boxed ~width:35 ~height:10
    (table ~id:"t" ~columns
       ~rows:[ row [ "Apples"; "2" ]; row [ "Oranges"; "3" ] ]
       ~show_footer:true ~footer_style ());
  [%expect_exact
    {|
┌───────────────────────────────────┐
│┏━━━━━━━━━┳━━━━━━━┓                │
││ Item    │ Count │                │
│├─────────┼───────┤                │
││ Apples  │     2 │                │
││ Oranges │     3 │                │
│├─────────┼───────┤                │
││ Total:  │     5 │                │
│└─────────┴───────┘                │
│                                   │
│                                   │
└───────────────────────────────────┘
|}]

let%expect_test "table with lines between rows" =
  render_boxed ~width:25 ~height:10
    (table ~id:"t"
       ~columns:[ column "A"; column "B" ]
       ~rows:[ row [ "1"; "2" ]; row [ "3"; "4" ]; row [ "5"; "6" ] ]
       ~show_lines:true ());
  [%expect_exact
    {|
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
|}]

let%expect_test "table with alternating row styles (layout smoke test)" =
  (* Note: This test verifies layout only. Colors are disabled so alternating
     row styles are not visible in output. Use colors:true for style verification. *)
  let gray = Ansi.Style.make ~bg:(Ansi.Color.of_rgba 80 80 80 255) () in
  render_boxed ~width:25 ~height:8
    (table ~id:"t"
       ~columns:[ column "Name"; column "ID" ]
       ~rows:
         [
           row [ "Alice"; "001" ]; row [ "Bob"; "002" ]; row [ "Carol"; "003" ];
         ]
       ~row_styles:[ Ansi.Style.default; gray ]
       ());
  [%expect_exact
    {|
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
|}]

let%expect_test "table with text overflow" =
  let columns =
    [
      column ~overflow:`Ellipsis ~max_width:8 "Ellipsis";
      column ~overflow:`Crop ~max_width:6 "Crop";
      column ~overflow:`Fold ~max_width:6 "Fold";
    ]
  in
  render_boxed ~width:30 ~height:10
    (table ~id:"t" ~columns
       ~rows:
         [
           row [ "Very long text"; "Long text"; "More text" ];
           row [ "Short"; "OK"; "Good" ];
         ]
       ());
  [%expect_exact
    {|
┌──────────────────────────────┐
│┏━━━━━━━━┳━━━━━━┳━━━━━━┓      │
││ Ell... │ Crop │ Fold │      │
│├────────┼──────┼──────┤      │
││ Ver... │ Long │ More │      │
││        │      │ text │      │
││ Short  │ OK   │ Good │      │
│└────────┴──────┴──────┘      │
│                              │
│                              │
│                              │
└──────────────────────────────┘
|}]

let%expect_test "table with leading (row spacing)" =
  render_boxed ~width:20 ~height:12
    (table ~id:"t"
       ~columns:[ column "A"; column "B" ]
       ~rows:[ row [ "1"; "2" ]; row [ "3"; "4" ] ]
       ~leading:1 ());
  [%expect_exact
    {|
┌────────────────────┐
│┏━━━┳━━━┓           │
││ A │ B │           │
│├───┼───┤           │
││ 1 │ 2 │           │
││      │            │
││ 3 │ 4 │           │
│└───┴───┘           │
│                    │
│                    │
│                    │
│                    │
│                    │
└────────────────────┘
|}]

let%expect_test "table with min_width constraint" =
  render_boxed ~width:50 ~height:6
    (table ~id:"t"
       ~columns:[ column "A"; column "B" ]
       ~rows:[ row [ "1"; "2" ] ]
       ~table_min_width:30 ());
  [%expect_exact
    {|
┌──────────────────────────────────────────────────┐
│┏━━━┳━━━┓                                         │
││ A │ B │                                         │
│├───┼───┤                                         │
││ 1 │ 2 │                                         │
│└───┴───┘                                         │
│                                                  │
└──────────────────────────────────────────────────┘
|}]

let%expect_test "table with vertical alignment" =
  (* Use overflow:Fold with max_width to create multi-line cells that show
     vertical alignment differences *)
  let columns =
    [
      column ~vertical:`Top ~overflow:`Fold ~max_width:6 "Top";
      column ~vertical:`Middle ~overflow:`Fold ~max_width:6 "Middle";
      column ~vertical:`Bottom ~overflow:`Fold ~max_width:6 "Bottom";
    ]
  in
  render_boxed ~width:30 ~height:10
    (table ~id:"t" ~columns
       ~rows:[ row [ "Line1 Line2 Line3"; "Mid"; "Bot" ] ]
       ());
  [%expect_exact
    {|
┌──────────────────────────────┐
│┏━━━━━━┳━━━━━━┳━━━━━━┓        │
││ Top  │ Midd │ Bott │        │
││      │ le   │ om   │        │
│├──────┼──────┼──────┤        │
││ Line │      │      │        │
││ 1    │      │      │        │
││ Line │ Mid  │      │        │
││ 2    │      │      │        │
││ Line │      │      │        │
││ 3    │      │ Bot  │        │
└──────────────────────────────┘
|}]

let%expect_test "grid table (no borders)" =
  render_boxed ~width:30 ~height:6
    (table ~id:"t"
       ~columns:[ column ""; column "" ]
       ~rows:
         [ row [ "Name:"; "John Doe" ]; row [ "Email:"; "john@example.com" ] ]
       ~box_style:Table.No_box ~show_edge:false ());
  [%expect_exact
    {|
┌──────────────────────────────┐
│                              │
│ Name:   John Doe             │
│ Email:  john@example.com     │
│                              │
│                              │
│                              │
└──────────────────────────────┘
|}]

let%expect_test "table with custom column widths" =
  let columns =
    [
      column ~min_width:10 ~max_width:10 "Fixed"; column ~min_width:5 "Flexible";
    ]
  in
  render_boxed ~width:35 ~height:8
    (table ~id:"t" ~columns
       ~rows:
         [ row [ "ABC"; "This is flexible content" ]; row [ "XYZ"; "Short" ] ]
       ());
  [%expect_exact
    {|
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
|}]

let%expect_test "table with header style (layout smoke test)" =
  (* Note: This test verifies layout only. Colors are disabled so styles
     are not visible in output. Use colors:true for style verification. *)
  let yellow = Ansi.Style.make ~fg:Ansi.Color.yellow ~bold:true () in
  let green = Ansi.Style.make ~fg:Ansi.Color.green ~bold:true () in
  render_boxed ~width:25 ~height:6
    (table ~id:"t"
       ~columns:
         [
           column ~header_style:yellow "Name";
           column ~header_style:green "Value";
         ]
       ~rows:[ row [ "Item1"; "100" ] ]
       ());
  [%expect_exact
    {|
┌─────────────────────────┐
│┏━━━━━━━┳━━━━━━━┓        │
││ Name  │ Value │        │
│├───────┼───────┤        │
││ Item1 │ 100   │        │
│└───────┴───────┘        │
│                         │
└─────────────────────────┘
|}]

let%expect_test "table with expand" =
  let columns = [ column ~ratio:1 "A"; column ~ratio:2 "B" ] in
  render_boxed ~width:40 ~height:6
    (table ~id:"t" ~columns
       ~rows:[ row [ "1"; "2" ] ]
       ~table_width:30 ~expand:true ());
  [%expect_exact
    {|
┌────────────────────────────────────────┐
│┏━━━━━━━━━┳━━━━━━━━━━━━━━━━━━┓          │
││ A       │ B                │          │
│├─────────┼──────────────────┤          │
││ 1       │ 2                │          │
│└─────────┴──────────────────┘          │
│                                        │
└────────────────────────────────────────┘
|}]

let%expect_test "table with collapse_padding" =
  render_boxed ~width:25 ~height:6
    (table ~id:"t"
       ~columns:[ column "A"; column "B"; column "C" ]
       ~rows:[ row [ "1"; "2"; "3" ] ]
       ~table_padding:(1, 2, 1, 2) ~collapse_padding:true ());
  [%expect_exact
    {|
┌─────────────────────────┐
│┏━━━━━┳━━━┳━━━┓          │
││     │   │   │          │
││  A  │B  │C  │          │
││     │   │   │          │
│├─────┼───┼───┤          │
││  1  │2  │3  │          │
└─────────────────────────┘
|}]

let%expect_test "table edge case - empty table" =
  render_boxed ~width:20 ~height:5 (table ~id:"t" ~columns:[] ~rows:[] ());
  [%expect_exact
    {|
┌────────────────────┐
│┏┓                  │
│├┤                  │
│└┘                  │
│                    │
│                    │
└────────────────────┘
|}]

let%expect_test "table edge case - headers only" =
  render_boxed ~width:20 ~height:5
    (table ~id:"t" ~columns:[ column "A"; column "B" ] ~rows:[] ());
  [%expect_exact
    {|
┌────────────────────┐
│┏━━━┳━━━┓           │
││ A │ B │           │
│├───┼───┤           │
│└───┴───┘           │
│                    │
└────────────────────┘
|}]

let%expect_test "table with heavy box style" =
  render_boxed ~width:20 ~height:6
    (table ~id:"t"
       ~columns:[ column "A"; column "B" ]
       ~rows:[ row [ "1"; "2" ] ]
       ~box_style:Table.Heavy ());
  [%expect_exact
    {|
┌────────────────────┐
│┏━━━┳━━━┓           │
│┃ A ┃ B ┃           │
│┣━━━╋━━━┫           │
│┃ 1 ┃ 2 ┃           │
│┗━━━┻━━━┛           │
│                    │
└────────────────────┘
|}]

let%expect_test "table with double edge box style" =
  render_boxed ~width:20 ~height:6
    (table ~id:"t"
       ~columns:[ column "A"; column "B" ]
       ~rows:[ row [ "1"; "2" ] ]
       ~box_style:Table.Double_edge ());
  [%expect_exact
    {|
┌────────────────────┐
│╔═══╤═══╗           │
│║ A ║ B ║           │
││───┼───│           │
│║ 1 ║ 2 ║           │
│╚═══╧═══╝           │
│                    │
└────────────────────┘
|}]

let%expect_test "table with rounded box style" =
  render_boxed ~width:20 ~height:6
    (table ~id:"t"
       ~columns:[ column "A"; column "B" ]
       ~rows:[ row [ "1"; "2" ] ]
       ~box_style:Table.Rounded ());
  [%expect_exact
    {|
┌────────────────────┐
│╭───┬───╮           │
││ A │ B │           │
││───┼───│           │
││ 1 │ 2 │           │
│╰───┴───╯           │
│                    │
└────────────────────┘
|}]

let%expect_test "table with width constraint" =
  render_boxed ~width:50 ~height:6
    (table ~id:"t"
       ~columns:[ column "Name"; column "Value" ]
       ~rows:[ row [ "Item"; "100" ] ]
       ~table_width:25 ());
  [%expect_exact
    {|
┌──────────────────────────────────────────────────┐
│┏━━━━━━┳━━━━━━━┓                                  │
││ Name │ Value │                                  │
│├──────┼───────┤                                  │
││ Item │ 100   │                                  │
│└──────┴───────┘                                  │
│                                                  │
└──────────────────────────────────────────────────┘
|}]

let%expect_test "table with column styles (layout smoke test)" =
  (* Note: This test verifies layout only. Colors are disabled so styles
     are not visible in output. Use colors:true for style verification. *)
  let green = Ansi.Style.make ~fg:Ansi.Color.green () in
  let yellow = Ansi.Style.make ~fg:Ansi.Color.yellow ~bold:true () in
  render_boxed ~width:30 ~height:6
    (table ~id:"t"
       ~columns:[ column ~style:green "Product"; column ~style:yellow "Price" ]
       ~rows:[ row [ "Widget"; "$19.99" ]; row [ "Gadget"; "$49.99" ] ]
       ());
  [%expect_exact
    {|
┌──────────────────────────────┐
│┏━━━━━━━━━┳━━━━━━━━┓          │
││ Product │ Price  │          │
│├─────────┼────────┤          │
││ Widget  │ $19.99 │          │
││ Gadget  │ $49.99 │          │
│└─────────┴────────┘          │
└──────────────────────────────┘
|}]

let%expect_test "table with row styles wraparound (layout smoke test)" =
  (* Note: This test verifies layout only. Colors are disabled so alternating
     row styles are not visible in output. Use colors:true for style verification. *)
  let highlight = Ansi.Style.make ~bg:(Ansi.Color.of_rgba 236 236 236 255) () in
  render_boxed ~width:25 ~height:10
    (table ~id:"t"
       ~columns:[ column "ID"; column "Name" ]
       ~rows:
         [
           row [ "1"; "Alice" ];
           row [ "2"; "Bob" ];
           row [ "3"; "Carol" ];
           row [ "4"; "Dave" ];
           row [ "5"; "Eve" ];
         ]
       ~row_styles:[ Ansi.Style.default; highlight ]
       ());
  [%expect_exact
    {|
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
|}]

let%expect_test "table with complex features combined" =
  let title_style = Ansi.Style.make ~fg:Ansi.Color.bright_white ~bold:true () in
  let footer_style = Ansi.Style.make ~fg:Ansi.Color.yellow ~bold:true () in
  let border_style = Ansi.Style.make ~fg:Ansi.Color.blue () in
  let columns =
    [
      column ~min_width:10
        ~header_style:(Ansi.Style.make ~fg:Ansi.Color.blue ~bold:true ())
        ~footer:(cell "Total") "Name";
      column ~justify:`Right
        ~style:(Ansi.Style.make ~fg:Ansi.Color.green ())
        ~footer:(cell "$150") "Value";
    ]
  in
  (* Increased height to show footer section *)
  render_boxed ~width:50 ~height:22
    (table ~id:"t" ~columns ~title:(cell "Complex Table")
       ~rows:[ row [ "Item A"; "$50" ]; row [ "Item B"; "$100" ] ]
       ~box_style:Table.Double_edge ~show_footer:true ~show_lines:true
       ~table_padding:(1, 2, 1, 2) ~border_style ~title_style ~footer_style ());
  [%expect_exact
    {|
┌──────────────────────────────────────────────────┐
│    Complex Table                                 │
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
│║          ║         ║                            │
││──────────┼─────────│                            │
│║          ║         ║                            │
│║  Total   ║   $150  ║                            │
│║          ║         ║                            │
│╚══════════╧═════════╝                            │
│                                                  │
│                                                  │
│                                                  │
│                                                  │
└──────────────────────────────────────────────────┘
|}]
