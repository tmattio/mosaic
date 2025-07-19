open Mosaic
open Test_utils

let%expect_test "text rendering" =
  print_ui (Ui.text "Hello");
  [%expect_exact {|
+--------------------+
|Hello               |
|                    |
|                    |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "style application" =
  print_ui ~width:10 ~height:1
    (Ui.text ~style:Style.(fg (Index 5) ++ bold) "Test");
  [%expect_exact {|
+----------+
|Test      |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "hbox layout" =
  print_ui (Ui.hbox [ Ui.text "A"; Ui.text "B"; Ui.text "C" ]);
  [%expect_exact {|
+--------------------+
|ABC                 |
|                    |
|                    |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "hbox with gap" =
  print_ui (Ui.hbox ~gap:2 [ Ui.text "A"; Ui.text "B"; Ui.text "C" ]);
  [%expect_exact {|
+--------------------+
|A  B  C             |
|                    |
|                    |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "vbox layout" =
  print_ui ~width:10 ~height:3
    (Ui.vbox [ Ui.text "Line1"; Ui.text "Line2"; Ui.text "Line3" ]);
  [%expect_exact {|
+----------+
|Line1     |
|Line2     |
|Line3     |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "vbox with gap" =
  print_ui ~width:10 (Ui.vbox ~gap:1 [ Ui.text "A"; Ui.text "B" ]);
  [%expect_exact {|
+----------+
|A         |
|          |
|B         |
|          |
|          |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "padding" =
  print_ui ~width:10 (Ui.hbox ~padding:(Ui.pad ~all:1 ()) [ Ui.text "Test" ]);
  [%expect_exact {|
+----------+
|          |
| Test     |
|          |
|          |
|          |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "asymmetric padding" =
  print_ui ~width:10
    (Ui.hbox ~padding:(Ui.pad ~left:2 ~top:1 ()) [ Ui.text "X" ]);
  [%expect_exact {|
+----------+
|          |
|  X       |
|          |
|          |
|          |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "borders" =
  print_ui ~width:10
    (Ui.hbox
       ~border:(Ui.border ~style:Solid ())
       ~width:10 ~height:5
       [ Ui.text "Hi" ]);
  [%expect_exact {|
+----------+
|┌────────┐|
|│Hi      │|
|│        │|
|│        │|
|└────────┘|
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "per-side borders - top and bottom only" =
  print_ui ~width:10
    (Ui.hbox
       ~border:(Ui.border ~left:false ~right:false ())
       ~width:10 ~height:5
       [ Ui.text "Test" ]);
  [%expect_exact {|
+----------+
|──────────|
|Test      |
|          |
|          |
|──────────|
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "per-side borders - left only" =
  print_ui ~width:5 ~height:5
    (Ui.vbox
       ~border:(Ui.border ~top:false ~bottom:false ~right:false ())
       ~width:5 ~height:5 []);
  [%expect_exact {|
+-----+
|│    |
|│    |
|│    |
|│    |
|│    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "alignment - center" =
  print_ui ~width:10 ~height:3
    (Ui.vbox ~align_items:`Center ~width:10 [ Ui.text "Hi" ]);
  [%expect_exact {|
+----------+
|    Hi    |
|          |
|          |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "alignment - end" =
  print_ui ~width:10 ~height:1
    (Ui.vbox ~align_items:`End ~width:10 [ Ui.text "End" ]);
  [%expect_exact {|
+----------+
|       End|
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "justify - center" =
  print_ui ~width:5 ~height:10
    (Ui.vbox ~justify_content:`Center ~height:10 [ Ui.text "A"; Ui.text "B" ]);
  [%expect_exact {|
+-----+
|     |
|     |
|     |
|     |
|A    |
|B    |
|     |
|     |
|     |
|     |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "expand/flex_spacer" =
  print_ui ~height:1 (Ui.hbox [ Ui.text "A"; Ui.flex_spacer (); Ui.text "B" ]);
  [%expect_exact {|
+--------------------+
|A                  B|
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "fixed dimensions" =
  print_ui ~width:20 ~height:10
    (Ui.hbox ~width:10 ~height:5 ~border:(Ui.border ()) []);
  [%expect_exact {|
+--------------------+
|┌────────┐          |
|│        │          |
|│        │          |
|│        │          |
|└────────┘          |
|                    |
|                    |
|                    |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "nested layout" =
  print_ui
    (Ui.vbox
       [
         Ui.text "Header";
         Ui.hbox ~border:(Ui.border ())
           [
             Ui.vbox [ Ui.text "A1"; Ui.text "A2" ];
             Ui.vbox [ Ui.text "B1"; Ui.text "B2" ];
           ];
       ]);
  [%expect_exact {|
+--------------------+
|Header              |
|┌──────────────────┐|
|│A1B1              │|
|│A2B2              │|
|└──────────────────┘|
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "space" =
  print_ui ~width:10 ~height:1
    (Ui.hbox [ Ui.text "A"; Ui.spacer 3; Ui.text "B" ]);
  [%expect_exact {|
+----------+
|A   B     |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "style combination" =
  print_ui ~width:10 ~height:1
    (Ui.text
       ~style:Style.(fg (Index 1) ++ bg (Index 2) ++ bold ++ italic)
       "Test");
  [%expect_exact {|
+----------+
|Test      |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "text alignment - center" =
  print_ui ~height:3 (Ui.text ~align:`Center "Hello\nWorld\n!");
  [%expect_exact {|
+--------------------+
|       Hello        |
|       World        |
|         !          |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "tab expansion" =
  print_ui ~height:1 (Ui.text ~tab_width:4 "A\tB");
  [%expect_exact {|
+--------------------+
|A   B               |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "adaptive colors" =
  let adaptive = Style.adaptive ~light:Style.Black ~dark:Style.White in
  print_ui ~width:10 ~height:1
    (Ui.text ~style:(Style.adaptive_fg adaptive) "Test");
  [%expect_exact {|
+----------+
|Test      |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "margins" =
  print_ui ~width:10 (Ui.hbox ~margin:(Ui.pad ~all:1 ()) [ Ui.text "Test" ]);
  [%expect_exact {|
+----------+
|          |
| Test     |
|          |
|          |
|          |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "margin with border" =
  print_ui ~width:10
    (Ui.hbox ~margin:(Ui.pad ~left:2 ~top:1 ()) ~border:(Ui.border ())
       [ Ui.text "X" ]);
  [%expect_exact {|
+----------+
|          |
|  ┌──────┐|
|  │X     │|
|  │      │|
|  └──────┘|
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "zstack - basic overlay" =
  print_ui ~width:10 (Ui.zstack [ Ui.text "AAAAA"; Ui.text "BB" ]);
  [%expect_exact {|
+----------+
|BBAAA     |
|          |
|          |
|          |
|          |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "zstack - with alignment" =
  print_ui ~width:10
    (Ui.zstack ~align:Center
       [ Ui.hbox ~width:10 ~height:5 ~border:(Ui.border ()) []; Ui.text "X" ]);
  [%expect_exact {|
+----------+
|┌────────┐|
|│        │|
|│    X   │|
|│        │|
|└────────┘|
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "grid - 2x2" =
  print_ui ~width:20 ~height:10
    (Ui.grid
       ~columns:[ `Fixed 5; `Fixed 5 ]
       ~rows:[ `Fixed 1; `Fixed 1 ]
       [ Ui.text "A1"; Ui.text "B1"; Ui.text "A2"; Ui.text "B2" ]);
  [%expect_exact {|
+--------------------+
|A1   B1             |
|A2   B2             |
|                    |
|                    |
|                    |
|                    |
|                    |
|                    |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "grid - with spacing" =
  print_ui ~width:20 ~height:10
    (Ui.grid ~col_spacing:2 ~row_spacing:1
       ~columns:[ `Fixed 3; `Fixed 3 ]
       ~rows:[ `Fixed 1; `Fixed 1 ]
       [ Ui.text "X"; Ui.text "Y"; Ui.text "Z"; Ui.text "W" ]);
  [%expect_exact {|
+--------------------+
|X    Y              |
|                    |
|Z    W              |
|                    |
|                    |
|                    |
|                    |
|                    |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "flow - basic wrap" =
  print_ui ~width:10
    (Ui.flow [ Ui.text "AAA"; Ui.text "BBB"; Ui.text "CCC"; Ui.text "DDD" ]);
  [%expect_exact {|
+----------+
|AAA BBB   |
|CCC DDD   |
|          |
|          |
|          |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "flow - with custom gaps" =
  print_ui ~width:7
    (Ui.flow ~h_gap:2 ~v_gap:2 [ Ui.text "XX"; Ui.text "YY"; Ui.text "ZZ" ]);
  [%expect_exact {|
+-------+
|XX  YY |
|       |
|ZZ     |
|       |
|       |
+-------+
|}] [@@ocamlformat "disable"]

let%expect_test "text wrapping" =
  print_ui ~width:7
    (Ui.flow ~h_gap:2 ~v_gap:2 [ Ui.text ~wrap:true "XX YY ZZ" ]);
  [%expect_exact {|
+-------+
|XX  YY |
|ZZ     |
|       |
|       |
|       |
+-------+
|}] [@@ocamlformat "disable"]

let%expect_test "rich text" =
  print_ui ~height:1
    (Ui.rich_text
       [
         ("Red", Style.fg Style.Red);
         (" ", Style.empty);
         ("Bold", Style.bold);
         (" ", Style.empty);
         ("Blue", Style.fg Style.Blue);
       ]);
  [%expect_exact {|
+--------------------+
|Red Bold Blue       |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "scroll - vertical" =
  print_ui ~width:5 ~height:3
    (Ui.scroll ~height:3 ~v_offset:1
       (Ui.vbox
          [
            Ui.text "Line1";
            Ui.text "Line2";
            Ui.text "Line3";
            Ui.text "Line4";
            Ui.text "Line5";
          ]));
  [%expect_exact {|
+-----+
|Line2|
|Line3|
|Line4|
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "scroll - horizontal" =
  print_ui ~width:5 ~height:1
    (Ui.scroll ~width:5 ~h_offset:3 (Ui.text "ABCDEFGHIJ"));
  [%expect_exact {|
+-----+
|DEFGH|
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "measure - simple text" =
  let w, h = Ui.measure (Ui.text "Hello") in
  Printf.printf "Width: %d, Height: %d\n" w h;
  [%expect {| Width: 5, Height: 1 |}] [@@ocamlformat "disable"]

let%expect_test "measure - multiline text" =
  let w, h = Ui.measure (Ui.text "Line1\nLine2\nLine3") in
  Printf.printf "Width: %d, Height: %d\n" w h;
  [%expect {| Width: 5, Height: 3 |}] [@@ocamlformat "disable"]

let%expect_test "measure - box with border" =
  let w, h = Ui.measure (Ui.hbox ~border:(Ui.border ()) [ Ui.text "Hi" ]) in
  Printf.printf "Width: %d, Height: %d\n" w h;
  [%expect {| Width: 4, Height: 3 |}] [@@ocamlformat "disable"]

let%expect_test "edge cases - empty layouts" =
  print_ui ~width:10 ~height:1 (Ui.hbox []);
  [%expect_exact {|
+----------+
|          |
+----------+
|}]; 
  print_ui ~width:10 ~height:1 (Ui.vbox []);
  [%expect_exact {|
+----------+
|          |
+----------+
|}];
  print_ui ~width:10 ~height:1 (Ui.zstack []);
  [%expect_exact {|
+----------+
|          |
+----------+
|}];
  print_ui ~width:10 ~height:1 (Ui.flow []);
  [%expect_exact {|
+----------+
|          |
+----------+
|}];
  print_ui ~width:10 ~height:1 (Ui.grid ~columns:[] ~rows:[] []);
  [%expect_exact {|
+----------+
|          |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "edge cases - zero dimensions" =
  print_ui ~width:10 (Ui.hbox ~width:0 ~height:0 [ Ui.text "Hidden" ]);
  [%expect_exact {|
+----------+
|          |
|          |
|          |
|          |
|          |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "edge cases - negative padding" =
  print_ui ~width:10 (Ui.hbox ~padding:(Ui.pad ~all:(-5) ()) [ Ui.text "X" ]);
  [%expect_exact {|
+----------+
|X         |
|          |
|          |
|          |
|          |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "flex grow - basic" =
  print_ui ~width:30 ~height:1
    (Ui.hbox ~width:30 [ Ui.text "A"; Ui.spacer ~flex:1 0; Ui.text "B" ]);
  [%expect_exact {|
+------------------------------+
|A                            B|
+------------------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "flex grow - weighted" =
  print_ui ~width:20 ~height:1
    (Ui.hbox ~width:20
       [
         Ui.text "X";
         Ui.hbox ~flex_grow:1 [ Ui.text "1" ];
         Ui.hbox ~flex_grow:2 [ Ui.text "2" ];
         Ui.text "Y";
       ]);
  [%expect_exact {|
+--------------------+
|X1     2           Y|
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "flex shrink" =
  print_ui ~width:15 ~height:1
    (Ui.hbox ~width:15
       [ Ui.hbox ~flex_shrink:1 [ Ui.text "Shrinkable" ]; Ui.text " Fixed" ]);
  [%expect_exact {|
+---------------+
|Shrinkab Fixed |
+---------------+
|}] [@@ocamlformat "disable"]

let%expect_test "text wrapping - basic" =
  print_ui ~width:20 ~height:3
    (Ui.text ~wrap:true "Hello world this is a long line that needs wrapping");
  [%expect_exact {|
+--------------------+
|Hello world this is |
|a long line that    |
|needs wrapping      |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "text wrapping - unicode" =
  print_ui ~width:10 ~height:2 (Ui.text ~wrap:true "你好世界 Hello 世界");
  [%expect_exact {|
+----------+
|你好世界  |
|Hello 世界|
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "auto-fill - vbox" =
  print_ui ~width:10 ~height:10
    (Ui.vbox ~height:10 [ Ui.text "Top"; Ui.flex_spacer (); Ui.text "Bottom" ]);
  [%expect_exact {|
+----------+
|Top       |
|          |
|          |
|          |
|          |
|          |
|          |
|          |
|          |
|Bottom    |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "auto-fill - hbox (no auto-fill)" =
  print_ui ~width:20 ~height:1
    (Ui.hbox ~width:20 [ Ui.text "Left"; Ui.text "Right" ]);
  [%expect_exact {|
+--------------------+
|LeftRight           |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "box wrap parameter" =
  print_ui ~width:20 ~height:2
    (Ui.hbox ~width:20 ~wrap:true ~gap:1
       [
         Ui.text "Item1";
         Ui.text "Item2";
         Ui.text "Item3";
         Ui.text "Item4";
         Ui.text "Item5";
       ]);
  [%expect_exact {|
+--------------------+
|Item1 Item2 Item3   |
|Item4 Item5         |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "margin exclusion from natural size" =
  let ui = Ui.hbox ~margin:(Ui.padding_all 2) [ Ui.text "Test" ] in
  let w, h = Ui.measure ui in
  Printf.printf "Width (excludes margin): %d, Height (excludes margin): %d\n" w
    h;
  [%expect {| Width (excludes margin): 4, Height (excludes margin): 1 |}] [@@ocamlformat "disable"]

let%expect_test "convenience functions - center" =
  print_ui ~width:5 ~height:3 (Ui.center (Ui.text "X"));
  [%expect_exact {|
+-----+
|     |
|  X  |
|     |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "convenience functions - styled" =
  print_ui ~width:5 ~height:1
    (Ui.styled ~fg:Render.Style.Red ~bg:Render.Style.Blue (Ui.text "Hi"));
  [%expect_exact {|
+-----+
|Hi   |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "complex flex layout - expand" =
  print_ui ~width:30 ~height:1
    (Ui.hbox ~width:30
       [
         Ui.text "[";
         Ui.hbox ~flex_grow:1 ~flex_shrink:1 [ Ui.text "Flexible content here" ];
         Ui.text "]";
       ]);
  [%expect_exact {|
+------------------------------+
|[Flexible content here       ]|
+------------------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "complex flex layout - shrink" =
  print_ui ~width:15 ~height:1
    (Ui.hbox ~width:15
       [
         Ui.text "[";
         Ui.hbox ~flex_grow:1 ~flex_shrink:1 [ Ui.text "Flexible content here" ];
         Ui.text "]";
       ]);
       
  [%expect_exact {|
+---------------+
|[Flexible co]  |
+---------------+
|}] [@@ocamlformat "disable"]

let%expect_test "border with clipping" =
  print_ui ~width:10 ~height:3
    (Ui.scroll ~width:10 ~height:3
       (Ui.hbox ~border:(Ui.border ()) [ Ui.text "This is long content" ]));
  [%expect_exact {|
+----------+
|┌────────┐|
|│This is │|
|└────────┘|
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "Padding and Borders" =
  print_ui ~width:20 ~height:7
    (Ui.vbox ~padding:(Ui.pad ~all:1 ())
       ~border:(Ui.border ~style:Ui.Rounded ())
       [ Ui.text "Hello"; Ui.text "World" ]);
  [%expect_exact {|
+--------------------+
|╭──────────────────╮|
|│                  │|
|│ Hello            │|
|│ World            │|
|│                  │|
|│                  │|
|╰──────────────────╯|
+--------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Alignment in Vbox" =
  let items = [ Ui.text "Short"; Ui.text "Longer line"; Ui.text "Mid" ] in
  print_ui ~width:20 ~height:5 (Ui.vbox ~align_items:`Start items);
  [%expect_exact {|
+--------------------+
|Short               |
|Longer line         |
|Mid                 |
|                    |
|                    |
+--------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Vbox align center" =
  let items = [ Ui.text "Short"; Ui.text "Longer line"; Ui.text "Mid" ] in
  print_ui ~width:20 ~height:5 (Ui.vbox ~align_items:`Center items);
  [%expect_exact {|
+--------------------+
|       Short        |
|    Longer line     |
|        Mid         |
|                    |
|                    |
+--------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Vbox align end" =
  let items = [ Ui.text "Short"; Ui.text "Longer line"; Ui.text "Mid" ] in
  print_ui ~width:20 ~height:5 (Ui.vbox ~align_items:`End items);
  [%expect_exact {|
+--------------------+
|               Short|
|         Longer line|
|                 Mid|
|                    |
|                    |
+--------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Justification in Hbox" =
  let items = [ Ui.text "A"; Ui.text "B"; Ui.text "C" ] in
  print_ui ~width:20 ~height:3 (Ui.hbox ~justify_content:`Start items);
  [%expect_exact {|
+--------------------+
|ABC                 |
|                    |
|                    |
+--------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Hbox justify center" =
  let items = [ Ui.text "A"; Ui.text "B"; Ui.text "C" ] in
  print_ui ~width:20 ~height:3 (Ui.hbox ~justify_content:`Center items);
  [%expect_exact {|
+--------------------+
|        ABC         |
|                    |
|                    |
+--------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Hbox justify end" =
  let items = [ Ui.text "A"; Ui.text "B"; Ui.text "C" ] in
  print_ui ~width:20 ~height:3 (Ui.hbox ~justify_content:`End items);
  [%expect_exact {|
+--------------------+
|                 ABC|
|                    |
|                    |
+--------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Expand element" =
  let header = Ui.text "Header" in
  let footer = Ui.text "Footer" in
  let body = Ui.vbox ~flex_grow:1 [] in
  print_ui ~width:20 ~height:8 (Ui.vbox [ header; body; footer ]);
  [%expect_exact
    {|
+--------------------+
|Header              |
|                    |
|                    |
|                    |
|                    |
|                    |
|                    |
|Footer              |
+--------------------+
|}];
  let left = Ui.text "Left" in
  let right = Ui.text "Right" in
  let center = Ui.hbox ~flex_grow:1 [] in
  print_ui ~width:20 ~height:3 (Ui.hbox [ left; center; right ]);
  [%expect_exact {|
+--------------------+
|Left           Right|
|                    |
|                    |
+--------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Complex nested layout" =
  let sidebar =
    Ui.vbox ~width:12
      ~border:(Ui.border ~style:Ui.Double ())
      [ Ui.text "Sidebar"; Ui.spacer ~flex:1 0; Ui.text "Status" ]
  in
  let main_content =
    Ui.vbox ~padding:(Ui.pad ~all:1 ()) ~border:(Ui.border ())
      [
        Ui.text "Main Content Header";
        Ui.text "-------------------";
        Ui.vbox [ Ui.text "Line 1"; Ui.text "Line 2" ];
      ]
  in
  let layout = Ui.hbox [ sidebar; main_content ] in
  print_ui ~width:40 ~height:8 layout;
  [%expect_exact {|
+----------------------------------------+
|╔══════════╗┌──────────────────────────┐|
|║Sidebar   ║│                          │|
|║          ║│ Main Content Header      │|
|║          ║│ -------------------      │|
|║          ║│ Line 1                   │|
|║          ║│ Line 2                   │|
|║Status    ║│                          │|
|╚══════════╝└──────────────────────────┘|
+----------------------------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Unicode and wide characters" =
  let ui =
    Ui.vbox
      [
        Ui.text "ASCII: Hello";
        Ui.text "Greek: αβγ";
        Ui.text "Emoji: Hi😀!";
        Ui.hbox [ Ui.text "你好"; Ui.text "world" ];
      ]
  in
  print_ui ~width:20 ~height:5 ui;
  [%expect_exact {|
+--------------------+
|ASCII: Hello        |
|Greek: αβγ          |
|Emoji: Hi😀 !        |
|你 好 world           |
|                    |
+--------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Stretch alignment" =
  let ui =
    Ui.vbox ~height:5 ~border:(Ui.border ())
      [
        Ui.hbox ~align_items:`Stretch
          [
            Ui.vbox ~width:5 ~border:(Ui.border ()) [ Ui.text "A" ];
            Ui.vbox ~width:5 ~border:(Ui.border ()) [ Ui.text "B"; Ui.text "C" ];
          ];
      ]
  in
  print_ui ~width:15 ~height:7 ui;
  [%expect_exact {|
+---------------+
|┌──────────┐   |
|│┌───┐┌───┐│   |
|││A  ││B  ││   |
|│└───┘│C  ││   |
|└─────└───┘┘   |
|               |
|               |
+---------------+
|}] [@ocamlformat "disable"]

let%expect_test "Border too small to render" =
  print_ui ~width:5 ~height:3 (Ui.hbox ~border:(Ui.border ()) [ Ui.text "x" ]);
  [%expect_exact {|
+-----+
|┌─┐  |
|│x│  |
|└─┘  |
+-----+
|}] [@ocamlformat "disable"]

let%expect_test "Min/max width constraints" =
  let ui =
    Ui.vbox ~gap:1
      [
        Ui.hbox ~min_width:15 ~border:(Ui.border ()) [ Ui.text "Min" ];
        Ui.hbox ~flex_shrink:1 ~min_width:15 ~border:(Ui.border ())
          [ Ui.text "Min" ];
        Ui.hbox ~max_width:10 ~border:(Ui.border ())
          [ Ui.text "Maximum width test" ];
      ]
  in
  print_ui ~width:30 ~height:10 ui;
  [%expect_exact {|
+------------------------------+
|┌────────────────────────────┐|
|│Min                         │|
|└────────────────────────────┘|
|                              |
|┌─────────────┐               |
|│Min          │               |
|└─────────────┘               |
|                              |
|┌────────┐                    |
|│Maximum │                    |
+------------------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Background padding" =
  let ui = Ui.hbox ~padding:(Ui.pad ~all:1 ()) [ Ui.text "Padded" ] in
  print_ui ~width:15 ~height:3 ui;
  [%expect_exact {|
+---------------+
|               |
| Padded        |
|               |
+---------------+
|}] [@ocamlformat "disable"]

let%expect_test "Z-stack alignment" =
  let main =
    Ui.hbox ~width:20 ~height:5 ~border:(Ui.border ()) [ Ui.text "Main" ]
  in
  let overlay = Ui.text "X" in
  let ui = Ui.zstack ~align:Center [ main; overlay ] in
  print_ui ~width:25 ~height:6 ui;
  [%expect_exact {|
+-------------------------+
|  ┌──────────────────┐   |
|  │Main              │   |
|  │         X        │   |
|  │                  │   |
|  └──────────────────┘   |
|                         |
+-------------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Flow layout wrapping" =
  let tag s =
    Ui.hbox ~border:(Ui.border ()) ~padding:(Ui.pad ~x:1 ()) [ Ui.text s ]
  in
  let ui =
    Ui.flow ~h_gap:1 ~v_gap:1
      [ tag "one"; tag "two"; tag "three"; tag "four"; tag "five" ]
  in
  print_ui ~width:25 ~height:8 ui;
  [%expect_exact {|
+-------------------------+
|┌─────┐ ┌─────┐ ┌───────┐|
|│ one │ │ two │ │ three │|
|└─────┘ └─────┘ └───────┘|
|                         |
|┌──────┐ ┌──────┐        |
|│ four │ │ five │        |
|└──────┘ └──────┘        |
|                         |
+-------------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Grid layout" =
  let ui =
    Ui.grid ~col_spacing:2 ~row_spacing:1
      ~columns:[ `Fixed 8; `Flex 1 ]
      ~rows:[ `Fixed 1; `Fixed 1 ]
      [
        Ui.text "Name:";
        Ui.hbox ~border:(Ui.border ()) [ Ui.text "John" ];
        Ui.text "Email:";
        Ui.hbox ~border:(Ui.border ()) [ Ui.text "john@example.com" ];
      ]
  in
  print_ui ~width:35 ~height:5 ui;
  [%expect_exact {|
+-----------------------------------+
|Name:                              |
|           John                    |
|Email:                             |
|           john@example.com        |
|                                   |
+-----------------------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Flex spacer" =
  let ui =
    Ui.hbox ~border:(Ui.border ())
      [ Ui.text "Left"; Ui.flex_spacer (); Ui.text "Right" ]
  in
  print_ui ~width:20 ~height:3 ui;
  [%expect_exact {|
+--------------------+
|┌──────────────────┐|
|│Left         Right│|
|└──────────────────┘|
+--------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Divider" =
  let ui =
    Ui.vbox ~gap:1 [ Ui.text "Section 1"; Ui.divider (); Ui.text "Section 2" ]
  in
  print_ui ~width:15 ~height:5 ui;
  [%expect_exact {|
+---------------+
|Section 1      |
|               |
|───────────────|
|               |
|Section 2      |
+---------------+
|}] [@ocamlformat "disable"]

let%expect_test "Text alignment" =
  let ui =
    Ui.vbox ~gap:1 ~width:20
      [
        Ui.text ~align:`Start "Left";
        Ui.text ~align:`Center "Center";
        Ui.text ~align:`End "Right";
      ]
  in
  print_ui ~width:22 ~height:5 ui;
  [%expect_exact {|
+----------------------+
|Left                  |
|                      |
|       Center         |
|                      |
|               Right  |
+----------------------+
|}] [@ocamlformat "disable"]

let%expect_test "Tab expansion" =
  let ui = Ui.text ~tab_width:4 "A\tB\tC" in
  print_ui ~width:15 ~height:2 ui;
  [%expect_exact {|
+---------------+
|A   B   C      |
|               |
+---------------+
|}] [@ocamlformat "disable"]

let%expect_test "grid with flex rows/columns" =
  print_ui ~width:20 ~height:5
    (Ui.grid
       ~columns:[ `Flex 1; `Flex 2 ]
       ~rows:[ `Flex 1 ]
       [ Ui.text "A"; Ui.text "B" ]);
  [%expect_exact {|
+--------------------+
|A      B            |
|                    |
|                    |
|                    |
|                    |
+--------------------+
|}] [@ocamlformat "disable"]

let%expect_test "divider with custom char and style" =
  print_ui ~width:15 ~height:3
    (Ui.divider ~char:"*" ~style:Render.Style.(fg Green) ());
  [%expect_exact {|
+---------------+
|***************|
|               |
|               |
+---------------+
|}] [@ocamlformat "disable"]

let%expect_test "scroll with h and v offsets" =
  print_ui ~width:5 ~height:3
    (Ui.scroll ~width:5 ~height:3 ~h_offset:2 ~v_offset:1
       (Ui.vbox [ Ui.text "ABCDEF"; Ui.text "GHIJKLM"; Ui.text "NOPQRS" ]));
  [%expect_exact {|
+-----+
|CDE  |
|IJK  |
|QRS  |
+-----+
|}] [@ocamlformat "disable"]
