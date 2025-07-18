open Mosaic

(** Helper to render a UI element to a string and print it for expect testing.
*)
let print_layout ?(width = 40) ?(height = 10) element =
  let output = Test_utils.render_to_string ~width ~height element in
  (* Add a visual border to make the output clearer in the test file *)
  let border_line = "+" ^ String.make width '-' ^ "+\n" in
  let lines = String.split_on_char '\n' output in
  let bordered_output =
    List.map
      (fun line ->
        let len = String.length line in
        let padded_line =
          if len < width then line ^ String.make (width - len) ' ' else line
        in
        "|" ^ padded_line ^ "|")
      lines
    |> String.concat "\n"
  in
  print_string (border_line ^ bordered_output ^ "\n" ^ border_line)

let%expect_test "Simple hbox" =
  print_layout (Ui.hbox [ Ui.text "One"; Ui.text "Two"; Ui.text "Three" ]);
  [%expect_exact
    {|+----------------------------------------+
|OneTwoThree                             |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
+----------------------------------------+
|}]

let%expect_test "Simple vbox" =
  print_layout (Ui.vbox [ Ui.text "One"; Ui.text "Two"; Ui.text "Three" ]);
  [%expect_exact
    {|+----------------------------------------+
|One                                     |
|Two                                     |
|Three                                   |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
+----------------------------------------+
|}]

let%expect_test "Hbox with gap" =
  print_layout
    (Ui.hbox ~gap:3 [ Ui.text "One"; Ui.text "Two"; Ui.text "Three" ]);
  [%expect_exact
    {|+----------------------------------------+
|One   Two   Three                       |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
+----------------------------------------+
|}]

let%expect_test "Vbox with gap" =
  print_layout
    (Ui.vbox ~gap:1 [ Ui.text "One"; Ui.text "Two"; Ui.text "Three" ]);
  [%expect_exact
    {|+----------------------------------------+
|One                                     |
|                                        |
|Two                                     |
|                                        |
|Three                                   |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
+----------------------------------------+
|}]

let%expect_test "Padding and Borders" =
  print_layout ~width:20 ~height:7
    (Ui.vbox ~padding:(Ui.pad ~all:1 ())
       ~border:(Ui.border ~style:Ui.Rounded ())
       [ Ui.text "Hello"; Ui.text "World" ]);
  [%expect_exact
    {|+--------------------+
|╭───────╮           |
|│       │           |
|│ Hello │           |
|│ World │           |
|│       │           |
|╰───────╯           |
|                    |
+--------------------+
|}]

let%expect_test "Alignment in Vbox" =
  let items = [ Ui.text "Short"; Ui.text "Longer line"; Ui.text "Mid" ] in
  print_layout ~width:20 ~height:5 (Ui.vbox ~align_items:Ui.Start items);
  [%expect_exact
    {|+--------------------+
|Short               |
|Longer line         |
|Mid                 |
|                    |
|                    |
+--------------------+
|}]

let%expect_test "Vbox align center" =
  let items = [ Ui.text "Short"; Ui.text "Longer line"; Ui.text "Mid" ] in
  print_layout ~width:20 ~height:5 (Ui.vbox ~align_items:Ui.Center items);
  [%expect_exact
    {|+--------------------+
|   Short            |
|Longer line         |
|    Mid             |
|                    |
|                    |
+--------------------+
|}]

let%expect_test "Vbox align end" =
  let items = [ Ui.text "Short"; Ui.text "Longer line"; Ui.text "Mid" ] in
  print_layout ~width:20 ~height:5 (Ui.vbox ~align_items:Ui.End items);
  [%expect_exact
    {|+--------------------+
|      Short         |
|Longer line         |
|        Mid         |
|                    |
|                    |
+--------------------+
|}]

let%expect_test "Justification in Hbox" =
  let items = [ Ui.text "A"; Ui.text "B"; Ui.text "C" ] in
  print_layout ~width:20 ~height:3 (Ui.hbox ~justify_content:Ui.Start items);
  [%expect_exact
    {|+--------------------+
|ABC                 |
|                    |
|                    |
+--------------------+
|}]

let%expect_test "Hbox justify center" =
  let items = [ Ui.text "A"; Ui.text "B"; Ui.text "C" ] in
  print_layout ~width:20 ~height:3 (Ui.hbox ~justify_content:Ui.Center items);
  [%expect_exact
    {|+--------------------+
|ABC                 |
|                    |
|                    |
+--------------------+
|}]

let%expect_test "Hbox justify end" =
  let items = [ Ui.text "A"; Ui.text "B"; Ui.text "C" ] in
  print_layout ~width:20 ~height:3 (Ui.hbox ~justify_content:Ui.End items);
  [%expect_exact
    {|+--------------------+
|ABC                 |
|                    |
|                    |
+--------------------+
|}]

let%expect_test "Expand element" =
  let header = Ui.text "Header" in
  let footer = Ui.text "Footer" in
  let body = Ui.vbox ~flex_grow:1 [] in
  print_layout ~width:20 ~height:8 (Ui.vbox [ header; body; footer ]);
  [%expect_exact
    {|+--------------------+
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
  print_layout ~width:20 ~height:3 (Ui.hbox [ left; center; right ]);
  [%expect_exact
    {|+--------------------+
|Left           Right|
|                    |
|                    |
+--------------------+
|}]

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
  print_layout ~width:40 ~height:8 layout;
  [%expect_exact
    {|+----------------------------------------+
|╔══════════╗┌──────────────────────────┐|
|║Sidebar   ║│                          │|
|║          ║│ Main Content Header      │|
|║          ║│ -------------------      │|
|║          ║│ Line 1                   │|
|║          ║│ Line 2                   │|
|║Status    ║│                          │|
|╚══════════╝└──────────────────────────┘|
+----------------------------------------+
|}]

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
  print_layout ~width:20 ~height:5 ui;
  [%expect_exact
    {|+--------------------+
|ASCII: Hello        |
|Greek: αβγ          |
|Emoji: Hi😀 !        |
|你 好 world           |
|                    |
+--------------------+
|}]

let%expect_test "Stretch alignment" =
  let ui =
    Ui.vbox ~height:5 ~border:(Ui.border ())
      [
        Ui.hbox ~align_items:Ui.Stretch
          [
            Ui.vbox ~width:5 ~border:(Ui.border ()) [ Ui.text "A" ];
            Ui.vbox ~width:5 ~border:(Ui.border ()) [ Ui.text "B"; Ui.text "C" ];
          ];
      ]
  in
  print_layout ~width:15 ~height:7 ui;
  [%expect_exact
    {|+---------------+
|┌──────────┐   |
|│┌───┐┌───┐│   |
|││A  ││B  ││   |
|│└───┘│C  ││   |
|└─────└───┘┘   |
|               |
|               |
+---------------+
|}]

let%expect_test "Border too small to render" =
  print_layout ~width:5 ~height:3
    (Ui.hbox ~border:(Ui.border ()) [ Ui.text "x" ]);
  [%expect_exact {|+-----+
|┌─┐  |
|│x│  |
|└─┘  |
+-----+
|}]

let%expect_test "Min/max width constraints" =
  let ui =
    Ui.vbox ~gap:1
      [
        Ui.hbox ~min_width:15 ~border:(Ui.border ()) [ Ui.text "Min" ];
        Ui.hbox ~max_width:10 ~border:(Ui.border ())
          [ Ui.text "Maximum width test" ];
      ]
  in
  print_layout ~width:30 ~height:6 ui;
  [%expect_exact
    {|+------------------------------+
|┌─────────────┐               |
|│Min          │               |
|└─────────────┘               |
|                              |
|┌────────┐                    |
|│Maximum width test           |
+------------------------------+
|}]

let%expect_test "Background padding" =
  let ui = Ui.hbox ~padding:(Ui.pad ~all:1 ()) [ Ui.text "Padded" ] in
  print_layout ~width:15 ~height:3 ui;
  [%expect_exact
    {|+---------------+
|               |
| Padded        |
|               |
+---------------+
|}]

let%expect_test "Rich text" =
  let ui =
    Ui.rich_text
      [
        ("Normal ", Render.Style.empty);
        ("Bold", Render.Style.empty);
        (" and ", Render.Style.empty);
        ("Red", Render.Style.empty);
      ]
  in
  print_layout ~width:20 ~height:2 ui;
  [%expect_exact
    {|+--------------------+
|Normal Bold and Red |
|                    |
+--------------------+
|}]

let%expect_test "Z-stack alignment" =
  let main =
    Ui.hbox ~width:20 ~height:5 ~border:(Ui.border ()) [ Ui.text "Main" ]
  in
  let overlay = Ui.text "X" in
  let ui = Ui.zstack ~align:Center [ main; overlay ] in
  print_layout ~width:25 ~height:6 ui;
  [%expect_exact
    {|+-------------------------+
|  ┌──────────────────┐   |
|  │Main              │   |
|  │         X        │   |
|  │                  │   |
|  └──────────────────┘   |
|                         |
+-------------------------+
|}]

let%expect_test "Flow layout wrapping" =
  let tag s =
    Ui.hbox ~border:(Ui.border ()) ~padding:(Ui.pad ~x:1 ()) [ Ui.text s ]
  in
  let ui =
    Ui.flow ~h_gap:1 ~v_gap:1
      [ tag "one"; tag "two"; tag "three"; tag "four"; tag "five" ]
  in
  print_layout ~width:25 ~height:8 ui;
  [%expect_exact
    {|+-------------------------+
|┌─────┐ ┌─────┐ ┌───────┐|
|│ one │ │ two │ │ three │|
|└─────┘ └─────┘ └───────┘|
|                         |
|┌──────┐ ┌──────┐        |
|│ four │ │ five │        |
|└──────┘ └──────┘        |
|                         |
+-------------------------+
|}]

let%expect_test "Grid layout" =
  let ui =
    Ui.grid ~col_spacing:2 ~row_spacing:1 ~columns:[ Fixed 8; Flex 1 ]
      ~rows:[ Fixed 1; Fixed 1 ]
      [
        Ui.text "Name:";
        Ui.hbox ~border:(Ui.border ()) [ Ui.text "John" ];
        Ui.text "Email:";
        Ui.hbox ~border:(Ui.border ()) [ Ui.text "john@example.com" ];
      ]
  in
  print_layout ~width:35 ~height:5 ui;
  [%expect_exact
    {|+-----------------------------------+
|Name:                              |
|           John                    |
|Email:                             |
|           john@example.com        |
|                                   |
+-----------------------------------+
|}]

let%expect_test "Flex spacer" =
  let ui =
    Ui.hbox ~border:(Ui.border ())
      [ Ui.text "Left"; Ui.flex_spacer (); Ui.text "Right" ]
  in
  print_layout ~width:20 ~height:3 ui;
  [%expect_exact
    {|+--------------------+
|┌──────────────────┐|
|│Left         Right│|
|└──────────────────┘|
+--------------------+
|}]

let%expect_test "Divider" =
  let ui =
    Ui.vbox ~gap:1 [ Ui.text "Section 1"; Ui.divider (); Ui.text "Section 2" ]
  in
  print_layout ~width:15 ~height:5 ui;
  [%expect_exact
    {|+---------------+
|Section 1      |
|               |
|───────────────|
|               |
|Section 2      |
+---------------+
|}]

let%expect_test "Text alignment" =
  let ui =
    Ui.vbox ~gap:1 ~width:20
      [
        Ui.text ~align:Start "Left";
        Ui.text ~align:Center "Center";
        Ui.text ~align:End "Right";
      ]
  in
  print_layout ~width:22 ~height:5 ui;
  [%expect_exact
    {|+----------------------+
|Left                  |
|                      |
|       Center         |
|                      |
|               Right  |
+----------------------+
|}]

let%expect_test "Tab expansion" =
  let ui = Ui.text ~tab_width:4 "A\tB\tC" in
  print_layout ~width:15 ~height:2 ui;
  [%expect_exact
    {|+---------------+
|A   B   C      |
|               |
+---------------+
|}]
