open Test_utils

let%expect_test "simple panel" =
  print_ui (Ui.panel (Ui.text "Hello, World!"));
  [%expect_exact {|
+--------------------+
|╭───────────────╮   |
|│ Hello, World! │   |
|╰───────────────╯   |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with title" =
  print_ui (Ui.panel (Ui.text "Content") ~title:"Panel Title");
  [%expect_exact {|
+--------------------+
|╭── Panel Title ──╮ |
|│ Content         │ |
|╰─────────────────╯ |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with title and subtitle" =
  print_ui ~height:7
    (Ui.panel
       (Ui.vbox ~gap:1
          [
            Ui.text "Line 1";
            Ui.text "Line 2";
          ])
       ~title:"Header" ~subtitle:"Footer");
  [%expect_exact {|
+--------------------+
|╭── Header ──╮      |
|│ Line 1     │      |
|│            │      |
|│ Line 2     │      |
|╰── Footer ──╯      |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with ascii border style" =
  print_ui ~height:4 (Ui.panel (Ui.text "ASCII") ~box_style:ASCII);
  [%expect_exact {|
+--------------------+
|+-------+           |
|| ASCII |           |
|+-------+           |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with double border style" =
  print_ui ~height:4 (Ui.panel (Ui.text "Double") ~box_style:Double);
  [%expect_exact {|
+--------------------+
|╔════════╗          |
|║ Double ║          |
|╚════════╝          |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with thick border style" =
  print_ui ~height:4 (Ui.panel (Ui.text "Thick") ~box_style:Thick);
  [%expect_exact {|
+--------------------+
|┏━━━━━━━┓           |
|┃ Thick ┃           |
|┗━━━━━━━┛           |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "fitted panel" =
  print_ui ~width:30 ~height:5 
    (Ui.panel (Ui.text "Compact") ~title:"Fitted" ~expand:false);
  [%expect_exact {|
+------------------------------+
|╭── Fitted ──╮                |
|│ Compact    │                |
|╰────────────╯                |
|                              |
|                              |
+------------------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with padding" =
  print_ui ~height:8
    (Ui.panel (Ui.text "Padded") ~padding:(Ui.padding_all 2));
  [%expect_exact {|
+--------------------+
|╭──────────╮        |
|│          │        |
|│          │        |
|│  Padded  │        |
|│          │        |
|│          │        |
|╰──────────╯        |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with colored border" =
  print_ui ~height:4
    (Ui.panel (Ui.text "Colored") ~border_style:Ui.Style.(fg Blue));
  [%expect_exact {|
+--------------------+
|╭─────────╮         |
|│ Colored │         |
|╰─────────╯         |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with highlight" =
  print_ui ~height:4
    (Ui.panel (Ui.text "Important") ~title:"Alert" ~highlight:true);
  [%expect_exact {|
+--------------------+
|╭── Alert ──╮       |
|│ Important │       |
|╰───────────╯       |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with aligned titles" =
  print_ui ~height:5
    (Ui.panel (Ui.text "Content") 
       ~title:"Left" ~title_align:`Left
       ~subtitle:"Right" ~subtitle_align:`Right);
  [%expect_exact {|
+--------------------+
|╭─ Left ────╮       |
|│ Content   │       |
|╰─── Right ─╯       |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with center aligned title" =
  print_ui ~height:5
    (Ui.panel (Ui.text "Content") 
       ~title:"Centered" ~title_align:`Center
       ~subtitle:"Also Center" ~subtitle_align:`Center);
  [%expect_exact {|
+--------------------+
|╭─── Centered ────╮ |
|│ Content         │ |
|╰── Also Center ──╯ |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "nested panels" =
  print_ui ~width:30 ~height:12
    (Ui.panel
       (Ui.vbox ~gap:1
          [
            Ui.panel (Ui.text "Inner 1") ~title:"First";
            Ui.panel (Ui.text "Inner 2") ~title:"Second" ~box_style:ASCII;
          ])
       ~title:"Outer Panel" ~box_style:Thick);
  [%expect_exact {|
+------------------------------+
|┏━━ Outer Panel ━━┓           |
|┃ ╭── First ──╮   ┃           |
|┃ │ Inner 1   │   ┃           |
|┃ ╰───────────╯   ┃           |
|┃                 ┃           |
|┃ +-- Second --+  ┃           |
|┃ | Inner 2    |  ┃           |
|┃ +------------+  ┃           |
|┗━━━━━━━━━━━━━━━━━┛           |
|                              |
|                              |
|                              |
+------------------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with fixed dimensions" =
  print_ui ~width:30 ~height:8
    (Ui.panel 
       (Ui.text "Fixed size content")
       ~title:"15x5 Panel"
       ~width:15 
       ~height:5);
  [%expect_exact {|
+------------------------------+
|╭─ 15x5 Panel─╮               |
|│ Fixed size  │               |
|│             │               |
|│             │               |
|╰─────────────╯               |
|                              |
|                              |
|                              |
+------------------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with empty title" =
  print_ui ~height:4
    (Ui.panel (Ui.text "No title shown") ~title:"");
  [%expect_exact {|
+--------------------+
|╭────────────────╮  |
|│ No title shown │  |
|╰────────────────╯  |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "panel with scrollable content" =
  print_ui ~height:8
    (Ui.panel
       (Ui.vbox ~gap:0
          (List.init 10 (fun i -> Ui.text (Printf.sprintf "Line %d" (i + 1)))))
       ~title:"Scrollable"
       ~height:6);
  [%expect_exact {|
+--------------------+
|╭── Scrollable ──╮  |
|│ Line 1         │  |
|│ Line 2         │  |
|│ Line 3         │  |
|│ Line 4         │  |
|╰────────────────╯  |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]
