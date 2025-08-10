open Test_utils

let%expect_test "canvas - basic plot" =
  print_ui ~width:15 ~height:5
    (Ui.Canvas.create ~width:(`Cells 15) ~height:(`Cells 5)
       (fun ~width ~height canvas ->
         (* Plot some points *)
         Ui.Canvas.plot canvas ~x:0 ~y:0 "A";
         Ui.Canvas.plot canvas ~x:5 ~y:2 "B";
         Ui.Canvas.plot canvas ~x:10 ~y:4 "C";
         Ui.Canvas.plot canvas ~x:(width-1) ~y:(height-1) "D"));
  [%expect_exact {|
┌───────────────┐
│A              │
│               │
│     B         │
│               │
│          C   D│
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - with styles" =
  print_ui ~width:20 ~height:5
    (Ui.Canvas.create ~width:(`Cells 20) ~height:(`Cells 5)
       (fun ~width:_ ~height:_ canvas ->
         Ui.Canvas.plot canvas ~x:2 ~y:2 ~style:Ui.Style.(fg Red) "R";
         Ui.Canvas.plot canvas ~x:5 ~y:2 ~style:Ui.Style.(fg Green) "G";
         Ui.Canvas.plot canvas ~x:8 ~y:2 ~style:Ui.Style.(fg Blue) "B";
         Ui.Canvas.plot canvas ~x:11 ~y:2 ~style:Ui.Style.(bold) "X";
         Ui.Canvas.plot canvas ~x:14 ~y:2 ~style:Ui.Style.(italic) "I"));
  [%expect_exact {|
┌────────────────────┐
│                    │
│                    │
│  R  G  B  X  I     │
│                    │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - draw horizontal line" =
  print_ui ~width:15 ~height:5
    (Ui.Canvas.create ~width:(`Cells 15) ~height:(`Cells 5)
       (fun ~width ~height:_ canvas ->
         Ui.Canvas.draw_line ~x1:0 ~y1:2 ~x2:(width-1) ~y2:2 canvas));
  [%expect_exact {|
┌───────────────┐
│               │
│               │
│───────────────│
│               │
│               │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - draw vertical line" =
  print_ui ~width:10 ~height:8
    (Ui.Canvas.create ~width:(`Cells 10) ~height:(`Cells 8)
       (fun ~width:_ ~height canvas ->
         Ui.Canvas.draw_line ~x1:5 ~y1:0 ~x2:5 ~y2:(height-1) canvas));
  [%expect_exact {|
┌──────────┐
│     │    │
│     │    │
│     │    │
│     │    │
│     │    │
│     │    │
│     │    │
│     │    │
└──────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - draw diagonal lines" =
  print_ui ~width:10 ~height:8
    (Ui.Canvas.create ~width:(`Cells 10) ~height:(`Cells 8)
       (fun ~width ~height canvas ->
         (* Top-left to bottom-right *)
         Ui.Canvas.draw_line ~x1:0 ~y1:0 ~x2:(width-1) ~y2:(height-1) canvas;
         (* Top-right to bottom-left *)
         Ui.Canvas.draw_line ~x1:(width-1) ~y1:0 ~x2:0 ~y2:(height-1) canvas));
  [%expect_exact {|
┌──────────┐
│\        /│
│ \      / │
│  \    /  │
│   \  /   │
│    \/    │
│   /  \   │
│  /    \  │
│ /      \ │
└──────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - draw line with style" =
  print_ui ~width:15 ~height:5
    (Ui.Canvas.create ~width:(`Cells 15) ~height:(`Cells 5)
       (fun ~width:_ ~height:_ canvas ->
         Ui.Canvas.draw_line ~x1:1 ~y1:1 ~x2:13 ~y2:3
           ~style:Ui.Style.(fg Green) canvas));
  [%expect_exact {|
┌───────────────┐
│               │
│ \             │
│  \            │
│   \\\\\\\\\\\─│
│               │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - draw box with border" =
  print_ui ~width:15 ~height:7
    (Ui.Canvas.create ~width:(`Cells 15) ~height:(`Cells 7)
       (fun ~width:_ ~height:_ canvas ->
         Ui.Canvas.draw_box ~x:2 ~y:1 ~width:10 ~height:5
           ~border:Ui.Border.normal canvas));
  [%expect_exact {|
┌───────────────┐
│               │
│  ┌────────┐   │
│  │        │   │
│  │        │   │
│  │        │   │
│  └────────┘   │
│               │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - draw filled box" =
  print_ui ~width:15 ~height:7
    (Ui.Canvas.create ~width:(`Cells 15) ~height:(`Cells 7)
       (fun ~width:_ ~height:_ canvas ->
         Ui.Canvas.draw_box ~x:3 ~y:2 ~width:8 ~height:3
           ~style:Ui.Style.(bg Blue) canvas));
  [%expect_exact {|
┌───────────────┐
│               │
│               │
│               │
│               │
│               │
│               │
│               │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - multiple boxes" =
  print_ui ~width:20 ~height:8
    (Ui.Canvas.create ~width:(`Cells 20) ~height:(`Cells 8)
       (fun ~width:_ ~height:_ canvas ->
         (* First box with border *)
         Ui.Canvas.draw_box ~x:1 ~y:1 ~width:6 ~height:3
           ~border:Ui.Border.normal canvas;
         (* Second box filled *)
         Ui.Canvas.draw_box ~x:8 ~y:2 ~width:5 ~height:4
           ~style:Ui.Style.(bg Red) canvas;
         (* Third box with border *)
         Ui.Canvas.draw_box ~x:14 ~y:3 ~width:5 ~height:3
           ~border:Ui.Border.normal canvas));
  [%expect_exact {|
┌────────────────────┐
│                    │
│ ┌────┐             │
│ │    │             │
│ └────┘       ┌───┐ │
│              │   │ │
│              └───┘ │
│                    │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - Braille drawing" =
  print_ui ~width:10 ~height:5
    (Ui.Canvas.create ~width:(`Cells 10) ~height:(`Cells 5)
       (fun ~width:_ ~height:_ canvas ->
         (* Draw using Braille patterns *)
         Ui.Canvas.draw_line ~x1:0 ~y1:2 ~x2:9 ~y2:2 ~kind:`Braille canvas;
         for i = 0 to 9 do
           let y = 2 + (if i mod 2 = 0 then -1 else 1) in
           Ui.Canvas.plot canvas ~x:i ~y "⠄"
         done));
  [%expect_exact {|
┌──────────┐
│          │
│⠄ ⠄ ⠄ ⠄ ⠄│
│⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄│
│ ⠄ ⠄ ⠄ ⠄ │
│          │
└──────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - complex drawing" =
  print_ui ~width:25 ~height:10
    (Ui.Canvas.create ~width:(`Cells 25) ~height:(`Cells 10)
       (fun ~width ~height canvas ->
         (* Draw a frame *)
         Ui.Canvas.draw_box ~x:0 ~y:0 ~width ~height
           ~border:Ui.Border.double canvas;
         (* Draw diagonal lines *)
         Ui.Canvas.draw_line ~x1:2 ~y1:2 ~x2:(width-3) ~y2:(height-3)
           ~style:Ui.Style.(fg Red) canvas;
         Ui.Canvas.draw_line ~x1:(width-3) ~y1:2 ~x2:2 ~y2:(height-3)
           ~style:Ui.Style.(fg Blue) canvas;
         (* Add center point *)
         Ui.Canvas.plot canvas ~x:(width/2) ~y:(height/2)
           ~style:Ui.Style.(fg Green ++ bold) "●"));
  [%expect_exact {|
┌─────────────────────────┐
│╔═══════════════════════╗│
│║                       ║│
│║  \                 /  ║│
│║   \               /   ║│
│║    \      ●      /    ║│
│║     \           /     ║│
│║      \         /      ║│
│║       \       /       ║│
│╚═══════════════════════╝│
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - fill entire canvas" =
  print_ui ~width:10 ~height:5
    (Ui.Canvas.create ~width:(`Cells 10) ~height:(`Cells 5)
       (fun ~width ~height canvas ->
         for y = 0 to height - 1 do
           for x = 0 to width - 1 do
             let char = if (x + y) mod 2 = 0 then "█" else "░" in
             Ui.Canvas.plot canvas ~x ~y char
           done
         done));
  [%expect_exact {|
┌──────────┐
│█░█░█░█░█░│
│░█░█░█░█░█│
│█░█░█░█░█░│
│░█░█░█░█░█│
│█░█░█░█░█░│
└──────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - unicode characters" =
  print_ui ~width:20 ~height:5
    (Ui.Canvas.create ~width:(`Cells 20) ~height:(`Cells 5)
       (fun ~width:_ ~height:_ canvas ->
         (* Various Unicode drawing characters *)
         Ui.Canvas.plot canvas ~x:2 ~y:2 "◆";
         Ui.Canvas.plot canvas ~x:5 ~y:2 "◇";
         Ui.Canvas.plot canvas ~x:8 ~y:2 "○";
         Ui.Canvas.plot canvas ~x:11 ~y:2 "●";
         Ui.Canvas.plot canvas ~x:14 ~y:2 "□";
         Ui.Canvas.plot canvas ~x:17 ~y:2 "■"));
  [%expect_exact {|
┌────────────────────┐
│                    │
│                    │
│  ◆  ◇  ○  ●  □  ■  │
│                    │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - plot out of bounds" =
  print_ui ~width:10 ~height:5
    (Ui.Canvas.create ~width:(`Cells 10) ~height:(`Cells 5)
       (fun ~width ~height canvas ->
         (* These should be clipped and not cause errors *)
         Ui.Canvas.plot canvas ~x:(-1) ~y:2 "X";
         Ui.Canvas.plot canvas ~x:width ~y:2 "X";
         Ui.Canvas.plot canvas ~x:5 ~y:(-1) "X";
         Ui.Canvas.plot canvas ~x:5 ~y:height "X";
         (* Valid plots *)
         Ui.Canvas.plot canvas ~x:0 ~y:0 "A";
         Ui.Canvas.plot canvas ~x:(width-1) ~y:(height-1) "B"));
  [%expect_exact {|
┌──────────┐
│A         │
│          │
│          │
│          │
│         B│
└──────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - empty canvas" =
  print_ui ~width:10 ~height:5
    (Ui.Canvas.create ~width:(`Cells 10) ~height:(`Cells 5)
       (fun ~width:_ ~height:_ _canvas ->
         (* Do nothing *)
         ()));
  [%expect_exact {|
┌──────────┐
│          │
│          │
│          │
│          │
│          │
└──────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - with border and padding" =
  print_ui ~width:20 ~height:8
    (Ui.Canvas.create ~width:(`Cells 20) ~height:(`Cells 8)
       ~border:Ui.Border.normal ~padding:(Ui.all 1)
       (fun ~width ~height canvas ->
         (* The canvas should be smaller due to border and padding *)
         for x = 0 to width - 1 do
           Ui.Canvas.plot canvas ~x ~y:0 "─";
           Ui.Canvas.plot canvas ~x ~y:(height - 1) "─"
         done));
  [%expect_exact {|
┌────────────────────┐
│┌──────────────────┐│
││                  ││
││ ──────────────── ││
││                  ││
││                  ││
││ ──────────────── ││
││                  ││
│└──────────────────┘│
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - in layout" =
  print_ui ~width:25 ~height:10
    (Ui.vbox ~gap:(`Cells 1) [
      Ui.text "Canvas in layout:";
      Ui.hbox ~gap:(`Cells 1) [
        Ui.Canvas.create ~width:(`Cells 10) ~height:(`Cells 5)
          (fun ~width ~height canvas ->
            Ui.Canvas.draw_box ~x:0 ~y:0 ~width ~height
              ~border:Ui.Border.normal canvas;
            Ui.Canvas.plot canvas ~x:(width/2) ~y:(height/2) "A");
        Ui.Canvas.create ~width:(`Cells 10) ~height:(`Cells 5)
          (fun ~width ~height canvas ->
            Ui.Canvas.draw_box ~x:0 ~y:0 ~width ~height
              ~border:Ui.Border.normal canvas;
            Ui.Canvas.plot canvas ~x:(width/2) ~y:(height/2) "B");
      ]
    ]);
  [%expect_exact {|
┌─────────────────────────┐
│Canvas in layout:        │
│                         │
│┌────────┐ ┌────────┐    │
││        │ │        │    │
││    A   │ │    B   │    │
││        │ │        │    │
│└────────┘ └────────┘    │
│                         │
│                         │
│                         │
└─────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - measure" =
  let canvas = Ui.Canvas.create ~width:(`Cells 15) ~height:(`Cells 5)
    (fun ~width:_ ~height:_ _canvas -> ()) in
  let w, h = Ui.measure canvas in
  Printf.printf "Canvas dimensions: %dx%d\n" w h;
  [%expect {| Canvas dimensions: 15x5 |}] [@@ocamlformat "disable"]

let%expect_test "canvas - flexible sizing" =
  print_ui ~width:20 ~height:5
    (Ui.Canvas.create ~width:(`Pct 1.0) ~height:(`Cells 5)
       (fun ~width ~height:_ canvas ->
         (* Should fill the available width *)
         for x = 0 to width - 1 do
           Ui.Canvas.plot canvas ~x ~y:2 
             (if x = 0 then "[" 
              else if x = width - 1 then "]" 
              else "─")
         done));
  [%expect_exact {|
┌────────────────────┐
│                    │
│                    │
│[──────────────────]│
│                    │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - grid pattern" =
  print_ui ~width:15 ~height:8
    (Ui.Canvas.create ~width:(`Cells 15) ~height:(`Cells 8)
       (fun ~width ~height canvas ->
         (* Draw a grid *)
         for x = 0 to width - 1 do
           if x mod 3 = 0 then
             for y = 0 to height - 1 do
               Ui.Canvas.plot canvas ~x ~y "│"
             done
         done;
         for y = 0 to height - 1 do
           if y mod 2 = 0 then
             for x = 0 to width - 1 do
               if x mod 3 <> 0 then
                 Ui.Canvas.plot canvas ~x ~y "─"
             done
         done));
  [%expect_exact {|
┌───────────────┐
││──│──│──│──│──│
││  │  │  │  │  │
││──│──│──│──│──│
││  │  │  │  │  │
││──│──│──│──│──│
││  │  │  │  │  │
││──│──│──│──│──│
││  │  │  │  │  │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - animation frame" =
  print_ui ~width:15 ~height:5
    (Ui.Canvas.create ~width:(`Cells 15) ~height:(`Cells 5)
       (fun ~width ~height:_ canvas ->
         (* Simulate an animation frame with a moving object *)
         let frame = 3 in (* Fixed frame for testing *)
         let x = frame mod width in
         let y = 2 in
         Ui.Canvas.plot canvas ~x ~y ~style:Ui.Style.(fg Yellow ++ bold) "★";
         (* Draw trail *)
         for i = 1 to min 3 x do
           Ui.Canvas.plot canvas ~x:(x - i) ~y 
             ~style:Ui.Style.(fg (Index (240 + i))) "·"
         done));
  [%expect_exact {|
┌───────────────┐
│               │
│               │
│   ★           │
│               │
│               │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - layered drawing" =
  print_ui ~width:20 ~height:8
    (Ui.Canvas.create ~width:(`Cells 20) ~height:(`Cells 8)
       (fun ~width ~height canvas ->
         (* Background layer - fill *)
         for y = 1 to height - 2 do
           for x = 1 to width - 2 do
             Ui.Canvas.plot canvas ~x ~y "·"
           done
         done;
         (* Middle layer - box *)
         Ui.Canvas.draw_box ~x:3 ~y:2 ~width:14 ~height:4
           ~border:Ui.Border.normal canvas;
         (* Foreground layer - text *)
         let text = "CANVAS" in
         let text_x = (width - String.length text) / 2 in
         String.iteri (fun i c ->
           Ui.Canvas.plot canvas ~x:(text_x + i) ~y:(height / 2)
             ~style:Ui.Style.(bold) (String.make 1 c)
         ) text));
  [%expect_exact {|
┌────────────────────┐
│                    │
│ ·················· │
│ ··┌──────────────┐ │
│ ··│   CANVAS     │ │
│ ··│              │ │
│ ··└──────────────┘ │
│ ·················· │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - plot with tabs and special chars" =
  print_ui ~width:15 ~height:5
    (Ui.Canvas.create ~width:(`Cells 15) ~height:(`Cells 5)
       (fun ~width:_ ~height:_ canvas ->
         (* Test tab expansion and special characters *)
         Ui.Canvas.plot canvas ~x:0 ~y:1 "A\tB"; (* Should only plot 'A' *)
         Ui.Canvas.plot canvas ~x:0 ~y:2 "\n"; (* Should not affect layout *)
         Ui.Canvas.plot canvas ~x:5 ~y:2 "X";
         Ui.Canvas.plot canvas ~x:0 ~y:3 "😀"; (* Emoji - wide char *)));
  [%expect_exact {|
┌───────────────┐
│               │
│A              │
│     X         │
│😀             │
│               │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "canvas - combined with other UI elements" =
  print_ui ~width:30 ~height:12
    (Ui.panel ~title:"Data Visualization" ~box_style:Ui.Border.Rounded
       (Ui.vbox ~gap:(`Cells 1) [
         Ui.text "Live data:";
         Ui.Canvas.create ~width:(`Cells 25) ~height:(`Cells 5)
           (fun ~width:_ ~height canvas ->
             (* Draw a simple bar chart *)
             let bars = [3; 5; 2; 4; 3] in
             List.iteri (fun i h ->
               let x = i * 5 + 2 in
               for y = (height - h) to (height - 1) do
                 for dx = 0 to 2 do
                   Ui.Canvas.plot canvas ~x:(x + dx) ~y
                     ~style:Ui.Style.(bg Blue) " "
                 done
               done
             ) bars);
         Ui.divider ();
         Ui.text "Status: Active";
       ]));
  [%expect_exact {|
┌──────────────────────────────┐
│╭─── Data Visualization ─────╮│
││Live data:                  ││
││                            ││
││                            ││
││                            ││
││  ███     ███        ███    ││
││  ██████  ███  ███   ███    ││
││  ███████ ███  █████ ███    ││
││                            ││
││────────────────────────────││
││Status: Active              ││
│╰────────────────────────────╯│
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]
