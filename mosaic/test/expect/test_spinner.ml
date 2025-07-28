open Test_utils

let%expect_test "spinner - braille dots" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Braille_dots);
  [%expect_exact {|
+-----+
|⠋    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - braille dots at time 0.1" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.1 Ui.Braille_dots);
  [%expect_exact {|
+-----+
|⠙    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - braille dots at time 0.2" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.2 Ui.Braille_dots);
  [%expect_exact {|
+-----+
|⠹    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - braille dots2" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Braille_dots2);
  [%expect_exact {|
+-----+
|⣾    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - braille dots3" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Braille_dots3);
  [%expect_exact {|
+-----+
|⠋    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - braille circle" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Braille_circle);
  [%expect_exact {|
+-----+
|⢎    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - line spin" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Line_spin);
  [%expect_exact {|
+-----+
|-    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - line pulse" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Line_pulse);
  [%expect_exact {|
+-----+
|⠂    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - pipe spin" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Pipe_spin);
  [%expect_exact {|
+-----+
|┤    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - ascii dots" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Ascii_dots);
  [%expect_exact {|
+-----+
|.    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - ascii dots scroll" =
  print_ui ~width:10 ~height:1 (Ui.spinner ~time:0.0 Ui.Ascii_dots_scroll);
  [%expect_exact {|
+----------+
|.         |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - ascii star" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Ascii_star);
  [%expect_exact {|
+-----+
|+    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - bar vertical grow" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Bar_vertical_grow);
  [%expect_exact {|
+-----+
|▁    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - bar horizontal grow" =
  print_ui ~width:10 ~height:1 (Ui.spinner ~time:0.0 Ui.Bar_horizontal_grow);
  [%expect_exact {|
+----------+
|▏         |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - bar bounce" =
  print_ui ~width:10 ~height:1 (Ui.spinner ~time:0.0 Ui.Bar_bounce);
  [%expect_exact {|
+----------+
|[    ]    |
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - block bounce" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Block_bounce);
  [%expect_exact {|
+-----+
|▖    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - circle quarters" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Circle_quarters);
  [%expect_exact {|
+-----+
|◴    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - circle halves" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Circle_halves);
  [%expect_exact {|
+-----+
|◐    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - arrow rotate" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Arrow_rotate);
  [%expect_exact {|
+-----+
|←    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - arrow rotate2" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Arrow_rotate2);
  [%expect_exact {|
+-----+
|⬆️    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - unicode clock at time 0.0" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Unicode_clock);
  [%expect_exact {|
+-----+
|🕛   |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - unicode clock at time 0.083" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:(1.0 /. 12.0) Ui.Unicode_clock);
  [%expect_exact {|
+-----+
|🕛   |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - unicode moon phases" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Unicode_moon_phases);
  [%expect_exact {|
+-----+
|🌑   |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - unicode earth rotate" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Unicode_earth_rotate);
  [%expect_exact {|
+-----+
|🌍   |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - emoji hearts" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Emoji_hearts);
  [%expect_exact {|
+-----+
|💛   |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - toggle box" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Toggle_box);
  [%expect_exact {|
+-----+
|▫    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - toggle circle" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Toggle_circle);
  [%expect_exact {|
+-----+
|ဝ    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - progress bar" =
  print_ui ~width:10 ~height:1 (Ui.spinner ~time:0.0 Ui.Progress_bar);
  [%expect_exact {|
+----------+
|█▁▁▁▁▁▁▁▁▁|
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - with custom speed at time 0.0" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~speed:2.0 ~time:0.0 Ui.Braille_dots);
  [%expect_exact {|
+-----+
|⠋    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - with custom speed at time 0.05" =
  (* Double speed should advance twice as fast *)
  print_ui ~width:5 ~height:1 (Ui.spinner ~speed:2.0 ~time:0.05 Ui.Braille_dots);
  [%expect_exact {|
+-----+
|⠙    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - custom frames at time 0.0" =
  let custom : Ui.spinner_kind = Ui.Custom { frames = ["|"; "/"; "-"; "\\"]; interval = 80 } in
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 custom);
  [%expect_exact {|
+-----+
||    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - custom frames at time 0.25" =
  let custom : Ui.spinner_kind= Ui.Custom { frames = ["|"; "/"; "-"; "\\"]; interval = 80 } in
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.25 custom);
  [%expect_exact {|
+-----+
|\    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - custom frames at time 0.5" =
  let custom : Ui.spinner_kind= Ui.Custom { frames = ["|"; "/"; "-"; "\\"]; interval = 80 } in
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.5 custom);
  [%expect_exact {|
+-----+
|-    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - custom frames at time 0.75" =
  let custom : Ui.spinner_kind = Ui.Custom { frames = ["|"; "/"; "-"; "\\"]; interval = 80 } in
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.75 custom);
  [%expect_exact {|
+-----+
|/    |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - in a layout" =
  print_ui ~width:20 ~height:3
    (Ui.hbox ~gap:1 [
      Ui.spinner ~time:0.0 Ui.Braille_dots;
      Ui.text "Loading...";
    ]);
  [%expect_exact {|
+--------------------+
|⠋ Loading...        |
|                    |
|                    |
+--------------------+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - anim pong" =
  print_ui ~width:10 ~height:1 (Ui.spinner ~time:0.0 Ui.Anim_pong);
  [%expect_exact {|
+----------+
|▐⠂       ▌|
+----------+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - pulse orange" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Pulse_orange);
  [%expect_exact {|
+-----+
|🔸   |
+-----+
|}] [@@ocamlformat "disable"]

let%expect_test "spinner - pulse blue" =
  print_ui ~width:5 ~height:1 (Ui.spinner ~time:0.0 Ui.Pulse_blue);
  [%expect_exact {|
+-----+
|🔹   |
+-----+
|}] [@@ocamlformat "disable"]
