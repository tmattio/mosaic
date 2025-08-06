open Test_utils

let%expect_test "basic progress bar at 0%" =
  print_ui ~height:1 (Ui.progress_bar ~completed:0.0 ());
  [%expect_exact {|
┌--------------------┐
|────────────────────|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "basic progress bar at 25%" =
  print_ui ~height:1 (Ui.progress_bar ~completed:25.0 ());
  [%expect_exact {|
┌--------------------┐
|━━━━━───────────────|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "basic progress bar at 50%" =
  print_ui ~height:1 (Ui.progress_bar ~completed:50.0 ());
  [%expect_exact {|
┌--------------------┐
|━━━━━━━━━━──────────|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "basic progress bar at 75%" =
  print_ui ~height:1 (Ui.progress_bar ~completed:75.0 ());
  [%expect_exact {|
┌--------------------┐
|━━━━━━━━━━━━━━━─────|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "basic progress bar at 100%" =
  print_ui ~height:1 (Ui.progress_bar ~completed:100.0 ());
  [%expect_exact {|
┌--------------------┐
|━━━━━━━━━━━━━━━━━━━━|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with custom width" =
  print_ui ~width:40 ~height:1 
    (Ui.progress_bar ~completed:60.0 ~width:30 ());
  [%expect_exact {|
┌----------------------------------------┐
|━━━━━━━━━━━━━━━━━━────────────          |
└----------------------------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with custom total" =
  print_ui ~height:1 
    (Ui.progress_bar ~total:200.0 ~completed:50.0 ());
  [%expect_exact {|
┌--------------------┐
|━━━━━───────────────|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with ASCII preset" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:50.0 ~preset:Ui.Progress_bar.ASCII ());
  [%expect_exact {|
┌--------------------┐
|[#########---------]|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with UTF8 preset" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:42.0 ~preset:Ui.Progress_bar.UTF8 ());
  [%expect_exact {|
┌--------------------┐
|│█████▉        │    |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with Line_double preset" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:50.0 ~preset:Ui.Progress_bar.Line_double ());
  [%expect_exact {|
┌--------------------┐
|╢═══════───────╟    |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with Line_single preset" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:50.0 ~preset:Ui.Progress_bar.Line_single ());
  [%expect_exact {|
┌--------------------┐
|├━━━━━━━───────┤    |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with Line_arrow preset" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:50.0 ~preset:Ui.Progress_bar.Line_arrow ());
  [%expect_exact {|
┌--------------------┐
|==========----------|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with Block_shade_light preset" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:50.0 ~preset:Ui.Progress_bar.Block_shade_light ());
  [%expect_exact {|
┌--------------------┐
|▓▓▓▓▓▓▓▓▓▓░░░░░░░░░░|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with Block_shade_medium preset" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:50.0 ~preset:Ui.Progress_bar.Block_shade_medium ());
  [%expect_exact {|
┌--------------------┐
|██████████▒▒▒▒▒▒▒▒▒▒|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with Block_shade_dark preset" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:50.0 ~preset:Ui.Progress_bar.Block_shade_dark ());
  [%expect_exact {|
┌--------------------┐
|██████████▓▓▓▓▓▓▓▓▓▓|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with Block_dotted preset" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:50.0 ~preset:Ui.Progress_bar.Block_dotted ());
  [%expect_exact {|
┌--------------------┐
|┋⣿⣿⣿⣿⣿⣿⣿⣀⣀⣀⣀⣀⣀⣀┋    |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with custom preset" =
  let custom_preset = Ui.Progress_bar.Custom {
    delimiters = Some ("<", ">");
    filled_char = "*";
    empty_char = ".";
    progress_stages = ["+"];
  } in
  print_ui ~height:1 
    (Ui.progress_bar ~completed:50.0 ~preset:custom_preset ());
  [%expect_exact {|
┌--------------------┐
|<*********.........>|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with custom delimiters" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:50.0 ~delimiters:(("[", "]")) ());
  [%expect_exact {|
┌--------------------┐
|[━━━━━━━━━─────────]|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with custom characters" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:50.0 ~filled_char:"█" ~empty_char:"░" ());
  [%expect_exact {|
┌--------------------┐
|██████████░░░░░░░░░░|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with custom styles" =
  print_ui ~height:1 
    (Ui.progress_bar 
       ~completed:50.0 
       ~bar_style:Ui.Style.(fg White)
       ~complete_style:Ui.Style.(fg Blue) 
       ());
  [%expect_exact {|
┌--------------------┐
|━━━━━━━━━━──────────|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar at finished state" =
  print_ui ~height:1 
    (Ui.progress_bar 
       ~completed:100.0 
       ~finished_style:Ui.Style.(fg Bright_green) 
       ());
  [%expect_exact {|
┌--------------------┐
|━━━━━━━━━━━━━━━━━━━━|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "pulse animation (indeterminate)" =
  print_ui ~height:1 
    (Ui.progress_bar ~pulse:true ~animation_time:0.0 ());
  [%expect_exact {|
┌--------------------┐
|                    |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "forced pulse animation" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:50.0 ~pulse:true ~animation_time:0.0 ());
  [%expect_exact {|
┌--------------------┐
|                    |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "progress bar with progress stages" =
  print_ui ~height:1 
    (Ui.progress_bar ~completed:35.0 ~progress_stages:["▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"] ());
  [%expect_exact {|
┌--------------------┐
|━━━━━━━─────────────|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "edge case - negative completion" =
  print_ui ~height:1 (Ui.progress_bar ~completed:(-10.0) ());
  [%expect_exact {|
┌--------------------┐
|────────────────────|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "edge case - over 100% completion" =
  print_ui ~height:1 (Ui.progress_bar ~completed:150.0 ());
  [%expect_exact {|
┌--------------------┐
|━━━━━━━━━━━━━━━━━━━━|
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "edge case - zero width" =
  print_ui ~height:1 (Ui.progress_bar ~completed:50.0 ~width:0 ());
  [%expect_exact {|
┌--------------------┐
|                    |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "edge case - single character width" =
  print_ui ~height:1 (Ui.progress_bar ~completed:50.0 ~width:1 ());
  [%expect_exact {|
┌--------------------┐
|╸                   |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "very small increments" =
  print_ui ~height:1 (Ui.progress_bar ~completed:12.5 ());
  [%expect_exact {|
┌--------------------┐
|━━╸─────────────────|
└--------------------┘
|}] [@@ocamlformat "disable"]
