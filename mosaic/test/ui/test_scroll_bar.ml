open Mosaic_ui

let render_boxed ?(colors = false) ?(width = 20) ?(height = 10) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors ~width:(width + 2) ~height:(height + 2) content

(* Scroll bar position tests with colors enabled to show thumb vs track. Without
   colors, both render as █ and position differences are invisible. *)

let%expect_test "vertical scroll bar at start (with colors)" =
  let element =
    scroll_bar ~id:"sb" ~orientation:`Vertical ~size:(size ~width:1 ~height:10)
      ()
    |> on_mount (fun node ->
           match Scroll_bar.mount node with
           | sb ->
               Scroll_bar.set_scroll_size sb 100;
               Scroll_bar.set_viewport_size sb 10;
               Scroll_bar.set_scroll_position sb 0)
  in
  render_boxed ~colors:true ~width:1 ~height:10 element;
  [%expect_exact
    {|
[0;38;2;255;255;255m┌─┐[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m└─┘[0m[0m
|}] [@@ocamlformat "disable"]

let%expect_test "vertical scroll bar at end (with colors)" =
  let element =
    scroll_bar ~id:"sb" ~orientation:`Vertical ~size:(size ~width:1 ~height:10)
      ()
    |> on_mount (fun node ->
           match Scroll_bar.mount node with
           | sb ->
               Scroll_bar.set_scroll_size sb 100;
               Scroll_bar.set_viewport_size sb 10;
               Scroll_bar.set_scroll_position sb 90)
  in
  render_boxed ~colors:true ~width:1 ~height:10 element;
  [%expect_exact
    {|
[0;38;2;255;255;255m┌─┐[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m└─┘[0m[0m
|}] [@@ocamlformat "disable"]

let%expect_test "vertical scroll bar midpoint (with colors)" =
  let element =
    scroll_bar ~id:"sb" ~orientation:`Vertical ~size:(size ~width:1 ~height:10)
      ()
    |> on_mount (fun node ->
           match Scroll_bar.mount node with
           | sb ->
               Scroll_bar.set_scroll_size sb 100;
               Scroll_bar.set_viewport_size sb 10;
               Scroll_bar.set_scroll_position sb 45)
  in
  render_boxed ~colors:true ~width:1 ~height:10 element;
  [%expect_exact
    {|
[0;38;2;255;255;255m┌─┐[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m└─┘[0m[0m
|}] [@@ocamlformat "disable"]

let%expect_test "horizontal scroll bar at start (with colors)" =
  let element =
    scroll_bar ~id:"sb" ~orientation:`Horizontal
      ~size:(size ~width:20 ~height:1) ()
    |> on_mount (fun node ->
           match Scroll_bar.mount node with
           | sb ->
               Scroll_bar.set_scroll_size sb 100;
               Scroll_bar.set_viewport_size sb 20;
               Scroll_bar.set_scroll_position sb 0)
  in
  render_boxed ~colors:true ~width:20 ~height:1 element;
  [%expect_exact
    {|
[0;38;2;255;255;255m┌────────────────────┐[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m████████████████████[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m└────────────────────┘[0m[0m
|}] [@@ocamlformat "disable"]

let%expect_test "horizontal scroll bar at end (with colors)" =
  let element =
    scroll_bar ~id:"sb" ~orientation:`Horizontal
      ~size:(size ~width:20 ~height:1) ()
    |> on_mount (fun node ->
           match Scroll_bar.mount node with
           | sb ->
               Scroll_bar.set_scroll_size sb 100;
               Scroll_bar.set_viewport_size sb 20;
               Scroll_bar.set_scroll_position sb 80)
  in
  render_boxed ~colors:true ~width:20 ~height:1 element;
  [%expect_exact
    {|
[0;38;2;255;255;255m┌────────────────────┐[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m████████████████████[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m└────────────────────┘[0m[0m
|}] [@@ocamlformat "disable"]

let%expect_test "horizontal scroll bar midpoint (with colors)" =
  let element =
    scroll_bar ~id:"sb" ~orientation:`Horizontal
      ~size:(size ~width:20 ~height:1) ()
    |> on_mount (fun node ->
           match Scroll_bar.mount node with
           | sb ->
               Scroll_bar.set_scroll_size sb 100;
               Scroll_bar.set_viewport_size sb 20;
               Scroll_bar.set_scroll_position sb 40)
  in
  render_boxed ~colors:true ~width:20 ~height:1 element;
  [%expect_exact
    {|
[0;38;2;255;255;255m┌────────────────────┐[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m████████████████████[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m└────────────────────┘[0m[0m
|}] [@@ocamlformat "disable"]

let%expect_test "vertical scroll bar with arrows at start" =
  let element =
    scroll_bar ~id:"sb" ~orientation:`Vertical ~show_arrows:true
      ~size:(size ~width:1 ~height:12) ()
    |> on_mount (fun node ->
           match Scroll_bar.mount node with
           | sb ->
               Scroll_bar.set_scroll_size sb 100;
               Scroll_bar.set_viewport_size sb 10;
               Scroll_bar.set_scroll_position sb 0)
  in
  render_boxed ~colors:true ~width:1 ~height:12 element;
  [%expect_exact
    {|
[0;38;2;255;255;255m┌─┐[0m
[0;38;2;255;255;255m│▲│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│▼│[0m
[0;38;2;255;255;255m└─┘[0m[0m
|}] [@@ocamlformat "disable"]

let%expect_test "vertical scroll bar with arrows at end" =
  let element =
    scroll_bar ~id:"sb" ~orientation:`Vertical ~show_arrows:true
      ~size:(size ~width:1 ~height:12) ()
    |> on_mount (fun node ->
           match Scroll_bar.mount node with
           | sb ->
               Scroll_bar.set_scroll_size sb 100;
               Scroll_bar.set_viewport_size sb 10;
               Scroll_bar.set_scroll_position sb 90)
  in
  render_boxed ~colors:true ~width:1 ~height:12 element;
  [%expect_exact
    {|
[0;38;2;255;255;255m┌─┐[0m
[0;38;2;255;255;255m│▲│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│[0;38;2;154;158;163;48;2;37;37;39m█[0;38;2;255;255;255m│[0m
[0;38;2;255;255;255m│▼│[0m
[0;38;2;255;255;255m└─┘[0m[0m
|}] [@@ocamlformat "disable"]
