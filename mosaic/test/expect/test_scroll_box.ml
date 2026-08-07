open Mosaic_ui
open Expect_harness

(* ── Scroll box with vertical scrolling ── *)

let%expect_test "scroll box renders children in viewport" =
  render ~width:20 ~height:8
    (Vnode.scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:8))
       [ Vnode.text "Line 1"; Vnode.text "Line 2"; Vnode.text "Line 3" ]);
  [%expect {|
    Line 1
    Line 2
    Line 3 |}]

(* ── Scroll box with border ── *)

let%expect_test "scroll box inside bordered box" =
  render ~width:22 ~height:8
    (Vnode.box ~border:true
       [
         Vnode.scroll_box
           ~style:
             (Toffee.Style.default
             |> Toffee.Style.set_width (Toffee.Style.Dimension.percent 1.)
             |> Toffee.Style.set_height (Toffee.Style.Dimension.percent 1.))
           [ Vnode.text "Hello"; Vnode.text "World" ];
       ]);
  [%expect
    {|
    ┌────────────────────┐
    │Hello               │
    │World               │
    │                    │
    │                    │
    │                    │
    │                    │
    └────────────────────┘ |}]

(* ── Reconciliation: vnode updates ── *)

let%expect_test "scroll box reconciles children" =
  let app = make_app () in
  reconcile app
    (Vnode.scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:5))
       [ Vnode.text "First" ]);
  frame app ~width:20 ~height:5;
  reconcile app
    (Vnode.scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:5))
       [ Vnode.text "Second" ]);
  frame app ~width:20 ~height:5;
  [%expect {|
    First




    Second |}]

let%expect_test "scroll box stays constrained in column flex layout" =
  render ~width:20 ~height:8
    (Vnode.box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column)
       [
         Vnode.text "head";
         Vnode.scroll_box
           ~style:(Toffee.Style.default |> Toffee.Style.set_flex_grow 1.)
           (List.init 10 (fun i ->
                Vnode.text (Printf.sprintf "line %d" (i + 1))));
         Vnode.text "foot";
       ]);
  [%expect
    {|
    head
    line 1
    line 2
    line 3
    line 4
    line 5
    line 6
    foot |}]

let reveal ?x ?y ?(align_x = `Nearest) ?(align_y = `Start) ?(margin = 0) key :
    Scroll_box.reveal =
  { key; x; y; align_x; align_y; margin }

let numbered_lines count =
  List.init count (fun i -> Vnode.text (Printf.sprintf "line %d" (i + 1)))

let%expect_test "scroll box reveal scrolls to content coordinate" =
  render ~width:20 ~height:5
    (Vnode.scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:5))
       ~reveal:(reveal ~y:6 "line-7") (numbered_lines 12));
  [%expect {|
    line 7
    line 8
    line 9
    line 10
    line 11 |}]

let%expect_test "scroll box reveal overrides sticky scroll" =
  let app = make_app () in
  reconcile app
    (Vnode.scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:5))
       ~sticky_scroll:true ~sticky_start:`Bottom ~reveal:(reveal ~y:2 "line-3")
       (numbered_lines 12));
  frame app ~width:20 ~height:5;
  frame app ~width:20 ~height:5;
  [%expect
    {|line 3
line 4
line 5
line 6
line 7
line 3
line 4             █
line 5             █
line 6
line 7|}]

let%expect_test "reset_sticky returns a parked viewport to its live edge once" =
  let app = make_app () in
  let node = ref None in
  let view ?reset_sticky count =
    Vnode.scroll_box ~key:"transcript" ~sticky_scroll:true ~sticky_start:`Bottom
      ~show_scrollbars:false ?reset_sticky
      ~ref:(fun renderable -> node := Some renderable)
      ~on_reset_sticky_applied:(fun ~key -> print_endline ("applied " ^ key))
      (numbered_lines count)
  in
  let show label vnode =
    print_endline label;
    reconcile app vnode;
    Matrix_grid.clear (Matrix_screen.next_grid (Renderer.screen app.renderer));
    settled_frame app ~width:12 ~height:4;
    print_newline ()
  in
  show "initial tail" (view 12);
  focus app (Option.get !node);
  send_key app Matrix_input.Key.Page_up;
  show "page up" (view 12);
  show "append stays parked" (view 16);
  show "new reset key follows" (view ~reset_sticky:"turn-2" 16);
  send_key app Matrix_input.Key.Page_up;
  show "manual page after reset" (view ~reset_sticky:"turn-2" 16);
  show "stable reset key stays parked" (view ~reset_sticky:"turn-2" 20);
  show "changed reset key follows again" (view ~reset_sticky:"turn-3" 20);
  [%expect
    {|initial tail

line 9
line 10
line 11
line 12
page up

line 7
line 8
line 9
line 10
append stays parked

line 7
line 8
line 9
line 10
new reset key follows
applied turn-2

line 13
line 14
line 15
line 16
manual page after reset

line 11
line 12
line 13
line 14
stable reset key stays parked

line 11
line 12
line 13
line 14
changed reset key follows again
applied turn-3

line 17
line 18
line 19
line 20|}]

(* ── Scroll bar visibility ── *)

let ten_lines = List.init 10 (fun i -> Vnode.text (Printf.sprintf "Line %d" i))

let scrollbar_app ?show_scrollbars () =
  let app = make_app () in
  reconcile app
    (Vnode.scroll_box ?show_scrollbars
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:4))
       ten_lines);
  app

let%expect_test "overflowing scroll box shows the vertical bar" =
  let app = scrollbar_app () in
  settled_frame app ~width:20 ~height:4;
  [%expect_exact {|
Line 0             █
Line 1             ▀
Line 2
Line 3|}]

let%expect_test "show_scrollbars false hides the bars despite overflow" =
  let app = scrollbar_app ~show_scrollbars:false () in
  settled_frame app ~width:20 ~height:4;
  [%expect_exact {|
Line 0
Line 1
Line 2
Line 3|}]
