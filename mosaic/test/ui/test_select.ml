open Mosaic_ui

let render_boxed ?(width = 20) ?(height = 8) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors:false ~width:(width + 2) ~height:(height + 2) content

let make_options ?(with_desc = false) names =
  List.map
    (fun name ->
      Select.
        {
          name;
          description = (if with_desc then Some ("Desc for " ^ name) else None);
        })
    names

let%expect_test "empty select renders blank" =
  render_boxed ~width:15 ~height:4
    (select ~id:"s" ~size:(size ~width:15 ~height:4) []);
  [%expect_exact
    {|
┌───────────────┐
│               │
│               │
│               │
│               │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "single item select" =
  render_boxed ~width:15 ~height:2
    (select ~id:"s" ~show_description:false ~size:(size ~width:15 ~height:2)
       (make_options [ "Option A" ]));
  [%expect_exact
    {|
┌───────────────┐
│ ▶ Option A    │
│               │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "multiple items with first selected" =
  render_boxed ~width:18 ~height:3
    (select ~id:"s" ~show_description:false ~size:(size ~width:18 ~height:3)
       (make_options [ "Alpha"; "Beta"; "Gamma" ]));
  [%expect_exact
    {|
┌──────────────────┐
│ ▶ Alpha          │
│   Beta           │
│   Gamma          │
└──────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "items with descriptions" =
  render_boxed ~width:20 ~height:6
    (select ~id:"s" ~show_description:true ~size:(size ~width:20 ~height:6)
       (make_options ~with_desc:true [ "Item1"; "Item2" ]));
  [%expect_exact
    {|
┌────────────────────┐
│ ▶ Item1            │
│   Desc for Item1   │
│   Item2            │
│   Desc for Item2   │
│                    │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "item spacing between items" =
  render_boxed ~width:16 ~height:5
    (select ~id:"s" ~show_description:false ~item_spacing:1
       ~size:(size ~width:16 ~height:5) (make_options [ "One"; "Two" ]));
  [%expect_exact
    {|
┌────────────────┐
│ ▶ One          │
│                │
│   Two          │
│                │
│                │
└────────────────┘
|}] [@@ocamlformat "disable"]

(* Interaction tests *)

let%expect_test "set_selected_index changes selection" =
  render_boxed ~width:18 ~height:3
    (select ~id:"s" ~show_description:false
       ~on_mount:(fun s -> Select.set_selected_index s 1)
       ~size:(size ~width:18 ~height:3)
       (make_options [ "Alpha"; "Beta"; "Gamma" ]));
  [%expect_exact
    {|
┌──────────────────┐
│   Alpha          │
│ ▶ Beta           │
│   Gamma          │
└──────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_selected_index to last item" =
  render_boxed ~width:18 ~height:3
    (select ~id:"s" ~show_description:false
       ~on_mount:(fun s -> Select.set_selected_index s 2)
       ~size:(size ~width:18 ~height:3)
       (make_options [ "Alpha"; "Beta"; "Gamma" ]));
  [%expect_exact
    {|
┌──────────────────┐
│   Alpha          │
│   Beta           │
│ ▶ Gamma          │
└──────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_options replaces options and clamps selection" =
  render_boxed ~width:18 ~height:3
    (select ~id:"s" ~show_description:false
       ~on_mount:(fun s ->
         Select.set_selected_index s 3; (* select "Four" *)
         Select.set_options s (make_options [ "A"; "B" ])) (* now only 2 items *)
       ~size:(size ~width:18 ~height:3)
       (make_options [ "One"; "Two"; "Three"; "Four" ]));
  [%expect_exact
    {|
┌──────────────────┐
│   A              │
│ ▶ B              │
│                  │
└──────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_show_description toggles description visibility" =
  render_boxed ~width:20 ~height:6
    (select ~id:"s" ~show_description:true
       ~on_mount:(fun s -> Select.set_show_description s false)
       ~size:(size ~width:20 ~height:6)
       (make_options ~with_desc:true [ "Item1"; "Item2" ]));
  [%expect_exact
    {|
┌────────────────────┐
│ ▶ Item1            │
│   Item2            │
│                    │
│                    │
│                    │
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_item_spacing changes spacing" =
  render_boxed ~width:16 ~height:6
    (select ~id:"s" ~show_description:false ~item_spacing:0
       ~on_mount:(fun s -> Select.set_item_spacing s 1)
       ~size:(size ~width:16 ~height:6) (make_options [ "One"; "Two"; "Three" ]));
  [%expect_exact
    {|
┌────────────────┐
│ ▶ One          │
│                │
│   Two          │
│                │
│   Three        │
│                │
└────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "scrolling select with many items" =
  render_boxed ~width:16 ~height:3
    (select ~id:"s" ~show_description:false
       ~on_mount:(fun s -> Select.set_selected_index s 3) (* select "D" *)
       ~size:(size ~width:16 ~height:3)
       (make_options [ "A"; "B"; "C"; "D"; "E" ]));
  [%expect_exact
    {|
┌────────────────┐
│   C            │
│ ▶ D            │
│   E            │
└────────────────┘
|}] [@@ocamlformat "disable"]
