open Mosaic_ui

let render_boxed ?(width = 40) ?(height = 3) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors:false ~width:(width + 2) ~height:(height + 2) content

let make_tabs ?(with_desc = false) labels =
  List.map
    (fun label ->
      (label, if with_desc then Some (label ^ " description") else None))
    labels

let%expect_test "empty tab select renders blank" =
  render_boxed ~width:20 ~height:1
    (tab_select ~id:"t" ~show_description:false ~show_underline:false
       ~size:(size ~width:20 ~height:1) []);
  [%expect_exact
    {|
┌────────────────────┐
│                    │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "single tab without underline" =
  render_boxed ~width:20 ~height:1
    (tab_select ~id:"t" ~show_description:false ~show_underline:false ~tab_width:10
       ~size:(size ~width:20 ~height:1) (make_tabs [ "Home" ]));
  [%expect_exact
    {|
┌────────────────────┐
│ Home               │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "single tab with underline" =
  render_boxed ~width:20 ~height:2
    (tab_select ~id:"t" ~show_description:false ~show_underline:true ~tab_width:10
       ~size:(size ~width:20 ~height:2) (make_tabs [ "Home" ]));
  [%expect_exact
    {|
┌────────────────────┐
│ Home               │
│▬▬▬▬▬▬▬▬▬▬          │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "multiple tabs first selected" =
  render_boxed ~width:30 ~height:2
    (tab_select ~id:"t" ~show_description:false ~show_underline:true ~tab_width:10
       ~size:(size ~width:30 ~height:2)
       (make_tabs [ "Home"; "Settings"; "Help" ]));
  [%expect_exact
    {|
┌──────────────────────────────┐
│ Home      Settings  Help     │
│▬▬▬▬▬▬▬▬▬▬                    │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "tab with description" =
  render_boxed ~width:30 ~height:3
    (tab_select ~id:"t" ~show_description:true ~show_underline:true ~tab_width:15
       ~size:(size ~width:30 ~height:3)
       (make_tabs ~with_desc:true [ "Home"; "Settings" ]));
  [%expect_exact
    {|
┌──────────────────────────────┐
│ Home           Settings      │
│▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬               │
│ Home description             │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "narrow tabs truncate labels" =
  render_boxed ~width:16 ~height:1
    (tab_select ~id:"t" ~show_description:false ~show_underline:false ~tab_width:8
       ~size:(size ~width:16 ~height:1)
       (make_tabs [ "VeryLongLabel"; "Another" ]));
  [%expect_exact
    {|
┌────────────────┐
│ VeryL…  Anoth… │
└────────────────┘
|}] [@@ocamlformat "disable"]

(* Interaction tests *)

let%expect_test "set_selected_index changes tab selection" =
  render_boxed ~width:30 ~height:2
    (tab_select ~id:"t" ~show_description:false ~show_underline:true ~tab_width:10
       ~on_mount:(fun ts -> Tab_select.set_selected_index ts 1)
       ~size:(size ~width:30 ~height:2)
       (make_tabs [ "Home"; "Settings"; "Help" ]));
  [%expect_exact
    {|
┌──────────────────────────────┐
│ Home      Settings  Help     │
│          ▬▬▬▬▬▬▬▬▬▬          │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_selected_index to last tab" =
  render_boxed ~width:30 ~height:2
    (tab_select ~id:"t" ~show_description:false ~show_underline:true ~tab_width:10
       ~on_mount:(fun ts -> Tab_select.set_selected_index ts 2)
       ~size:(size ~width:30 ~height:2)
       (make_tabs [ "Home"; "Settings"; "Help" ]));
  [%expect_exact
    {|
┌──────────────────────────────┐
│ Home      Settings  Help     │
│                    ▬▬▬▬▬▬▬▬▬▬│
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_options replaces tabs and clamps selection" =
  render_boxed ~width:30 ~height:2
    (tab_select ~id:"t" ~show_description:false ~show_underline:true ~tab_width:10
       ~on_mount:(fun ts ->
         Tab_select.set_selected_index ts 3; (* select "D" *)
         Tab_select.set_options ts (make_tabs [ "X"; "Y" ])) (* now only 2 tabs *)
       ~size:(size ~width:30 ~height:2)
       (make_tabs [ "A"; "B"; "C"; "D" ]));
  [%expect_exact
    {|
┌──────────────────────────────┐
│ X         Y                  │
│          ▬▬▬▬▬▬▬▬▬▬          │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_show_description toggles description visibility" =
  render_boxed ~width:30 ~height:3
    (tab_select ~id:"t" ~show_description:true ~show_underline:true ~tab_width:15
       ~on_mount:(fun ts -> Tab_select.set_show_description ts false)
       ~size:(size ~width:30 ~height:3)
       (make_tabs ~with_desc:true [ "Home"; "Settings" ]));
  [%expect_exact
    {|
┌──────────────────────────────┐
│ Home           Settings      │
│▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬               │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_show_underline toggles underline" =
  render_boxed ~width:30 ~height:2
    (tab_select ~id:"t" ~show_description:false ~show_underline:true ~tab_width:10
       ~on_mount:(fun ts -> Tab_select.set_show_underline ts false)
       ~size:(size ~width:30 ~height:2) (make_tabs [ "Home"; "Settings" ]));
  [%expect_exact
    {|
┌──────────────────────────────┐
│ Home      Settings           │
│                              │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_tab_width changes tab sizing" =
  render_boxed ~width:30 ~height:2
    (tab_select ~id:"t" ~show_description:false ~show_underline:true ~tab_width:10
       ~on_mount:(fun ts -> Tab_select.set_tab_width ts 15)
       ~size:(size ~width:30 ~height:2) (make_tabs [ "Home"; "Settings" ]));
  [%expect_exact
    {|
┌──────────────────────────────┐
│ Home           Settings      │
│▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬               │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_wrap_selection setter is callable" =
  (* Smoke test: verifies set_wrap_selection can be called without error.
     Wrapping behavior itself requires keyboard input simulation which
     is not available in this test harness. *)
  render_boxed ~width:30 ~height:2
    (tab_select ~id:"t" ~show_description:false ~show_underline:true ~tab_width:10
       ~wrap_selection:false
       ~on_mount:(fun ts -> Tab_select.set_wrap_selection ts true)
       ~size:(size ~width:30 ~height:2) (make_tabs [ "A"; "B"; "C" ]));
  [%expect_exact
    {|
┌──────────────────────────────┐
│ A         B         C        │
│▬▬▬▬▬▬▬▬▬▬                    │
└──────────────────────────────┘
|}] [@@ocamlformat "disable"]
