open Mosaic_ui
open Expect_harness

let branch label =
  Vnode.box ~border:true
    ~style:
      (Toffee.Style.default
      |> Toffee.Style.set_size
           {
             Toffee.Geometry.Size.width = Toffee.Style.Dimension.percent 1.;
             height = Toffee.Style.Dimension.length 3.;
           })
    [ Vnode.text label ]

let view =
  Vnode.viewport_switch ~at_least_width:30 ~wide:(branch "wide branch")
    ~narrow:(branch "narrow branch")

let%expect_test "selects narrow branch below viewport threshold" =
  render ~width:20 ~height:4 view;
  [%expect_exact
    {|
┌──────────────────┐
│narrow branch     │
└──────────────────┘
|}]

let%expect_test "selects wide branch at viewport threshold" =
  render ~width:30 ~height:4 view;
  [%expect_exact
    {|
┌────────────────────────────┐
│wide branch                 │
└────────────────────────────┘
|}]

let%expect_test "reselects on resize crossing with the same vnode" =
  let app = make_app () in
  reconcile ~viewport_width:20 app view;
  frame app ~width:20 ~height:4;
  [%expect_exact
    {|
┌──────────────────┐
│narrow branch     │
└──────────────────┘
|}];
  reconcile ~viewport_width:30 app view;
  frame app ~width:30 ~height:4;
  [%expect_exact
    {|
┌────────────────────────────┐
│wide branch                 │
└────────────────────────────┘
|}];
  reconcile ~viewport_width:20 app view;
  frame app ~width:20 ~height:4;
  [%expect_exact
    {|
┌──────────────────┐
│narrow branch     │
└──────────────────┘
|}]
