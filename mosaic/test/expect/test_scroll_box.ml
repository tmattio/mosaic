open Mosaic_tea
module Renderable = Mosaic_ui.Renderable
module Mouse = Mosaic_ui.Event.Mouse
module Key = Matrix.Input.Key

type model = { scroll : int }
type msg = Scrolled of int

let init () = ({ scroll = 0 }, Cmd.none)

let update (Scrolled y) model =
  if model.scroll = y then (model, Cmd.none) else ({ scroll = y }, Cmd.none)

let items =
  List.init 20 (fun i -> text ~content:(Printf.sprintf "Row %02d" (i + 1)) ())

let view ~scroll_ref model =
  let header =
    box ~size:(size ~width:22 ~height:1)
      [ text ~content:(Printf.sprintf "scroll=%d" model.scroll) () ]
  in
  let scroll =
    scroll_box
      ~ref:(fun n -> scroll_ref := Some n)
      ~flex_grow:1.
      ~on_scroll:(fun ~x:_ ~y -> Some (Scrolled y))
      items
  in
  let scroll_area =
    box ~padding:(padding 1) ~flex_grow:1.
      [ box ~border:true ~flex_grow:1. [ scroll ] ]
  in
  box ~flex_direction:Column
    ~size:(size ~width:22 ~height:10)
    [ header; scroll_area ]

let scroll_event ~x ~y =
  Mouse.scroll ~x ~y ~direction:Mouse.Scroll_down ~delta:1
    ~modifiers:Key.no_modifier

let center_of_scroll_box scroll_ref =
  match !scroll_ref with
  | None -> failwith "scroll box ref was not set"
  | Some node ->
      let bounds = Renderable.bounds node in
      (bounds.x + (bounds.width / 2), bounds.y + (bounds.height / 2))

let%expect_test "nested scroll box should receive wheel events" =
  let scroll_ref = ref None in
  let t =
    Harness.create ~width:22 ~height:10 ~init ~update
      ~view:(view ~scroll_ref) ()
  in
  ignore (Harness.step t ~delta:0.0);
  let before = (Harness.model t).scroll in
  let x, y = center_of_scroll_box scroll_ref in
  Harness.handle_mouse t (scroll_event ~x ~y);
  ignore (Harness.step t ~delta:0.0);
  let after = (Harness.model t).scroll in
  Printf.printf "scroll before: %d\nscroll after: %d\n" before after;
  [%expect_exact {|scroll before: 0
scroll after: 1|}]
[@@ocamlformat "disable"]
