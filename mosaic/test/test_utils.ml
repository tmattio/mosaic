(** Test utilities for Mosaic tests *)

open Mosaic

(* Helper functions for tests *)
let screen_to_string screen =
  let width = Screen.cols screen in
  let height = Screen.rows screen in
  let buf = Buffer.create ((width + 1) * height) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      (* After present(), the front buffer contains the rendered content *)
      match Grid.get (Screen.front screen) ~row:y ~col:x with
      | None -> Buffer.add_char buf ' '
      | Some cell ->
          (* Skip continuation cells for wide characters *)
          if Grid.Cell.is_continuation cell then ()
          else
            let text = Grid.Cell.get_text cell in
            if text = "" then Buffer.add_char buf ' '
            else Buffer.add_string buf text
    done;
    if y < height - 1 then Buffer.add_char buf '\n'
  done;
  Buffer.contents buf

(** Renders a UI element to a raw string representation of the terminal grid.
    This is the foundation for visual snapshot and expect testing. *)
let render_to_string ?(width = 80) ?(height = 24) element =
  let screen = Screen.create ~rows:height ~cols:width () in
  Screen.begin_frame screen;
  Ui.render screen element;
  let _ = Screen.present screen in
  screen_to_string screen

(** Helper to render a UI element to a string and print it for expect testing *)
let print_ui ?(width = 20) ?(height = 5) element =
  (* Wrap the element in a box with border for visual clarity *)
  let width = width + 2 in
  let height = height + 2 in
  let bordered_element =
    Ui.box ~width:(`Cells width) ~height:(`Cells height)
      ~border:Ui.Border.normal ~border_style:Ui.Style.empty [ element ]
  in
  let output = render_to_string ~width ~height bordered_element in
  print_string ("\n" ^ output ^ "\n")
