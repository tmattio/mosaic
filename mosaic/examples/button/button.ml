open Mosaic
open Tile

let app () =
  let count, set_count, _ = use_state 0 in

  (* Simple button UI without mouse subscription causing re-renders *)
  vbox ~gap:(`Cells 2)
    [
      text (Printf.sprintf "Count: %d" count);
      button ~label:"Click me!" ~on_click:(fun () -> set_count (count + 1)) ();
      button ~label:"Reset" ~variant:`Secondary
        ~on_click:(fun () -> set_count 0)
        ();
      text "Use Tab to focus buttons, Enter/Space to click";
      text "Or click with mouse!";
    ]

let () = Mosaic.run ~mouse:true app
