(* Simple example demonstrating the Mosaic TUI framework *)

open Mosaic

type model = { counter : int }

let init () = ({ counter = 0 }, Cmd.none)

let update msg model =
  match msg with
  | `Increment -> ({ counter = model.counter + 1 }, Cmd.none)
  | `Decrement -> ({ counter = model.counter - 1 }, Cmd.none)
  | `Quit -> (model, Cmd.quit)
  | `KeyEvent { key; modifier } -> (
      match (key, modifier) with
      (* Handle Ctrl+C *)
      | Char c, { ctrl = true; _ } when Uchar.to_int c = Char.code 'C' ->
          (model, Cmd.quit)
      | Char c, _
        when Uchar.to_int c = Char.code 'q' || Uchar.to_int c = Char.code 'Q' ->
          (model, Cmd.quit)
      | Escape, _ -> (model, Cmd.quit)
      | Char c, _ when Uchar.to_int c = Char.code '+' ->
          ({ counter = model.counter + 1 }, Cmd.none)
      | Up, _ -> ({ counter = model.counter + 1 }, Cmd.none)
      | Char c, _ when Uchar.to_int c = Char.code '-' ->
          ({ counter = model.counter - 1 }, Cmd.none)
      | Down, _ -> ({ counter = model.counter - 1 }, Cmd.none)
      | _ -> (model, Cmd.none))

let view model =
  let open Ui in
  let title =
    text ~style:Style.(fg (Index 5) ++ bold) "Mosaic Counter Example"
  in
  let counter_text =
    text
      ~style:Style.(fg (Index 2))
      (Printf.sprintf "Counter: %d" model.counter)
  in
  let help =
    text
      ~style:Style.(fg (Index 8))
      "Press +/↑ to increment, -/↓ to decrement, q to quit"
  in

  vbox ~gap:1 [ title; counter_text; text ""; help ]

let subscriptions _model = Sub.keyboard (fun key_event -> `KeyEvent key_event)

let () =
  let app = Mosaic.app ~init ~update ~view ~subscriptions () in
  Mosaic.run app
