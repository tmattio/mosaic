open Mosaic

type model = int
type msg = [ `Key_event of Input.key_event | `Tick | `Quit ]

let init () : model * msg Cmd.t = (5, Cmd.after 1.0 `Tick)

let update (msg : msg) (model : model) : model * msg Cmd.t =
  match msg with
  | `Tick ->
      let new_count = model - 1 in
      if new_count <= 0 then (new_count, Cmd.quit)
      else (new_count, Cmd.after 1.0 `Tick)
  | `Quit -> (model, Cmd.quit)
  | `Key_event { key; modifier; _ } -> (
      match (key, modifier) with
      | Char c, { ctrl = true; _ } when Uchar.to_int c = Char.code 'C' ->
          (model, Cmd.quit)
      | Char c, _
        when Uchar.to_int c = Char.code 'q' || Uchar.to_int c = Char.code 'Q' ->
          (model, Cmd.quit)
      | Escape, _ -> (model, Cmd.quit)
      | _ -> (model, Cmd.none))

let view model =
  let open Ui in
  let message =
    if model > 0 then
      Printf.sprintf "Hi. This program will exit in %d seconds." model
    else "Bye!"
  in
  vbox ~gap:1
    [
      text ~style:Style.(fg (Index 5)) message;
      text "";
      text
        ~style:Style.(fg (Index 8))
        "To quit sooner press ctrl-c, or q to quit...";
    ]

let subscriptions _model = Sub.keyboard (fun key_event -> `Key_event key_event)

let () =
  let app = Mosaic.app ~init ~update ~view ~subscriptions () in
  Mosaic.run app
