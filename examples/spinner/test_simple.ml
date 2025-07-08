open Mosaic

type model = int
type msg = [ `Tick | `Quit | `KeyEvent of key_event ]

let frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let init () : model * msg Cmd.t = (0, Cmd.after 0.08 `Tick)

let update (msg : msg) (model : model) : model * msg Cmd.t =
  match msg with
  | `Tick -> ((model + 1) mod 10, Cmd.after 0.08 `Tick)
  | `Quit -> (model, Cmd.quit)
  | `KeyEvent _ -> (model, Cmd.quit)

let view model =
  let open Ui in
  let spinner = frames.(model) in
  vbox
    [
      text "";
      text "";
      text (Printf.sprintf "   %s Loading forever...press q to quit" spinner);
      text "";
      text ~style:Style.(fg (Index 8)) "   (press q to quit)";
    ]

let subscriptions _model = Sub.keyboard (fun _ -> `KeyEvent ())

let () =
  let app = Mosaic.app ~init ~update ~view ~subscriptions () in
  Mosaic.run app