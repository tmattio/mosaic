open Mosaic

type model = { input : Mosaic_tiles.Input.model }

type msg =
  [ `Input_msg of Mosaic_tiles.Input.msg
  | `Key_event of Input.key_event
  | `Submit
  | `Quit ]

let init () : model * msg Cmd.t =
  let input_model, input_cmd =
    Mosaic_tiles.Input.init ~placeholder:"Pikachu" ~width:20 ()
  in
  let input_model = Mosaic_tiles.Input.focus input_model |> fst in
  ({ input = input_model }, Cmd.map (fun m -> `Input_msg m) input_cmd)

let update (msg : msg) (model : model) : model * msg Cmd.t =
  match msg with
  | `Input_msg input_msg ->
      let new_input, cmd = Mosaic_tiles.Input.update input_msg model.input in
      ({ input = new_input }, Cmd.map (fun m -> `Input_msg m) cmd)
  | `Submit | `Quit -> (model, Cmd.quit)
  | `Key_event { key; modifier; _ } -> (
      match (key, modifier) with
      | Char c, { ctrl = true; _ } when Uchar.to_int c = Char.code 'C' ->
          (model, Cmd.quit)
      | Escape, _ -> (model, Cmd.quit)
      | Enter, _ -> (model, Cmd.quit)
      | _ -> (model, Cmd.none))

let view model =
  let open Ui in
  vbox ~gap:1
    [
      text ~style:Style.(fg (Index 5)) "What's your favorite Pokémon?";
      text "";
      Mosaic_tiles.Input.view model.input;
      text "";
      text ~style:Style.(fg (Index 8)) "(esc to quit)";
    ]

let subscriptions model =
  Sub.batch
    [
      Sub.map
        (fun m -> `Input_msg m)
        (Mosaic_tiles.Input.subscriptions model.input);
      Sub.keyboard (fun key_event -> `Key_event key_event);
    ]

let () =
  let app = Mosaic.app ~init ~update ~view ~subscriptions () in
  Mosaic.run ~debug:true app
