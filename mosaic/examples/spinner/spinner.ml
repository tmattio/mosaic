open Mosaic

type model = { spinner : Mosaic_tiles.Spinner.model; quitting : bool }

type msg =
  [ `Spinner_msg of Mosaic_tiles.Spinner.msg
  | `Key_event of Input.key_event
  | `Quit ]

let init () : model * msg Cmd.t =
  let spinner_model, spinner_cmd =
    Mosaic_tiles.Spinner.init ~style:Mosaic_tiles.Spinner.dots
      ~color:Style.(fg (Index 205))
      ()
  in
  ( { spinner = spinner_model; quitting = false },
    Cmd.map (fun m -> `Spinner_msg m) spinner_cmd )

let update (msg : msg) (model : model) : model * msg Cmd.t =
  match msg with
  | `Spinner_msg spinner_msg ->
      let new_spinner, cmd =
        Mosaic_tiles.Spinner.update spinner_msg model.spinner
      in
      ( { model with spinner = new_spinner },
        Cmd.map (fun m -> `Spinner_msg m) cmd )
  | `Quit -> ({ model with quitting = true }, Cmd.quit)
  | `Key_event { key; modifier; _ } -> (
      match (key, modifier) with
      | Char c, { ctrl = true; _ } when Uchar.to_int c = Char.code 'C' ->
          ({ model with quitting = true }, Cmd.quit)
      | Char c, _
        when Uchar.to_int c = Char.code 'q' || Uchar.to_int c = Char.code 'Q' ->
          ({ model with quitting = true }, Cmd.quit)
      | Escape, _ -> ({ model with quitting = true }, Cmd.quit)
      | _ -> (model, Cmd.none))

let view model =
  let open Ui in
  vbox
    [
      text "";
      text "";
      hbox ~gap:1
        [
          text "   ";
          Mosaic_tiles.Spinner.view model.spinner;
          text "Loading forever...press q to quit";
        ];
      text "";
      text "";
      (if model.quitting then text "Quitting..." else text "");
    ]

let subscriptions model =
  Sub.batch
    [
      Sub.map
        (fun m -> `Spinner_msg m)
        (Mosaic_tiles.Spinner.subscriptions model.spinner);
      Sub.keyboard (fun key_event -> `Key_event key_event);
    ]

let () =
  let app = Mosaic.app ~init ~update ~view ~subscriptions () in
  Mosaic.run app
