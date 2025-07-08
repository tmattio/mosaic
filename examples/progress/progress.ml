open Mosaic

type model = { progress : Mosaic_tiles.Progress.model; percent : float }

type msg =
  [ `ProgressMsg of Mosaic_tiles.Progress.msg
  | `Tick
  | `Quit
  | `KeyEvent of key_event ]

let init () : model * msg Cmd.t =
  let progress_model, progress_cmd =
    Mosaic_tiles.Progress.init ~percent:0.0 ~width:60 ~show_percentage:true
      ~style_mode:(Mosaic_tiles.Progress.Gradient (Index 205, Index 226))
      ()
  in
  ( { progress = progress_model; percent = 0.0 },
    Cmd.batch
      [ Cmd.map (fun m -> `ProgressMsg m) progress_cmd; Cmd.after 1.0 `Tick ] )

let update (msg : msg) (model : model) : model * msg Cmd.t =
  match msg with
  | `ProgressMsg progress_msg ->
      let new_progress, cmd =
        Mosaic_tiles.Progress.update progress_msg model.progress
      in
      ( { model with progress = new_progress },
        Cmd.map (fun m -> `ProgressMsg m) cmd )
  | `Tick ->
      let new_percent = model.percent +. 0.25 in
      if new_percent >= 1.0 then
        let new_progress, cmd =
          Mosaic_tiles.Progress.set_percent 1.0 model.progress
        in
        ( { progress = new_progress; percent = 1.0 },
          Cmd.batch [ Cmd.map (fun m -> `ProgressMsg m) cmd; Cmd.quit ] )
      else
        let new_progress, cmd =
          Mosaic_tiles.Progress.set_percent new_percent model.progress
        in
        ( { progress = new_progress; percent = new_percent },
          Cmd.batch
            [ Cmd.map (fun m -> `ProgressMsg m) cmd; Cmd.after 1.0 `Tick ] )
  | `Quit -> (model, Cmd.quit)
  | `KeyEvent _ -> (model, Cmd.quit)

let view model =
  let open Ui in
  vbox
    [
      text "";
      hbox [ text "  "; Mosaic_tiles.Progress.view model.progress ];
      text "";
      text ~style:Style.(fg (Index 8)) "  Press any key to quit";
    ]

let subscriptions model =
  Sub.batch
    [
      Sub.map
        (fun m -> `ProgressMsg m)
        (Mosaic_tiles.Progress.subscriptions model.progress);
      Sub.keyboard (fun key_event -> `KeyEvent key_event);
    ]

let () =
  let app = Mosaic.app ~init ~update ~view ~subscriptions () in
  Mosaic.run app
