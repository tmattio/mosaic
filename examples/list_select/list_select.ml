open Mosaic

type model = {
  select : string Mosaic_tiles.Select.model;
  choice : string option;
  quitting : bool;
}

type msg =
  [ `SelectMsg of Mosaic_tiles.Select.msg
  | `Quit
  | `KeyEvent of key_event
  | `Choose ]

let food_items =
  [
    ("ramen", "Ramen");
    ("tomato_soup", "Tomato Soup");
    ("hamburgers", "Hamburgers");
    ("cheeseburgers", "Cheeseburgers");
    ("currywurst", "Currywurst");
    ("okonomiyaki", "Okonomiyaki");
    ("pasta", "Pasta");
    ("fillet_mignon", "Fillet Mignon");
    ("caviar", "Caviar");
    ("just_wine", "Just Wine");
  ]

let init () : model * msg Cmd.t =
  let select_model, select_cmd =
    Mosaic_tiles.Select.init ~placeholder:"What do you want for dinner?"
      ~options:food_items ~height:10 ()
  in
  let select_model, focus_cmd = Mosaic_tiles.Select.focus select_model in
  ( { select = select_model; choice = None; quitting = false },
    Cmd.batch
      [
        Cmd.map (fun m -> `SelectMsg m) select_cmd;
        Cmd.map (fun m -> `SelectMsg m) focus_cmd;
      ] )

let update (msg : msg) (model : model) : model * msg Cmd.t =
  match msg with
  | `SelectMsg select_msg ->
      let new_select, cmd =
        Mosaic_tiles.Select.update select_msg model.select
      in
      ({ model with select = new_select }, Cmd.map (fun m -> `SelectMsg m) cmd)
  | `Choose -> (
      match Mosaic_tiles.Select.value model.select with
      | Some value -> ({ model with choice = Some value }, Cmd.quit)
      | None -> (model, Cmd.none))
  | `Quit -> ({ model with quitting = true }, Cmd.quit)
  | `KeyEvent { key; modifier } -> (
      match (key, modifier) with
      | Char c, { ctrl = true; _ } when Uchar.to_int c = Char.code 'C' ->
          ({ model with quitting = true }, Cmd.quit)
      | Char c, _
        when Uchar.to_int c = Char.code 'q' || Uchar.to_int c = Char.code 'Q' ->
          ({ model with quitting = true }, Cmd.quit)
      | Enter, _ -> (
          match Mosaic_tiles.Select.value model.select with
          | Some value -> ({ model with choice = Some value }, Cmd.quit)
          | None -> (model, Cmd.none))
      | _ -> (model, Cmd.none))

let view model =
  let open Ui in
  if model.choice <> None then
    let choice_name =
      match model.choice with
      | Some key ->
          Stdlib.List.find_opt (fun (k, _) -> k = key) food_items
          |> Option.map snd
          |> Option.value ~default:"Unknown"
      | None -> ""
    in
    vbox ~gap:1
      [
        text "";
        text
          ~style:Style.(fg (Index 5))
          (Printf.sprintf "    %s? Sounds good to me." choice_name);
        text "";
      ]
  else if model.quitting then
    vbox ~gap:1
      [
        text "";
        text ~style:Style.(fg (Index 5)) "    Not hungry? That's cool.";
        text "";
      ]
  else
    vbox
      [
        text "";
        text ~style:Style.(fg (Index 5) ++ bold) "What do you want for dinner?";
        text "";
        Mosaic_tiles.Select.view model.select;
      ]

let subscriptions model =
  Sub.batch
    [
      Sub.map
        (fun m -> `SelectMsg m)
        (Mosaic_tiles.Select.subscriptions model.select);
      Sub.keyboard (fun key_event -> `KeyEvent key_event);
    ]

let () =
  let app = Mosaic.app ~init ~update ~view ~subscriptions () in
  Mosaic.run ~debug:true ~alt_screen:false app
