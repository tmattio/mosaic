open Mosaic

type model = { selected : int }
type msg = Up | Down | Quit

let init () = ({ selected = 0 }, Cmd.none)

let update msg model =
  match msg with
  | Up -> ({ selected = max 0 (model.selected - 1) }, Cmd.none)
  | Down -> ({ selected = min 2 (model.selected + 1) }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let view model =
  let open Ui in
  let style_selected = Style.(fg (Index 2) ++ bold) in
  let style_normal = Style.(fg (Index 7)) in

  let menu_item idx label =
    let style = if model.selected = idx then style_selected else style_normal in
    let prefix = if model.selected = idx then "> " else "  " in
    text ~style (prefix ^ label)
  in

  vbox ~gap:1
    [
      (* Header with border *)
      hbox
        ~border:(border ~style:Rounded ~color:(Style.Index 6) ())
        ~padding:(padding_all 1) ~justify_content:Center
        [ text ~style:Style.(fg (Index 6) ++ bold) "Layout Demo" ];
      (* Main content area *)
      hbox ~gap:2
        [
          (* Left sidebar with border and padding *)
          vbox ~width:20 ~border:(border ~style:Solid ())
            ~padding:(pad ~top:1 ~left:2 ~right:2 ~bottom:1 ())
            [
              text ~style:Style.bold "Menu";
              space 1;
              menu_item 0 "First Item";
              menu_item 1 "Second Item";
              menu_item 2 "Third Item";
            ];
          (* Main content with different alignment *)
          vbox
            ~border:(border ~style:Double ~color:(Style.Index 5) ())
            ~padding:(padding_all 2) ~align_items:Center ~width:40 ~height:15
            [
              text ~style:Style.(fg (Index 3) ++ bold) "Content Area";
              space 2;
              text "This demonstrates the new";
              text "layout features including:";
              space 1;
              text "• Padding";
              text "• Borders (multiple styles)";
              text "• Alignment";
              text "• Fixed dimensions";
              expand (space 1);
              (* This will expand to fill space *)
              text ~style:Style.dim "Press ↑/↓ to navigate, q to quit";
            ];
          (* Right panel with ASCII border *)
          vbox ~width:20 ~border:(border ~style:ASCII ())
            ~padding:(padding_xy 1 1)
            [
              text ~style:Style.bold "Info";
              space 1;
              text "Selected:";
              text ~style:style_selected (string_of_int model.selected);
            ];
        ];
      (* Footer *)
      hbox ~padding:(pad ~top:1 ()) ~justify_content:End
        [ text ~style:Style.dim "Press 'q' to quit" ];
    ]

let subscriptions _model =
  Sub.keyboard_filter (function
    (* Handle Ctrl+C *)
    | { key = Char c; modifier = { ctrl = true; _ } }
      when Uchar.to_int c = Char.code 'C' ->
        Some Quit
    | { key = Char c; _ } when Uchar.to_char c = 'q' -> Some Quit
    | { key = Char c; _ } when Uchar.to_char c = 'j' -> Some Down
    | { key = Down; _ } -> Some Down
    | { key = Char c; _ } when Uchar.to_char c = 'k' -> Some Up
    | { key = Up; _ } -> Some Up
    | _ -> None)

let () =
  let app = Mosaic.app ~init ~update ~view ~subscriptions () in
  Mosaic.run app
