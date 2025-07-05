(* Interactive example with mouse support and multiple components *)

open Mosaic

let take n list =
  let rec aux n acc = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | h :: t -> aux (n - 1) (h :: acc) t
  in
  aux n [] list

type model = {
  cursor_x : int;
  cursor_y : int;
  clicks : (int * int) list;
  focused : bool;
  window_size : int * int;
  input_text : string;
  messages : string list;
}

let init () =
  let model =
    {
      cursor_x = 0;
      cursor_y = 0;
      clicks = [];
      focused = true;
      window_size = (80, 24);
      input_text = "";
      messages = [ "Welcome to Mosaic!" ];
    }
  in
  (model, Cmd.none)

let update msg model =
  match msg with
  | `MouseMove (x, y) -> ({ model with cursor_x = x; cursor_y = y }, Cmd.none)
  | `MouseClick (x, y) ->
      let clicks = (x, y) :: model.clicks |> take 10 in
      let messages =
        Printf.sprintf "Clicked at (%d, %d)" x y :: model.messages |> take 20
      in
      ({ model with clicks; messages }, Cmd.none)
  | `Resize (w, h) ->
      let messages =
        Printf.sprintf "Resized to %dx%d" w h :: model.messages |> take 20
      in
      ({ model with window_size = (w, h); messages }, Cmd.none)
  | `Focus ->
      let messages = "Terminal focused" :: model.messages |> take 20 in
      ({ model with focused = true; messages }, Cmd.none)
  | `Blur ->
      let messages = "Terminal blurred" :: model.messages |> take 20 in
      ({ model with focused = false; messages }, Cmd.none)
  | `TextInput text ->
      ({ model with input_text = model.input_text ^ text }, Cmd.none)
  | `KeyEvent { key; modifier } -> (
      match (key, modifier) with
      (* Handle Ctrl+C *)
      | Char c, { ctrl = true; _ } when Uchar.to_int c = Char.code 'C' ->
          (model, Cmd.quit)
      | Char c, _
        when Uchar.to_int c = Char.code 'q' || Uchar.to_int c = Char.code 'Q' ->
          (model, Cmd.quit)
      | Escape, _ -> (model, Cmd.quit)
      | Backspace, _ when String.length model.input_text > 0 ->
          let len = String.length model.input_text in
          ( { model with input_text = String.sub model.input_text 0 (len - 1) },
            Cmd.none )
      | Enter, _ when model.input_text <> "" ->
          let messages =
            Printf.sprintf "Input: %s" model.input_text :: model.messages
            |> take 20
          in
          ({ model with input_text = ""; messages }, Cmd.none)
      | Char c, { ctrl = false; _ }
        when Uchar.to_int c >= 32 && Uchar.to_int c < 127 ->
          ( {
              model with
              input_text = model.input_text ^ String.make 1 (Uchar.to_char c);
            },
            Cmd.none )
      | _ -> (model, Cmd.none))

let view model =
  let open Ui in
  let w, h = model.window_size in

  (* Header *)
  let header =
    hbox ~width:w
      [
        text
          ~style:Style.(bg (Index 4) ++ fg (Index 15) ++ bold)
          " Mosaic Interactive Demo ";
        space 1;
        text ~style:Style.(fg (Index 8)) (Printf.sprintf "Size: %dx%d" w h);
        space 1;
        text
          ~style:Style.(fg (if model.focused then Index 10 else Index 8))
          (if model.focused then "● Focused" else "○ Blurred");
      ]
  in

  (* Mouse info *)
  let mouse_info =
    vbox ~gap:0
      [
        text ~style:Style.(fg (Index 6) ++ underline) "Mouse Information";
        text (Printf.sprintf "Position: (%d, %d)" model.cursor_x model.cursor_y);
        text (Printf.sprintf "Clicks: %d" (List.length model.clicks));
      ]
  in

  (* Recent clicks *)
  let recent_clicks =
    let click_lines =
      model.clicks |> take 5
      |> List.map (fun (x, y) ->
             text
               ~style:Style.(fg (Index 3))
               (Printf.sprintf "  • (%d, %d)" x y))
    in
    vbox ~gap:0
      (text ~style:Style.(fg (Index 6) ++ underline) "Recent Clicks"
      :: click_lines)
  in

  (* Input field *)
  let input_field =
    vbox ~gap:0
      [
        text ~style:Style.(fg (Index 6) ++ underline) "Text Input";
        hbox
          [
            text "> ";
            text ~style:Style.(bg (Index 0)) model.input_text;
            text ~style:Style.(blink) "█";
          ];
      ]
  in

  (* Message log *)
  let message_log =
    let msgs =
      model.messages |> take 10
      |> List.map (fun msg -> text ~style:Style.(fg (Index 7)) msg)
    in
    vbox ~gap:0
      (text ~style:Style.(fg (Index 6) ++ underline) "Event Log" :: msgs)
  in

  (* Help *)
  let help =
    text
      ~style:Style.(fg (Index 8) ++ italic)
      "Type text and press Enter • Click anywhere • Press 'q' to quit"
  in

  (* Layout *)
  vbox ~gap:1
    [
      header;
      hbox ~gap:2
        [
          vbox ~gap:1 [ mouse_info; recent_clicks ];
          vbox ~gap:1 [ input_field; message_log ];
        ];
      space 1;
      help;
    ]

let subscriptions _ =
  Sub.batch
    [
      Sub.keyboard (fun key_event -> `KeyEvent key_event);
      Sub.mouse_filter (function
        | Motion (x, y, _, _) -> Some (`MouseMove (x, y))
        | _ -> None);
      Sub.on_left_click (fun x y -> `MouseClick (x, y));
      Sub.on_resize (fun width height -> `Resize (width, height));
      Sub.on_focus `Focus;
      Sub.on_blur `Blur;
    ]

let () =
  let app = Mosaic.app ~init ~update ~view ~subscriptions () in
  Mosaic.run ~mouse:true app
