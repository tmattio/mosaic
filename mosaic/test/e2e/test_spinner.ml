open Mosaic
open Test_utils

(** Test app that wraps a Spinner component *)
module App = struct
  type model = {
    spinner : Mosaic_tiles.Spinner.model;
    frame_count : int;
    quit : bool;
  }

  type msg = Spinner_msg of Mosaic_tiles.Spinner.msg | NextFrame | Quit

  let create_app ?style ?color () =
    let init () =
      let spinner_model, spinner_cmd =
        Mosaic_tiles.Spinner.init ?style ?color ()
      in
      (* Start the spinner immediately *)
      let started_model, start_cmd = Mosaic_tiles.Spinner.start spinner_model in
      ( { spinner = started_model; frame_count = 0; quit = false },
        Cmd.batch
          [
            Cmd.map (fun m -> Spinner_msg m) spinner_cmd;
            Cmd.map (fun m -> Spinner_msg m) start_cmd;
          ] )
    in
    let update msg model =
      match msg with
      | Spinner_msg spinner_msg ->
          let new_spinner, spinner_cmd =
            Mosaic_tiles.Spinner.update spinner_msg model.spinner
          in
          ( { model with spinner = new_spinner },
            Cmd.map (fun m -> Spinner_msg m) spinner_cmd )
      | NextFrame ->
          (* Simulate time passing to trigger spinner animation *)
          ({ model with frame_count = model.frame_count + 1 }, Cmd.none)
      | Quit -> ({ model with quit = true }, Cmd.quit)
    in
    let view model =
      let open Ui in
      vbox
        [
          Mosaic_tiles.Spinner.view model.spinner;
          text (Printf.sprintf " Frame: %d" model.frame_count);
        ]
    in
    let subscriptions model =
      Sub.batch
        [
          Sub.map
            (fun m -> Spinner_msg m)
            (Mosaic_tiles.Spinner.subscriptions model.spinner);
          Sub.on_key ~ctrl:true (Char (Uchar.of_char 'c')) Quit;
        ]
    in
    Mosaic.app ~init ~update ~view ~subscriptions ()
end

(** Helper to print test output with borders *)
let print_test_output ?(width = 20) ?height:(_ = 3) output =
  let border_line = "+" ^ String.make width '-' ^ "+\n" in
  let lines = String.split_on_char '\n' output in
  let bordered_output =
    List.map
      (fun line ->
        let len = String.length line in
        let padded_line =
          if len < width then line ^ String.make (width - len) ' ' else line
        in
        "|" ^ padded_line ^ "|")
      lines
    |> String.concat "\n"
  in
  print_string (border_line ^ bordered_output ^ "\n" ^ border_line)

let%expect_test "Default dots spinner" =
  let app = App.create_app () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|⠋                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Line spinner style" =
  let app = App.create_app ~style:Mosaic_tiles.Spinner.line () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|-                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Arrow spinner with color" =
  let app =
    App.create_app ~style:Mosaic_tiles.Spinner.arrow
      ~color:(Style.fg Style.Green) ()
  in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|←                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Box bounce spinner" =
  let app = App.create_app ~style:Mosaic_tiles.Spinner.box_bounce () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|▖                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Circle spinner" =
  let app = App.create_app ~style:Mosaic_tiles.Spinner.circle () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|◐                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Bar spinner" =
  let app = App.create_app ~style:Mosaic_tiles.Spinner.bar () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|▏                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Pulse spinner" =
  let app = App.create_app ~style:Mosaic_tiles.Spinner.pulse () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|▁                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Bounce spinner" =
  let app = App.create_app ~style:Mosaic_tiles.Spinner.bounce () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|⠁                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Custom spinner from string" =
  let custom = Mosaic_tiles.Spinner.from_string "|/-\\" in
  let app = App.create_app ~style:custom () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
||                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Custom spinner with frames" =
  let custom = Mosaic_tiles.Spinner.custom [| "◐"; "◓"; "◑"; "◒" |] 0.2 in
  let app = App.create_app ~style:custom () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|◐                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Stopped spinner" =
  let app =
    let init () =
      let spinner_model, spinner_cmd = Mosaic_tiles.Spinner.init () in
      (* Stop the spinner (it starts automatically) *)
      let stopped_spinner = Mosaic_tiles.Spinner.stop spinner_model in
      ( { App.spinner = stopped_spinner; frame_count = 0; quit = false },
        Cmd.map (fun m -> App.Spinner_msg m) spinner_cmd )
    in
    let update msg model =
      match msg with
      | App.Spinner_msg spinner_msg ->
          let new_spinner, spinner_cmd =
            Mosaic_tiles.Spinner.update spinner_msg model.App.spinner
          in
          ( { model with App.spinner = new_spinner },
            Cmd.map (fun m -> App.Spinner_msg m) spinner_cmd )
      | App.NextFrame ->
          ({ model with frame_count = model.frame_count + 1 }, Cmd.none)
      | App.Quit -> ({ model with quit = true }, Cmd.quit)
    in
    let view model =
      let open Ui in
      vbox
        [
          text
            (if Mosaic_tiles.Spinner.is_spinning model.App.spinner then
               "Spinning"
             else "Stopped");
          Mosaic_tiles.Spinner.view model.App.spinner;
        ]
    in
    let subscriptions model =
      Sub.batch
        [
          Sub.map
            (fun m -> App.Spinner_msg m)
            (Mosaic_tiles.Spinner.subscriptions model.App.spinner);
          Sub.on_key ~ctrl:true (Char (Uchar.of_char 'c')) App.Quit;
        ]
    in
    Mosaic.app ~init ~update ~view ~subscriptions ()
  in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|Stopped             |
|⠋                   |
|                    |
+--------------------+
|}]
