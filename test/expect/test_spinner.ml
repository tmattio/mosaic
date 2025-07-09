open Mosaic
open Mosaic_tiles
open Test_utils

(** Test app that wraps a Spinner component *)
module SpinnerTestApp = struct
  type model = { spinner : Spinner.model; frame_count : int; quit : bool }
  type msg = SpinnerMsg of Spinner.msg | NextFrame | Quit

  let create_app ?style ?color () =
    let init () =
      let spinner_model, spinner_cmd = Spinner.init ?style ?color () in
      (* Start the spinner immediately *)
      let started_model, start_cmd = Spinner.start spinner_model in
      ( { spinner = started_model; frame_count = 0; quit = false },
        Cmd.batch
          [
            Cmd.map (fun m -> SpinnerMsg m) spinner_cmd;
            Cmd.map (fun m -> SpinnerMsg m) start_cmd;
          ] )
    in
    let update msg model =
      match msg with
      | SpinnerMsg spinner_msg ->
          let new_spinner, spinner_cmd =
            Spinner.update spinner_msg model.spinner
          in
          ( { model with spinner = new_spinner },
            Cmd.map (fun m -> SpinnerMsg m) spinner_cmd )
      | NextFrame ->
          (* Simulate time passing to trigger spinner animation *)
          ({ model with frame_count = model.frame_count + 1 }, Cmd.none)
      | Quit -> ({ model with quit = true }, Cmd.quit)
    in
    let view model =
      let open Ui in
      vbox
        [
          Spinner.view model.spinner;
          text (Printf.sprintf " Frame: %d" model.frame_count);
        ]
    in
    let subscriptions model =
      Sub.batch
        [
          Sub.map (fun m -> SpinnerMsg m) (Spinner.subscriptions model.spinner);
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
  let app = SpinnerTestApp.create_app () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|⠋                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Line spinner style" =
  let app = SpinnerTestApp.create_app ~style:Spinner.line () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:20 ~height:3 harness in
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
    SpinnerTestApp.create_app ~style:Spinner.arrow ~color:(Style.fg Style.Green)
      ()
  in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|←                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Box bounce spinner" =
  let app = SpinnerTestApp.create_app ~style:Spinner.box_bounce () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|▖                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Circle spinner" =
  let app = SpinnerTestApp.create_app ~style:Spinner.circle () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|◐                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Bar spinner" =
  let app = SpinnerTestApp.create_app ~style:Spinner.bar () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|▏                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Pulse spinner" =
  let app = SpinnerTestApp.create_app ~style:Spinner.pulse () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|▁                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Bounce spinner" =
  let app = SpinnerTestApp.create_app ~style:Spinner.bounce () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|⠁                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Custom spinner from string" =
  let custom = Spinner.from_string "|/-\\" in
  let app = SpinnerTestApp.create_app ~style:custom () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
||                   |
| Frame: 0           |
|                    |
+--------------------+
|}]

let%expect_test "Custom spinner with frames" =
  let custom = Spinner.custom [| "◐"; "◓"; "◑"; "◒" |] 0.2 in
  let app = SpinnerTestApp.create_app ~style:custom () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:20 ~height:3 harness in
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
      let spinner_model, spinner_cmd = Spinner.init () in
      (* Stop the spinner (it starts automatically) *)
      let stopped_spinner = Spinner.stop spinner_model in
      ( {
          SpinnerTestApp.spinner = stopped_spinner;
          frame_count = 0;
          quit = false;
        },
        Cmd.map (fun m -> SpinnerTestApp.SpinnerMsg m) spinner_cmd )
    in
    let update msg model =
      match msg with
      | SpinnerTestApp.SpinnerMsg spinner_msg ->
          let new_spinner, spinner_cmd =
            Spinner.update spinner_msg model.SpinnerTestApp.spinner
          in
          ( { model with SpinnerTestApp.spinner = new_spinner },
            Cmd.map (fun m -> SpinnerTestApp.SpinnerMsg m) spinner_cmd )
      | SpinnerTestApp.NextFrame ->
          ({ model with frame_count = model.frame_count + 1 }, Cmd.none)
      | SpinnerTestApp.Quit -> ({ model with quit = true }, Cmd.quit)
    in
    let view model =
      let open Ui in
      vbox
        [
          text
            (if Spinner.is_spinning model.SpinnerTestApp.spinner then "Spinning"
             else "Stopped");
          Spinner.view model.SpinnerTestApp.spinner;
        ]
    in
    let subscriptions model =
      Sub.batch
        [
          Sub.map
            (fun m -> SpinnerTestApp.SpinnerMsg m)
            (Spinner.subscriptions model.SpinnerTestApp.spinner);
          Sub.on_key ~ctrl:true (Char (Uchar.of_char 'c')) SpinnerTestApp.Quit;
        ]
    in
    Mosaic.app ~init ~update ~view ~subscriptions ()
  in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:20 ~height:3 harness in
  print_test_output output;
  [%expect_exact
    {|+--------------------+
|Stopped             |
|⠋                   |
|                    |
+--------------------+
|}]
