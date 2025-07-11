open Mosaic
open Mosaic_tiles
open Test_utils

(** Test app that wraps a Progress component *)
module App = struct
  type model = { progress : Progress.model; quit : bool }
  type msg = Progress_msg of Progress.msg | Quit

  let create_app ?percent ?width ?show_percentage ?style_mode () =
    let init () =
      let progress_model, progress_cmd =
        Progress.init ?percent ?width ?show_percentage ?style_mode ()
      in
      ( { progress = progress_model; quit = false },
        Cmd.map (fun m -> Progress_msg m) progress_cmd )
    in
    let update msg model =
      match msg with
      | Progress_msg progress_msg ->
          let new_progress, progress_cmd =
            Progress.update progress_msg model.progress
          in
          ( { model with progress = new_progress },
            Cmd.map (fun m -> Progress_msg m) progress_cmd )
      | Quit -> ({ model with quit = true }, Cmd.quit)
    in
    let view model = Progress.view model.progress in
    let subscriptions model =
      Sub.batch
        [
          Sub.map
            (fun m -> Progress_msg m)
            (Progress.subscriptions model.progress);
          Sub.on_key ~ctrl:true (Char (Uchar.of_char 'c')) Quit;
        ]
    in
    Mosaic.app ~init ~update ~view ~subscriptions ()
end

(** Helper to print test output with borders *)
let print_test_output ?(width = 40) ?height:(_ = 3) output =
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

let%expect_test "Default progress bar (0%)" =
  let app = App.create_app ~show_percentage:false () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
    {|+----------------------------------------+
|░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░|
+----------------------------------------+
|}]

let%expect_test "Progress at 25%" =
  let app = App.create_app ~percent:0.25 ~show_percentage:false () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
    {|+----------------------------------------+
|██████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░|
+----------------------------------------+
|}]

let%expect_test "Progress at 50%" =
  let app = App.create_app ~percent:0.5 ~show_percentage:false () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
    {|+----------------------------------------+
|████████████████████░░░░░░░░░░░░░░░░░░░░|
+----------------------------------------+
|}]

let%expect_test "Progress at 75%" =
  let app = App.create_app ~percent:0.75 ~show_percentage:false () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
    {|+----------------------------------------+
|██████████████████████████████░░░░░░░░░░|
+----------------------------------------+
|}]

let%expect_test "Progress at 100%" =
  let app = App.create_app ~percent:1.0 ~show_percentage:false () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
    {|+----------------------------------------+
|████████████████████████████████████████|
+----------------------------------------+
|}]

let%expect_test "Progress with percentage display" =
  let app = App.create_app ~percent:0.42 ~show_percentage:true () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
    {|+----------------------------------------+
|████████████████░░░░░░░░░░░░░░░░░░░░░░░░|
+----------------------------------------+
|}]

let%expect_test "Custom width progress bar" =
  let app = App.create_app ~percent:0.6 ~width:20 () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
    {|+----------------------------------------+
|████████████░░░░░░░░  60%               |
+----------------------------------------+
|}]

let%expect_test "Custom width with percentage" =
  let app = App.create_app ~percent:0.33 ~width:20 ~show_percentage:true () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
    {|+----------------------------------------+
|██████░░░░░░░░░░░░░░  33%               |
+----------------------------------------+
|}]

let%expect_test "Gradient style mode" =
  let app =
    App.create_app ~percent:0.5
      ~style_mode:(Progress.Gradient (Style.Index 33, Style.Index 39))
      ~show_percentage:false ()
  in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
    {|+----------------------------------------+
|████████████████████░░░░░░░░░░░░░░░░░░░░|
+----------------------------------------+
|}]

let%expect_test "Update progress dynamically" =
  (* Initial (20%) *)
  let app1 = App.create_app ~percent:0.2 ~show_percentage:true () in
  let harness1 = Test_harness.create app1 in

  print_string "Initial (20%):\n";
  let output = Test_harness.view ~width:40 ~height:1 harness1 in
  print_test_output ~height:1 output;

  (* After increment (50%) *)
  let app2 = App.create_app ~percent:0.5 ~show_percentage:true () in
  let harness2 = Test_harness.create app2 in

  print_string "\nAfter increment (50%):\n";
  let output = Test_harness.view ~width:40 ~height:1 harness2 in
  print_test_output ~height:1 output;

  (* After set (85%) *)
  let app3 = App.create_app ~percent:0.85 ~show_percentage:true () in
  let harness3 = Test_harness.create app3 in

  print_string "\nAfter set (85%):\n";
  let output = Test_harness.view ~width:40 ~height:1 harness3 in
  print_test_output ~height:1 output;

  [%expect_exact
    {|Initial (20%):
+----------------------------------------+
|████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░|
+----------------------------------------+

After increment (50%):
+----------------------------------------+
|████████████████████░░░░░░░░░░░░░░░░░░░░|
+----------------------------------------+

After set (85%):
+----------------------------------------+
|██████████████████████████████████░░░░░░|
+----------------------------------------+
|}]

let%expect_test "Edge cases" =
  (* Test negative percent *)
  let app1 = App.create_app ~percent:(-0.5) ~show_percentage:true () in
  let harness1 = Test_harness.create app1 in

  print_string "Negative percent (clamped to 0%):\n";
  let output = Test_harness.view ~width:40 ~height:1 harness1 in
  print_test_output ~height:1 output;

  (* Test > 100% *)
  let app2 = App.create_app ~percent:1.5 ~show_percentage:true () in
  let harness2 = Test_harness.create app2 in

  print_string "\nOver 100% (clamped to 100%):\n";
  let output = Test_harness.view ~width:40 ~height:1 harness2 in
  print_test_output ~height:1 output;

  [%expect_exact
    {|Negative percent (clamped to 0%):
+----------------------------------------+
|░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░|
+----------------------------------------+

Over 100% (clamped to 100%):
+----------------------------------------+
|████████████████████████████████████████|
+----------------------------------------+
|}]

let%expect_test "Complete and reset" =
  (* Complete to 100% *)
  let app1 = App.create_app ~percent:1.0 ~show_percentage:true () in
  let harness1 = Test_harness.create app1 in

  print_string "After complete:\n";
  let output = Test_harness.view ~width:40 ~height:1 harness1 in
  print_test_output ~height:1 output;

  (* Reset to 0% *)
  let app2 = App.create_app ~percent:0.0 ~show_percentage:true () in
  let harness2 = Test_harness.create app2 in

  print_string "\nAfter reset:\n";
  let output = Test_harness.view ~width:40 ~height:1 harness2 in
  print_test_output ~height:1 output;

  [%expect_exact
    {|After complete:
+----------------------------------------+
|████████████████████████████████████████|
+----------------------------------------+

After reset:
+----------------------------------------+
|░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░|
+----------------------------------------+
|}]
