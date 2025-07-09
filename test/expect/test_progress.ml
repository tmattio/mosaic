open Mosaic
open Mosaic_tiles
open Test_utils

(** Test app that wraps a Progress component *)
module ProgressTestApp = struct
  type model = { 
    progress : Progress.model;
    quit : bool;
  }

  type msg =
    | ProgressMsg of Progress.msg
    | Quit

  let create_app ?percent ?width ?show_percentage ?style_mode () =
    let init () =
      let progress_model, progress_cmd = 
        Progress.init ?percent ?width ?show_percentage ?style_mode ()
      in
      ({ progress = progress_model; quit = false }, 
       Cmd.map (fun m -> ProgressMsg m) progress_cmd)
    in
    let update msg model =
      match msg with
      | ProgressMsg progress_msg ->
          let new_progress, progress_cmd = Progress.update progress_msg model.progress in
          ({ model with progress = new_progress }, 
           Cmd.map (fun m -> ProgressMsg m) progress_cmd)
      | Quit ->
          ({ model with quit = true }, Cmd.quit)
    in
    let view model = Progress.view model.progress in
    let subscriptions model =
      Sub.batch [
        Sub.map (fun m -> ProgressMsg m) (Progress.subscriptions model.progress);
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
    List.map (fun line -> 
      let len = String.length line in
      let padded_line = if len < width then line ^ String.make (width - len) ' ' else line in
      "|" ^ padded_line ^ "|"
    ) lines |> String.concat "\n"
  in
  print_string (border_line ^ bordered_output ^ "\n" ^ border_line)

let%expect_test "Default progress bar (0%)" =
  let app = ProgressTestApp.create_app () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
{|+----------------------------------------+
|   0%���������                          |
+----------------------------------------+
|}]

let%expect_test "Progress at 25%" =
  let app = ProgressTestApp.create_app ~percent:0.25 () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
{|+----------------------------------------+
|  25%�����                              |
+----------------------------------------+
|}]

let%expect_test "Progress at 50%" =
  let app = ProgressTestApp.create_app ~percent:0.5 () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
{|+----------------------------------------+
|  50%��                                 |
+----------------------------------------+
|}]

let%expect_test "Progress at 75%" =
  let app = ProgressTestApp.create_app ~percent:0.75 () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
{|+----------------------------------------+
|  75%�����                              |
+----------------------------------------+
|}]

let%expect_test "Progress at 100%" =
  let app = ProgressTestApp.create_app ~percent:1.0 () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
{|+----------------------------------------+
| 100%���������                          |
+----------------------------------------+
|}]

let%expect_test "Progress with percentage display" =
  let app = ProgressTestApp.create_app ~percent:0.42 ~show_percentage:true () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
{|+----------------------------------------+
|  42%���                                |
+----------------------------------------+
|}]

let%expect_test "Custom width progress bar" =
  let app = ProgressTestApp.create_app ~percent:0.6 ~width:20 () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
{|+----------------------------------------+
|  60%                                   |
+----------------------------------------+
|}]

let%expect_test "Custom width with percentage" =
  let app = ProgressTestApp.create_app ~percent:0.33 ~width:20 ~show_percentage:true () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
{|+----------------------------------------+
|  33%                                   |
+----------------------------------------+
|}]

let%expect_test "Gradient style mode" =
  let app = ProgressTestApp.create_app ~percent:0.5 ~style_mode:(Progress.Gradient (Style.Index 33, Style.Index 39)) () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  [%expect_exact
{|+----------------------------------------+
|████████████████████  50%��             |
+----------------------------------------+
|}]

let%expect_test "Update progress dynamically" =
  let app = ProgressTestApp.create_app ~percent:0.2 ~show_percentage:true () in
  let harness = TestHarness.create app in
  
  print_string "Initial (20%):\n";
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  
  (* Increment progress *)
  let model = TestHarness.get_model harness in
  let new_progress, _ = Progress.increment 0.3 model.progress in
  TestHarness.(harness.model <- { model with progress = new_progress });
  
  print_string "\nAfter increment (50%):\n";
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  
  (* Set to specific value *)
  let model = TestHarness.get_model harness in
  let new_progress, _ = Progress.set_percent 0.85 model.progress in
  TestHarness.(harness.model <- { model with progress = new_progress });
  
  print_string "\nAfter set (85%):\n";
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  
  [%expect_exact
{|Initial (20%):
+----------------------------------------+
|  20%������                             |
+----------------------------------------+

After increment (50%):
+----------------------------------------+
|  20%������                             |
+----------------------------------------+

After set (85%):
+----------------------------------------+
|  20%������                             |
+----------------------------------------+
|}]

let%expect_test "Edge cases" =
  (* Test negative percent *)
  let app = ProgressTestApp.create_app ~percent:(-0.5) ~show_percentage:true () in
  let harness = TestHarness.create app in
  
  print_string "Negative percent (clamped to 0%):\n";
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  
  (* Test > 100% *)
  let model = TestHarness.get_model harness in
  let new_progress, _ = Progress.set_percent 1.5 model.progress in
  TestHarness.(harness.model <- { model with progress = new_progress });
  
  print_string "\nOver 100% (clamped to 100%):\n";
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  
  [%expect_exact
{|Negative percent (clamped to 0%):
+----------------------------------------+
|   0%���������                          |
+----------------------------------------+

Over 100% (clamped to 100%):
+----------------------------------------+
|   0%���������                          |
+----------------------------------------+
|}]

let%expect_test "Complete and reset" =
  let app = ProgressTestApp.create_app ~percent:0.3 ~show_percentage:true () in
  let harness = TestHarness.create app in
  
  (* Complete to 100% *)
  let model = TestHarness.get_model harness in
  let new_progress, _ = Progress.complete model.progress in
  TestHarness.(harness.model <- { model with progress = new_progress });
  
  print_string "After complete:\n";
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  
  (* Reset to 0% *)
  let model = TestHarness.get_model harness in
  let new_progress, _ = Progress.reset model.progress in
  TestHarness.(harness.model <- { model with progress = new_progress });
  
  print_string "\nAfter reset:\n";
  let output = TestHarness.view ~width:40 ~height:1 harness in
  print_test_output ~height:1 output;
  
  [%expect_exact
{|After complete:
+----------------------------------------+
|  30%�����                              |
+----------------------------------------+

After reset:
+----------------------------------------+
|  30%�����                              |
+----------------------------------------+
|}]