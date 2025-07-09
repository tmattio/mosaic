open Mosaic
open Mosaic_tiles
open Test_utils

(** Test app that wraps an Input component *)
module InputTestApp = struct
  type model = { input : Input.model; quit : bool }
  type msg = InputMsg of Input.msg | Quit

  let create_app ?placeholder ?initial_value ?password ?suggestions ?validate
      ?width () =
    let init () =
      let input_model, input_cmd =
        Input.init ?placeholder ?initial_value ?is_password:password
          ?suggestions ?validate ?width ()
      in
      (* Focus the input by default so keyboard events work *)
      let focused_model, focus_cmd = Input.focus input_model in
      ( { input = focused_model; quit = false },
        Cmd.batch
          [
            Cmd.map (fun m -> InputMsg m) input_cmd;
            Cmd.map (fun m -> InputMsg m) focus_cmd;
          ] )
    in
    let update msg model =
      match msg with
      | InputMsg input_msg ->
          let new_input, input_cmd = Input.update input_msg model.input in
          ( { model with input = new_input },
            Cmd.map (fun m -> InputMsg m) input_cmd )
      | Quit -> ({ model with quit = true }, Cmd.quit)
    in
    let view model = Input.view model.input in
    let subscriptions model =
      Sub.batch
        [
          Sub.map (fun m -> InputMsg m) (Input.subscriptions model.input);
          Sub.on_key ~ctrl:true (Char (Uchar.of_char 'c')) Quit;
        ]
    in
    Mosaic.app ~init ~update ~view ~subscriptions ()
end

(** Helper to print test output with borders *)
let print_test_output ?(width = 40) ?height:(_ = 10) output =
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

let%expect_test "Basic input with placeholder" =
  let app = InputTestApp.create_app ~placeholder:"Enter your name..." () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│                                      │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Input with initial value" =
  let app = InputTestApp.create_app ~initial_value:"Hello, World!" () in
  let harness = TestHarness.create app in
  let output = TestHarness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Hello, World!                        │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Typing in input" =
  let app = InputTestApp.create_app ~placeholder:"Type here..." () in
  let harness = TestHarness.create app in

  (* Type some text *)
  TestHarness.push_input "OCaml" harness;

  let output = TestHarness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ OCaml                                │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Password input" =
  let app =
    InputTestApp.create_app ~placeholder:"Enter password..." ~password:true ()
  in
  let harness = TestHarness.create app in

  (* Type a password *)
  TestHarness.push_input "secret123" harness;

  let output = TestHarness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ *********                            │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Input with validation error" =
  let validate input =
    if String.length input < 5 then Error "Must be at least 5 characters"
    else Ok ()
  in
  let app =
    InputTestApp.create_app ~placeholder:"Min 5 chars..." ~validate ()
  in
  let harness = TestHarness.create app in

  (* Type too short text *)
  TestHarness.push_input "Hi" harness;

  let output = TestHarness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Hi                                   │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Input with suggestions" =
  let suggestions = [ "OCaml"; "Rust"; "Go"; "Python"; "JavaScript" ] in
  let app = InputTestApp.create_app ~placeholder:"Search..." ~suggestions () in
  let harness = TestHarness.create app in

  (* Type to filter suggestions *)
  TestHarness.push_input "o" harness;

  let output = TestHarness.view ~width:40 ~height:8 harness in
  print_test_output ~height:8 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ o                                    │|
|└──────────────────────────────────────┘|
|┌────────────────────────────────────┐  |
|│                                    │  |
|│ OCaml                              │  |
|│                                    │  |
|└────────────────────────────────────┘  |
+----------------------------------------+
|}]

let%expect_test "Navigate and select suggestion" =
  let suggestions = [ "OCaml"; "Rust"; "Go" ] in
  let app = InputTestApp.create_app ~suggestions () in
  let harness = TestHarness.create app in

  (* Type to show suggestions *)
  TestHarness.push_input "o" harness;

  (* Navigate down *)
  TestHarness.push_key_event (key Down) harness;

  (* Select with Tab *)
  TestHarness.push_key_event (key Tab) harness;

  let output = TestHarness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ OCaml                                │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Backspace and cursor movement" =
  let app = InputTestApp.create_app () in
  let harness = TestHarness.create app in

  (* Type some text *)
  TestHarness.push_input "Hello World" harness;

  (* Move cursor left *)
  TestHarness.push_key_event (key Left) harness;
  TestHarness.push_key_event (key Left) harness;
  TestHarness.push_key_event (key Left) harness;
  TestHarness.push_key_event (key Left) harness;
  TestHarness.push_key_event (key Left) harness;

  (* Delete a character *)
  TestHarness.push_key_event (key Backspace) harness;

  let output = TestHarness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ HelloWorld                           │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Home and End keys" =
  let app = InputTestApp.create_app ~initial_value:"Start Middle End" () in
  let harness = TestHarness.create app in

  (* Move to beginning *)
  TestHarness.push_key_event (key Home) harness;

  (* Type at beginning *)
  TestHarness.push_input ">" harness;

  let output1 = TestHarness.view ~width:40 ~height:3 harness in
  print_string "After Home and typing:\n";
  print_test_output ~height:3 output1;

  (* Move to end *)
  TestHarness.push_key_event (key End) harness;

  (* Type at end *)
  TestHarness.push_input "<" harness;

  let output2 = TestHarness.view ~width:40 ~height:3 harness in
  print_string "\nAfter End and typing:\n";
  print_test_output ~height:3 output2;

  [%expect_exact
    {|After Home and typing:
+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ >Start Middle End                    │|
|└──────────────────────────────────────┘|
+----------------------------------------+

After End and typing:
+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ >Start Middle End<                   │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Fixed width input" =
  let app =
    InputTestApp.create_app ~width:20 ~placeholder:"Fixed width..." ()
  in
  let harness = TestHarness.create app in

  (* Type text longer than width *)
  TestHarness.push_input "This is a very long text that exceeds the width"
    harness;

  let output = TestHarness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────┐                    |
|│ This is a very long text that exceeds |
|└──────────────────┘                    |
+----------------------------------------+
|}]

let%expect_test "Clear input with Ctrl+U" =
  let app = InputTestApp.create_app ~initial_value:"Clear me!" () in
  let harness = TestHarness.create app in

  (* Clear with Ctrl+U *)
  TestHarness.push_key_event (key ~ctrl:true (Char (Uchar.of_char 'u'))) harness;

  let output = TestHarness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│                                      │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]
