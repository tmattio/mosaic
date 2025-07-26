open Mosaic
open Test_utils

(** Test app that wraps an Input component *)
module App = struct
  open Mosaic_tiles

  type model = { input : Input.model; quit : bool }
  type msg = Input_msg of Input.msg | Quit

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
            Cmd.map (fun m -> Input_msg m) input_cmd;
            Cmd.map (fun m -> Input_msg m) focus_cmd;
          ] )
    in
    let update msg model =
      match msg with
      | Input_msg input_msg ->
          let new_input, input_cmd = Input.update input_msg model.input in
          ( { model with input = new_input },
            Cmd.map (fun m -> Input_msg m) input_cmd )
      | Quit -> ({ model with quit = true }, Cmd.quit)
    in
    let view model = Input.view model.input in
    let subscriptions model =
      Sub.batch
        [
          Sub.map (fun m -> Input_msg m) (Input.subscriptions model.input);
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
  let app = App.create_app ~placeholder:"Enter your name..." () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│                                      │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Input with initial value" =
  let app = App.create_app ~initial_value:"Hello, World!" () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Hello, World!                        │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Typing in input" =
  let app = App.create_app ~placeholder:"Type here..." () in
  let harness = Test_harness.create app in

  (* Type some text *)
  Test_harness.push_input "OCaml" harness;

  let output = Test_harness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ OCaml                                │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Password input" =
  let app = App.create_app ~placeholder:"Enter password..." ~password:true () in
  let harness = Test_harness.create app in

  (* Type a password *)
  Test_harness.push_input "secret123" harness;

  let output = Test_harness.view ~width:40 ~height:3 harness in
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
  let app = App.create_app ~placeholder:"Min 5 chars..." ~validate () in
  let harness = Test_harness.create app in

  (* Type too short text *)
  Test_harness.push_input "Hi" harness;

  let output = Test_harness.view ~width:40 ~height:3 harness in
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
  let app = App.create_app ~placeholder:"Search..." ~suggestions () in
  let harness = Test_harness.create app in

  (* Type to filter suggestions *)
  Test_harness.push_input "o" harness;

  let output = Test_harness.view ~width:40 ~height:8 harness in
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
  let app = App.create_app ~suggestions () in
  let harness = Test_harness.create app in

  (* Type to show suggestions *)
  Test_harness.push_input "o" harness;

  (* Navigate down *)
  Test_harness.push_key_event (Input.key Down) harness;

  (* Select with Tab *)
  Test_harness.push_key_event (Input.key Tab) harness;

  let output = Test_harness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ OCaml                                │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Backspace and cursor movement" =
  let app = App.create_app () in
  let harness = Test_harness.create app in

  (* Type some text *)
  Test_harness.push_input "Hello World" harness;

  (* Move cursor left *)
  Test_harness.push_key_event (Input.key Left) harness;
  Test_harness.push_key_event (Input.key Left) harness;
  Test_harness.push_key_event (Input.key Left) harness;
  Test_harness.push_key_event (Input.key Left) harness;
  Test_harness.push_key_event (Input.key Left) harness;

  (* Delete a character *)
  Test_harness.push_key_event (Input.key Backspace) harness;

  let output = Test_harness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ HelloWorld                           │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]

let%expect_test "Home and End keys" =
  let app = App.create_app ~initial_value:"Start Middle End" () in
  let harness = Test_harness.create app in

  (* Move to beginning *)
  Test_harness.push_key_event (Input.key Home) harness;

  (* Type at beginning *)
  Test_harness.push_input ">" harness;

  let output1 = Test_harness.view ~width:40 ~height:3 harness in
  print_string "After Home and typing:\n";
  print_test_output ~height:3 output1;

  (* Move to end *)
  Test_harness.push_key_event (Input.key End) harness;

  (* Type at end *)
  Test_harness.push_input "<" harness;

  let output2 = Test_harness.view ~width:40 ~height:3 harness in
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
  let app = App.create_app ~width:20 ~placeholder:"Fixed width..." () in
  let harness = Test_harness.create app in

  (* Type text longer than width *)
  Test_harness.push_input "This is a very long text that exceeds the width"
    harness;

  let output = Test_harness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────┐                    |
|│ This is a very l │                    |
|└──────────────────┘                    |
+----------------------------------------+
|}]

let%expect_test "Clear input with Ctrl+U" =
  let app = App.create_app ~initial_value:"Clear me!" () in
  let harness = Test_harness.create app in

  (* Clear with Ctrl+U *)
  Test_harness.push_key_event
    (Input.char ~modifier:{ Input.no_modifier with ctrl = true } 'u')
    harness;

  let output = Test_harness.view ~width:40 ~height:3 harness in
  print_test_output ~height:3 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│                                      │|
|└──────────────────────────────────────┘|
+----------------------------------------+
|}]
