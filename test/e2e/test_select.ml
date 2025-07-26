open Mosaic
open Test_utils

(** Test app that wraps a Select component *)
module App = struct
  open Mosaic_tiles

  type model = { select : string Select.model; quit : bool }
  type msg = Select_msg of Select.msg | Quit

  let create_app ?options ?default ?height ?filterable ?placeholder () =
    let init () =
      let select_model, select_cmd =
        Select.init ?options ?default ?height ?filterable ?placeholder ()
      in
      (* Focus the select by default so keyboard events work *)
      let focused_model, focus_cmd = Select.focus select_model in
      ( { select = focused_model; quit = false },
        Cmd.batch
          [
            Cmd.map (fun m -> Select_msg m) select_cmd;
            Cmd.map (fun m -> Select_msg m) focus_cmd;
          ] )
    in
    let update msg model =
      match msg with
      | Select_msg select_msg ->
          let new_select, select_cmd = Select.update select_msg model.select in
          ( { model with select = new_select },
            Cmd.map (fun m -> Select_msg m) select_cmd )
      | Quit -> ({ model with quit = true }, Cmd.quit)
    in
    let view model = Select.view model.select in
    let subscriptions model =
      Sub.batch
        [
          Sub.map (fun m -> Select_msg m) (Select.subscriptions model.select);
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

let%expect_test "Basic select - closed dropdown" =
  let app =
    App.create_app
      ~options:
        [
          ("ocaml", "OCaml");
          ("rust", "Rust");
          ("go", "Go");
          ("python", "Python");
        ]
      ~placeholder:"Choose a language..." ()
  in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:10 harness in
  print_test_output output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Choose a language...               ▶ │|
|└──────────────────────────────────────┘|
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
+----------------------------------------+
|}]

let%expect_test "Select with default value" =
  let app =
    App.create_app
      ~options:
        [
          ("ocaml", "OCaml");
          ("rust", "Rust");
          ("go", "Go");
          ("python", "Python");
        ]
      ~default:"rust" ()
  in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:40 ~height:10 harness in
  print_test_output output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Rust                               ▶ │|
|└──────────────────────────────────────┘|
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
+----------------------------------------+
|}]

let%expect_test "Open dropdown with keyboard" =
  let app =
    App.create_app
      ~options:
        [
          ("ocaml", "OCaml");
          ("rust", "Rust");
          ("go", "Go");
          ("python", "Python");
        ]
      ~height:5 ()
  in
  let harness = Test_harness.create app in

  (* Press Enter to open dropdown *)
  Test_harness.push_key_event (Input.key Enter) harness;

  let output = Test_harness.view ~width:40 ~height:10 harness in
  print_test_output output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Select...                          ▼ │|
|└──────────────────────────────────────┘|
|┌──────────────────────────────────────┐|
|│                                      │|
|│ ▸ OCaml                              │|
|│   Rust                               │|
|│   Go                                 │|
|│   Python                             │|
|│                                      │|
+----------------------------------------+
|}]

let%expect_test "Navigate with arrow keys" =
  let app =
    App.create_app
      ~options:
        [
          ("ocaml", "OCaml");
          ("rust", "Rust");
          ("go", "Go");
          ("python", "Python");
        ]
      ()
  in
  let harness = Test_harness.create app in

  (* Open dropdown *)
  Test_harness.push_key_event (Input.key Enter) harness;

  (* Navigate down twice *)
  Test_harness.push_key_event (Input.key Down) harness;
  Test_harness.push_key_event (Input.key Down) harness;

  let output = Test_harness.view ~width:40 ~height:10 harness in
  print_test_output output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Select...                          ▼ │|
|└──────────────────────────────────────┘|
|┌──────────────────────────────────────┐|
|│                                      │|
|│   OCaml                              │|
|│   Rust                               │|
|│ ▸ Go                                 │|
|│   Python                             │|
|│                                      │|
+----------------------------------------+
|}]

let%expect_test "Select item and close dropdown" =
  let app =
    App.create_app
      ~options:
        [
          ("ocaml", "OCaml");
          ("rust", "Rust");
          ("go", "Go");
          ("python", "Python");
        ]
      ()
  in
  let harness = Test_harness.create app in

  (* Open dropdown *)
  Test_harness.push_key_event (Input.key Enter) harness;

  (* Navigate to Rust and select it *)
  Test_harness.push_key_event (Input.key Down) harness;
  Test_harness.push_key_event (Input.key Enter) harness;

  let output = Test_harness.view ~width:40 ~height:10 harness in
  print_test_output output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Rust                               ▶ │|
|└──────────────────────────────────────┘|
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
+----------------------------------------+
|}]

let%expect_test "Filterable select - typing to filter" =
  let app =
    App.create_app
      ~options:
        [
          ("ocaml", "OCaml");
          ("rust", "Rust");
          ("go", "Go");
          ("python", "Python");
          ("javascript", "JavaScript");
          ("typescript", "TypeScript");
        ]
      ~filterable:true ~height:4 ()
  in
  let harness = Test_harness.create app in

  (* Open dropdown *)
  Test_harness.push_key_event (Input.key Enter) harness;

  (* Type "script" to filter *)
  Test_harness.push_input "script" harness;

  let output = Test_harness.view ~width:40 ~height:8 harness in
  print_test_output ~height:8 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Select...                          ▼ │|
|└──────────────────────────────────────┘|
|┌──────────────────────────────────────┐|
|│                                      │|
|│  Filter: script                      │|
|│ ▸ JavaScript                         │|
|│   TypeScript                         │|
+----------------------------------------+
|}]

let%expect_test "Escape key closes dropdown" =
  let app =
    App.create_app
      ~options:[ ("ocaml", "OCaml"); ("rust", "Rust"); ("go", "Go") ]
      ()
  in
  let harness = Test_harness.create app in

  (* Open dropdown *)
  Test_harness.push_key_event (Input.key Enter) harness;

  (* Verify it's open *)
  let output1 = Test_harness.view ~width:40 ~height:6 harness in
  print_string "Dropdown open:\n";
  print_test_output ~height:6 output1;

  (* Press Escape to close *)
  Test_harness.push_key_event (Input.key Escape) harness;

  (* Verify it's closed *)
  let output2 = Test_harness.view ~width:40 ~height:6 harness in
  print_string "\nDropdown closed:\n";
  print_test_output ~height:6 output2;

  [%expect_exact
    {|Dropdown open:
+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Select...                          ▼ │|
|└──────────────────────────────────────┘|
|┌──────────────────────────────────────┐|
|│                                      │|
|│ ▸ OCaml                              │|
+----------------------------------------+

Dropdown closed:
+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Select...                          ▶ │|
|└──────────────────────────────────────┘|
|                                        |
|                                        |
|                                        |
+----------------------------------------+
|}]

let%expect_test "Page navigation in long list" =
  let options =
    List.init 20 (fun i ->
        let n = i + 1 in
        (string_of_int n, Printf.sprintf "Option %d" n))
  in
  let app = App.create_app ~options ~height:5 () in
  let harness = Test_harness.create app in

  (* Open dropdown *)
  Test_harness.push_key_event (Input.key Enter) harness;

  print_string "Initial view:\n";
  let output1 = Test_harness.view ~width:40 ~height:8 harness in
  print_test_output ~height:8 output1;

  (* Page down *)
  Test_harness.push_key_event (Input.key Page_down) harness;

  print_string "\nAfter Page Down:\n";
  let output2 = Test_harness.view ~width:40 ~height:8 harness in
  print_test_output ~height:8 output2;

  [%expect_exact
    {|Initial view:
+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Select...                          ▼ │|
|└──────────────────────────────────────┘|
|┌──────────────────────────────────────┐|
|│                                      │|
|│ ▸ Option 1                           │|
|│   Option 2                           │|
|│   Option 3                           │|
+----------------------------------------+

After Page Down:
+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Select...                          ▼ │|
|└──────────────────────────────────────┘|
|┌──────────────────────────────────────┐|
|│                                      │|
|│ ▸ Option 6                           │|
|│   Option 7                           │|
|│   Option 8                           │|
+----------------------------------------+
|}]

let%expect_test "Empty select behavior" =
  let app = App.create_app ~options:[] ~placeholder:"No options available" () in
  let harness = Test_harness.create app in

  (* Try to open dropdown *)
  Test_harness.push_key_event (Input.key Enter) harness;

  let output = Test_harness.view ~width:40 ~height:10 harness in
  print_test_output output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ No options available               ▼ │|
|└──────────────────────────────────────┘|
|┌──────────────────────────────────────┐|
|│                                      │|
|│                                      │|
|└──────────────────────────────────────┘|
|                                        |
|                                        |
|                                        |
+----------------------------------------+
|}]

let%expect_test "Home and End key navigation" =
  let app =
    App.create_app
      ~options:
        [
          ("first", "First");
          ("second", "Second");
          ("third", "Third");
          ("fourth", "Fourth");
          ("fifth", "Fifth");
        ]
      ()
  in
  let harness = Test_harness.create app in

  (* Open dropdown *)
  Test_harness.push_key_event (Input.key Enter) harness;

  (* Go to end *)
  Test_harness.push_key_event (Input.key End) harness;

  print_string "After End key:\n";
  let output1 = Test_harness.view ~width:40 ~height:8 harness in
  print_test_output ~height:8 output1;

  (* Go to home *)
  Test_harness.push_key_event (Input.key Home) harness;

  print_string "\nAfter Home key:\n";
  let output2 = Test_harness.view ~width:40 ~height:8 harness in
  print_test_output ~height:8 output2;

  [%expect_exact
    {|After End key:
+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Select...                          ▼ │|
|└──────────────────────────────────────┘|
|┌──────────────────────────────────────┐|
|│                                      │|
|│ ▸ First                              │|
|│   Second                             │|
|│   Third                              │|
+----------------------------------------+

After Home key:
+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Select...                          ▼ │|
|└──────────────────────────────────────┘|
|┌──────────────────────────────────────┐|
|│                                      │|
|│ ▸ First                              │|
|│   Second                             │|
|│   Third                              │|
+----------------------------------------+
|}]

let%expect_test "Tab key behavior" =
  let app =
    App.create_app ~options:[ ("ocaml", "OCaml"); ("rust", "Rust") ] ()
  in
  let harness = Test_harness.create app in

  (* Open dropdown *)
  Test_harness.push_key_event (Input.key Enter) harness;

  (* Navigate to Rust *)
  Test_harness.push_key_event (Input.key Down) harness;

  (* Press Tab to select and close *)
  Test_harness.push_key_event (Input.key Tab) harness;

  let output = Test_harness.view ~width:40 ~height:10 harness in
  print_test_output output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Rust                               ▶ │|
|└──────────────────────────────────────┘|
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
|                                        |
+----------------------------------------+
|}]

let%expect_test "Unicode and long labels" =
  let app =
    App.create_app
      ~options:
        [
          ("emoji", "🎉 Celebration Mode");
          ("chinese", "中文选项");
          ("long", "This is a very long option label that might get truncated");
          ("mixed", "Mixed 文字 and émojis 🌟");
        ]
      ~height:5 ()
  in
  let harness = Test_harness.create app in

  (* Open dropdown *)
  Test_harness.push_key_event (Input.key Enter) harness;

  let output = Test_harness.view ~width:40 ~height:8 harness in
  print_test_output ~height:8 output;
  [%expect_exact
    {|+----------------------------------------+
|┌──────────────────────────────────────┐|
|│ Select...                          ▼ │|
|└──────────────────────────────────────┘|
|┌──────────────────────────────────────┐|
|│                                      │|
|│ ▸ 🎉  Celebration Mode                │|
|│   中 文 选 项                            │|
|│   This is a very long option label t │|
+----------------------------------------+
|}]
