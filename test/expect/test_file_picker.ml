open Mosaic
open Test_utils

(** Test app that wraps a File_picker component *)
module App = struct
  open Mosaic_tiles

  type model = { file_picker : File_picker.model; quit : bool }
  type msg = FilePicker_msg of File_picker.msg | Quit

  let create_app ?start_path ?show_hidden ?directories_only ?extensions ?height
      () =
    let init () =
      let file_picker_model, file_picker_cmd =
        File_picker.init ?start_path ?show_hidden ?directories_only ?extensions
          ?height ()
      in
      (* Focus the file picker by default so keyboard events work *)
      let focused_model, focus_cmd = File_picker.focus file_picker_model in
      ( { file_picker = focused_model; quit = false },
        Cmd.batch
          [
            Cmd.map (fun m -> FilePicker_msg m) file_picker_cmd;
            Cmd.map (fun m -> FilePicker_msg m) focus_cmd;
          ] )
    in
    let update msg model =
      match msg with
      | FilePicker_msg file_picker_msg ->
          let new_file_picker, file_picker_cmd =
            File_picker.update file_picker_msg model.file_picker
          in
          ( { model with file_picker = new_file_picker },
            Cmd.map (fun m -> FilePicker_msg m) file_picker_cmd )
      | Quit -> ({ model with quit = true }, Cmd.quit)
    in
    let view model = File_picker.view model.file_picker in
    let subscriptions model =
      Sub.batch
        [
          Sub.map
            (fun m -> FilePicker_msg m)
            (File_picker.subscriptions model.file_picker);
          Sub.on_key ~ctrl:true (Char (Uchar.of_char 'c')) Quit;
        ]
    in
    Mosaic.app ~init ~update ~view ~subscriptions ()
end

(** Helper to print test output with borders *)
let print_test_output ?(width = 60) ?height:(_ = 10) output =
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

let%expect_test "File picker with nonexistent directory" =
  let app = App.create_app ~start_path:"/nonexistent_directory_xyz123" () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:60 ~height:12 harness in
  print_test_output ~height:12 output;
  (* This shows the bug - displays empty directory instead of error *)
  [%expect_exact
    {|+------------------------------------------------------------+
|Path: /nonexistent_directory_xyz123                         |
|┌──────────────────────────────────────────────────────────┐|
|│                                                          │|
|│                                                          │|
|│                                                          │|
|│                                                          │|
|│                                                          │|
|│                                                          │|
|│                                                          │|
|│                                                          │|
|│                                                          │|
|│                                                          │|
+------------------------------------------------------------+
|}]

let%expect_test "File picker with current directory" =
  let app = App.create_app ~start_path:"." ~height:5 () in
  let harness = Test_harness.create app in
  let output = Test_harness.view ~width:60 ~height:10 harness in
  print_string "File picker shows current directory contents:\n";
  print_test_output ~height:10 output;
  (* Should show actual files in current directory *)
  [%expect
    {|
    File picker shows current directory contents:
    +------------------------------------------------------------+
    |Path: .                                                     |
    |┌──────────────────────────────────────────────────────────┐|
    |│                                                          │|
    |│                                                          │|
    |│                                                          │|
    |│                                                          │|
    |│                                                          │|
    |└──────────────────────────────────────────────────────────┘|
    |                                                            |
    |                                                            |
    +------------------------------------------------------------+
    |}]

let%expect_test "Navigate with arrow keys" =
  let app = App.create_app ~start_path:"." ~height:5 () in
  let harness = Test_harness.create app in

  (* Navigate down twice *)
  Test_harness.push_key_event (Input.key Down) harness;
  Test_harness.push_key_event (Input.key Down) harness;

  let output = Test_harness.view ~width:60 ~height:10 harness in
  print_string "After navigating down:\n";
  print_test_output ~height:10 output;
  [%expect
    {|
    After navigating down:
    +------------------------------------------------------------+
    |Path: .                                                     |
    |┌──────────────────────────────────────────────────────────┐|
    |│                                                          │|
    |│                                                          │|
    |│                                                          │|
    |│                                                          │|
    |│                                                          │|
    |└──────────────────────────────────────────────────────────┘|
    |                                                            |
    |                                                            |
    +------------------------------------------------------------+
    |}]

let%expect_test "Toggle hidden files" =
  let app = App.create_app ~start_path:"." ~height:5 () in
  let harness = Test_harness.create app in

  (* Toggle hidden files with 'h' *)
  Test_harness.push_key_event (Input.key (Char (Uchar.of_char 'h'))) harness;

  let output = Test_harness.view ~width:60 ~height:10 harness in
  print_string "After toggling hidden files:\n";
  print_test_output ~height:10 output;
  [%expect
    {|
    After toggling hidden files:
    +------------------------------------------------------------+
    |Path: .                                                     |
    |┌──────────────────────────────────────────────────────────┐|
    |│                                                          │|
    |│                                                          │|
    |│                                                          │|
    |│                                                          │|
    |│                                                          │|
    |└──────────────────────────────────────────────────────────┘|
    |                                                            |
    |                                                            |
    +------------------------------------------------------------+
    |}]

let%expect_test "Permission denied directory" =
  (* Create a directory with no read permissions *)
  let temp_dir =
    Filename.temp_dir ~temp_dir:(Sys.getcwd ()) "test_file_picker" ""
  in
  Unix.chmod temp_dir 0o000;
  Fun.protect
    ~finally:(fun () ->
      Unix.chmod temp_dir 0o755;
      Unix.rmdir temp_dir)
    (fun () ->
      let app = App.create_app ~start_path:temp_dir () in
      let harness = Test_harness.create app in
      let output = Test_harness.view ~width:60 ~height:12 harness in
      print_string "Permission denied directory shows empty (bug):\n";
      print_test_output ~height:12 output;
      (* This shows the bug - empty directory instead of error *)
      [%expect
        {|
      Permission denied directory shows empty (bug):
      +------------------------------------------------------------+
      |Path: /Users/tmattio/Workspace/mosaic/_build/.sandbox/9d671b|
      |┌──────────────────────────────────────────────────────────┐|
      |│                                                          │|
      |│                                                          │|
      |│                                                          │|
      |│                                                          │|
      |│                                                          │|
      |│                                                          │|
      |│                                                          │|
      |│                                                          │|
      |│                                                          │|
      |│                                                          │|
      +------------------------------------------------------------+
      |}])
