(** Integration tests for GIF renderer *)

open Renderer

(** Test basic GIF rendering *)
let test_basic_rendering () =
  let vte = Vte.create ~rows:5 ~cols:10 () in

  (* Feed some text *)
  let text = "Hello\nWorld!\n" in
  Vte.feed vte (Bytes.of_string text) 0 (String.length text);

  let config =
    {
      Gif_renderer.char_width = 6;
      char_height = 8;
      frame_delay = 10;
      theme = Gif_renderer.default_theme;
      font_path = None;
      font_size = 12;
      target_width = None;
      target_height = None;
      padding = 0;
    }
  in

  let renderer = Gif_renderer.create vte config in

  (* Capture a frame *)
  Gif_renderer.capture_frame renderer;

  (* Render to get GIF data *)
  let gif_data = Gif_renderer.render renderer in

  (* Verify data is non-empty *)
  Alcotest.(check bool) "GIF data created" true (String.length gif_data > 0);

  (* Optionally save to temporary file for inspection *)
  let temp_file = Filename.temp_file "test_gif_" ".gif" in
  let oc = open_out_bin temp_file in
  output_string oc gif_data;
  close_out oc;

  (* Verify it's a valid GIF *)
  let ic = open_in_bin temp_file in
  let header = really_input_string ic 6 in
  close_in ic;
  Alcotest.(check string) "GIF header" "GIF89a" header;

  (* Clean up *)
  Unix.unlink temp_file

(** Test ANSI color rendering *)
let test_ansi_colors () =
  let vte = Vte.create ~rows:8 ~cols:20 () in

  (* Feed ANSI color sequences *)
  let sequences =
    [
      "\027[31mRed\027[0m\n";
      "\027[32mGreen\027[0m\n";
      "\027[34mBlue\027[0m\n";
      "\027[33mYellow\027[0m\n";
      "\027[35mMagenta\027[0m\n";
      "\027[36mCyan\027[0m\n";
      "\027[37mWhite\027[0m\n";
      "\027[30mBlack\027[0m\n";
    ]
  in

  List.iter
    (fun seq -> Vte.feed vte (Bytes.of_string seq) 0 (String.length seq))
    sequences;

  let config =
    {
      Gif_renderer.char_width = 6;
      char_height = 8;
      frame_delay = 10;
      theme = Gif_renderer.default_theme;
      font_path = None;
      font_size = 12;
      target_width = None;
      target_height = None;
      padding = 0;
    }
  in

  let renderer = Gif_renderer.create vte config in
  Gif_renderer.capture_frame renderer;

  (* Render to get GIF data *)
  let gif_data = Gif_renderer.render renderer in

  (* Verify data is non-empty *)
  Alcotest.(check bool)
    "Color GIF data created" true
    (String.length gif_data > 0);

  (* Save to temp file for verification *)
  let temp_file = Filename.temp_file "test_colors_" ".gif" in
  let oc = open_out_bin temp_file in
  output_string oc gif_data;
  close_out oc;

  (* Verify file was created *)
  Alcotest.(check bool)
    "Color GIF file created" true
    (Sys.file_exists temp_file);

  Unix.unlink temp_file

(** Test cursor rendering *)
let test_cursor_rendering () =
  let vte = Vte.create ~rows:3 ~cols:10 () in

  (* Position cursor and explicitly set initial cursor state *)
  Vte.feed vte (Bytes.of_string "Hello") 0 5;
  Vte.set_cursor_visible vte true;

  let config =
    {
      Gif_renderer.char_width = 6;
      char_height = 8;
      frame_delay = 10;
      theme = Gif_renderer.default_theme;
      font_path = None;
      font_size = 12;
      target_width = None;
      target_height = None;
      padding = 0;
    }
  in

  let renderer = Gif_renderer.create vte config in

  (* Capture with cursor visible *)
  Gif_renderer.capture_frame renderer;

  (* Hide cursor and capture again *)
  Vte.set_cursor_visible vte false;
  Gif_renderer.capture_frame renderer;

  (* Show cursor and capture - add some text to make frame different *)
  Vte.feed vte (Bytes.of_string "!") 0 1;
  Vte.set_cursor_visible vte true;
  Gif_renderer.capture_frame renderer;

  let temp_file = Filename.temp_file "test_cursor_" ".gif" in
  let gif_data = Gif_renderer.render renderer in
  let oc = open_out_bin temp_file in
  output_string oc gif_data;
  close_out oc;

  (* Verify we have 3 frames using the decoder *)
  let ic = open_in_bin temp_file in
  let data = really_input_string ic (in_channel_length ic) in
  close_in ic;

  (* Use the GIF decoder to count frames properly *)
  let frame_count =
    match Gif.decode_frames data with
    | Ok frames -> List.length frames
    | Error _ -> failwith "Failed to decode GIF"
  in

  Alcotest.(check int) "Three frames captured" 3 frame_count;

  Unix.unlink temp_file

(** Test 256-color support *)
let test_256_colors () =
  let vte = Vte.create ~rows:16 ~cols:16 () in

  (* Generate 256-color palette test *)
  for i = 0 to 15 do
    for j = 0 to 15 do
      let color = (i * 16) + j in
      let seq = Printf.sprintf "\027[38;5;%dm#" color in
      Vte.feed vte (Bytes.of_string seq) 0 (String.length seq)
    done;
    Vte.feed vte (Bytes.of_string "\n") 0 1
  done;

  let config =
    {
      Gif_renderer.char_width = 6;
      char_height = 8;
      frame_delay = 10;
      theme = Gif_renderer.default_theme;
      font_path = None;
      font_size = 12;
      target_width = None;
      target_height = None;
      padding = 0;
    }
  in

  let renderer = Gif_renderer.create vte config in
  Gif_renderer.capture_frame renderer;

  let temp_file = Filename.temp_file "test_256colors_" ".gif" in
  let gif_data = Gif_renderer.render renderer in
  let oc = open_out_bin temp_file in
  output_string oc gif_data;
  close_out oc;

  (* Check that file is reasonably large (has color data) *)
  let stats = Unix.stat temp_file in
  Alcotest.(check bool) "256-color GIF has content" true (stats.st_size > 1000);

  Unix.unlink temp_file

(** Test dimensions *)
let test_output_dimensions () =
  let test_dims rows cols =
    let vte = Vte.create ~rows ~cols () in

    let char_width = 8 in
    let char_height = 12 in

    let config =
      {
        Gif_renderer.char_width;
        char_height;
        frame_delay = 10;
        theme = Gif_renderer.default_theme;
        font_path = None;
        font_size = 12;
        target_width = None;
        target_height = None;
        padding = 0;
      }
    in

    let renderer = Gif_renderer.create vte config in
    Gif_renderer.capture_frame renderer;

    let temp_file = Filename.temp_file "test_dims_" ".gif" in
    let gif_data = Gif_renderer.render renderer in
    let oc = open_out_bin temp_file in
    output_string oc gif_data;
    close_out oc;

    (* Read and parse GIF dimensions *)
    let ic = open_in_bin temp_file in
    let _ = really_input_string ic 6 in
    (* Skip header *)
    let width_bytes = really_input_string ic 2 in
    let height_bytes = really_input_string ic 2 in
    close_in ic;

    let width =
      Char.code width_bytes.[0] lor (Char.code width_bytes.[1] lsl 8)
    in
    let height =
      Char.code height_bytes.[0] lor (Char.code height_bytes.[1] lsl 8)
    in

    let expected_width = cols * char_width in
    let expected_height = rows * char_height in

    Alcotest.(check int)
      (Printf.sprintf "%dx%d terminal width" rows cols)
      expected_width width;
    Alcotest.(check int)
      (Printf.sprintf "%dx%d terminal height" rows cols)
      expected_height height;

    Unix.unlink temp_file
  in

  test_dims 24 80;
  (* Standard terminal *)
  test_dims 10 40;
  (* Small terminal *)
  test_dims 50 120 (* Large terminal *)

(** Test animation with multiple frames *)
let test_animation () =
  let vte = Vte.create ~rows:5 ~cols:20 () in

  let config =
    {
      Gif_renderer.char_width = 6;
      char_height = 8;
      frame_delay = 50;
      (* 0.5 seconds *)
      theme = Gif_renderer.default_theme;
      font_path = None;
      font_size = 12;
      target_width = None;
      target_height = None;
      padding = 0;
    }
  in

  let renderer = Gif_renderer.create vte config in

  (* Animate a simple counter *)
  for i = 0 to 9 do
    (* Clear and write number *)
    Vte.feed vte (Bytes.of_string "\027[2J\027[H") 0 7;
    (* Clear screen, home *)
    let text = Printf.sprintf "Frame %d" i in
    Vte.feed vte (Bytes.of_string text) 0 (String.length text);

    Gif_renderer.capture_frame renderer
  done;

  let temp_file = Filename.temp_file "test_animation_" ".gif" in
  let gif_data = Gif_renderer.render renderer in
  let oc = open_out_bin temp_file in
  output_string oc gif_data;
  close_out oc;

  (* Verify we have 10 frames *)
  let ic = open_in_bin temp_file in
  let data = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let frame_count = ref 0 in
  for i = 0 to String.length data - 1 do
    if data.[i] = '\x2C' then incr frame_count
  done;

  Alcotest.(check int) "Ten frames in animation" 10 !frame_count;

  Unix.unlink temp_file

(** Test special characters and edge cases *)
let test_special_characters () =
  let vte = Vte.create ~rows:5 ~cols:30 () in

  (* Test various special cases *)
  let test_strings =
    [
      "Normal text";
      "Spaces     between";
      "\tTab character";
      "";
      (* Empty *)
      String.make 30 'X';
      (* Full line *)
    ]
  in

  List.iter
    (fun s -> Vte.feed vte (Bytes.of_string (s ^ "\n")) 0 (String.length s + 1))
    test_strings;

  let config =
    {
      Gif_renderer.char_width = 6;
      char_height = 8;
      frame_delay = 10;
      theme = Gif_renderer.default_theme;
      font_path = None;
      font_size = 12;
      target_width = None;
      target_height = None;
      padding = 0;
    }
  in

  let renderer = Gif_renderer.create vte config in
  Gif_renderer.capture_frame renderer;

  let temp_file = Filename.temp_file "test_special_" ".gif" in
  let gif_data = Gif_renderer.render renderer in
  let oc = open_out_bin temp_file in
  output_string oc gif_data;
  close_out oc;

  (* Just verify it doesn't crash and produces output *)
  Alcotest.(check bool)
    "Special characters GIF created" true
    (Sys.file_exists temp_file);

  Unix.unlink temp_file

(** Main test suite *)
let () =
  let open Alcotest in
  run "GIF renderer integration tests"
    [
      ( "basic",
        [
          test_case "Basic rendering" `Quick test_basic_rendering;
          test_case "Output dimensions" `Quick test_output_dimensions;
        ] );
      ( "colors",
        [
          test_case "ANSI colors" `Quick test_ansi_colors;
          test_case "256 colors" `Quick test_256_colors;
        ] );
      ( "features",
        [
          test_case "Cursor rendering" `Quick test_cursor_rendering;
          test_case "Animation frames" `Quick test_animation;
          test_case "Special characters" `Quick test_special_characters;
        ] );
    ]
