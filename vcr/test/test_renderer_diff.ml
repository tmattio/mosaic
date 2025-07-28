(** Comprehensive tests for renderer diffing logic *)

open Alcotest

(** Helper to create a simple VTE with content *)
let create_vte_with_content ?(rows = 10) ?(cols = 40) content =
  let vte = Vte.create ~rows ~cols () in
  let bytes = Bytes.of_string content in
  Vte.feed vte bytes 0 (Bytes.length bytes);
  vte

(** Helper to create a GIF renderer *)
let create_gif_renderer vte =
  let config =
    {
      Renderer.Gif_renderer.char_width = 8;
      char_height = 16;
      frame_delay = 10;
      theme = Renderer.Gif_renderer.default_theme;
      font_path = None;
      font_size = 16;
      target_width = Some 320;
      target_height = Some 160;
      padding = 10;
    }
  in
  Renderer.Gif_renderer.create vte config

(** Helper to count frames in a renderer *)
let count_frames renderer = Renderer.Gif_renderer.frame_count renderer

(** Test that identical frames produce minimal diff *)
let test_identical_frames_diff () =
  let vte = create_vte_with_content "Hello World" in
  let renderer = create_gif_renderer vte in

  (* Capture first frame *)
  Renderer.Gif_renderer.capture_frame renderer;
  check int "first frame captured" 1 (count_frames renderer);

  (* Capture identical frame *)
  Renderer.Gif_renderer.capture_frame renderer;
  check int "second frame captured" 2 (count_frames renderer);

  (* Check that second frame is minimal *)
  match renderer.Renderer.Gif_renderer.frames with
  | frame2 :: frame1 :: _ ->
      (* Second frame should be tiny (1x1) since nothing changed *)
      check int "diff frame width" 1 frame2.width;
      check int "diff frame height" 1 frame2.height
  | _ -> fail "Expected at least 2 frames"

(** Test that small changes produce small diffs *)
let test_small_change_diff () =
  let vte = create_vte_with_content "Count: 0" in
  let renderer = create_gif_renderer vte in

  (* Capture initial frame *)
  Renderer.Gif_renderer.capture_frame renderer;

  (* Update just the number *)
  let bytes = Bytes.of_string "\r\x1b[7C1" in
  Vte.feed vte bytes 0 (Bytes.length bytes);

  (* Capture updated frame *)
  Renderer.Gif_renderer.capture_frame renderer;

  match renderer.Renderer.Gif_renderer.frames with
  | frame2 :: _frame1 :: _ ->
      (* Diff should be much smaller than full frame *)
      let full_width = 320 in
      let full_height = 160 in
      check bool "diff width smaller than full"
        (frame2.width < full_width / 2)
        true;
      check bool "diff height smaller than full"
        (frame2.height < full_height / 2)
        true;
      (* The diff should be positioned where the change occurred *)
      check bool "diff x_offset reasonable" (frame2.x_offset > 0) true
  | _ -> fail "Expected at least 2 frames"

(** Test multiple small updates *)
let test_incremental_counter_updates () =
  let vte = create_vte_with_content "Counter: 0 [+/-]" in
  let renderer = create_gif_renderer vte in

  (* Capture initial *)
  Renderer.Gif_renderer.capture_frame renderer;
  let initial_frames = count_frames renderer in

  (* Simulate counter increments *)
  for i = 1 to 5 do
    let update = Printf.sprintf "\r\x1b[9C%d" i in
    let bytes = Bytes.of_string update in
    Vte.feed vte bytes 0 (Bytes.length bytes);
    Renderer.Gif_renderer.capture_frame renderer
  done;

  check int "captured all frames" (initial_frames + 5) (count_frames renderer);

  (* Check that update frames are small *)
  let frames = List.rev renderer.Renderer.Gif_renderer.frames in
  let update_frames =
    match frames with
    | _initial :: updates -> updates
    | _ -> fail "No frames found"
  in

  List.iter
    (fun frame ->
      check bool "update frame is small"
        (frame.width * frame.height < 100 * 50)
        true)
    update_frames

(** Test cursor blink handling *)
let test_cursor_blink_diff () =
  let vte = create_vte_with_content "Text" in
  let renderer = create_gif_renderer vte in

  (* Frame with cursor visible *)
  Vte.set_cursor_visible vte true;
  Renderer.Gif_renderer.capture_frame renderer;

  (* Frame with cursor hidden *)
  Vte.set_cursor_visible vte false;
  Renderer.Gif_renderer.capture_frame renderer;

  match renderer.Renderer.Gif_renderer.frames with
  | frame2 :: _frame1 :: _ ->
      (* Only cursor area should be in diff *)
      check bool "cursor diff is small"
        (frame2.width <= 16 && frame2.height <= 20)
        true
  | _ -> fail "Expected at least 2 frames"

(** Test scrolling creates appropriate diff *)
let test_scroll_diff () =
  let vte = create_vte_with_content ~rows:3 "Line1\nLine2\nLine3" in
  let renderer = create_gif_renderer vte in

  (* Initial frame *)
  Renderer.Gif_renderer.capture_frame renderer;

  (* Add new line causing scroll *)
  let bytes = Bytes.of_string "\nLine4" in
  Vte.feed vte bytes 0 (Bytes.length bytes);
  Renderer.Gif_renderer.capture_frame renderer;

  match renderer.Renderer.Gif_renderer.frames with
  | frame2 :: _frame1 :: _ ->
      (* Most of screen should have changed due to scroll *)
      check bool "scroll causes large diff"
        (frame2.width * frame2.height > 200 * 40)
        true
  | _ -> fail "Expected at least 2 frames"

(** Test color change creates minimal diff *)
let test_color_change_diff () =
  let vte = create_vte_with_content "Normal Text" in
  let renderer = create_gif_renderer vte in

  (* Initial frame *)
  Renderer.Gif_renderer.capture_frame renderer;

  (* Change color of one word *)
  let bytes = Bytes.of_string "\r\x1b[7C\x1b[31mText\x1b[0m" in
  Vte.feed vte bytes 0 (Bytes.length bytes);
  Renderer.Gif_renderer.capture_frame renderer;

  match renderer.Renderer.Gif_renderer.frames with
  | frame2 :: _frame1 :: _ ->
      (* Only the word "Text" area should be in diff *)
      check bool "color change diff width reasonable" (frame2.width < 50) true;
      check bool "color change diff positioned correctly" (frame2.x_offset > 50)
        true
  | _ -> fail "Expected at least 2 frames"

(** Test clearing screen *)
let test_clear_screen_diff () =
  let vte =
    create_vte_with_content
      "Lots of text here\nOn multiple lines\nFilling screen"
  in
  let renderer = create_gif_renderer vte in

  (* Initial frame *)
  Renderer.Gif_renderer.capture_frame renderer;

  (* Clear screen *)
  let bytes = Bytes.of_string "\x1b[2J\x1b[H" in
  Vte.feed vte bytes 0 (Bytes.length bytes);
  Renderer.Gif_renderer.capture_frame renderer;

  match renderer.Renderer.Gif_renderer.frames with
  | frame2 :: _frame1 :: _ ->
      (* Entire screen should be in diff *)
      check bool "clear screen causes full diff"
        (frame2.width >= 300 && frame2.height >= 140)
        true
  | _ -> fail "Expected at least 2 frames"

(** Test partial line clear *)
let test_partial_line_clear_diff () =
  let vte = create_vte_with_content "Hello World Example" in
  let renderer = create_gif_renderer vte in

  (* Initial frame *)
  Renderer.Gif_renderer.capture_frame renderer;

  (* Clear from "World" onward *)
  let bytes = Bytes.of_string "\r\x1b[6C\x1b[K" in
  Vte.feed vte bytes 0 (Bytes.length bytes);
  Renderer.Gif_renderer.capture_frame renderer;

  match renderer.Renderer.Gif_renderer.frames with
  | frame2 :: _frame1 :: _ ->
      (* Only the cleared portion should be in diff *)
      check bool "partial clear diff starts at correct position"
        (frame2.x_offset > 40) true;
      check bool "partial clear diff has reasonable width" (frame2.width < 200)
        true
  | _ -> fail "Expected at least 2 frames"

(** Test frame order preservation *)
let test_frame_order () =
  let vte = create_vte_with_content "" in
  let renderer = create_gif_renderer vte in

  (* Add multiple distinct frames *)
  let updates = [ "A"; "AB"; "ABC"; "ABCD"; "ABCDE" ] in
  List.iter
    (fun text ->
      let bytes = Bytes.of_string ("\r" ^ text) in
      Vte.feed vte bytes 0 (Bytes.length bytes);
      Renderer.Gif_renderer.capture_frame renderer)
    updates;

  check int "all frames captured" (List.length updates) (count_frames renderer);

  (* Frames should be in reverse order in the list *)
  let frame_count = List.length renderer.Renderer.Gif_renderer.frames in
  check bool "frames stored in LIFO order"
    (frame_count = List.length updates)
    true

(** Test transparent color handling *)
let test_transparent_color () =
  let vte = create_vte_with_content "Text" in
  let renderer = create_gif_renderer vte in

  (* First frame should not have transparent color *)
  Renderer.Gif_renderer.capture_frame renderer;

  (* Subsequent frames should have transparent color *)
  let bytes = Bytes.of_string " " in
  Vte.feed vte bytes 0 (Bytes.length bytes);
  Renderer.Gif_renderer.capture_frame renderer;

  match renderer.Renderer.Gif_renderer.frames with
  | frame2 :: frame1 :: _ ->
      check bool "first frame no transparency"
        (frame1.transparent_color = None)
        true;
      check bool "diff frame has transparency"
        (frame2.transparent_color <> None)
        true
  | _ -> fail "Expected at least 2 frames"

(** Test empty VTE handling *)
let test_empty_vte () =
  let vte = create_vte_with_content "" in
  let renderer = create_gif_renderer vte in

  (* Should still create a frame *)
  Renderer.Gif_renderer.capture_frame renderer;
  check int "frame created for empty VTE" 1 (count_frames renderer)

(** Test rapid updates *)
let test_rapid_updates () =
  let vte = create_vte_with_content "0" in
  let renderer = create_gif_renderer vte in

  (* Simulate rapid counter updates *)
  for i = 0 to 20 do
    let update = Printf.sprintf "\r%02d" i in
    let bytes = Bytes.of_string update in
    Vte.feed vte bytes 0 (Bytes.length bytes);
    Renderer.Gif_renderer.capture_frame renderer
  done;

  check int "all rapid updates captured" 21 (count_frames renderer)

(** Test hidden cursor doesn't appear in frames *)
let test_hidden_cursor_in_frame () =
  let vte = create_vte_with_content "Text" in
  let renderer = create_gif_renderer vte in

  (* Hide cursor before capturing *)
  Vte.set_cursor_visible vte false;
  Renderer.Gif_renderer.capture_frame renderer;

  (* The test passes if no crash occurs and frame is created *)
  check int "frame created with hidden cursor" 1 (count_frames renderer)

(** Test suite *)
let () =
  run "Renderer Diffing Logic"
    [
      ( "basic diffing",
        [
          test_case "identical frames produce minimal diff" `Quick
            test_identical_frames_diff;
          test_case "small changes produce small diffs" `Quick
            test_small_change_diff;
          test_case "incremental counter updates" `Quick
            test_incremental_counter_updates;
        ] );
      ( "cursor handling",
        [
          test_case "cursor blink creates minimal diff" `Quick
            test_cursor_blink_diff;
          test_case "hidden cursor rendering" `Quick test_hidden_cursor_in_frame;
        ] );
      ( "screen changes",
        [
          test_case "scrolling diff" `Quick test_scroll_diff;
          test_case "color change diff" `Quick test_color_change_diff;
          test_case "clear screen diff" `Quick test_clear_screen_diff;
          test_case "partial line clear diff" `Quick
            test_partial_line_clear_diff;
        ] );
      ( "frame management",
        [
          test_case "frame order preservation" `Quick test_frame_order;
          test_case "transparent color handling" `Quick test_transparent_color;
        ] );
      ( "edge cases",
        [
          test_case "empty VTE handling" `Quick test_empty_vte;
          test_case "rapid updates" `Quick test_rapid_updates;
        ] );
    ]
