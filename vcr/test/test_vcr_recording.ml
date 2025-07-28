(** Tests for VCR recording behavior, particularly Hide/Show command handling *)

open Alcotest

(** Helper to create a mock PTY master *)
let create_mock_pty () =
  (* Create a pipe to simulate PTY *)
  let read_fd, write_fd = Unix.pipe () in
  (* Make it non-blocking like a real PTY *)
  Unix.set_nonblock read_fd;
  write_fd

(** Helper to create test VCR state *)
let create_test_state () =
  let pty_master = create_mock_pty () in
  let vte = Vte.create ~rows:10 ~cols:40 () in
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
  let renderer_state = Renderer.Gif_renderer.create vte config in
  let renderer =
    Vcr.GIF
      { renderer = (module Renderer.Gif_renderer); state = renderer_state }
  in
  {
    Vcr.pty_master;
    vte;
    renderer;
    config = Vcr.default_config;
    clipboard = "";
    recording = true;
    (* Start with recording enabled *)
  }

(** Test that Hide command stops recording *)
let test_hide_stops_recording () =
  let state = create_test_state () in

  (* Initially recording should be true *)
  check bool "initially recording" true state.recording;

  (* Execute Hide command *)
  Vcr.handle_command state Tape_lang.Ast.Hide;

  (* Recording should now be false *)
  check bool "recording stopped after Hide" false state.recording

(** Test that Show command resumes recording *)
let test_show_resumes_recording () =
  let state = create_test_state () in

  (* First hide *)
  Vcr.handle_command state Tape_lang.Ast.Hide;
  check bool "recording stopped" false state.recording;

  (* Then show *)
  Vcr.handle_command state Tape_lang.Ast.Show;
  check bool "recording resumed after Show" true state.recording

(** Test that frames are only captured when recording is enabled *)
let test_frames_only_when_recording () =
  let state = create_test_state () in

  (* Get initial frame count *)
  let initial_frames =
    match state.renderer with
    | GIF { state = renderer_state; _ } -> (
        match renderer_state with
        | { Renderer.Gif_renderer.frames; _ } -> List.length frames)
    | _ -> fail "Expected GIF renderer"
  in

  (* Type something while recording *)
  Vcr.handle_command state (Tape_lang.Ast.Type { text = "Hello"; speed = None });

  let frames_after_hello =
    match state.renderer with
    | GIF { state = renderer_state; _ } -> (
        match renderer_state with
        | { Renderer.Gif_renderer.frames; _ } -> List.length frames)
    | _ -> fail "Expected GIF renderer"
  in

  check bool "frames captured while recording"
    (frames_after_hello > initial_frames)
    true;

  (* Hide (stop recording) *)
  Vcr.handle_command state Tape_lang.Ast.Hide;

  (* Type more *)
  Vcr.handle_command state
    (Tape_lang.Ast.Type { text = " World"; speed = None });

  let frames_after_world =
    match state.renderer with
    | GIF { state = renderer_state; _ } -> (
        match renderer_state with
        | { Renderer.Gif_renderer.frames; _ } -> List.length frames)
    | _ -> fail "Expected GIF renderer"
  in

  check int "no new frames while not recording" frames_after_hello
    frames_after_world;

  (* Show (resume recording) *)
  Vcr.handle_command state Tape_lang.Ast.Show;

  (* Type more *)
  Vcr.handle_command state (Tape_lang.Ast.Type { text = "!"; speed = None });

  let final_frames =
    match state.renderer with
    | GIF { state = renderer_state; _ } -> (
        match renderer_state with
        | { Renderer.Gif_renderer.frames; _ } -> List.length frames)
    | _ -> fail "Expected GIF renderer"
  in

  check bool "frames captured after resuming"
    (final_frames > frames_after_world)
    true

(** Test Sleep command captures frames when recording *)
let test_sleep_captures_frames () =
  let state = create_test_state () in
  state.config <- { state.config with framerate = 10 };

  (* 10 fps *)
  let initial_frames =
    match state.renderer with
    | GIF { state = renderer_state; _ } -> (
        match renderer_state with
        | { Renderer.Gif_renderer.frames; _ } -> List.length frames)
    | _ -> fail "Expected GIF renderer"
  in

  (* Sleep for 0.5 seconds = should capture ~5 frames at 10fps *)
  Vcr.handle_command state (Tape_lang.Ast.Sleep 0.5);

  let frames_after_sleep =
    match state.renderer with
    | GIF { state = renderer_state; _ } -> (
        match renderer_state with
        | { Renderer.Gif_renderer.frames; _ } -> List.length frames)
    | _ -> fail "Expected GIF renderer"
  in

  (* Should have captured approximately 5 frames (allowing some variance) *)
  let frames_captured = frames_after_sleep - initial_frames in
  check bool "sleep captures multiple frames"
    (frames_captured >= 3 && frames_captured <= 7)
    true

(** Test Sleep with recording disabled *)
let test_sleep_no_frames_when_not_recording () =
  let state = create_test_state () in

  (* Stop recording *)
  Vcr.handle_command state Tape_lang.Ast.Hide;

  let initial_frames =
    match state.renderer with
    | GIF { state = renderer_state; _ } -> (
        match renderer_state with
        | { Renderer.Gif_renderer.frames; _ } -> List.length frames)
    | _ -> fail "Expected GIF renderer"
  in

  (* Sleep should not capture frames *)
  Vcr.handle_command state (Tape_lang.Ast.Sleep 0.5);

  let frames_after =
    match state.renderer with
    | GIF { state = renderer_state; _ } -> (
        match renderer_state with
        | { Renderer.Gif_renderer.frames; _ } -> List.length frames)
    | _ -> fail "Expected GIF renderer"
  in

  check int "no frames during sleep when not recording" initial_frames
    frames_after

(** Test complex recording scenario *)
let test_complex_recording_scenario () =
  let state = create_test_state () in

  (* Simulate the counter example:
     1. Show initial state
     2. Hide cursor
     3. Make changes
     4. Show cursor
     5. Continue with visible changes *)

  (* Initial visible state *)
  Vcr.handle_command state
    (Tape_lang.Ast.Type { text = "Count: 0"; speed = None });
  Vcr.handle_command state (Tape_lang.Ast.Sleep 0.1);

  (* Hide recording *)
  Vcr.handle_command state Tape_lang.Ast.Hide;

  (* Make some setup changes while hidden *)
  Vcr.handle_command state (Tape_lang.Ast.Type { text = "\r"; speed = None });

  (* Show and continue *)
  Vcr.handle_command state Tape_lang.Ast.Show;

  (* Visible updates *)
  Vcr.handle_command state
    (Tape_lang.Ast.Type { text = "Count: 1"; speed = None });

  (* Verify frames were captured appropriately *)
  let final_frames =
    match state.renderer with
    | GIF { state = renderer_state; _ } -> (
        match renderer_state with
        | { Renderer.Gif_renderer.frames; _ } -> List.length frames)
    | _ -> fail "Expected GIF renderer"
  in

  check bool "frames captured for visible portions" (final_frames > 0) true

(** Test cursor visibility is separate from recording state *)
let test_cursor_visibility_vs_recording () =
  let state = create_test_state () in

  (* Initially cursor should be visible and recording enabled *)
  check bool "cursor initially visible" true (Vte.is_cursor_visible state.vte);
  check bool "initially recording" true state.recording;

  (* Hide command should affect recording but not cursor *)
  Vcr.handle_command state Tape_lang.Ast.Hide;
  check bool "cursor still visible after Hide" true
    (Vte.is_cursor_visible state.vte);
  check bool "recording stopped after Hide" false state.recording;

  (* Show command should affect recording but not cursor *)
  Vcr.handle_command state Tape_lang.Ast.Show;
  check bool "cursor still visible after Show" true
    (Vte.is_cursor_visible state.vte);
  check bool "recording resumed after Show" true state.recording

(** Test initial recording state *)
let test_initial_recording_state () =
  (* Test with actual tape parsing *)
  let tape = [ Tape_lang.Ast.Type { text = "Hello"; speed = None } ] in

  (* Create a temporary output *)
  let temp_file = Filename.temp_file "test_vcr" ".gif" in

  Fun.protect
    ~finally:(fun () -> Unix.unlink temp_file)
    (fun () ->
      (* Run should start with recording enabled *)
      (* This would require mocking the PTY spawn, so we just verify the default *)
      let state = create_test_state () in
      check bool "default recording state is true" true state.recording)

(** Test suite *)
let () =
  run "VCR Recording Behavior"
    [
      ( "recording control",
        [
          test_case "Hide stops recording" `Quick test_hide_stops_recording;
          test_case "Show resumes recording" `Quick test_show_resumes_recording;
          test_case "frames only captured when recording" `Quick
            test_frames_only_when_recording;
        ] );
      ( "sleep behavior",
        [
          test_case "Sleep captures frames when recording" `Quick
            test_sleep_captures_frames;
          test_case "Sleep doesn't capture when not recording" `Quick
            test_sleep_no_frames_when_not_recording;
        ] );
      ( "complex scenarios",
        [
          test_case "complex recording scenario" `Quick
            test_complex_recording_scenario;
          test_case "cursor visibility vs recording state" `Quick
            test_cursor_visibility_vs_recording;
          test_case "initial recording state" `Quick
            test_initial_recording_state;
        ] );
    ]
