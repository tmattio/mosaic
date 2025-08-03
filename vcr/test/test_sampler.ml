open Alcotest

(* Create Alcotest testable for frames *)
let frame_testable =
  let pp fmt frame =
    Format.fprintf fmt
      "@[<v>Frame {@;\
       timestamp = %.3f;@;\
       cursor_row = %d;@;\
       cursor_col = %d;@;\
       cursor_visible = %b;@;\
       cursor_moved = %b;@;\
       dirty_regions = %d@;\
       }@]"
      frame.Vcr.Sampler.timestamp frame.Vcr.Sampler.cursor_row
      frame.Vcr.Sampler.cursor_col frame.Vcr.Sampler.cursor_visible
      frame.Vcr.Sampler.cursor_moved
      (List.length frame.Vcr.Sampler.dirty_regions)
  in
  let equal f1 f2 =
    Float.abs (f1.Vcr.Sampler.timestamp -. f2.Vcr.Sampler.timestamp) < 0.001
    && f1.Vcr.Sampler.cursor_row = f2.Vcr.Sampler.cursor_row
    && f1.Vcr.Sampler.cursor_col = f2.Vcr.Sampler.cursor_col
    && f1.Vcr.Sampler.cursor_visible = f2.Vcr.Sampler.cursor_visible
    && f1.Vcr.Sampler.cursor_moved = f2.Vcr.Sampler.cursor_moved
  in
  testable pp equal

let test_demo_tape_sampling () =
  let tape_content =
    {|
Output vcr/examples/demo.gif

Require echo

Set Shell "bash"
Set FontSize 32
Set Width 1200
Set Height 600

Type "echo 'Welcome to VCR!'" Sleep 500ms  Enter

Sleep 3s
|}
  in

  let tape =
    match Tape_lang.from_string tape_content with
    | Ok tape -> tape
    | Error err -> failwith (Printf.sprintf "Failed to parse tape: %s" err)
  in

  (* Run executor to get events *)
  let events, _vte, config =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    match Vcr.Executor.run ~sw ~env tape with
    | Error err ->
        failwith
          (Printf.sprintf "Executor failed: %s" (Vcr.Error.to_string err))
    | Ok result -> result
  in

  (* Calculate terminal dimensions *)
  let char_width = max 6 (config.font_size * 6 / 10) in
  let char_height = max 8 (config.font_size * 12 / 10) in
  let term_cols = config.width / char_width in
  let term_rows = config.height / char_height in

  (* Sample at 50 FPS *)
  let fps = 50.0 in
  let frames =
    Vcr.Sampler.sample ~fps ~strategy:Vcr.Sampler.Drop ~initial_cols:term_cols
      ~initial_rows:term_rows events
  in

  (* Verify we have frames *)
  check bool "has frames" true (List.length frames > 0);

  (* Check frame count - we have ~4.65s of content at 50 FPS *)
  (* Type takes 1.1s + Sleep 0.5s + Enter + Sleep 3s = ~4.6s *)
  let expected_frame_count = int_of_float (4.6 *. fps) in
  let actual_frame_count = List.length frames in
  check bool "frame count approximately correct" true
    (abs (actual_frame_count - expected_frame_count) < 10);

  (* Check first frame *)
  let first_frame = List.hd frames in
  check frame_testable "first frame"
    {
      Vcr.Sampler.timestamp = 0.0;
      cursor_row = 0;
      cursor_col = 0;
      cursor_visible = true;
      cursor_moved = false;
      grid = first_frame.Vcr.Sampler.grid;
      dirty_regions = [];
    }
    first_frame;

  (* Check that cursor moves during typing *)
  let typing_frames =
    List.filter (fun f -> f.Vcr.Sampler.timestamp < 1.1) frames
  in
  let cursor_positions =
    List.map (fun f -> f.Vcr.Sampler.cursor_col) typing_frames
  in
  let unique_positions = List.sort_uniq compare cursor_positions in
  check bool "cursor advances during typing" true
    (List.length unique_positions >= 20);

  (* Should see at least 20 positions for 22 chars *)

  (* Check final cursor position after Enter *)
  let final_frame = List.rev frames |> List.hd in
  check int "final cursor row" 1 final_frame.Vcr.Sampler.cursor_row;
  check int "final cursor col" 0 final_frame.Vcr.Sampler.cursor_col

let test_drop_strategy () =
  (* Create a simple event list *)
  let events =
    [
      Vcr.Event.Cursor_move { at = 0.0; row = 0; col = 0 };
      Vcr.Event.Sleep { at = 0.1; duration = 0.9 };
      Vcr.Event.Cursor_move { at = 1.0; row = 0; col = 1 };
    ]
  in

  (* Test Drop strategy at 10 FPS (should have 11 frames for 1s: 0.0, 0.1, ..., 1.0) *)
  let frames =
    Vcr.Sampler.sample ~fps:10.0 ~strategy:Drop ~initial_cols:80
      ~initial_rows:25 events
  in
  check int "drop strategy frame count" 11 (List.length frames);

  (* Verify frame timestamps *)
  List.iteri
    (fun i frame ->
      let expected_time = float_of_int i *. 0.1 in
      check bool
        (Printf.sprintf "frame %d timing" i)
        true
        (Float.abs (frame.Vcr.Sampler.timestamp -. expected_time) < 0.001))
    frames

let () =
  run "VCR Sampler"
    [
      ( "demo tape",
        [ test_case "samples frames correctly" `Quick test_demo_tape_sampling ]
      );
      ( "sampling strategies",
        [ test_case "drop strategy" `Quick test_drop_strategy ] );
    ]
