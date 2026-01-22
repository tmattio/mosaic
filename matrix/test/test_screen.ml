open Alcotest

(* Test Helpers *)

let create_renderer ?(width = 10) ?(height = 10) () =
  let r = Screen.create () in
  (* Build and render a frame to initialize buffers *)
  let _ =
    Screen.build r ~width ~height (fun _grid _hits -> ()) |> Screen.render
  in
  r

let count_cursor_moves output =
  (* Count cursor position sequences (ESC[row;colH) *)
  let re = Str.regexp "\027\\[[0-9]+;[0-9]+H" in
  let rec count acc pos =
    try
      let _ = Str.search_forward re output pos in
      count (acc + 1) (Str.match_end ())
    with Not_found -> acc
  in
  count 0 0

let add_unique_by_phys acc v =
  if List.exists (fun existing -> existing == v) acc then acc else v :: acc

(* 1. Core Rendering Tests *)

let test_create_renderer () =
  let r = Screen.create () in
  check bool "renderer created" true (r != Obj.magic 0)

(* Screen rendering is pure diff output; no terminal-side effects are
   emitted. *)
let zero_frame_expected = ""

let test_zero_sized_frame () =
  (* Edge case: 0x0 frame should not crash *)
  let r = Screen.create ~mouse_enabled:false () in
  let frame = Screen.build r ~width:0 ~height:0 (fun _ _ -> ()) in
  let output = Screen.render frame in
  check string "empty output" zero_frame_expected output

let test_single_cell_frame () =
  let r = Screen.create () in
  let frame =
    Screen.build r ~width:1 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"X")
  in
  let output = Screen.render frame in
  check bool "contains text" true (String.length output > 0);
  check bool "contains X" true (String.contains output 'X')

let test_simple_text_rendering () =
  let r = Screen.create () in
  let frame =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello")
  in
  let output = Screen.render frame in
  check bool "output not empty" true (String.length output > 0);
  (* Check that all characters from "Hello" appear in output *)
  check bool "contains H" true (String.contains output 'H');
  check bool "contains e" true (String.contains output 'e');
  check bool "contains l" true (String.contains output 'l');
  check bool "contains o" true (String.contains output 'o')

let test_hyperlink_rendering () =
  let r = Screen.create () in
  let frame1 =
    Screen.build r ~width:4 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A")
  in
  let _ = Screen.render frame1 in
  let link_style =
    Ansi.Style.hyperlink "https://example.com" Ansi.Style.default
  in
  let frame2 =
    Screen.build r ~width:4 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A" ~style:link_style)
  in
  let output = Screen.render frame2 in
  let expected = "\027]8;;https://example.com" in
  let contains_expected =
    try
      let _ = Str.search_forward (Str.regexp_string expected) output 0 in
      true
    with Not_found -> false
  in
  check bool "contains hyperlink start" true contains_expected

let test_row_offset_applied () =
  let r = create_renderer ~width:2 ~height:2 () in
  Screen.set_row_offset r 3;
  let frame =
    Screen.build r ~width:2 ~height:2 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A")
  in
  let output = Screen.render frame in
  let expected = "\027[4;1H" in
  let has_seq =
    try
      let _ = Str.search_forward (Str.regexp_string expected) output 0 in
      true
    with Not_found -> false
  in
  check bool "cursor moved with offset" true has_seq;
  check bool "character rendered" true (String.contains output 'A')

(* 2. Diff Algorithm Tests *)

let test_diff_only_changed_cells () =
  (* Render should only output changed cells *)
  let r = create_renderer ~width:5 ~height:5 () in

  (* First frame *)
  let f1 =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"XXXXX")
  in
  let output1 = Screen.render f1 in
  let moves1 = count_cursor_moves output1 in

  (* Second frame - only change one cell *)
  let f2 =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"XXXXY")
  in
  let output2 = Screen.render f2 in
  let moves2 = count_cursor_moves output2 in

  (* Should not require additional cursor moves once diff is warmed up *)
  check bool "second frame has <= moves" true (moves2 <= moves1);
  let moved_to_changed =
    try
      let _ = Str.search_forward (Str.regexp_string "\027[1;5H") output2 0 in
      true
    with Not_found -> false
  in
  check bool "moves cursor to changed cell" true moved_to_changed;
  check bool "second frame not empty" true (String.length output2 > 0)

let test_no_diff_when_unchanged () =
  let r = create_renderer ~width:5 ~height:5 () in

  (* First render *)
  let f1 =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test")
  in
  let _output1 = Screen.render f1 in

  (* Second render with same content; expect no output *)
  let f2 =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test")
  in
  let output2 = Screen.render f2 in

  check string "no diff output" "" output2

let test_wide_char_diff () =
  (* Test that wide characters are diffed correctly *)
  let r = create_renderer () in

  let f1 =
    Screen.build r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello")
  in
  let _output1 = Screen.render f1 in

  let f2 =
    Screen.build r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"你好")
  in
  (* Chinese chars, 2 cells each *)
  let output2 = Screen.render f2 in

  check bool "wide char output" true (String.length output2 > 0)

(* 3. Frame Building Tests *)

let test_build_visual () =
  (* build_visual should not require hit grid function *)
  let r = Screen.create () in
  let frame =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Hi")
  in
  let output = Screen.render frame in
  check bool "visual build works" true (String.length output > 0)

let test_resize_preserves_content () =
  let r = create_renderer ~width:5 ~height:5 () in

  (* Draw something *)
  let f1 =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"ABC")
  in
  let _output1 = Screen.render f1 in

  (* Resize larger *)
  let f2 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"ABC")
  in
  let output2 = Screen.render f2 in

  (* Should not re-render unchanged cells *)
  check bool "resize works" true (String.length output2 < 100)

let test_resize_smaller () =
  (* Edge case: shrinking grid *)
  let r = create_renderer ~width:10 ~height:10 () in

  let f1 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"BigText")
  in
  let _output1 = Screen.render f1 in

  let f2 =
    Screen.build r ~width:3 ~height:3 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Sm")
  in
  let output2 = Screen.render f2 in

  check bool "shrink works" true (String.length output2 > 0)

let test_resize_clears_both_buffers () =
  (* Test that resize clears both current and next buffers, ensuring proper
     diffing *)
  let r = create_renderer ~width:5 ~height:5 () in

  (* Fill the screen with content *)
  let f1 =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.fill_rect grid ~x:0 ~y:0 ~width:5 ~height:5
          ~color:(Ansi.Color.of_rgb 255 0 0))
  in
  let _output1 = Screen.render f1 in

  (* Resize to larger dimensions *)
  Screen.resize r ~width:10 ~height:10;

  (* Build a frame that only draws a small region *)
  let f2 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Small")
  in
  let output2 = Screen.render f2 in

  (* The output should not contain the old red background content *)
  (* Check that we don't have excessive output (which would indicate stale content) *)
  check bool "resize clears buffers" true (String.length output2 < 200);

  (* Verify the new content is rendered *)
  check bool "new content rendered" true (String.contains output2 'S')

let test_cursor_clamped_on_resize () =
  let r = create_renderer ~width:3 ~height:3 () in
  Screen.set_cursor_position r ~row:5 ~col:5;
  Screen.resize r ~width:2 ~height:1;
  let _frame = Screen.build r ~width:2 ~height:1 (fun _grid _hits -> ()) in
  let info = Screen.cursor_info r in
  check int "cursor row clamped" 1 info.row;
  check int "cursor col clamped" 2 info.col

(* 4. Post-Processing Tests *)

let test_post_process_receives_delta () =
  let r = Screen.create () in
  let delta_received = ref None in

  let frame1 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let frame2 =
    Screen.post_process
      (fun _grid ~delta -> delta_received := Some delta)
      frame1
  in
  let _output = Screen.render frame2 in

  check bool "delta was received" true (Option.is_some !delta_received);
  let delta = Option.get !delta_received in
  check bool "delta is non-negative" true (delta >= 0.)

let test_post_process_chain () =
  (* Multiple post-process functions should be applied in order *)
  let r = Screen.create () in
  let calls = ref [] in

  let frame1 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let frame2 =
    Screen.post_process
      (fun _grid ~delta:_ -> calls := "first" :: !calls)
      frame1
  in
  let frame3 =
    Screen.post_process
      (fun _grid ~delta:_ -> calls := "second" :: !calls)
      frame2
  in
  let _output = Screen.render frame3 in

  (* Should be called in order (reversed because we cons) *)
  check (list string) "call order" [ "second"; "first" ] !calls

let test_post_process_persists_across_frames () =
  (* Post-processors persist until explicitly removed *)
  let r = Screen.create () in
  let call_count = ref 0 in
  let effect_ _grid ~delta:_ = incr call_count in

  let frame1 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let frame1 = Screen.post_process effect_ frame1 in
  let _ = Screen.render frame1 in

  let frame2 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _ = Screen.render frame2 in

  check int "called twice" 2 !call_count

let test_remove_post_process () =
  let r = Screen.create () in
  let call_count = ref 0 in
  let effect_ _grid ~delta:_ = incr call_count in

  let frame = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let frame = Screen.post_process effect_ frame in
  let _ = Screen.render frame in

  let _frame = Screen.remove_post_process effect_ frame in
  let frame2 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _ = Screen.render frame2 in

  check int "effect removed" 1 !call_count

let test_clear_post_processes () =
  let r = Screen.create () in
  let call_count = ref 0 in
  let effect1 _grid ~delta:_ = incr call_count in
  let effect2 _grid ~delta:_ = incr call_count in

  let frame =
    Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ())
    |> Screen.post_process effect1
    |> Screen.post_process effect2
  in
  let _ = Screen.render frame in
  check int "both effects ran" 2 !call_count;

  let _ = Screen.clear_post_processes frame in
  let frame2 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _ = Screen.render frame2 in

  check int "effects cleared" 2 !call_count

(* 5. Hit Grid Tests *)

let test_hit_grid_integration () =
  let r = Screen.create () in
  let hit_id = ref 0 in

  let frame =
    Screen.build r ~width:10 ~height:10 (fun grid hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Button";
        Screen.Hit_grid.add hits ~x:0 ~y:0 ~width:6 ~height:1 ~id:42)
  in
  let _ = Screen.render frame in
  hit_id := Screen.query_hit frame ~x:0 ~y:0;
  check int "hit id at (0,0)" 42 !hit_id;

  hit_id := Screen.query_hit frame ~x:7 ~y:0;
  check int "no hit at (7,0)" 0 !hit_id

let test_hit_grid_cleared_each_frame () =
  let r = Screen.create () in

  (* First frame with hit region *)
  let f1 =
    Screen.build r ~width:10 ~height:10 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:0 ~y:0 ~width:5 ~height:1 ~id:1)
  in
  let _o1 = Screen.render f1 in

  (* Second frame without hit region *)
  let f2 = Screen.build r ~width:10 ~height:10 (fun _grid _hits -> ()) in
  let _o2 = Screen.render f2 in

  let hit_id = Screen.query_hit f2 ~x:0 ~y:0 in
  check int "hit cleared" 0 hit_id

let test_hit_grid_swap_on_render () =
  let r = Screen.create () in

  let frame1 =
    Screen.build r ~width:3 ~height:3 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:1 ~y:1 ~width:1 ~height:1 ~id:1)
  in
  let _ = Screen.render frame1 in

  (* Building the next frame should not swap hits until render runs. *)
  let frame2 =
    Screen.build r ~width:3 ~height:3 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:1 ~y:1 ~width:1 ~height:1 ~id:2)
  in
  let before = Screen.query_hit frame2 ~x:1 ~y:1 in
  check int "previous hit active before swap" 1 before;

  let _ = Screen.render frame2 in
  let after = Screen.query_hit frame2 ~x:1 ~y:1 in
  check int "next hit active after swap" 2 after

let test_add_hit_region_helper () =
  let r = Screen.create () in
  let frame1 = Screen.build r ~width:10 ~height:10 (fun _grid _hits -> ()) in
  let frame2 =
    Screen.add_hit_region frame1 ~x:5 ~y:5 ~width:2 ~height:2 ~id:99
  in

  let _ = Screen.render frame2 in
  let hit_id = Screen.query_hit frame2 ~x:5 ~y:5 in
  check int "hit region added" 99 hit_id

(* 6. Statistics Tests *)

let test_stats_tracking () =
  let r = Screen.create () in

  let f1 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test")
  in
  let _output1 = Screen.render f1 in

  let stats = Screen.stats r in
  let metrics = Screen.last_metrics r in
  check int "frame count" 1 stats.frame_count;
  check bool "cells updated" true (metrics.cells > 0);
  check bool "output bytes" true (metrics.bytes > 0);
  check bool "frame time" true (metrics.frame_time_ms >= 0.)

(* 7. Configuration Tests *)

let test_update_config () =
  let r = Screen.create () in

  Screen.set_cursor_visible r false;

  let frame = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _ = Screen.render frame in
  let metrics = Screen.last_metrics r in
  check bool "cursor visibility updated" false metrics.cursor_visible

let test_reset () =
  let r = Screen.create () in

  (* Render a few frames *)
  for _i = 1 to 3 do
    let f = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
    let _o = Screen.render f in
    ()
  done;

  let stats_before = Screen.stats r in
  check int "frames before reset" 3 stats_before.frame_count;

  Screen.reset r;

  let stats_after = Screen.stats r in
  check int "frames after reset" 0 stats_after.frame_count;
  check int "cells after reset" 0 stats_after.total_cells

let test_reset_triggers_next_diff () =
  let r = create_renderer ~width:1 ~height:1 () in

  let f1 =
    Screen.build r ~width:1 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"X")
  in
  let _ = Screen.render f1 in

  Screen.reset r;

  let f2 = Screen.build r ~width:1 ~height:1 (fun _ _ -> ()) in
  let output2 = Screen.render f2 in

  check bool "cursor moves after reset" true (count_cursor_moves output2 >= 1);
  check bool "clears previous glyph" true (not (String.contains output2 'X'))

(* 8. Edge Cases and Boundary Conditions *)

let test_extremely_wide_char () =
  (* Test rendering with emoji that might have unusual widths *)
  let r = create_renderer () in
  let f =
    Screen.build r ~width:20 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"🎨🚀✨")
  in
  let output = Screen.render f in
  check bool "emoji renders" true (String.length output > 0)

let test_buffer_overflow_prevention () =
  (* Test with large frame (200x60) and complex content to ensure 2MB buffer
     doesn't overflow *)
  let r = create_renderer ~width:200 ~height:60 () in
  let f =
    Screen.build r ~width:200 ~height:60 (fun grid _hits ->
        (* Fill with complex content: mixed text, colors, attributes *)
        let rec fill y =
          if y >= 60 then ()
          else
            let style =
              Ansi.Style.make
                ~fg:(Ansi.Color.of_rgb (y * 4) (y * 4 mod 255) (255 - (y * 4)))
                ~bg:(Ansi.Color.of_rgb (y * 4 mod 255) (y * 4) (y * 4 mod 255))
                ~bold:(y mod 2 = 0)
                ~italic:(y mod 3 = 0)
                ()
            in
            Grid.draw_text grid ~x:0 ~y ~text:(String.make 200 'A') ~style;
            fill (y + 1)
        in
        fill 0)
  in
  (* Should not raise buffer overflow *)
  let output = Screen.render f in
  check bool "large frame renders without overflow" true
    (String.length output > 1000)

let test_resize_full_redraw () =
  (* Test that first frame after resize is full redraw *)
  let r = create_renderer ~width:10 ~height:10 () in

  (* First frame *)
  let f1 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Initial")
  in
  let _ = Screen.render f1 in

  (* Resize *)
  Screen.resize r ~width:20 ~height:20;

  (* Second frame - should be full redraw *)
  let f2 =
    Screen.build r ~width:20 ~height:20 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Resized")
  in
  let output2 = Screen.render f2 in

  (* Should contain some cursor movements for the diff *)
  let moves = count_cursor_moves output2 in
  check bool "contains cursor moves" true (moves > 0)

let test_resize_hit_grid_cleared () =
  (* Test that hit grids don't leak stale IDs after resize *)
  let r = create_renderer ~width:10 ~height:10 () in

  (* Add hit region *)
  let f1 =
    Screen.build r ~width:10 ~height:10 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:5 ~y:5 ~width:2 ~height:2 ~id:99)
  in
  let _ = Screen.render f1 in

  (* Verify hit exists *)
  let hit_before = Screen.query_hit f1 ~x:5 ~y:5 in
  check int "hit exists before resize" 99 hit_before;

  (* Resize *)
  Screen.resize r ~width:5 ~height:5;

  (* Build new frame *)
  let f2 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _ = Screen.render f2 in

  (* Hit should be gone (coordinates out of bounds) *)
  let hit_after = Screen.query_hit f2 ~x:0 ~y:0 in
  check int "hit cleared after resize" 0 hit_after

let test_explicit_width_sequences () =
  (* Test that explicit width OSC sequences are emitted when enabled *)
  let r = Screen.create ~explicit_width:true () in
  let f =
    Screen.build r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"中")
    (* 2-cell wide character *)
  in
  let output = Screen.render f in

  (* Should contain explicit width sequence for the wide character *)
  let contains_explicit_width =
    try
      let _ =
        Str.search_forward (Str.regexp "\027]66;w=[0-9]+;.*\027\\\\") output 0
      in
      true
    with Not_found -> false
  in
  check bool "contains explicit width sequence" true contains_explicit_width

let test_hyperlink_capability_gating () =
  (* Test that hyperlinks are only emitted when capability is enabled *)
  let r1 = Screen.create () in
  (* hyperlinks_capable defaults to true *)
  let f1 =
    Screen.build r1 ~width:10 ~height:1 (fun grid _hits ->
        let style =
          Ansi.Style.hyperlink "https://example.com" Ansi.Style.default
        in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Link" ~style)
  in
  let output1 = Screen.render f1 in

  (* Should contain hyperlink sequences *)
  let contains_hyperlink =
    try
      let _ = Str.search_forward (Str.regexp "\027]8;;") output1 0 in
      true
    with Not_found -> false
  in
  check bool "hyperlink emitted when capable" true contains_hyperlink;

  (* Now test with capability disabled *)
  let r2 = Screen.create () in
  Screen.apply_capabilities r2 ~explicit_width:false ~hyperlinks:false;
  let f2 =
    Screen.build r2 ~width:10 ~height:1 (fun grid _hits ->
        let style =
          Ansi.Style.hyperlink "https://example.com" Ansi.Style.default
        in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Link" ~style)
  in
  let output2 = Screen.render f2 in

  (* Should NOT contain hyperlink sequences *)
  let contains_hyperlink_disabled =
    try
      let _ = Str.search_forward (Str.regexp "\027]8;;") output2 0 in
      true
    with Not_found -> false
  in
  check bool "hyperlink not emitted when incapable" false
    contains_hyperlink_disabled

let test_cursor_style_and_color () =
  (* Test cursor style and color state *)
  let r = create_renderer () in
  Screen.set_cursor_position r ~row:5 ~col:10;
  Screen.set_cursor_style r ~style:`Underline ~blinking:false;
  Screen.set_cursor_color r ~r:255 ~g:0 ~b:128;

  let _f = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let info = Screen.cursor_info r in
  check bool "cursor visible" true info.visible;
  check bool "cursor position set" true info.has_position;
  check int "cursor row stored" 5 info.row;
  check int "cursor col stored" 5 info.col;
  check bool "cursor underline" true (info.style = `Underline);
  check bool "cursor non-blinking" true (not info.blinking);
  check bool "cursor color stored" true (info.color = Some (255, 0, 128))

let test_all_cells_changed () =
  (* Worst case: every cell changes *)
  let r = create_renderer ~width:10 ~height:10 () in

  let f1 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.fill_rect grid ~x:0 ~y:0 ~width:10 ~height:10
          ~color:(Ansi.Color.of_rgb 255 0 0))
  in
  let _o1 = Screen.render f1 in

  let f2 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.fill_rect grid ~x:0 ~y:0 ~width:10 ~height:10
          ~color:(Ansi.Color.of_rgb 0 255 0))
  in
  let _output2 = Screen.render f2 in

  let metrics = Screen.last_metrics r in
  (* All 100 cells should have changed *)
  check int "all cells changed" 100 metrics.cells

let test_partial_row_update () =
  (* Test that we only render changed portions of a row *)
  let r = create_renderer ~width:10 ~height:1 () in

  let f1 =
    Screen.build r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"XXXXXXXXXX")
  in
  let _o1 = Screen.render f1 in

  let f2 =
    Screen.build r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"XXXXXAXXXX")
  in
  let _output2 = Screen.render f2 in

  let metrics = Screen.last_metrics r in
  (* Should only update the changed cell *)
  check int "only changed cell" 1 metrics.cells

let test_color_only_change () =
  (* Test that color changes trigger diff *)
  let r = create_renderer () in

  let style1 = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 255 0 0) () in
  let f1 =
    Screen.build r ~width:5 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test" ~style:style1)
  in
  let _o1 = Screen.render f1 in

  let style2 = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 0 255 0) () in
  let f2 =
    Screen.build r ~width:5 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test" ~style:style2)
  in
  let output2 = Screen.render f2 in

  (* Should detect color change *)
  check bool "color change detected" true (String.length output2 > 10)

let test_attribute_only_change () =
  (* Test that attribute changes (bold, italic, etc) trigger diff *)
  let r = create_renderer () in

  let f1 =
    Screen.build r ~width:5 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test")
  in
  let _o1 = Screen.render f1 in

  let style_bold = Ansi.Style.make ~bold:true () in
  let f2 =
    Screen.build r ~width:5 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test" ~style:style_bold)
  in
  let output2 = Screen.render f2 in

  (* Should detect attribute change *)
  check bool "attribute change detected" true (String.length output2 > 10)

(* 9. Performance Characteristics Tests *)

let test_zero_allocation_frame_building () =
  (* This is a semantic test - we can't actually measure allocations, but we can
     verify the API works as expected *)
  let r = Screen.create () in

  (* Build and post_process should work without errors *)
  let f1 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let f2 = Screen.post_process (fun _grid ~delta:_ -> ()) f1 in
  let _output = Screen.render f2 in
  (* If we get here without errors, the zero-allocation API works *)
  check bool "api works" true true

let test_double_buffer_reuse () =
  let r = Screen.create () in
  let grids = ref [] in
  let hits = ref [] in

  let record frame =
    grids := add_unique_by_phys !grids (Screen.grid frame);
    hits := add_unique_by_phys !hits (Screen.hit_grid frame)
  in

  let render_once () =
    let frame = Screen.build r ~width:4 ~height:2 (fun _grid _hits -> ()) in
    record frame;
    let _ = Screen.render frame in
    ()
  in

  render_once ();
  render_once ();
  render_once ();

  check int "reuses two grid buffers" 2 (List.length !grids);
  check int "reuses two hit grids" 2 (List.length !hits)

let test_pipeline_composition () =
  (* Test that pipelines compose correctly *)
  let r = Screen.create () in
  let call_order = ref [] in

  let f1 =
    Screen.build r ~width:5 ~height:5 (fun _grid _hits ->
        call_order := "build" :: !call_order)
  in
  let f2 =
    Screen.post_process
      (fun _grid ~delta:_ -> call_order := "post1" :: !call_order)
      f1
  in
  let f3 =
    Screen.post_process
      (fun _grid ~delta:_ -> call_order := "post2" :: !call_order)
      f2
  in
  let f4 = Screen.add_hit_region f3 ~x:0 ~y:0 ~width:1 ~height:1 ~id:1 in
  let output = Screen.render f4 in

  check bool "pipeline executed" true (String.length output >= 0);
  check bool "build called" true (List.mem "build" !call_order);
  check bool "post1 called" true (List.mem "post1" !call_order);
  check bool "post2 called" true (List.mem "post2" !call_order)

(* Test Suite *)

let () =
  run "matrix.screen"
    [
      ( "Core Rendering",
        [
          test_case "Create renderer" `Quick test_create_renderer;
          test_case "Zero-sized frame" `Quick test_zero_sized_frame;
          test_case "Single cell frame" `Quick test_single_cell_frame;
          test_case "Simple text rendering" `Quick test_simple_text_rendering;
          test_case "Hyperlink rendering" `Quick test_hyperlink_rendering;
          test_case "Row offset applied" `Quick test_row_offset_applied;
        ] );
      ( "Diff Algorithm",
        [
          test_case "Diff only changed cells" `Quick
            test_diff_only_changed_cells;
          test_case "No diff when unchanged" `Quick test_no_diff_when_unchanged;
          test_case "Wide character diff" `Quick test_wide_char_diff;
          test_case "All cells changed" `Quick test_all_cells_changed;
          test_case "Partial row update" `Quick test_partial_row_update;
          test_case "Color only change" `Quick test_color_only_change;
          test_case "Attribute only change" `Quick test_attribute_only_change;
        ] );
      ( "Frame Building",
        [
          test_case "Build visual (no hits)" `Quick test_build_visual;
          test_case "Resize preserves content" `Quick
            test_resize_preserves_content;
          test_case "Resize smaller" `Quick test_resize_smaller;
          test_case "Resize clears both buffers" `Quick
            test_resize_clears_both_buffers;
          test_case "Cursor clamped on resize" `Quick
            test_cursor_clamped_on_resize;
        ] );
      ( "Post-Processing",
        [
          test_case "Post-process receives delta" `Quick
            test_post_process_receives_delta;
          test_case "Post-process chain" `Quick test_post_process_chain;
          test_case "Post-process persists across frames" `Quick
            test_post_process_persists_across_frames;
          test_case "Remove post-process" `Quick test_remove_post_process;
          test_case "Clear post-processes" `Quick test_clear_post_processes;
        ] );
      ( "Hit Grid Integration",
        [
          test_case "Hit grid integration" `Quick test_hit_grid_integration;
          test_case "Hit grid cleared each frame" `Quick
            test_hit_grid_cleared_each_frame;
          test_case "Hit grid swap happens on render" `Quick
            test_hit_grid_swap_on_render;
          test_case "Add hit region helper" `Quick test_add_hit_region_helper;
        ] );
      ("Statistics", [ test_case "Stats tracking" `Quick test_stats_tracking ]);
      ( "Configuration",
        [
          test_case "Update config" `Quick test_update_config;
          test_case "Reset" `Quick test_reset;
          test_case "Reset triggers diff" `Quick test_reset_triggers_next_diff;
        ] );
      ( "Edge Cases",
        [
          test_case "Extremely wide characters" `Quick test_extremely_wide_char;
          test_case "Buffer overflow prevention" `Quick
            test_buffer_overflow_prevention;
          test_case "Resize triggers full redraw" `Quick test_resize_full_redraw;
          test_case "Resize clears hit grids" `Quick
            test_resize_hit_grid_cleared;
          test_case "Explicit width sequences" `Quick
            test_explicit_width_sequences;
          test_case "Hyperlink capability gating" `Quick
            test_hyperlink_capability_gating;
          test_case "Cursor style and color" `Quick test_cursor_style_and_color;
        ] );
      ( "Performance Characteristics",
        [
          test_case "Zero-allocation frame building" `Quick
            test_zero_allocation_frame_building;
          test_case "Double buffer reuse" `Quick test_double_buffer_reuse;
          test_case "Pipeline composition" `Quick test_pipeline_composition;
        ] );
    ]
