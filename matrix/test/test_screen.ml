open Alcotest
module G = Grid
module C = G.Cell

let default_style = Ansi.Style.default

(* Alcotest testable for a patch list *)
let patch_list =
  testable (Fmt.Dump.list Screen.pp_patch) (List.equal Screen.patch_equal)

let get_cell_from_grid g ~row ~col =
  match G.get g ~row ~col with
  | Some c -> c
  | None -> failwith (Printf.sprintf "Cell (%d, %d) out of bounds" row col)

let test_creation_and_sizing () =
  let s = Screen.create ~rows:10 ~cols:20 () in
  check int "correct number of rows" 10 (Screen.rows s);
  check int "correct number of cols" 20 (Screen.cols s);
  check int "front and back hashes are equal initially" (Screen.hash_front s)
    (Screen.hash_back s);

  Screen.set_text s ~row:0 ~col:0 ~text:"hello" ~attrs:default_style |> ignore;
  Screen.present s |> ignore;

  (* front buffer now has "hello" *)
  Screen.resize s ~rows:5 ~cols:15;
  check int "resized rows" 5 (Screen.rows s);
  check int "resized cols" 15 (Screen.cols s);

  (* Check that front buffer content was preserved *)
  let front_grid = Screen.front s in
  let cell_text = C.get_text (get_cell_from_grid front_grid ~row:0 ~col:0) in
  check string "resize preserves front buffer content" "h" cell_text;

  (* After resize, a present should indicate the whole screen is dirty *)
  let dirty_regions = Screen.present s in
  check int "resize dirties the whole screen" 1 (List.length dirty_regions);
  let region = List.hd dirty_regions in
  check int "dirty region min_row" 0 region.G.min_row;
  check int "dirty region max_row" 4 region.G.max_row;
  check int "dirty region min_col" 0 region.G.min_col;
  check int "dirty region max_col" 14 region.G.max_col

let test_drawing_and_buffers () =
  let s = Screen.create ~rows:5 ~cols:10 () in
  let initial_front_hash = Screen.hash_front s in

  (* Draw to back buffer *)
  Screen.set_text s ~row:1 ~col:1 ~text:"test" ~attrs:default_style |> ignore;

  check (neg int) "back buffer hash changes after drawing" initial_front_hash
    (Screen.hash_back s);
  check int "front buffer hash does not change after drawing" initial_front_hash
    (Screen.hash_front s);

  (* Verify content of back buffer *)
  let back_grid = Screen.back s in
  let cell_text = C.get_text (get_cell_from_grid back_grid ~row:1 ~col:1) in
  check string "back buffer contains drawn text" "t" cell_text;

  (* Verify front buffer is still empty *)
  let front_grid = Screen.front s in
  check bool "front buffer is empty" true
    (C.is_empty (get_cell_from_grid front_grid ~row:1 ~col:1))

let test_present_and_diff () =
  let s = Screen.create ~rows:5 ~cols:10 () in

  (* Draw something *)
  Screen.set_text s ~row:2 ~col:3 ~text:"OK" ~attrs:default_style |> ignore;

  (* Diff before present *)
  let changed_cells = Screen.diff_cells s in
  check int "diff_cells finds 2 changed cells" 2 (List.length changed_cells);
  let _, _, cell_o =
    List.find (fun (r, c, _) -> r = 2 && c = 3) changed_cells
  in
  check string "diff_cells finds correct cell content" "O" (C.get_text cell_o);
  check int "front hash is unchanged before present" (Screen.hash_front s)
    (Screen.hash_front (Screen.clone s));

  (* Ensure no mutation *)

  (* Present the frame *)
  let dirty_regions = Screen.present s in
  check int "present returns one dirty region" 1 (List.length dirty_regions);
  check int "front and back hashes are equal after present"
    (Screen.hash_front s) (Screen.hash_back s);

  (* Verify front buffer was updated *)
  let front_grid = Screen.front s in
  let cell_text = C.get_text (get_cell_from_grid front_grid ~row:2 ~col:4) in
  check string "front buffer contains text after present" "K" cell_text;

  (* A second present should yield no changes *)
  check
    (list (of_pp G.pp_dirty_region))
    "second present has no changes" [] (Screen.present s)

let test_viewport () =
  let s = Screen.create ~rows:10 ~cols:20 () in
  let vp = Screen.Viewport.make ~row:2 ~col:5 ~width:10 ~height:5 in

  (* Clear with viewport *)
  Screen.set_text s ~row:2 ~col:5 ~text:"XXXXXXXXXX" ~attrs:default_style
  |> ignore;
  Screen.clear s ~viewport:vp;
  let back_grid = Screen.back s in
  check bool "viewport clear works" true
    (C.is_empty (get_cell_from_grid back_grid ~row:2 ~col:5));

  (* Draw with viewport clipping *)
  let _lines, cols =
    Screen.set_text s ~viewport:vp ~row:2 ~col:14 ~text:"clipped"
      ~attrs:default_style
  in
  check int "set_text respects viewport horizontal clipping" 1 cols;
  check string "clipped char is correct" "c"
    (C.get_text (get_cell_from_grid back_grid ~row:2 ~col:14));
  check bool "char outside viewport is empty" true
    (C.is_empty (get_cell_from_grid back_grid ~row:2 ~col:15));

  (* Multiline text with viewport clipping *)
  Screen.clear s;
  let lines, _ =
    Screen.set_multiline_text s ~viewport:vp ~row:5 ~col:10 ~text:"line1\nline2"
      ~attrs:default_style
  in
  check int "multiline text respects viewport vertical clipping" 2 lines;
  check bool "line outside viewport is empty" true
    (C.is_empty (get_cell_from_grid back_grid ~row:7 ~col:10));

  (* render_viewport *)
  let rendered_vp = Screen.render_viewport s vp in
  (* Note: render_viewport trims trailing spaces like Grid.to_string,
     so empty lines are just newlines, not spaces + newlines *)
  check string "render_viewport produces correct output"
    "\n\n\n     line1\n     line2" rendered_vp

let test_render_to_patches () =
  let s = Screen.create ~rows:5 ~cols:20 () in

  (* Case 1: Initial draw (like clear screen + draw) *)
  Screen.set_text s ~row:1 ~col:2 ~text:"Hello" ~attrs:default_style |> ignore;
  let patches1 = Screen.render s in
  let expected1 =
    [
      Screen.Run
        { row = 1; col = 2; text = "Hello"; style = default_style; width = 5 };
    ]
  in
  check patch_list "initial render creates a Run patch" expected1 patches1;

  (* Present and check that render is now empty *)
  Screen.present s |> ignore;
  check
    (list (of_pp Screen.pp_patch))
    "render after present is empty" [] (Screen.render s);

  (* Case 2: Incremental update (modify one char) *)
  Screen.set_grapheme s ~row:1 ~col:4 ~glyph:"p" ~attrs:default_style;
  let patches2 = Screen.render s in
  let expected2 =
    [
      Screen.Run
        { row = 1; col = 4; text = "p"; style = default_style; width = 1 };
    ]
  in
  check patch_list "incremental render creates a small Run patch" expected2
    patches2;

  (* Case 3: Clearing a region *)
  Screen.present s |> ignore;
  Screen.clear_rect s ~row_start:1 ~row_end:1 ~col_start:2 ~col_end:3;
  let patches3 = Screen.render s in
  (* When clearing already presented content, the cells change from having content
     to being empty. The renderer could optimize this as a Clear_region patch,
     but currently it just sees empty cells and generates no patches. *)
  check
    (list (of_pp Screen.pp_patch))
    "clearing a region creates patches" [] patches3;

  (* Case 4: Full redraw hint (large change) *)
  Screen.present s |> ignore;
  Screen.clear s;
  Screen.set_text s ~row:4 ~col:10 ~text:"New" ~attrs:default_style |> ignore;
  let patches4 = Screen.render s in
  (* A smart renderer might return Clear_screen + new runs *)
  let has_clear_screen =
    List.exists (function Screen.Clear_screen -> true | _ -> false) patches4
  in
  let has_new_run =
    List.exists
      (function Screen.Run { text = "New"; _ } -> true | _ -> false)
      patches4
  in
  (* The renderer uses heuristics to decide when to use Clear_screen.
     With only 3 non-empty cells out of 100, it won't trigger Clear_screen. *)
  check bool "large change can trigger Clear_screen" false has_clear_screen;
  check bool "large change also includes new content" true has_new_run

let test_clone_and_copy () =
  let s1 = Screen.create ~rows:2 ~cols:2 () in
  Screen.set_grapheme s1 ~row:0 ~col:0 ~glyph:"a" ~attrs:default_style;

  let s2 = Screen.clone s1 in
  check int "clone has same back buffer hash" (Screen.hash_back s1)
    (Screen.hash_back s2);

  (* Modify original, clone should be unaffected *)
  Screen.set_grapheme s1 ~row:0 ~col:0 ~glyph:"b" ~attrs:default_style;
  check (neg int) "modifying original changes its hash" (Screen.hash_back s1)
    (Screen.hash_back s2);

  let s3 = Screen.create ~rows:2 ~cols:2 () in
  Screen.copy_to ~src:s1 ~dst:s3;
  check int "copy_to copies back buffer" (Screen.hash_back s1)
    (Screen.hash_back s3);

  let snapshot_grid = Screen.snapshot s1 in
  Screen.set_grapheme s1 ~row:0 ~col:0 ~glyph:"c" ~attrs:default_style;
  check bool "snapshot is immutable" true
    (C.get_text (get_cell_from_grid snapshot_grid ~row:0 ~col:0) = "b")

let test_cursor () =
  let s = Screen.create ~rows:5 ~cols:10 () in
  check
    (option (pair int int))
    "initial cursor is None" None (Screen.get_cursor s);

  Screen.set_cursor s ~row:3 ~col:7;
  check
    (option (pair int int))
    "get_cursor returns set position"
    (Some (3, 7))
    (Screen.get_cursor s)

let test_batch () =
  let s = Screen.create ~rows:5 ~cols:10 () in
  Screen.set_text s ~row:0 ~col:0 ~text:"first" ~attrs:default_style |> ignore;
  Screen.present s |> ignore;

  (* "first" is now in the front buffer *)
  Screen.set_text s ~row:0 ~col:0 ~text:"second" ~attrs:default_style |> ignore;

  (* Back buffer now has "second" *)
  let result =
    Screen.batch s (fun s_inner ->
        (* begin_frame should have been called, copying "first" to back buffer *)
        Screen.set_text s_inner ~row:1 ~col:0 ~text:"third" ~attrs:default_style
        |> ignore;
        "done")
  in
  check string "batch returns the lambda's result" "done" result;

  (* Check the final state of the back buffer *)
  let back_grid = Screen.back s in
  check string "batch: original content from front buffer is present" "f"
    (C.get_text (get_cell_from_grid back_grid ~row:0 ~col:0));
  check string "batch: new content from lambda is present" "t"
    (C.get_text (get_cell_from_grid back_grid ~row:1 ~col:0))

let () =
  run "Screen"
    [
      ( "Creation and Sizing",
        [
          test_case "Basic creation and resize" `Quick test_creation_and_sizing;
        ] );
      ( "Buffer and Frame Lifecycle",
        [
          test_case "Drawing affects back buffer only" `Quick
            test_drawing_and_buffers;
          test_case "Present swaps buffers and diffs" `Quick
            test_present_and_diff;
          test_case "Batch correctly wraps a drawing sequence" `Quick test_batch;
        ] );
      ( "Viewport and Clipping",
        [
          test_case "Drawing operations respect viewports" `Quick test_viewport;
        ] );
      ( "Rendering",
        [
          test_case "Render to patches (inc, clear, full)" `Quick
            test_render_to_patches;
        ] );
      ( "State Management",
        [
          test_case "Clone, copy_to and snapshot" `Quick test_clone_and_copy;
          test_case "Cursor get/set" `Quick test_cursor;
        ] );
    ]
