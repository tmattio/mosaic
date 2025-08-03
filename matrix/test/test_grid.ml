open Alcotest
module G = Grid
module C = G.Cell

let default_style = Ansi.Style.default
let cell = testable C.pp C.equal

let get_cell g ~row ~col =
  match G.get g ~row ~col with
  | Some c -> c
  | None -> failwith (Printf.sprintf "Cell (%d, %d) out of bounds" row col)

let test_creation_and_sizing () =
  let g = G.create ~rows:10 ~cols:20 () in
  check int "correct number of rows" 10 (G.rows g);
  check int "correct number of cols" 20 (G.cols g);
  check cell "initial cell is empty" C.empty (get_cell g ~row:5 ~col:10);

  G.resize g ~rows:5 ~cols:15;
  check int "resized rows" 5 (G.rows g);
  check int "resized cols" 15 (G.cols g);
  check (option cell) "get out of bounds after resize" None
    (G.get g ~row:8 ~col:18);

  let g_zero = G.create ~rows:0 ~cols:0 () in
  check int "zero rows" 0 (G.rows g_zero);
  check int "zero cols" 0 (G.cols g_zero)

let test_cell_access () =
  let g = G.create ~rows:3 ~cols:5 () in
  let glyph = C.make_glyph "a" ~style:default_style ~east_asian_context:false in
  G.set g ~row:1 ~col:2 (Some glyph);
  check (option cell) "get returns set cell" (Some glyph)
    (G.get g ~row:1 ~col:2);
  check (option cell) "other cells remain empty" (Some C.empty)
    (G.get g ~row:1 ~col:1);

  G.set g ~row:1 ~col:2 None;
  check (option cell) "set None clears cell" (Some C.empty)
    (G.get g ~row:1 ~col:2);

  G.set g ~row:10 ~col:10 (Some glyph);
  (* No-op, should not raise error *)
  check (option cell) "get out of bounds is None" None (G.get g ~row:10 ~col:10)

let test_set_grapheme () =
  let g = G.create ~rows:2 ~cols:10 () in
  (* Test narrow char *)
  G.set_grapheme g ~row:0 ~col:0 ~glyph:"a" ~attrs:default_style;
  let expected_a =
    C.make_glyph "a" ~style:default_style ~east_asian_context:false
  in
  check cell "set narrow grapheme" expected_a (get_cell g ~row:0 ~col:0);

  (* Test wide char *)
  G.set_grapheme g ~row:0 ~col:1 ~glyph:"宽" ~attrs:default_style;
  let expected_wide =
    C.make_glyph "宽" ~style:default_style ~east_asian_context:false
  in
  let expected_cont = C.make_continuation ~style:default_style in
  check cell "set wide grapheme" expected_wide (get_cell g ~row:0 ~col:1);
  check cell "continuation cell is correct" expected_cont
    (get_cell g ~row:0 ~col:2);

  (* Test overwriting wide char with narrow *)
  G.set_grapheme g ~row:0 ~col:1 ~glyph:"b" ~attrs:default_style;
  let expected_b =
    C.make_glyph "b" ~style:default_style ~east_asian_context:false
  in
  check cell "overwrite wide with narrow" expected_b (get_cell g ~row:0 ~col:1);
  check cell "continuation cell is cleared" C.empty (get_cell g ~row:0 ~col:2);

  (* Test wide char at edge of screen (should be replaced with U+FFFD) *)
  G.set_grapheme g ~row:0 ~col:9 ~glyph:"宽" ~attrs:default_style;
  let expected_replace =
    C.make_glyph "�" ~style:default_style ~east_asian_context:false
  in
  check cell "wide grapheme at edge is replaced" expected_replace
    (get_cell g ~row:0 ~col:9)

let test_set_text () =
  let g = G.create ~rows:2 ~cols:10 () in
  let width = G.set_text g ~row:0 ~col:1 ~text:"hello" ~attrs:default_style in
  check int "set_text returns correct width" 5 width;
  check char "char 'h' is set" 'h' (C.get_text (get_cell g ~row:0 ~col:1)).[0];
  check char "char 'o' is set" 'o' (C.get_text (get_cell g ~row:0 ~col:5)).[0];

  (* Test with mixed-width and clipping *)
  let width2 = G.set_text g ~row:1 ~col:7 ~text:"a宽b" ~attrs:default_style in
  check int "clipped mixed-width text returns correct width" 3 width2;
  check string "clipped text 'a'" "a" (C.get_text (get_cell g ~row:1 ~col:7));
  let wide_cell = get_cell g ~row:1 ~col:8 in
  check string "clipped text '宽'" "宽" (C.get_text wide_cell);
  check int "clipped wide cell width" 2 (C.width wide_cell);
  (* Column 9 is the continuation of the wide character at column 8 *)
  check bool "col 9 is continuation cell" true
    (C.is_continuation (get_cell g ~row:1 ~col:9))

let test_set_text_emoji () =
  let g = G.create ~rows:1 ~cols:10 ~east_asian_context:true () in
  (* Test complex ZWJ emoji sequence: woman + ZWJ + woman + ZWJ + girl + ZWJ + boy *)
  let emoji = "👩‍👩‍👧‍👦" in
  let width = G.set_text g ~row:0 ~col:0 ~text:emoji ~attrs:default_style in
  check int "complex emoji width" 2 width;
  let emoji_cell = get_cell g ~row:0 ~col:0 in
  check bool "emoji cell is a glyph" true (C.is_glyph emoji_cell);
  check string "emoji cell text" emoji (C.get_text emoji_cell);
  check int "emoji cell reported width" 2 (C.width emoji_cell);
  check bool "continuation cell for emoji" true
    (C.is_continuation (get_cell g ~row:0 ~col:1))

let test_clear_operations () =
  let g = G.create ~rows:5 ~cols:5 () in
  let _ = G.set_text g ~row:1 ~col:0 ~text:"12345" ~attrs:default_style in
  let _ = G.set_text g ~row:2 ~col:0 ~text:"abcde" ~attrs:default_style in
  let _ = G.set_text g ~row:3 ~col:0 ~text:"XYZ" ~attrs:default_style in

  (* Test clear_line *)
  G.clear_line g 2 2;
  check string "clear_line keeps start" "a"
    (C.get_text (get_cell g ~row:2 ~col:0));
  check string "clear_line keeps middle" "b"
    (C.get_text (get_cell g ~row:2 ~col:1));
  check cell "clear_line clears cell" C.empty (get_cell g ~row:2 ~col:2);
  check cell "clear_line clears to end" C.empty (get_cell g ~row:2 ~col:4);

  (* Test clear_rect *)
  G.clear_rect g ~row_start:1 ~row_end:3 ~col_start:0 ~col_end:1;
  check cell "clear_rect clears top-left" C.empty (get_cell g ~row:1 ~col:0);
  check string "clear_rect leaves outside" "3"
    (C.get_text (get_cell g ~row:1 ~col:2));
  check cell "clear_rect clears bottom-right" C.empty (get_cell g ~row:3 ~col:1);
  check string "clear_rect leaves outside" "Z"
    (C.get_text (get_cell g ~row:3 ~col:2));

  (* Test clear *)
  G.clear g;
  check cell "clear clears all" C.empty (get_cell g ~row:1 ~col:2)

let test_row_operations () =
  let g = G.create ~rows:2 ~cols:5 () in
  let _ = G.set_text g ~row:0 ~col:0 ~text:"hello" ~attrs:default_style in
  let row_copy = G.copy_row g 0 in
  check int "copied row has correct length" 5 (Array.length row_copy);
  check string "copied row content" "e" (C.get_text row_copy.(1));

  G.set_row g 1 row_copy;
  check string "set_row copies content" "h"
    (C.get_text (get_cell g ~row:1 ~col:0));
  check string "set_row copies content" "o"
    (C.get_text (get_cell g ~row:1 ~col:4));

  let empty_row = G.make_empty_row ~cols:5 in
  G.set_row g 0 empty_row;
  check cell "make_empty_row works" C.empty (get_cell g ~row:0 ~col:2)

let test_blit () =
  let src = G.create ~rows:5 ~cols:5 () in
  let dst = G.create ~rows:5 ~cols:5 () in
  let _ = G.set_text src ~row:1 ~col:1 ~text:"ab" ~attrs:default_style in
  let _ = G.set_text src ~row:2 ~col:1 ~text:"cd" ~attrs:default_style in

  (* Simple blit *)
  let src_rect = { G.row = 1; col = 1; width = 2; height = 2 } in
  G.blit ~src ~src_rect ~dst ~dst_pos:(2, 2);

  check string "blit content top-left" "a"
    (C.get_text (get_cell dst ~row:2 ~col:2));
  check string "blit content bottom-right" "d"
    (C.get_text (get_cell dst ~row:3 ~col:3));
  check cell "blit leaves other cells empty" C.empty
    (get_cell dst ~row:1 ~col:1);

  (* Overlapping blit on same grid *)
  let g = G.create ~rows:5 ~cols:5 () in
  let _ = G.set_text g ~row:0 ~col:0 ~text:"12" ~attrs:default_style in
  let overlap_rect = { G.row = 0; col = 0; width = 2; height = 1 } in
  G.blit ~src:g ~src_rect:overlap_rect ~dst:g ~dst_pos:(0, 1);

  check string "overlapping blit shifts content" "1"
    (C.get_text (get_cell g ~row:0 ~col:1));
  check string "overlapping blit shifts content" "2"
    (C.get_text (get_cell g ~row:0 ~col:2))

let test_swap_and_copy () =
  let g1 = G.create ~rows:2 ~cols:2 () in
  G.set_grapheme g1 ~row:0 ~col:0 ~glyph:"a" ~attrs:default_style;
  let g2 = G.create ~rows:2 ~cols:2 () in
  G.set_grapheme g2 ~row:0 ~col:0 ~glyph:"b" ~attrs:default_style;

  G.swap (g1, g2);
  check string "g1 has g2's content after swap" "b"
    (C.get_text (get_cell g1 ~row:0 ~col:0));
  check string "g2 has g1's content after swap" "a"
    (C.get_text (get_cell g2 ~row:0 ~col:0));

  let g3 = G.copy g1 in
  G.set_grapheme g1 ~row:0 ~col:0 ~glyph:"c" ~attrs:default_style;
  check string "copy is a deep copy" "b"
    (C.get_text (get_cell g3 ~row:0 ~col:0))

let test_damage_and_diff () =
  let prev = G.create ~rows:5 ~cols:10 () in
  let _ = G.set_text prev ~row:0 ~col:0 ~text:"line 0" ~attrs:default_style in
  let _ = G.set_text prev ~row:4 ~col:0 ~text:"line 4" ~attrs:default_style in

  let curr = G.copy prev in

  (* No changes *)
  check (list int) "diff_rows on identical grids is empty" []
    (G.diff_rows prev curr);
  check
    (list (pair int int))
    "diff_cells on identical grids is empty" [] (G.diff_cells prev curr);

  (* Make some changes *)
  G.set_grapheme curr ~row:1 ~col:5 ~glyph:"X" ~attrs:default_style;
  G.set_grapheme curr ~row:3 ~col:2 ~glyph:"Y" ~attrs:default_style;
  G.set_grapheme curr ~row:3 ~col:3 ~glyph:"Z" ~attrs:default_style;

  (* Verify the changes actually happened *)
  check string "change 1 applied" "X" (C.get_text (get_cell curr ~row:1 ~col:5));
  check string "change 2 applied" "Y" (C.get_text (get_cell curr ~row:3 ~col:2));
  check string "change 3 applied" "Z" (C.get_text (get_cell curr ~row:3 ~col:3));

  (* Check that prev is unchanged *)
  check cell "prev unchanged at 1,5" C.empty (get_cell prev ~row:1 ~col:5);
  check cell "prev unchanged at 3,2" C.empty (get_cell prev ~row:3 ~col:2);

  let dirty_rows = G.diff_rows prev curr in
  check (list int) "diff_rows finds changed rows" [ 1; 3 ] dirty_rows;

  let dirty_cells = G.diff_cells prev curr in
  check
    (list (pair int int))
    "diff_cells finds changed cells"
    [ (1, 5); (3, 2); (3, 3) ]
    dirty_cells;

  let regions = G.diff prev curr in
  check int "diff finds correct number of regions" 2 (List.length regions);
  let r1 = List.nth regions 0 in
  let r2 = List.nth regions 1 in
  check int "region 1 min_row" 1 r1.G.min_row;
  check int "region 1 max_row" 1 r1.G.max_row;
  check int "region 2 min_row" 3 r2.G.min_row;
  check int "region 2 max_row" 3 r2.G.max_row;

  (* Test flush_damage *)
  let g_damage = G.create ~rows:5 ~cols:10 () in
  G.set_grapheme g_damage ~row:1 ~col:1 ~glyph:"A" ~attrs:default_style;
  G.set_grapheme g_damage ~row:2 ~col:1 ~glyph:"B" ~attrs:default_style;
  let damage_regions = G.flush_damage g_damage in
  check int "flush_damage merges contiguous rows" 1 (List.length damage_regions);
  let dr = List.hd damage_regions in
  check int "flushed region start row" 1 dr.G.row;
  check int "flushed region start col" 0 dr.G.col;
  check int "flushed region height" 2 dr.G.height;
  check int "flushed region width" 10 dr.G.width;

  let no_damage = G.flush_damage g_damage in
  check (list (of_pp G.pp_rect)) "flush_damage clears damage" [] no_damage

let test_to_string () =
  let g = G.create ~rows:3 ~cols:10 () in
  let _ = G.set_text g ~row:0 ~col:0 ~text:"hello" ~attrs:default_style in
  let _ = G.set_text g ~row:1 ~col:0 ~text:"a宽b" ~attrs:default_style in
  let _ = G.set_text g ~row:2 ~col:0 ~text:"end  " ~attrs:default_style in
  let expected = "hello\na宽b\nend" in
  check string "to_string represents grid content correctly" expected
    (G.to_string g)

let test_with_updates () =
  let g = G.create ~rows:2 ~cols:5 () in
  let prev_copy = G.copy g in

  G.with_updates g (fun g' ->
      G.set_grapheme g' ~row:0 ~col:0 ~glyph:"a" ~attrs:default_style;
      G.set_grapheme g' ~row:1 ~col:4 ~glyph:"b" ~attrs:default_style;
      (* At this point, inside the function, row hashes are not yet updated *)
      let dirty_rows_inside = G.diff_rows prev_copy g' in
      (* This check is a bit implementation-specific, but demonstrates the deferral *)
      check (list int) "hashes are not updated inside with_updates" []
        dirty_rows_inside);

  (* After the function, hashes should be updated *)
  let dirty_rows_after = G.diff_rows prev_copy g in
  check (list int) "hashes are updated after with_updates" [ 0; 1 ]
    dirty_rows_after;
  check string "cell 'a' is set" "a" (C.get_text (get_cell g ~row:0 ~col:0));
  check string "cell 'b' is set" "b" (C.get_text (get_cell g ~row:1 ~col:4))

(* Test suite definition *)
let () =
  run "Grid"
    [
      ( "Creation and Sizing",
        [
          test_case "Basic creation and resize" `Quick test_creation_and_sizing;
        ] );
      ( "Cell Access and Modification",
        [
          test_case "Get and Set" `Quick test_cell_access;
          test_case "Set Grapheme (narrow, wide, overwrite, clip)" `Quick
            test_set_grapheme;
          test_case "Set Text (basic, clip, mixed-width)" `Quick test_set_text;
          test_case "Set Text (complex emoji)" `Quick test_set_text_emoji;
        ] );
      ( "Bulk Operations",
        [
          test_case "Clear, Clear Line, Clear Rect" `Quick test_clear_operations;
          test_case "Row Copy and Set" `Quick test_row_operations;
          test_case "Blit (simple, overlapping)" `Quick test_blit;
          test_case "Swap and Deep Copy" `Quick test_swap_and_copy;
        ] );
      ( "Damage Tracking and Diffing",
        [
          test_case "Damage and Diffing logic" `Quick test_damage_and_diff;
          test_case "to_string conversion" `Quick test_to_string;
        ] );
      ( "Performance and Batching",
        [
          test_case "with_updates defers hash calculation" `Quick
            test_with_updates;
        ] );
    ]
