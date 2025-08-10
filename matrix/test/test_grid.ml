open Alcotest
module G = Grid
module C = G.Cell

let default_style = Ansi.Style.default
let cell = testable C.pp C.equal

let get_cell g ~row ~col =
  match G.get g ~row ~col with
  | Some c -> c
  | None -> failwith (Printf.sprintf "Cell (%d, %d) out of bounds" row col)

let dirty_region = testable G.pp_dirty_region G.equal_dirty_region

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
  let _ = G.set_text g ~row:2 ~col:0 ~text:"end " ~attrs:default_style in
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

(* New tests start here *)
let test_set_grapheme_link () =
  let g = G.create ~rows:1 ~cols:1 () in
  let link = "https://example.com" in
  G.set_grapheme ~link g ~row:0 ~col:0 ~glyph:"a" ~attrs:default_style;
  let cell = get_cell g ~row:0 ~col:0 in
  check string "glyph with link" "a" (C.get_text cell);
  (* Note: Link ID is internal; test via diff or copy to ensure preservation *)
  let copy = G.copy g in
  check bool "link preserved in copy" (G.diff_cells g copy = []) true

let test_set_grapheme_east_asian_override () =
  let g = G.create ~rows:1 ~cols:3 ~east_asian_context:false () in
  let ambiguous = "￦" in
  (* U+FFE6, ambiguous width *)
  G.set_grapheme ~east_asian_context:true g ~row:0 ~col:0 ~glyph:ambiguous
    ~attrs:default_style;
  check int "ambiguous wide in east_asian override" 2
    (C.width (get_cell g ~row:0 ~col:0));
  check bool "continuation for ambiguous wide" true
    (C.is_continuation (get_cell g ~row:0 ~col:1))

let test_set_grapheme_alpha_blend () =
  let g = G.create ~rows:1 ~cols:1 () in
  let fg = Ansi.Style.RGB (255, 0, 0) in
  (* Red *)
  let bg = Ansi.Style.RGBA (0, 255, 0, 128) in
  (* Semi-transparent green *)
  let attrs = Ansi.Style.with_fg fg (Ansi.Style.with_bg bg default_style) in
  G.set_grapheme g ~row:0 ~col:0 ~glyph:"a" ~attrs;
  let cell = get_cell g ~row:0 ~col:0 in
  let style = C.get_style cell in
  check bool "blended bg is opaque"
    (match Ansi.Style.bg style with Ansi.Style.RGB _ -> true | _ -> false)
    true

let test_set_text_link () =
  let g = G.create ~rows:1 ~cols:5 () in
  let link = "https://example.com" in
  ignore (G.set_text ~link g ~row:0 ~col:0 ~text:"abc" ~attrs:default_style);
  let copy = G.copy g in
  check bool "links preserved across cells" (G.diff_cells g copy = []) true

let test_set_text_east_asian_override () =
  let g = G.create ~rows:1 ~cols:5 ~east_asian_context:false () in
  let text = "￦￦" in
  (* Two ambiguous width chars *)
  let width =
    G.set_text ~east_asian_context:true g ~row:0 ~col:0 ~text
      ~attrs:default_style
  in
  check int "ambiguous wide in override" 4 width

let test_set_text_max_width () =
  let g = G.create ~rows:1 ~cols:5 () in
  let width =
    G.set_text ~max_width:3 g ~row:0 ~col:0 ~text:"abcde" ~attrs:default_style
  in
  check int "truncated at max_width" 3 width;
  check cell "beyond max_width unchanged" C.empty (get_cell g ~row:0 ~col:3)

let test_set_text_zero_width () =
  let g = G.create ~rows:1 ~cols:5 () in
  let width =
    G.set_text g ~row:0 ~col:0 ~text:"a\u{200D}b" ~attrs:default_style
  in
  (* a + ZWJ + b - ZWJ sticks to 'a' but doesn't join with 'b' *)
  (* First grapheme cluster "a\u{200D}" goes to cell 0 *)
  check string "first cluster contains a+ZWJ" "a\u{200D}"
    (C.get_text (get_cell g ~row:0 ~col:0));
  check int "first cluster width" 1 (C.width (get_cell g ~row:0 ~col:0));
  (* Second grapheme cluster "b" goes to cell 1 *)
  check string "second cluster contains b" "b"
    (C.get_text (get_cell g ~row:0 ~col:1));
  check int "second cluster width" 1 (C.width (get_cell g ~row:0 ~col:1));
  (* Total width should be 2 *)
  check int "total width" 2 width

let test_set_text_combining_marks () =
  let g = G.create ~rows:1 ~cols:5 () in
  ignore (G.set_text g ~row:0 ~col:0 ~text:"e\u{0301}" ~attrs:default_style);
  (* e + acute *)
  check string "combining mark appends" "e\u{0301}"
    (C.get_text (get_cell g ~row:0 ~col:0));
  check int "combining mark width 1" 1 (C.width (get_cell g ~row:0 ~col:0))

let test_clear_with_style () =
  let g = G.create ~rows:1 ~cols:1 () in
  let style = Ansi.Style.with_bg (Ansi.Style.RGB (255, 0, 0)) default_style in
  G.clear ~style g;
  check bool "clear with style sets bg"
    (Ansi.Style.bg (C.get_style (get_cell g ~row:0 ~col:0))
    = Ansi.Style.RGB (255, 0, 0))
    true

let test_fill_space () =
  let g = G.create ~rows:1 ~cols:2 () in
  G.fill_space g;
  check string "fills with space" " " (C.get_text (get_cell g ~row:0 ~col:0));
  let style = Ansi.Style.with_fg (Ansi.Style.RGB (0, 255, 0)) default_style in
  G.fill_space ~style g;
  check bool "fills with style"
    (Ansi.Style.fg (C.get_style (get_cell g ~row:0 ~col:0))
    = Ansi.Style.RGB (0, 255, 0))
    true

let test_resize_preservation () =
  let g = G.create ~rows:2 ~cols:2 () in
  G.set_grapheme g ~row:0 ~col:0 ~glyph:"a" ~attrs:default_style;
  G.set_grapheme g ~row:1 ~col:1 ~glyph:"b" ~attrs:default_style;
  G.resize g ~rows:3 ~cols:3;
  check string "preserves content" "a" (C.get_text (get_cell g ~row:0 ~col:0));
  check cell "new cells empty" C.empty (get_cell g ~row:2 ~col:2);
  G.resize g ~rows:1 ~cols:1;
  check string "truncates content" "a" (C.get_text (get_cell g ~row:0 ~col:0))

let test_blit_clipping () =
  let src = G.create ~rows:3 ~cols:3 () in
  let _ = G.set_text src ~row:0 ~col:0 ~text:"abc" ~attrs:default_style in
  let dst = G.create ~rows:2 ~cols:2 () in
  let src_rect = { G.row = 0; col = 0; width = 3; height = 3 } in
  G.blit ~src ~src_rect ~dst ~dst_pos:(0, 0);
  check string "clips to dst bounds" "a"
    (C.get_text (get_cell dst ~row:0 ~col:0));
  check string "clips to dst bounds" "b"
    (C.get_text (get_cell dst ~row:0 ~col:1))

let test_blit_negative_pos () =
  let src = G.create ~rows:1 ~cols:1 () in
  G.set_grapheme src ~row:0 ~col:0 ~glyph:"a" ~attrs:default_style;
  let dst = G.create ~rows:1 ~cols:1 () in
  let src_rect = { G.row = 0; col = 0; width = 1; height = 1 } in
  G.blit ~src ~src_rect ~dst ~dst_pos:(-1, -1);
  check cell "negative pos no-op" C.empty (get_cell dst ~row:0 ~col:0)

let test_flush_damage_multi_ops () =
  let g = G.create ~rows:3 ~cols:3 () in
  G.set_grapheme g ~row:0 ~col:0 ~glyph:"a" ~attrs:default_style;
  G.clear_line g 1 0;
  let regions = G.flush_damage g in
  check int "multi ops merge regions" 1 (List.length regions);
  let r = List.hd regions in
  check int "covers all dirty" 2 r.G.height

let test_diff_after_resize () =
  let prev = G.create ~rows:2 ~cols:2 () in
  let curr = G.copy prev in
  G.resize curr ~rows:3 ~cols:3;
  let regions = G.diff prev curr in
  check int "diff detects resize changes" 1 (List.length regions)

let test_copy_preserves_links () =
  let g = G.create ~rows:1 ~cols:1 () in
  G.set_grapheme ~link:"url" g ~row:0 ~col:0 ~glyph:"a" ~attrs:default_style;
  let copy = G.copy g in
  check bool "copy preserves links via diff" (G.diff_cells g copy = []) true

let test_char_width () =
  check int "narrow char" 1 (G.char_width (Uchar.of_char 'a'));
  check int "wide char" 2 (G.char_width (Uchar.of_int 0x4E9C));
  (* 亚 *)
  check int "control char" (-1) (G.char_width (Uchar.of_int 0x000A));
  (* LF *)
  check int "ambiguous wide in east_asian" 2
    (G.char_width ~east_asian:true (Uchar.of_int 0xFFE6))

let test_string_width () =
  check int "simple string" 5 (G.string_width "hello");
  check int "with wide" 3 (G.string_width "a宽");
  check int "emoji sequence" 2 (G.string_width "👩‍👩‍👧‍👦");
  check int "combining" 1 (G.string_width "e\u{0301}");
  check int "zero width" 1 (G.string_width "a\u{200D}")

let test_rect_equality () =
  let r1 = { G.row = 0; col = 0; width = 1; height = 1 } in
  let r2 = { G.row = 0; col = 0; width = 1; height = 1 } in
  check bool "equal rects" true (G.equal_rect r1 r2);
  let r3 = { r1 with width = 2 } in
  check bool "unequal rects" false (G.equal_rect r1 r3)

let test_dirty_region_equality () =
  let dr1 = { G.min_row = 0; max_row = 1; min_col = 0; max_col = 1 } in
  let dr2 = { G.min_row = 0; max_row = 1; min_col = 0; max_col = 1 } in
  check bool "equal dirty regions" true (G.equal_dirty_region dr1 dr2);
  let dr3 = { dr1 with max_row = 2 } in
  check bool "unequal dirty regions" false (G.equal_dirty_region dr1 dr3)

let test_compute_dirty_regions () =
  let dirty_flags = [| false; true; true; false |] in
  let regions = G.compute_dirty_regions dirty_flags 5 in
  check (list dirty_region) "groups contiguous dirty rows"
    [ { G.min_row = 1; max_row = 2; min_col = 0; max_col = 4 } ]
    regions

let test_compute_update_regions () =
  let cells = [ (0, 0); (0, 2); (1, 0); (1, 1); (3, 0) ] in
  let regions = G.compute_update_regions cells in
  check (list dirty_region) "merges into minimal regions"
    [
      { G.min_row = 0; max_row = 0; min_col = 0; max_col = 2 };
      { G.min_row = 1; max_row = 1; min_col = 0; max_col = 1 };
      { G.min_row = 3; max_row = 3; min_col = 0; max_col = 0 };
    ]
    regions

let test_diff_regions_detailed () =
  let prev = G.create ~rows:2 ~cols:2 () in
  let curr = G.copy prev in
  G.set_grapheme curr ~row:0 ~col:0 ~glyph:"a" ~attrs:default_style;
  G.set_grapheme curr ~row:1 ~col:1 ~glyph:"b" ~attrs:default_style;
  let detailed = G.diff_regions_detailed prev curr in
  check int "detailed regions count" 2 (List.length detailed);
  let _r1, c1 = List.nth detailed 0 in
  check (list (pair int int)) "cells in region 1" [ (0, 0) ] c1;
  let _r2, c2 = List.nth detailed 1 in
  check (list (pair int int)) "cells in region 2" [ (1, 1) ] c2

let test_edge_cases_zero_dims () =
  let g = G.create ~rows:0 ~cols:0 () in
  G.set_grapheme g ~row:0 ~col:0 ~glyph:"a" ~attrs:default_style;
  (* no-op *)
  check int "set on zero dims no crash" 0 (G.rows g);
  G.resize g ~rows:1 ~cols:1;
  check cell "resize from zero" C.empty (get_cell g ~row:0 ~col:0)

let test_edge_cases_negative_indices () =
  let g = G.create ~rows:2 ~cols:2 () in
  G.set g ~row:(-1) ~col:(-1)
    (Some (C.make_glyph "a" ~style:default_style ~east_asian_context:false));
  (* no-op *)
  check (option cell) "negative get" None (G.get g ~row:(-1) ~col:(-1));
  G.clear_rect g ~row_start:(-1) ~row_end:1 ~col_start:(-1) ~col_end:1;
  check string "negative clear doesn't affect" ""
    (C.get_text (get_cell g ~row:0 ~col:0))
(* still empty *)

let test_edge_cases_empty_inputs () =
  let g = G.create ~rows:1 ~cols:1 () in
  let width = G.set_text g ~row:0 ~col:0 ~text:"" ~attrs:default_style in
  check int "empty text width 0" 0 width;
  check cell "empty text no change" C.empty (get_cell g ~row:0 ~col:0);
  G.set_grapheme g ~row:0 ~col:0 ~glyph:"" ~attrs:default_style;
  check cell "empty glyph no change" C.empty (get_cell g ~row:0 ~col:0)

let test_edge_cases_wide_overwrite_wide () =
  let g = G.create ~rows:1 ~cols:3 () in
  G.set_grapheme g ~row:0 ~col:0 ~glyph:"宽" ~attrs:default_style;
  G.set_grapheme g ~row:0 ~col:0 ~glyph:"広" ~attrs:default_style;
  (* Another wide *)
  check string "wide overwrites wide" "広"
    (C.get_text (get_cell g ~row:0 ~col:0));
  check bool "continuation updated" true
    (C.is_continuation (get_cell g ~row:0 ~col:1));
  check cell "no orphan continuation" C.empty (get_cell g ~row:0 ~col:2)

let test_edge_cases_clear_wide () =
  let g = G.create ~rows:1 ~cols:3 () in
  G.set_grapheme g ~row:0 ~col:0 ~glyph:"宽" ~attrs:default_style;
  G.clear_rect g ~row_start:0 ~row_end:0 ~col_start:0 ~col_end:1;
  check cell "clear removes glyph" C.empty (get_cell g ~row:0 ~col:0);
  check cell "clear removes continuation" C.empty (get_cell g ~row:0 ~col:1)

let test_batch_many_changes () =
  let g = G.create ~rows:10 ~cols:10 () in
  G.with_updates g (fun g' ->
      for i = 0 to 9 do
        G.set_grapheme g' ~row:i ~col:i ~glyph:"x" ~attrs:default_style
      done);
  let prev = G.create ~rows:10 ~cols:10 () in
  check (list int) "batch detects all rows"
    (List.init 10 (fun i -> i))
    (G.diff_rows prev g)

(* Add to "Cell Access and Modification" suite *)

let test_set_text_regional_indicators () =
  let g = G.create ~rows:1 ~cols:4 () in
  let flag = "🇺🇸" in
  (* US flag: two regional indicators *)
  let width = G.set_text g ~row:0 ~col:0 ~text:flag ~attrs:default_style in
  check int "flag width 2" 2 width;
  check string "flag cluster" flag (C.get_text (get_cell g ~row:0 ~col:0));
  check bool "flag continuation" true
    (C.is_continuation (get_cell g ~row:0 ~col:1))
(* Covers regional indicator pairs as single wide cluster. *)

let test_set_grapheme_skin_tone () =
  let g = G.create ~rows:1 ~cols:2 () in
  let emoji = "👍🏽" in
  (* Thumbs up with medium skin tone *)
  G.set_grapheme g ~row:0 ~col:0 ~glyph:emoji ~attrs:default_style;
  check int "skin tone emoji width 2" 2 (C.width (get_cell g ~row:0 ~col:0));
  check string "skin tone preserved" emoji
    (C.get_text (get_cell g ~row:0 ~col:0))
(* Covers emoji with skin tone modifiers in clusters. *)

let test_set_text_malformed_utf8 () =
  let g = G.create ~rows:1 ~cols:1 () in
  let malformed = "\x80" in
  (* Invalid UTF-8 byte *)
  let width = G.set_text g ~row:0 ~col:0 ~text:malformed ~attrs:default_style in
  check int "malformed treated as width 1" 1 width;
  check string "malformed as replacement" "�"
    (C.get_text (get_cell g ~row:0 ~col:0))
(* Covers robustness to malformed UTF-8, using replacement char. *)

let test_set_grapheme_variation_selector () =
  let g = G.create ~rows:1 ~cols:2 () in
  let text_vs = "⭐\u{FE0E}" in
  (* Star with VS-15: text presentation, width 1 *)
  let emoji_vs = "⭐\u{FE0F}" in
  (* Star with VS-16: emoji, width 2 *)
  G.set_grapheme g ~row:0 ~col:0 ~glyph:text_vs ~attrs:default_style;
  check int "VS15 forces width 1" 1 (C.width (get_cell g ~row:0 ~col:0));
  G.set_grapheme g ~row:0 ~col:0 ~glyph:emoji_vs ~attrs:default_style;
  check int "VS16 forces width 2" 2 (C.width (get_cell g ~row:0 ~col:0))
(* Covers variation selectors affecting presentation/width. *)

let test_set_grapheme_alpha_blend_transparent () =
  let g = G.create ~rows:1 ~cols:1 () in
  let existing_bg =
    Ansi.Style.with_bg (Ansi.Style.RGB (255, 0, 0)) default_style
  in
  G.set_grapheme g ~row:0 ~col:0 ~glyph:" " ~attrs:existing_bg;
  let transparent = Ansi.Style.RGBA (0, 255, 0, 0) in
  (* Fully transparent green *)
  let attrs = Ansi.Style.with_bg transparent default_style in
  G.set_grapheme g ~row:0 ~col:0 ~glyph:"a" ~attrs;
  let final_bg = Ansi.Style.bg (C.get_style (get_cell g ~row:0 ~col:0)) in
  check bool "transparent blends to existing"
    (final_bg = Ansi.Style.RGB (255, 0, 0))
    true
(* Covers fully transparent blending preserving underlying color. *)

(* Add to "Bulk Operations" suite *)

let test_resize_cut_wide_char () =
  let g = G.create ~rows:1 ~cols:3 () in
  G.set_grapheme g ~row:0 ~col:1 ~glyph:"宽" ~attrs:default_style;
  (* Wide at col 1-2 *)
  G.resize g ~rows:1 ~cols:2;
  (* Cut to cols=2 *)
  check bool "wide char cleared when cut" true
    (C.is_empty (get_cell g ~row:0 ~col:1));
  G.resize g ~rows:1 ~cols:3;
  (* Expand back *)
  check cell "no orphan on expand" C.empty (get_cell g ~row:0 ~col:2)
(* Covers resize truncating wide chars, preventing orphans on re-expand. *)

let test_blit_wide_cross_boundary () =
  let src = G.create ~rows:1 ~cols:3 () in
  G.set_grapheme src ~row:0 ~col:1 ~glyph:"宽" ~attrs:default_style;
  (* Wide at 1-2 *)
  let dst = G.create ~rows:1 ~cols:3 () in
  let src_rect = { G.row = 0; col = 1; width = 2; height = 1 } in
  G.blit ~src ~src_rect ~dst ~dst_pos:(0, 0);
  check string "blit wide preserved" "宽"
    (C.get_text (get_cell dst ~row:0 ~col:0));
  check bool "blit continuation" true
    (C.is_continuation (get_cell dst ~row:0 ~col:1))
(* Covers blitting regions with wide chars, preserving continuations. *)

let test_clear_styled_with_links () =
  let g = G.create ~rows:1 ~cols:1 () in
  G.set_grapheme ~link:"url" g ~row:0 ~col:0 ~glyph:"a" ~attrs:default_style;
  let bg_style =
    Ansi.Style.with_bg (Ansi.Style.RGB (0, 255, 0)) default_style
  in
  G.clear ~style:bg_style g;
  let cell = get_cell g ~row:0 ~col:0 in
  check bool "clear removes link"
    (Ansi.Style.get_link_id (C.get_style cell) = 0)
    true;
  check bool "clear sets bg"
    (Ansi.Style.bg (C.get_style cell) = Ansi.Style.RGB (0, 255, 0))
    true
(* Covers clear removing links while applying styles. *)

(* Add to "Damage Tracking and Diffing" suite *)

let test_diff_mismatched_sizes () =
  let prev = G.create ~rows:2 ~cols:2 () in
  let curr = G.create ~rows:3 ~cols:3 () in
  let regions = G.diff prev curr in
  check int "treats extra as dirty" 1 (List.length regions);
  let r = List.hd regions in
  check int "full height dirty" 3 (r.G.max_row + 1)
(* Covers diff on size mismatch, treating new areas as dirty. *)

let test_cell_hash_equal_direct () =
  let s1 = Ansi.Style.with_fg (Ansi.Style.RGB (255, 0, 0)) default_style in
  let c1 = C.make_continuation ~style:s1 in
  let c2 = C.make_continuation ~style:s1 in
  check bool "equal continuations" true (C.equal c1 c2);
  check int "hash match" (C.hash c1) (C.hash c2);
  let c3 = C.make_continuation ~style:default_style in
  check bool "unequal styles" false (C.equal c1 c3)
(* Covers direct hash/equal for styled continuations, verifying collision avoidance. *)

(* Add to "Performance and Batching" suite *)

let test_batch_with_exception () =
  let g = G.create ~rows:1 ~cols:1 () in
  let prev = G.copy g in
  (try
     G.with_updates g (fun g' ->
         G.set_grapheme g' ~row:0 ~col:0 ~glyph:"x" ~attrs:default_style;
         raise Not_found)
   with Not_found -> ());
  (* Changes should be rolled back on exception *)
  let diffs = G.diff_cells prev g in
  check int "no changes after exception rollback" 0 (List.length diffs)
(* Covers batch transactional behavior - rollback on errors. *)

let test_storage_inline_vs_pooled () =
  let g = G.create ~rows:1 ~cols:20 () in
  let short = "abc" in
  (* <8 bytes - will be stored inline *)
  let long = String.make 10 'x' in
  (* >7 bytes - will be pooled *)
  (* Set text at different positions with enough space *)
  let _ = G.set_text g ~row:0 ~col:0 ~text:short ~attrs:default_style in
  let _ = G.set_text g ~row:0 ~col:5 ~text:long ~attrs:default_style in
  let copy = G.copy g in
  check bool "inline/pool preserved in copy" true (G.diff_cells g copy = []);
  (* Verify text is preserved correctly across inline/pooled boundary *)
  (* For set_text, each character goes to a separate cell *)
  check string "first char of short" "a"
    (C.get_text (get_cell copy ~row:0 ~col:0));
  check string "second char of short" "b"
    (C.get_text (get_cell copy ~row:0 ~col:1));
  check string "third char of short" "c"
    (C.get_text (get_cell copy ~row:0 ~col:2));
  check string "first char of long" "x"
    (C.get_text (get_cell copy ~row:0 ~col:5))
(* Covers inline (<8) vs pooled storage correctness. *)

let test_string_width_cache () =
  let east_asian = false in
  for i = 1 to 3000 do
    (* Many unique strings *)
    ignore (G.string_width ~east_asian (string_of_int i))
  done;
  let hit1 = G.string_width ~east_asian "1" in
  (* Should still work correctly *)
  check int "cache handles many strings" 1 hit1
(* Covers string width cache under load. *)

(* Add to "Utilities and Equality" suite *)

let test_string_width_control_in_cluster () =
  let s = "a\x0A b" in
  (* a + LF (control) + space + b *)
  check int "control skips width" 3 (G.string_width s)
(* Covers control chars in clusters not adding width. *)

let test_diff_no_changes () =
  let g1 = G.create ~rows:1 ~cols:1 () in
  let g2 = G.copy g1 in
  let diffs = G.diff_cells g1 g2 in
  check int "no changes detected" 0 (List.length diffs)
(* Covers diff optimization for identical grids. *)

(* Test suite definition *)
let () =
  run "Grid"
    [
      ( "Creation and Sizing",
        [
          test_case "Basic creation and resize" `Quick test_creation_and_sizing;
          test_case "Zero dimensions and resize from zero" `Quick
            test_edge_cases_zero_dims;
        ] );
      ( "Cell Access and Modification",
        [
          test_case "Get and Set" `Quick test_cell_access;
          test_case "Set Grapheme (narrow, wide, overwrite, clip)" `Quick
            test_set_grapheme;
          test_case "Set Grapheme with link" `Quick test_set_grapheme_link;
          test_case "Set Grapheme east_asian override" `Quick
            test_set_grapheme_east_asian_override;
          test_case "Set Grapheme alpha blending" `Quick
            test_set_grapheme_alpha_blend;
          test_case "Set Text (basic, clip, mixed-width)" `Quick test_set_text;
          test_case "Set Text with link" `Quick test_set_text_link;
          test_case "Set Text east_asian override" `Quick
            test_set_text_east_asian_override;
          test_case "Set Text with max_width" `Quick test_set_text_max_width;
          test_case "Set Text (complex emoji)" `Quick test_set_text_emoji;
          test_case "Set Text zero-width joiner" `Quick test_set_text_zero_width;
          test_case "Set Text combining marks" `Quick
            test_set_text_combining_marks;
          test_case "Set Text regional indicators" `Quick
            test_set_text_regional_indicators;
          test_case "Set Grapheme skin tone" `Quick test_set_grapheme_skin_tone;
          test_case "Set Text malformed UTF-8" `Quick
            test_set_text_malformed_utf8;
          test_case "Set Grapheme variation selector" `Quick
            test_set_grapheme_variation_selector;
          test_case "Set Grapheme alpha blend transparent" `Quick
            test_set_grapheme_alpha_blend_transparent;
        ] );
      ( "Bulk Operations",
        [
          test_case "Clear, Clear Line, Clear Rect" `Quick test_clear_operations;
          test_case "Clear with style" `Quick test_clear_with_style;
          test_case "Fill space (with/without style)" `Quick test_fill_space;
          test_case "Row Copy and Set" `Quick test_row_operations;
          test_case "Blit (simple, overlapping)" `Quick test_blit;
          test_case "Blit with clipping" `Quick test_blit_clipping;
          test_case "Blit negative position" `Quick test_blit_negative_pos;
          test_case "Swap and Deep Copy" `Quick test_swap_and_copy;
          test_case "Resize with preservation/truncation" `Quick
            test_resize_preservation;
          test_case "Resize cut wide char" `Quick test_resize_cut_wide_char;
          test_case "Blit wide cross boundary" `Quick
            test_blit_wide_cross_boundary;
          test_case "Clear styled with links" `Quick
            test_clear_styled_with_links;
        ] );
      ( "Damage Tracking and Diffing",
        [
          test_case "Damage and Diffing logic" `Quick test_damage_and_diff;
          test_case "Flush damage after multi ops" `Quick
            test_flush_damage_multi_ops;
          test_case "Diff after resize" `Quick test_diff_after_resize;
          test_case "to_string conversion" `Quick test_to_string;
          test_case "Compute dirty regions" `Quick test_compute_dirty_regions;
          test_case "Compute update regions" `Quick test_compute_update_regions;
          test_case "Diff regions detailed" `Quick test_diff_regions_detailed;
          test_case "Diff mismatched sizes" `Quick test_diff_mismatched_sizes;
          test_case "Cell hash equal direct" `Quick test_cell_hash_equal_direct;
        ] );
      ( "Performance and Batching",
        [
          test_case "with_updates defers hash calculation" `Quick
            test_with_updates;
          test_case "Batch with many changes" `Quick test_batch_many_changes;
          test_case "Batch with exception" `Quick test_batch_with_exception;
          test_case "Storage inline vs pooled" `Quick
            test_storage_inline_vs_pooled;
        ] );
      ( "Utilities and Equality",
        [
          test_case "Copy preserves links" `Quick test_copy_preserves_links;
          test_case "Char width (narrow, wide, control, ambiguous)" `Quick
            test_char_width;
          test_case "String width (simple, wide, emoji, combining)" `Quick
            test_string_width;
          test_case "Rect equality" `Quick test_rect_equality;
          test_case "Dirty region equality" `Quick test_dirty_region_equality;
          test_case "String width cache" `Quick test_string_width_cache;
          test_case "String width control in cluster" `Quick
            test_string_width_control_in_cluster;
          test_case "Diff no changes" `Quick test_diff_no_changes;
        ] );
      ( "Edge Cases",
        [
          test_case "Negative indices" `Quick test_edge_cases_negative_indices;
          test_case "Empty inputs" `Quick test_edge_cases_empty_inputs;
          test_case "Wide overwrite wide" `Quick
            test_edge_cases_wide_overwrite_wide;
          test_case "Clear wide characters" `Quick test_edge_cases_clear_wide;
        ] );
    ]
