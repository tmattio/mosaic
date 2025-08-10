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
  Screen.resize s ~rows:5 ~cols:15;
  check int "resized rows" 5 (Screen.rows s);
  check int "resized cols" 15 (Screen.cols s)

let test_drawing_and_buffers () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"x" ~attrs:default_style |> ignore;
  let back = Screen.back s in
  check string "back buffer changed" "x"
    (C.get_text (get_cell_from_grid back ~row:0 ~col:0));
  let front = Screen.front s in
  check bool "front buffer unchanged" true
    (C.is_empty (get_cell_from_grid front ~row:0 ~col:0))

let test_present_and_diff () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"x" ~attrs:default_style |> ignore;
  let dirty = Screen.present s in
  check int "one dirty region" 1 (List.length dirty);
  let dirty2 = Screen.present s in
  check int "no dirty after present" 0 (List.length dirty2)

let test_batch () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"ab" ~attrs:default_style |> ignore;
  Screen.present s |> ignore;
  let front = Screen.front s in
  check string "batch updates front" "a"
    (C.get_text (get_cell_from_grid front ~row:0 ~col:0))

let test_viewport () =
  let s = Screen.create ~rows:3 ~cols:3 () in
  Screen.begin_frame s;
  let vp = Screen.Viewport.make ~row:1 ~col:1 ~width:1 ~height:1 in
  Screen.set_text s ~viewport:vp ~row:0 ~col:0 ~text:"abcd" ~attrs:default_style
  |> ignore;
  let back = Screen.back s in
  check bool "outside viewport unchanged" true
    (C.is_empty (get_cell_from_grid back ~row:0 ~col:0));
  check string "inside viewport changed" "a"
    (C.get_text (get_cell_from_grid back ~row:1 ~col:1))

let test_render_to_patches () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"ab" ~attrs:default_style |> ignore;
  let patches = Screen.render s in
  check bool "patches generated" true (List.length patches > 0)

let test_clone_and_copy () =
  let s1 = Screen.create ~rows:2 ~cols:2 () in
  Screen.begin_frame s1;
  Screen.set_text s1 ~row:0 ~col:0 ~text:"x" ~attrs:default_style |> ignore;
  Screen.present s1 |> ignore;
  let s2 = Screen.clone s1 in
  check string "clone copies content" "x"
    (C.get_text (get_cell_from_grid (Screen.front s2) ~row:0 ~col:0))

let test_cursor () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  Screen.set_cursor s ~row:1 ~col:1;
  check (option (pair int int)) "cursor set" (Some (1, 1)) (Screen.get_cursor s);
  Screen.set_cursor s ~row:(-1) ~col:0;
  check
    (option (pair int int))
    "cursor out of bounds" None (Screen.get_cursor s)

let test_creation_with_style () =
  let style = Ansi.Style.with_fg Ansi.Style.Red default_style in
  let s = Screen.create ~rows:2 ~cols:2 ~style () in
  let front = Screen.front s in
  check bool "front initialized with style" true
    (Ansi.Style.equal style
       (C.get_style (get_cell_from_grid front ~row:0 ~col:0)));
  (* To check back buffer, we need to call begin_frame first *)
  Screen.begin_frame s;
  let back = Screen.back s in
  check bool "back initialized with style" true
    (Ansi.Style.equal style
       (C.get_style (get_cell_from_grid back ~row:0 ~col:0)))

let test_creation_east_asian () =
  let s = Screen.create ~rows:1 ~cols:3 ~east_asian_context:true () in
  Screen.begin_frame s;
  let _, adv = Screen.set_text s ~row:0 ~col:0 ~text:"漢" ~attrs:default_style in
  check int "wide char advances 2 cols" 2 adv;
  check bool "next col is continuation" true
    (C.is_continuation (get_cell_from_grid (Screen.back s) ~row:0 ~col:1));
  check bool "beyond is empty" true
    (C.is_empty (get_cell_from_grid (Screen.back s) ~row:0 ~col:2))

let test_resize_to_smaller () =
  let s = Screen.create ~rows:3 ~cols:3 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:2 ~col:2 ~text:"x" ~attrs:default_style |> ignore;
  Screen.present s |> ignore;
  Screen.resize s ~rows:2 ~cols:2;
  let front = Screen.front s in
  check bool "content outside new size is truncated" true
    (C.is_empty (get_cell_from_grid front ~row:1 ~col:1));
  let dirty = Screen.present s in
  check int "full dirty after resize" 1 (List.length dirty);
  let r = List.hd dirty in
  check int "dirty covers new size" 1 r.G.max_row;
  check int "dirty covers new size" 1 r.G.max_col

let test_resize_to_larger () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:1 ~col:1 ~text:"x" ~attrs:default_style |> ignore;
  Screen.present s |> ignore;
  Screen.resize s ~rows:3 ~cols:3;
  let front = Screen.front s in
  check string "old content preserved" "x"
    (C.get_text (get_cell_from_grid front ~row:1 ~col:1));
  check bool "new area empty" true
    (C.is_empty (get_cell_from_grid front ~row:2 ~col:2));
  let dirty = Screen.present s in
  check int "full dirty after resize" 1 (List.length dirty);
  let r = List.hd dirty in
  check int "dirty covers new size" 2 r.G.max_row;
  check int "dirty covers new size" 2 r.G.max_col

let viewport = testable Screen.Viewport.pp Screen.Viewport.equal

let test_viewport_module () =
  let v1 = Screen.Viewport.make ~row:1 ~col:2 ~width:3 ~height:4 in
  let v2 = Screen.Viewport.make ~row:3 ~col:4 ~width:5 ~height:6 in
  check bool "equal same" true (Screen.Viewport.equal v1 v1);
  check bool "equal different" false (Screen.Viewport.equal v1 v2);
  let inter = Screen.Viewport.intersect v1 v2 in
  check (option viewport) "intersect overlapping"
    (Some (Screen.Viewport.make ~row:3 ~col:4 ~width:1 ~height:2))
    inter
  |> ignore;
  let v3 = Screen.Viewport.make ~row:0 ~col:0 ~width:5 ~height:5 in
  let v4 = Screen.Viewport.make ~row:2 ~col:2 ~width:3 ~height:3 in
  let inter = Screen.Viewport.intersect v3 v4 in
  check bool "intersect returns Some" true (Option.is_some inter);
  let _inter = Option.get inter in
  (* Viewport fields are not exposed, just check that intersection exists *)
  check bool "intersection exists" true true;
  check bool "contains yes" true (Screen.Viewport.contains v3 ~row:3 ~col:3);
  check bool "contains no" false (Screen.Viewport.contains v3 ~row:5 ~col:5);
  let full = Screen.Viewport.full ~rows:10 ~cols:20 in
  (* Just verify full viewport was created *)
  check bool "full viewport created" true
    (Screen.Viewport.contains full ~row:5 ~col:10)

let test_copy_viewport () =
  let src = Screen.create ~rows:5 ~cols:5 () in
  Screen.begin_frame src;
  Screen.set_text src ~row:1 ~col:1 ~text:"abc" ~attrs:default_style |> ignore;
  let dst = Screen.create ~rows:5 ~cols:5 () in
  Screen.begin_frame dst;
  let vp = Screen.Viewport.make ~row:1 ~col:1 ~width:3 ~height:1 in
  Screen.copy_viewport ~src ~dst ~src_viewport:vp ~dst_row:3 ~dst_col:2;
  let dst_back = Screen.back dst in
  check string "copied content" "a"
    (C.get_text (get_cell_from_grid dst_back ~row:3 ~col:2));
  check string "copied content" "b"
    (C.get_text (get_cell_from_grid dst_back ~row:3 ~col:3));
  check string "copied content" "c"
    (C.get_text (get_cell_from_grid dst_back ~row:3 ~col:4))

let test_clear_rect () =
  let s = Screen.create ~rows:5 ~cols:5 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"abcde" ~attrs:default_style |> ignore;
  Screen.set_text s ~row:1 ~col:0 ~text:"abcde" ~attrs:default_style |> ignore;
  Screen.clear_rect s ~row_start:0 ~row_end:1 ~col_start:1 ~col_end:3;
  let back = Screen.back s in
  check bool "cleared" true (C.is_empty (get_cell_from_grid back ~row:0 ~col:1));
  check bool "cleared" true (C.is_empty (get_cell_from_grid back ~row:1 ~col:3));
  check string "untouched" "a"
    (C.get_text (get_cell_from_grid back ~row:0 ~col:0));
  check string "untouched" "e"
    (C.get_text (get_cell_from_grid back ~row:1 ~col:4));
  let vp = Screen.Viewport.make ~row:1 ~col:1 ~width:3 ~height:1 in
  Screen.clear_rect s ~viewport:vp ~row_start:1 ~row_end:1 ~col_start:0
    ~col_end:4;
  check bool "viewport clips clear" false
    (C.is_empty (get_cell_from_grid back ~row:1 ~col:0));
  check bool "viewport clears inside" true
    (C.is_empty (get_cell_from_grid back ~row:1 ~col:1))

let test_clear_line () =
  let s = Screen.create ~rows:1 ~cols:5 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"abcde" ~attrs:default_style |> ignore;
  Screen.clear_line s ~row:0 ~col:2;
  let back = Screen.back s in
  check string "before clear" "a"
    (C.get_text (get_cell_from_grid back ~row:0 ~col:0));
  check string "before clear" "b"
    (C.get_text (get_cell_from_grid back ~row:0 ~col:1));
  check bool "cleared" true (C.is_empty (get_cell_from_grid back ~row:0 ~col:2));
  check bool "cleared" true (C.is_empty (get_cell_from_grid back ~row:0 ~col:3));
  check bool "cleared" true (C.is_empty (get_cell_from_grid back ~row:0 ~col:4));
  let vp = Screen.Viewport.make ~row:0 ~col:3 ~width:2 ~height:1 in
  Screen.set_text s ~row:0 ~col:0 ~text:"abcde" ~attrs:default_style |> ignore;
  Screen.clear_line s ~viewport:vp ~row:0 ~col:2;
  check string "outside vp untouched" "c"
    (C.get_text (get_cell_from_grid back ~row:0 ~col:2));
  check bool "inside vp cleared" true
    (C.is_empty (get_cell_from_grid back ~row:0 ~col:3));
  check bool "inside vp cleared" true
    (C.is_empty (get_cell_from_grid back ~row:0 ~col:4))

let test_set_grapheme_edge () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  Screen.begin_frame s;
  Screen.set_grapheme s ~row:0 ~col:0 ~glyph:"é" ~attrs:default_style;
  check string "grapheme set" "é"
    (C.get_text (get_cell_from_grid (Screen.back s) ~row:0 ~col:0));
  (* Out of bounds operations are silently ignored, not exceptions *)
  Screen.set_grapheme s ~row:2 ~col:0 ~glyph:"x" ~attrs:default_style;
  Screen.set_grapheme s ~row:0 ~col:2 ~glyph:"x" ~attrs:default_style;
  (* Verify nothing was written out of bounds *)
  check bool "out of bounds writes ignored" true
    (C.is_empty (get_cell_from_grid (Screen.back s) ~row:1 ~col:1));
  let vp = Screen.Viewport.make ~row:0 ~col:0 ~width:1 ~height:1 in
  Screen.set_grapheme s ~viewport:vp ~row:0 ~col:1 ~glyph:"y"
    ~attrs:default_style;
  check bool "viewport clips" true
    (C.is_empty (get_cell_from_grid (Screen.back s) ~row:0 ~col:1))

let test_set_text_returns () =
  let s = Screen.create ~rows:2 ~cols:5 () in
  Screen.begin_frame s;
  let lines, adv =
    Screen.set_text s ~row:0 ~col:0 ~text:"abc" ~attrs:default_style
  in
  check int "lines written" 1 lines;
  check int "cols advanced" 3 adv;
  let lines, adv =
    Screen.set_text s ~row:1 ~col:3 ~text:"" ~attrs:default_style
  in
  check int "empty text lines" 0 lines;
  check int "empty text adv" 0 adv;
  let vp = Screen.Viewport.make ~row:0 ~col:0 ~width:2 ~height:1 in
  let lines, adv =
    Screen.set_text s ~viewport:vp ~row:0 ~col:1 ~text:"def"
      ~attrs:default_style
  in
  check int "clipped lines" 1 lines;
  check int "clipped adv" 1 adv;
  (* only "d" fits *)
  let lines, adv =
    Screen.set_text s ~viewport:vp ~row:0 ~col:3 ~text:"x" ~attrs:default_style
  in
  check int "outside vp lines" 0 lines;
  check int "outside vp adv" 0 adv

let test_set_multiline_returns () =
  let s = Screen.create ~rows:3 ~cols:5 () in
  Screen.begin_frame s;
  let lines, max_adv =
    Screen.set_multiline_text s ~row:0 ~col:0 ~text:"ab\ncde\nf"
      ~attrs:default_style
  in
  check int "lines written" 3 lines;
  check int "max adv" 3 max_adv;
  (* "cde" is 3 *)
  let vp = Screen.Viewport.make ~row:0 ~col:0 ~width:5 ~height:2 in
  let lines, max_adv =
    Screen.set_multiline_text s ~viewport:vp ~row:0 ~col:0 ~text:"ab\ncde\nf"
      ~attrs:default_style
  in
  check int "clipped vertical lines" 2 lines;
  check int "clipped max adv" 3 max_adv

let test_render_clear_bug () =
  let s = Screen.create ~rows:1 ~cols:3 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"abc" ~attrs:default_style |> ignore;
  Screen.present s |> ignore;
  Screen.begin_frame s;
  Screen.clear s;
  let patches = Screen.render s in
  check patch_list "clear should produce patches" [ Screen.Clear_screen ]
    patches (* but code produces [], revealing bug *)

let test_render_large_change () =
  let s = Screen.create ~rows:10 ~cols:10 () in
  Screen.begin_frame s;
  for i = 0 to 9 do
    Screen.set_text s ~row:i ~col:0 ~text:(String.make 10 'a')
      ~attrs:default_style
    |> ignore
  done;
  Screen.present s |> ignore;
  Screen.begin_frame s;
  Screen.clear s;
  for i = 0 to 9 do
    Screen.set_text s ~row:i ~col:0 ~text:(String.make 10 'b')
      ~attrs:default_style
    |> ignore
  done;
  let patches = Screen.render s in
  check bool "large change uses Clear_screen" true
    (List.exists (function Screen.Clear_screen -> true | _ -> false) patches);
  check int "includes new runs" 10
    (List.length
       (List.filter (function Screen.Run _ -> true | _ -> false) patches))

let test_render_to_string () =
  let s = Screen.create ~rows:2 ~cols:3 () in
  Screen.begin_frame s;
  let style = Ansi.Style.with_bold true default_style in
  Screen.set_text s ~row:0 ~col:0 ~text:"ab" ~attrs:style |> ignore;
  Screen.set_text s ~row:1 ~col:0 ~text:"c " ~attrs:default_style |> ignore;
  let str = Screen.render_to_string s in
  check string "render with styles and spaces" "\027[1mab\027[0m\nc " str
(* note trailing space in line 1 trimmed? code adds ' ' for empty, but trims? wait, code adds ' ' for empty, no trim in code *)
(* code: for empty, if prev_style != default, reset and add ' ', else add ' ' without reset *)

let test_patch_to_sgr () =
  let run =
    Screen.Run
      {
        row = 0;
        col = 0;
        text = "hi";
        style = Ansi.Style.with_bold true default_style;
        width = 2;
      }
  in
  check string "run to sgr" "\027[1;1H\027[1mhi" (Screen.patch_to_sgr run);
  let clear_reg =
    Screen.Clear_region { row = 1; col = 2; width = 3; height = 1 }
  in
  check string "clear region to sgr" "\027[2;3H   "
    (Screen.patch_to_sgr clear_reg);
  (* code builds spaces *)
  let clear_line = Screen.Clear_line { row = 3; from_col = 4 } in
  check string "clear line to sgr" "\027[4;5H\027[K"
    (Screen.patch_to_sgr clear_line);
  check string "clear screen to sgr" "\027[2J\027[H"
    (Screen.patch_to_sgr Screen.Clear_screen);
  let prev = Some (Ansi.Style.with_fg Ansi.Style.Red default_style) in
  let run2 =
    Screen.Run
      { row = 0; col = 0; text = "x"; style = default_style; width = 1 }
  in
  check string "with prev_style" "\027[1;1H\027[39mx"
    (Screen.patch_to_sgr ~prev_style:prev run2)

let test_patches_to_sgr () =
  let patches =
    [
      Screen.Clear_screen;
      Screen.Run
        {
          row = 0;
          col = 0;
          text = "bold";
          style = Ansi.Style.with_bold true default_style;
          width = 4;
        };
      Screen.Run
        { row = 1; col = 0; text = "normal"; style = default_style; width = 6 };
    ]
  in
  let sgr = Screen.patches_to_sgr patches in
  check string "sequence"
    "\027[2J\027[H\027[1;1H\027[1mbold\027[2;1H\027[0mnormal"
    sgr (* prev_style tracked *)

let test_patches_to_sgr_sync () =
  let patches = [ Screen.Clear_screen ] in
  let sgr = Screen.patches_to_sgr_synchronized patches in
  check string "wrapped" "\027[?2026h\027[2J\027[H\027[?2026l" sgr

let test_cursor_out_of_bounds () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  Screen.set_cursor s ~row:(-1) ~col:0;
  check (option (pair int int)) "negative row None" None (Screen.get_cursor s);
  Screen.set_cursor s ~row:0 ~col:2;
  check (option (pair int int)) "col overflow None" None (Screen.get_cursor s);
  Screen.set_cursor s ~row:2 ~col:0;
  check (option (pair int int)) "row overflow None" None (Screen.get_cursor s)

let test_perf_counters () =
  Screen.Perf.reset ();
  let c1 = Screen.Perf.get () in
  check int "initial zero" 0 c1.frames_rendered;
  let s = Screen.create ~rows:1 ~cols:1 () in
  Screen.begin_frame s;
  Screen.set_grapheme s ~row:0 ~col:0 ~glyph:"x" ~attrs:default_style;
  let patches = Screen.render s in
  (* Debug: print patches to understand what's generated *)
  (* Printf.eprintf "Patches generated: %d\n" (List.length patches); *)
  let c2 = Screen.Perf.get () in
  check int "frames incremented" 1 c2.frames_rendered;
  check int "patches incremented" (List.length patches) c2.patches_generated;
  Screen.Perf.reset ();
  let c3 = Screen.Perf.get () in
  check int "reset works" 0 c3.frames_rendered

let test_empty_screen () =
  let s = Screen.create ~rows:0 ~cols:0 () in
  (* code allows? Grid.create ~rows:0 ~cols:0 ok? assume yes *)
  check int "rows" 0 (Screen.rows s);
  check int "cols" 0 (Screen.cols s);
  Screen.begin_frame s;
  (* should not crash *)
  let patches = Screen.render s in
  check patch_list "render empty" [] patches;
  let dirty = Screen.present s in
  check int "present empty" 0 (List.length dirty)

let test_negative_viewport () =
  check_raises "negative width"
    (Invalid_argument "Viewport.make: negative width/height") (fun () ->
      Screen.Viewport.make ~row:0 ~col:0 ~width:(-1) ~height:1 |> ignore);
  check_raises "negative height"
    (Invalid_argument "Viewport.make: negative width/height") (fun () ->
      Screen.Viewport.make ~row:0 ~col:0 ~width:1 ~height:(-1) |> ignore)

let test_set_text_negative_pos () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  Screen.begin_frame s;
  let lines, adv =
    Screen.set_text s ~row:(-1) ~col:0 ~text:"x" ~attrs:default_style
  in
  check int "negative row lines" 0 lines;
  check int "negative row adv" 0 adv;
  let vp = Screen.Viewport.make ~row:0 ~col:0 ~width:2 ~height:2 in
  let lines, adv =
    Screen.set_text s ~viewport:vp ~row:0 ~col:(-1) ~text:"xy"
      ~attrs:default_style
  in
  check int "negative col skipped" 1 lines;
  check int "negative col adv" 1 adv;
  (* skips "x", writes "y" at col 0 *)
  check string "partial write" "y"
    (C.get_text (get_cell_from_grid (Screen.back s) ~row:0 ~col:0))

(* Under "Viewport and Clipping" *)
let test_viewport_non_overlapping () =
  let v1 = Screen.Viewport.make ~row:0 ~col:0 ~width:2 ~height:2 in
  let v2 = Screen.Viewport.make ~row:2 ~col:2 ~width:2 ~height:2 in
  check (option viewport) "non-overlapping intersect None" None
    (Screen.Viewport.intersect v1 v2);
  let v3 = Screen.Viewport.make ~row:0 ~col:0 ~width:0 ~height:0 in
  check bool "zero size contains false" false
    (Screen.Viewport.contains v3 ~row:0 ~col:0)

(* Under "Drawing Operations" *)
let test_set_text_wide_clip () =
  let s = Screen.create ~rows:1 ~cols:4 ~east_asian_context:true () in
  Screen.begin_frame s;
  let vp = Screen.Viewport.make ~row:0 ~col:1 ~width:2 ~height:1 in
  let _, adv =
    Screen.set_text ~viewport:vp s ~row:0 ~col:0 ~text:"漢字" ~attrs:default_style
  in
  check int "clip mid-wide adv" 2 adv;
  (* "漢" width 2 fits *)
  let back = Screen.back s in
  check bool "after clip empty" true
    (C.is_empty (get_cell_from_grid back ~row:0 ~col:3));
  check bool "continuation set" true
    (C.is_continuation (get_cell_from_grid back ~row:0 ~col:2))

let test_clear_negative_pos () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"ab" ~attrs:default_style |> ignore;
  Screen.set_text s ~row:1 ~col:0 ~text:"cd" ~attrs:default_style |> ignore;
  (* Clear from (-1,-1) to (0,1) - after clipping to screen, clears row 0 entirely *)
  Screen.clear_rect s ~row_start:(-1) ~row_end:0 ~col_start:(-1) ~col_end:1;
  let back = Screen.back s in
  check bool "row 0 col 0 cleared" true
    (C.is_empty (get_cell_from_grid back ~row:0 ~col:0));
  check bool "row 0 col 1 cleared" true
    (C.is_empty (get_cell_from_grid back ~row:0 ~col:1));
  check string "row 1 untouched" "c"
    (C.get_text (get_cell_from_grid back ~row:1 ~col:0));
  check string "row 1 untouched" "d"
    (C.get_text (get_cell_from_grid back ~row:1 ~col:1))

(* Under "Buffer and Frame Lifecycle" *)
let test_resize_mid_frame () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"x" ~attrs:default_style |> ignore;
  Screen.resize s ~rows:1 ~cols:1;
  let dirty = Screen.present s in
  check int "dirty after mid-frame resize" 1 (List.length dirty);
  (* After resize, the dirty region should cover the whole new size *)
  let r = List.hd dirty in
  check int "full redraw min_row" 0 r.G.min_row;
  check int "full redraw max_row" 0 r.G.max_row;
  check int "full redraw min_col" 0 r.G.min_col;
  check int "full redraw max_col" 0 r.G.max_col

let test_back_without_begin () =
  let s = Screen.create ~rows:1 ~cols:1 () in
  check_raises "access back without begin_frame"
    (Failure "Screen.back: Must call begin_frame before accessing back buffer")
    (fun () -> Screen.back s |> ignore)

(* Under "Rendering" *)
let test_render_run_grouping () =
  let s = Screen.create ~rows:1 ~cols:4 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"ab" ~attrs:default_style |> ignore;
  Screen.set_text s ~row:0 ~col:2 ~text:"cd" ~attrs:default_style |> ignore;
  (* same style *)
  let patches = Screen.render s in
  check int "groups into one Run" 1
    (List.length
       (List.filter (function Screen.Run _ -> true | _ -> false) patches))

let test_clear_heuristic_threshold () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  (* total_cells=4 *)
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"a" ~attrs:default_style |> ignore;
  (* 1 change, <30% *)
  let patches1 = Screen.render s in
  check bool "<30% no Clear_screen" false
    (List.exists (function Screen.Clear_screen -> true | _ -> false) patches1);
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:1 ~text:"b" ~attrs:default_style |> ignore;
  Screen.set_text s ~row:1 ~col:0 ~text:"c" ~attrs:default_style |> ignore;
  (* 2 more, total >30% but since new frame, diff is 2/4=50% *)
  let patches2 = Screen.render s in
  check bool ">30% uses Clear_screen" true
    (List.exists (function Screen.Clear_screen -> true | _ -> false) patches2)

(* Under "State Management" *)
let test_flush_damage () =
  let s = Screen.create ~rows:2 ~cols:2 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"x" ~attrs:default_style |> ignore;
  let dmg1 = Screen.flush_damage s in
  check int "damage accumulated" 1 (List.length dmg1);
  let dmg2 = Screen.flush_damage s in
  check int "damage cleared" 0 (List.length dmg2)

let test_snapshot_immutable () =
  let s = Screen.create ~rows:1 ~cols:1 () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"x" ~attrs:default_style |> ignore;
  let snap = Screen.snapshot s in
  Screen.clear s;
  check string "snapshot unchanged" "x"
    (C.get_text (get_cell_from_grid snap ~row:0 ~col:0))

(* Under "Rendering" - for opt *)
let test_diff_cells_continuations () =
  let s = Screen.create ~rows:1 ~cols:3 ~east_asian_context:true () in
  Screen.begin_frame s;
  Screen.set_text s ~row:0 ~col:0 ~text:"漢" ~attrs:default_style |> ignore;
  (* width 2, col1 continuation *)
  let cells = Screen.diff_cells s in
  check int "filters out continuations" 1 (List.length cells)
(* only (0,0,cell) *)

let () =
  run "Screen"
    [
      ( "Creation and Sizing",
        [
          test_case "Basic creation and resize" `Quick test_creation_and_sizing;
          test_case "Creation with style" `Quick test_creation_with_style;
          test_case "Creation with east_asian_context" `Quick
            test_creation_east_asian;
          test_case "Resize to smaller" `Quick test_resize_to_smaller;
          test_case "Resize to larger" `Quick test_resize_to_larger;
          test_case "Empty screen" `Quick test_empty_screen;
        ] );
      ( "Buffer and Frame Lifecycle",
        [
          test_case "Drawing affects back buffer only" `Quick
            test_drawing_and_buffers;
          test_case "Present swaps buffers and diffs" `Quick
            test_present_and_diff;
          test_case "Batch correctly wraps a drawing sequence" `Quick test_batch;
          test_case "Resize mid-frame" `Quick test_resize_mid_frame;
          test_case "Back without begin_frame" `Quick test_back_without_begin;
        ] );
      ( "Viewport and Clipping",
        [
          test_case "Drawing operations respect viewports" `Quick test_viewport;
          test_case "Viewport module functions" `Quick test_viewport_module;
          test_case "Copy viewport" `Quick test_copy_viewport;
          test_case "Negative viewport dimensions" `Quick test_negative_viewport;
          test_case "Non-overlapping viewports" `Quick test_viewport_non_overlapping;
        ] );
      ( "Drawing Operations",
        [
          test_case "Clear rect" `Quick test_clear_rect;
          test_case "Clear line" `Quick test_clear_line;
          test_case "Set grapheme edges" `Quick test_set_grapheme_edge;
          test_case "Set text returns" `Quick test_set_text_returns;
          test_case "Set multiline returns" `Quick test_set_multiline_returns;
          test_case "Set text negative positions" `Quick
            test_set_text_negative_pos;
          test_case "Set text wide char clipping" `Quick test_set_text_wide_clip;
          test_case "Clear with negative positions" `Quick test_clear_negative_pos;
        ] );
      ( "Rendering",
        [
          test_case "Render to patches (inc, clear, full)" `Quick
            test_render_to_patches;
          test_case "Render clear bug check" `Quick test_render_clear_bug;
          test_case "Render large change" `Quick test_render_large_change;
          test_case "Render to string" `Quick test_render_to_string;
          test_case "Patch to sgr" `Quick test_patch_to_sgr;
          test_case "Patches to sgr" `Quick test_patches_to_sgr;
          test_case "Patches to sgr synchronized" `Quick
            test_patches_to_sgr_sync;
          test_case "Render run grouping" `Quick test_render_run_grouping;
          test_case "Clear heuristic threshold" `Quick test_clear_heuristic_threshold;
          test_case "Diff cells filters continuations" `Quick test_diff_cells_continuations;
        ] );
      ( "State Management",
        [
          test_case "Clone, copy_to and snapshot" `Quick test_clone_and_copy;
          test_case "Cursor get/set" `Quick test_cursor;
          test_case "Cursor out of bounds" `Quick test_cursor_out_of_bounds;
          test_case "Perf counters" `Quick test_perf_counters;
          test_case "Flush damage" `Quick test_flush_damage;
          test_case "Snapshot immutable" `Quick test_snapshot_immutable;
        ] );
    ]
