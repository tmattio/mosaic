module G = Grid
module C = G.Cell
open Ubench

let cols = 80
let rows = 24
let large_cols = 1000
let large_rows = 1000
let text_short = "Hello"
let text_long = String.make cols 'A'
let style = Ansi.Style.make ~fg:Ansi.Style.Red ~bg:Ansi.Style.Blue ~bold:true ()

let wide_grapheme =
  "\xF0\x9F\x91\xA8\xE2\x80\x8D\xF0\x9F\x91\xA9\xE2\x80\x8D\xF0\x9F\x91\xA6"
(* Family emoji *)

let rect_small = { G.row = 0; col = 0; width = 10; height = 5 }
let rect_large = { G.row = 0; col = 0; width = cols; height = rows }

let bench_creation () =
  let _ = G.create ~rows ~cols () in
  ()

let bench_creation_large () =
  let _ = G.create ~rows:large_rows ~cols:large_cols () in
  ()

let bench_set_cell grid () =
  G.set grid ~row:0 ~col:0
    (Some (C.make_glyph ~style ~east_asian_context:false "A"))

let bench_set_grapheme_narrow grid () =
  G.set_grapheme grid ~row:0 ~col:0 ~glyph:"A" ~attrs:style

let bench_set_grapheme_wide grid () =
  G.set_grapheme grid ~row:0 ~col:0 ~glyph:wide_grapheme ~attrs:style

let bench_set_text_short grid () =
  let _ = G.set_text grid ~row:0 ~col:0 ~text:text_short ~attrs:style in
  ()

let bench_set_text_long grid () =
  let _ = G.set_text grid ~row:0 ~col:0 ~text:text_long ~attrs:style in
  ()

let bench_clear grid () = G.clear grid
let bench_clear_line grid () = G.clear_line grid 0 0

let bench_clear_rect grid () =
  G.clear_rect grid ~row_start:0 ~row_end:(rows - 1) ~col_start:0
    ~col_end:(cols - 1)

let bench_resize_larger grid () = G.resize grid ~rows:(rows * 2) ~cols:(cols * 2)

let bench_resize_smaller grid () =
  G.resize grid ~rows:(rows / 2) ~cols:(cols / 2)

let bench_blit_small src dst () =
  G.blit ~src ~src_rect:rect_small ~dst ~dst_pos:(0, 0)

let bench_blit_large src dst () =
  G.blit ~src ~src_rect:rect_large ~dst ~dst_pos:(0, 0)

let bench_diff_no_change prev curr () =
  let _ = G.diff prev curr in
  ()

let bench_diff_single_change prev curr () =
  G.set curr ~row:0 ~col:0
    (Some (C.make_glyph ~style ~east_asian_context:false "B"));
  let _ = G.diff prev curr in
  ()

let bench_diff_row_change prev curr () =
  for c = 0 to cols - 1 do
    G.set curr ~row:0 ~col:c
      (Some (C.make_glyph ~style ~east_asian_context:false "B"))
  done;
  let _ = G.diff prev curr in
  ()

let bench_diff_full_change prev curr () =
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      G.set curr ~row:r ~col:c
        (Some (C.make_glyph ~style ~east_asian_context:false "B"))
    done
  done;
  let _ = G.diff prev curr in
  ()

let bench_scroll grid () =
  let src_rect = { G.row = 1; col = 0; width = cols; height = rows - 1 } in
  G.blit ~src:grid ~src_rect ~dst:grid ~dst_pos:(0, 0);
  G.clear_line grid (rows - 1) 0

let bench_insert_line grid () =
  let src_rect = { G.row = 5; col = 0; width = cols; height = rows - 6 } in
  G.blit ~src:grid ~src_rect ~dst:grid ~dst_pos:(6, 0);
  G.clear_line grid 5 0

let bench_typing grid () =
  for i = 0 to 10 do
    let _ = G.set_text grid ~row:0 ~col:i ~text:"A" ~attrs:style in
    ()
  done

let creation_benches =
  [
    create "create_small" bench_creation;
    create "create_large" bench_creation_large;
  ]

let setting_benches =
  [
    create_with_setup "set_cell"
      ~setup:(fun () -> G.create ~rows ~cols ())
      ~teardown:(fun _ -> ())
      ~f:bench_set_cell;
    create_with_setup "set_grapheme_narrow"
      ~setup:(fun () -> G.create ~rows ~cols ())
      ~teardown:(fun _ -> ())
      ~f:bench_set_grapheme_narrow;
    create_with_setup "set_grapheme_wide"
      ~setup:(fun () -> G.create ~rows ~cols ())
      ~teardown:(fun _ -> ())
      ~f:bench_set_grapheme_wide;
    create_with_setup "set_text_short"
      ~setup:(fun () -> G.create ~rows ~cols ())
      ~teardown:(fun _ -> ())
      ~f:bench_set_text_short;
    create_with_setup "set_text_long"
      ~setup:(fun () -> G.create ~rows ~cols ())
      ~teardown:(fun _ -> ())
      ~f:bench_set_text_long;
  ]

let clearing_benches =
  [
    create_with_setup "clear"
      ~setup:(fun () -> G.create ~rows ~cols ())
      ~teardown:(fun _ -> ())
      ~f:bench_clear;
    create_with_setup "clear_line"
      ~setup:(fun () -> G.create ~rows ~cols ())
      ~teardown:(fun _ -> ())
      ~f:bench_clear_line;
    create_with_setup "clear_rect"
      ~setup:(fun () -> G.create ~rows ~cols ())
      ~teardown:(fun _ -> ())
      ~f:bench_clear_rect;
  ]

let resizing_benches =
  [
    create_with_setup "resize_larger"
      ~setup:(fun () -> G.create ~rows ~cols ())
      ~teardown:(fun _ -> ())
      ~f:bench_resize_larger;
    create_with_setup "resize_smaller"
      ~setup:(fun () -> G.create ~rows:(rows * 2) ~cols:(cols * 2) ())
      ~teardown:(fun _ -> ())
      ~f:bench_resize_smaller;
  ]

let blitting_benches =
  [
    create_with_setup "blit_small"
      ~setup:(fun () ->
        let src = G.create ~rows ~cols () in
        let dst = G.create ~rows ~cols () in
        (src, dst))
      ~teardown:(fun _ -> ())
      ~f:(fun (src, dst) -> bench_blit_small src dst ());
    create_with_setup "blit_large"
      ~setup:(fun () ->
        let src = G.create ~rows ~cols () in
        let dst = G.create ~rows ~cols () in
        (src, dst))
      ~teardown:(fun _ -> ())
      ~f:(fun (src, dst) -> bench_blit_large src dst ());
  ]

let diffing_benches =
  [
    create_with_setup "diff_no_change"
      ~setup:(fun () ->
        let prev = G.create ~rows ~cols () in
        let curr = G.copy prev in
        (prev, curr))
      ~teardown:(fun _ -> ())
      ~f:(fun (prev, curr) -> bench_diff_no_change prev curr ());
    create_with_setup "diff_single_change"
      ~setup:(fun () ->
        let prev = G.create ~rows ~cols () in
        let curr = G.copy prev in
        (prev, curr))
      ~teardown:(fun _ -> ())
      ~f:(fun (prev, curr) -> bench_diff_single_change prev curr ());
    create_with_setup "diff_row_change"
      ~setup:(fun () ->
        let prev = G.create ~rows ~cols () in
        let curr = G.copy prev in
        (prev, curr))
      ~teardown:(fun _ -> ())
      ~f:(fun (prev, curr) -> bench_diff_row_change prev curr ());
    create_with_setup "diff_full_change"
      ~setup:(fun () ->
        let prev = G.create ~rows ~cols () in
        let curr = G.copy prev in
        (prev, curr))
      ~teardown:(fun _ -> ())
      ~f:(fun (prev, curr) -> bench_diff_full_change prev curr ());
  ]

let composite_benches =
  [
    create_with_setup "scroll"
      ~setup:(fun () -> G.create ~rows ~cols ())
      ~teardown:(fun _ -> ())
      ~f:bench_scroll;
    create_with_setup "insert_line"
      ~setup:(fun () -> G.create ~rows ~cols ())
      ~teardown:(fun _ -> ())
      ~f:bench_insert_line;
    create_with_setup "typing"
      ~setup:(fun () -> G.create ~rows ~cols ())
      ~teardown:(fun _ -> ())
      ~f:bench_typing;
  ]

let all_benches =
  [
    group "creation" creation_benches;
    group "setting" setting_benches;
    group "clearing" clearing_benches;
    group "resizing" resizing_benches;
    group "blitting" blitting_benches;
    group "diffing" diffing_benches;
    group "composite" composite_benches;
  ]

let () = Ubench.run_cli all_benches
