let rows, cols = (24, 80 (* Standard terminal size *))
let large_rows, large_cols = (100, 200 (* Larger for stress testing *))

let random_style () =
  Ansi.Style.make ~bold:(Random.bool ()) ~italic:(Random.bool ())
    ~underline:(Random.bool ())
    ~fg:(Ansi.Style.Index (Random.int 256))
    ~bg:(Ansi.Style.Index (Random.int 256))
    ()

let random_text len = String.init len (fun _ -> Char.chr (32 + Random.int 95))

let full_screen_text () =
  String.concat "\n" (List.init rows (fun _ -> random_text cols))

(* Group 1: Creation and Resizing *)
let bench_create () = ignore (Screen.create ~rows ~cols ())

let bench_resize s =
  Screen.resize s ~rows:large_rows ~cols:large_cols;
  Screen.resize s ~rows ~cols

(* Group 2: Drawing Operations *)
let bench_clear s = Screen.clear s

let bench_set_grapheme s =
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      Screen.set_grapheme s ~row:r ~col:c ~glyph:"A" ~attrs:(random_style ())
    done
  done

let bench_set_text s =
  for r = 0 to rows - 1 do
    ignore
      (Screen.set_text s ~row:r ~col:0 ~text:(random_text cols)
         ~attrs:(random_style ()))
  done

let bench_set_multiline_text s =
  ignore
    (Screen.set_multiline_text s ~row:0 ~col:0 ~text:(full_screen_text ())
       ~attrs:(random_style ()))

(* Group 3: Frame Lifecycle *)
let bench_begin_present s =
  Screen.begin_frame s;
  bench_set_text s;
  (* Simulate drawing *)
  ignore (Screen.present s)

let bench_batch s = Screen.batch s (fun s' -> bench_set_multiline_text s')

(* Group 4: Diffing and Rendering *)
let bench_diff_cells s =
  Screen.begin_frame s;
  bench_set_grapheme s;
  (* Maximize differences *)
  ignore (Screen.diff_cells s)

let bench_render s =
  Screen.begin_frame s;
  bench_set_text s;
  ignore (Screen.render s)

let bench_render_to_string s =
  Screen.begin_frame s;
  bench_set_multiline_text s;
  ignore (Screen.render_to_string s)

let bench_patches_to_sgr s =
  Screen.begin_frame s;
  bench_set_text s;
  let patches = Screen.render s in
  ignore (Screen.patches_to_sgr patches)

(* Group 5: Viewport Operations *)
let bench_with_viewport s =
  let vp =
    Screen.Viewport.make ~row:0 ~col:0 ~width:(cols / 2) ~height:(rows / 2)
  in
  Screen.with_viewport s vp (fun s' -> bench_set_multiline_text s')

let bench_render_viewport s =
  let vp = Screen.Viewport.full ~rows ~cols in
  ignore (Screen.render_viewport s vp)

let bench_copy_viewport s_src s_dst =
  let vp = Screen.Viewport.full ~rows ~cols in
  Screen.copy_viewport ~src:s_src ~dst:s_dst ~src_viewport:vp ~dst_row:0
    ~dst_col:0

(* Group 6: Cloning and Copying *)
let bench_clone s = ignore (Screen.clone s)
let bench_copy_to s_src s_dst = Screen.copy_to ~src:s_src ~dst:s_dst

(* Setup for benchmarks requiring screens *)
let setup_screen () =
  let s = Screen.create ~rows ~cols () in
  Screen.batch s (fun s' -> bench_set_multiline_text s');
  s

let () =
  let open Ubench in
  let benches =
    [
      group "Creation/Resizing"
        [
          create "create" bench_create;
          create_with_setup "resize" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_resize;
        ];
      group "Drawing"
        [
          create_with_setup "clear" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_clear;
          create_with_setup "set_grapheme" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_set_grapheme;
          create_with_setup "set_text" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_set_text;
          create_with_setup "set_multiline_text" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_set_multiline_text;
        ];
      group "Frame Lifecycle"
        [
          create_with_setup "begin_present" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_begin_present;
          create_with_setup "batch" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_batch;
        ];
      group "Diffing/Rendering"
        [
          create_with_setup "diff_cells" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_diff_cells;
          create_with_setup "render" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_render;
          create_with_setup "render_to_string" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_render_to_string;
          create_with_setup "patches_to_sgr" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_patches_to_sgr;
        ];
      group "Viewport"
        [
          create_with_setup "with_viewport" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_with_viewport;
          create_with_setup "render_viewport" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_render_viewport;
          create_with_setup "copy_viewport"
            ~setup:(fun () -> (setup_screen (), setup_screen ()))
            ~teardown:(fun _ -> ())
            ~f:(fun (src, dst) -> bench_copy_viewport src dst);
        ];
      group "Cloning/Copying"
        [
          create_with_setup "clone" ~setup:setup_screen
            ~teardown:(fun _ -> ())
            ~f:bench_clone;
          create_with_setup "copy_to"
            ~setup:(fun () -> (setup_screen (), setup_screen ()))
            ~teardown:(fun _ -> ())
            ~f:(fun (src, dst) -> bench_copy_to src dst);
        ];
    ]
  in
  run_cli benches
