module C = Ansi.Color
module S = Ansi.Style

let grid_width = 160
let grid_height = 48
let solid_fill = C.of_rgb 28 32 36
let translucent_fill = C.of_rgba 64 128 224 110

let ascii_style =
  S.make ~fg:(C.of_rgb 232 232 232) ~bg:(C.of_rgb 18 20 24) ~bold:true ()

let make_ascii_line len =
  String.init len (fun i -> Char.chr (Char.code 'a' + (i mod 26)))

let ascii_line = make_ascii_line (grid_width - 2)

let emoji_line =
  (* Mixed emoji payload to stress grapheme/width handling *)
  let fragments = [| "👩‍🚀"; "🛰️"; "🌌"; "✨"; "🚀"; "🪐"; "🌠"; "👨‍💻" |] in
  let buf = Buffer.create (Array.length fragments * 32) in
  for i = 0 to 31 do
    Buffer.add_string buf fragments.(i land 7)
  done;
  Buffer.contents buf

let scroll_seed =
  (* Representative line for terminal scroll / log output *)
  let base = "Matrix terminal benchmark line " in
  let target = grid_width in
  let buf = Buffer.create (String.length base * 4) in
  while Buffer.length buf < target do
    Buffer.add_string buf base
  done;
  let contents = Buffer.contents buf in
  String.sub contents 0 target

let make_grid ?(respect_alpha = false) () =
  Grid.create ~width:grid_width ~height:grid_height ~respect_alpha ()

(* Bulk fills: opaque & translucent overlays *)

let fill_rect_opaque_full =
  Ubench.create_with_setup "grid.fill_rect/opaque-full"
    ~setup:(fun () -> make_grid ())
    ~teardown:(fun _ -> ())
    ~f:(fun grid ->
      Grid.fill_rect grid ~x:0 ~y:0 ~width:grid_width ~height:grid_height
        ~color:solid_fill)

let fill_rect_translucent_overlay =
  Ubench.create_with_setup "grid.fill_rect/translucent-overlay"
    ~setup:(fun () -> make_grid ~respect_alpha:true ())
    ~teardown:(fun _ -> ())
    ~f:(fun grid ->
      Grid.fill_rect grid ~x:0 ~y:0 ~width:grid_width ~height:grid_height
        ~color:translucent_fill)

(* Full-screen text: ASCII vs emoji-heavy *)

let draw_text_ascii_full =
  Ubench.create_with_setup "grid.draw_text/ascii-full-screen"
    ~setup:(fun () ->
      let grid = make_grid () in
      Grid.clear grid ~color:(C.of_rgb 0 0 0);
      grid)
    ~teardown:(fun _ -> ())
    ~f:(fun grid ->
      (* Typical "code editor" / log viewer workload: full ASCII text. *)
      for row = 0 to grid_height - 1 do
        Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:ascii_line
      done)

let draw_text_emoji_full =
  Ubench.create_with_setup "grid.draw_text/emoji-full-screen"
    ~setup:(fun () ->
      let grid = make_grid () in
      Grid.clear grid ~color:(C.of_rgb 0 0 0);
      grid)
    ~teardown:(fun _ -> ())
    ~f:(fun grid ->
      (* Emoji-heavy / chat-like workload: full screen mixed-width graphemes. *)
      for row = 0 to grid_height - 1 do
        Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:emoji_line
      done)

(* Scrolling region: terminal-like scrollback *)

let scroll_terminal_region =
  let top = 0 in
  let bottom = grid_height - 2 in
  let iterations = 20 in
  Ubench.create_with_setup "grid.scroll/terminal-region"
    ~setup:(fun () ->
      let grid = make_grid () in
      Grid.clear grid ~color:(C.of_rgb 0 0 0);
      for row = 0 to grid_height - 1 do
        Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:scroll_seed
      done;
      grid)
    ~teardown:(fun _ -> ())
    ~f:(fun grid ->
      (* Simulate a burst of log lines arriving. *)
      for _ = 1 to iterations do
        Grid.scroll_up grid ~top ~bottom ~n:1;
        Grid.draw_text ~style:ascii_style grid ~x:0 ~y:bottom ~text:scroll_seed
      done)

(* Partial updates: status line + sparse cells (cursor/status) *)

let partial_status_line =
  let status_text_1 =
    "matrix.ml  [NORMAL]  line 42, col 7   3 warnings (F5: build)"
  in
  let status_text_2 =
    "matrix.ml  [INSERT]  line 42, col 9   modified   (Ctrl+S: save)"
  in
  let toggle = ref false in
  let grid_ref = ref None in
  Ubench.create_with_setup "grid.partial_update/status-line"
    ~setup:(fun () ->
      let grid =
        match !grid_ref with
        | Some g -> g
        | None ->
            let g = make_grid () in
            Grid.clear g ~color:(C.of_rgb 0 0 0);
            (* Fill main area once with ASCII text to approximate editor body. *)
            for row = 0 to grid_height - 2 do
              Grid.draw_text ~style:ascii_style g ~x:0 ~y:row ~text:ascii_line
            done;
            grid_ref := Some g;
            g
      in
      toggle := false;
      grid)
    ~teardown:(fun _ -> ())
    ~f:(fun grid ->
      toggle := not !toggle;
      let status_bg = C.of_rgb 200 200 200 in
      let status_fg = C.of_rgb 0 0 0 in
      let status_style = S.make ~fg:status_fg ~bg:status_bg ~bold:true () in
      let y = grid_height - 1 in
      let text = if !toggle then status_text_1 else status_text_2 in
      Grid.draw_text ~style:status_style grid ~x:0 ~y ~text)

let partial_update_sparse_cells =
  (* Cursor location + three status bar "slots" across the top row. *)
  let update_positions =
    [| (40, 12); (0, 0); (grid_width / 2, 0); (grid_width - 1, 0) |]
  in
  let grid_ref = ref None in
  Ubench.create_with_setup "grid.partial_update/sparse-cells"
    ~setup:(fun () ->
      let grid =
        match !grid_ref with
        | Some g -> g
        | None ->
            let g = make_grid () in
            Grid.clear g ~color:(C.of_rgb 0 0 0);
            for row = 0 to grid_height - 1 do
              Grid.draw_text ~style:ascii_style g ~x:0 ~y:row ~text:ascii_line
            done;
            grid_ref := Some g;
            g
      in
      grid)
    ~teardown:(fun _ -> ())
    ~f:(fun grid ->
      (* Small, scattered updates as you'd get from a cursor + tiny UI chrome. *)
      for i = 0 to Array.length update_positions - 1 do
        let x, y = update_positions.(i) in
        Grid.set_cell_alpha grid ~x ~y
          ~code:(Char.code 'A' + i)
          ~fg:(C.of_rgb 255 255 0) ~bg:(C.of_rgb 0 0 0) ~attrs:Ansi.Attr.empty
          ()
      done)

(* Group + entry point *)

let benchmarks =
  [
    (* Bulk operations *)
    fill_rect_opaque_full;
    fill_rect_translucent_overlay;
    (* Full-screen text workloads *)
    draw_text_ascii_full;
    draw_text_emoji_full;
    (* Terminal-style scrollback *)
    scroll_terminal_region;
    (* Fine-grained partial updates *)
    partial_status_line;
    partial_update_sparse_cells;
  ]
  |> Ubench.group "grid"

let () = Ubench.run_cli [ benchmarks ]
