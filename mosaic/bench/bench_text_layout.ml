open Mosaic_ui

let make_surface spans =
  let renderer = Renderer.create () in
  let node = Renderable.create ~parent:(Renderer.root renderer) () in
  let buffer = Text_buffer.create () in
  Text_buffer.set_styled_text buffer spans;
  let surface = Text_surface.create node buffer in
  Text_surface.set_wrap surface `Word;
  (renderer, surface)

let plain =
  [
    { Text_buffer.text = "A short terminal label"; style = Ansi.Style.default };
  ]

let styled =
  List.init 10_000 (fun i ->
      {
        Text_buffer.text = "word text ";
        style =
          (if i mod 2 = 0 then Ansi.Style.default
           else Ansi.Style.make ~bold:true ());
      })

let reflow (renderer, surface) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  Text_surface.set_wrap_width surface (Some 79);
  let a = Text_surface.display_line_count surface in
  Text_surface.set_wrap_width surface (Some 81);
  a + Text_surface.display_line_count surface

let cold_reflow ((_, surface) as input) =
  Text_surface.set_wrap surface `Char;
  Text_surface.set_wrap surface `Word;
  reflow input

let editor_text = String.make 1_000_000 'x'

let edit buffer =
  Edit_buffer.set_cursor buffer 500_000;
  ignore (Edit_buffer.insert buffer "y" : bool);
  let length = Edit_buffer.length buffer in
  ignore (Edit_buffer.undo buffer : bool);
  length

let paint_surface content ~height ~scroll_x =
  let renderer, surface =
    make_surface [ { Text_buffer.text = content; style = Ansi.Style.default } ]
  in
  Text_surface.set_wrap surface `None;
  let node = Text_surface.node surface in
  Renderable.Private.update_layout node ~x:0. ~y:0. ~width:80.
    ~height:(float_of_int height);
  Text_surface.set_scroll_x surface scroll_x;
  let grid = Matrix_grid.create ~width:80 ~height:20 () in
  Renderable.Private.render node grid ~delta:0.;
  (renderer, node, grid)

let paint (renderer, node, grid) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  Renderable.Private.render node grid ~delta:0.

let tall lines = String.concat "" (List.init lines (fun _ -> "one line\n"))

let overlay_revision buffer =
  Text_buffer.clear_highlights buffer;
  for i = 0 to 199 do
    Text_buffer.add_highlight buffer
      (Text_buffer.Highlight.make ~start_offset:i ~end_offset:(400 - i)
         ~style:(Ansi.Style.make ~bold:true ())
         ~ref_id:i ())
  done;
  Text_buffer.style_at buffer 200

let focus_surface length =
  let renderer = Renderer.create () in
  let area =
    Textarea.create ~parent:(Renderer.root renderer)
      ~focused_text_color:Ansi.Color.cyan
      ~state:(Editor.of_string (String.make length 'x'))
      ~on_change:ignore ()
  in
  let node = Textarea.node area in
  Renderable.set_focusable node true;
  Renderable.Private.update_layout node ~x:0. ~y:0. ~width:80. ~height:20.;
  ignore (Text_surface.layout (Textarea.surface area) : Matrix_text.Layout.t);
  (renderer, area)

let focus_layout (renderer, area) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  let node = Textarea.node area and surface = Textarea.surface area in
  ignore (Renderable.focus node : bool);
  let focused = Text_surface.display_line_count surface in
  Renderable.blur node;
  focused + Text_surface.display_line_count surface

let restyle_surface () =
  let renderer, surface = make_surface styled in
  Text_surface.set_wrap_width surface (Some 80);
  ignore (Text_surface.layout surface : Matrix_text.Layout.t);
  let alternate =
    List.map
      (fun (span : Text_buffer.span) ->
        { span with style = Ansi.Style.make ~italic:true () })
      styled
  in
  (renderer, surface, alternate)

let restyle_layout (renderer, surface, alternate) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  let buffer = Text_surface.buffer surface in
  Text_buffer.set_styled_text buffer alternate;
  let first = Text_surface.display_line_count surface in
  Text_buffer.set_styled_text buffer styled;
  first + Text_surface.display_line_count surface

let fragment_text count =
  let renderer = Renderer.create () in
  let text = Text.create ~parent:(Renderer.root renderer) () in
  let first = List.init count (fun _ -> Text.Fragment.text "word ") in
  let second = List.init count (fun _ -> Text.Fragment.text "text ") in
  (renderer, text, first, second)

let replace_fragments (renderer, text, first, second) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  Text.set_fragments text first;
  Text.set_fragments text second

let memory_check () =
  let module L = Matrix_text.Layout in
  Gc.full_major ();
  let before = (Gc.stat ()).live_words in
  let source = L.Source.of_string editor_text in
  let layout = L.create source ~width:1 ~wrap:`Char in
  Gc.full_major ();
  let live = ((Gc.stat ()).live_words - before) * (Sys.word_size / 8) in
  let replacement =
    L.create (L.Source.of_string (editor_text ^ "y")) ~width:1 ~wrap:`Char
  in
  Gc.full_major ();
  let peak = (Gc.stat ()).top_heap_words * (Sys.word_size / 8) in
  Printf.printf "one-source-and-rows=%d bytes; rebuild-heap-peak=%d bytes\n"
    live peak;
  ignore (Sys.opaque_identity (layout, replacement));
  if live > 128 * 1024 * 1024 || peak > 256 * 1024 * 1024 then
    failwith "text layout memory budget exceeded"

let history_check () =
  Gc.full_major ();
  let before = (Gc.stat ()).live_words in
  let buffer = Edit_buffer.create ~max_length:2_000_000 editor_text in
  let old_sources = Weak.create 200 in
  for i = 0 to 199 do
    Weak.set old_sources i (Some (Edit_buffer.source buffer));
    Edit_buffer.set_cursor buffer 500_000;
    ignore (Edit_buffer.insert buffer "y" : bool)
  done;
  Gc.full_major ();
  let live = ((Gc.stat ()).live_words - before) * (Sys.word_size / 8) in
  for i = 0 to 199 do
    if Weak.check old_sources i then
      failwith "undo retained an obsolete source index"
  done;
  Printf.printf "200-version-history-and-current-index=%d bytes\n" live;
  ignore (Sys.opaque_identity buffer)

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "memory" then memory_check ()
  else if Array.length Sys.argv = 2 && Sys.argv.(1) = "history" then
    history_check ()
  else
    Thumper.run "text_layout"
      ~config:
        (Thumper.Config.default
        |> Thumper.Config.metrics
             (if Sys.getenv_opt "TEXT_LAYOUT_TIMING" = Some "1" then
                Thumper.Metric.[ alloc_words; wall_time; promoted_words ]
              else [ Thumper.Metric.alloc_words ]))
      ~budgets:[ Thumper.Budget.no_more_alloc_than 0.05 ]
      [
        Thumper.bench "text/plain-props" (fun () ->
            Text.Props.make ~content:(Thumper.black_box "plain label") ());
        Thumper.bench_with_setup
          ~setup:(fun () -> fragment_text 1_000)
          "fragments/normalize-1k" replace_fragments;
        Thumper.bench_with_setup
          ~setup:(fun () -> fragment_text 10_000)
          "fragments/normalize-10k" replace_fragments;
        Thumper.bench_with_setup
          ~setup:(fun () -> focus_surface 128)
          "focus/layout-128b" focus_layout;
        Thumper.bench_with_setup
          ~setup:(fun () -> focus_surface 1_000_000)
          "focus/layout-1mb" focus_layout;
        Thumper.bench_with_setup ~setup:restyle_surface
          "styles/same-text-layout-100k" restyle_layout;
        Thumper.bench_with_setup
          ~setup:(fun () -> editor_text)
          "index/create-1mb"
          (fun text -> Edit_buffer.create ~max_length:2_000_000 text);
        Thumper.bench_with_setup
          ~setup:(fun () -> Edit_buffer.create ~max_length:2_000_000 "")
          "index/set-text-1mb"
          (fun buffer -> Edit_buffer.set_text buffer editor_text);
        Thumper.bench_with_setup
          ~setup:(fun () -> make_surface plain)
          "reflow/label" reflow;
        Thumper.bench_with_setup
          ~setup:(fun () -> make_surface styled)
          "reflow/styled-100k" reflow;
        Thumper.bench_with_setup
          ~setup:(fun () -> make_surface styled)
          "reflow/styled-100k-cold" cold_reflow;
        Thumper.bench_with_setup
          ~setup:(fun () ->
            Edit_buffer.create ~max_length:2_000_000 editor_text)
          "edit/1mb-middle-undo" edit;
        Thumper.bench_with_setup
          ~setup:(fun () ->
            paint_surface (tall 1_000) ~height:1_000 ~scroll_x:0)
          "paint/tall-1k" paint;
        Thumper.bench_with_setup
          ~setup:(fun () ->
            paint_surface (tall 100_000) ~height:100_000 ~scroll_x:0)
          "paint/tall-100k" paint;
        Thumper.bench_with_setup
          ~setup:(fun () ->
            paint_surface (String.make 100_000 'x') ~height:1 ~scroll_x:99_900)
          "paint/horizontal-100k" paint;
        Thumper.bench_with_setup
          ~setup:(fun () ->
            paint_surface editor_text ~height:1 ~scroll_x:999_900)
          "paint/horizontal-1m" paint;
        Thumper.bench_with_setup
          ~setup:(fun () ->
            let b = Text_buffer.create () in
            Text_buffer.set_text b (String.make 400 'x');
            b)
          "styles/nested-revision-200" overlay_revision;
      ]
