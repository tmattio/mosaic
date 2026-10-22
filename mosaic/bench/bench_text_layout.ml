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

(* Five widths cycled through a cache of four: every step lays out again. *)
let alternating_widths ((renderer, surface), step) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  incr step;
  Text_surface.set_wrap_width surface (Some (76 + (!step mod 5)));
  Text_surface.display_line_count surface

let editor_text = String.make 1_000_000 'x'

let edit editor =
  let edited = Editor.insert "y" editor in
  Editor.length edited + Editor.length (Editor.undo edited)

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

(* ───── Reference paint workloads ─────

   A 80x20 viewport over text whose rows, columns or overlays mostly lie
   outside it. Setup builds and paints once; the measured call repaints the
   unchanged view. *)

let viewport ?(wrap = `None) ?(rows = 20) ?(scroll = `Top) ?(scroll_x = 0)
    ?(setup = ignore) spans =
  let renderer, surface = make_surface spans in
  Text_surface.set_wrap surface wrap;
  setup (Text_surface.buffer surface);
  let node = Text_surface.node surface in
  Renderable.Private.update_layout node ~x:0. ~y:0. ~width:80.
    ~height:(float_of_int rows);
  (match scroll with
  | `Top -> ()
  | `Middle ->
      Text_surface.set_scroll_y surface (Text_surface.max_scroll_y surface / 2));
  Text_surface.set_scroll_x surface scroll_x;
  let grid = Matrix_grid.create ~width:80 ~height:rows () in
  Renderable.Private.render node grid ~delta:0.;
  (renderer, node, grid)

let span text = [ { Text_buffer.text; style = Ansi.Style.default } ]

(* [count] highlights on the first [count] lines of [tall], each with its own
   color, so that none reaches the middle of the text. *)
let hidden_overlays count buffer =
  for i = 0 to count - 1 do
    Text_buffer.add_highlight buffer
      (Text_buffer.Highlight.make ~start_offset:(i * 9)
         ~end_offset:((i * 9) + 4)
         ~style:(Ansi.Style.make ~fg:(Ansi.Color.indexed (i mod 256)) ())
         ~ref_id:i ())
  done

(* [count] nested highlights over the visible text, alternating colors, so
   that every column starts a style run under up to [count] overlays. *)
let dense_overlays count buffer =
  let n = 20 * 81 in
  for i = 0 to count - 1 do
    Text_buffer.add_highlight buffer
      (Text_buffer.Highlight.make ~start_offset:(i * 4)
         ~end_offset:(n - (i * 4))
         ~style:(Ansi.Style.make ~fg:(Ansi.Color.indexed (i mod 256)) ())
         ~priority:i ~ref_id:i ())
  done

let visible_lines =
  String.concat "\n" (List.init 20 (fun _ -> String.make 80 'x'))

(* Allocation of one unchanged repaint, on the real paint path. Paint allocates
   per visited row and per entered style run (each batch it draws copies its
   bytes); a hidden row, column or overlay costs nothing if paint never visits
   it. Equal counts across hidden sizes are therefore the measurement that
   repeated paint does not grow with hidden content. *)
let paint_words (_, node, grid) =
  Renderable.Private.render node grid ~delta:0.;
  let before = Gc.minor_words () in
  Renderable.Private.render node grid ~delta:0.;
  int_of_float (Gc.minor_words () -. before)

let paint_check () =
  let words name setup =
    let w = paint_words (setup ()) in
    Printf.printf "%s=%d words\n" name w;
    w
  in
  let same what a b =
    if a <> b then
      failwith (Printf.sprintf "paint work grows with hidden %s" what)
  in
  same "rows"
    (words "tall-1k-middle" (fun () ->
         viewport ~scroll:`Middle (span (tall 1_000))))
    (words "tall-100k-middle" (fun () ->
         viewport ~scroll:`Middle (span (tall 100_000))));
  same "columns"
    (words "horizontal-100k" (fun () ->
         viewport ~rows:1 ~scroll_x:99_920 (span (String.make 100_000 'x'))))
    (words "horizontal-1m" (fun () ->
         viewport ~rows:1 ~scroll_x:999_920 (span editor_text)));
  same "overlays"
    (words "overlays-hidden-0" (fun () ->
         viewport ~scroll:`Middle (span (tall 100_000))))
    (words "overlays-hidden-10k" (fun () ->
         viewport ~scroll:`Middle ~setup:(hidden_overlays 10_000)
           (span (tall 100_000))));
  same "wrapped rows"
    (words "styled-10kb-middle" (fun () ->
         viewport ~wrap:`Word ~scroll:`Middle
           (List.filteri (fun i _ -> i < 1_000) styled)))
    (words "styled-100kb-middle" (fun () ->
         viewport ~wrap:`Word ~scroll:`Middle styled));
  let ten =
    words "visible-10-rows" (fun () -> viewport ~rows:10 (span visible_lines))
  in
  let twenty =
    words "visible-20-rows" (fun () -> viewport (span visible_lines))
  in
  Printf.printf "per-visible-row=%d words\n" ((twenty - ten) / 10);
  let buffer = ref None in
  let dense =
    words "visible-20-rows-200-overlays" (fun () ->
        viewport
          ~setup:(fun b ->
            dense_overlays 200 b;
            buffer := Some b)
          (span visible_lines))
  in
  (* Paint looks up a style run where each visible row starts and where the
     previous run ends: count those lookups. *)
  let buffer = Option.get !buffer and runs = ref 0 in
  for row = 0 to 19 do
    let g = ref (row * 81) in
    while !g < (row * 81) + 80 do
      incr runs;
      g := snd (Text_buffer.style_at buffer !g)
    done
  done;
  Printf.printf "visible-runs=%d; per-visible-run=%d words\n" !runs
    ((dense - twenty) / (!runs - 20))

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

(* A megabyte of prose in paragraphs of 585 bytes, laid out at the four widths
   a text surface caches. *)
let prose =
  let sentence = "The quick brown fox jumps over the lazy dog; " in
  let paragraph = String.concat "" (List.init 13 (fun _ -> sentence)) in
  String.concat "\n" (List.init 1_709 (fun _ -> paragraph))

let mib = 1024 * 1024

let memory_widths_check () =
  let live () = (Gc.stat ()).live_words * (Sys.word_size / 8) in
  let renderer = Renderer.create () in
  let node = Renderable.create ~parent:(Renderer.root renderer) () in
  let buffer = Text_buffer.create () in
  let surface = Text_surface.create node buffer in
  Text_surface.set_wrap surface `Word;
  Gc.full_major ();
  let base = live () in
  Text_buffer.set_text buffer prose;
  ignore (Text_buffer.source buffer : Matrix_text.Layout.Source.t);
  Gc.full_major ();
  let index = live () - base in
  Printf.printf "source=%d bytes, %d graphemes; index=%d bytes\n"
    (String.length prose)
    (Matrix_text.Layout.Source.length (Text_buffer.source buffer))
    index;
  let previous = ref (live ()) in
  List.iter
    (fun width ->
      Text_surface.set_wrap_width surface (Some width);
      let rows = Text_surface.display_line_count surface in
      Gc.full_major ();
      Printf.printf "width-%d=%d rows, %d bytes\n" width rows
        (live () - !previous);
      previous := live ())
    [ 40; 60; 80; 120 ];
  ignore
    (Matrix_text.Layout.measure
       (Text_buffer.source buffer)
       ~width:100 ~wrap:`Word
      : int * int);
  Gc.full_major ();
  Printf.printf "count-only-probe=%d bytes\n" (live () - !previous);
  let retained = live () - base in
  Text_buffer.set_text buffer (prose ^ "y");
  ignore (Text_surface.display_line_count surface : int);
  Gc.full_major ();
  let peak = (Gc.stat ()).top_heap_words * (Sys.word_size / 8) in
  Printf.printf "four-widths-retained=%d bytes; replace-heap-peak=%d bytes\n"
    retained peak;
  ignore (Sys.opaque_identity (renderer, surface));
  if retained > 32 * mib || peak > 64 * mib then
    failwith "text layout memory budget exceeded at the cached widths"

let history_check () =
  Gc.full_major ();
  let before = (Gc.stat ()).live_words in
  let editor = ref (Editor.of_string editor_text) in
  let old_sources = Weak.create 200 in
  for i = 0 to 199 do
    Weak.set old_sources i (Some (Editor.source !editor));
    editor := !editor |> Editor.set_cursor 500_000 |> Editor.insert "y"
  done;
  Gc.full_major ();
  let live = ((Gc.stat ()).live_words - before) * (Sys.word_size / 8) in
  for i = 0 to 199 do
    if Weak.check old_sources i then
      failwith "undo retained an obsolete source index"
  done;
  Printf.printf "200-edit-history-and-current-index=%d bytes\n" live;
  ignore (Sys.opaque_identity !editor)

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "memory" then memory_check ()
  else if Array.length Sys.argv = 2 && Sys.argv.(1) = "history" then
    history_check ()
  else if Array.length Sys.argv = 2 && Sys.argv.(1) = "paint" then
    paint_check ()
  else if Array.length Sys.argv = 2 && Sys.argv.(1) = "memory-widths" then
    memory_widths_check ()
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
          "index/create-1mb" Editor.of_string;
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
            Editor.of_string editor_text |> Editor.set_cursor 500_000)
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
          ~setup:(fun () -> (make_surface styled, ref 0))
          "reflow/alternating-widths-100k" alternating_widths;
        Thumper.bench_with_setup
          ~setup:(fun () -> viewport plain)
          "paint/label" paint;
        Thumper.bench_with_setup
          ~setup:(fun () -> viewport ~scroll:`Middle (span (tall 100_000)))
          "paint/tall-100k-middle" paint;
        Thumper.bench_with_setup
          ~setup:(fun () -> viewport ~wrap:`Word ~scroll:`Middle styled)
          "paint/styled-100k-middle" paint;
        Thumper.bench_with_setup
          ~setup:(fun () ->
            viewport ~setup:(dense_overlays 200) (span visible_lines))
          "paint/overlays-dense-200" paint;
        Thumper.bench_with_setup
          ~setup:(fun () ->
            viewport ~scroll:`Middle ~setup:(hidden_overlays 10_000)
              (span (tall 100_000)))
          "paint/overlays-hidden-10k" paint;
        Thumper.bench_with_setup
          ~setup:(fun () ->
            let b = Text_buffer.create () in
            Text_buffer.set_text b (String.make 400 'x');
            b)
          "styles/nested-revision-200" overlay_revision;
      ]
