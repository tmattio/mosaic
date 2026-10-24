(* What repeated work costs, against what the view hides. A check counts the
   words one call allocates at two sizes, in one process: equal counts show
   that the work follows what is visible, on any machine, compiler and
   profile, with no baseline. The memory checks are fixed caps. *)

open Windtrap
open Mosaic_ui

(* The words [f ()] allocates, after a first call that warms its caches. *)
let words f =
  f ();
  let minor, promoted, major = Gc.counters () in
  f ();
  let minor', promoted', major' = Gc.counters () in
  Float.to_int (minor' -. minor +. (major' -. major) -. (promoted' -. promoted))

let same what ~small ~large =
  if small <> large then
    failf "%s: %d words at the small size, %d at the large one" what small large

(* ───── Repainting a text surface ─────

   A 80x20 viewport over text whose rows, columns or overlays mostly lie
   outside it. Setup builds and paints once; the measured call repaints the
   unchanged view. Paint allocates per visited row and per entered style run,
   so a hidden row, column or overlay costs nothing if paint never visits
   it. *)

let make_surface spans =
  let renderer = Renderer.create () in
  let node = Renderable.create ~parent:(Renderer.root renderer) () in
  let buffer = Text_buffer.create () in
  Text_buffer.set_styled_text buffer spans;
  let surface = Text_surface.create node buffer in
  Text_surface.set_wrap surface `Word;
  (renderer, surface)

let span text = [ { Text_buffer.text; style = Ansi.Style.default } ]
let tall lines = String.concat "" (List.init lines (fun _ -> "one line\n"))
let megabyte = String.make 1_000_000 'x'

let styled =
  List.init 10_000 (fun i ->
      {
        Text_buffer.text = "word text ";
        style =
          (if i mod 2 = 0 then Ansi.Style.default
           else Ansi.Style.make ~bold:true ());
      })

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
  Renderable.Private.render node grid ~dt:0.;
  (renderer, node, grid)

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

let repaint (renderer, node, grid) =
  words (fun () ->
      ignore (Sys.opaque_identity renderer : Renderer.t);
      Renderable.Private.render node grid ~dt:0.)

let hidden_rows () =
  same "repaint grows with hidden rows"
    ~small:(repaint (viewport ~scroll:`Middle (span (tall 1_000))))
    ~large:(repaint (viewport ~scroll:`Middle (span (tall 100_000))))

let hidden_columns () =
  same "repaint grows with hidden columns"
    ~small:
      (repaint
         (viewport ~rows:1 ~scroll_x:99_920 (span (String.make 100_000 'x'))))
    ~large:(repaint (viewport ~rows:1 ~scroll_x:999_920 (span megabyte)))

let hidden_overlay_count () =
  same "repaint grows with hidden overlays"
    ~small:(repaint (viewport ~scroll:`Middle (span (tall 100_000))))
    ~large:
      (repaint
         (viewport ~scroll:`Middle ~setup:(hidden_overlays 10_000)
            (span (tall 100_000))))

let hidden_wrapped_rows () =
  same "repaint grows with hidden wrapped rows"
    ~small:
      (repaint
         (viewport ~wrap:`Word ~scroll:`Middle
            (List.filteri (fun i _ -> i < 1_000) styled)))
    ~large:(repaint (viewport ~wrap:`Word ~scroll:`Middle styled))

(* The converse: what paint does visit, it pays for. *)
let visible_work () =
  let ten = repaint (viewport ~rows:10 (span visible_lines)) in
  let twenty = repaint (viewport (span visible_lines)) in
  let dense =
    repaint (viewport ~setup:(dense_overlays 200) (span visible_lines))
  in
  is_true
    ~msg:
      (Printf.sprintf "visible rows cost: 10 rows %d words, 20 rows %d" ten
         twenty)
    (twenty > ten);
  is_true
    ~msg:
      (Printf.sprintf "visible style runs cost: plain %d words, 200 overlays %d"
         twenty dense)
    (dense > twenty)

(* ───── Frames of a scrolled transcript ───── *)

let render_frame renderer =
  Renderer.render_frame renderer ~width:80 ~height:24 ~dt:16.;
  ignore (Renderer.render renderer : string)

(* [rows] laid-out rows behind a 24-row viewport. *)
let transcript rows =
  let renderer = Renderer.create () in
  let px n = Toffee.Style.Dimension.length (Float.of_int n) in
  let scroll =
    Scroll_box.create ~parent:(Renderer.root renderer)
      ~style:Toffee.Style.(default |> set_width (px 80) |> set_height (px 24))
      ~show_scrollbars:false ()
  in
  for _ = 1 to rows do
    ignore
      (Text.create ~parent:(Scroll_box.node scroll)
         ~style:Toffee.Style.(default |> set_width (px 80) |> set_height (px 1))
         ~content:"A representative transcript row." ~selectable:false ()
        : Text.t)
  done;
  render_frame renderer;
  renderer

let hidden_transcript_rows () =
  let frame rows =
    let renderer = transcript rows in
    words (fun () -> render_frame renderer)
  in
  same "a steady frame grows with the rows the viewport hides"
    ~small:(frame 1_000) ~large:(frame 10_000)

(* ───── Memory ───── *)

let mib = 1024 * 1024

let at_most ~msg cap bytes =
  if bytes > cap then failf "%s: %d bytes, over the cap of %d" msg bytes cap

let live_bytes () = (Gc.stat ()).live_words * (Sys.word_size / 8)
let peak_bytes () = (Gc.stat ()).top_heap_words * (Sys.word_size / 8)

(* A megabyte at width 1 is a million rows. *)
let million_rows () =
  let module L = Matrix_text.Layout in
  Gc.full_major ();
  let before = live_bytes () in
  let layout = L.create (L.Source.of_string megabyte) ~width:1 ~wrap:`Char in
  Gc.full_major ();
  let retained = live_bytes () - before in
  let replacement =
    L.create (L.Source.of_string (megabyte ^ "y")) ~width:1 ~wrap:`Char
  in
  Gc.full_major ();
  let peak = peak_bytes () in
  ignore (Sys.opaque_identity (layout, replacement));
  at_most ~msg:"a source and its million rows, retained" (128 * mib) retained;
  at_most ~msg:"heap peak while a second layout is built" (256 * mib) peak

(* A megabyte of prose in paragraphs of 585 bytes, laid out at the four widths
   a text surface caches. The measured 20.4 MiB retained and 42.0 MiB peak
   leave less room than one more array per grapheme (7.6 MiB): such an array
   exceeds both caps. The peak allows the old and the new text's geometry
   together. *)
let prose =
  let sentence = "The quick brown fox jumps over the lazy dog; " in
  let paragraph = String.concat "" (List.init 13 (fun _ -> sentence)) in
  String.concat "\n" (List.init 1_709 (fun _ -> paragraph))

let cached_widths () =
  let renderer = Renderer.create () in
  let node = Renderable.create ~parent:(Renderer.root renderer) () in
  let buffer = Text_buffer.create () in
  let surface = Text_surface.create node buffer in
  Text_surface.set_wrap surface `Word;
  Gc.full_major ();
  let base = live_bytes () in
  Text_buffer.set_text buffer prose;
  List.iter
    (fun width ->
      Text_surface.set_wrap_width surface (Some width);
      ignore (Text_surface.display_line_count surface : int))
    [ 40; 60; 80; 120 ];
  ignore
    (Matrix_text.Layout.measure
       (Text_buffer.source buffer)
       ~width:100 ~wrap:`Word
      : int * int);
  Gc.full_major ();
  let retained = live_bytes () - base in
  Text_buffer.set_text buffer (prose ^ "y");
  ignore (Text_surface.display_line_count surface : int);
  Gc.full_major ();
  let peak = peak_bytes () in
  ignore (Sys.opaque_identity (renderer, surface));
  at_most ~msg:"a megabyte of prose at four cached widths, retained" (24 * mib)
    retained;
  at_most ~msg:"heap peak while the text is replaced" (48 * mib) peak

let () =
  run "mosaic.cost"
    [
      (* First, in this order: a heap peak is the process's highest yet. *)
      group "Memory"
        [
          test "four cached widths of a megabyte" cached_widths;
          test "a million rows" million_rows;
        ];
      group "Repaint follows the visible area"
        [
          test "hidden rows" hidden_rows;
          test "hidden columns" hidden_columns;
          test "hidden overlays" hidden_overlay_count;
          test "hidden wrapped rows" hidden_wrapped_rows;
          test "visible rows and runs" visible_work;
        ];
      group "Frames follow the visible area"
        [ test "a scrolled transcript" hidden_transcript_rows ];
    ]
