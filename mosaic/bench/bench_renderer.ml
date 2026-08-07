open Mosaic_ui

let transcript_rows = 1_000
let viewport_width = 80
let viewport_height = 24

let row_style =
  Toffee.Style.default
  |> Toffee.Style.set_width
       (Toffee.Style.Dimension.length (Float.of_int viewport_width))
  |> Toffee.Style.set_height (Toffee.Style.Dimension.length 1.)

let viewport_style =
  Toffee.Style.default
  |> Toffee.Style.set_width
       (Toffee.Style.Dimension.length (Float.of_int viewport_width))
  |> Toffee.Style.set_height
       (Toffee.Style.Dimension.length (Float.of_int viewport_height))

let row_text =
  Thumper.black_box
    "A representative transcript row with enough text to exercise grapheme \
     batching."

let render_frame renderer =
  Renderer.render_frame renderer ~width:viewport_width ~height:viewport_height
    ~delta:16.;
  ignore (Renderer.render renderer : string)

let make_transcript_pair () =
  let renderer = Renderer.create () in
  let scroll =
    Scroll_box.create ~parent:(Renderer.root renderer) ~style:viewport_style
      ~show_scrollbars:false ()
  in
  for _ = 1 to transcript_rows do
    ignore
      (Text.create ~parent:(Scroll_box.node scroll) ~style:row_style
         ~content:row_text ~selectable:false ()
        : Text.t)
  done;
  render_frame renderer;
  (renderer, scroll)

let make_transcript () = fst (make_transcript_pair ())

let steady_scrolled_transcript =
  Thumper.bench_with_setup ~setup:make_transcript
    "frame/scrolled-transcript-1000" render_frame

(* One-row wheel tick in a full-screen transcript. With the DECSTBM scroll
   hint the presentation shifts the region in hardware and rewrites only the
   revealed row; without it the scrolled content changes every viewport row.
   Rows carry distinct content — a transcript of identical rows diffs to
   nothing when shifted and would measure an empty present. Direction
   alternates so the position stays put across samples. *)
type scroll_state = {
  scroll_renderer : Renderer.t;
  scroll_box : Scroll_box.t;
  mutable scroll_dir : int;
}

let scroll_phrases =
  Thumper.black_box
    [|
      "the daemon reported a checkpoint and forty-one workers resumed cleanly";
      "compaction finished in the background while queries kept their latency";
      "a replica lagged behind the quorum until the log shipped its segment";
      "the scheduler rebalanced shards across nodes without dropping a frame";
      "cache pressure stayed flat as the resident set crossed the threshold";
    |]

let make_scroll_by_one () =
  let renderer = Renderer.create () in
  let scroll =
    Scroll_box.create ~parent:(Renderer.root renderer) ~style:viewport_style
      ~show_scrollbars:false ()
  in
  for i = 1 to transcript_rows do
    let content =
      Printf.sprintf "%04d %s" i
        scroll_phrases.(i mod Array.length scroll_phrases)
    in
    ignore
      (Text.create ~parent:(Scroll_box.node scroll) ~style:row_style ~content
         ~selectable:false ()
        : Text.t)
  done;
  render_frame renderer;
  Scroll_box.scroll_to scroll ~y:(transcript_rows / 2) ();
  render_frame renderer;
  { scroll_renderer = renderer; scroll_box = scroll; scroll_dir = 1 }

let scroll_by_one_frame st =
  Scroll_box.scroll_by st.scroll_box ~y:st.scroll_dir ();
  st.scroll_dir <- -st.scroll_dir;
  render_frame st.scroll_renderer

let scroll_by_one =
  Thumper.bench_with_setup ~setup:make_scroll_by_one "frame/scroll-by-one-row"
    scroll_by_one_frame

(* Steady full-frame scenario at 200x50: no scrolling, no layout changes.
   Exercises the per-frame fixed costs — buffer clears, command build, text
   painting, and the diff scan — at a size where clear cost is visible. *)
let idle_width = 200
let idle_height = 50

let idle_root_style =
  Toffee.Style.default
  |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
  |> Toffee.Style.set_size (Toffee.Style.Size_dim.pct ~w:100. ~h:100.)

let idle_row_style =
  Toffee.Style.default
  |> Toffee.Style.set_width
       (Toffee.Style.Dimension.length (Float.of_int idle_width))
  |> Toffee.Style.set_height (Toffee.Style.Dimension.length 1.)

let idle_text =
  Thumper.black_box
    "A steady full-width row whose glyphs are repainted every frame while \
     nothing changes."

let render_idle_frame renderer =
  Renderer.render_frame renderer ~width:idle_width ~height:idle_height
    ~delta:16.;
  ignore (Renderer.render renderer : string)

let make_idle_screen () =
  let renderer = Renderer.create ~style:idle_root_style () in
  for _ = 1 to idle_height do
    ignore
      (Text.create ~parent:(Renderer.root renderer) ~style:idle_row_style
         ~content:idle_text ~selectable:false ()
        : Text.t)
  done;
  render_idle_frame renderer;
  renderer

let idle_full_frame =
  Thumper.bench_with_setup ~setup:make_idle_screen "frame/full-200x50"
    render_idle_frame

(* Editor-typing scenario: a word-wrapped document whose content changes
   every frame, as a textarea does on every keystroke. Each frame pays the
   full measure + display rebuild for the new buffer version; the display
   cache must make measurement and rendering share one rebuild per width. *)
let typing_width = 80
let typing_height = 24

let typing_doc suffix =
  String.concat "\n"
    (List.init 150 (fun i ->
         Printf.sprintf
           "line %03d: the quick brown fox jumps over the lazy dog while the \
            lazy dog naps under the quick brown fox, wrapping past eighty \
            columns %s"
           i suffix))

let typing_a = Thumper.black_box (typing_doc "a")
let typing_b = Thumper.black_box (typing_doc "ab")

type typing_state = {
  renderer : Renderer.t;
  text : Text.t;
  mutable flip : bool;
}

let typing_frame st =
  st.flip <- not st.flip;
  Text.set_content st.text (if st.flip then typing_b else typing_a);
  Renderer.render_frame st.renderer ~width:typing_width ~height:typing_height
    ~delta:16.;
  ignore (Renderer.render st.renderer : string)

let make_typing () =
  let renderer = Renderer.create ~style:idle_root_style () in
  let text =
    Text.create ~parent:(Renderer.root renderer) ~content:typing_a ~wrap:`Word
      ~selectable:false ()
  in
  let st = { renderer; text; flip = false } in
  typing_frame st;
  st

let editor_typing =
  Thumper.bench_with_setup ~setup:make_typing "typing-wrapped-150-lines"
    typing_frame

let () =
  (* Frame loops care about retention as much as speed: promotions are
     objects escaping the minor heap mid-frame, and minor collections count
     GC pressure per frame. Both are counters, so thumper can prove them
     exact and gate them even on a busy machine. *)
  Thumper.run "renderer"
    ~config:
      Thumper.Config.(
        default
        |> metrics
             Thumper.Metric.
               [ wall_time; alloc_words; promoted_words; minor_collections ])
    ~budgets:
      [
        Thumper.Budget.no_slower_than 0.05; Thumper.Budget.no_more_alloc_than 0.;
      ]
    [
      Thumper.group "viewport" [ steady_scrolled_transcript; scroll_by_one ];
      Thumper.group "idle" [ idle_full_frame ];
      Thumper.group "editor" [ editor_typing ];
    ]
