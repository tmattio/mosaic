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

let make_transcript () =
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
  renderer

let steady_scrolled_transcript =
  Thumper.bench_with_setup ~setup:make_transcript
    "frame/scrolled-transcript-1000" render_frame

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
    [ Thumper.group "viewport" [ steady_scrolled_transcript ] ]
