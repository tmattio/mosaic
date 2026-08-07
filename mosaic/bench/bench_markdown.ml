open Mosaic_ui

let viewport_width = 80
let viewport_height = 24

let render_frame renderer =
  Renderer.render_frame renderer ~width:viewport_width ~height:viewport_height
    ~delta:16.;
  ignore (Renderer.render renderer : string)

(* A code-syntax hook that keeps code blocks on the Code-widget path without
   pulling in a highlighter. Shared between create and props so physical
   equality holds, as the API requires. *)
let hook ~language:_ ~content:_ = None

let chunk i =
  Printf.sprintf
    "## Section %d\n\n\
     Paragraph with **bold** and `code` spans, number %d.\n\n\
     > A quoted line %d\n\n\
     - item a %d\n\
     - item b %d\n\n\
     ```ocaml\n\
     let f_%d x = x + %d\n\
     ```\n\n"
    i i i i i i i

let transcript n =
  let buf = Buffer.create (n * 200) in
  for i = 1 to n do
    Buffer.add_string buf (chunk i)
  done;
  Buffer.contents buf

type state = {
  renderer : Renderer.t;
  md : Markdown.t;
  base : string;
  appended : string;
  mutable flip : bool;
}

let make_stream () =
  let renderer = Renderer.create () in
  let base = transcript (Thumper.black_box 16) in
  let md =
    Markdown.create ~parent:(Renderer.root renderer) ~content:base
      ~streaming:true ~code_syntax:hook ()
  in
  render_frame renderer;
  {
    renderer;
    md;
    base;
    appended = base ^ "\n\nstreamed tail paragraph";
    flip = false;
  }

(* Alternate between the transcript and the transcript plus one appended
   paragraph: both directions exercise the content-only reconcile path over a
   prefix full of quote, list, and highlighted code blocks — the streaming
   chat-transcript shape this widget is built for. *)
let streaming_append st =
  st.flip <- not st.flip;
  Markdown.apply_props st.md
    (Markdown.Props.make
       ~content:(if st.flip then st.appended else st.base)
       ~streaming:true ~code_syntax:hook ());
  render_frame st.renderer

let steady_stream =
  Thumper.bench_with_setup ~setup:make_stream "update/streaming-append-16blocks"
    streaming_append

let () =
  Thumper.run "markdown"
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
    [ Thumper.group "transcript" [ steady_stream ] ]
