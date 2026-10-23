# Syntax highlighting with tree-sitter

Mosaic colors source code from scope-annotated byte ranges and does not depend
on a parser. An application that wants tree-sitter highlighting writes the glue
between the parser and Mosaic itself. This page is that glue; the rules it
relies on are stated in the interfaces listed at the end.

## The glue

The independently packaged [Tree-sitter bindings](https://github.com/invariant-hq/ocaml-tree-sitter)
provide `tree-sitter.ocaml` and `tree-sitter.json`. Add those libraries to your
application's Dune stanza alongside `mosaic`. The
[runnable integration example](https://github.com/invariant-hq/ocaml-tree-sitter/tree/main/examples/mosaic)
uses this complete glue:

```ocaml
let highlight ~language ~content =
  let captures =
    match language with
    | "ocaml" | "ml" -> Tree_sitter_ocaml.highlight content
    | "mli" -> Tree_sitter_ocaml.highlight_interface content
    | "json" -> Tree_sitter_json.highlight content
    | _ -> []
  in
  Mosaic.Syntax_highlight.of_triples ~content captures

let highlighter = Mosaic.Code.Highlighter.threaded highlight

let code_view src =
  Mosaic.code
    ~syntax:(Mosaic.Code.with_highlighter ~language:"ocaml" highlighter)
    src

let markdown_view md = Mosaic.markdown ~highlighter md
```

`highlight` is the application's resolver: put the language names, aliases and
filename extensions your application uses there, and a cache if you want one.
Unknown languages stay plain.

## Editable text

A textarea takes annotations instead of a highlighter. Resolve the captures
once when they arrive, keep both in the model, and pass the annotations while
they still describe the editor's text:

```ocaml
let highlighted captures =
  ( captures,
    Mosaic.Syntax_highlight.to_annotations ~style:Mosaic.Syntax_style.default
      captures )

let editor_view state (captures, annotations) =
  let annotations =
    if
      String.equal (Mosaic.Editor.text state)
        (Mosaic.Syntax_highlight.content captures)
    then Some annotations
    else None
  in
  Mosaic.textarea ~state ?annotations ~on_change:(fun state -> Edit state) ()
```

## Where the rules live

- [`Code`](../mosaic/lib/ui/renderables/code.mli): highlighters, their
  requests and failures, the worker of `Highlighter.threaded`, and what a view
  shows while a result is pending.
- [`Syntax_highlight`](../mosaic/lib/ui/syntax_highlight.mli): byte ranges,
  captures bound to their source, and their resolution into annotations.
- [`Syntax_style`](../mosaic/lib/ui/syntax_style.mli): the scope vocabulary
  shared by highlighters and styles, and how scopes fall back.
- [`Markdown`](../mosaic/lib/ui/renderables/markdown.mli) and
  [`Diff`](../mosaic/lib/ui/renderables/diff.mli): how fenced code blocks and
  diff panes use a highlighter.
- [`Textarea`](../mosaic/lib/ui/renderables/textarea.mli) and
  [`Text_buffer.Annotations`](../mosaic/lib/ui/text_buffer.mli): annotations
  on editable text.

Mosaic's [examples](../mosaic/examples/common/toy_highlighter.ml) use a small
lexical highlighter in place of a parser package.
