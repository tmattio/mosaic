# Syntax highlighting with tree-sitter

Mosaic colors source code from scope-annotated byte ranges and does not depend
on a parser. An application that wants tree-sitter highlighting writes the glue
between the parser and Mosaic itself. This page is that glue; the rules it
relies on are stated in the interfaces listed at the end.

## The glue

The independently packaged [Tree-sitter bindings](https://github.com/invariant-hq/ocaml-tree-sitter)
provide `tree-sitter`, `tree-sitter.ocaml` and `tree-sitter.json`. Add those
libraries to your application's Dune stanza alongside `mosaic`. The
[runnable integration example](https://github.com/invariant-hq/ocaml-tree-sitter/tree/main/examples/mosaic)
uses this complete glue:

```ocaml
module Ts = Tree_sitter

(* Tree-sitter's point for [byte] in [s]: its row, and its column in bytes. *)
let point s byte =
  let row = ref 0 and line = ref 0 in
  for i = 0 to byte - 1 do
    if s.[i] = '\n' then begin
      incr row;
      line := i + 1
    end
  done;
  { Ts.row = !row; column = byte - !line }

(* A grammar parses again from the tree of the view's latest result, edited as
   the view says its content changed, then queries the whole new tree. A
   highlighter makes one call at a time, so one parser per grammar is enough. *)
let grammar language query =
  let parser = Ts.Parser.create language in
  let query = Ts.Query.create language ~source:query in
  fun ~content
      (previous : Ts.Tree.t option Mosaic.Code.Highlighter.previous option) ->
    let old =
      match previous with
      | Some
          {
            content = source;
            state = Some tree;
            edit = { start; removed; inserted };
          } ->
          (* After a failure the view passes the same tree again: edit a copy. *)
          let old = Ts.Tree.copy tree in
          Ts.Tree.edit old ~start_byte:start ~old_end_byte:(start + removed)
            ~new_end_byte:(start + inserted) ~start_point:(point content start)
            ~old_end_point:(point source (start + removed))
            ~new_end_point:(point content (start + inserted));
          Some old
      | Some { state = None; _ } | None -> None
    in
    let tree = Ts.Parser.parse_string ?old parser content in
    (Ts.highlight query tree, Some tree)

let ocaml =
  grammar (Tree_sitter_ocaml.ocaml ()) Tree_sitter_ocaml.ocaml_highlights_query

let interface =
  grammar
    (Tree_sitter_ocaml.interface ())
    Tree_sitter_ocaml.interface_highlights_query

let json = grammar (Tree_sitter_json.language ()) Tree_sitter_json.highlights_query

let highlight { Mosaic.Code.Highlighter.language; content; previous } =
  let captures, tree =
    match language with
    | "ocaml" | "ml" -> ocaml ~content previous
    | "mli" -> interface ~content previous
    | "json" -> json ~content previous
    | _ -> ([], None)
  in
  (Mosaic.Syntax_highlight.of_triples ~content captures, tree)

let highlighter = Mosaic.Code.Highlighter.threaded highlight

let code_view src =
  Mosaic.code
    ~syntax:(Mosaic.Code.with_highlighter ~language:"ocaml" highlighter)
    src

let markdown_view md = Mosaic.markdown ~highlighter md
```

`highlight` is the application's resolver: put the language names, aliases and
filename extensions your application uses there. Unknown languages stay plain.
Its state is the tree of the view's latest result, so a keystroke in a large
file parses again only around the edit; the query still covers the whole tree,
so the captures are the same as from a fresh parse of valid code, whatever the
query relates.

## Editable text

A textarea takes the same syntax settings as a code view, and may share its
highlighter:

```ocaml
let editor_view highlighter state =
  Mosaic.textarea ~state
    ~syntax:(Mosaic.Code.with_highlighter ~language:"ocaml" highlighter)
    ~on_change:(fun state -> Edit state)
    ()
```

Styles the application resolves itself, such as search matches or
diagnostics, go in `~annotations` instead.

## Where the rules live

- [`Code`](../mosaic/lib/ui/renderables/code.mli): highlighters, their
  requests, states and failures, what a result must hold, the worker of
  `Highlighter.threaded`, and what a view shows while a result is pending.
- [`Syntax_highlight`](../mosaic/lib/ui/syntax_highlight.mli): byte ranges,
  captures bound to their source, and their resolution into annotations.
- [`Syntax_style`](../mosaic/lib/ui/syntax_style.mli): the scope vocabulary
  shared by highlighters and styles, and how scopes fall back.
- [`Markdown`](../mosaic/lib/ui/renderables/markdown.mli) and
  [`Diff`](../mosaic/lib/ui/renderables/diff.mli): how fenced code blocks and
  diff panes use a highlighter.
- [`Textarea`](../mosaic/lib/ui/renderables/textarea.mli) and
  [`Text_buffer.Annotations`](../mosaic/lib/ui/text_buffer.mli): syntax and
  annotations on editable text.

Mosaic's [examples](../mosaic/examples/common/toy_highlighter.ml) use a small
lexical highlighter in place of a parser package.
