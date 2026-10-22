# Syntax highlighting

Mosaic renders scope-annotated UTF-8 byte ranges. The application chooses the
highlighter and interprets language names; Mosaic has no language registry or
parser dependency. A synchronous backend is simply:

```ocaml
let highlighter =
  Mosaic.Code.Highlighter.sync (fun ~language:_ ~content ->
      if String.starts_with ~prefix:"let " content then
        Mosaic.Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ]
      else Mosaic.Syntax_highlight.make ~content [])
```

For an actual backend, the ranges must describe the supplied whole buffer.
Ranges are half-open byte offsets, not character indexes. Use precomputed
ranges with `Code.syntax`, or attach a reusable backend with
`Code.with_highlighter`.

## Tree-sitter recipe

The independently packaged [Tree-sitter bindings](https://github.com/invariant-hq/ocaml-tree-sitter)
provide `tree-sitter.ocaml` and `tree-sitter.json`. Add those libraries to your
application's Dune stanza alongside `mosaic`; Mosaic itself does not depend on
them. The [runnable integration example](https://github.com/invariant-hq/ocaml-tree-sitter/tree/main/examples/mosaic)
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

Both views share one worker. Markdown renders each fenced block as a `Code`
view and passes the language its info string names to the highlighter;
fences without one stay plain, and the resolver above leaves unknown languages
plain. Language aliases, filename extensions, and caching on
`(language, content)` belong in the application. Bound caches by size.

## Worker lifetime

Construct a highlighter once and reuse it across renders and code blocks.
`threaded f` serializes calls to `f` on at most one worker systhread per
highlighter. The worker starts when needed and exits when its queue empties.
Only live requests remain queued: replacing content, changing syntax, or
destroying a code widget removes its pending request. Cancellation cannot
interrupt a running function; its result is discarded. A slow running call
therefore delays later requests on the same highlighter.

The worker publishes immutable ranges and schedules a render. Polling applies
them on the rendering thread. Exceptions produce plain text for that request;
they do not stop subsequent work. Do not access renderables from `f`, and
synchronize parser state shared outside the highlighter. Systhreads provide
concurrency, not CPU parallelism; native code that retains the OCaml runtime
lock can still block rendering. Use `Highlighter.async` with your own
poll/cancel jobs for a different scheduler or a worker process.

A highlighter is a resource: `Code` compares highlighters by physical
identity, so create one at module scope or in the model, never during a render.
Styles and syntax settings are values and may be rebuilt on every render; an
equal style changes nothing. Theme changes reuse the completed captures and source
index without restarting the highlighter. Pending results use the latest
accepted theme. A result bound to different source bytes is rejected and the
current source renders as plain text.

## Scope names and styling

The normative vocabulary is documented in
[`Syntax_style`](../mosaic/lib/ui/syntax_style.mli): `comment`, `string`,
`number`, `constant`, `keyword`, `operator`, `punctuation`, `function`, `type`,
`variable`, `module`, `attribute`, `label`, `embedded`, and `tag`. Normalize
backend-specific capture names into those categories. Dotted refinements
resolve through their parents: `variable.member` falls back to `variable`.
Unknown scopes are retained and fall back to the base style; applications may
style their own extensions explicitly.

Start with `Syntax_style.default`, or provide a palette:

```ocaml
let style =
  Mosaic.Syntax_style.make ~base:Mosaic.Ansi.Style.default
    [ ("keyword", Mosaic.Ansi.Style.make ~bold:true ~fg:Mosaic.Ansi.Color.cyan ());
      ("string", Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.yellow ());
      ("comment", Mosaic.Ansi.Style.make ~italic:true ~fg:(Mosaic.Ansi.Color.grayscale ~level:12) ()) ]
```

Pass `style` to `Code.syntax` or `Code.with_highlighter`.
Mosaic's [self-contained examples](../mosaic/examples/common/toy_highlighter.ml)
use a small lexical highlighter to demonstrate the same boundary without a
parser package.

## Editable annotations

Captures retain the exact source bytes passed to `Syntax_highlight.make` or
`of_triples`. They cannot silently recolour an unrelated source of the same
length. `to_annotations` resolves a theme without splitting or concatenating
text. Its default keeps concealed text visible, preserving editor positions:

```ocaml
let editor_view state captures =
  let annotations =
    if String.equal (Mosaic.Editor.text state)
         (Mosaic.Syntax_highlight.content captures) then
      Some (Mosaic.Syntax_highlight.to_annotations
              ~style:Mosaic.Syntax_style.default captures)
    else None
  in
  Mosaic.textarea ~state ?annotations ~on_change:(fun state -> Edit state) ()
```

Keep captures until the source changes; cursor moves need no highlighting.
Asynchronous applications discard stale results using the content check above.
Passing mismatched annotations directly raises `Invalid_argument` before
changing the editor. Equivalent source bytes are accepted after a remount.

For search or diagnostics, construct `Mosaic_ui.Text_buffer.Annotations.make`
with `~content:(Mosaic.Editor.text state)` and style ranges. Ranges use half-open
byte offsets, including line breaks, with later ranges overriding conflicting
style attributes. Bytes inside a grapheme resolve downstream, matching styled
text rendering; invalid UTF-8 follows the source index's replacement units.

`Code` still applies conceal metadata by default. Its projected visible source
is reused across themes, while the text buffer retains the indexed source and
geometry. `Syntax_highlight.to_spans` remains an
explicit export to styled text fragments; rendering annotations avoids those
substring copies. Static labels and rich text retain their fragment API.
