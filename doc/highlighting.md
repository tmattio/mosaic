# Syntax highlighting

Mosaic renders scope-annotated UTF-8 byte ranges. The application chooses the
highlighter and interprets language names; Mosaic has no language registry or
parser dependency. A synchronous backend is simply:

```ocaml
let highlighter =
  Mosaic.Code.Highlighter.sync (fun ~language:_ ~content ->
      if String.starts_with ~prefix:"let " content then
        Mosaic.Syntax_highlight.of_triples [ (0, 3, "keyword") ]
      else [])
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
  (match language with
    | "ocaml" | "ml" -> Some Tree_sitter_ocaml.highlight
    | "mli" -> Some Tree_sitter_ocaml.highlight_interface
    | "json" -> Some Tree_sitter_json.highlight
    | _ -> None)
  |> Option.map (fun f -> Mosaic.Syntax_highlight.of_triples (f content))

let highlighter =
  Mosaic.Code.Highlighter.threaded (fun ~language ~content ->
      Option.value (highlight ~language ~content) ~default:[])

let code_view src =
  Mosaic.code
    ~syntax:(Mosaic.Code.with_highlighter ~language:"ocaml" highlighter)
    src

let markdown_view md =
  Mosaic.markdown
    ~code_syntax:(fun ~language ~content ->
      Option.bind language (fun language ->
          highlight ~language ~content
          |> Option.map (fun ranges -> Mosaic.Code.syntax ~language ranges)))
    md
```

The code view uses the worker. The Markdown example computes pre-existing
ranges synchronously per fence and leaves unknown languages plain. A fence
resolver can instead return `Code.with_highlighter ~language highlighter` to
schedule work asynchronously. Language aliases, filename extensions, and
caching on `(language, content)` belong in the application. Bound caches by
size; skip work for fences whose content is still streaming when appropriate.

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

`Code` compares highlighter and style values by physical identity. Recreating
them inside a view restarts highlighting on every reconcile; keep them at
module scope or memoize them.

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

Pass the stable `style` value to `Code.syntax` or `Code.with_highlighter`.
Mosaic's [self-contained examples](../mosaic/examples/common/toy_highlighter.ml)
use a small lexical highlighter to demonstrate the same boundary without a
parser package.
