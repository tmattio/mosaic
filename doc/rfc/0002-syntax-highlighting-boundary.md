# RFC 0002: The syntax-highlighting boundary — cookbook glue, and tree-sitter leaves the repository

- Status: Decided 2026-08-21 (the maintainer's direction) and refined 2026-09-24 (`85a60e89`).
  Implemented on 2026-09-24, `50072f7d` through `07d38086`; R4's alignment with tree-sitter, R6
  and the pending-display rule were finished on 2026-10-22, `3acfbf30` through `e1b2ad00`. This
  text states what was built, including the decisions taken during implementation (§3).
- Bindings: public at [invariant-hq/ocaml-tree-sitter](https://github.com/invariant-hq/ocaml-tree-sitter)
  @ `880ae2c`. Its mosaic example (`examples/mosaic`) does not compile against the API of `07d38086`
  and later. A patch that makes it the recipe of [doc/highlighting.md](../highlighting.md) again is
  pending in that repository.
- Baseline: mosaic @ `295460c0`; spice/mentat @ `e8506bc9`; OpenTUI's `packages/core`, a
  datapoint.
- Process: a syntax-coupling audit (2026-08-21), then the maintainer's direction. No blind design,
  skeptic or reference audit: it predates raven's "Becoming certain". On 2026-10-22 an audit of
  the implementation against this text found R6 undelivered, the vocabulary unchecked against
  tree-sitter's names, and two of the recipe's three copies no longer compiling. The commits of
  2026-10-22 deliver R6, align the vocabulary, and compile the guide's copy in CI (D4).
- Relation: [RFC 0001](0001-single-owner-state.md) owns application state. R6 was handed to it,
  it kept physical identity for syntax settings, and this RFC closed R6 itself.
  [RFC 0003](0003-text-layout.md) installs highlights as source-bound annotations (its D6).
- Compatibility: breaking, before 1.0. The breaks are in §5.

## Summary

Mosaic renders highlights; it does not compute them. Yet one opam line made every consumer
compile tree-sitter's grammars, the glue each application wrote had no documented home, and the
repository hosted bindings mosaic did not use. The decision:
- **Mosaic knows scopes, not languages.** A highlighter maps a language name and a whole buffer to
  scope-annotated byte ranges bound to that buffer. The application owns the resolver from names
  to grammars.
- **The scope vocabulary is the contract** between producers and themes: tree-sitter's highlight
  names plus `label`, stated in `Syntax_style`.
- **tree-sitter lives in its own repository.** Mosaic, its packages and its examples are free of
  it. A fresh install went from 89.6 s to 60.4 s.
- **Highlighting configuration is a value, except the highlighter**, a resource that may own a
  worker. Code, Diff and Markdown share one highlighter concept.

## 1. Background: the evidence

From the 2026-08-21 audit, at `295460c0`:
- **One dependency edge.** No `(library …)` stanza depended on tree-sitter, and `Tree_sitter*`
  appeared in four examples. The only edge was a bare `tree-sitter` in the `mosaic` package's
  `(depends …)` (`dune-project:107`). Through it every consumer built about 1.16M lines (36 MB) of
  generated C, a vendored runtime and a configure step.
- **The glue is small.** Mentat's whole integration was a 13-line resolver and 9 lines of style
  (`spice/lib/tui/code_highlight.ml:11-37`), synchronous, over six mosaic functions.
- **Incrementality went unused.** OpenTUI built incremental reparse (edit queues, buffer
  identities, `Tree.edit` and `getChangedRanges`, `parser.worker.ts:548-651`); its shipped
  `Code.ts:332` calls the stateless `highlightOnce`.
- **Injections have no producer.** The waist carries their metadata and `to_spans` honors it;
  markdown in code is composition, one child Code per fence, as in OpenTUI.

The waist (`Syntax_highlight`, `Code.Highlighter`, `Syntax_style`) was already backend-neutral,
with the fields of OpenTUI's `SimpleHighlight`.

## 2. The rulings

Confidence: high where a reference or measurement decides; medium for a mosaic choice resting on
its reason. Tests are in `mosaic/test/unit/` unless stated.

| ruling | rule | reference / reason | confidence | pinned by |
|---|---|---|---|---|
| R1. No language registry | `language` is an opaque string the application interprets. Mosaic holds no language names, extensions or fence info-strings | One job per library (design.md). Naming is policy that differs per application: mentat maps `ml` and `mli` to two grammars of one package. OpenTUI keeps a registry in core (`resolve-ft.ts`, 186 lines; datapoint) | high | "highlighter receives fence language" (`test_markdown.ml`) |
| R2. Cookbook, not a bridge package | The glue is a recipe each application owns, in `doc/highlighting.md`. No `mosaic-tree-sitter` package | The glue is about ten lines; a package adds release surface, version coupling and a second home for the resolver. Revisit if a third-party backend ecosystem appears | medium (§7) | `mosaic/test/doc` compiles the guide's snippets |
| R3. Whole-buffer requests | `{content; language}`: no edit deltas, no buffer identity. A backend may keep state behind `Highlighter.t`. Caching stays with the application, keyed on `(language, content)` | OpenTUI's unused incremental machinery (datapoint) | medium-high | "async content changes coalesce" (`test_code.ml`) |
| R4. The scope vocabulary is the contract | A producer emits scopes whose first segment is one of `comment string number constant keyword operator punctuation function type constructor variable property module attribute label embedded tag`. Dotted refinements fall back to their base; unmapped scopes are kept and resolve to the base style. `Syntax_style.default` styles `variable.member` as `property` | tree-sitter, [Syntax Highlighting](https://tree-sitter.github.io/tree-sitter/3-syntax-highlighting.html), Queries › Highlights: "Commonly used highlight names include `keyword`, `function`, `type`, `property`, and `string`." Its CLI's default theme (`crates/cli/src/highlight.rs`, `Theme::default`) styles the rest (datapoint). `label` is mosaic's | medium-high | "scope vocabulary fallback and extensions" (`test_syntax_highlight.ml`) |
| R5. tree-sitter leaves the repository | The bindings, the json and ocaml grammars, their tests and their opam package (`tree-sitter`, name unchanged) live in invariant-hq/ocaml-tree-sitter. `dune-project`, `mosaic.opam`, the lock and CI do not mention it; the examples use a keyword toy (`mosaic/examples/common/toy_highlighter.ml`) | No dependency used only inside an implementation (design.md); measured install (§6) | high | the build |
| R6. Configuration compares by value | `Syntax_style.t` and `Markdown.Style.t` are values compared with `equal`. Markdown takes `?highlighter` (a `Code.Highlighter.t`) and `?syntax_style`, not a per-fence closure. Only the highlighter compares by identity: it is a resource that may own a worker and its queue | A configuration owned by an allocation site re-renders on every view call: 20 fences over 5 refreshes ran 100 highlights with a closure capturing a theme, and run 20 now (§6) | high | "styles compare by value" (`test_syntax_highlight.ml`); "syntax compares styles by value" (`test_code.ml`); "view refreshes do not highlight again", "element style rebuilt renders nothing" (`test_markdown.ml`) |

R6 first deferred the widget wiring to RFC 0001, which kept physical identity for the
highlighter, the style and Markdown's `code_syntax`. It was delivered here: styles by value
(`3acfbf30`), Markdown's highlighter and style (`b16067a5`), Markdown's element style as a value
(`e1b2ad00`).

The decided text also fixed two surfaces:
- **One name per grammar.** Each grammar package exposes
  `highlight : string -> (int * int * string) list`; secondary grammars suffix it
  (`Tree_sitter_ocaml.highlight_interface`, `highlight_type`). The triples are the boundary's
  currency and keep the grammars ignorant of mosaic.
- **Labeled requests, and the off-thread one-liner.** `Code.Highlighter.sync` and `threaded` take
  `language:string -> content:string -> Syntax_highlight.t`. `async` and `job` remain for
  backends with their own scheduling.

## 3. Decisions taken during implementation

- **D1. Captures are bound to their source** (`07d38086`). `Syntax_highlight.t` is abstract and
  built with `make ~content` or `of_triples ~content`. A result that does not match its request's
  bytes shows plain text instead of coloring other bytes. Code installs captures as source-bound
  annotations, so a theme change reuses them and rebuilds no geometry. This changed the waist the
  decided text called stable, and broke the recipe's copies in this RFC and in the bindings'
  example.
- **D2. One pending-display rule** (`0c051d74`) replaces `?draw_unstyled` and `?streaming`. A
  pending view shows its current content; bytes it shares with the latest result's source, as a
  common prefix or suffix, keep that result's styling, and the rest is plain. A result for obsolete
  content is never shown as such but becomes the latest result, so styling keeps up with a stream.
  Diff's hidden mode goes with it, and `Code.line_info_stable`, which restated `is_highlighting`.
- **D3. The worker contract** (`50072f7d`, `329eb6b7`, `038cad94`; the guide corrected in
  `d9265a26`). `threaded f` runs at most one call to `f` at a time per highlighter, on a systhread
  started on demand that exits when its queue empties. Each code view has at most one request in
  flight; later content and syntax changes coalesce into one follow-up request, started when the
  job completes. Only destroying the view or exiting the application cancels: a queued request is
  removed, and a running call finishes with its result discarded. Completion wakes an idle loop
  through an atomic redraw flag. The bindings copy their input before releasing the runtime lock,
  and each grammar guards its cached parser and query with a mutex.
- **D4. The guide's snippets are typechecked in CI** (`eafdf527`). A dune rule extracts every OCaml
  block of `doc/highlighting.md`, with line directives, and `mosaic/test/doc` compiles them against
  the current API. Stub `Tree_sitter_ocaml` and `Tree_sitter_json` modules give the grammars'
  highlighting interface, so mosaic gains no dependency.
- **D5. Wrapped split diffs settle under asynchronous highlighting** (`1aff4a2a`). Each rebuild
  reinstalled the unaligned pane text, which restarted highlighting, so the panes alternated
  forever: six jobs became eleven over ten more frames. The diff now keeps its alignment, keyed by
  the unaligned texts, the pane widths, the wrap mode and the highlight settings. Pinned by "async
  wrapped split settles" (`test_diff.ml`).

## 4. What was weighed and rejected

- **A language registry in mosaic** (OpenTUI's `addFiletypeParser`). Mosaic would hold policy each
  application decides differently (R1). The accepted cost: each application writes its resolver,
  and resolvers drift between applications.
- **A `mosaic-tree-sitter` bridge package** (the jsont, bytesrw, jsont_bytesrw pattern). Release
  surface and version coupling, to save about ten lines.
- **Incremental requests** (edit deltas, buffer identity). OpenTUI paid for them, and its own
  renderables do not use them.
- **A `GRAMMAR` module type.** Its only content beyond the function was a `name` nothing reads,
  at the price of first-class modules in every resolver.
- **Two booleans, or a variant, for the pending display.** Each policy was wrong somewhere: plain
  text dropped every color on each streamed token, hiding showed a blank pane, and the previous
  buffer froze while content changed faster than it was highlighted (D2).
- **A per-fence closure for Markdown** (`?code_syntax`). It carried nothing a highlighter does not:
  the language travels in the request and the style is data. Compared by identity, it
  re-highlighted every fence on each view call that rebuilt it.
- **Not designed:** a viewport-limited or streaming highlight API, an injection producer, a
  per-line highlighter (byte ranges subsume it).

## 5. Behavior breaks

- `Code.Highlighter.sync` takes `~language ~content` instead of a request record;
  `Code.Highlighter.threaded` is added (`50072f7d`).
- The `mosaic` package no longer depends on `tree-sitter` (`8e31eee7`). In the bindings,
  `Tree_sitter_ocaml.highlight_ocaml` is `highlight`, and `highlight_type` is added.
- `Syntax_highlight.t` is abstract; `make` and `of_triples` take `~content`; textarea takes
  `~annotations` instead of `~spans` (`07d38086`).
- `Code.with_highlighter` and `Diff.syntax` drop `?draw_unstyled` and `?streaming`.
  `Code.line_info_stable`, `Text_surface.render_enabled`, `Text_surface.set_render_enabled` and
  `Text_renderable.set_render_enabled` are removed (`0c051d74`).
- Markdown drops `?code_syntax` and `set_code_syntax` for `?highlighter`, `?syntax_style`,
  `set_highlighter` and `set_syntax_style`. A fence without a language is plain; aliases move into
  the highlighter (`b16067a5`).
- `Markdown.style` and `Markdown.default_style` become `Markdown.Style.t` and
  `Markdown.Style.default`; a heading key outside 1..6 raises (`e1b2ad00`).

## 6. Using it, and verification

The recipe, the worker's lifetime, scope styling and editable annotations are in
[doc/highlighting.md](../highlighting.md), whose snippets compile in CI (D4). The contracts are in
`mosaic/lib/ui/syntax_highlight.mli`, `syntax_style.mli`, `renderables/code.mli` and
`renderables/markdown.mli`. This RFC does not copy them.

- **Build.** A fresh-directory install with the same lock and cache took 60.4 s instead of
  89.6 s, with no tree-sitter compilation (`8e31eee7`). The standalone bindings and a mosaic-only
  consumer built locally.
- **Scopes.** The extracted grammars @ `880ae2c` emit nine names, all in the vocabulary: `comment`,
  `constant`, `keyword`, `module`, `number`, `string`, `type`, `variable`, `variable.member`.
- **P3.** Twenty fences over five unchanged refreshes: 100 highlight calls at `739cf2a1` with a
  closure capturing a theme (20 with a closure capturing nothing); 20 after R6.
- **The guide.** `mosaic/test/doc` fails mosaic's build when an API the guide uses changes.

## 7. Open

design.md makes the `.mli` the source of truth and rules out a guide re-explaining interfaces,
while R2 chose a cookbook page: should the recipe move into `code.mli` and the guide go?
