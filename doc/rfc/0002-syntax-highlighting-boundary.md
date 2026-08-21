# RFC 0002: The syntax-highlighting boundary — cookbook glue, and tree-sitter leaves the repository

- Status: Draft. Records the maintainer's 2026-08-21 direction: mosaic stays
  highlighter-agnostic with cookbook-documented glue (no registry, no bridge
  package), and tree-sitter moves to its own repository. The API design pass
  (§4) is open to refinement; the rulings (§3) are settled unless new
  evidence appears.
- Audience: Mosaic maintainers; tree-sitter bindings maintainers (same
  people, soon two repos); Mentat maintainers (spice pins both).
- Baselines: mosaic @ `295460c0`, spice/mentat @ `e8506bc9`, OpenTUI
  reference at `_opentui/packages/core`. Evidence from the 2026-08-21
  syntax-coupling audit, summarized in §2.
- Relation to RFC 0001: independent and compatible. The highlight waist
  (`Syntax_highlight`, scope vocabulary, the pull contract) is stable under
  both the current API and RFC 0001's single-owner redesign. The widget-side
  prop wiring (`Code.syntax`, physical-equality gating) is deliberately NOT
  polished here — RFC 0001 reshapes it (§3, R6).
- Compatibility: mosaic's installed libraries change only in documentation
  and small additive API (§4.2). The `mosaic` opam package loses its
  `tree-sitter` dependency. The tree-sitter package survives under its own
  repository with its opam name intact, so downstream is a pin-URL change.

## Summary

Mosaic is already decoupled from tree-sitter at the code level: no library
stanza links it, no public `.mli` mentions it, and `Syntax_highlight` is an
explicitly backend-neutral bridge. What remains is one spurious opam
dependency line that charges every mosaic consumer ~1.16M lines of generated
C on a cold build, a per-app glue layer (~13 lines) with no documented home,
and the repository itself hosting a subproject that mosaic does not use.

This RFC rules:

1. **No language registry in mosaic, ever.** `language` stays an opaque
   string the application interprets. The registry OpenTUI keeps in core is
   the coupling this boundary exists to avoid.
2. **Cookbook glue, not a bridge package.** The sanctioned integration is a
   documented ~12-line recipe each application owns. No `mosaic-tree-sitter`
   package.
3. **The request contract stays whole-buffer.** No incrementality hooks.
4. **The scope vocabulary is the seam's stability contract** and gets a
   normative home in `Syntax_style`'s documentation.
5. **tree-sitter moves to its own repository.** Mosaic's repo, packages, and
   examples become tree-sitter-free; one real integration example lives with
   the bindings.
6. A small design pass (§4) makes both API surfaces uniform so the cookbook
   recipe is as short as the boundary allows.

## 1. The boundary as it stands

The waist, already in place (`mosaic/lib/ui/syntax_highlight.mli:1-8`: "the
stable bridge between syntax highlighters and code renderables … without
depending on a concrete highlighter such as Tree-sitter"):

- `Syntax_highlight.t` — scope-annotated byte ranges over a UTF-8 source,
  with conceal/injection metadata. `of_triples : (int * int * string) list
  -> t` lifts the neutral currency every backend can produce.
- `Code.Highlighter.t` — a pull-based backend: `sync` of
  `request -> Syntax_highlight.t`, or `async` returning a poll/cancel job,
  polled from the render hook with a generation counter discarding stale
  results. No threading or event-loop assumption leaks through.
- `Syntax_style.t` — dotted scope names resolved with hierarchical fallback
  (`keyword.control.flow` → `keyword.control` → `keyword`) onto
  `Ansi.Style.t`, merged over a base style by `to_spans`.
- `Markdown`'s `?code_syntax:(language:string option -> content:string ->
  Code.syntax option)` — per-fence backend selection, push-shaped.
- `Diff` wraps two `Code.syntax` values (highlighter-only, because diff
  layouts synthesize source buffers).

This is structurally the same waist OpenTUI uses (`SimpleHighlight` ↔
`Syntax_highlight.range`, field for field, conceal and injection metadata
included; `treeSitterToTextChunks` ↔ `to_spans`).

## 2. Evidence

From the 2026-08-21 coupling audit:

- **Dependency edges.** No mosaic `(library …)` stanza depends on any
  tree-sitter library. `Tree_sitter*` appears in exactly four example
  programs. The single real edge is `dune-project:107` — a bare
  `tree-sitter` in the `mosaic` package `(depends …)` — propagated to
  `mosaic.opam`. `(implicit_transitive_deps false)` rules out hidden
  leakage. Examples carry no `(package …)` field and sit outside
  `dune build -p mosaic`.
- **Build cost of that one line.** The tree-sitter package compiles
  ~1.16M lines of generated C (~36 MB: the OCaml grammar alone is three
  11–14 MB parser files) plus a vendored runtime and a configure step. An
  application that never highlights code pays all of it.
- **The glue every consumer writes.** Mentat's entire integration is a
  13-line language resolver plus 9 lines of style
  (`spice/lib/tui/code_highlight.ml:11-37`): a match from language strings
  to grammar highlight functions, a filename→language map, and a
  `Syntax_style.make` over five scopes. It touches only `of_triples`,
  `Syntax_style.make`, `Code.syntax`, `Code.Highlighter.sync`,
  `Diff.highlight`, and Markdown's `~code_syntax`. It never uses the async
  path and needs no language beyond ocaml/mli/json.
- **The incrementality datum.** OpenTUI built full incremental reparse —
  edit queues, buffer identities, `Tree.edit` + `getChangedRanges`
  (`parser.worker.ts:548-651`) — and no shipped renderable uses it:
  `Code.ts:332` always calls stateless `highlightOnce`. They paid the
  complexity and never plugged it into their own UI.
- **Injections.** The waist's metadata supports them and `to_spans` honors
  them (pinned by `test/unit/test_syntax_highlight.ml:69-78`), but no
  producer emits them; markdown-in-code is handled by renderable
  composition (one child Code per fence), which is also OpenTUI's shipped
  path. Nothing in mosaic is keyed on parser state; all caching is
  `(language, content)`-keyed strings.

## 3. Rulings

**R1 — No registry in mosaic.** A registry means mosaic knows language
names, filename extensions, and fence info-strings — policy that differs per
application (mentat maps `"ml"` and `"mli"` to two different grammars of one
package) and drifts with every grammar ecosystem. OpenTUI's contrary choice
(`addFiletypeParser`, `resolve-ft.ts`'s 186-line table in core) is exactly
the coupling this RFC removes. The cost is stated honestly: every consumer
writes the resolver, and resolver tables drift between apps. That cost is
~12 lines (§5) and is policy the app should own anyway.

**R2 — Cookbook, not a bridge package.** A `mosaic-tree-sitter` bridge was
considered (the jsont/bytesrw/jsont_bytesrw pattern) and rejected for now:
the glue is small enough that a package adds release surface, version
coupling, and a second place for the resolver table to live, while saving a
dozen lines. The cookbook page (§5) is the single sanctioned recipe, kept
honest by a runnable example. Revisit only if a third-party backend
ecosystem actually materializes.

**R3 — The request contract stays whole-buffer.** `{content; language}`
(reshaped in §4.2), no edit deltas, no buffer identity. Backends that want
incremental parsing may keep internal state keyed however they like behind
`Highlighter.t`; the contract does not know. OpenTUI's unused incremental
machinery is the cautionary evidence. Caching stays consumer-side and
`(language, content)`-keyed, as both mentat caches already are.

**R4 — The scope vocabulary is the contract.** The seam's stability lives
in scope *names*, not types: backends emit them, themes cover them, and
today the convention is implicit in what the grammars happen to emit and
what `Syntax_style.default` happens to style. The design pass gives it a
normative home: `Syntax_style`'s documentation lists the base scope set —
proposal: `comment`, `string`, `number`, `constant`, `keyword`, `operator`,
`punctuation`, `function`, `type`, `variable`, `module`, `attribute`,
`label`, `embedded` — with dotted refinements permitted (resolved by the
existing hierarchical fallback) and the rule that a backend must emit
scopes whose first segment is in the base set. The exact list is confirmed
against the shipped grammar queries during the pass; grammars and
`Syntax_style.default` are then audited against it.

**R5 — tree-sitter leaves the repository.** The bindings, the json/ocaml
grammar packages, their tests, and their opam package move to a standalone
repository. Mosaic's repo drops the `tree-sitter/` tree, the dune-project
package stanza, and the depends line. Checklist in §6.

**R6 — Widget-side wiring defers to RFC 0001.** `Code.syntax`'s
construction, the physical-equality gating on highlighter/style/
`code_syntax` props (`code.ml:82-93`, `markdown.ml:87`), and the
`is_highlighting` surface are all reshaped by RFC 0001's single-owner
model, where syntax configuration becomes value-keyed state and the
identity footgun class disappears. Polishing them twice is waste; this RFC
touches them only where §4.2's additive changes require.

## 4. The design pass

Goal: both surfaces uniform enough that the cookbook recipe has no
incidental noise. Everything here is small and additive.

### 4.1 tree-sitter side: one shape per grammar

Today the grammar surface is ad hoc: `Tree_sitter_ocaml.highlight_ocaml`,
`highlight_interface`, `Tree_sitter_json.highlight` — three spellings for
one idea. The pass makes every grammar a module conforming to one signature,
specified in the bindings:

```ocaml
(* tree-sitter core *)
module type GRAMMAR = sig
  val name : string
  (** Canonical lowercase name, e.g. ["ocaml"], ["json"]. *)

  val highlight : string -> (int * int * string) list
  (** [highlight src] is the scope-annotated byte ranges of [src], sorted by
      start byte, empty ranges and [_]-prefixed captures dropped. Scopes
      follow the base vocabulary (see mosaic's [Syntax_style]). *)
end
```

Grammar packages export their grammars as such modules —
`Tree_sitter_ocaml.Ocaml`, `Tree_sitter_ocaml.Interface`,
`Tree_sitter_ocaml.Type`, `Tree_sitter_json.Json` — keeping the existing
`Parser`/`Tree`/`Query` core for advanced use (custom queries, editors)
untouched. The triples type is the deliberate currency of the boundary: it
keeps the grammars ignorant of mosaic, and `Syntax_highlight.of_triples` is
the one-call lift on the other side.

### 4.2 mosaic side: labeled request, and the off-thread one-liner

Two additive changes to `Code.Highlighter`:

```ocaml
val sync : (language:string -> content:string -> Syntax_highlight.t) -> t
(* replaces the request-record variant; one less type to learn *)

val threaded : (language:string -> content:string -> Syntax_highlight.t) -> t
(** [threaded f] is [f] run on a worker thread through the async job
    machinery: poll delivers the result on a later frame, cancel abandons
    a stale generation. The one-line spelling of the off-thread path. *)
```

The async path exists today but has zero users because nobody packages the
worker side; `threaded` makes not-blocking-the-frame as cheap as blocking
it. (`mosaic` already links `threads.posix`.) The `request` record and the
low-level `async`/`job` constructors remain for backends with their own
scheduling. `Syntax_style` gains the R4 vocabulary documentation;
`Syntax_highlight` and the widgets are otherwise untouched.

### 4.3 What is deliberately not designed

No streaming/viewport-limited highlight API (the `streaming` flag on
`with_highlighter` already covers the append-only case); no injection
producer (waist metadata stays, producers may arrive later, markdown
composition covers today's need); no per-line highlighter variant
(byte ranges subsume it).

## 5. The cookbook page

To be added as `doc/highlighting.md` in mosaic (and linked from
`Code`/`Markdown` docs). The complete glue, which is also the literal
source of the runnable example in the tree-sitter repository:

````markdown
# Syntax highlighting

Mosaic renders highlights; it does not compute them. A highlighter is any
function producing scope-annotated byte ranges; the resolver from language
names to highlighters is yours, because language naming is application
policy. With the tree-sitter grammar packages:

```ocaml
let highlight ~language ~content =
  let ranges (module G : Tree_sitter.GRAMMAR) =
    Mosaic.Syntax_highlight.of_triples (G.highlight content)
  in
  match language with
  | "ocaml" | "ml" -> Some (ranges (module Tree_sitter_ocaml.Ocaml))
  | "mli" -> Some (ranges (module Tree_sitter_ocaml.Interface))
  | "json" -> Some (ranges (module Tree_sitter_json.Json))
  | _ -> None

(* A code element, highlighted off-thread: *)
let code_view src =
  Mosaic.code
    ~syntax:
      (Mosaic.Code.with_highlighter ~language:"ocaml"
         (Mosaic.Code.Highlighter.threaded (fun ~language ~content ->
              Option.value
                (highlight ~language ~content)
                ~default:[])))
    src

(* Markdown fences pick their backend per fence: *)
let markdown_view md =
  Mosaic.markdown
    ~code_syntax:(fun ~language ~content ->
      Option.bind language (fun language ->
          highlight ~language ~content
          |> Option.map (fun hl -> Mosaic.Code.syntax ~language hl)))
    md
```

Styling maps scope names to terminal styles; the base vocabulary is
documented in {!Mosaic.Syntax_style}. Start from
[Syntax_style.default] or build your own over your palette:

```ocaml
let style =
  Mosaic.Syntax_style.make ~base:Ansi.Style.default
    [ ("keyword", keyword_style); ("string", string_style);
      ("comment", comment_style); ("type", type_style);
      ("number", number_style) ]
```

Highlighting is pure input → ranges, so cache on `(language, content)` if
you re-render the same blocks (a plain `Hashtbl` with a size cap is
enough), and skip highlighting on fences that are still streaming.
````

(The recipe is 12 lines of resolver; everything else is ordinary element
construction. Mentat's `code_highlight.ml` becomes this page's first
consumer.)

## 6. Extraction checklist

1. **New repository** for the bindings + grammars + their tests (the
   windtrap-migrated `tree-sitter/test` travels with them). Opam package
   names unchanged (`tree-sitter`), so consumers change a pin URL, not a
   dependency name. Naming caveat: before any opam-repository publication,
   check the `tree-sitter` name for collision with existing community
   bindings and rename if contested (Q2).
2. **mosaic repo**: delete the `tree-sitter/` tree; drop the tree-sitter
   package stanza and `dune-project:107`'s depends line; regenerate
   `mosaic.opam`; relock. `dune build @install` must be tree-sitter-free.
3. **Examples**: `10-code`, `13-markdown`, `x-code-editor`, `x-dashboard`
   switch to a ~20-line built-in toy highlighter (keyword-list over their
   own sample text) — better pedagogy anyway, since it demonstrates the
   seam is backend-neutral and keeps mosaic's examples self-contained. The
   real tree-sitter integration example moves to the tree-sitter
   repository, which dev-pins mosaic for that example only; the cookbook
   page links to it.
4. **spice/mentat**: repoint the tree-sitter pin to the new repository
   (`spice/dune-project:30-33`); `lib/tui`'s direct grammar deps and the
   dependency-law allowlist (`test/tools/test_tools_dependency_laws.ml:322`)
   are unchanged by the move itself. Optionally: replace
   `code_highlight.ml`'s resolver with the cookbook shape once §4.1's
   uniform grammar modules land.
5. **Repo docs**: update `AGENTS.md`/`CLAUDE.local.md` (tree-sitter listed
   as an in-repo project with `./tree-sitter/` paths), `README.md`, and CI
   configuration that builds or tests the tree-sitter tree.
6. **Verification**: cold-build timing of `dune build @install` before and
   after (the ~36 MB of generated C is the expected win); `dune runtest`
   on all remaining trees; a scratch consumer project depending on
   `mosaic` alone must build without any C grammar compilation.

## 7. Open questions

- **Q1 — Grammar module granularity.** One module per grammar
  (`Tree_sitter_ocaml.Ocaml` / `.Interface` / `.Type`) as proposed, or one
  per package with a language argument? Per-grammar modules keep `GRAMMAR`
  first-class (passable as `(module G)`) and are recommended; confirm the
  spelling reads well at mentat's call sites.
- **Q2 — Opam naming.** `tree-sitter` is a generic name; community bindings
  exist (e.g. the semgrep lineage). Decide the published name before the
  first opam release of either package; the pin-based ecosystem is
  unaffected meanwhile.
- **Q3 — The base scope set.** Confirm §3-R4's proposed list against the
  shipped grammar queries and `Syntax_style.default`; decide whether
  unknown first segments are dropped, styled as base, or a documented
  extension point.
- **Q4 — `threaded` cancellation semantics.** Worker threads cannot be
  killed; `cancel` abandons the generation but the thread runs to
  completion. Acceptable for highlight workloads (bounded by content size),
  but say so in the doc, and decide whether a shared single worker or
  thread-per-request is the implementation.
- **Q5 — Timing with RFC 0001.** The extraction (§6) is independent and can
  land immediately. The §4 API pass touches surfaces RFC 0001 will move
  into the widget-state model; decide whether §4.2 lands on the current API
  or waits to land once, on the new one.
