# RFC 0003: Text layout — one source-backed geometry for every text consumer

- Status: Decided 2026-09-24 (maintainer's approval). Implemented the same day, `766eb0e3`
  (`Matrix_text.Layout`) through `07d38086`; diff moved onto the pane's source index afterwards.
  §3 records the decisions taken during implementation and the gaps.
- Baseline: mosaic @ `8e31eee7`, the parent of `766eb0e3` (the audit's working-tree hashes
  `7a6454d5`–`168d4772` are not in the history); OpenTUI @
  `4b1474e009bf78365a7a4866e83a9e522533ef42`, a datapoint.
- Process: a source audit of Mosaic and OpenTUI, then approval; no blind design, skeptic or
  reference audit (it predates raven's "Becoming certain"). On 2026-10-22 the §1 defects were
  reproduced at the baseline and both trees benchmarked (§6).
- Relation: [RFC 0001](0001-single-owner-state.md) owns application state,
  [RFC 0002](0002-syntax-highlighting-boundary.md) the highlighter boundary;
  [RFC 0004](0004-terminal-emulation.md) renamed the display-unit functions.
- Compatibility: breaking. Display-line inspection and the cursor's type change. Amends RFC 0001's
  promise to preserve text internals and requires revision-safe proposals; its model ownership and
  Toffee sizing stand.

## Summary

Copied display strings served as both what to paint and where the paint came from; truncation,
zero-width clusters and style boundaries broke the second use, and each consumer rebuilt
coordinates in its own units. The decision:
- **A source owns the installed bytes and their grapheme and line index; a layout maps it to rows
  and cells** for one width and policy. Both live in `matrix.text`, with no styles or widgets.
- **Every consumer asks that layout**: measurement, paint, selection, editor movement, gutters,
  diff.
- **Mosaic keeps policy**: styles, viewport, Toffee probes, cache retention, gestures.

Indexing, reflow and flat edits stay linear. Repeated paint follows the visible area: 20 rows of a
100,000-line text went from 6.9 ms to 9.9 µs (§6).

## 1. Background: the evidence

Reproduced at `8e31eee7` by a probe that installs text in a `Text_surface`, renders it, and drags
a selection with `set_local_selection`:

| input | baseline | main |
|---|---|---|
| `abcdef\nXY`, no wrap, width 3, truncation | Rows `ab…`, `XY`; row 2 started at grapheme 4, not 7 (`compute_display_info` advanced by displayed graphemes). Dragging over `XY` copied `ef` | row 2 at 7; copies `XY` |
| `a` U+200B `b` | Paint counted only width-bearing graphemes: dragging over the cell showing `b` copied U+200B; over `ab`, `a`+U+200B | copies `b`; all three |
| `e` (red) + U+0301`x` | Each span painted and wrapped alone: the grid showed `e` without its accent; at width 1, row 2 started at grapheme 2 (unstyled: 1) | `é` in one cell; row 2 at 1 |
| 👩 (red) + ZWJ💻`x` | Two emoji in 4 cells, `x` at column 4 (unstyled: one 2-cell cluster, `x` at 2) | one 2-cell cluster |

Repeated work at the baseline (§6): painting 20 visible rows cost 75 µs for 1,000 lines and
6.9 ms for 100,000; scrolling to a line's end, 0.34 ms at 100 KB and 3.4 ms at 1 MB; two word-wrap
reflows of a 100 KB line of 10,000 styled spans, 175 ms. Read but not timed: selection scanned
offscreen text, highlights were sorted per paint, the editor copied source prefixes.

Kept, not counted as gains: the four-entry layout cache shared by measurement and paint,
max-content measurement independent of the previous width, the ordered logical-line sweep, the
early return for fully clipped text, resize callbacks on both diff panes.

**Diagnosis.** Truncation changed what was painted without keeping where it came from, and
walkers rebuilt coordinates in different units and segmentations. What was missing is a relation
from the installed buffer to terminal geometry, not a document model: layout cannot recover bytes
its input lacks. **Source means the exact text installed in Text_buffer**: the supplied text for
Text, the text after input and length policies for editors, the materialized text for concealed
Code, Markdown children and diff panes.

## 2. The decision

Confidence: high where a reference or §1 decides; medium for a Mosaic choice resting on its
reason.

### 2.1 Placement and surface

| decision | rule | reference / reason | confidence |
|---|---|---|---|
| Placement | `Matrix_text.Layout` in the existing `matrix.text`; no Renderable, style or interaction state, which Mosaic owns with the viewport, Toffee probes and cache retention | Low-level consumers need the geometry; a library uses only those below it. OpenTUI's editor and grid share one buffer view (datapoint) | high |
| Source, layout | `Layout.Source.t`: immutable bytes, width-independent grapheme and line index. `Layout.t`: rows for one width, width method, tab width, wrap, truncation. Widths share a source; arrays and caches are hidden | one index per text | high |
| Rows | A source range and cumulative columns, never a substring. A trailing ellipsis holds an omitted-source range | §1 | high |
| Queries | Construction, measurement, bounded rows, byte/grapheme conversion, `coverage`, `hit`, `locate`, `iter_cells`. Mosaic decides what a gesture does and proposes it under RFC 0001 | — | high |
| Row access | Counts, and bounded iteration over absolute visual rows of one geometry revision, giving row, logical line and continuation; adapters convert viewport coordinates. Full traversal stays for diff alignment; arrays are immutable | a gutter needs only visible rows | high |
| Caret | `{ grapheme; affinity }`: RFC 0001's interaction value plus soft-wrap affinity | §2.4 | high |
| Storage | Flat strings; no storage-provider interface. A later representation meets the same contracts (flattening a rope per event needs its own cost case). RFC 0001's persistent editor is unaffected | no second implementation | high |

### 2.2 Bytes, graphemes and cells

| decision | rule | reference / reason | confidence |
|---|---|---|---|
| Units | Bytes give half-open ranges; grapheme boundaries are editing positions, independent of width policy; cells are columns. A display-unit count (`position.units`, `iter_units`) never becomes an editor index | UAX #29 §3: user-perceived characters "correspond to the concept of grapheme clusters" | high |
| Styles | Segment the concatenated text, then style. A cluster takes the style at its first byte | §1 rows 3–4 | high |
| Width | `Unicode` and `Unicode_wide` measure the whole cluster. `Wcwidth` sums its display units (a scalar of positive width plus the zero-width scalars after it), i.e. its scalars' widths. Boundaries are the same under every method | `matrix_text.mli`; RFC 0004 | high |
| Tabs | A TAB is `tab_width` cells, at least 1; no column-relative stops | Layout must match `Matrix_grid.draw_text`, which paints `tab_width` cells. Constant widths keep columns a prefix sum, so one column array serves every wrap width; a positional stop depends on its row's start | medium |
| Policy change | A new width method or tab width invalidates metrics, never positions | — | high |
| Emission | The grid is given the buffer's width method and tab width, never its own default. Paint and hit use the same whole-cluster column interval | one width fact | high |
| Indexing | Every cluster is indexed, zero-width ones included | §1 row 2 | high |
| Lines | CR, LF and CRLF each end a line; CRLF is two bytes, one grapheme. Empty text has one line; a trailing terminator adds an empty one | UAX #29 GB3: "Do not break between a CR and LF." | high |
| Bytes | Invalid UTF-8 follows Matrix's replacement boundaries; copy keeps the original bytes; nothing is normalized | copy returns what was installed | high |

### 2.3 Wrapping, measurement and truncation

| decision | rule | reference / reason | confidence |
|---|---|---|---|
| Char wrap | Never splits a grapheme | UAX #29 | high |
| Word wrap | Opportunities from `iter_wrap_breaks`, separators kept, grapheme fallback when no break fits. Not UAX #14; word movement unchanged. See D1 | the audited policy without its span walkers | medium |
| Overwide | A cluster wider than the row overflows one row; each positive-width wrap step consumes source. Width ≤ 0: one unwrapped row per line, nothing painted | termination, no lost text | high |
| One builder | Definite-width measurement and paint share one row builder at one width; `Layout.measure` uses a count-only sink. Explicit wrap width stays | OpenTUI's `.measure` mode (datapoint) | high |
| Intrinsic size | Max-content reads natural line widths, independent of truncation, previous width and probe order; min-content unchanged; probes cannot change the active geometry | kept from the baseline | high |
| Ellipsis | No wrap plus truncation: one trailing one-cell `…` covering the hidden suffix of its line (terminator excluded), never a grapheme; at width 1 it may stand for the whole line. Later lines start at their indexed positions; horizontal clipping transforms nothing | keeps Mosaic's ellipsis; OpenTUI's middle `...` is a datapoint not taken | medium |

### 2.4 Hit testing, selection and copy

| decision | rule | reference / reason | confidence |
|---|---|---|---|
| Selection | Half-open range of source graphemes. Copy slices its exact bytes: soft wraps add no newline, an ellipsis no character; ghost text, placeholders and gutters add nothing | §1 | high |
| Two queries | `hit`: the insertion boundary nearest a click. `coverage`: the source range under a cell | a caret is not a selected cell | high |
| Nearest boundary | Nearest to the clicked cell's left edge; a tie goes to the following boundary | Ties occur only inside a cluster wider than one cell; forward sends each of its cells to the nearer edge | medium |
| Drag | A zero-extent gesture is empty. A drag between distinct cells selects the hull of both cells' coverage, wide continuations and ellipsis included. Programmatic ranges are not expanded | dragging across a cell includes it | medium |
| Ellipsis | Inserts at its two source edges; covers its whole interval. Blanks past content snap to line end and own nothing | It alone stands for the hidden text: owning nothing, a drag across a truncated line would silently drop the rest. A press stays empty, so a click never selects hidden text | medium |
| Affinity | A soft-wrap boundary has two positions: `Upstream` ends the previous row, `Downstream` starts the next. `Visual_line_end` gives `Upstream`, `Visual_line_home` `Downstream`; a mouse hit keeps its row; an integer position is `Downstream`. Resize keeps affinity | otherwise End on a wrapped row lands on the next row | high |
| Its owner | The cursor's model state (`Editor`), never the layout | RFC 0001 | high |
| Zero-width runs | Coincident boundaries snap: `Upstream` to the earliest, `Downstream` to the latest. Invisible clusters stay reachable and copyable. A boundary in an omitted suffix locates at the ellipsis edge its affinity names | — | high |
| Round trip | Only visible, unambiguous boundaries round-trip through `hit` and `locate`; elsewhere snapping is canonical | zero-width runs and ellipses have no unique cell | high |

### 2.5 Revisions, edits, decoration and consumers

| decision | rule | reference / reason | confidence |
|---|---|---|---|
| Revisions | A geometry query names its source revision; a proposal, the complete state it came from (source, cursor, selection) | — | high |
| Stale proposals | The owner discards a proposal whose base is not its current state, even with unchanged text, or dispatch recomputes from the input. A stale proposal never restores old text or overwrites newer cursor or selection state. Applications need not retain layouts. See D5 | a queued next state cannot tell it is stale | high |
| Replacement | Follows the widget's reset and clamp policy; a layout keeps its source while referenced | — | high |
| Edits | Resolve endpoints to old-source bytes, splice, reindex. Rebase the caret from the byte after the insertion or at the deletion seam, snapping forward from inside a cluster; retained selection endpoints likewise. Never add the inserted fragment's grapheme count | U+0301 after `e` makes one cluster | high |
| Length limit | Applies to the resegmented result; positions clamp | — | high |
| History | Flat storage and undo stay, on this index; undo versions hold no layout caches | — | high |
| Cache keys | Geometry: content, width method, tab width, wrap, effective width, truncation. Styles and highlights: a paint revision. Selection and scroll rebuild nothing. Over-invalidation is allowed, stale rows are not. A materializing conceal change is a content change | — | high |
| Paint runs | Highlight order and overlaps resolved once per paint revision with the existing precedence; paint seeks the runs | §1 | high |
| RFC 0002 | Its byte-range producer and async boundary are unchanged | — | high |
| Gutters | Request only clipped rows | §1 | high |
| Diff | Positions from the pane's source index. Rebuilding alignment may traverse all rows when pane geometry changes; paint is bounded. Original line numbers stay apart from pane positions | — | high |

### 2.6 Cost

With N bytes, G graphemes, E style endpoints, R rows and H undo versions:

| decision | rule | reference / reason | confidence |
|---|---|---|---|
| Complexity | Indexing O(N); a new width O(G + R) by ordered sweeps, never rows × spans; flat edits O(N); memory O(N + G + E + R), plus O(H·N) for history (H ≤ 200). Packed arrays change constants, not order | §6 | high |
| Paint | Intersect the node with the grid's scissor before asking for rows; per row, seek columns and style runs and stop at the right edge; skip indexed zero-width runs. Repeated paint scales with visible rows, visible clusters and their bytes, and style transitions, plus logarithmic seeks; never with height, hidden prefix or total overlays. Batches may be copied: no zero-allocation promise | §6 | high |
| Highlights | Sorting is O(E log E); resolving A simultaneous overlays is O(E·A), stated where it is implemented (`text_buffer.ml`). Unchanged paint never repeats it | no algorithm here reaches O(E log E) total | high |
| Caches | At most four widths share one source index (D7 adds a byte bound). Probes use the count-only builder unless the layout exists, and their results go first. Replacement and unmount release obsolete references. An optional cache without repeatable benefit, or over its budget, is removed; the coordinate correction stays | — | medium |

### 2.7 Scope

Out of scope, as separate features: alignment, a new CJK break policy (but see D1), copying
unconcealed source, word or line click selection, positional tab stops, incremental editor storage,
and the rest of RFC 0001's migration. In scope: Text, materialized Code, the editor buffer and edit
surface, crop helpers, gutters, diff. No shipped consumer keeps another coordinate algorithm.

## 3. Decisions taken during implementation, and gaps

- **D1. CJK line opportunities** (`b46d699a`): `iter_wrap_breaks` gained OpenTUI's breaks between
  CJK graphemes (none before Kana combining marks, U+30A0 or U+30FB, or after a zero-width
  grapheme), departing from §2.3 and §2.7. Word movement keeps CJK runs whole, through the new
  `iter_word_breaks`.
- **D2. `Unicode_wide`** (`ad0006dc`): a third width method, for Ghostty, carried like the others.
- **D3. Thai and Lao spacing marks** (`25c4e2c6`): SARA AM and Lao AM widen their cluster;
  `น้ำ` is one two-cell cluster under `Unicode` and `Wcwidth`.
- **D4. `Renderable.line_info` survives** as a record of closures over one layout, beside
  `iter_lines`. No array is exposed, so access stays bounded; but nothing calls `iter_lines`: the
  gutter and diff read the closures, and the gutter visits only clipped rows.
- **D5. The acceptance guard is a function.** Editor values carry a revision and their base;
  `Editor.accept ~current proposal` returns `current` unless it is the proposal's base (tests
  "stale same-source proposals", "stale affinity proposal"). The edit surface proposes only from
  its current state, synchronously. An application deferring a proposal must call `accept`;
  dispatch does not enforce it.
- **D6. Source-bound annotations** (`07d38086`): `Text_buffer.Annotations` binds styles to byte
  ranges of the installed source, without copies; an interior boundary resolves downstream (the
  first-byte rule). Code installs highlights this way, so a theme change rebuilds no geometry.
- **D7. A byte bound on cached rows** (`5690a3f8`): besides four widths, cached rows share 32 MiB,
  and an oversized active layout is kept alone. Count-only probes have their own cache.
- **D8. No edit retains a selection**: insertion and deletion clear it, so the rule for retained
  endpoints has no case yet.
- **D9. Diff kept two coordinate algorithms** until `f6f86367`: it placed spans by counting each
  row's graphemes plus one for the LF, and took pane row i for logical line i. With a zero-width
  space before `ab`, a span on `b` colored `a` (fixed by `3f460f57`); a CRLF row shifted later
  spans by one; a final CR numbered a line the patch lacks; a lone CR inside a row shifted every
  later number, sign and color. Diff now maps rows, logical lines and graphemes through the pane
  buffer's own source index, and aligns split panes with `iter_lines`.
- **G1. The approved performance gates were never recorded**: no reference workloads; no
  provisional or final retained and peak memory budgets; no separate measurement of committed
  rows, probe rows, old and new sources, input spans, capacities and undo strings; no latency
  distributions (promoted words only sampled); no count of visited rows, clusters and runs; no
  predeclared tolerance for label regressions; no GC timing apart from reachability. §6 lists what
  exists.
- **G2. Untested**: a proposal made stale by a text change (cursor-, selection- and affinity-only
  changes are tested, through the same comparison); property tests of `hit` and `locate` with their
  preconditions.
- **G3. Diff limits left as they were**: span bytes refer to the unconcealed patch line while a
  concealing Code buffer holds other text, so emphasis can be misplaced; split panes align only
  when wrapping, so a lone-CR row misaligns them without wrap; a final CR leaves an empty,
  unnumbered row at the pane's end.

## 4. What was weighed and rejected

- **Patching the display spans.** Smallest, but separate editor, wrap and hit algorithms remain,
  so there is no single coordinate contract.
- **A private Mosaic kernel**, the strongest alternative, with a smaller public surface. Low-level
  consumers need the geometry too, and an abstract Matrix module hides its representation without
  depending upward, at the cost of a public contract earlier.
- **A package with a generic source provider.** New construction and lifetime concepts for every
  caller, for storage callbacks no second implementation needs.
- **A rope or piece tree with incremental layout** (OpenTUI's `UnifiedRope`, datapoint). It may
  help long-document edits and undo, but changes storage, history and invalidation without fixing
  the coordinates; it needs its own editor profile, starting from §6's flat-string numbers.
- **The original document plus a projection map.** It would let Code copy unconcealed text, at the
  price of overlap, boundary and provenance contracts nothing has today. The materialized buffer
  fixes every confirmed defect and keeps that limitation explicit.

The accepted cost is eager rows: layout and reflow stay linear and retain O(rows) metadata.

## 5. What it replaced

| before (baseline) | after |
|---|---|
| `Text_surface.compute_display_info` used displayed-grapheme counts as source offsets; `local_coords_to_offset` read them back | `Layout.row_start`, `hit`, `coverage` |
| `slice_spans`, `wrap_char_line`, `wrap_word_line`, `truncate_line` copied spans, segmenting each alone | one row builder over the concatenated source |
| Public `Text_surface.display_info` (copied lines, parallel arrays); `Renderable.line_info` arrays | `Text_surface.layout`; closures and `iter_lines` (D4) |
| `Text_buffer.ensure_grapheme_offsets`, per-line metric arrays, `line_spans` | `Text_buffer.source`, `styled_text` |
| The editor's own offsets, widths and line starts; `Edit_surface.offset_at_col` and prefix-copy cursor mapping | the editor's `Layout.Source.t`, shared with its buffer; `locate`, `hit` |
| Highlights sorted per paint, folded per grapheme | runs per paint revision, found by `style_at` |
| `Text_crop` walked and copied graphemes itself | `Source.columns`, `Source.slice` (`df879007`) |

Later, RFC 0004 renamed `iter_grapheme_info` to `iter_units` and `position.grapheme_count` to
`position.units`, and removed `prev_grapheme` (`bc29d791`, `fdd3030a`).

## 6. Performance and verification

`mosaic/bench/bench_text_layout.ml`, 2026-10-22, Intel Core Ultra 5 235, OCaml 5.5.1, release. The
ten cases whose APIs existed at `8e31eee7` were ported there and run interleaved with main, three
rounds each, on a shared host (load 3.4–4.6). Times are the range over rounds; allocation did not
vary.

| case | baseline | words | main | words |
|---|---|---|---|---|
| `index/create-1mb` (editor from 1 MB) | 9.05–9.25 ms | 2,000,097 | 3.82–4.14 ms | 1,000,055 |
| `index/set-text-1mb` | 8.92–9.42 ms | 2,000,087 | 4.56–4.89 ms | 1,000,039 |
| `reflow/label` (widths 79, 81) | 1.11 µs | 460 | 39 ns | 20 |
| `reflow/styled-100k` (10,000 spans) | 175.3–176.2 ms | 861,950 | 53–55 ns | 20 |
| `reflow/styled-100k-cold` (cache missed) | 175.3–175.6 ms | 861,954 | 324–328 µs | 27,951 |
| `edit/1mb-middle-undo` | 14.7–15.1 ms | 4,312,630 | 8.8–9.2 ms | 2,312,603 |
| `paint/tall-1k` (20 visible rows) | 74–76 µs | 141,471 | 9.8–9.9 µs | 1,819 |
| `paint/tall-100k` (20 visible rows) | 6.8–7.0 ms | 14,001,471 | 9.8–10.0 µs | 1,819 |
| `paint/horizontal-100k` (scrolled to end) | 335–339 µs | 778 | 3.9–4.1 µs | 118 |
| `paint/horizontal-1m` (scrolled to end) | 3.35–3.42 ms | 778 | 3.9 µs | 118 |

Paint no longer grows with hidden rows or prefix; 1 MB middle edits stay O(N) at half the
allocation. On `reflow/styled-100k` main hits its width cache and the baseline does not; the cold
row compares the builders. Main only, one run at load 10.7 on 14 cores:
`styles/nested-revision-200` 187 µs, 291,023 words; `styles/same-text-layout-100k` 139 µs, 28
words.

The recorded budgets, run by `dune runtest`:
- a 1 MB source at width 1 (a million rows) retains 80.0 MB, 80 bytes per grapheme; the heap
  peaks at 186.9 MB while a second layout is built. The check fails above 128 MiB or 256 MiB;
- 200 undo versions of 1 MB retain 208.0 MB, the O(H·N) of a flat history capped at 200; a weak
  array checks that no obsolete source index survives;
- `text_layout.thumper` holds each case's allocation within 5% of its recorded baseline;
- the 32 MiB row cache (D7).

Tests (`matrix/test/test_text.ml`, `mosaic/test/unit/`, `mosaic/test/expect/test_diff.ml`) pin the
§1 regressions ("truncation keeps later source offsets", "invisible clusters stay in source
ranges", "style boundaries and grid defaults cannot split geometry"), invalid UTF-8, CRLF,
zero-width snapping, every width method, widths 0, 1, 2 and 7, overwide clusters, the ellipsis,
affinity, clipping, combining-mark insertion, flag rejoining after deletion, and stale proposals
(G2). The
baseline's cache, probe and intrinsic-measurement tests pass unchanged; `dune build
@mosaic/test/unit/runtest @matrix/test/runtest` passes (one recorded expected failure).

## Appendix: OpenTUI datapoints

Read at `4b1474e0`; they informed the decision and do not decide it.

| what | where |
|---|---|
| Byte and column row windows (`VirtualChunk`) | [text-buffer-view.zig](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/text-buffer-view.zig#L138) |
| Editor and grid share one buffer view | [editor-view.zig](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/editor-view.zig#L38), [buffer.zig](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/buffer.zig#L1617) |
| `UnifiedRope` storage | [text-buffer.zig](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/text-buffer.zig#L55) |
| Count-only measurement (`measureForDimensions`) | [text-buffer-view.zig](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/text-buffer-view.zig#L1450) |
| Styled input segmented after joining | [text-buffer.zig](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/text-buffer.zig#L1131) |
| Gutter paints clipped rows | [LineNumberRenderable.ts](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/core/src/renderables/LineNumberRenderable.ts#L242) |

UAX #29 is cited from revision 49, <https://www.unicode.org/reports/tr29/>, §3 and §3.1.1.
