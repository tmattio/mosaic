# RFC 0001: Single-owner state — the application owns interactive state

- Status: Decided 2026-08-21 and scoped 2026-09-24 (the maintainer's direction). Implemented on
  2026-09-24, `3e045c29` through `163cd6fa`. Finished on 2026-10-22 and 23, `3d3125ba` through
  `818cd9d1`, which fixed what an audit of the implementation found and closed the latency gate
  (§6). This text states what was built, including the decisions taken during implementation
  (§3). Mentat's migration belongs to the spice repository and is not part of it.
- Baseline: mosaic @ `712ad398` for the evidence; spice/mentat @ `e8506bc9`; OpenTUI @
  `4b1474e009bf78365a7a4866e83a9e522533ef42`, a datapoint. The parent of the first implementing
  commit is `b46d699a`.
- Process: three audits on 2026-08-21 (Mentat's friction, mosaic's fix history, the API), then the
  maintainer's decision. No blind design, skeptic or reference audit: it predates raven's
  "Becoming certain". On 2026-10-22 an audit of the implementation against this text found memo
  identity aliasing, an `on_size` loop that never presented a frame, configuration compared by
  physical equality, migration leftovers, an editor viewport owned by the widget, undo history
  bounded by count, and per-input cost linear in mounted content. Each fix follows a published
  design: Elm's `Html.Lazy`, CSS Resize Observer, prosemirror-history.
- Relation: [RFC 0002](0002-syntax-highlighting-boundary.md) delivered configuration compared by
  value (its R6). [RFC 0003](0003-text-layout.md) owns text geometry, replaces this RFC's promise to
  preserve text internals, and required revision-safe proposals until its D5 was amended.
- Compatibility: breaking, before 1.0. The breaks are in §5.

## Summary

Mosaic ran an Elm loop over retained widgets that moved on their own (keys, clicks, wheel, sticky
scroll). The reconciler decided whether to write a widget's state by comparing the new declaration
with the old one, never with the widget. A widget the application forgot to round-trip diverged
from the model silently; one forced from props reverted the user's typing. Every fix at that seam
traded one failure for the other. The decision:

> **Declarative widgets take their accepted interaction state from a plain value in the
> application's model; the widget renders it, and when an event would change it, the framework
> hands the application the entire proposed next value as a message. Storing that value is the
> only way anything moves.**

- **The model owns interaction state**: editors, scroll positions, selections, tree expansion,
  focus. The retained tree is the renderer's cache, so `~key` affects cost, never behavior.
- **The framework proposes, the model disposes.** Transitions that need geometry run at dispatch
  from the accepted value; the application never measures.
- **Unchanged content costs nothing per input.** A keystroke behind a 20,000-block transcript went
  from 64.7 ms to 0.28 ms, and no longer depends on the transcript's length (§6).

Labelled constructor arguments, layout defaults, MLX, the widget catalog, `embed` and retained
rendering stay.

## 1. Background: the evidence

From the 2026-08-21 audits, at `712ad398` and spice @ `e8506bc9`:
- **Mentat paid for the seam.** Its TUI (34,685 lines) had already been rewritten once on Mosaic,
  and the friction stayed. It kept shadow flags to stay in phase with retained widgets
  (`widget_desync`, `shell_trigger_pending`), a keyed request, acknowledgement and serial counter to
  page its transcript, and a parallel focus model that never called `Cmd.focus`. A lost focus could
  not be recovered (`ISSUES.md:6` [spice]).
- **Mosaic's fixes clustered at the seam.** `create` against `apply_props`: five fixes across seven
  widgets in four months (`a4cb678f` through `712ad398`). Node identity: four fixes (`764760da`,
  `140eaa8f`, `68daf04f`). Frame non-atomicity: four (`384cf945`, `d3cc62bb`, `9f10bcff`).
- **The API had two opposite policies.** `Text_input` and `Textarea` were overwritten from props on
  every reconcile, so both shipped editing examples reverted typing; the other sixteen widget kinds
  were written only when a prop changed. Scroll had no position prop, only keyed one-shot requests
  the application had to retire. An `empty` sibling was spliced out before positional matching, so
  the next sibling inherited the previous widget's buffer and focus.
- **Sizing** (web flexbox defaults; 7 of Mentat's 10 framework-caused fixes) was diagnosed by the
  same audits. Its remedy was withdrawn from this RFC (§4).

## 2. The decision

Confidence: high where a reference or §1 decides; medium for a Mosaic choice resting on its reason.
Tests are in `mosaic/test/` unless stated.

| decision | rule | reference / reason | confidence | pinned by |
|---|---|---|---|---|
| Owner | A declarative widget renders the accepted value it is given (`~state`, `~scroll`, `~selected`, `~focused`) and reports the whole proposed next value through a semantic callback. Nothing moves until `update` stores it. Low-level renderables and `embed` belong to their integrator | §1: each fix at the seam traded silent divergence for forced revert | high | "normalization is authoritative before the next key in a burst" (`test_loop.ml`) |
| Editor | `Editor.t` is a persistent value: text, caret with affinity, selection, undo history, viewport. Caret-only proposals share the text and its index; content effects compare `Editor.text` | RFC 0003 §2.4 | high | `test_editor.ml` |
| Scroll | `Scroll.t` is `at`, `follow` or `reveal`, resolved against real extents at render, so it needs no write-back and nothing to retire | removes the request and acknowledgement protocol | high | "symbolic follow reflow", "symbolic reveal is idempotent" (`test_scroll_box.ml`) |
| Selection widgets | `select`, `tab_select` and `table` take `~selected:'a option ~equal`, with items carrying application values; reordering keeps the selection by value; `None` shows none. A tree keeps expansion and selection in `Tree.State.t` and reports items | callbacks reported indices into widget-private lists | high | "reorder, filter and accepted activation", "absent and empty selection" (`test_value_selection.ml`) |
| Callbacks | Semantic callbacks (`on_change`, `on_scroll`, `on_submit`, `on_activate`) return `'msg`; raw event callbacks return `'msg option`, `None` filtering the event | a proposal the widget already identified needs no option | high | `test_button.ml` |
| Dispatch | Inputs run in order. An input's messages go through `update`, and declarations refresh from the accepted model before the next transition; subscriptions share that order. `Sub.on_key` and `on_paste` capture before the focused handlers and the default action; `prevent_default` cancels the default, `stop_propagation` the traversal | a rejected proposal must never seed the next event. OpenTUI's `KeyHandler` order (datapoint) | high | "global capture updates focus and editor before default editing", "paste capture runs before the focused editor" (`test_loop.ml`) |
| Geometry | Wheel, paging, wrapped cursor motion and hit-to-caret run framework-side from the accepted value and the current layout. Layout reaches the model only through `on_size` and the geometry baked into proposals; nothing rendered feeds back into intrinsic measurement | Mentat measured in application code, against spice RFC 0016 §9.3 | high | "accepted scrolling leaves follow until the model follows again" (`test_scroll_box.ml`) |
| `on_size` termination | Between two paints, once a layout pass has delivered reports, later passes deliver only nodes deeper than the shallowest reported. A refused report waits for the next frame | W3C Resize Observer §3.4.1, §3.6.1: "In each iteration, only nodes deeper than the shallowest node in previous iteration can notify." | high | "on_size reports settle within the frame or the next one", "an on_size response that resizes its own node presents frames" (`test_loop.ml`) |
| Focus | A model field, declared with `~focused`. Several visible focused nodes raise; hidden subtrees do not count; with none, only global capture sees keys. A press asks `~on_focus_request` first, and caret and selection defaults run only if that live node is then focused. `Focus.next` and `prev ~equal` traverse an application order | Mentat lost focus for good | high | "duplicate visible focus is rejected", "hidden subtrees do not contribute focus" (`test_reconciler.ml`); "rejected or unmounted focus targets cannot edit or steal focus" (`test_loop.ml`); `test_focus.ml` |
| Keys | `~key` is non-semantic. Every value that depends on interaction history is in the model; the rest is computed from it | identity caused state transplants (§1) | high | "remounted surface keeps the wheel viewport" (`test_textarea.ml`); "… viewport follows the accepted value" (`test_value_selection.ml`) |
| `memo` | `memo ~key view` matches only the same view function, compared physically, with an equal key and the same message mappers. Keys compare by representation (floats by their bits); a key holding a function, object or abstract value raises at `memo`. An equal memo's children are not visited | Elm's `Html.Lazy` "checks to see if all the arguments are equal by reference", the function among them | high | "memos in one slot keep their views", "view function is part of identity", "keys of different types do not alias", "functions in keys are rejected at memo" (`test_reconciler.ml`) |
| Configuration | Props compare by value. Only a highlighter compares by identity: it is a resource | RFC 0002 R6 | high | RFC 0002's tests |
| Subscriptions | Every kind accumulates in declaration order. `Sub.every` and `on_tick` take an `~id`; a duplicate raises | timers were re-paired by interval and declaration order | high | "non-key subscriptions accumulate in declaration order", "timer ids must be unique across every and on_tick" (`test_loop.ml`) |
| Presentation | Input, update and refresh may repeat before a frame; paint presents once. Work that would dirty a frame mid-frame resolves inside layout and paint | four non-atomicity fixes (§1) | high | "in-frame geometry changes settle without input" (`test_loop.ml`) |
| Undo | An entry holds the inverse change: the bytes replaced, where, and the caret, affinity and selection to restore. No count bound. Since 2026-10-23 typing is grouped with no clock: an edit joins the open step when it is of the same kind (typing, backward or forward deletion) and continues it at the caret with nothing in between; a word boundary, a line break, a paste and word or line deletions start their own | prosemirror-history (`src/history.ts`): items hold "the inverted step"; grouping as VS Code's `shouldPushStackElementBetween` (`cursorTypeEditOperations.ts`) | high | "matches snapshot history over edit scripts" (property, `test_editor.ml`), the "Undo steps" tests |
| Stale proposals | None arise: the surface proposes from the value it was given and dispatch is synchronous. `Editor.t` is a plain value; `Editor.accept`, which discarded a proposal whose base was not the current value, was removed on 2026-10-23 (RFC 0003 D5) | RFC 0003 D5 | high | "normalization is authoritative before the next key in a burst" (`test_loop.ml`), "values carry no identity" (`test_editor.ml`), "a deferred proposal is a complete value" (`test_textarea.ml`) |
| Surface | Labelled layout and paint arguments, layout defaults and MLX are kept | state ownership needs none of them changed (§4) | medium | `test_mlx.ml`, the examples |

## 3. Decisions taken during implementation

The commits named give each decision's reasons and tests.

On 2026-09-24:
- **One shortcut resolver** (`360a9195`, `569b5d59`) for single shortcuts, widget keymaps and
  subscriptions: the key, then lowercase ASCII, then the base-layout key; a later equivalent
  binding overrides an earlier one, so `Sub.on_keys` takes the last match. Bindings are
  `Shortcut.t`; navigation runs after raw handlers, so `prevent_default` cancels it.
- **Buttons** (`b40c3af7`) compose box and text. They activate on a primary press and release
  inside without a drag, or on an unmodified Enter or Space press, never a repeat or a release.
  Focus adds inverse video; `~disabled` adds dim and suppresses activation and focus requests.
- **Tree setters install state** without calling `on_change` (`0785fbec`); gestures propose.
- **One text content owner** (`f13dc329`): plain text, fragments or styled spans; applying props
  replaces content set imperatively.
- **Scroll boxes own their scrollbars' geometry** (`9c13d4c0`): axis, range, position and
  visibility; an override carries appearance only.
- **`rich_text`** (`163cd6fa`): fragments in one text widget, so wrapping and selection span them;
  plain and rich declarations reuse the node.
- **Declared focus only** (`d60f2bd8`): `Vnode.autofocus` and the mount-time focus queue go.

On 2026-10-22 and 23, after the audit:
- **Memo identity** (`3d3125ba`), as in §2. The first implementation matched a memo by its position
  among its parent's memos and a key compared as an untyped value: removing the second of two memos
  showed the removed one's view, and `()`, `0`, `None` and `[]` were one key.
- **Memo spans** (`80d7e0c3`): a memo owns its span of its parent's children. An equal memo leaves
  the span unvisited, a changed one reconciles only the span, and a new viewport width re-resolves
  the cached view without rebuilding it.
- **The renderer owns the `on_size` rule** (`31526e5a`), since it owns frames and traversal. A
  response that keeps resizing its own node alternates on every frame and presents each.
- **Toffee rounds only what changed** (`99f55538`): a node's rounded layout is a function of its
  unrounded layout and its parent's position. Toffee now promises that a node's layout value is
  replaced exactly when it, its position or a layout below it changed (`1941fbbf`).
- **Culling in the parent's coordinates** (`e068612c`), where scrolling moves nothing: an index of
  visible children sorted by paint region, kept while their layout values are the same values. A
  child's paint region grows by its descendants' on the axes it does not clip.
- **The editor viewport is interaction state** (`3dc1cd12`): it depends on the history of wheel
  gestures, so no rendering rule can recompute it. It is in `Editor.t`; the wheel proposes, and an
  edit follows the caret from the viewport shown.
- **List viewports are computed** (`d1391b1d`): a function of the accepted selection, the items and
  the widget's size, with no owner.
- **One click-focus regime** (`81a06fa8`): a press invokes the nearest focusable node's request; a
  program that owns focus imperatively installs a request that calls `Renderer.focus`.
- **Migration leftovers removed**: `Vnode`'s `~id` and `~ref` (`eddc8ab8`); `Edit_buffer` and
  `Editor.Private` (`6158620e`), the engine now internal to `editor.ml`.
- **Undo as inverse changes** (`7ed70ef9`), as in §2; typing was not grouped until 2026-10-23,
  when steps came to join typing and same-direction deletion without a clock (§7).
- **One layout per primary frame** (`d5fd4857`): `Renderer.layout_frame` and `paint_frame` split
  `render_frame`, so the rows a frame requires and the frame share one layout.
- **Input hit-tests the command list** (`85611be2`) instead of a second hit grid, and the frame's
  hit grid fills with plain stores (`68b9e5ba`).
- **Text follows the screen's width method** (`818cd9d1`): no buffer holds one; widgets read the
  screen's at each measurement and paint, and the renderer re-measures when it changes.
- **Markdown's element style is a value** (`e1b2ad00`), completing RFC 0002 R6 with `3acfbf30` and
  `b16067a5`.

## 4. What was weighed and rejected

- **The status quo plus patching.** Five months of principled fixes at the seam reached no fixed
  point, because the seam is the defect.
- **Retained widget ownership** (OpenTUI's core under a declarative adapter). Coherent, and keeps
  TEA for domain state without signals, but widget identity owns interaction state, so a model
  snapshot no longer captures it.
- **Application-owned mutable editors** (`textarea ~editor:model.editor`). One object survives
  remounting, but old models alias new interaction state, so whole-model replay is lost.
- **Signals** (OpenTUI with Solid). Replaces the TEA contract to gain nothing retained ownership
  lacks.
- **Bubbletea-literal** (raw events, string widgets). Forfeits Toffee layout and hit testing, which
  Mentat uses.
- **Nested TEA per widget.** Mapping ceremony at every embedding; callbacks give the same purity.
- **Application-side transitions** (`Editor.handle_key` in `update`). Wrapped motion and wheel
  scrolling need geometry, so applications would measure.
- **A new styling API and flex defaults** (an attribute list, TUI-first defaults), withdrawn on
  2026-09-24: shorter signatures alone are no user benefit. The sizing fault of §1 is tracked
  nowhere: no file in this repository and no other RFC owns it.
- **A memo matched by position and key** (the first implementation): aliasing (§3).
- **Reporting each node's size at most once per frame.** It terminates too, but defers a response
  that only resizes descendants, which the depth rule settles within the frame.
- **The editor viewport as a renderer cache.** It depends on gesture history (§3).
- **Undo bounded by count** (200 whole-text snapshots). An arbitrary limit that still retained
  209 MB for a 1 MB document; an inverse change costs the size of its edit.

## 5. Behavior breaks

- Interactive values live in the model; semantic callbacks return messages; selection widgets take
  application values (`2e4b1b98`).
- `Cmd.focus`, `~id`, `~autofocus`, `Vnode.autofocus` and the one-frame retry are removed
  (`2e4b1b98`, `d60f2bd8`); `Vnode` constructors lose `~id` and `~ref` (`eddc8ab8`).
- `Renderer.dispatch_mouse` loses `?autofocus` and never focuses by itself (`81a06fa8`).
- Editor bindings are `Shortcut.t` pairs; string bindings and `key_aliases` go (`360a9195`).
  `Sub.on_keys` takes the last matching binding (`569b5d59`).
- Tree setters no longer call `on_change` (`0785fbec`). Applying text props replaces imperative
  content (`f13dc329`). Scrollbar overrides are appearance only (`9c13d4c0`).
- A memo's view function is part of its identity, so a closure allocated per render never
  matches; `memo` raises on a function, object or abstract value in its key; `Vnode.memo_key` and
  the memo record's fields go (`3d3125ba`).
- `Toffee_tree.ROUND_TREE` requires `is_rounded`, and `set_final_layout` takes the parent's
  position (`99f55538`).
- `Mosaic_ui.Edit_buffer` and `Editor.Private` are removed (`6158620e`).
- A wheel over an editor scrolls only once the application stores the proposal (`3dc1cd12`).
- The undo history has no 200-entry limit (`7ed70ef9`).
- `Renderable.Private.children_in_viewport` and `iter_children_in_viewport` lose `~padding`
  (`e068612c`). `Renderer.render_frame` loses `?layout_height`; a host that lays out against more
  rows than it draws calls `layout_frame` and `paint_frame` (`d5fd4857`).
- `Text_buffer.create` loses `?width_method`; `width_method` and `set_width_method` go
  (`818cd9d1`). Markdown's style breaks are RFC 0002's.

## 6. Verification

§2 names the tests that pin each rule. Property tests compare incremental rounding with a full pass
(`toffee/test/test_toffee.ml`), culling with a scan of every child (`test_renderer.ml`), and undo
with a snapshot history (`test_editor.ml`). `dune build @mosaic/test/runtest @toffee/test/runtest
@matrix/test/runtest` passes.

**The latency gate is closed.** `mosaic/bench/bench_runtime.ml` drives input, update, view,
layout, paint and terminal output through `Matrix_test` to the presented frame. Each block has
three text rows behind a scroll box; a wheel sample is a burst of eight events down and eight up.
Intel Core Ultra 5 235, OCaml 5.5.1, release, load about 2; medians of three runs each.

| workload | `739cf2a1` | words | `cdc4fac2` | words |
|---|---|---|---|---|
| keystroke to presented frame, 5,000 blocks | 16.2 ms | 3.85 M | 0.28 ms | 34.5 k |
| keystroke to presented frame, 20,000 blocks | 64.7 ms | 15.3 M | 0.28 ms | 34.5 k |
| wheel round trip, 5,000 blocks | 48.7 ms | 4.07 M | 0.76 ms | 154.8 k |
| wheel round trip, 20,000 blocks | 234.7 ms | 15.5 M | 0.76 ms | 154.8 k |
| keystroke, primary mode, 5,000 blocks | — | — | 0.18 ms | 25.3 k |
| wheel round trip, primary mode, 5,000 blocks | — | — | 0.49 ms | 104.1 k |
| resize 249×69 ↔ 250×70, 5,000 blocks | 258 ms | 143 M | 259 ms | 144 M |

At `739cf2a1` rounding was 92% of a keystroke's allocation, and reconciling the memoized transcript
44% of a wheel event's. Incremental rounding and memo spans took the 5,000-block keystroke to
1.7 ms and the wheel to 14 ms (`80d7e0c3`). Culling in the parent's coordinates made both
independent of the block count (`e068612c`). Plain stores into the hit grid and hit testing against
the command list took the wheel from about 9 ms to 0.76 ms and the keystroke to 0.28 ms
(`68b9e5ba`, `85611be2`). Promotion is sampled: about 240 words per keystroke and 550 per wheel
round trip. A resize reflows every block, so it stays linear in mounted content.

Memory and allocation, re-measured on 2026-10-23:
- Undo: a 1 MB editor after 250 one-byte inserts retained 209.1 MB before `7ed70ef9` and 9.0 MB
  after, the text and its index; 200 middle inserts into 1 MB retain 9,022,968 bytes, where they
  retained 208,042,136 (`bench_text_layout.exe history`).
- Editor: a left-right cursor pair allocates 56 words at 128 bytes and at 1 MB; a 1 MB insertion
  1,312,586 words, one flat copy.
- Selection: declaring 5,000 values allocates 20,128 words for `select` (20,126 for tabs, 20,145
  for a table) against 53 positional, about four words per item; a tree selection round trip
  allocates 52 words at 1,000 and at 10,000 rows.

`mosaic/bench/{runtime,editor,selection}.thumper` hold each case's allocation within 5% in
`dune runtest`. Baselines are recorded per machine: runtime has this machine's and an Apple
M1 Max's; editor and selection only the M1 Max's, so on another host they propose a baseline
instead of checking one.

The decided text compared allocations with an archive of `40c094cd`. That commit is in no ref of
this repository, so its figures cannot be reproduced and are not kept.

## 7. Open

- ~~Should adjacent typing be grouped into one undo step, as prosemirror-history does within
  `newGroupDelay`, when an `Editor.t` observes no clock?~~ Yes, on 2026-10-23, by VS Code's
  clockless rule: same kind, continued at the caret with no caret move between, a new step at a
  word boundary or a line break; a paste is a step of its own. ProseMirror's and CodeMirror's
  500 ms window and Emacs's count of 20 amalgamated commands were not taken.
- Should the runtime report an `on_size` response deferred on every frame, as Resize Observer's
  loop error does, rather than alternate silently?
- ~~Should `Editor.t` become a plain value?~~ Yes, on 2026-10-23: its acceptance tokens and
  `Editor.accept` went, as delivery is synchronous (RFC 0003 D5, amended).
- Is Toffee's promise that an unchanged layout value means an unchanged subtree an acceptable
  public contract, or should culling learn that fact through a narrower interface?
