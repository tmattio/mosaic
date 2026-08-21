# RFC 0001: Single-owner state — re-founding Mosaic's application contract

- Status: Draft; input to the design pipeline. Evidence gathered 2026-08-21
  from three independent audits (Mentat friction, Mosaic fix-history
  forensics, Mosaic API/mental-model review), summarized in §1 and Appendix A.
- Audience: Mosaic maintainers; Mentat maintainers (spice RFC 0016 §9.3
  delegates all measurement, wrapping, truncation, and scrolling to Mosaic and
  is a direct consumer of this contract).
- Baselines: mosaic @ `712ad398`, spice/mentat @ `e8506bc9`, OpenTUI reference
  at `_opentui/packages/core`. Commit hashes below belong to the mosaic repo
  unless marked `[spice]`.
- Compatibility: none. This is a breaking re-founding of the `mosaic` contract
  layer (element API, reconciler contract, widget state ownership, focus,
  sizing defaults). Matrix, Toffee, the renderer core, and the widget
  internals (`Edit_buffer`, `Text_surface`, `Selection`) are preserved.
  `mosaic.mlx` is deleted (§8, Q8).

## Summary

Mosaic runs an Elm loop — immutable model, full re-render per frame — over
retained, self-mutating widget objects inherited from OpenTUI's design. The
boundary between the two, reconciliation, is unsound by construction: it
decides whether to write widget state by comparing the new declaration to the
old declaration, never the declaration to the widget. Since widgets move on
their own (keys, clicks, wheel, sticky scroll), every interactive widget
either silently diverges from the model or destroys the user's live edits,
and every fix at this seam trades one failure mode for the other (§2.1).
A second, independent fault compounds it: element sizing is faithful web
flexbox with web defaults, in which the universal TUI case — fill the
remaining rows, truncate, never collapse — requires four correlated optional
arguments and fails silently and non-locally (§2.2).

This RFC rules one principle and derives the API from it:

> **The framework holds no interactive state. Every widget's state is a plain
> value in the application's model; the widget renders it, and when an event
> would change it, the framework hands the application the entire proposed
> next value as a message. Storing that value is the only way anything
> moves.**

The retained renderable tree survives as the renderer's private output cache
— the role `Matrix.Screen`'s cell grid already plays — so a wrong diff is a
redraw glitch, never data loss. Layout-dependent transitions (wheel scrolling
against viewport extents, cursor motion in wrapped text) run framework-side
at dispatch time, where geometry lives, and deliver whole state values to
`update`: the application never measures ("framework proposes, model
disposes", §4.3). Sizing gets TUI-shaped semantics: column as the default
axis, a first-class `fill`, zero minimums on flexible children, and a hard
one-way rule between measurement and rendering (§4.5).

By construction this deletes: the controlled/uncontrolled distinction and
both of its failure modes; the `create`/`apply_props` dual path and its
seven-widget fix cycle; the keyed one-shot scroll request + `on_*_applied`
retirement protocol; `Cmd.focus`-by-string-id and its one-frame retry;
reconciliation-driven state loss and transplant; callbacks reporting indices
into widget-private lists; `~ref`; and every shadow-state flag in Mentat
(§6). Performance is preserved because Bubbletea's slowness came from its
render target (strings, no retained layer), not from state living in the
model; Mosaic keeps cell buffers, retained layout, and diff-driven damage
tracking, and the redesign is gated on benchmarks (§7).

## 1. Background: the evidence

Three audits ran independently on 2026-08-21 and converged. This section is
the data the pipeline should decide from; Appendix A carries the full tables.

### 1.1 The symptom, measured in Mentat

Mentat's TUI layer (`spice/lib/tui/`) is 34,685 lines across 117 files — the
hottest directory in the spice repo — with a fix-to-feature ratio of 2:1
(63 fix vs 32 feat over 47 days) and a near 1:1 golden-test layer needing
constant re-acceptance. The TUI has already been fully rewritten once
(2026-07-21..25, `35a66bf7a` [spice]: +19,028/−21,616 in `lib/tui` alone);
the predecessor was also Mosaic-based. Rewriting the application did not
remove the friction, which is strong evidence the friction is not the
application's.

Of 36 sampled fix diffs, 61% were pure application logic, 28% root-caused in
mosaic/toffee/matrix — and 7 of those 10 root-cause commits are layout/sizing.
The framework-caused pain is not diffuse; it is concentrated (§2).

Mentat's structural responses are themselves findings:

- Shadow state kept only to stay in phase with Mosaic's retained state:
  `widget_desync` and `shell_trigger_pending` flags on the composer (13 touch
  points; the comment reads "a controlled value push does not cancel an
  in-flight Mosaic cursor report"); a keyed request + ack-message + serial
  counter (`next_page`/`page`/`tail_reset` plus `Transcript_page_applied`/
  `Transcript_tail_reset_applied`) just to page the transcript; a parallel
  focus model (`strip_focus`, `drill_focus`, hand-written traversal) that
  calls `Cmd.focus` zero times.
- An open, unrecoverable bug (`ISSUES.md:6` [spice]): "I managed to lose the
  focus of the composer… there's nothing I can do to focus back the composer
  to type things. Tab doesn't work, click doesn't work."
- A formal governance policy for framework workarounds ("No workaround
  without a name … a single wrapper in one module — never scattered call-site
  hacks") and a shared quirk vocabulary
  (`project_mosaic_flex_truncate_quirk`, `project_mosaic_abspos_truncates`).
- The `fill_remaining`/`zero_size`/`min_size` sizing incantations copy-pasted
  68 times across 26 files; the review pane abandons flex entirely and
  hand-computes pixel widths, violating spice RFC 0016 §9.3.
- Wrapper modules (`scrollport`, `prims`, `prose`, `panel`, `pane`, `strip`)
  that are each a specification, written in application code, of what the
  framework API should have been.
- API usage profile: direct `Mosaic` (zero `.mlx` files), `text` ×863,
  `box` ×186, `input` ×156; `~flex_shrink` ×264, `~min_size` ×172,
  `px 0` ×129; `Cmd.focus` ×0.

### 1.2 Mosaic's own fix history

1,013 commits (2025-06-28..2026-08-20), 322 fix vs 167 feat. Since the
OpenTUI-based rewrite (`1f3d3bb8`, 2025-12-13), the fix epicentre is
`mosaic/lib/ui` — the renderable/renderer core — at 32% fix density, not the
widgets (26%), not the TEA layer (22%), not Toffee (10 commits, 1 fix since
the rewrite), not mlx (6%). Of ~71 post-rewrite fixes in `mosaic/lib`,
roughly 45 are semantic — the designed behavior itself was wrong or ambiguous
and had to be redefined — vs 26 implementation bugs. Several fixes rewrite
the `.mli` to match new semantics rather than the code to match the docs;
`renderer.mli` and `renderable.mli` each carry five fix commits of their own.

The semantic fixes cluster in exactly four places:

1. **Measure↔render feedback** (~16 fixes, the largest cluster). The
   recurring defect: a rendering result feeding back into a measurement
   input. `7c9902bd` names the mechanism — "a self-reinforcing fixed point:
   measure, layout, and the next measure all agree on the stale width, so a
   container that shrinks and grows back leaves the text narrow forever" —
   and the principle, discovered on the fourth attempt: "truncation is a
   render concern and must not feed back into intrinsic sizing." "What does
   a zero-sized box mean" was redefined four times in 30 days (`146bf73c` →
   `5723aaa3` → `01fd6dff` → `1891def3`), each fix caused by the previous,
   with an xfail still open at `test_renderer.ml:1695`. The last round
   shipped straight into Mentat as its two most recent commits
   (`304c2ca9a`, `e8506bc96` [spice]), re-declaring `min_size` floors on
   five surfaces.
2. **Reconciler node identity** (4 fixes). Duplicate keys resolved by "an
   accident of `Hashtbl` semantics" until pinned (`764760da`); `id` changes
   never diffed on reused fibers so `Cmd.focus` silently missed
   (`140eaa8f`); focus lost on keyed reorder (`68daf04f`).
3. **`create` vs `apply_props`** (5 fixes across 7 widgets over 4 months:
   `a4cb678f` Table/Tree → `28858d4d` Scroll_bar → `5522da8d`
   Select/Tree/Table/Tab_select/Slider → `712ad398` Scroll_box, current
   HEAD). `5522da8d` names the type-level cause: props typed as plain values
   with defaults make "unset" and "set to the default" indistinguishable —
   "navigating and then changing ANY other prop made `Props.equal` false,
   and `apply_props` reset the state to the default… the selection widgets
   echoed `on_change` with the app's own value, while Slider clobbered
   silently."
4. **Frame non-atomicity** (4 fixes). Layout dirties itself mid-frame
   (`384cf945`: scrollbar revealed "after child commands have already been
   collected", follow-up frame lost); render re-entrancy (`d3cc62bb`);
   doc-only contracts turned into enforced ones (`9f10bcff`); the test
   harness needs a four-pass settlement loop (`expect_harness.ml:138`).

Coverage caveat, for honesty: the 326-test expect suite had never executed
until 2026-08-17 (`c11448fd`: "each binary's main did nothing and exited 0");
coverage and mutation instrumentation landed the same day (`d555daa5`). Some
historical pain was blindness. But the semantic redefinitions above happened
in code the maintainer was actively studying; they are not artifacts of the
coverage gap.

### 1.3 The API as it stands

The mental-model audit's headline: explaining Mosaic honestly to a new user
takes a twelve-clause paragraph in which every clause is load-bearing.
Representative hazards (full list in Appendix A.3):

- **Two opposite reconciliation policies.** `reconciler.ml:251-258` marks
  `Text_input`/`Textarea` as live-controlled and force-overwrites them from
  props every reconcile — forget to round-trip a keystroke and the frame
  reverts the user's typing. The other sixteen widget kinds are written only
  when the declared prop changes (`reconciler.ml:288-390`) — forget one
  `on_change` arm and the widget disagrees with the model silently and
  permanently. Mosaic's own shipped examples fall in: `04-input` and
  `18-textarea` both pass a controlled `~value` they never feed back, so
  both text-editing demos revert typing. The behavior is deliberate and
  pinned (`test/unit/test_reconciler.ml:367-388`).
- **State you can only nudge.** `scroll_box` has no scroll-position prop; it
  is driven by keyed one-shot requests (`reveal`/`scroll_by`/`reset_sticky`)
  the application must retire from its model when `on_*_applied` fires.
  Reuse a key: nothing happens. Forget to retire: it replays. No example in
  the repo uses this API; Mentat implements the full protocol.
- **Identity leaks.** `empty` nodes are spliced out before positional
  matching (`reconciler.ml:850`), so flipping a conditional makes the next
  sibling inherit the previous widget's edit buffer, cursor, and focus — a
  state transplant, not a reset. `~key` (reconciliation) and `~id`
  (`Cmd.focus`) are separate namespaces; `Cmd.focus` runs before the view it
  targets exists and papers over it with a documented one-frame retry.
- **Machinery inconsistencies.** Five of thirteen `Sub` kinds are
  last-one-wins (`mosaic.ml:504-514`); `Sub.every` timers are re-paired by
  interval and declaration order (Mentat picks 2.0s vs 2.15s intervals so
  two timers never share a clock); element messages are queued while
  subscription messages dispatch synchronously, so one keystroke can apply
  its two updates in reverse order across two frames; markdown/syntax
  re-render gating uses physical equality of closures, which forced Mentat's
  memoization sub-architecture and still re-runs tree-sitter over the whole
  transcript per keystroke in the theme picker.
- **Surface.** 19 constructors × 50–76 optional arguments; `mosaic.mli` is
  2,864 lines, ~1,660 of which repeat the same fifty layout parameters
  nineteen times; `Renderable` exposes 65 mutating functions as a public
  escape hatch; no component abstraction beyond nested-TEA `map` boilerplate.

## 2. Diagnosis

### 2.1 Fault one: no single owner of interactive state

Mosaic straddles three paradigms: an Elm runtime, React-retained widgets
without React's controlled/uncontrolled discipline, and immediate-mode
escape hatches (`~ref`, `embed`, the `Renderable` surface). The seam is
narrow and structural: **the reconciler is the only writer of widget state,
and it decides whether to write by comparing two declarations to each other,
never the declaration to the widget.** The retained widget moves on its own;
that motion is invisible to a declaration-vs-declaration diff; so the widget
drifts and the reconciler never notices. The `has_live_controlled_value`
bypass for exactly two of eighteen widget kinds is an explicit
acknowledgement of the hole — and closing it uniformly would convert silent
divergence into forced-revert everywhere, which is the failure the examples
already demonstrate. Every fix at this seam trades one failure mode for the
other; §1.1's shadow state and §1.2's clusters 2–3 are this one fault seen
from the application and history sides.

OpenTUI itself does not have this fault: it is honestly retained. Its Solid
binding writes a property only when a signal fires and never re-asserts, so
uncontrolled is the natural state. Mosaic's divergence — full re-render TEA
over those same retained objects — created the seam. There are only two
coherent positions, and Mosaic occupies the one indefensible spot between
them.

### 2.2 Fault two: web flexbox with web defaults

Independent of the seam, and responsible for 7 of Mentat's 10
framework-root-caused fixes. The universal TUI layout has no safe default:
default flex direction is `Row` (so `~size:{height = px 0}` on a wrapper
became a cross-axis constraint and blanked the conversation view,
`2c003440b` [spice]); non-wrapping text's automatic minimum is its whole
content (so `~truncate:true ~flex_shrink:1.` does nothing without
`~min_size:{px 0; px 0}`, `f60c65934` [spice]); percentages against an
indefinite box silently fall back to content size. Mosaic's side of the same
fault is §1.2 cluster 1: measurement contaminated by rendering, four times.

### 2.3 Third tier: edges any design must also close

Frame non-atomicity (§1.2 cluster 4); three event-propagation rules (keys
focused-only and never bubbling, mouse bubbling without capture,
`prevent_default` meaning different things per event class); `Sub`
last-one-wins and timer identity; physical-equality memoization;
constructor-surface bloat. Real, but fixable inside whatever model is
chosen; listed so scope is honest.

### 2.4 What is not the problem

Toffee (near-inert since the rewrite); Matrix (ordinary terminal-plumbing
maturation); the reconciler's diff machinery (O(1) key matching, stable
handler closures, physical-equality subtree sharing — competent machinery
pointed at an unsound contract); the widget internals (`Edit_buffer`,
`Text_surface`, `Selection` are already value-shaped). And 61% of Mentat's
UI bugs are its own domain logic; no redesign touches those.

## 3. The ruling

Adopt the full-Elm position: all interactive state lives in the model as
plain values; widgets are pure functions of that state; the renderable tree
is disposable output. Rejected: the full-signals position (OpenTUI+Solid
fine-grained reactivity) — coherent, but abandons TEA, is a far larger
departure for OCaml, and gives up the property Mentat's architecture is
built on (one model, one update, replayable messages). Rejected: the status
quo plus patching — §1.2 shows five months of principled patching at the
seam converging on no fixed point, because the seam itself is the defect.

One consequence worth naming up front: **`~key` stops being semantic.** With
no state in the tree, reconciliation identity is purely a paint
optimization. The `empty`-splice transplant, duplicate-key remounting, and
focus-lost-on-reorder become unrepresentable rather than fixed.

## 4. The proposed surface

### 4.1 The application (unchanged shape)

```ocaml
type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> 'msg t;
  subscriptions : 'model -> 'msg Sub.t;
}
```

`Cmd` survives minus `Focus` (§4.4). `Sub` survives with two semantic fixes:
every kind accumulates (no last-one-wins), and `Sub.every`/`Sub.on_tick`
take an `~id` so timer identity stops depending on declaration order.

### 4.2 The widget pattern

Every stateful widget is the same shape: a module whose `t` is a persistent
value with pure operations, an element constructor taking `state:` and a
non-optional `on_change:`, and optional interception hooks.

```ocaml
module Editor : sig
  type t                              (* persistent: text, cursor, selection *)
  val empty : t
  val of_string : string -> t
  val text : t -> string
  val clear : t -> t
  val insert : string -> t -> t       (* pure edits for update-side use *)
  (* ... the Edit_buffer operations, as pure functions *)
end

val textarea :
  ?style:Style.attr list ->
  ?focused:bool ->                    (* derived from the model, §4.4 *)
  state:Editor.t ->
  on_change:(Editor.t -> 'msg option) ->  (* the ENTIRE next state *)
  ?on_key:(Event.key -> 'msg option) ->   (* Some consumes before default *)
  ?placeholder:string ->
  unit -> 'msg t

module Scroll : sig
  type t
  val at : int -> t
  val follow : [ `Top | `Bottom ] -> t -> t   (* symbolic, stable, idempotent *)
  val reveal : line:int -> t -> t             (* resolved at render *)
end

val scroll_box :
  ?style:Style.attr list -> ?focused:bool ->
  scroll:Scroll.t -> on_scroll:(Scroll.t -> 'msg option) ->
  'msg t list -> 'msg t
```

`select`, `tab_select`, `table`, `tree`, `slider` follow identically; for
`tree`, the expansion set becomes the application's value, so callbacks
report nodes, not indices into a widget-private flattened list.
`on_change` returning `None` rejects the proposal (read-only for free);
returning a modified value overrides it (input filtering for free).
`on_change` is a non-optional label so an inert widget is a type error, not
a silent runtime state.

### 4.3 Framework proposes, model disposes

Transition functions that need geometry — wheel scrolling against viewport
and content extents, PageDown, cursor motion across wrapped lines, mouse
hit-to-cursor mapping — run framework-side at dispatch time, where last
frame's layout lives. They read the state the application declared in the
last view, compute the whole next value, and deliver it to `update`. The
application never measures; there is never a second copy to drift. Symbolic
scroll positions (`Follow `Bottom`, `Reveal line`) are resolved against real
extents at render time each frame, so they are stable values requiring no
write-back and no retirement protocol. Applications that do need geometry as
data get it explicitly via `?on_size:(w:int -> h:int -> 'msg option)`
(ResizeObserver-shaped), never by reading the tree.

### 4.4 Focus

Focus is a model field. The view marks the focused node
(`~focused:(model.focus = `Composer)`); keys are delivered to the node
marked focused; clicks move focus only if a handler dispatches a message
saying so. `Cmd.focus`, the `~id` namespace, the one-frame retry, framework
click-to-focus mutation, and `~autofocus` are deleted. Tab traversal is
application-owned; the library ships a pure ring helper. The multiplicity
rule for `~focused:true` is Q5.

### 4.5 Sizing

One style vocabulary, defined once, with TUI defaults:

```ocaml
module Style : sig
  type attr
  val fill : attr          (* grow + shrink, min 0 — §1.1's 68-site incantation *)
  val w : dim -> attr      val h : dim -> attr
  val pad : int -> attr    val gap : int -> attr
  val row : attr           (* column is the default axis *)
  val border : ?style:Border.t -> unit -> attr
  val fg : Color.t -> attr val bg : Color.t -> attr
  val truncate : attr      (* render-only; never feeds measurement *)
  (* ~30 attrs total, one flat list, one documentation site *)
end

val column : ?style:Style.attr list -> ?on_click:(Event.mouse -> 'msg option) ->
             ?on_size:(w:int -> h:int -> 'msg option) -> 'msg t list -> 'msg t
val row    : (* same *)
val text   : ?style:Style.attr list -> string -> 'msg t
```

Rules: column is the default axis; flexible children default to zero
minimum; truncation and wrapping are strictly render-side (the §1.2 cluster-1
principle, made structural); constructors carry ~5 parameters instead of
~50. Whether `Style.attr list` beats a `Layout.t` record is Q3.

### 4.6 Render caching

```ocaml
val memo : key:'k -> ('k -> 'msg t) -> 'msg t   (* structural equality on key *)
```

Replaces physical-equality gating. Keys are small values by discipline
(`(block_id, revision, theme_version)`), so an unchanged transcript block
costs one comparison per frame and cannot be silently broken by a fresh
closure or record identity. The static area (`Cmd.static_commit` /
`static_clear`) is preserved as the structural answer for settled
chat-transcript content.

## 5. Semantic contracts

1. **Single owner.** The renderable tree is the renderer's private output
   cache. It holds paint and layout state only, never interactive state. A
   wrong diff is a redraw glitch; `create` vs `apply_props` cannot diverge
   because widgets render from prop state on every frame.
2. **`~key` is non-semantic.** Identity affects performance, never behavior.
3. **One dispatch discipline.** Every message — widget proposal, mouse
   handler, subscription — is queued and applied in order; `view` runs once
   after the batch. Within a batch the framework threads proposals (a second
   wheel tick computes against the first tick's proposed value), so the
   contract stays "on_change hands you the next state" (details: Q1).
4. **Measurement is one-way.** Layout reads the model through `view`; the
   model reads layout only through explicit data (`on_size`, and the
   geometry baked into proposals). Nothing rendered feeds back into
   intrinsic measurement.
5. **The frame is atomic.** Events → update batch → view → layout → paint,
   once. Anything that would dirty the frame mid-frame (scrollbar reveal,
   symbolic scroll resolution) is resolved inside the layout/paint pass, not
   by scheduling another frame the renderer may lose.

## 6. What this deletes

| Pain (evidence) | Fate |
| --- | --- |
| Controlled/uncontrolled split, both failure modes (§1.3, `5522da8d`) | Unrepresentable: state is always the model's |
| `create`/`apply_props` divergence ×7 widgets (§1.2 cluster 3) | Unrepresentable: one render path |
| Scroll request/ack protocol; Mentat `next_page`/`page`/`tail_reset` | Deleted: `Scroll.t` symbolic values |
| Sticky-bottom re-arm bugs (`1a6fb9d5e` [spice]) | `Follow `Bottom` is a stable model value |
| `widget_desync`/`shell_trigger_pending` (§1.1) | Deleted: no in-flight widget buffer exists |
| `Cmd.focus` retry; focus lost unrecoverably (`ISSUES.md:6` [spice]) | Focus is a model field; can't be elsewhere |
| `empty`-splice state transplant; duplicate-key remount (§1.3) | `~key` non-semantic; nothing to transplant |
| Tree callbacks as indices into invisible flattened list | Expansion is the app's value; callbacks report nodes |
| Physical-equality memoization (`per_palette`, theme-picker re-parse) | `memo` by structural key |
| Sub last-one-wins; timer phase swap; dual dispatch ordering | Accumulate-all; `~id`; one queue |
| 68 × sizing incantation across 26 files [spice] | `Style.fill` is the default posture |
| 19 × ~50 optional args; 2,864-line mli | ~5 params + one `Style` vocabulary |
| `~ref`, `Renderable` as public escape hatch | Removed from the app-facing surface |

Not addressed by this RFC (out of scope, tracked separately): Mentat's own
domain-logic bug volume; matrix-level terminal issues; syntax-highlighting
architecture beyond the `memo` contract.

## 7. Performance

Context: OpenTUI exists because Bubbletea is slow. Bubbletea's slowness is
its render target — `view` returns a styled **string**, layout is string
manipulation with ANSI re-scanning, and diffing happens at line-of-string
granularity after the allocation already happened. It is Elm without the
retained half. None of that is "state in the model": Elm on the web is fast
because a pure view feeds a vdom diff against a retained DOM.

This design keeps OpenTUI's retained half — which is also how OpenTUI is
actually used (React/Solid over the retained core):

- Cells, not strings: Matrix grid with cell-level damage diffing.
- Retained layout: Toffee with dirty tracking; unchanged subtrees skip
  relayout (and Toffee is the stable layer: 1 fix in 10 commits since the
  rewrite).
- The reconciler survives as the damage tracker: pure view → vnode diff →
  patch; unchanged subtrees skip layout and paint.

Costs, honestly:

- Per-frame view reconstruction is today's cost already (Mentat
  `doc/dev/performance.md`: "a dirty frame runs view rebuild → reconcile →
  layout → paint over the entire mounted tree; cost is O(mounted
  content)"). The redesign improves both bounding tools: `memo` becomes
  reliable (value keys) and the static area remains for settled content.
- Persistent `Editor.t`: O(log n) per keystroke on a rope/zipper vs O(1)
  amortized mutable — noise at human input rates on OCaml's minor heap.
  Rendering from the value reuses the `text_surface` display-cache pattern,
  keyed on `(value, wrap_width)` — honestly this time (§1.2 cluster 1's
  cache was keyed dishonestly four times).
- Proposal messages allocate one small record per input event. Irrelevant.

Risk concentration: frames where everything legitimately changes (resize
reflow, theme switch) on a large mounted tree. That cost exists identically
today; the mitigations exist today minus their fragility.

**Acceptance gates.** The redesign lands only with a benchmark suite wired
into `dune runtest` (thumper-style baselines), covering at minimum:

1. Keystroke → presented-frame latency, 5,000-block transcript behind a
   scroll box (memo discipline under fire).
2. Wheel-scroll storm (proposal threading under batching, Q1).
3. Full resize reflow at 250×70 (worst-case whole-tree frame).
4. Idle-but-subscribed app: a pending timer performs zero view/reconcile/
   paint work (invariant already pinned by `test_loop.ml:224`).
5. Editor keystroke on a 1MB buffer (persistent `Editor.t`, Q2).

Baselines are recorded against current mosaic HEAD before the redesign
branches, so every decision in the pipeline can be checked against numbers.

## 8. Alternatives considered

- **Status quo + patching.** Rejected: §1.2 documents five months of
  principled patching at the seam with no fixed point, because each fix
  trades silent divergence for forced revert or back. The two open xfails at
  HEAD are unclosed semantic holes, not backlog.
- **Full signals/retained (OpenTUI+Solid).** Rejected: coherent and fast,
  but abandons TEA (Mentat's architecture), requires a fine-grained
  reactivity runtime OCaml does not have, and gives up replayable
  single-model semantics.
- **Bubbletea-literal** (no framework layout or routing; apps receive raw
  events + window size; widgets render to strings). Rejected: forfeits
  Toffee layout and mouse hit-testing, which Mentat genuinely uses, and
  imports Bubbletea's render-target problem.
- **Nested TEA per widget** (each widget has its own `msg` and `update`,
  wired with `Cmd.map`/`Sub.map`). Rejected: uniform but pays mapping
  ceremony at every embedding — the boilerplate §1.3 already flags. OCaml's
  polymorphism lets widget views produce parent messages directly through
  callbacks, which is strictly less ceremony for the same purity.
- **App-side transition functions** (`Editor.handle_key : key -> t -> t`
  called from `update`). Rejected: purest on paper, but wrapped-cursor
  motion and wheel scrolling need geometry, so applications would end up
  measuring — the exact violation Mentat's review pane already committed
  against spice RFC 0016 §9.3. Framework-side proposals keep geometry out of
  application code entirely.

## 9. Open questions for the pipeline

- **Q1 — Proposal batching semantics.** Within one update batch, proposals
  thread framework-side (each computed against the latest proposed value).
  Specify the exact ordering with interleaved key/mouse/subscription
  messages, and what a rejected proposal (`on_change` returns `None`) means
  for subsequent proposals in the same batch.
- **Q2 — Persistent `Editor.t` representation.** Rope vs zipper vs piece
  table; grapheme-aware indexing; undo as a value (list of states vs
  inverted operations); the display-cache key and eviction policy. This is
  the largest single piece of implementation work and the enabling move.
- **Q3 — Style surface.** `Style.attr list` (one vocabulary, permissive) vs
  a `Layout.t` record with defaults (typed per-node applicability, `with`
  update syntax). Decide with Mentat call-site diffs as the benchmark:
  rewrite 10 representative Mentat views under each candidate.
- **Q4 — Layout-data channel.** Is `on_size` sufficient, or do some
  Mentat cases (reviewed pane widths, gutter alignment) need a richer
  explicit geometry snapshot delivered to `update`? Enumerate the actual
  Mentat call sites that hand-measure today and check each against the
  proposal mechanism.
- **Q5 — `~focused` multiplicity.** Zero focused nodes: keys fall through to
  `Sub.on_key` only. Multiple: first in tree order wins vs a runtime
  diagnostic. Decide; also decide whether a `Focus.ring` helper ships in
  core or as a library.
- **Q6 — Key interception layering.** `on_key` returning `Some` consumes
  before the default edit. Does Mentat's capture-phase ask (global shortcuts
  that beat the focused composer, from its upstream-asks list) need a
  capture hook, or is `Sub.on_key` ordering (Q1) enough?
- **Q7 — Widget catalog scope.** Which of the 19 elements survive the
  re-shape as core widgets vs become library code over the primitives
  (`markdown`, `code`, `diff`, `table` are candidates for the latter given
  the `memo` contract).
- **Q8 — `mosaic.mlx`.** Zero users in either repo, already drifted.
  Proposal: delete; revisit only with a concrete consumer.
- **Q9 — Migration staging.** Mentat pins mosaic at HEAD and already absorbs
  semantic changes as unplanned work (an entire spice branch,
  `campaign/tui-mosaic-adaptation`, absorbs one renderer change plus ~33
  golden re-records). Decide: parallel library (`mosaic2`-style incubation)
  vs in-place breaking change behind the pin; and which Mentat surface
  migrates first (the composer + transcript is the highest-pain, and its
  wrappers `scrollport`/`prims`/`prose` are the spec for acceptance).
- **Q10 — Event model details.** One propagation story for mouse (bubble +
  optional capture?), one meaning for consumption, and whether
  `prevent_default`/`stop_propagation` both survive.

## Appendix A: evidence tables

### A.1 Mosaic commit-type and post-rewrite fix density

1,013 commits total: 322 fix (31.8%), 171 refactor, 167 feat, 110 chore,
76 test, 70 docs, 66 perf. File-tree classification since the 2025-12-13
rewrite:

| tree | total | fix | fix % |
| --- | --- | --- | --- |
| matrix/lib | 317 | 124 | 39% |
| mosaic/lib/ui | 178 | 57 | 32% |
| mosaic/lib/ui/renderables | 106 | 28 | 26% |
| mosaic/lib/mosaic | 75 | 17 | 22% |
| toffee/lib | 10 | 1 | 10% |
| mosaic/lib/mlx | 17 | 1 | 6% |

Hottest fix files: `renderer.ml` (16 fixes/45 commits), `mosaic.ml` (13),
`renderable.ml` (9), `table.ml` (6), `edit_buffer.ml` (6), `edit_surface.ml`
(5), `text_surface.ml` (5), `reconciler.ml` (5); `renderer.mli` and
`renderable.mli` carry 5 fix commits each (the contract, not just the code).

### A.2 Re-fix cycles

- **Zero-extent/quantization**: `146bf73c` → `5723aaa3` → `01fd6dff` →
  `1891def3`, four chained fixes in 30 days, each caused by the previous;
  xfail open at `test_renderer.ml:1695`.
- **`Sub.every` timers**: `f1a0b85c` (add epsilon + 13-line justification) →
  `519dc94d` (remove it, absolute deadlines) → `a169cec4` (reinstate a
  microsecond slack, origin+count schedule) — fix, un-fix, re-fix in six
  weeks.
- **`text_surface` measure cache**: `3d13a1f8` → `9b68155f` → `6ec997f0` →
  `7c9902bd` — intrinsic size contaminated by previous layout, four times.
- **`apply_props` divergence**: `a4cb678f` → `28858d4d` → `5522da8d` →
  `712ad398` — same structural defect, seven widgets, four months.
- **Perf regressing semantics**: `ff51f862` (cull) caused the zero-extent
  loss `cda0bacb` documents as "Spice's TUI goldens lost whole lines inside
  scroll boxes"; `b862d6df` (skip clean-frame layout) silently narrowed the
  measure-closure contract to require `mark_dirty`.
- **Width method**: `7bfed589` → `6bd7cfe0`, "wave 1"/"remaining widgets",
  same day — no single chokepoint for text measurement.

### A.3 API hazards (mental-model audit, abridged)

H1 two opposite reconciliation policies (`reconciler.ml:251/288`); H2
nudge-only scroll/expansion state; H3 tree callbacks as flattened indices
(`tree.ml:242-260`, expansion reset by `set_items`, `tree.ml:728`); H4 five
last-one-wins subscription kinds (`mosaic.ml:504-514`); H5 queued element
vs immediate subscription dispatch (`mosaic.ml:181/599`); H6 `~key` vs `~id`
namespaces + `Cmd.focus` phase leak (`mosaic.ml:371-412`); H7 `empty`-splice
positional transplant (`reconciler.ml:850,910-917`); H8 three propagation
rules, two meanings of consume (`renderer.ml:939-977`,
`event.mli:207-218`); H9 all geometry reads one frame stale
(`mosaic.ml:805-811`); H10 mlx drift; H11 no component abstraction; H12
`~ref` lifetime, focus dropped on destroy (`renderable.ml:565-571`),
renderer-global selection.

### A.4 Mentat friction (spice repo, abridged)

Fix categorization of 36 sampled TUI fix diffs: layout/sizing 8,
app-domain 9, focus/keys 6, styling 6, state-sync 3, lifecycle/timers 2,
scroll 1, reconciliation 1. Framework-root-caused: 10 (7 of them sizing).
Key sites: `composer.ml:32-39` (`widget_desync`), `app.ml:247-249/567-568/
4653-4665/5560-5582` (scroll request/ack), `app.ml:474/390/1039-1042/
3691-3860` (parallel focus model), `prims.ml:8-13` + `prose.ml:13-90`
(physical-equality memo architecture), `review_panel.ml:250-262`
(hand-computed widths), 68 sizing-incantation sites across 26 files.
Upstream-asks list (from `doc/plans/tui-next-composer.md`, deleted but in
history): declarative box title styling; a capture-phase key/paste hook;
the flex-truncate measurement fix; controlled-value push invalidating
in-flight cursor reports.
