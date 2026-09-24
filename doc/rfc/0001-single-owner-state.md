# RFC 0001: Single-owner state — re-founding Mosaic's application contract

- Status: Implemented in the worktree; latency acceptance remains pending
  (2026-09-24). The scope correction below preserves labelled styling and MLX.
  Original evidence gathered 2026-08-21 from three independent audits
  (Mentat friction, fix history, API review), summarized in §1 and Appendix A.
- Audience: Mosaic maintainers; Mentat maintainers (spice RFC 0016 §9.3
  delegates all measurement, wrapping, truncation, and scrolling to Mosaic and
  is a direct consumer of this contract).
- Baselines: mosaic @ `712ad398`, spice/mentat @ `e8506bc9`, OpenTUI reference
  refreshed from `anomalyco/opentui` main on 2026-09-24 at
  `4b1474e009bf78365a7a4866e83a9e522533ef42` (§1.4). Commit hashes below
  belong to the mosaic repo unless marked `[spice]`.
- Compatibility: none. This is a breaking re-founding of the `mosaic` contract
  layer (element API, reconciler contract, widget state ownership, focus,
  and event ordering). Existing labelled layout/paint arguments and sizing
  defaults are preserved. Preserve Matrix/Toffee boundaries and retained rendering.
  Widget internals require adaptation: `Edit_buffer` and `Selection` are
  mutable today. [RFC 0003](0003-text-layout.md) owns shared text geometry
  and supersedes the original promise to preserve text internals.
  `mosaic.mlx` remains supported and follows the same state-ownership contract.
- Scope decision (2026-09-24): implement state ownership independently of any
  styling API redesign. Keep labelled constructor arguments and the MLX syntax
  adapter. Usage in this repository does not establish external library usage.

## Summary

Mosaic runs an Elm loop — immutable model, full re-render per frame — over
retained, self-mutating widget objects inherited from OpenTUI's design. The
current boundary between the two, reconciliation, has ambiguous ownership: it
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

> **Declarative widgets take their accepted interaction state from a plain
> value in the application's model; the widget renders it, and when an event
> would change it, the framework hands the application the entire proposed
> next value as a message. Storing that value is the only way anything
> moves.**

This contract covers declarative editor, scroll, selection-widget and focus
state. Low-level renderables and `embed` remain explicit integration boundaries:
the integrator owns their state. Renderer-managed pointer gestures and native
text selection are not persistent application widget values.

The retained renderable tree survives as the renderer's private output cache
— the role `Matrix.Screen`'s cell grid already plays — so a wrong diff is a
redraw glitch, never data loss. Layout-dependent transitions (wheel scrolling
against viewport extents, cursor motion in wrapped text) run framework-side
at dispatch time, where geometry lives, and deliver whole state values to
`update`: the application never measures ("framework proposes, model
disposes", §4.3). Existing sizing defaults and labelled layout arguments remain unchanged
(§4.5). Measurement and rendering must still share consistent geometry.

By construction this deletes: the controlled/uncontrolled distinction and
both of its failure modes; the `create`/`apply_props` dual path and its
seven-widget fix cycle; the keyed one-shot scroll request + `on_*_applied`
retirement protocol; `Cmd.focus`-by-string-id and its one-frame retry;
reconciliation-driven state loss and transplant; callbacks reporting indices
into widget-private lists; `~ref`; and every shadow-state flag in Mentat
(§6). Mosaic keeps cell buffers, retained layout, and diff-driven damage
tracking. These are design constraints, not evidence that persistent state
or dispatch changes are free; the redesign is gated on measurements (§7).

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

### 1.4 Primary reference: current OpenTUI upstream

The primary implementation reference is upstream OpenTUI, fetched from
`https://github.com/anomalyco/opentui.git` main on 2026-09-24. Its head was
`4b1474e009bf78365a7a4866e83a9e522533ef42` (2026-09-23), also the local
checkout at verification time. These are source observations, not performance
measurements:

- [EditBuffer](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/edit-buffer.zig)
  is a mutable owner of text, cursors and edit operations. Its underlying
  [rope](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/rope.zig)
  has persistent nodes and saved roots for undo. Persistent storage does
  not imply a persistent application-facing editor.
- Both [React](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/react/src/utils/index.ts)
  and [Solid](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/solid/src/reconciler.ts)
  adapt declarations to retained objects. React compares old and new props;
  a retained design does not require a signals runtime.
- [Textarea](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/core/src/renderables/Textarea.ts)
  distinguishes initial text from subsequent editing.
  [ScrollBox](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/core/src/renderables/ScrollBox.ts)
  retains viewport position and sticky-edge state. These are deliberate
  ownership choices, not bugs that model persistence inherently fixes.
- [TextBufferView](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/text-buffer-view.zig)
  shares the row-building algorithm between measurement and rendering,
  caches by content epoch and geometry inputs, and reuses allocation
  capacity. Wrapping participates in measurement.
- [KeyHandler](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/core/src/lib/KeyHandler.ts)
  runs global listeners before focused-renderable handlers and distinguishes
  stopping propagation from preventing default behavior.

Follow these storage, geometry, event-ordering and retained-rendering
principles first. Model-owned interaction remains the proposed Mosaic
departure for replayable application state; it is not an OpenTUI contract.
Its acceptance and synchronization costs must be established before a
catalog-wide migration. RFC 0003's approved model-ownership assumptions
remain in effect; changing them requires an explicit revision of both RFCs.

## 2. Diagnosis

### 2.1 Fault one: no single owner of interactive state

Mosaic straddles three paradigms: an Elm runtime, React-retained widgets
without React's controlled/uncontrolled discipline, and immediate-mode
escape hatches (`~ref`, `embed`, the `Renderable` surface). The seam is
narrow and structural: **the reconciler writes widget state alongside event
handlers, and decides whether to write by comparing two declarations,
never the declaration to the widget.** The retained widget moves on its own;
that motion is invisible to a declaration-vs-declaration diff; so the widget
drifts and the reconciler never notices. The `has_live_controlled_value`
bypass for exactly two of eighteen widget kinds is an explicit
acknowledgement of the hole — and closing it uniformly would convert silent
divergence into forced-revert everywhere, which is the failure the examples
already demonstrate. Every fix at this seam trades one failure mode for the
other; §1.1's shadow state and §1.2's clusters 2–3 are this one fault seen
from the application and history sides.

OpenTUI's retained ownership is a coherent alternative. Both its Solid and
React bindings patch retained objects; full view reconstruction alone does
not make that architecture unsound. Mosaic's fault is its ambiguous contract
for which owner may change a given value, and when a declaration is binding.
Single ownership is the requirement; immutability is one way to satisfy it.

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
pointed at an ambiguous contract). The existing mutable `Edit_buffer`,
`Text_surface` and `Selection` are useful implementations, but cannot be
treated as persistent values without adaptation. And 61% of Mentat's UI
bugs are its own domain logic; no redesign touches those.

## 3. The ruling

Adopt the full-Elm position: all interactive state lives in the model as
plain values; widgets are pure functions of that state; the renderable tree
is disposable output. This remains the proposed target because it preserves
replayable whole-model interaction. An OpenTUI-style retained design could
keep TEA for domain state without adding signals, but would give up that
whole-model property (§8). Rejected: the status
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

Every stateful widget takes its accepted value from the model and reports
proposals through a required semantic callback. Editors, trees and scroll
positions use persistent values; scalar controls and selection widgets use
ordinary application values. Optional raw-event hooks provide interception.

```ocaml
module Editor : sig
  type t                              (* persistent: text, cursor, selection *)
  val empty : t
  val of_string : ?max_length:int -> string -> t
  val text : t -> string
  val clear : t -> t
  val insert : string -> t -> t       (* pure edits for update-side use *)
  (* ... the Edit_buffer operations, as pure functions *)
end

val textarea :
  (* Existing labelled layout and paint arguments are retained. *)
  ?focused:bool ->                    (* derived from the model, §4.4 *)
  ?on_focus_request:'msg ->
  state:Editor.t ->
  on_change:(Editor.t -> 'msg) ->         (* the ENTIRE next state *)
  ?on_key:(Event.key -> 'msg option) ->   (* interception contract: Q6 *)
  ?placeholder:string ->
  unit -> 'msg t

module Scroll : sig
  type t
  val at : ?x:int -> int -> t
  val follow : [ `Top | `Bottom | `Left | `Right ] -> t -> t
  val reveal : ?column:int -> line:int -> t -> t  (* resolved at render *)
end

val scroll_box :
  (* Existing labelled layout and paint arguments are retained. *)
  ?focused:bool ->
  scroll:Scroll.t -> on_scroll:(Scroll.t -> 'msg) ->
  'msg t list -> 'msg t
```

`select`, `tab_select`, and `table` take `~selected:'a option` and an explicit
`~equal:('a -> 'a -> bool)`. `Select.item value label`,
`Tab_select.item value label`, and `Table.row value cells` associate presentation
with application values. Values must be distinct under `equal`. Reordering
preserves selection by value; `None` or an absent value displays no selection,
without choosing a replacement or emitting a callback. Navigation proposes a
current value through `on_change:('a -> 'msg)`; activation without a selected
item does nothing. The tree keeps its expansion set and selected ID in
`Tree.State.t`, and activation reports a current `Tree.item`.

Semantic callbacks (`on_change`, `on_scroll`, `on_submit`, `on_activate`)
return messages directly. `update` accepts, transforms, or rejects a proposal;
delivery alone does not imply acceptance. Raw event callbacks retain
`'msg option`, with `None` filtering that event. No optional-message wrapper
is needed for a semantic proposal already identified by the widget.

Editor proposals also include caret, selection, and affinity changes. Accepting
those changes must not mark text modified or trigger text-dependent commands.
Compare `Editor.text` before and after the proposal for dirty flags, submission
status, autosave, and other content-dependent work, while retaining the complete
accepted `Editor.t`. Cursor-only transitions share text and source indexes.

### 4.3 Framework proposes, model disposes

Transition functions that need geometry — wheel scrolling against viewport
and content extents, PageDown, cursor motion across wrapped lines, mouse
hit-to-cursor mapping — run framework-side at dispatch time. They read the
latest accepted state, compute the whole next value, and deliver it to
`update`. Before deriving the next input's proposal, the runtime applies the
resulting messages and refreshes widget declarations from the resulting model
(§5). A rejected or transformed proposal must never become the next event's
starting state.

Geometry must describe that accepted source and the relevant layout inputs.
Reuse cached geometry when valid; refresh it before a geometry-dependent
transition when content or layout has changed. This is framework work, not
application measurement. Symbolic scroll positions (`Follow `Bottom`,
`Reveal line`) are resolved against real
extents at render time each frame, so they are stable values requiring no
write-back and no retirement protocol. Applications that do need geometry as
data get it explicitly via `?on_size:(w:int -> h:int -> 'msg option)`
(ResizeObserver-shaped), never by reading the tree.

### 4.4 Focus

Focus is a model field. The view marks the focused node
(`~focused:(model.focus = `Composer)`); keys are delivered to the node
marked focused. `~on_focus_request:msg` declares the message for an ordinary
left-button press on an unfocused focusable node. The nearest focusable
ancestor's request is delivered before caret and selection defaults, which
proceed only if the resulting declaration leaves that same live node focused.
Rejection or removal cancels those defaults without mutating the target editor.
Already-focused nodes need no request; Shift/native-selection gestures do not
request focus. Constructing or mapping a view does not dispatch the message.
`~on_click` remains a release gesture callback.

`button ~on_activate:msg label` composes the existing box and text elements.
It activates once for a primary press/release inside without dragging, or an
unmodified Enter/Space press; reported Repeat/Release events do not activate it.
Focus adds inverse video to the label; `~disabled:true` adds dim instead and
suppresses activation and focus requests. Caller label styles are preserved.
Buttons participate
in the application's focus order alongside inputs and selectors. Form inputs
use `on_submit`; a global Enter binding must not steal button activation.

`Cmd.focus`, the `~id` namespace, the one-frame retry, framework
click-to-focus mutation, and `~autofocus` are deleted. Tab traversal is
application-owned; `Focus.next` and `Focus.prev` traverse an application-supplied
order using explicit equality. Empty orders yield no focus. Missing or unknown
focus chooses the first/last entry; traversal wraps. Multiple visible focused
nodes raise `Invalid_argument`; hidden subtrees do not participate.

### 4.5 Layout and styling: preserve the existing API

Keep the existing labelled layout and paint arguments, including `~size`,
`~padding`, `~flex_direction`, `~background`, and text's `~style:Ansi.Style.t`.
Keep the existing layout defaults. State ownership does not require replacing
these arguments with an attribute list or changing flex-shrink behavior.

The earlier attribute-list proposal is withdrawn from this implementation.
Any future styling redesign must demonstrate better user code, discoverability,
and performance independently; reducing signature size is not sufficient.

Wrapping affects measured height at a definite width, using the same geometry
as paint. Truncation must not replace source content in intrinsic measurement,
and a previous paint width must not contaminate an unconstrained probe
(RFC 0003).

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

1. **Single owner.** Declarative widget renderables cache the accepted model
   for paint, layout and event proposals. They never commit a proposed value
   independently of the model. Explicit low-level integrations own their state. A
   wrong diff is a redraw glitch; `create` vs `apply_props` cannot diverge
   because widgets render from prop state on every frame.
2. **`~key` is non-semantic.** Identity affects performance, never behavior.
3. **One dispatch discipline.** Process input transactions in order. Apply
   an input's resulting messages through `update`, then refresh declarations
   from the accepted model before deriving the next interactive transition.
   Subscription messages participate in the same ordering. Batch painting,
   not speculative proposals. With an opaque model, the framework cannot
   infer acceptance merely because `on_change` returned a message.
4. **Measurement is one-way.** Layout reads the model through `view`; the
   model reads layout only through explicit data (`on_size`, and the
   geometry baked into proposals). Nothing rendered feeds back into
   intrinsic measurement.
5. **Presentation is atomic.** Input/update/declaration synchronization may
   repeat before a frame; paint presents the resulting state once. Refresh
   layout earlier when a subsequent input needs changed geometry. Anything
   that would dirty the frame mid-frame (scrollbar reveal,
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
| Repeated sizing declarations [spice] | Separate design work; existing labels and defaults retained |
| Repeated layout labels in signatures | Retained; signature reduction alone is not a user benefit |
| `~ref` | Removed from declarative constructors; `embed` remains an explicit integration boundary |

Not addressed by this RFC (out of scope, tracked separately): Mentat's own
domain-logic bug volume; matrix-level terminal issues; syntax-highlighting
architecture beyond the `memo` contract.

## 7. Performance

OpenTUI's implementation is the reference for retaining work and bounding
allocation. Comparisons with other frameworks are not a substitute for
measuring Mosaic's actual workloads. Preserve:

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
  Synchronizing accepted declarations between inputs adds work beyond that
  per-frame baseline; measure wheel storms and rapid input explicitly.
- Current `Edit_buffer` edits copy flat strings and invalidate whole-text
  indexing. Mutability does not make those operations O(1). OpenTUI's
  persistent rope is the first storage reference; measure edits, indexing,
  undo retention and layout together before choosing a representation.
  RFC 0003 starts with flat strings and does not deliver incremental edits.
- Proposals and persistent versions add allocations and may retain old
  buffers. Record allocation, promotion and retained-history costs alongside
  latency; do not assume they are negligible at human input rates.

Risk concentration: input bursts requiring repeated synchronization, retained
editor history, and resize/theme changes on large mounted trees. Shared
geometry must avoid flattening or re-indexing unchanged text per event.

**Acceptance gates.** The redesign lands only with a benchmark suite wired
into `dune runtest` (thumper-style baselines), covering at minimum:

1. Keystroke → presented-frame latency, 5,000-block transcript behind a
   scroll box (memo discipline under fire).
2. Wheel-scroll storm (accepted-state synchronization with batched paint, Q1).
3. Full resize reflow at 250×70 (worst-case whole-tree frame).
4. Idle-but-subscribed app: a pending timer performs zero view/reconcile/
   paint work (invariant already pinned by `test_loop.ml:224`).
5. Editor keystroke on a 1MB buffer (persistent `Editor.t`, Q2).

Baselines are recorded against current mosaic HEAD before the redesign
branches, so every decision in the pipeline can be checked against numbers.

### 7.1 Implementation measurements (2026-09-24)

The runtime harness drives the parser, update, view, layout, paint and terminal
submission through `Matrix_test`, stopping at the presented idle boundary.
Mounting and the output oracle are outside measurement. Its 5,000 blocks have
three text rows each; eight wheel events arrive together. A wheel sample includes both a down and
up burst, presenting after each, to avoid direction-dependent sampling. The pre-change
comparison is an isolated archive of `40c094cd`, with equivalent old-API input,
scroll and cached transcript declarations. Both use OCaml 5.5.1, release profile,
on the same Apple M1 Max host.

| Runtime action | Before, allocated words | RFC baseline, allocated words | Change |
| --- | ---: | ---: | ---: |
| Insert/backspace through presentation | 5,846,986 | 3,858,948 | −34.0% |
| Wheel roundtrip, two eight-event bursts | 4,604,250 | 4,084,260 | −11.3% |
| Resize between 249×69 and 250×70 | 145,352,687 | 142,546,693 | −1.9% |

The RFC figures are the stored release-profile `runtime.thumper` baseline.
The subsequent API and focus improvements pass its existing allocation budget;
the baseline has not been raised to accommodate them.

The resize cost remains substantial in both versions. Allocation profiling
points to Toffee flex item construction and whole-tree layout rounding; this
RFC does not change Toffee. The 500-row reconciler microbenchmark drops from
60,670 to 36,193 words for an unchanged tree, and from 78,665 to 54,188 for
reordering.

Domain-value selection adds a linear lookup and conversion when declaring a
select, tab strip or table. With 5,000 preconstructed items, declarations
allocate 20,130 / 20,128 / 20,147 words respectively, versus 55 / 53 / 67 for
equivalent positional Vnodes. This includes the application layout wrapper;
the variable overhead is about four words per item. Native navigation keeps
indices, and callbacks map to values in constant time. The allocation-only
`selection.thumper` baseline makes this conversion cost explicit.

Persistent editor cursor movement allocates 58 words per left/right pair for
both 128-byte and 1MB buffers, versus zero for the mutable engine. Independent
1MB insertions allocate 1,312,585 words versus 1,312,567 for an engine copy:
18 additional words for the persistent wrapper. The history retention check
verifies the existing 200-snapshot bound and release of obsolete editor states
and text indexes. Flat-string snapshot storage still scales with document size.

`mosaic/bench/editor.thumper` and `runtime.thumper` gate allocation increases
above 5% through `dune runtest mosaic/bench`. `EDITOR_TIMING=1` and
`RUNTIME_TIMING=1` additionally measure wall time and promotion. The idle timer
invariant is covered by loop tests.

**Open gate:** latency and promotion comparison on a quiet host. The current
host was fully CPU-saturated during measurement, making timing comparisons
inconclusive. Lower allocations are not evidence that latency improved; this
RFC is not ready for performance sign-off until that comparison is completed.

## 8. Alternatives considered

- **Status quo + patching.** Rejected: §1.2 documents five months of
  principled patching at the seam with no fixed point, because each fix
  trades silent divergence for forced revert or back. The two open xfails at
  HEAD are unclosed semantic holes, not backlog.
- **Retained widget ownership (OpenTUI core with a declarative adapter).**
  Credible alternative: initialize an editor once, then use explicit editing
  operations and notifications rather than mirroring its text in props.
  Retains TEA for domain state without requiring signals. Widget identity
  owns interaction state; whole-model snapshots no longer capture it.
- **Application-owned mutable editors and scroll state.** The view borrows
  an explicit handle, for example `textarea ~editor:model.editor ()`;
  `update` may call `Editor.clear model.editor`. One object replaces the
  model/widget copies and survives remounting. This simplifies acceptance
  mechanics, but old models alias new interaction state, so whole-model
  replay is lost. Not selected while replay and RFC 0003's model-owned
  interaction remain requirements. Reconsider explicitly if the persistent
  design fails its performance or complexity gates.
- **Full signals (OpenTUI+Solid).** Separate from retained ownership:
  introducing a reactive application runtime is unnecessary to obtain the
  retained-core benefits and would replace the existing TEA contract.
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

## 9. Implementation decisions and remaining integration work

The implementation resolves the library choices below. Performance acceptance
remains pending: §7.1 records allocation measurements, but latency and promotion
must still be compared on a quiet host. Mentat migration has not been performed
as part of this implementation.

- **Q1 — Accepted-state synchronization. Resolved.** Input and subscription
  messages pass through `update` synchronously; declarations refresh from the
  accepted model before the next interactive transition. Rejected or normalized
  proposals never become speculative input state. Geometry preparation settles
  synchronous layout invalidations before input dispatch and presentation;
  layout-only preparation does not advance animations. Deliberately deferred
  editor proposals can be checked with `Editor.accept ~current`, which rejects
  proposals based on a superseded source, caret, selection or affinity state.
  Runtime and editor measurements are recorded in §7.1; its open performance
  gate remains applicable.
- **Q2 — Persistent `Editor.t` representation. Resolved for this implementation.**
  `Editor.t` wraps the existing flat-string editing engine with persistent
  operations. Versions share immutable text, grapheme indexes and history
  entries; widget scratch buffers cannot mutate published editor values.
  Cursor-only changes share the indexed source. Text edits still splice and
  index flat strings, and undo history retains at most 200 text snapshots per
  branch. This is not an incremental rope implementation; its allocation and
  retention costs are described in §7.1.
- **Q3 — Style surface. Resolved.** Preserve labelled layout/paint arguments
  and existing defaults (§4.5). A separate proposal may evaluate alternatives
  against real call sites and measured costs.
- **Q4 — Layout-data channel. Consumer validation remains open.** `on_size`
  delivers allocated dimensions, and geometry-dependent widget actions produce
  state proposals without application measurement. Mentat's pane-width and
  gutter-alignment call sites still need to be checked against these channels.
  Whether those consumers need additional geometry data remains an integration
  question; no richer snapshot API is introduced here.
- **Q5 — Focus. Resolved.** No focused node means only global key capture;
  multiple visible focused declarations raise `Invalid_argument`. Hidden
  subtrees do not participate. `Focus.next/prev ~equal order current` provide
  pure application-owned traversal. `on_focus_request` proposes pointer focus
  before caret/selection defaults and requires accepted focus on the same live
  node (§4.4).
- **Q6 — Key interception layering. Resolved.** `Sub.on_key` captures before
  the focused element's `on_key` handlers and default editing action, following
  OpenTUI's ordering. Capture messages update the accepted declarations before
  focused-widget dispatch. Returning a message does not consume the event;
  `prevent_default` suppresses the default action and `stop_propagation` stops
  subsequent handler traversal. Paste uses the same capture-before-default
  layering through `Sub.on_paste`.
- **Q7 — Widget catalog scope. Resolved.** The existing catalog remains in
  Mosaic, including `markdown`, `code`, `diff` and `table`. Interactive
  declarative widgets receive application-owned state and report proposals;
  this change does not extract widgets into separate libraries. `embed` remains
  an explicit integration boundary whose owner manages the embedded
  renderable's properties and interaction state.
- **Q8 — `mosaic.mlx`. Resolved.** The supported syntax adapter is retained
  and follows the same labelled constructors and state-ownership contract as
  Mosaic. It shares the rendering implementation; repository-local syntax
  usage is not treated as evidence of external usage.
- **Q9 — Migration staging. Library staging resolved; Mentat migration open.**
  The library changes in place, with a breaking state-ownership API, rather
  than introducing a parallel `mosaic2` library. Repository examples and
  adapters use the new contract. Mentat still needs a coordinated dependency
  pin and migration plan, including which surface moves first and validation
  of its composer/transcript wrappers (`scrollport`, `prims`, `prose`). This
  implementation does not establish that consumer migration is complete.
- **Q10 — Event model details. Resolved.** Mouse events are hit-tested and
  bubble through the renderable tree; `Sub.on_mouse` then receives unconsumed
  events, while `Sub.on_mouse_all` also receives consumed events. No mouse
  capture subscription is added. `prevent_default` controls default behavior
  and consumed-event filtering; `stop_propagation` controls handler traversal.
  Declarative mouse dispatch proposes focus through `on_focus_request` and
  applies pointer defaults only after acceptance. Semantic callbacks return
  messages directly; raw event callbacks return optional messages. All
  subscription kinds accumulate in declaration order, and timer/tick identity
  uses explicit, unique IDs.

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
