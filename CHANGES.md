# Changelog

## [Unreleased]

### Mosaic

#### Added

- **Shortcut subscriptions** – `Mosaic.Shortcut` and `Sub.on_keys` bind keys to messages declaratively, with base-key and modifier matching
- **Runtime probe** – `Mosaic.run ~probe` hands test harnesses a quiescence probe (`Probe.is_settled`) for deterministic settling in headless tests
- **Idle-cheap timers** – a pending `Sub.every` arms a one-shot wakeup instead of holding the render cadence live; only `Sub.on_tick` keeps the cadence running
- **Syntax API** – `Code.syntax` replaces the widget's public `spans` argument; `Code.Highlighter` runs highlighting synchronously or asynchronously with cancellation, stale-result protection, and a plain-text fallback
- **Terminal commands** – `Cmd.set_clipboard`, `Cmd.copy_selection`, `Cmd.clear_selection`, `Cmd.bell`, `Cmd.notify`, and `Cmd.query_color_scheme` paired with `Sub.on_color_scheme`
- `scroll_bar` and `diff` widgets; `diff` renders unified and split patches with per-line signs, highlighted source lines, sub-line highlight spans, line click hits, and source-row reveal
- `Mosaic.viewport_switch` picks a subtree by terminal width before layout, keeping compatible keyed children across the switch
- `scroll_box` takes keyed one-shot `?reveal`, `?scroll_by`, and `?reset_sticky` requests with `?on_scroll_by_applied` / `?on_reset_sticky_applied` acknowledgements, plus `?show_scrollbars` for a bare scrolling surface
- `markdown` takes a `?code_syntax` hook that highlights fenced code blocks, and `?on_selection` reporting the selected rendered text
- `table` gains passive selection, overflow indicators, hover and activation callbacks, and PageUp/PageDown paging over its measured visible rows
- A drag that reaches a scroll box edge auto-scrolls and keeps the selection in sync with the new viewport
- Viewport culling for scroll containers and a clean-frame layout skip: off-screen children are neither laid out nor rendered, and layout recomputes only when the tree is dirty
- Scroll containers emit hardware scroll hints: an alt-mode scroll-by-one now reaches the terminal as a DECSTBM row shift plus edge-row diffs
- Widgets follow the terminal-negotiated width method instead of hardcoding Unicode tables

#### Changed

- Frame deltas are milliseconds everywhere (runtime, widgets, Screen post-processors); widget animation constants were split between units before
- Selection/value props are controlled when provided and uncontrolled otherwise, uniformly across `slider`, `select`, `tab_select`, `table`, and `tree`
- Editable widgets take key bindings and aliases, and user paste handlers run before widget default paste behavior, matching OpenTUI's default-prevention semantics
- `Cmd.focus` expires after one deferred attempt instead of retrying forever; every-timers carry absolute deadlines and no longer phase-merge
- Incremental markdown reconciliation: streaming appends re-render only the tail; `table` caches auto column widths; `line_number` indexes its props
- Edit-buffer undo history is bounded (200 snapshots)

#### Fixed

- `Cmd.perform` messages can no longer be lost to a cross-thread race
- `Sub.on_mouse` and `Sub.on_paste` now see event consumption (they behaved like the `_all` variants)
- Left-click focuses selectable widgets even when it starts a selection; wheel over dead space reaches the focused scrollable
- Tree and `select` mouse handling uses widget-local coordinates; `scroll_bar` applies orientation and arrow-color prop updates
- Reconciler tracks id changes on reused fibers, keys duplicates first-occurrence-wins, and places children in linear time
- Reordering keyed elements no longer detaches and reattaches reused nodes, so focus survives the reorder
- Text measurement no longer feeds back on the previous frame's width: no-wrap text measures untruncated, and wrapped text measures at its natural width under max-content, so a node can grow back after a shrink
- Terminal extents derive from rounded absolute edges, so adjacent flex children no longer paint over each other; clipping containers keep at least one cell
- Render work scheduled during a frame settles on the next frame instead of waiting for unrelated input, and superseded render passes no longer leak cells into adjacent widgets
- Sticky scroll boxes re-engage following when a reflow reaches the parked edge; `textarea` clamps scroll offsets on resize
- `table` and `tree` apply the `focused_selected_*` colors declared as props
- Word motions treat newlines as boundaries in the edit buffer
- Single-line editors reserve the caret cell, so an empty focused input shows its cursor
- Builds on OCaml 5.4 again (`Option.for_all` is 5.5-only)

### Matrix

#### Added

- **Pluggable backends** – `Matrix.Backend` is a record of I/O primitives with a shared `bootstrap` handshake; `matrix-eio` provides an Eio runtime on top of it
- **Headless test backend (`matrix.test`)** – virtual clock, byte-level input through the real parser, plain-text frame snapshots; `Screen.create` takes an injectable `?clock`
- **Unicode text layer (`matrix.text`)** – grapheme iteration, width measurement, wrap-break and line-break discovery, and position helpers, independent of cell storage
- **Terminal charts (`matrix.charts`)** – declarative chart composition rendered into grids
- **Timer wakeups and paced one-shot redraws** – `Matrix.schedule_wakeup`, and `request_redraw` bursts coalesce to `target_fps`; `Matrix.request_immediate_redraw` renders one frame off-cadence
- **Primary-mode static output** – `Matrix.static_write ?preserve_live_region` keeps the live region across writes, `Matrix.static_replace` swaps all static content in one frame transaction, and `Matrix.static_clear` resets it
- Clipboard (OSC 52), desktop notifications, bell, and colour-scheme queries on `Terminal`, with tmux passthrough
- Scroll hints: `Matrix.set_scroll_hint` turns scrolling into a DECSTBM hardware scroll plus edge-row diffs

#### Changed

- **Prefixed module names** – the sublibraries install `Matrix_grid`, `Matrix_screen`, `Matrix_input`, `Matrix_text`, `Matrix_terminal`, and `Matrix_image` (Toffee likewise `Toffee_style`, `Toffee_tree`, `Toffee_geometry`, `Toffee_compute`) so they cannot collide with other packages' top-level modules; `Matrix.Grid` and friends are unchanged via the umbrella. `Ansi`, `Vte`, and `Pty` deliberately keep their bare names
- **Input responses are separate from events** – `Input.Response` carries clipboard, OSC, capability, and unknown protocol replies out of the user event stream, and the parser defers only the replies a probe currently expects
- Mouse input is one `Input.Mouse` record of coordinates, modifiers, and a kind (scroll included), replacing tuple events and wheel pseudo-buttons; modifiers live in a shared `Input.Modifier` instead of nested under keys
- Colors are emitted at the terminal's negotiated depth: RGB downgrades to ANSI256 or ANSI16 when truecolor is unavailable, and indexed colors are preserved verbatim
- The Image DSL is its own `matrix.image` sublibrary; its aliases collapsed into one `text` constructor and one grid-order `draw`
- All screen buffers share one grapheme and hyperlink store, making the diff sound for complex cells and hyperlinks across frames
- `matrix.screen` drops legacy chaining and runtime-owned metrics; cursor state is a single zero-based value
- The render and submit paths are allocation-free at steady state (retained buffers and writers, unboxed bookkeeping, no per-frame string copies), and each frame leaves the terminal in a single write
- Strict VT100 semantics in the VTE: LF moves down only, BS is pure movement
- Raw mode no longer changes `O_NONBLOCK`; readiness is the backend's concern
- Backtrace recording policy is left to the application; the runtime prints a backtrace on an uncaught exception rather than enabling recording itself
- `Screen.render_to_bytes` removed (no consumers); `render_to_buffer` is the per-frame eliminator
- Vendored uuseg stripped to its boundary core; one shared grapheme-width state machine

#### Fixed

- Capability probing: stray cursor-position replies no longer flip `explicit_width`/`scaled_text` for the session; late capability replies are still applied, and modern terminals seed synchronized output
- Kitty keyboard stack balanced on flag changes; teardown resets only the title/cursor appearance the app actually set
- Terminal setup rolls back transactionally when probing or protocol negotiation fails, so no path escapes with the terminal left raw; app-owned signal dispositions, including SIGHUP, are restored in reverse
- Raw mode clears `IEXTEN`, so the kernel stops swallowing `^O` and `^V` before they reach the application
- The runtime stops at input EOF instead of spinning on a hung-up descriptor; a same-size debounced resize no longer pins the loop timeout at zero; a ready wakeup descriptor wakes the loop with no input pending
- Primary (inline) mode: static output anchors at column one, the shell row survives attach, full-height viewports scroll static rows into history instead of erasing, the erase-below cleanup stops at the viewport bottom, preserved writes are never dropped on partial layouts, and the live view repaints after a static write
- Screen rendering is transactional: a failed write no longer mutates the presented buffer, hit grid, stats, or timing; buffers clear on resize and the diff baseline survives an unchanged resize
- Grid: wide-glyph spans stay atomic across fills, clipping, resizes, and ANSI export; blit alpha is source-driven; terminal-default glyphs cover their destination instead of reading as transparent
- Cursor style is emitted on the first frame and after `suspend`, and cursor visuals are re-emitted when the cursor is shown
- Alt-mode submit skips unchanged frames; `suspend` is idempotent; `on_resize` reports the usable size consistently
- Input parser: colon SGR sub-parameters (undercurl, colon colors), rxvt `$`/`^` keys mid-chunk, bounded SGR-mouse deferral, split-invariant streaming UTF-8, Ctrl letters normalized to lowercase, Ctrl punctuation and Cygwin/libuv F-keys, Alt as Meta, Kitty base-key metadata in lookups, and recovery from interrupted escapes
- Bracketed paste has a bounded lifecycle with configurable payload and idle limits, so an unterminated paste can no longer swallow input or grow without limit
- `matrix.pty`: a child that fails session setup, chdir, or exec exits without running cleanup inherited from the parent
- VTE: scrollback rendering order, wide graphemes at the right margin, insert-mode wrap, DECRC style restore, typed DECSET/DECRST (multi-parameter modes), reverse-index scrolling, scrollback cleared on ED 3, default colors resolved for terminal text, and emulated scroll control sequences
- Combining marks stay attached to ASCII bases in grapheme segmentation; wrapping keeps periods inside their token
- Charts: sub-cell circle rasterization, zoomed y-axis labels, candlestick hit-testing, charset-faithful glyphs
- Grapheme storage no longer leaks ids for clipped draws

#### Removed

- The unsound Windows ConPTY backend: `matrix.pty` is POSIX-only and says so; non-POSIX platforms raise `ENOSYS`
- The `matrix.glyph` library: text semantics moved to `matrix.text` and cell storage into `matrix.grid`
- The `matrix.input` `Keymap` API, superseded by `Mosaic.Shortcut`
- `Ansi.Color.downgrade`, the legacy CSI-u toggles, and the obsolete color-scheme DSR query
- `Input.equal_full`; `Input.equal` is now structural
- The unused `matrix.terminfo` library

### matrix-eio

- Termination signals are handled on the event loop, not inside the OCaml signal handler, so the terminal is restored on SIGTERM/SIGHUP
- The idle wait races input, a level-triggered wake, and a deadline as peer fibers, so a wake arriving outside the wait can no longer strand the render loop under scheduler pressure

### Toffee

#### Fixed

- Flexbox clamps a known cross size by the item's own min/max when measuring, so a percent-width item under `max-width` is no longer measured at a width the final layout never gives it (which let the next sibling paint over it)

#### Removed

- Compatibility aliases `Size.equal_option` and `Available_space.maybe_set`

## [0.1.0] - 2026-02-26

### Mosaic

Terminal UI framework for OCaml built on Matrix and Toffee. Implements The Elm Architecture (TEA) for declarative, composable terminal applications.

- **The Elm Architecture** – Pure functional Model-View-Update pattern with `init`, `update`, `view`, and `subscriptions`. Commands (`Cmd`) handle side effects; subscriptions (`Sub`) handle external events
- **Flexbox and Grid layout** – Powered by Toffee, define complex layouts with familiar CSS properties like `flex_direction`, `align_items`, `justify_content`, and CSS Grid support
- **Rich widget library** – `box`, `text`, `input`, `textarea`, `select`, `tab_select`, `slider`, `table`, `spinner`, `scroll_box`, `canvas`, `code`, `markdown`, `tree`, `progress_bar`, and `line_number`
- **Rich text** – Styled fragments, word/character wrapping, text selection, and full Unicode support
- **Syntax highlighting** – Tree-sitter-based code highlighting with the `code` widget
- **Event system** – Mouse, keyboard, paste, focus, and resize events with `Sub.on_key`, `Sub.on_mouse`, `Sub.on_paste`, `Sub.on_resize`, `Sub.on_focus`, and `Sub.on_blur`
- **Canvas drawing** – Procedural drawing API with shapes, lines, and braille patterns for charts and visualizations
- **Markdown rendering** – CommonMark rendering with the `markdown` widget
- **Dirty tracking and viewport culling** – Only re-layouts dirty subtrees; scroll containers cull off-screen children

### Matrix

Terminal toolkit for OCaml providing rendering, input, and terminal management.

- **Immediate-mode runtime** – `Matrix.run` with `on_frame`, `on_render`, `on_input`, `on_resize` callbacks, configurable FPS capping, and safe teardown that restores terminal state even on exceptions
- **Two display modes** – Full-screen alternate buffer (`Alt`) or inline on primary screen (`Primary`) with dynamic height and full scrollback
- **Declarative Image API** – Notty-inspired compositional DSL with `hcat`, `vcat`, `overlay`, padding, cropping, and hit regions for mouse interaction
- **Full Unicode support** – Grapheme cluster handling, emoji, wide characters, and configurable width calculation (wcwidth, Unicode tables)
- **Modern terminal protocols** – Kitty keyboard (with auto-detection), SGR/X10/URXVT mouse tracking, bracketed paste, focus reporting—all negotiated automatically
- **Native alpha blending** – RGBA colors with proper alpha compositing for translucent overlays
- **Double-buffered diffing** – Screen module diffs cell changes between frames to emit minimal ANSI output
- **PTY & VTE** – Spawn processes in pseudo-terminals and embed terminal output in your UI; cross-platform with Windows ConPTY support
- **Automatic capability detection** – Two-stage probing (environment heuristics + active queries) detects RGB, Kitty keyboard, sixel, hyperlinks, and Unicode width support without manual configuration
- **Hit testing** – O(1) spatial indexing maps mouse coordinates to UI element IDs for clickable widgets
- **Built-in devtools** – Debug overlay for frame timing/FPS, frame dumps to disk for diagnostics

### Toffee

CSS layout engine for OCaml, ported from [Taffy](https://github.com/DioxusLabs/taffy) (Rust).

- **CSS Grid Level 1** – Full implementation including `grid-template-columns/rows`, `grid-auto-flow`, named lines/areas, `repeat()`, `minmax()`, `fr` units, and auto-placement
- **Flexbox** – Complete algorithm with `flex-direction`, `flex-wrap`, `flex-grow/shrink/basis`, `align-items/self/content`, `justify-content`, `gap`, and `order`
- **Block layout** – Traditional CSS block formatting context with proper margin collapsing
- **Pure OCaml, zero dependencies** – No C stubs, no runtime dependencies; works wherever OCaml 5 runs
- **Arena storage and layout caching** – Nodes stored in a flat arena; layout results cached and invalidated via `mark_dirty`
- **Composable architecture** – Use the high-level `Toffee` tree API or the lower-level `toffee.tree` and `toffee.compute` libraries to plug layout into your own node representation
- **Custom measure functions** – Integrate text shaping, images, or any content with intrinsic sizing via `compute_layout_with_measure`
