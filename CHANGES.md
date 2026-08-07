# Changelog

## [Unreleased]

### Mosaic

#### Added

- `scroll_bar` and `diff` widgets
- **Runtime probe** – `Mosaic.run ~probe` hands test harnesses a quiescence probe (`Probe.is_settled`) for deterministic settling in headless tests
- **Idle-cheap timers** – a pending `Sub.every` arms a one-shot wakeup instead of holding the render cadence live; only `Sub.on_tick` keeps the cadence running
- Viewport culling for scroll containers and a clean-frame layout skip: off-screen children are neither laid out nor rendered, and layout recomputes only when the tree is dirty
- Scroll containers emit hardware scroll hints: an alt-mode scroll-by-one now reaches the terminal as a DECSTBM row shift plus edge-row diffs
- Widgets follow the terminal-negotiated width method instead of hardcoding Unicode tables

#### Changed

- Frame deltas are milliseconds everywhere (runtime, widgets, Screen post-processors); widget animation constants were split between units before
- Selection/value props are controlled when provided and uncontrolled otherwise, uniformly across `slider`, `select`, `tab_select`, `table`, and `tree`
- `Cmd.focus` expires after one deferred attempt instead of retrying forever; every-timers carry absolute deadlines and no longer phase-merge
- Incremental markdown reconciliation: streaming appends re-render only the tail; `table` caches auto column widths; `line_number` indexes its props
- Edit-buffer undo history is bounded (200 snapshots)

#### Fixed

- `Cmd.perform` messages can no longer be lost to a cross-thread race
- `Sub.on_mouse` and `Sub.on_paste` now see event consumption (they behaved like the `_all` variants)
- Left-click focuses selectable widgets even when it starts a selection; wheel over dead space reaches the focused scrollable
- Tree mouse handling uses widget-local coordinates; `scroll_bar` applies orientation and arrow-color prop updates
- Reconciler tracks id changes on reused fibers, keys duplicates first-occurrence-wins, and places children in linear time
- Single-line editors reserve the caret cell, so an empty focused input shows its cursor

### Matrix

#### Added

- **Pluggable backends** – `Matrix.Backend` is a record of I/O primitives with a shared `bootstrap` handshake; `matrix-eio` provides an Eio runtime on top of it
- **Headless test backend (`matrix.test`)** – virtual clock, byte-level input through the real parser, plain-text frame snapshots; `Screen.create` takes an injectable `?clock`
- **Terminal charts (`matrix.charts`)** – declarative chart composition rendered into grids
- **Timer wakeups and paced one-shot redraws** – `Matrix.schedule_wakeup`, and `request_redraw` bursts coalesce to `target_fps`
- Clipboard (OSC 52), desktop notifications, bell, and colour-scheme queries on `Terminal`, with tmux passthrough
- Scroll hints: `Matrix.set_scroll_hint` turns scrolling into a DECSTBM hardware scroll plus edge-row diffs

#### Changed

- The Image DSL is its own `matrix.image` sublibrary; its aliases collapsed into one `text` constructor and one grid-order `draw`
- All screen buffers share one grapheme and hyperlink store, making the diff sound for complex cells and hyperlinks across frames
- The render and submit paths are allocation-free at steady state (retained buffers and writers, unboxed bookkeeping, no per-frame string copies)
- Strict VT100 semantics in the VTE: LF moves down only, BS is pure movement
- `Screen.render_to_bytes` removed (no consumers); `render_to_buffer` is the per-frame eliminator
- Vendored uuseg stripped to its boundary core; one shared grapheme-width state machine

#### Fixed

- Capability probing: stray cursor-position replies no longer flip `explicit_width`/`scaled_text` for the session
- Kitty keyboard stack balanced on flag changes; teardown resets only the title/cursor appearance the app actually set
- Alt-mode submit skips unchanged frames; `suspend` is idempotent; `on_resize` reports the usable size consistently
- Input parser: colon SGR sub-parameters (undercurl, colon colors), rxvt `$`/`^` keys mid-chunk, bounded SGR-mouse deferral, split-invariant streaming UTF-8
- VTE: scrollback rendering order, wide graphemes at the right margin, insert-mode wrap, DECRC style restore, typed DECSET/DECRST (multi-parameter modes)
- Combining marks stay attached to ASCII bases in grapheme segmentation
- Charts: sub-cell circle rasterization, zoomed y-axis labels, candlestick hit-testing, charset-faithful glyphs
- Grapheme storage no longer leaks ids for clipped draws

#### Removed

- The unsound Windows ConPTY backend: `matrix.pty` is POSIX-only and says so; non-POSIX platforms raise `ENOSYS`
- The unused `matrix.terminfo` library

### matrix-eio

- Termination signals are handled on the event loop, not inside the OCaml signal handler, so the terminal is restored on SIGTERM/SIGHUP

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
