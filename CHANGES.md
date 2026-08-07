# Changelog

## [0.1.0] - Unreleased

### Mosaic

Terminal UI framework for OCaml built on Matrix and Toffee. Implements The Elm Architecture (TEA) for declarative, composable terminal applications.

- **The Elm Architecture** – Pure functional Model-View-Update pattern with `init`, `update`, `view`, and `subscriptions`. Commands (`Cmd`) handle side effects; subscriptions (`Sub`) handle external events
- **Flexbox and Grid layout** – Powered by Toffee, define complex layouts with familiar CSS properties like `flex_direction`, `align_items`, `justify_content`, and CSS Grid support
- **Rich widget library** – `box`, `text`, `input`, `textarea`, `select`, `tab_select`, `slider`, `table`, `spinner`, `scroll_box`, `scroll_bar`, `canvas`, `code`, `diff`, `markdown`, `tree`, `progress_bar`, and `line_number`
- **Rich text** – Styled fragments, word/character wrapping, text selection, and full Unicode support
- **Syntax highlighting** – Tree-sitter-based code highlighting with the `code` widget
- **Event system** – Mouse, keyboard, paste, focus, and resize events with `Sub.on_key`, `Sub.on_mouse`, `Sub.on_paste`, `Sub.on_resize`, `Sub.on_focus`, and `Sub.on_blur`
- **Canvas drawing** – Procedural drawing API with shapes, lines, and braille patterns for charts and visualizations
- **Markdown rendering** – CommonMark rendering with the `markdown` widget
- **Dirty tracking and viewport culling** – Only re-layouts dirty subtrees; scroll containers cull off-screen children
- **Runtime probe** – `Mosaic.run ~probe` hands test harnesses a quiescence probe (`Probe.is_settled`: pending messages, in-flight performs, renderer settlement) for deterministic settling in headless tests
- **Idle-cheap timers** – `Sub.every` no longer holds the render cadence live: a pending timer arms a one-shot loop wakeup and renders only when it fires, so a timer-bearing app stops paying a full view/reconcile/layout/paint pass per frame while nothing changes. Only `Sub.on_tick` keeps the cadence running

### Matrix

Terminal toolkit for OCaml providing rendering, input, and terminal management.

- **Immediate-mode runtime** – `Matrix.run` with `on_frame`, `on_render`, `on_input`, `on_resize` callbacks, configurable FPS capping, and safe teardown that restores terminal state even on exceptions
- **Pluggable backends** – The runtime drives a small `Matrix.Backend` record of I/O primitives (clock, wake, read, write, resize), so alternative event loops can host Matrix; the `matrix-eio` package provides an Eio-based runtime on top of it
- **Two display modes** – Full-screen alternate buffer (`Alt`) or inline on primary screen (`Primary`) with dynamic height and full scrollback
- **Declarative Image API (`matrix.image`)** – Notty-inspired compositional DSL with `hcat`, `vcat`, `overlay`, padding, cropping, and hit regions for mouse interaction
- **Full Unicode support** – Grapheme cluster handling, emoji, wide characters, and configurable width calculation (wcwidth, Unicode tables)
- **Modern terminal protocols** – Kitty keyboard (with auto-detection), SGR/X10/URXVT mouse tracking, bracketed paste, focus reporting—all negotiated automatically
- **Native alpha blending** – RGBA colors with proper alpha compositing for translucent overlays
- **Double-buffered diffing** – Screen module diffs cell changes between frames to emit minimal ANSI output; all screen buffers share one grapheme and hyperlink store so complex cells diff by handle, and scrollable regions can pass a scroll hint that turns scrolling into a DECSTBM hardware scroll plus edge-row diffs
- **Terminal charts (`matrix.charts`)** – Declarative chart composition (line, bar, heatmap, candlestick, sparkline, and more) rendered into grids
- **PTY & VTE** – Spawn processes in POSIX pseudo-terminals and embed terminal output in your UI through a virtual terminal emulator
- **Automatic capability detection** – Two-stage probing (environment heuristics + active queries) detects RGB, Kitty keyboard, sixel, hyperlinks, and Unicode width support without manual configuration
- **Hit testing** – O(1) spatial indexing maps mouse coordinates to UI element IDs for clickable widgets
- **Built-in devtools** – Debug overlay for frame timing/FPS, frame dumps to disk for diagnostics
- **Headless test backend (`matrix.test`)** – Drive any Matrix or Mosaic application in-process with a virtual clock, byte-level input through the real parser, and plain-text frame snapshots; supported by new core eliminators `Grid.to_text`, `Matrix.current_grid` (the presented frame), `Matrix.redraw_requested`, and `Matrix.now`. `Screen.create` (and `Renderer.create`) accept an injectable `?clock` so frame timestamps and post-processor deltas follow the runtime clock instead of the wall clock
- **Timer wakeups and paced one-shot redraws** – `Matrix.schedule_wakeup` arms a one-shot `on_frame` wakeup so time-based work advances off the clock without rendering; outside live mode `request_redraw` bursts coalesce to `target_fps` instead of rendering back-to-back

### Toffee

CSS layout engine for OCaml, ported from [Taffy](https://github.com/DioxusLabs/taffy) (Rust).

- **CSS Grid Level 1** – Full implementation including `grid-template-columns/rows`, `grid-auto-flow`, named lines/areas, `repeat()`, `minmax()`, `fr` units, and auto-placement
- **Flexbox** – Complete algorithm with `flex-direction`, `flex-wrap`, `flex-grow/shrink/basis`, `align-items/self/content`, `justify-content`, `gap`, and `order`
- **Block layout** – Traditional CSS block formatting context with proper margin collapsing
- **Pure OCaml, zero dependencies** – No C stubs, no runtime dependencies; works wherever OCaml 5 runs
- **Arena storage and layout caching** – Nodes stored in a flat arena; layout results cached and invalidated via `mark_dirty`
- **Composable architecture** – Use the high-level `Toffee` tree API or the lower-level `toffee.tree` and `toffee.compute` libraries to plug layout into your own node representation
- **Custom measure functions** – Integrate text shaping, images, or any content with intrinsic sizing via `compute_layout_with_measure`
