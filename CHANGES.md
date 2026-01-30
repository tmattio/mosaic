# Changelog

## [0.1.0] - Unreleased

### Mosaic

Mosaic is a high-level terminal UI framework for OCaml, built on Matrix and Toffee. It follows The Elm Architecture (TEA) for building declarative, composable terminal applications. Key features include:

- **The Elm Architecture** – Pure functional Model-View-Update pattern with `init`, `update`, `view`, and `subscriptions`. Commands (`Cmd`) handle side effects; subscriptions (`Sub`) handle external events
- **Flexbox and Grid layout** – Powered by Toffee, define complex layouts with familiar CSS properties like `flex_direction`, `align_items`, `justify_content`, and CSS Grid support
- **Rich component library** – Pre-built UI components including `Box`, `Text`, `Text_input`, `Select`, `Tab_select`, `Slider`, `Table`, `Spinner`, `Scroll_box`, `Canvas`, and `Code`
- **Built-in text handling** – Rich text with styled fragments, word/character wrapping, text selection, and full Unicode support via the `Text` and `Text_surface` components
- **Syntax highlighting** – Tree-sitter-based code highlighting with the `Code` component and `mosaic.syntax` library
- **Event system** – Mouse, keyboard, paste, focus, and resize events with `Sub.on_key`, `Sub.on_mouse`, `Sub.on_paste`, `Sub.on_resize`, `Sub.on_focus`, and `Sub.on_blur`
- **Canvas drawing** – Procedural drawing API with shapes, lines, and braille patterns for charts and visualizations
- **Markdown rendering** – CommonMark renderer via `mosaic.markdown` for displaying formatted documentation
- **Performance-first** – Designed for 60+ FPS with minimal allocations, dirty tracking, viewport culling, and smart layout caching

### Matrix

Matrix is a low-level, high-performance terminal UI library for OCaml, designed from the ground up to leverage modern terminal capabilities and provide a solid base for building rich terminal applications. Key features include:

- **Immediate-mode runtime** – `Matrix.run` with `on_frame`, `on_render`, `on_input`, `on_resize` callbacks, configurable FPS capping, and safe teardown that restores terminal state even on exceptions
- **Two display modes** – Full-screen alternate buffer (`Alt`) or inline on primary screen (`Primary`) with dynamic height and full scrollback
- **Declarative Image API** – Notty-inspired compositional DSL with `hcat`, `vcat`, `overlay`, padding, cropping, and hit regions for mouse interaction
- **Full Unicode support** – Grapheme cluster handling, emoji, wide characters, and configurable width calculation (wcwidth, Unicode tables)
- **Modern terminal protocols** – Kitty keyboard (with auto-detection), SGR/X10/URXVT mouse tracking, bracketed paste, focus reporting—all negotiated automatically
- **Native alpha blending** – RGBA colors with proper alpha compositing for translucent overlays
- **Zero-allocation diffing** – Screen module emits minimal ANSI output by comparing frames
- **PTY & VTE** – Spawn processes in pseudo-terminals and embed terminal output in your UI; cross-platform with Windows ConPTY support
- **Automatic capability detection** – Two-stage probing (environment heuristics + active queries) detects RGB, Kitty keyboard, sixel, hyperlinks, and Unicode width support without manual configuration
- **Hit testing** – O(1) spatial indexing maps mouse coordinates to UI element IDs for clickable widgets
- **Built-in devtools** – Debug overlay for frame timing/FPS, frame dumps to disk for diagnostics

### Toffee

Toffee is a high-performance CSS layout engine for OCaml, ported from the battle-tested Rust library [Taffy](https://github.com/DioxusLabs/taffy). It implements the CSS Block, Flexbox, and Grid layout algorithms and is designed to be embedded in UI frameworks, terminal applications, game engines, or any project that needs reliable 2D layout. Key features include:

- **CSS Grid Level 1** – Full implementation including `grid-template-columns/rows`, `grid-auto-flow`, named lines/areas, `repeat()`, `minmax()`, `fr` units, and auto-placement
- **Flexbox** – Complete algorithm with `flex-direction`, `flex-wrap`, `flex-grow/shrink/basis`, `align-items/self/content`, `justify-content`, `gap`, and `order`
- **Block layout** – Traditional CSS block formatting context with proper margin collapsing
- **Pure OCaml, zero dependencies** – No C stubs, no runtime dependencies; works wherever OCaml 5 runs
- **High performance** – Arena-style node storage, layout caching, and tight loops minimize allocations and GC pressure
- **Composable architecture** – Use the high-level `Toffee` tree API or the lower-level `toffee.tree` and `toffee.compute` libraries to plug layout into your own node representation
- **Custom measure functions** – Integrate text shaping, images, or any content with intrinsic sizing via `compute_layout_with_measure`
