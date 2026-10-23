# Changelog

## [Unreleased]

### Mosaic

#### Added

- **Shortcut subscriptions** – `Mosaic.Shortcut` and `Sub.on_keys` bind keys to messages declaratively, with base-key and modifier matching
- **Runtime probe** – `Mosaic.run ~probe` hands test harnesses a quiescence probe (`Probe.is_settled`) for deterministic settling in headless tests
- **Idle-cheap timers** – a pending `Sub.every` arms a one-shot wakeup instead of holding the render cadence live; only `Sub.on_tick` keeps the cadence running
- **Syntax API** – `Code.syntax` replaces the widget's public `spans` argument; `Code.Highlighter` runs highlighting synchronously or asynchronously with cancellation, stale-result protection, and a plain-text fallback
- **A stalled paste can be ended** – `Sub.on_paste_stalled seconds msg` reports a paste in progress that has received nothing for `seconds`, and `Cmd.abort_paste` ends and drops it. A paste still ends only at its end marker: one whose marker never comes takes every later key as content, Ctrl+C included, and the application decides when to give up
- **Terminal commands** – `Cmd.set_clipboard`, `Cmd.copy_selection`, `Cmd.clear_selection`, `Cmd.bell`, `Cmd.notify`, and `Cmd.query_color_scheme` paired with `Sub.on_color_scheme`
- `scroll_bar` and `diff` widgets; `diff` renders unified and split patches with per-line signs, highlighted source lines, sub-line highlight spans, line click hits, and source-row reveal
- `Mosaic.viewport_switch` picks a subtree by terminal width before layout, keeping compatible keyed children across the switch
- `scroll_box` takes keyed one-shot `?reveal`, `?scroll_by`, and `?reset_sticky` requests with `?on_scroll_by_applied` / `?on_reset_sticky_applied` acknowledgements, plus `?show_scrollbars` for a bare scrolling surface
- `markdown` takes a `?highlighter` and a `?syntax_style` that highlight fenced code blocks, and `?on_selection` reporting the selected rendered text
- `table` gains passive selection, overflow indicators, hover and activation callbacks, and PageUp/PageDown paging over its measured visible rows
- A drag that reaches a scroll box edge auto-scrolls and keeps the selection in sync with the new viewport
- Viewport culling for scroll containers and a clean-frame layout skip: off-screen children are neither laid out nor rendered, and layout recomputes only when the tree is dirty
- Scroll containers emit hardware scroll hints: an alt-mode scroll-by-one now reaches the terminal as a DECSTBM row shift plus edge-row diffs
- Widgets follow the terminal-negotiated width method instead of hardcoding Unicode tables

#### Changed

- **Listeners have a phase** – `Sub.on_key`, `on_keys`, `on_mouse` and `on_paste` take `?phase`. A `Bubble` listener, the default, runs after the elements and sees only the events nothing stopped or prevented, so `Sub.on_keys [ (Shortcut.char 'q', Quit) ]` no longer quits while `q` is typed into a focused input. A `Capture` listener runs before the elements and sees every event; stopping an event never hides it from another listener of the application. `on_key_all`, `on_mouse_all` and `on_paste_all` are gone (use `~phase:Capture`), and `Sub.On_key`, `On_mouse` and `On_paste` carry the phase
- **Events go along a path, as in the DOM** – a key or a paste, like a mouse event, goes along a path taken when it is dispatched: the target's handlers, then its behavior (the character a key types, the text a paste inserts), then each ancestor's handlers, where keys and pastes reached the focused node only. The behavior runs on the node focused when it runs. `stop_propagation` keeps an event from the nodes after the current one, never from the current node's other handlers, and cancels no behavior; `prevent_default` cancels the behavior and the renderer's defaults. A behavior that acts stops the event. A press reaches its handlers before its defaults (text selection, the caret, the click), which a handler can now prevent. `Renderer.dispatch_key`, `dispatch_paste` and `dispatch_mouse` lose `?capture` and take a path from `Renderer.focus_path` or `Renderer.mouse_path`; `Renderable.set_default_key_handler` and `set_paste_handler` are `set_key_behavior` and `set_paste_behavior`; `Renderable.Private.emit_mouse` runs one node's handlers
- Frame deltas are milliseconds everywhere (runtime, widgets, Screen post-processors); widget animation constants were split between units before
- Selection/value props are controlled when provided and uncontrolled otherwise, uniformly across `slider`, `select`, `tab_select`, `table`, and `tree`
- Editable widgets take key bindings and aliases, and user paste handlers run before widget default paste behavior, matching OpenTUI's default-prevention semantics
- **Shortcuts match exactly** – a shortcut matches the key and its modifiers, then the base-layout key; the lowercase fallback, keypad normalization and Alt-implies-Meta are gone, `char ~shift:true 'r'` is `'R'`, and `Line_feed` bindings are Ctrl+J. Editors insert the keystroke's text; key releases reach `Sub.on_key_release`, never widgets; a paste reaches widgets whole once it completes
- `Cmd.focus` expires after one deferred attempt instead of retrying forever; every-timers carry absolute deadlines and no longer phase-merge
- Incremental markdown reconciliation: streaming appends re-render only the tail; `table` caches auto column widths; `line_number` indexes its props
- **Undo works in steps** – an `Editor` keeps its whole undo history as inverse changes, each costing the size of its change, and groups typing with no clock: typing, `delete_backward` and `delete_forward` each join the step they continue at the caret, until the caret moves, the selection changes or the kind of edit changes. A space after other text, other text after two or more spaces, and a line break start a new step; a paste, a word or line deletion and `clear` are steps of their own
- **Text follows the screen's width method** – a text buffer holds no width method: `Text_buffer.create` loses `?width_method`, `width_method` and `set_width_method` go, and `line_width` and `max_line_width` take the method to count by. Text, Code, editors, spinners, canvases and static output measure and paint by the screen's method at the time, and the renderer measures every node again when it changes, so a terminal that reports its method after the first frame no longer leaves existing widgets laid out by the old one
- **Primary frames are laid out once** – `Renderer.render_frame` is `Renderer.layout_frame` followed by `Renderer.paint_frame`, and loses `?layout_height`. In primary mode the runtime lays the frame out when it reports the rows it requires and paints that layout into the live region, where it laid the tree out once to learn the rows and again to draw; input geometry is laid out against the terminal's height too, so the tree is no longer laid out at two heights in turn

#### Fixed

- `Cmd.perform` messages can no longer be lost to a cross-thread race
- `Sub.on_mouse` and `Sub.on_paste` now see event consumption (they behaved like the `_all` variants)
- A mouse event a widget stopped reached only the first `Sub.on_mouse_all` listener; every capture listener now sees it
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
- **Headless test backend (`matrix.test`)** – virtual clock, byte-level input through the real decoder, plain-text frame snapshots; `Screen.create` takes an injectable `?clock`
- **Unicode text layer (`matrix.text`)** – grapheme iteration, width measurement, wrap-break and line-break discovery, and position helpers, independent of cell storage
- **Terminal charts (`matrix.charts`)** – declarative chart composition rendered into grids
- **Timer wakeups and paced one-shot redraws** – `Matrix.schedule_wakeup`, and `request_redraw` bursts coalesce to `target_fps`; `Matrix.request_immediate_redraw` renders one frame off-cadence
- **Primary-mode static output** – `Matrix.static_write ?preserve_live_region` keeps the live region across writes, `Matrix.static_replace` swaps all static content in one frame transaction, and `Matrix.static_clear` resets it
- Clipboard (OSC 52), desktop notifications, bell, and colour-scheme queries on `Terminal`, with tmux passthrough
- Scroll hints: `Matrix.set_scroll_hint` turns scrolling into a DECSTBM hardware scroll plus edge-row diffs
- **Application queries** – `Matrix.query app ~owed q` sends the application's queries as a batch closed by DA1, owed as the runtime's own are, and delivers their replies as `Matrix.Reply` events ending with the DA1 reply. Every DA1 reply is alike, so one batch that ends with DA1, the runtime's or the application's, is in flight at a time. A query sent with `Terminal.send` is not owed: its reply never reaches the application as a reply, and one that is a control string (DA3, XTVERSION, OSC 10/11, an OSC 52 read, XTGETTCAP) decodes as typing

#### Changed

- **Prefixed module names** – the sublibraries install `Matrix_grid`, `Matrix_screen`, `Matrix_input`, `Matrix_text`, `Matrix_terminal`, and `Matrix_image` (Toffee likewise `Toffee_style`, `Toffee_tree`, `Toffee_geometry`, `Toffee_compute`) so they cannot collide with other packages' top-level modules; `Matrix.Grid` and friends are unchanged via the umbrella. `Ansi`, `Vte`, and `Pty` deliberately keep their bare names
- **Input is decoded with no clock** – `Input.Decoder` replaces `Input.Parser` and `Input.Event`: one ordered stream of keystrokes (`Input.Keystroke`), mouse reports (`Input.Mouse_report`), paste, focus and replies (`Input.Reply`), the runtime saying when the terminal was silent. A keystroke has one value in every encoding: `Char` is the character typed with Shift consumed (Shift+a is `'A'`, and Caps Lock with a is `'A'` under the kitty protocol's associated text too), Ctrl+j is the old `Line_feed`, keypad keys are main keys with `keypad`, releases are `Key_release`, Alt no longer sets Meta, and bytes that are not UTF-8 are `Invalid`. A reply is never typed while its batch of queries is owed, and a control string begins only while a string reply is owed
- **The application's events are `Matrix.event`** – paste streams as `Paste_start`, `Paste_data` and `Paste_end` with no limit, and `Matrix.abort_paste` ends one; focus is `Focus_in`/`Focus_out`; `Text` carries text a terminal reports without a key. `create` lost `focus_reporting` and `` `Enabled flags ``, takes `?key_releases` and `?caps` (in place of `initial_caps`), and `Backend.t` carries the session's environment
- **Capabilities are derived, not accumulated** – `Terminal.Caps.derive` is a pure function of the environment, read once, and the answers so far, by one precedence: user policy, cited corrections, the answer of the terminal doing the work, identity, guesses. A late answer can take back a guess, and the runtime then changes the modes by their difference. Identity comes from the environment only when the terminal is directly attached; inside tmux, questions go to tmux itself, its facts come from its version when it is silent, and only questions about the outer terminal are wrapped, once passthrough has answered. The VS Code rule (no recorded reason) and `KITTY_WINDOW_ID` implying SIXEL are gone, and a color scheme report no longer implies mode 2031. On Windows, SIXEL and the kitty protocols are no longer turned off by a guess: they follow the terminal's answers, as everywhere else. The probe no longer sends iTerm2's `OSC 1337;ReportCellSize`, whose reply nothing used, and `identity` no longer records whether it came from XTVERSION or `TERM_PROGRAM`
- **Input modes** – the startup probe sends S7C1T first, so no reply comes with 8-bit controls; focus (1004), color scheme (2031) and resize (2048) reports are set without asking, since xterm, kitty, foot and tmux ignore a private mode they do not know (their source); an XTWINOPS text-area report, `CSI 8 ; rows ; cols t`, no longer resizes: it is the reply to `CSI 18 t`, which the runtime never sends, and the in-band resize report is mode 2048's, so an application that asks with `Matrix.query` receives it as a `Reply`; metaSendsEscape (1036) is set where the terminal has it reset, and reset on exit; the kitty flags are pushed once the terminal answers, then asked for again; a batch of queries stays owed until its DA1, however late, or for max(5 s, 4 RTT) if the terminal never answers, while the runtime waits for an answer at most max(500 ms, 4 RTT); a late DA1 still measures the round trip, and the ESC and sequence waits scale with it; the probe's width tests are written on the second row, so their cursor reports are never a modified F3
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
- **Primary layout by scrolling** – the live region claims rows only by scrolling the rows above it into the terminal's scrollback, and static rows go at its top, so a transcript row is never overwritten. No scrolling margins (DECSTBM) are set, no blank row separates static output from the live region, and `static_write ~preserve_live_region` keeps the live height whatever is above it
- `Grid.active_height` counts every row that shows something a terminal's erase does not: a space with a background, an attribute or a hyperlink counts, not only a character. A primary-mode full redraw no longer erases a trailing row of background, and a static commit keeps it
- **Primary frames have their rows before they are drawn** – `Matrix.prepare ?primary_required_rows` grows the live region and sizes the frame to it, and `run` asks `primary_required_rows` before `on_render`, so a frame that needs more rows shows them, where the first grown frame showed only the old height. `Matrix.submit` no longer takes the rows; static writes go to the transcript in `prepare`, after which the live region still has the rows the frame requires. Mosaic lays its tree out before the frame to know them
- `Grid` reads and writes its cell planes with direct loads and stores, where every access called into the runtime: a full repaint takes a third of the time it took, and drawing a screen of text less than half
- `Screen.Hit_grid` fills its cells with direct stores, where every cell called into the runtime: a wheel event in a 250x70 Mosaic application costs a fifth of what it did, and a frame a quarter less

#### Fixed

- Capability probing: stray cursor-position replies no longer flip `explicit_width`/`scaled_text` for the session; late capability replies are still applied, and modern terminals seed synchronized output. The environment is no longer read behind the caller's back (a `$TMUX` set in the developer's shell wrapped the probe)
- Kitty keyboard stack balanced on flag changes; teardown resets only the title/cursor appearance the app actually set
- Terminal setup rolls back transactionally when probing or protocol negotiation fails, so no path escapes with the terminal left raw; app-owned signal dispositions, including SIGHUP, are restored in reverse
- Raw mode clears `IEXTEN`, so the kernel stops swallowing `^O` and `^V` before they reach the application
- The runtime stops at input EOF instead of spinning on a hung-up descriptor; a same-size debounced resize no longer pins the loop timeout at zero; a ready wakeup descriptor wakes the loop with no input pending
- Primary (inline) mode: static output anchors at column one, the shell row survives attach, full-height viewports scroll static rows into history instead of erasing, the erase-below cleanup stops at the viewport bottom, preserved writes are never dropped on partial layouts, and the live view repaints after a static write
- Screen rendering is transactional: a failed write no longer mutates the presented buffer, hit grid, stats, or timing; buffers clear on resize and the diff baseline survives an unchanged resize
- Grid: wide-glyph spans stay atomic across fills, clipping, resizes, and ANSI export; blit alpha is source-driven; terminal-default glyphs cover their destination instead of reading as transparent
- Cursor style is emitted on the first frame and after `suspend`, and cursor visuals are re-emitted when the cursor is shown
- Alt-mode submit skips unchanged frames; `suspend` is idempotent; `on_resize` reports the usable size consistently
- Input: long replies are never typed whatever their length or split, a stalled or late reply is never typed while its batch is owed, nor a stalled paste, modified F1–F4, SS3 with modifiers and Alt with an introducer decode, SGR motion reads its button from the report, and Ctrl punctuation, Cygwin/libuv F-keys and rxvt keys decode in every split
- `matrix.pty`: a child that fails session setup, chdir, or exec exits without running cleanup inherited from the parent
- VTE: scrollback rendering order, wide graphemes at the right margin, insert-mode wrap, DECRC style restore, typed DECSET/DECRST (multi-parameter modes), reverse-index scrolling, scrollback cleared on ED 3, default colors resolved for terminal text, and emulated scroll control sequences
- Combining marks stay attached to ASCII bases in grapheme segmentation; wrapping keeps periods inside their token
- `matrix.text` measures 897 East_Asian_Width N pictographs as one cell, as UAX #11 and terminals do, instead of two: emoji without Emoji_Presentation such as ❤ U+2764 and ⚠ U+26A0, and symbols such as mahjong tiles, dominoes, alchemical symbols and chess pieces. VS16 still widens an emoji to two cells
- Charts: sub-cell circle rasterization, zoomed y-axis labels, candlestick hit-testing, charset-faithful glyphs
- Grapheme storage no longer leaks ids for clipped draws
- A scroll hint that shifts its whole region is ignored: it keeps no row, and xterm 398 also blanks the rows outside the region on such an SU, which the diff never repainted
- Primary mode follows the terminal through a resize: the runtime finds the live region again from a cursor position report, where it overwrote transcript rows or left a stale copy of the UI. A minimum height that does not fit below the prompt scrolls the shell's rows up instead of covering them, `min_tui_height` survives a terminal shorter than it, and a static area of one row no longer raises
- `Screen` keeps apart two cells the terminal would read as one grapheme, such as a letter and a lone emoji modifier or a Prepend character and a letter, which shifted the rest of the row
- After a resize, `Screen` paints every cell of its next frame; its blank baseline skipped the cells blank in the new frame and left the old frame's there, as on the frame after a primary live region grows
- A visible cursor without a position shows in alt mode, at the bottom-left; in primary mode a cursor below the painted rows of a full redraw keeps its row instead of rising to the last one
- `Grid` keeps one rendition per grapheme, its start cell's: a translucent background drawn over a wide character blends over the start cell's background for both halves, which a terminal could not show apart, and a translucent fill over a wide character tints it instead of erasing it and blending its second cell twice
- The runtime sets the terminal's width method and the screen's together whenever the capabilities change, as when a DECRPM 2027 report arrives after the first frame, and repaints: the screen kept the method of its creation and laid out cells the terminal measured otherwise. With `explicit_width` the screen measures with wcwidth, as the terminal does
- `suspend` hands the screen over clean: in primary mode it sends what the frame had planned for the transcript, erases the live region and leaves the cursor shown at its start, so a program run meanwhile writes below the transcript rather than over the stale UI, and `resume` starts the live region below that output; in alt mode it erases the screen unless it leaves it. The cursor was left hidden

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
