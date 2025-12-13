# Mosaic Examples

Mosaic ships with runnable examples that demonstrate the TEA architecture and
UI components. From the repo root, run any example with:

```bash
dune exec ./mosaic/examples/<name>/main.exe
```

## Examples

| Example        | Description                                      |
| -------------- | ------------------------------------------------ |
| `01-counter`   | Simple counter with TEA basics                   |
| `02-text`      | Styled text with colors and formatting           |
| `03-spinner`   | Animated spinners with presets                   |
| `04-input`     | Text input with cursor and placeholder           |
| `05-select`    | Vertical list selection with keyboard navigation |
| `06-tabs`      | Horizontal tab bar with scroll arrows            |
| `07-slider`    | Value sliders with sub-cell precision            |
| `08-table`     | Data tables with columns and styling             |
| `09-canvas`    | Procedural drawing with shapes and braille       |
| `10-code`      | Syntax-highlighted code with tree-sitter         |
| `11-scrollbox` | Scrollable content with scroll bars              |
| `12-form`      | Multi-component form with focus management       |
| `13-markdown`  | Markdown rendering with scrollable content       |
| `14-selection` | Text selection across renderables                |
| `x-dashboard`  | Component composition with TEA `map`             |

## Descriptions

### `01-counter` – TEA basics

Minimal example showing the TEA (The Elm Architecture) pattern: model, msg,
init, update, view, and subscriptions. Demonstrates keyboard input handling
and the quit pattern.

### `02-text` – Styled text

Rich text rendering with styled fragments. Shows colors (foreground/background),
text attributes (bold, italic, underline), and different wrap modes.

### `03-spinner` – Loading indicators

Animated spinners using built-in presets (Dots, Line, Circle, Bounce, Bar,
Arrow). Demonstrates start/stop control and custom frame intervals.

### `04-input` – Text input

Single-line text input with cursor navigation, placeholder text, and different
cursor styles (block, line, underline). Shows on_change event handling.

### `05-select` – List selection

Vertical list selector with keyboard (Up/Down, j/k, Enter) and mouse support.
Features scroll indicator, item descriptions, and wrap selection.

### `06-tabs` – Tab navigation

Horizontal tab bar with Left/Right navigation. Shows scroll arrows when tabs
exceed available width, selection underline, and optional descriptions.

### `07-slider` – Value control

Horizontal and vertical sliders with sub-cell precision using Unicode
half-blocks. Demonstrates mouse dragging and value change callbacks.

### `08-table` – Data display

Rich tables with configurable columns, headers, footers, and cell styling.
Shows different column width strategies and text overflow handling.

### `09-canvas` – Procedural drawing

Off-screen drawing surface for custom graphics. Demonstrates plotting text,
drawing boxes and lines (including braille sub-cell lines), and fills.

### `10-code` – Syntax highlighting

Code display with tree-sitter grammar-based syntax highlighting. Shows OCaml
and JSON examples with customizable color themes.

### `11-scrollbox` – Scrollable content

Scrollable container with viewport clipping and scroll bars. Demonstrates
vertical scrolling with mouse wheel and keyboard support.

### `12-form` – Form layout

Multi-field form combining text inputs and select components. Shows focus
management with Tab navigation and form submission.

### `13-markdown` – Markdown rendering

Rich markdown display with full CommonMark support. Shows headings, text
formatting (bold, italic, strikethrough, code), links, lists (ordered and
unordered with nesting), code blocks with syntax highlighting, blockquotes,
tables, and task lists. Demonstrates scroll_box for large content.

### `14-selection` – Text selection

Cross-renderable text selection with mouse. Shows selection across multiple
text elements, within scrollable content, and with custom selection colors.
Demonstrates Unicode text selection and selection state tracking.

### `x-dashboard` – Component composition

Multi-component dashboard using TEA's `map` function to compose independent
components. Shows status bar, counters, and stopwatch in a unified layout.
