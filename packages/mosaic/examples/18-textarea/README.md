# `18-textarea`

Multi-line text editor demonstrating the `textarea` widget with wrapping
modes, placeholder text, and input callbacks.

```bash
dune exec ./packages/mosaic/examples/18-textarea/main.exe
```

## Controls

- `F2` &mdash; cycle wrap mode (Word, Char, None).
- `Ctrl+Enter` &mdash; submit text.
- `Esc` &mdash; quit.

## Features

- Full multi-line editing with cursor, selection, and scrolling.
- Three wrap modes: word, character, and none.
- Status bar showing character, word, and line counts.
- Placeholder text when empty.
- Submit flash message on `Ctrl+Enter`.

## Highlights

- Demonstrates the `textarea` widget with model-owned `Editor.t`, `on_change`, and `on_submit`.
- Shows `~wrap` mode switching at runtime.
- Uses `focused:true` for immediate input readiness.
