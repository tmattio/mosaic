# `04-runes`

A typography stress test for `Matrix.Image`. The demo centers dozens of Unicode
strings, draws a border using composable padding helpers, and ends with a note
block to show mixed styling and alignment.

```bash
dune exec ./matrix/examples/04-runes/main.exe
```

## Highlights

- Exercises shaping for Indic, Japanese, Korean, and emoji-heavy text.
- Builds reusable helpers (`centered_text`, `outline_box`, `note_block`) for
  composing images before handing them to the renderer.
- Demonstrates padding, snapping, and background layering utilities.

## Controls

- `Esc` or `Ctrl+C` &mdash; quit.
