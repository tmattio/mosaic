# Mosaic_markdown

A library for rendering markdown on the terminal within the [Mosaic](https://github.com/ocaml-tui/mosaic) framework, inspired by [Glamour](https://github.com/charmbracelet/glamour).

This library parses markdown text using [Cmarkit](https://erratique.ch/software/cmarkit) and transforms it into a `Mosaic.Ui.element`, which can be seamlessly integrated into your TUI applications.

## Features

- Renders markdown to `Mosaic.Ui.element`s.
- Supports common markdown elements: headings, lists (ordered/unordered, nested), block-quotes, code blocks, and more.
- Inline styling for **bold**, *italic*, `code`, and [links](https://example.com).
- Rich, declarative styling system similar to Glamour's.
- Automatic word-wrapping.

## Usage

First, add `mosaic.markdown` to your `dune` file:

```dune
(executable
 (name my_app)
 (libraries mosaic mosaic.markdown))
```

Then, you can use it in your application's `view` function:

```ocaml
open Mosaic
open Mosaic_markdown

let my_markdown_content =
"
# Hello, Mosaic!

This is a paragraph rendered with `Mosaic_markdown`.

- One
- Two
  - Nested
- Three

> A blockquote.
"

let view model =
  (* Simply call render on your markdown string *)
  Markdown.render ~width:80 my_markdown_content

(* ... rest of your application ... *)
```

## Custom Styling

You can customize the appearance by providing a `Style.t` record. Start with `Style.default` and modify the fields you want to change.

```ocaml
let custom_style =
  let open Mosaic_markdown.Style in
  let open Ui.Style in
  {
    default with
    h1 = { default.h1 with style = bg (Index 63) ++ fg (Index 228) };
    code = fg (Index 117);
    block_quote = { default.block_quote with style = fg (Index 135) };
  }

let view model =
  Mosaic_markdown.render ~style:custom_style my_markdown_content
```
