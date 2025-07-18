open Mosaic

let markdown_content =
  {|# Markdown Demo

This demonstrates **Mosaic's** markdown rendering capabilities.

## Text Formatting

- **Bold text** with double asterisks
- *Italic text* with single asterisks
- ***Bold and italic*** with triple asterisks
- `inline code` with backticks
- ~~Strikethrough~~ with double tildes

## Links

- [OCaml website](https://ocaml.org)
- [Mosaic GitHub](https://github.com/spice/mosaic)
- <https://example.com> - autolinks

## Lists

### Unordered Lists

- First item
- Second item
  - Nested item 1
  - Nested item 2
    - Deep nested
- Third item

### Ordered Lists

1. First step
2. Second step
   1. Sub-step A
   2. Sub-step B
3. Third step

## Code Blocks

```ocaml
(* OCaml code *)
let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let () = Printf.printf "5! = %d\n" (factorial 5)
```

```bash
# Shell commands
dune build @all
dune exec my_app
```

## Blockquotes

> This is a blockquote.
> It can span multiple lines.
>
> > Nested blockquotes are supported.
> > They can contain **formatted text**.

## Tables

| Column 1 | Column 2 | Column 3 |
|----------|----------|----------|
| Cell A1  | Cell B1  | Cell C1  |
| Cell A2  | Cell B2  | Cell C2  |
| Cell A3  | Cell B3  | Cell C3  |

## Horizontal Rules

---

## Inline HTML (as text)

<div>HTML tags are rendered as plain text</div>

## Task Lists

- [x] Completed task
- [ ] Incomplete task
- [x] Another completed task

## Definitions

Term 1
: Definition for term 1

Term 2
: Definition for term 2
: Alternative definition

## Footnotes

Here's a sentence with a footnote[^1].

[^1]: This is the footnote text.

## Images (as text)

![Alt text](image.png)

## Complex Example

### A Real-World Code Example

```ocaml
module App = struct
  type model = {
    count : int;
    message : string;
  }

  type msg =
    | Increment
    | Decrement
    | Reset
    | SetMessage of string

  let init () =
    ({ count = 0; message = "Welcome!" }, Cmd.none)

  let update msg model =
    match msg with
    | Increment ->
        ({ model with count = model.count + 1 }, Cmd.none)
    | Decrement ->
        ({ model with count = model.count - 1 }, Cmd.none)
    | Reset ->
        ({ model with count = 0 }, Cmd.none)
    | SetMessage text ->
        ({ model with message = text }, Cmd.none)

  let view model =
    Ui.vstack [
      Ui.text ~style:Style.(fg Green) model.message;
      Ui.text (Printf.sprintf "Count: %d" model.count);
    ]
end
```

> **Note**: This example shows how The Elm Architecture works in Mosaic.
> The `model` holds state, `msg` defines events, and `update` handles
> state transitions.
|}

let init () =
  let element = Ui.flow [ Mosaic_markdown.render markdown_content ] in
  ((), Cmd.seq [ Cmd.print element; Cmd.quit ])

let update _msg model = (model, Cmd.none)
let view _model = Ui.text ""
let app = Mosaic.app ~init ~update ~view ()
let () = Mosaic.run ~alt_screen:false app
