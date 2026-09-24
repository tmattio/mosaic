# `19-tree`

Hierarchical tree view with expand/collapse and guide lines.

```bash
dune exec ./mosaic/examples/19-tree/main.exe
```

## Controls

- Up/Down arrows or `j`/`k` &mdash; navigate.
- Left/Right arrows &mdash; collapse/expand.
- Space &mdash; toggle expand/collapse.
- Enter &mdash; activate (open) the selected node.
- `g` &mdash; toggle guide lines.
- `s` &mdash; toggle guide style (rounded/single).
- `q` or `Esc` &mdash; quit.

## Features

- Nested file-system-like tree with two root projects.
- Expand/collapse via keyboard with guide line indicators.
- Switchable guide styles (rounded and single box-drawing).
- Info panel showing selected item ID, guide state, and activated item.
- Wrap selection at list boundaries.

## Highlights

- Stores selection and expansion together in `Tree.State.t`.
- Accepts complete `on_change` proposals; `on_activate` supplies the item.
- Gives every `Tree.item` a stable, unique ID.
- Uses `Border.rounded` and `Border.single` for guide style switching.
