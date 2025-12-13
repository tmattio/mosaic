# `13-dragdrop`

A drag-and-drop interface with movable tiles and drop zones. Click a tile to
start dragging, move it over a zone, and release to record the drop. Zones
highlight when a tile hovers over them.

```bash
dune exec ./matrix/examples/13-dragdrop/main.exe
```

## Controls

- Left click on a tile &mdash; start dragging.
- Release over a zone &mdash; drop the tile; the zone remembers the last drop.
- `Esc` while dragging &mdash; cancel and snap back.
- `Esc` or `Q` &mdash; quit.

## Highlights

- Full mouse tracking with press, release, and motion events.
- Alpha-blended tiles and zones using `respect_alpha` mode.
- Demonstrates z-order management by moving dragged items to the front.
