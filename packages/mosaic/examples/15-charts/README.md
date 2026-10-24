# `15-charts`

Interactive charts demo showcasing the matrix.charts library integration with
mosaic. Features multiple chart types with zoom, pan, and hover tooltips.

```bash
dune exec ./mosaic/examples/15-charts/main.exe
```

## Controls

- `Tab` / `Shift+Tab` or `<` / `>` &mdash; switch between chart types.
- Mouse wheel &mdash; zoom at cursor position.
- Drag &mdash; pan the view.
- `+` / `-` &mdash; zoom at center.
- Arrow keys &mdash; pan view.
- `r` &mdash; reset current chart view.
- `0` &mdash; reset all chart views.
- `g` &mdash; toggle grid visibility.
- `G` &mdash; cycle grid pattern (Solid/Dashed/Dotted).
- `c` &mdash; cycle charset (Light/Heavy/Rounded/ASCII).
- `t` &mdash; toggle dark/light theme.
- `m` &mdash; cycle chart-specific rendering mode.
- `p` &mdash; toggle data points on line chart.
- `h` &mdash; toggle help overlay.
- `q` or `Esc` &mdash; quit.

## Chart Types

- **Line** &mdash; waveform with resolution modes (Cell/Wave/Block2x2/Braille).
- **Scatter** &mdash; point cloud with density rendering modes.
- **Bar** &mdash; categorical bar chart with labeled axes.
- **Stacked Bar** &mdash; multi-segment stacked bars with legend.
- **Heatmap** &mdash; intensity grid with multiple render modes.
- **Candlestick** &mdash; OHLC financial chart.

## Highlights

- Per-chart view persistence (zoom/pan state preserved when switching).
- Hit testing with contextual tooltips showing data values.
- Crosshair overlay following cursor position.
- Theme switching between dark and light modes.
