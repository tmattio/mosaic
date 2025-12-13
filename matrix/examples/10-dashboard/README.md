# `10-dashboard`

A system-monitor-style dashboard with animated progress bars, a statistics
panel, and a scrollable activity log. Metrics update continuously and log
entries appear at random intervals.

```bash
dune exec ./matrix/examples/10-dashboard/main.exe
```

## Controls

- `↑` / `↓` &mdash; scroll the activity log.
- `Page Up` / `Page Down` &mdash; scroll by larger increments.
- `Q` or `Esc` &mdash; quit.

## Highlights

- Multi-panel layout with box-drawing characters.
- Progress bars with color-coded fill and percentage labels.
- Log entries colored by severity (INFO, WARN, ERROR, DEBUG).
