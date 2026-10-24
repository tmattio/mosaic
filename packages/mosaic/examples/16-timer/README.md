# `16-timer`

Countdown timer with start, stop, and reset controls. Demonstrates time-based
subscriptions and input handling in the TEA architecture.

```bash
dune exec ./mosaic/examples/16-timer/main.exe
```

## Controls

- `Space` &mdash; start or stop the timer.
- `r` &mdash; reset timer to initial time.
- `q` or `Esc` &mdash; quit.

## Features

- Minutes and seconds input fields for setting custom duration.
- Start/Stop/Reset buttons with mouse interaction.
- Real-time countdown using tick subscriptions.
- Status display showing Idle, Running, or Paused state.

## Highlights

- Demonstrates `Sub.on_tick` for time-based updates.
- Shows input component usage with validation.
- Combines keyboard shortcuts with clickable buttons.
