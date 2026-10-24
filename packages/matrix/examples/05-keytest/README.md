# `05-keytest`

A diagnostic tool for inspecting keyboard and mouse input. The demo displays a
live modifier panel and an event history that shows every keystroke, mouse
action, and resize event in real time.

```bash
dune exec ./matrix/examples/05-keytest/main.exe
```

## Controls

- Press any key &mdash; see it appear in the event history with full metadata.
- Move the mouse &mdash; motion events are logged along with button state.
- `Esc` &mdash; quit.

## Highlights

- Real-time modifier indicator panel (Ctrl, Alt, Shift, Super, Hyper, Meta,
  CapsLock, NumLock).
- Scrolling event history with timestamp-based dimming for older entries.
- Demonstrates `Input.pp` formatting and Kitty keyboard protocol metadata.
