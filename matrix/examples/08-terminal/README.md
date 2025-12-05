# `08-terminal`

An embedded terminal emulator using a PTY and VTE. Spawns your default shell
inside the Matrix grid and forwards keyboard input, creating a split-screen
terminal within the TUI.

```bash
dune exec ./matrix/examples/08-terminal/main.exe
```

## Controls

- Type normally &mdash; input is forwarded to the embedded shell.
- `Ctrl+D` &mdash; send EOF to the shell (usually exits it).
- `Esc` &mdash; quit the demo.

## Highlights

- Integrates `Pty` and `Vte` modules to spawn and render a real shell.
- Handles resize events by updating both the VTE state and PTY window size.
- Demonstrates non-blocking PTY reads inside the `on_frame` callback.
