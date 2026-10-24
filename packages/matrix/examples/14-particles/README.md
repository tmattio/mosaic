# `14-particles`

A configurable particle system with multiple emitter modes: burst, rainbow,
fountain, explosion, and firework. Particles spawn from the center or follow
the mouse, obeying gravity and fading over their lifetime.

```bash
dune exec ./matrix/examples/14-particles/main.exe
```

## Controls

- `1`&ndash;`5` &mdash; switch emitter mode (Burst, Rainbow, Fountain, Explosion, Firework).
- `G` / `H` &mdash; increase / decrease gravity.
- `+` / `-` &mdash; increase / decrease spawn rate.
- `Space` &mdash; emit a burst at the current mouse position.
- Left click / drag &mdash; spawn particles continuously at the cursor.
- `P` &mdash; pause / resume.
- `C` &mdash; clear all particles.
- `Q` or `Esc` &mdash; quit.

## Highlights

- Thousands of particles with per-frame physics updates.
- Lifetime-based color gradients using HSV and RGB interpolation.
- Firework mode spawns secondary bursts when rockets expire.
