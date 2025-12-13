# `07-hexagon`

A spinning hexagon with a bouncing ball inside. The ball obeys gravity and
reflects off the rotating walls, transferring energy from the hexagon's spin
into its velocity.

```bash
dune exec ./matrix/examples/07-hexagon/main.exe
```

## Controls

- `Space` &mdash; reset the ball with a random velocity.
- `Esc` &mdash; quit.

## Highlights

- Physics simulation with gravity, bounce damping, and rotating reference frame.
- Bresenham line drawing for the hexagon edges.
- Demonstrates `on_frame` delta-time updates for smooth animation.
