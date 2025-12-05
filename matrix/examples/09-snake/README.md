# `09-snake`

The classic Snake game rendered in the terminal. Guide the snake to eat food
and grow longer while avoiding walls and your own tail.

```bash
dune exec ./matrix/examples/09-snake/main.exe
```

## Controls

- Arrow keys &mdash; change direction.
- `P` &mdash; pause / resume.
- `R` &mdash; restart after game over.
- `Esc` &mdash; quit.

## Highlights

- Fixed-rate game tick implemented via time accumulation in `on_frame`.
- Demonstrates boundary collision detection and self-collision logic.
- Dynamic resizing resets the game to fit the new terminal dimensions.
