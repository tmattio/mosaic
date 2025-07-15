# Component Counter Example

This example demonstrates the Component API for Mosaic, showing how to compose reusable UI components.

The example creates two independent counter components that are composed into a single application. Each counter maintains its own state and responds to its own keyboard shortcuts.

## Controls

- Top Counter: Press `+` to increment, `-` to decrement
- Bottom Counter: Press `+` to increment, `-` to decrement  
- Press `r` to reset both counters
- Press `q` to quit

## Running

```bash
dune exec mosaic/examples/component_counter/main.exe
```
