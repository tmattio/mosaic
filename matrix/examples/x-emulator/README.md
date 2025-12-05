# matrix-term

A (toy) terminal emulator built with OCaml and SDL2, using [Matrix](../matrix/).

## Features

- Pure OCaml implementation with minimal dependencies
- Complete VTE (Virtual Terminal Emulator) implementation
- 256-color and true color support
- Mouse selection and clipboard integration
- Window resizing with content preservation
- Unicode support with fallback rendering
- Texture caching for efficient glyph rendering

## Architecture

matrix-term is built on top of the [Matrix](../matrix/) library, which provides:

- **VTE**: Virtual Terminal Emulator with ANSI escape sequence parsing
- **PTY**: Pseudo-terminal handling for process communication  
- **Grid**: Efficient grid management with damage tracking
- **ANSI**: Complete ANSI color and escape sequence support

The terminal uses SDL2 for cross-platform window management and rendering, with JetBrains Mono as the default font.

## License

ISC License. See [LICENSE](../LICENSE) for details.
