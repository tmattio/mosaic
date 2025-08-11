# Changelog

All notable changes to this project will be documented in this file.

## [1.0.0~alpha0] - 2025-08-10

### Initial Release

We're thrilled to announce the first release of Mosaic, a high-performance terminal UI framework for OCaml that brings React's component model to the terminal.

### Added

#### Core Libraries

- **Mosaic** - React-inspired terminal UI framework
  - Direct-style hooks API powered by OCaml 5 algebraic effects
  - Comprehensive hooks: `use_state`, `use_effect`, `use_memo`, `use_context`, `use_ref`
  - Animation hooks: `use_tick`, `use_timer`, `use_scroll`.
  - Rich component library: buttons, inputs, tables, trees, progress bars, spinners
  - Event routing with focus management and tab navigation
  - Alternative Elm Architecture module for functional reactive programming

- **Matrix** - High-performance terminal infrastructure
  - Efficient grid representation with Unicode and grapheme cluster support
  - Damage tracking for minimal screen updates
  - Double-buffered rendering for flicker-free display
  - Incremental input parser supporting Kitty keyboard protocol
  - Complete ANSI escape sequence generation (ECMA-48/ANSI X3.64)
  - Terminal control (TTY), pseudo-terminal (PTY), and virtual terminal emulator (VTE)
  - Mouse support: X10, SGR, URXVT protocols with motion tracking

- **Toffee** - Pure OCaml port of the Taffy CSS layout engine
  - Full CSS Grid Level 1 implementation
  - Complete Flexbox algorithm with all properties
  - Traditional block layout with margin collapsing
  - High-performance layout computation

[1.0.0~alpha0]: https://github.com/tmattio/mosaic/releases/tag/v1.0.0~alpha0
