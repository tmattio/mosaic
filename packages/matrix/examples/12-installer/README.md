# `12-installer`

A package-manager simulation showcasing `static_print` for persistent output.
As each package finishes installing, a completion message is printed above the
live progress UI so it remains visible after the app exits.

```bash
dune exec ./matrix/examples/12-installer/main.exe
```

## Controls

- `Q` or `Esc` &mdash; quit early.

## Highlights

- Demonstrates `Matrix.static_print` for appending lines outside the animated
  region.
- Per-package download and install progress bars.
- Automatically exits once all packages are installed.
