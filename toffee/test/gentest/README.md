# Toffee Test Generation

This directory contains the test generation infrastructure for Toffee, similar to Taffy's approach.

## How it works

1. **HTML Test Fixtures**: Tests are written as HTML files in `test_fixtures/` with CSS styles
2. **Browser Rendering**: Playwright renders the HTML in a real browser
3. **Layout Extraction**: JavaScript extracts computed layout information
4. **OCaml Generation**: The script generates OCaml test code in `tests/generated/`

## Usage

From the Toffee root directory:

```bash
# Install dependencies (first time only)
make install-gentest

# Generate tests
make gentest

# Run generated tests
make test
```

## Adding new tests

1. Create an HTML file in the appropriate `test_fixtures/` subdirectory:
   - `grid/` for CSS Grid tests
   - `flex/` for Flexbox tests
   - `block/` for Block layout tests

2. Use inline styles on a root element with `id="test-root"`

3. Run `make gentest` to regenerate the OCaml tests

## Test Helper

The `test_helper.js` file contains functions to extract style and layout information from the DOM. It parses CSS properties into a format that can be converted to OCaml types.

## Generated Tests

Tests are generated in `tests/generated/` as OCaml modules. Each test:
1. Creates nodes with the extracted styles
2. Builds the tree structure
3. Computes the layout
4. Verifies the computed positions and sizes match the browser's layout