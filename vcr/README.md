# VCR

Terminal recorder that generates GIF/SVG/ASCII from [VHS](https://github.com/charmbracelet/vhs) tape files.

## Installation

```bash
dune build
dune install
```

## Usage

```bash
# Run a tape file
vcr demo.tape

# Specify output format
vcr demo.tape -o output.gif
vcr demo.tape -o output.svg
vcr demo.tape -o output.txt

# Read from stdin
echo "Type 'Hello'" | vcr -

# No output (useful for screenshots)
vcr demo.tape
```

## Tape Format

VCR uses the VHS tape format:

```tape
# Set terminal size
Set Width 800
Set Height 600

# Type text with optional speed
Type "Hello, World!"
Type@100ms " Slow typing"

# Press keys
Enter
Backspace 3
Tab

# Control sequences
Ctrl+C
Alt+Tab
Shift+A

# Wait and sleep
Sleep 500ms
Wait               # Wait for prompt
Wait@5s /regex/    # Wait for pattern with timeout

# Take screenshots
Screenshot output.txt

# Output file
Output demo.gif
```

## Examples

```bash
# Create a simple demo
cat > demo.tape << EOF
Output demo.gif
Type "echo 'Hello, VCR!'"
Enter
Sleep 1s
EOF

vcr demo.tape
```

## Comparison with VHS

VCR is a lightweight alternative to [VHS](https://github.com/charmbracelet/vhs) with a different architecture:

### VHS
- **Dependencies**: Requires Chrome, ttyd, and ffmpeg
- **Architecture**: Uses browser automation (xterm.js in headless Chrome)
- **Output**: All formats generated via ffmpeg
- **Platform**: Cross-platform (Windows, macOS, Linux)
- **Features**: Rich styling options (window bars, borders, shadows)

### VCR
- **Dependencies**: None (pure OCaml, includes PTY and terminal emulator)
- **Architecture**: Native terminal emulation without browser
- **Output**: Native renderers for each format (no ffmpeg needed)
- **Platform**: Unix-like systems (Linux, macOS)
- **Features**: Fast, lightweight, modular renderer system

VCR is ideal when you want a minimal, fast solution without system dependencies.
