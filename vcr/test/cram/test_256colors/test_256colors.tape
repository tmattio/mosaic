# Test 256-color support
Set Shell "bash"
Set Width 800
Set Height 400
Set TypingSpeed 0.01

Type "clear"
Enter
Sleep 0.2

Type "echo '256 Color Palette Test:'"
Enter
Sleep 0.5

# Standard 16 colors
Type "echo -e 'Standard colors:'"
Enter
Type "for i in {0..15}; do"
Enter
Type "  printf '\\033[48;5;%sm  ' $i"
Enter
Type "done"
Enter
Type "echo -e '\\033[0m'"
Enter
Sleep 0.5

# Color cube
Type "echo -e '\\nColor cube (6x6x6):'"
Enter
Type "for r in {0..5}; do"
Enter
Type "  for g in {0..5}; do"
Enter
Type "    for b in {0..5}; do"
Enter
Type "      color=$((16 + r*36 + g*6 + b))"
Enter
Type "      printf '\\033[48;5;%sm  ' $color"
Enter
Type "    done"
Enter
Type "    echo -e '\\033[0m'"
Enter
Type "  done"
Enter
Type "done"
Enter
Sleep 1

# Grayscale
Type "echo -e '\\nGrayscale:'"
Enter
Type "for i in {232..255}; do"
Enter
Type "  printf '\\033[48;5;%sm  ' $i"
Enter
Type "done"
Enter
Type "echo -e '\\033[0m'"
Enter
Sleep 1