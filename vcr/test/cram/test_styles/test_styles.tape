# Test text styles and attributes
Set Shell "bash"
Set Width 600
Set Height 300
Set TypingSpeed 0.05

Type "clear"
Enter
Sleep 0.2

Type "echo 'Text Style Tests:'"
Enter
Sleep 0.5

Type "echo -e '\\033[1mBold text\\033[0m'"
Enter
Sleep 0.3

Type "echo -e '\\033[3mItalic text\\033[0m'"
Enter
Sleep 0.3

Type "echo -e '\\033[4mUnderlined text\\033[0m'"
Enter
Sleep 0.3

Type "echo -e '\\033[9mStrikethrough text\\033[0m'"
Enter
Sleep 0.3

Type "echo -e '\\033[7mReversed text\\033[0m'"
Enter
Sleep 0.3

Type "echo -e '\\033[1;4;31mBold underlined red\\033[0m'"
Enter
Sleep 0.3

Type "echo -e '\\033[5mBlinking text\\033[0m (if supported)'"
Enter
Sleep 1