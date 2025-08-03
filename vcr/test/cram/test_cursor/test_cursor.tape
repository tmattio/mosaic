# Test cursor visibility
Set Shell "bash"
Set Width 480
Set Height 240
Set TypingSpeed 0.1

Type "echo 'Testing cursor visibility...'"
Enter
Sleep 0.5

Type "echo -n 'Typing with cursor: '"
Sleep 0.5
Type "H"
Sleep 0.2
Type "e"
Sleep 0.2
Type "l"
Sleep 0.2
Type "l"
Sleep 0.2
Type "o"
Sleep 0.5

Hide
Type " (cursor hidden)"
Sleep 1

Show
Enter
Type "echo 'Cursor is back!'"
Enter
Sleep 0.5