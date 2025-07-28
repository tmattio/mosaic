# Test frame timing and animation
Set Shell "bash"
Set Width 40
Set Height 10
Set TypingSpeed 0.01

Type "clear"
Enter
Sleep 0.2

Type "echo 'Animation Test'"
Enter
Sleep 0.5

Type "for i in {1..10}; do"
Enter
Type "  printf '\\r[%02d/10] ' $i"
Type "  for j in {1..$i}; do printf '='; done"
Type "  sleep 0.2"
Enter
Type "done"
Enter
Sleep 3

Type "echo"
Enter
Type "echo 'Done!'"
Enter
Sleep 0.5