# Test ANSI color rendering
Set Shell "bash"
Set Width 480
Set Height 240
Set TypingSpeed 0.05

Type "echo 'Testing ANSI Colors:'"
Enter
Sleep 0.5

Type "echo -e '\\033[31mRed\\033[0m \\033[32mGreen\\033[0m \\033[34mBlue\\033[0m'"
Enter
Sleep 0.5

Type "echo -e '\\033[33mYellow\\033[0m \\033[35mMagenta\\033[0m \\033[36mCyan\\033[0m'"
Enter
Sleep 0.5

Type "echo -e '\\033[1;31mBright Red\\033[0m \\033[1;32mBright Green\\033[0m'"
Enter
Sleep 0.5

Type "echo -e '\\033[40;37mWhite on Black\\033[0m \\033[47;30mBlack on White\\033[0m'"
Enter
Sleep 1