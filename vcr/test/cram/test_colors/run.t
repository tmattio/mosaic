Test ANSI color rendering with GIF output

  $ vcr --output=test_colors.gif test_colors.tape 2>&1
  File: test_colors.tape
  Output test_colors.gif
  Set Shell bash
  Set Width 480
  Set Height 240
  Type echo 'Testing ANSI Colors:'
  Enter 1
  Sleep 500ms
  Type echo -e '\\033[31mRed\\033[0m \\033[32mGreen\\033[0m \\033[34mBlue\\033[0m'
  Enter 1
  Sleep 500ms
  Type echo -e '\\033[33mYellow\\033[0m \\033[35mMagenta\\033[0m \\033[36mCyan\\033[0m'
  Enter 1
  Sleep 500ms
  Type echo -e '\\033[1;31mBright Red\\033[0m \\033[1;32mBright Green\\033[0m'
  Enter 1
  Sleep 500ms
  Type echo -e '\\033[40;37mWhite on Black\\033[0m \\033[47;30mBlack on White\\033[0m'
  Enter 1
  Sleep 1000ms
  Creating test_colors.gif...
  [TIMING] Parse tape: * (glob)
  [TIMING] Total Vcr.run: * (glob)
  [TIMING] Total execution: * (glob)

Verify the GIF was created
  $ test -f test_colors.gif && echo "GIF created"
  GIF created

Check GIF header
  $ head -c 6 test_colors.gif
  GIF89a

Verify file size is reasonable (should have color data)
  $ test $(wc -c < test_colors.gif) -gt 1000 && echo "GIF has content"
  GIF has content
