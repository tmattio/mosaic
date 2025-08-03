Test text styles and attributes

  $ vcr --output=test_styles.gif test_styles.tape 2>&1
  File: test_styles.tape
  Output test_styles.gif
  Set Shell bash
  Set Width 600
  Set Height 300
  Type clear
  Enter 1
  Sleep 200ms
  Type echo 'Text Style Tests:'
  Enter 1
  Sleep 500ms
  Type echo -e '\\033[1mBold text\\033[0m'
  Enter 1
  Sleep 300ms
  Type echo -e '\\033[3mItalic text\\033[0m'
  Enter 1
  Sleep 300ms
  Type echo -e '\\033[4mUnderlined text\\033[0m'
  Enter 1
  Sleep 300ms
  Type echo -e '\\033[9mStrikethrough text\\033[0m'
  Enter 1
  Sleep 300ms
  Type echo -e '\\033[7mReversed text\\033[0m'
  Enter 1
  Sleep 300ms
  Type echo -e '\\033[1;4;31mBold underlined red\\033[0m'
  Enter 1
  Sleep 300ms
  Type echo -e '\\033[5mBlinking text\\033[0m (if supported)'
  Enter 1
  Sleep 1000ms
  Creating test_styles.gif...
  [TIMING] Parse tape: * (glob)
  [TIMING] Total Vcr.run: * (glob)
  [TIMING] Total execution: * (glob)

Verify the GIF was created
  $ test -f test_styles.gif && echo "GIF created"
  GIF created

Check GIF is valid
  $ head -c 6 test_styles.gif
  GIF89a

Verify reasonable file size
  $ test $(wc -c < test_styles.gif) -gt 1000 && echo "GIF has content"
  GIF has content
