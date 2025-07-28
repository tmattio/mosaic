Test ANSI color rendering with GIF output

  $ vcr --output=test_colors.gif test_colors.tape 2>&1
  Tape finished.
  Saving recording...
  Saving GIF to test_colors.gif

Verify the GIF was created
  $ test -f test_colors.gif && echo "GIF created"
  GIF created

Check GIF header
  $ head -c 6 test_colors.gif
  GIF89a

Verify file size is reasonable (should have color data)
  $ test $(wc -c < test_colors.gif) -gt 1000 && echo "GIF has content"
  GIF has content
