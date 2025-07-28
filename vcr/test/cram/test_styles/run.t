Test text styles and attributes

  $ vcr --output=test_styles.gif test_styles.tape 2>&1
  Tape finished.
  Saving recording...
  Saving GIF to test_styles.gif

Verify the GIF was created
  $ test -f test_styles.gif && echo "GIF created"
  GIF created

Check GIF is valid
  $ head -c 6 test_styles.gif
  GIF89a

Verify reasonable file size
  $ test $(wc -c < test_styles.gif) -gt 1000 && echo "GIF has content"
  GIF has content
