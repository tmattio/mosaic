Test 256-color palette support

  $ vcr --output=test_256colors.gif test_256colors.tape 2>&1
  Tape finished.
  Saving recording...
  Saving GIF to test_256colors.gif

Verify the GIF was created
  $ test -f test_256colors.gif && echo "GIF created"
  GIF created

Check that the GIF is large enough to contain color data
  $ test $(wc -c < test_256colors.gif) -gt 5000 && echo "GIF has substantial color data"
  GIF has substantial color data

Verify it has a global color table
  $ od -t x1 -N 13 test_256colors.gif | grep -q "91" && echo "Global color table present"
  Global color table present
