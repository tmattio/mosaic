Test cursor visibility handling

  $ vcr --output=test_cursor.gif test_cursor.tape 2>&1
  Tape finished.
  Saving recording...
  Saving GIF to test_cursor.gif

Verify the GIF was created
  $ test -f test_cursor.gif && echo "GIF created"
  GIF created

Check GIF has multiple frames (cursor show/hide states)
  $ od -An -t x1 test_cursor.gif | grep -c " 2c " | test $(cat) -ge 3 && echo "Multiple frames for cursor states"
  Multiple frames for cursor states
