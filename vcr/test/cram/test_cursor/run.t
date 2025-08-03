Test cursor visibility handling

  $ vcr --output=test_cursor.gif test_cursor.tape 2>&1
  File: test_cursor.tape
  Output test_cursor.gif
  Set Shell bash
  Set Width 480
  Set Height 240
  Type echo 'Testing cursor visibility...'
  Enter 1
  Sleep 500ms
  Type echo -n 'Typing with cursor: '
  Sleep 500ms
  Type H
  Sleep 200ms
  Type e
  Sleep 200ms
  Type l
  Sleep 200ms
  Type l
  Sleep 200ms
  Type o
  Sleep 500ms
  Type  (cursor hidden)
  Sleep 1000ms
  Enter 1
  Type echo 'Cursor is back!'
  Enter 1
  Sleep 500ms
  Creating test_cursor.gif...
  [TIMING] Parse tape: * (glob)
  [TIMING] Total Vcr.run: * (glob)
  [TIMING] Total execution: * (glob)

Verify the GIF was created
  $ test -f test_cursor.gif && echo "GIF created"
  GIF created

Check GIF has multiple frames (cursor show/hide states)
  $ od -An -t x1 test_cursor.gif | grep -c " 2c " | test $(cat) -ge 3 && echo "Multiple frames for cursor states"
  Multiple frames for cursor states
