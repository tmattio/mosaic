Test animation and frame timing

  $ vcr --output=test_animation.gif test_animation.tape 2>&1
  Tape finished.
  Saving recording...
  Saving GIF to test_animation.gif

Verify the GIF was created
  $ test -f test_animation.gif && echo "GIF created"
  GIF created

Check that it's an animated GIF (multiple frames)
  $ strings test_animation.gif | grep -c "^!.*" | test $(cat) -gt 5 && echo "Multiple frames detected"
  Multiple frames detected

Check that the GIF has multiple image descriptors (frames)
  $ od -An -t x1 test_animation.gif | grep -c "2c" | test $(cat) -gt 3 && echo "Multiple frames found"
  Multiple frames found
