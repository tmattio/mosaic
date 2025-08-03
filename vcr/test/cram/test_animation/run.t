Test animation and frame timing

  $ vcr --output=test_animation.gif test_animation.tape 2>&1
  File: test_animation.tape
  Output test_animation.gif
  Set Shell bash
  Set Width 480
  Set Height 240
  Type clear
  Enter 1
  Sleep 200ms
  Type echo 'Animation Test'
  Enter 1
  Sleep 500ms
  Type for i in {1..10}; do
  Enter 1
  Type   printf '\\r[%02d/10] ' $i
  Type   for j in {1..$i}; do printf '='; done
  Type   sleep 0.2
  Enter 1
  Type done
  Enter 1
  Sleep 3000ms
  Type echo
  Enter 1
  Type echo 'Done!'
  Enter 1
  Sleep 500ms
  Creating test_animation.gif...
  [TIMING] Parse tape: * (glob)
  [TIMING] Total Vcr.run: * (glob)
  [TIMING] Total execution: * (glob)

Verify the GIF was created
  $ test -f test_animation.gif && echo "GIF created"
  GIF created

Check that it's an animated GIF (multiple frames)
  $ strings test_animation.gif | grep -c "^!.*" | test $(cat) -gt 5 && echo "Multiple frames detected"
  Multiple frames detected

Check that the GIF has multiple image descriptors (frames)
  $ od -An -t x1 test_animation.gif | grep -c "2c" | test $(cat) -gt 3 && echo "Multiple frames found"
  Multiple frames found
