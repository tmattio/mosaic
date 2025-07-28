Test GIF format compliance

  $ vcr --output=output.gif simple.tape 2>&1
  Tape finished.
  Saving recording...
  Saving GIF to output.gif

Verify GIF header
  $ head -c 6 output.gif | od -c
  0000000    G   I   F   8   9   a                                        
  0000006

Check logical screen descriptor
  $ od -An -t x1 -j 6 -N 7 output.gif | tr -d ' \n' | cut -c1-14
  e001c000910000

Extract and validate dimensions (default 80x24 terminal with 6x8 char size = 480x192 pixels)
  $ od -An -t u2 -j 6 -N 4 output.gif
                480     192                                                
  

Verify trailer byte at end
  $ tail -c 1 output.gif | od -An -t x1
             3b                                                            
  
