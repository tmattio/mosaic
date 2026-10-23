# xterm_diff: matrix's VTE against xterm

A differential fuzzer. It writes the same random bytes to matrix's VTE and to a
real xterm, and compares the screens (text, attributes, colours), the cursor
and the replies. Where matrix follows a rule RFC 0004 decides, or xterm
departs from the reference matrix follows, a named normalisation removes the
expected difference (see [Normalisations](#normalisations)); every difference
left is a finding.

It is not a test: it needs xterm and an X server, which CI lacks, so nothing
runs it under `dune build @runtest`.

## Running it

You need xterm (tested with xterm 398) and an X server. Xvfb is enough:

```sh
Xvfb :97 -screen 0 1024x768x24 -nolisten tcp &
export DISPLAY=:97
dune build ./matrix/test/xterm_diff/xterm_diff.exe
X=_build/default/matrix/test/xterm_diff/xterm_diff.exe

$X fuzz --seed 1 --count 5000 --out /tmp/xd      # a campaign
$X case 'a\e[2@b' --out /tmp/xd                   # one case, both screens
$X case @cases.txt --out /tmp/xd                  # one case per line
$X matrix 'a\e[2@b'                               # matrix only, no X needed
$X gen --seed 1 --start 1129 --count 1            # print a campaign's case
$X list                                           # the normalisations
```

If xterm is not on `PATH`, pass `--xterm PATH` or set `XTERM_DIFF_XTERM`. For
an unpacked xterm, also set `LD_LIBRARY_PATH` to its libraries.

Bytes on the command line are escaped: `\e`, `\xHH`, `\u{HHHH}`, `\n`, `\r`,
`\t`, `\\`.

Options: `--rows`/`--cols` (default 6x12: small screens make wrapping,
scrolling and margins frequent; 1x1 and 2x2 work too), `--seed`, `--count`,
`--start`, `--decid` (xterm's `decTerminalID`, default 100; 420 makes xterm a
VT420, with DECIC/DECDC, single shifts and national mode), `--max-tokens`
(the longest case, default 40; up to 200 makes long cases),
`--strict N1,N2` (turn these normalisations off, `all` for every one),
`--exclude L1,L2` (never generate tokens whose label starts with one of
these), `--budget` (runs per minimization), `--no-minimize`.

## Output

In the `--out` directory:

- `summary.txt`: the count of unexplained differences, the number of cases in
  which each normalisation fired, and the differing cases grouped by kinds of
  difference and the labels of their minimized tokens, largest group first.
  A clean campaign says `0 unexplained differences`.
- `results.tsv`: one line per case: index, `same`/`diff`/`timeout`, the
  normalisations that fired, and for a difference its kinds, minimized labels
  and minimized bytes.
- `widths.txt`: xterm's width of each non-ASCII character of the alphabet, as
  measured (see below).
- `diffs/NNNNNN.txt`: for each differing case, the original and the minimized
  case, the differences and both screens.
- `log.txt`: progress, xterm's measured widths, timeouts.

Kinds: `text` (a cell's grapheme), `attr` (attributes or colours), `cursor`
(the CPR after the case, and after one more character, which tells a pending
wrap apart), `reply` (the case's replies: DA, DSR, CPR...), `line` (line
attributes, DECDWL/DECDHL), `crash` (matrix raised or took over 5 s).

## How it works

`xterm_diff fuzz` starts one xterm whose program (`-e`) is this executable in
agent mode, so that the agent's standard input and output are xterm's pty. The
agent puts the tty in raw mode and, for each case, writes in one go:

1. `ESC c` (RIS): xterm's power-up state; the matrix side is a fresh VTE.
   xterm's RIS keeps some state, which would leak from one case into the
   next, so the agent also, on the xterm side only:
   - turns national mode off first (`CSI ? 42 l`): RIS clears the mode
     without turning UTF-8 back on;
   - clears the alternate screen and saves the home cursor there
     (`CSI ? 47 h`, `CSI 2 J`, `ESC 7`, `CSI ? 47 l`): RIS leaves the
     alternate screen's saved cursor, and once in 24,000 cases its text;
   - designates ASCII into G0 to G3: from level 2, RIS leaves G2 as a case
     designated it, and with G2 not ASCII xterm reads every byte 0x80..0xFF
     raw, so all later UTF-8 would break;
   - from level 3, sets ANSI conformance level 3 (`ESC SP N`): RIS keeps the
     level a case set, and level 1 turns LS2 and LS3 off.
2. The case, then `ESC \` (ST), which ends any sequence or string the case left
   open. Both sides receive it.
3. `CSI 6 n`: the cursor.
4. `CSI i` (media copy): xterm prints the screen to its printer, a FIFO the
   agent reads.
5. `#` and `CSI 6 n`: where the next character lands, which shows a pending
   wrap.
6. `CSI > 0 q` (XTVERSION), whose reply marks the end of the case's replies.
   The generator never produces it.

Before writing, the agent rewrites the case for xterm where a normalisation
asks (`norm.ml`). The agent decodes xterm's print (see `screen.ml` for the
format and its traps), runs matrix on the original bytes at the same size
through `vte_adapter.ml` (the only module that uses matrix's API), applies
the screen and reply normalisations, and compares. Differing cases are
minimized by delta debugging over their tokens, then by replacing each token
with a simpler variant; a smaller case must still be one the generator could
make.

The generator (`gen.ml`) is weighted and seeded. Case `i` of seed `s` depends
on `(s, i)` only. Cases are 1 to 40 tokens drawn from printable ASCII,
Latin-1, wide CJK, combining and spacing marks, emoji with VS15/VS16, skin
tones (on U+261D too) and ZWJ, Prepend, regional indicators, Hangul jamo, a
Devanagari conjunct; the C0 controls; every control function matrix
implements, with parameters at the
edges (absent, 0, 1, the screen's size and its neighbours, 2^16, 2^31, 2^32,
20 digits); charsets and shifts; strings (OSC, DCS, APC, PM, SOS) with each
terminator; C1 controls in UTF-8 and raw; ill-formed UTF-8; and malformed
syntax (C0, ESC, CAN or non-ASCII inside a CSI, 300 parameters, colons,
private markers and intermediates on other finals).

Speed: about 2,000 comparisons per second (12x6, xterm 398 on Xvfb),
minimization included; several campaigns can run at once on one display.

## xterm's configuration

`xterm_diff.ml` passes every resource below with `-xrm`, plus `-u8 +sb -ut`.

| resource | value | why |
|---|---|---|
| `decTerminalID` | 100 | DA1 `CSI ? 1 ; 2 c`, matrix's identity. xterm then acts as a VT100 (see below). |
| `utf8` | true | UTF-8 in and out. |
| `utf8Weblike` | true | Overlong forms, surrogates and code points past U+10FFFF end at their first byte, as Unicode §3.9's maximal subparts do in matrix; by default xterm makes one U+FFFD of the whole sequence. It does not make xterm follow §3.9: a continuation byte that begins no character, followed by a byte 0x21..0x7F, is still taken raw, as a Latin-1 character or a C1 control (ptydata.c, decodeUtf8). So `\xc0\x80a` is one U+FFFD in xterm (the 0x80 is a C1 control there) and two in matrix, and `\xed\xa0\x80a` two in xterm and three in matrix: normalisation `lone-continuation`. |
| `precompose` | false | xterm composes combining sequences to NFC by default; matrix never does. |
| `combiningChars` | 10 | Default 2; xterm clamps it to 5 marks per cell (matrix keeps any number; the generator writes at most 3). |
| `mkWidth` / `cjkWidth` | true / false | xterm's built-in width table, independent of the host's C library. |
| `autoWrap` / `reverseWrap` / `titeInhibit` | true / false / false | Matrix's power-up modes; the alternate screen allowed. |
| `boldColors`, `colorBDMode`, `colorULMode`, `colorBLMode`, `colorRVMode`, `colorITMode`, `colorAttrMode` | false | Attributes stay attributes; bold does not brighten colours. |
| `colorMode` / `directColor` | true / true | Indexed and direct colours are stored and printed. |
| `scrollBar` / `logging` / `loginShell` / `utmpInhibit` / `saveLines` | false / false / false / true / 0 | No scroll bar, no log, no login shell, no utmp entry, no scrollback. |
| `allowWindowOps` / `allowColumnMode` | false / false | The generator sends no window operation or DECCOLM; the geometry never changes. |
| `allowC1Printable` | false | Default. It changes nothing in UTF-8 mode (measured). |
| `answerbackString` | "" | ENQ answers nothing, as in matrix. |
| `printerCommand` | `cat > FIFO` | The print-screen goes to the agent. |
| `printerExtent` | true | `CSI i` prints the whole screen, not the scrolling region. |
| `printAttributes` | 2 | SGR attributes and colours in the print. |
| `printRawChars` | true | Cells as stored: U+FFFF marks the second half of a wide character, U+FFFD prints as itself (otherwise as `#`). |
| `printerFormFeed` / `printerNewLine` | true / true | A form feed ends each print (and flushes the pipe); every row ends with a newline. |
| `printerAutoClose` / `printModeImmediate` / `printOptsImmediate` | false / 0 / 0 | One printer process for the session. |
| `jumpScroll` | false | Scroll at once. With jump scroll xterm defers redrawing the rows a scroll brings in, and while that is pending EL 1 and DECSEL 1 skip the Last Column Flag and protection (util.c, `ClearLeft`): a difference that depends on timing. |

Tab stops are xterm's default, every 8 columns, as in matrix.

## Limits and traps

- **xterm as a VT100.** With `decTerminalID: 100`, xterm ignores what a VT100
  lacks: DECIC/DECDC (VT420), SS2/SS3, LS2/LS3, the national character sets,
  and it shows CAN and SUB as a checkerboard. Use `--decid 420` to compare
  those functions; normalisation `identity` then rewrites xterm's DA.
- **Erased cells.** xterm's print omits trailing cells no character was drawn
  in, whatever their colour, and matrix cannot tell a drawn space from an
  erased one; so trailing spaces are dropped on both sides, and a space's
  foreground is compared only when inverse or a line attribute shows it.
- **Combining marks.** xterm prints a cell's combining characters after its
  base, so the decoder must know xterm's widths: the agent measures each
  non-ASCII character of the generator's alphabet once per session (in
  `log.txt`).
- **Printer controller mode.** xterm reads a non-ASCII character inside a CSI
  as its low 7 bits (U+00E9 is `i`), so `CSI 5 é` would turn printer
  controller mode on and swallow the probes. The generator avoids U+00E9 there;
  after any timeout the agent sends `CSI 4 i`, resets, and resynchronises. If
  xterm stays wedged, a new xterm resumes after that case.
- **xterm's printer process.** A print with no printer open forks a printer
  process and closes it after, and xterm's SIGCHLD handler then can block in
  `wait()` on the agent. The agent opens the printer once at start (printer
  controller mode around a form feed) so that it stays open.
- **xterm removes `XTERM_*` variables** from its child's environment; the
  agent's I/O trace is `XDIFF_TRACE=FILE`.
- **xterm crashes.** xterm 398 dies of SIGSEGV on a few inputs (6 cases in
  24,000; whether it does depends on its memory layout, so a case that crashes
  in a campaign may not crash alone; about 1 in 5,000). The parent then starts
  a new xterm after that case; its log says `killed by signal -10`.
- **Rows a scroll brings in.** xterm stores their cells with no character, and
  its print skips the SGR of such cells, so their colour (BCE) is not
  compared. In general a space with no SGR before it in the print is compared
  by its text only.
- **Orphan halves.** xterm can keep the U+FFFF of a wide character whose
  first half was overwritten (after RI or DL); it draws nothing there and the
  decoder reads it as a space.
- **Widths.** The agent measures xterm's width of each non-ASCII character of
  the generator's alphabet once per output directory and keeps them in
  `widths.txt`; it writes an `A` and the character and reads where the cursor
  went, so below 4 columns the parent first runs a measuring xterm of 12x6.
  Other characters take matrix's wcwidth.

## Normalisations

`xterm_diff list` prints them all, with their reasons and citations; the
summary of a campaign counts the cases in which each fired. There are four
ways, each chosen to hide as little as possible:

- a **generator restriction** never produces the input (`vt-level`,
  `national-designation`...);
- an **xterm rewrite** gives xterm an input it treats as matrix treats the
  original, while matrix gets the original (`wrap-erase` puts EL 0 before a
  wide character that will wrap, where matrix erases); the rewrites follow
  matrix's state with a VTE of their own, fed the same bytes, as many depend
  on where the cursor is;
- a **matrix rerun**: `graphemes` compares xterm with matrix under the
  `` `Wcwidth `` method when the two methods give matrix different results,
  since xterm places characters as wcwidth does;
- a **screen or reply normalisation** (`identity` rewrites xterm's DA2).

Some screen normalisations follow cells as they move: a second VTE gets the
same bytes and a private-use tag in the cells concerned (a blank a selective
erase made, the half a cut left, a cell xterm loses), and the tags still
there at the end are the cells normalised.

Each one is a deviation that RFC 0004 decides, an xterm quirk cited to its
source or changelog, or a configuration difference (xterm's level, its
print-screen). `--strict NAME` turns one off; `--strict all` shows the raw
differences.

## input_diff: the runtime's input against xterm

`input_diff` runs a matrix application inside a real xterm, so that the
runtime sends its real startup probe and decodes real keys. It needs the same
xterm and X server; keys also need python3 and libXtst (`keys.py` drives
xterm through XTest with ctypes, so no xdotool is needed):

```sh
dune build ./matrix/test/xterm_diff/input_diff.exe
I=_build/default/matrix/test/xterm_diff/input_diff.exe
D=matrix/test/xterm_diff

$I caps --decid 100 --out /tmp/id     # the probe's replies, the capabilities
$I caps --decid 420 --out /tmp/id
$I keys --decid 420 --out /tmp/id --driver $D/keys.py --chords $D/chords.txt \
  --xtst /usr/lib/x86_64-linux-gnu/libXtst.so.6
```

`caps` starts xterm twice on an agent: once to write the runtime's probe and
record the raw replies (`raw-N.txt`), once to create an application and write
the capabilities the runtime derived (`caps-N.txt`), which it checks against
xterm 398's. xterm's environment is reduced to a terminal started fresh: no
multiplexer, ssh or terminal-program variables.

`keys` starts an application in xterm that records every event it delivers,
then presses each chord of `chords.txt` (X keysym names joined by `+`) at
the window and checks the keystroke the runtime decoded for each; the events
between a chord and the next are its. The second time a chord is pressed,
Caps Lock is on. The runtime asks xterm for its modes, finds no kitty keyboard
protocol, and sets modifyOtherKeys 1 and, since xterm answers that it is
reset, metaSendsEscape: the expected keystrokes are those of xterm's bytes in
that configuration.

Both exit 0 when nothing differs.
