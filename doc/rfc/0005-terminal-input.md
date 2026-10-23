# RFC 0005: Terminal input — decoding what the terminal sends, and learning what it can do

- Status: Decided 2026-10-23 and implemented on `next`: phase 1 `ec529e49`–`66b4e45f` (with
  `d4fdec96`), phase 2 `ba3626ff`–`7b5ec572`, and the final review's fixes `49a8cce0`–`9d897c96`.
  This text states what was built, including the decisions taken during implementation (§2.1).
- Baseline: main @ `739cf2a1`, where `Input.Parser` decoded input with its own CSI reader and a
  clock, and `Terminal.Caps` folded capabilities from replies and the environment.
- Process: the "Becoming certain" campaign of raven's `doc/dev/design.md`, applied to matrix, for
  two areas (C1: decoding; C2: capabilities). Two blind designs, one from the specifications'
  model (d1) and one from callers' needs (d2); an audit comparing them and today's code on equal
  footing, with six prototypes (P1–P6); a skeptic re-reading every citation, re-running every
  measurement, and measuring a real xterm 398 (its replies, and the bytes of 31 chords in five
  configurations through XTest) and tmux 3.5a (detached and inside xterm, passthrough off and
  on); an adjudication. Then two implementation phases (the decoder beside the parser; the
  runtime switched over and the parser removed), and a final review with its own fuzzers and
  probes, whose findings were fixed (§2.1).
- Compatibility: none before 1.0. Breaks are listed in §4 and stated in their commits.
- Amended 2026-10-23 (the width method; settles §6 questions 8 and 9's first half): a DECRQM
  2027 answer of 0 (not recognized), or none, means `Wcwidth` unless the terminal's identity says
  it places clusters (kitty, which answers 0); 3 means `Unicode` and 4 `Wcwidth`, as DECRPM
  states them (VT510) and as mode 2027's specification has a terminal with the mode off behave,
  "as if this specification was not implemented at all". The default width method is `Wcwidth`.
  Measured, by a cursor report after each of 50 clusters written in the terminal as it starts:
  xterm 398 and VTE 0.80.1 answer 0 and st 0.9.2 nothing, and all three place every cluster as
  `Wcwidth` does (50/50, 50/50, 36/36; `Unicode` 26/50), where matrix chose `Unicode`; kitty
  0.49.2 answers 0 and places them as `Unicode` does on 46/50 (`Wcwidth` 22/50); WezTerm
  20240203 answers 3 and foot 1.21.0 answers 1. Whether "reset" should make the runtime set the
  mode stays open: no measured terminal answered 2.

## Summary

Main's input code gave some facts two owners, or the wrong one:
- the ECMA-48 header syntax, restated in a second lexer capped at 8 parameters;
- time, kept by the parser and steered by a five-flag context the runtime pushed down, which
  deferred a reply prefix with no bound;
- whether bytes were a reply or typing, decided by a 4096-byte cap that typed the rest of a long
  reply, and by a 5 s idle cut that typed the rest of a paste;
- a keystroke's value, which differed by encoding (`A` in legacy, `a` with Shift in kitty);
- capabilities, folded monotonically from an environment read behind the caller's back, so that
  a guess beat the terminal's answer.

The redesign gives each fact one owner:
- **A framer in front of `Ansi.Parser`.** Input owns only what output never has; header and
  string syntax stay ansi's.
- **The decoder has no clock.** It says what silence would decide; the runtime owns time and
  what is owed.
- **What is owed tells replies from keys.** A control string begins only while one is owed, and
  a reply is typed only if it comes after its batch was given up.
- **One keystroke, one value**: the character typed, with Shift consumed.
- **Capabilities are derived** by one precedence from the environment, read once, and the
  answers so far.

## 1. Area C1: decoding (`matrix/lib/input`)

| decision | rule | references | confidence |
|---|---|---|---|
| Tokenizing | a framer in front of `Ansi.Parser`. The framer owns only an ESC before a key (Alt), X10's raw mouse bytes, rxvt's `CSI n $`, the Linux console's `CSI [ A`, SS3 with parameters, bytes that are not UTF-8, and the paste body; the parser reads every header and string. Ansi does not change. A C0, DEL, ESC or non-ASCII byte inside a sequence cuts it (`Reply.Cut`) and is the user's next key, a stated departure from ECMA-48 | RFC 0004 §1; ECMA-48 §5.4; P1 (the parser alone loses or misplaces bytes in 13 of 26 input forms); P2 (49,226 split checks, 0 differences) | high |
| ESC | ESC before a character, a control or DEL is Alt and that key. `ESC [` before 0x20–0x7E, and `ESC O` before a digit, `;` or an SS3 final, begin a sequence; any other byte makes Alt and the introducer, then the byte. ESC ESC before a byte that begins nothing is Alt+Escape and that byte, whatever is owed | kitty §Disambiguate: "pressing the Esc key generates the byte 0x1b which also is used to indicate the start of an escape code" | high |
| Strings | `ESC P`, `ESC ]` and `ESC _` begin a control string only while a string reply is owed, and are Alt keys otherwise: every string matrix receives answers a query. A begun string ends at ST (or BEL), CAN, SUB, an ESC not beginning ST, or the end of input, never at silence. PM, SOS and other APCs are Alt keys | skeptic T1, T2, T6: under an eager commit, Alt+] then a digit swallowed typing until 5 s of silence | high |
| Time | the decoder holds no clock. `pending` is `Nothing`, `Ambiguous` (ESC, ESC ESC, ESC before an introducer) or `Sequence` (a sequence, X10 report or UTF-8 character in progress). The caller calls `idle` only after a wait that read nothing: held ESCs become keys, a sequence is `Cut`, a character `Invalid`. With no `idle` in between, read boundaries change nothing | no reference specifies a timeout; ncurses (datapoint): "to distinguish sequences produced by a function key from those typed by a user" | high |
| CPR or F3 | `CSI 1 ; m R` is a `Reply.Cursor` while a cursor report is owed, F3 otherwise | ctlseqs DSR: "it is possible for this sequence to be sent by a function key"; ECMA-48 §8.3.14; kitty dropped F3's `CSI R` ("conflicts with the Cursor Position Report"); measured xterm Shift+F3 = `CSI 1;2R` | high |
| Key value | `Char c` is the character typed, Shift consumed (Shift+a is `'A'`); Shift stays only where it changed nothing (Shift+Space). Under kitty, a key that types one character takes its associated text, locks applied, as the key; a chord keeps the shifted key. Without kitty flag 4 a chord carries only the unshifted key and Shift: a stated loss | kitty §Key codes ("the shifted key must be present only if shift is also present"); P5 (Alt+Shift: two values in 21/47 cases under d1, 0/47 under d2); skeptic S7 | medium-high |
| Locks, keypad | locks never change a chord's key; `caps_lock`/`num_lock` are state that matching ignores. Keypad keys are main keys with `keypad`: a fact an encoding cannot carry (legacy) is absent, never guessed | kitty: "the Lock modifiers are not reported for text producing keys"; measured kitty Ctrl+s with Caps Lock `CSI 115;69u`, xterm 0x13 | high / medium (keypad) |
| Other keys | releases are `Key_release {key; mods; base; keypad}`; `Text` is text reported without a key (kitty key code 0). A C0 is Ctrl and a key (0x0A is Ctrl+J); Tab, Enter, Escape and Backspace keep their names. Bit 8 is super; Alt is the ESC prefix; `meta` only from a reported Meta bit | kitty §Text as code points ("if the terminal emulator receives no key information, the key number 0 must be used"); kitty §Modifiers (super = 8) against ctlseqs's table ("9 \| Meta") | high / medium (bit 8) |
| Not UTF-8 | a byte that cannot begin or continue a character is `Invalid`, one maximal subpart at a time, never a Meta key | Unicode C10: "shall not interpret such sequences as characters"; measured: default xterm sends Alt as well-formed Latin-1 in UTF-8 | high |
| Mouse | each report read from its own bits (SGR motion's button too); X10, SGR and urxvt forms; one notch per scroll; coordinates clamped to the first cell. 1005 and 1016 are not used | ctlseqs Mouse Tracking: "On button-motion events, xterm adds 32 to the event code"; skeptic: rejecting a 0-coordinate release sticks a drag | high |
| Paste | streamed (`Paste_start`, `Paste_data`, `Paste_end`), exactly as sent, nothing decoded inside. It ends at the first end marker, the end of input or `cut_paste`, never at silence. The decoder holds at most 5 bytes; matrix sets no budget | ctlseqs: "so that the program can differentiate pasted text from typed-in text"; RFC 0004 (strings streamed) | medium-high |
| Replies | typed: DA1/2/3, one `Mode` per DECRPM pair with five settings (3 counts as set), CPR/DECXCPR, XTWINOPS 4/6/8, 2048 (sub-parameters ignored), kitty flags, 997, XTVERSION, XTGETTCAP, OSC colors, OSC 52, kitty graphics. Anything else is `Csi`, `Ss3`, `Osc` or `Dcs`, never a key. Focus is `CSI I`/`CSI O` | VT510 DECRPM; ctlseqs (incl. FocusIn/FocusOut); 2048 gist ("it MUST ignore subparameters"); contour 996/997 | high |
| Totality, bounds | `feed` raises only on a bad range. Values saturate; scalars, kitty event types and modifiers outside their reference's range make a `Reply.Csi`; extra kitty fields are ignored. Held: ansi's 256 fields, 3 bytes of a character, 5 of a paste marker, and up to 16 MiB of a string's content (a memory budget), beyond which it is `Too_long` | kitty states no extension rule (skeptic §4); RFC 0004 | high; 16 MiB: medium |
| Cost | linear; 0.7–1.5× main's parser, allocating less on keys and mouse (§5) | P3; measured | high |

## 2. Area C2: capabilities and the runtime (`matrix/lib/terminal`, `matrix.ml`, mosaic)

| decision | rule | references | confidence |
|---|---|---|---|
| Owed counts | the decoder keeps two counts, cursor reports and string replies; only the caller changes them (`expect`, `settle`). Each query declares what it is owed | skeptic T7, T8 (an unbounded, unstated "owed") | medium-high |
| Batches | every batch that owes a reply ends with DA1, whose reply says it was answered or never will be. One such batch is in flight at a time; the others wait, oldest first. A cursor batch settles at its report | kitty §Detection: "If an answer for the device attributes is received without getting back an answer for the progressive enhancement the terminal does not support this protocol"; measured: tmux drops a wrapped DA1 | medium-high |
| Owing and waiting | the runtime waits for an answer at most max(500 ms, 4 RTT), then goes on; the batch stays owed until its answer, however late. A batch never answered is given up after max(5 s, 4 RTT), and a reply after that is input. Cursor reports owed by a given-up batch are counted and answered first | **No reference bounds it.** It must exceed a real answer's delay; meanwhile only Alt+], Alt+P, Alt+_ and a modified F3 can be misread; 5 s is ten times the answer wait and main's startup window | medium (rule) / low (5 s) |
| Waits | an ambiguous ESC waits max(50 ms, 2 RTT); a begun sequence max(500 ms, 4 RTT); strings and pastes never. RTT is the probe's round trip to its DA1, late or not; 0 until measured | **No reference fixes a value.** Datapoints: libtermkey and neovim 50 ms, vim 100 ms, readline 500 ms, ncurses 1 s, tmux 10 ms, extended while replies are outstanding. The RTT term follows from the question: has the terminal finished writing? | low (values) / medium (rule) |
| Probe | S7C1T; DECRQM of 1016, 2027, 2031, 1004, 2004, 2026 and 1036; two OSC 66 width tests on row 2, each with a CPR; XTVERSION; `CSI ? u`; kitty graphics unless a multiplexer is in the way; DA1 | measured: xterm answers with raw 0x9B after S8C1T, even in UTF-8 mode; `CSI 2 ; c R` is no key | high |
| Precedence | user policy (`MATRIX_FORCE_*`, `VHS_RECORD`) > corrections keyed on identity, each cited (Ghostty from 1.3) > the answer of the terminal doing the work > identity > environment guess > default. `Caps.derive` is pure and not monotone; a change applies the modes by their difference | **no reference**; argued from who knows each fact (audit §6.3) | medium |
| Environment | read once by the runtime (`Backend.t.env`). `TERM_PROGRAM` gives an identity only with none of `SSH_*`, `TMUX`, `STY`, `ZELLIJ` set | skeptic §6.2 (`TERM_PROGRAM` survives ssh, screen, zellij) | medium-high |
| Multiplexers | drawing and key questions go to tmux unwrapped; when silent, its facts come from its version (2004 and 1004 from 3.6, 2026 from 3.7, 2027 never). Outer-terminal questions (kitty graphics) are wrapped only after a wrapped DA1 alone is answered, each group closed by its own wrapped DA1 | measured tmux 3.5a: no DECRQM answered, wrapped replies after tmux's DA1, wrapped queries silent with passthrough off (default since 3.3); tmux CHANGES (datapoint) | medium |
| Unasked modes | 1004, 2031 and 2048 are set without asking and reset on exit; 2004 follows configuration | **no reference** says what a terminal does with a mode it lacks. Datapoints: xterm answers these DECRQMs with 0 and its `dpmodes` only traces an unknown mode; tmux 3.5a ignores them | medium |
| Meta, kitty | 1036 is queried and set only where answered reset, so Alt arrives as ESC. `Auto` pushes kitty flags 1\|4 (1\|2\|4\|8\|16 with `key_releases`) once `CSI ? u` answers, then asks again and records the flags in effect; modifyOtherKeys 1 otherwise | measured xterm `?1036;2$y`; kitty: "first setting the desired progressive enhancements and then querying" | medium-high |
| Application queries | `Matrix.query app ~owed q` sends `q` and DA1 as an owed batch and delivers `Reply` events, ending with the DA1 reply. `Terminal.send` owes nothing | the batch rule | medium-high |
| Layers | ansi (syntax) < input (framer, keys, replies; no clock, no environment) < terminal (`Caps`) < runtime (time, counts, batches, routing) < mosaic (matching, paste assembly) | design.md | high |
| Mosaic | a shortcut matches the key and its non-lock modifiers, then the base-layout key. Releases reach `Sub.on_key_release` only. A paste reaches widgets whole; `Sub.on_paste_stalled` reports a stall after a time the application picks, and `Cmd.abort_paste` ends it | skeptic T10 (a paste without end absorbed Ctrl+C) | medium-high |

### 2.1 Decisions taken during implementation

The implementation and the final review met questions the adjudication had not answered, or had
answered twice. Each is decided by the same rule, stated in the .mli, and pinned by a test:
- **Owing outlasts waiting (H1).** As adjudicated, a batch settled when the wait for its answer
  ended. The probe's RTT is unknown until its DA1, so on any link slower than 500 ms its replies
  were typed (Shift+F3, Alt+F3, then `Alt+P >|XTerm(398)`). "Replies are never keys" and the
  expiry contradicted each other; the first wins, and a late DA1 still measures the RTT.
- **The width tests run on row 2**, so a late report is `CSI 2 ; c R`, which no key sends.
- **A cursor batch settles at its report**: it sends no DA1.
- **One DA1-closed batch in flight.** A DA1 reply says that a batch was answered, not which.
  tmux drops the wrapped passthrough check, so its answer to the next DA1 passed for the check's
  and an application batch could close before its replies came. The cost: inside tmux without
  passthrough, later batches wait for the check to be given up.
- **The runtime's one-off queries are batches** (the pixel size, the kitty flags after a push),
  so their replies are never the application's, and are sent only from a raw-mode terminal.
- **1036 is queried, then set only where reset**, rather than sent unconditionally.
- **Kitty text is the key (M1).** Caps Lock with r is `CSI 114;65;82u` under all enhancements;
  the shifted key ignores Caps Lock and the text does not, so the key is `'R'`, as in legacy.
- **ESC ESC decodes one way (L1)**: a count the user cannot see no longer changes the key.
- **`Matrix.query` (M2).** Main dropped replies to an application's own queries, and the decoder
  typed them; the application now declares what it is owed.
- **A stalled paste is visible in mosaic (L4)**, and the application ends it. Time still never
  ends a paste: how long is too long is the application's choice.
- **`CSI 8 ; rows ; cols t` no longer resizes.** It answers `CSI 18 t`, which nothing sends; the
  in-band resize report is 2048's, and `Matrix.query` delivers the reply to whoever asks.
- **Reply payloads are read in `Reply`**, so `decoder.ml` (1,254 → 1,099 lines) only frames
  bytes and names keys.
- **`Keystroke` and `Mouse_report`** are the module names: `Key` and `Mouse` were taken while
  the old `Event` lived beside the decoder.
- **Suspend** drops what the decoder holds, keeps the owed counts, and cuts a paste in progress;
  **Ctrl+C** exits on a press, never a release.

## 3. What was weighed and rejected
- **`Ansi.Parser` unchanged for input.** It loses a lone ESC, Alt+[, X10 coordinates, and paste
  bytes (P1).
- **`Ansi.Parser` with an input mode.** A mode that changes the rule in five states, and output
  states for time, paste and raw bytes that output never needs.
- **d1's `Parser.Header` API.** A stateful public API on ansi with one outside consumer; P2 has
  the same single owner with ansi unchanged.
- **d2's re-feed of held units.** It holds a 5,000-digit header whole, an unstated budget, and
  cannot tokenize rxvt's `CSI 2 $`, which has no final byte.
- **Main's second lexer.** Two owners of one format; the 8-parameter cap and wrapping integers
  were its symptoms.
- **Re-decoding the buffered tail on each read.** Quadratic: a 6 MB OSC 52 in 4 KB reads is
  scanned about 1,500 times.
- **Time inside the decoder** (main, d2). The decoder must then be told every query sent (d2's
  `ask`, main's context), coupling input to every query kind.
- **A 5 s cut for strings after an eager commit** (the audit). Two typable bytes swallow typing
  until silence, and a stalled reply's tail is typed; no duration fixes both.
- **A longer ESC wait while replies are owed** (the audit). Escape then j becomes Alt+j for a vim
  user during any query (T8).
- **No time unless kitty flag 1 is confirmed.** Escape is unusable in legacy terminals.
- **d1's unshifted key plus Shift.** One keystroke decodes two ways in 21 of 47 Alt+Shift cases
  (P5), and 13 example sites match `Char` directly.
- **Caps Lock applied to ASCII letters** (the audit). Refuted: 26 Ctrl and 26 Alt shortcuts stop
  firing in kitty with Caps Lock on (S7).
- **Main's `KP_*` keys.** Two names where legacy cannot tell them apart.
- **Meta as a high byte.** Ill-formed UTF-8 is no character, and xterm's default Meta is
  well-formed Latin-1; 1036 gives Alt instead.
- **The paste idle cut** (main: 5 s, then typed; d2: partial delivery), and **a matrix paste
  budget** (main's 16 MiB, kept by the audit). Both defeat bracketing; RFC 0004 streamed.
- **Discarding a paste after N seconds of silence** (skeptic). The duration is the
  application's, which now sees the stall.
- **Settling a batch when its wait ends** (the first implementation): H1.
- **Several DA1 batches in flight, paired in order.** Sound only while every DA1 is answered.
- **Monotone capabilities** (main). A late answer could not take back a guess.
- **The environment beating answers** (main's re-applied overrides, d1's "known-broken
  override"). Kept only as identity corrections with a cited reason; the VS Code rule had none.
- **Wrapped multiplexer queries by default** (main). The outer terminal answers for what tmux
  draws, nothing answers with passthrough off, and wrapped replies come after tmux's DA1.
- **Rejecting mouse 0 or extra kitty fields** (d2, the audit). A rejected release sticks a drag;
  a rejected field drops a future kitty's keys.
- **The runtime rebuilding F3 from an ambiguous report.** Key decoding in the wrong layer.

## 4. Behavior breaks
- `Matrix_input` is `Keystroke`, `Mouse_report`, `Reply` and `Decoder`: `Input.Parser` (with
  its clock, `drain`, protocol context and paste options) and the `Event` types (`Key`, `Mouse`,
  `Modifier`, `Response`, ...) are removed.
- `Matrix.run`'s `on_input` and `Backend.wait` take `Matrix.event` (`Key`, `Key_release`,
  `Text`, `Mouse`, `Paste_start`, `Paste_data`, `Paste_end`, `Focus_in`, `Focus_out`, `Resize`,
  `Color_scheme`, `Invalid`, `Reply`). `Backend.t` has an `env` field.
- `Matrix.create`, `attach` and `Matrix_eio.create` lose `focus_reporting` and `initial_caps`,
  and take `?key_releases` and `?caps`; `kitty_keyboard` is `` [`Auto | `Off] ``.
- `Terminal.capabilities` is `Terminal.Caps.t`: `kitty_keyboard` holds the flags last reported,
  `term` is gone, `meta_sends_escape`, `multiplexer` and `identity` are new (the identity keeps
  no source). `terminal_info`, `apply_capability_event` and `probe` are gone, `Terminal.make`
  takes the capabilities, and `set_unicode_width` no longer changes them.
  `Ansi.query Kitty_graphics` no longer ends with DA1.
- Keys: `Char` is the character typed, Shift consumed, under kitty's text too; 0x0A is Ctrl+J
  (`Line_feed` is gone); keypad keys carry `keypad`, and `CSI E` is `Begin`; releases are
  `Key_release`; `text` is empty for chords and `shifted_key` is gone; Alt no longer sets Meta;
  bit 8 is super; an out-of-range key sequence is a reply; bytes that are not UTF-8 are
  `Invalid`. Modified F1–F4 and `CSI 1 ; m R` decode as keys when no report is owed.
- `ESC ]`, `ESC P` and `ESC _` are Alt keys unless a string reply is owed, and `ESC ^` and
  `ESC X` always. An unowed reply (one sent with `Terminal.send`) is input, where main dropped it.
- Mouse: `Down`, `Up`, `Move`, `Drag` and `Button n` are `Press`, `Release`, `Motion None`,
  `Motion (Some b)` and `Extra n`; `Scroll` has no delta; coordinates are clamped.
- Paste streams with no limit and no idle cut; `Matrix.abort_paste` ends one, and suspend cuts
  one in progress.
- Capabilities are not monotone: `KITTY_WINDOW_ID` no longer implies SIXEL, 997 no longer
  implies 2031, the VS Code rule is gone, the kitty keyboard comes from its answer only, DECRPM 3
  counts, and on Windows SIXEL and the kitty protocols follow the answers. `TERM_PROGRAM` names
  the terminal only when directly attached; tmux is asked unwrapped.
- The probe starts with S7C1T, asks for 1036, writes its width tests on row 2, and drops iTerm2's
  `OSC 1337;ReportCellSize`. 1004, 2031 and 2048 are set unasked; 1036 where reset.
- `CSI 8 ; rows ; cols t` no longer resizes. The pixel size is not queried without a raw-mode
  terminal input. One DA1 batch is in flight at a time, so a `Matrix.query` can wait up to 5 s
  inside tmux without passthrough. Ctrl+C exits on a press only.
- Mosaic: `Shortcut` and `Keymap` take `Input.Keystroke` values and match exactly, then on the
  base key; the lowercase fallback, keypad normalization and Alt implying Meta are gone, and
  `char ~shift:true 'r'` is `'R'`. Editors insert the keystroke's text; releases never reach
  widgets; a paste reaches them whole. `Event.Mouse.modifier` is `Input.Keystroke.mods`,
  `scroll_direction` is mosaic's own, and `Renderer.dispatch_key` and `dispatch_mouse` take
  `Input.Keystroke.t` and `Input.Mouse_report.t`.

## 5. Verification
Every behavior is pinned by a unit test, the runtime's on a virtual clock (waits, round trip,
give-up, late answers, paste, Ctrl+C). The key tests replay the 53 recordings of xterm 398's
bytes for 31 chords in five configurations, and sweep 1,872 keystrokes (legacy, kitty 1|4 and
kitty 1, Caps Lock off and on) plus 658 under every enhancement: each decodes to its legacy value
where the encoding carries the same facts. The capability tests are tables of the pure
derivation, with the environment built in the test.

**Fuzzing.** In CI, `test_decoder_props` checks split invariance and silence against a reference
model (20,000 cases each), that a cut reply is never typed, and totality with bounded memory.
The final review's fuzzers, re-run on `9d897c96`:
- the decoder against an independent model of framing and silence: 2,000,000 cases, 0
  differences, 0 exceptions; against one feed under random chunking: 2,000,000, 0 differences;
- never typed: 21 replies cut at every position, 505 checks, 0 typed; totality: 20,000 random call orders,
  0 exceptions, at most 605 words held after `finish`;
- the runtime on a virtual clock against a simulated terminal, in-order, 100 runs per latency:
  before H1's fix, 100 of 100 runs typed a reply from 0.55 s; now none does from 1 ms to 4 s.
  At 4.9 s, where replies straddle the 5 s bound, 82 do, as the bound decides. With 10–60% of
  replies lost: 0 of 600;
- 3,000 random runs (latency, jitter, loss, stalls, reordering, resizes): 0 exceptions, and no
  reply typed in the 1,643 runs without stalls or reordering. A stall after ESC or `ESC [`
  longer than the ESC wait types the rest, the trade 2d makes; reordering, which no single
  terminal does, also leaks. Two lossy runs at 2 s ended with a report still owed, so their last
  Shift+F3 read as a report: the owed rule's stated cost.

**xterm 398.** `input_diff` (`matrix/test/xterm_diff`, outside CI) runs a matrix application in
xterm under Xvfb. At decTerminalID 100 and 420 it checks the capabilities derived from xterm's
replies to the real probe, then presses the 31 entries of `chords.txt` through XTest (25 chords,
four again with Caps Lock on) and checks each keystroke. Re-run on `9d897c96`: 0 unexpected, at
both identities, for capabilities and keys.

**tmux 3.5a** (skeptic): DECRQM of 2026, 2027, 2004, 2031, 2048, 1004 and 1016 gets no answer;
DA1 is `?1;2;4c`, XTVERSION `tmux 3.5a`; `CSI ? u` is silent. Wrapped queries are silent with
passthrough off; with it on, the outer terminal answers after tmux's own DA1.

**Benches.** The decoder against main's parser on the same workloads, side by side at `66b4e45f`
(Core Ultra 5 235, median of three; time, then allocated words): ASCII typing 0.90× / 0.02×,
Unicode typing 1.08× / 0.59×, legacy keys 1.50× (110 against 73 µs) / 0.63×, kitty keys 1.06× /
0.37×, keys byte by byte 0.78× / 0.55×, OSC in 1-byte reads 1.02–1.03× / 0.70×, a 3 KB OSC in
32-byte reads 1.82× (7.8 against 4.3 µs) / 1.15×, mouse 1.06× / 0.40×, paste 64 KiB and 1 MiB
0.85× and 0.69× / 1.0×. Legacy keys exceed the adjudicated 0.8–1.34×; at about 57 ns a key,
nothing depends on it. `9d897c96` records the decoder's baselines. Mosaic's runtime bench: a
wheel round trip allocates about 2,100 fewer words; its 7 cases give 5 passed and 2 inconclusive
(primary mode loses exactness). The input library is 1,603 lines of .ml against main's 2,557,
`caps.ml` 455 against 726, and `matrix.ml` grew from 1,547 to 1,807.

**Known defects.** Main's 18 defect classes are fixed or behave as decided: the 4096 cap (a
6,000-byte OSC 52 split anywhere decodes the same), raising on untrusted bytes, modified F1–F4,
SS3 with modifiers, Alt prefixes, the unbounded deferral, SGR motion, overflow, parameter
counts, reply meanings, 2048, one keystroke with two values, end of input and stalled paste,
split invariance, the non-hermetic test, the duplicated grammar, and the environment. Decoding
that depends on timing remains, inherently, under the waits above.

## 6. Open questions for the maintainer
Questions 1–5 were adjudicated as §1–§2 state, for the maintainer to confirm.
1. Is a key's value the character typed with Shift consumed, rather than kitty's unshifted key
   plus Shift?
2. Is bit 8 of the xterm modifier parameter super (kitty) rather than Meta (xterm's table)?
3. Should bytes that are not UTF-8 be `Invalid`, with 1036 set where the terminal answers reset?
4. Are the waits max(50 ms, 2 RTT) and max(500 ms, 4 RTT) acceptable when no reference fixes a
   value?
5. Should paste stream to the application with no matrix budget, mosaic assembling it?
6. Is the 5 s bound on an unanswered batch, until which a solicited reply is never a key,
   acceptable?
7. Inside tmux without `allow-passthrough`, should the passthrough check be sent only when an
   outer-terminal fact is needed, so batches do not queue behind it for 5 s?
8. Since xterm answers 2027 with 0 and measures by code point, should xterm get an identity row
   of `Wcwidth`? (Settled 2026-10-23: no row is needed, since "not recognized" means `Wcwidth`.)
9. Should a 2027 answer of "not recognized" mean `Wcwidth` (settled 2026-10-23: yes), and
   "reset" mean that the runtime sets the mode?
10. Should tmux's rows keep 2004 and 1004 from 3.6, when it began answering, though it had both
    earlier?
11. Should input-method `Text` reach mosaic widgets as a paste, or as an event of its own?
12. Is a small Python XTest driver acceptable in `matrix/test/xterm_diff`, outside CI?
13. A kitty release carries no text, so with Caps Lock a press is `'R'` and its release `'r'`:
    is that acceptable?
