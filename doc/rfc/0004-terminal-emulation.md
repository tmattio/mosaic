# RFC 0004: Terminal emulation — the escape-sequence parser, the VTE, and the grid beneath them

- Status: Decided 2026-10-22; implementation on branch `vte-redesign`.
- Baseline: main @ `07d38086` plus the 35 fix commits of PRs #46–#96 (Thomas Gazagnaire),
  rebased, which this redesign builds on and keeps the tests of.
- Process: the "Becoming certain" campaign of raven's `doc/dev/design.md`, applied to matrix. For
  each of two areas (A: parser; B: terminal model), two blind designs, one from the
  specifications' model and one from callers' needs, then an audit comparing them and today's
  code (today's design treated as a candidate on equal footing, with design defects separated
  from implementation defects), a skeptic re-reading every citation and re-running every
  measurement, and an adjudication.
- Compatibility: none before 1.0. Breaks are listed in §5 and stated in their commits.

## Summary

The 35 PRs fixed real bugs, but the same bug classes recurred because some facts had no single
owner:
- what a byte means in each parser state (seven PRs, two of them contradicting each other);
- the colour of a blank, decided in four places;
- the halves of a wide character cut by an edit, repaired by both the grid and the VTE;
- the grapheme cluster in progress, rebuilt from the grid instead of being kept as state, with
  four mechanisms in two layers;
- per-screen state, kept in parallel fields.

The redesign gives each fact one owner:
- **The parser reports syntax only.** The VTE gives a sequence its meaning.
- **The grid owns whole graphemes and the blank.** The VTE chooses the blank's colour.
- **`Matrix_text.Segmenter` carries UAX #29 state across writes.** The VTE keeps the open
  cluster as state, so the screen is a function of the bytes, not of how they were split.

## 1. Area A: the parser (`matrix/lib/ansi/parser`)

| decision | rule | references | confidence |
|---|---|---|---|
| Input | UTF-8 decoded first; each ill-formed subsequence is one U+FFFD; chunking never changes the tokens once adjacent `Text`/`Data` are joined | Unicode §3.9 | high |
| Tokens | `Text`, `Control of char` (C0, DEL, C1 written ESC Fe), `Esc {intermediates; final}`, `Csi {marker; params; intermediates; final}`, `Open`/`Data`/`Close` for strings. Meaning lives in the VTE, one named function per arm | ECMA-48 §5, §6 (HPR ≠ CUF in general); ctlseqs SCOSC/DECSLRM | high |
| Recovery | ESC, CAN, SUB end any sequence or string. A C0 inside an ESC/CSI sequence is performed and the sequence continues; inside strings and DCS headers it never is. A malformed sequence is consumed to its final byte and has no effect | VT100 UG ch. 3; VT510 §4.3.5; Williams' DEC parser | high |
| C1 in UTF-8 | U+0080–U+009F are never controls, and are dropped (no glyph). Raw 0x80–0x9F bytes are ill-formed → U+FFFD | xterm (removed `utf8controls`); Unicode §23.1; kitty, foot, Ghostty, st, Alacritty, Windows Terminal | medium-high |
| Strings | streamed, no content held. `Close` reports `St`, `Bel` (OSC) or `Aborted`; an aborted string commits nothing. Content is ECMA-48's command-string set plus non-ASCII | ECMA-48 §5.6, §8.3.6 | high |
| Parameters | absent ≠ 0. `Params.pn` (absent or 0 → default: counts and positions, VT510 §4.3.3) and `Params.ps` (0 is 0). Values saturate at `max_int`. 256 fields; a longer sequence has no effect (a memory budget, the one bound no specification sets) | ECMA-48 §5.4.2–5.4.3; VT510 §4.3.3 | high; 256: medium |
| Intermediates | up to 4 stored (ECMA-35 allows more; no consumer acts on more, so the bound is unobservable) | ECMA-35 §13.2.2, §14.4.2 | high |
| Non-ASCII inside ESC/CSI | ends the sequence with no effect and is dropped | xterm, foot (datapoints; no reference governs) | low-medium |
| SGR | decoded by `Style.apply_sgr` in ansi, sharing attribute codes with the writer through `Sgr_attrs` | ECMA-48 §8.3.117; T.416 colon form | high |
| `Ansi.strip` | the `Text` and C0 `Control` tokens of the parser (one tokenizer) | measured 13 → 35 µs per 18 KB paste | high |

## 2. Area B: the terminal model (`matrix/lib/vte`, `grid`, `text`)

| decision | rule | references | confidence |
|---|---|---|---|
| Cursor | `{row; col; last_column}`. Printing at the right margin with autowrap on sets the Last Column Flag (LCF); the next graphic wraps first. Every function that sets the cursor position or changes cells clears it, and so does resetting DECAWM. HT and CHT keep it | wraptest hardware results (VT220/420/510); STD-070 via wraptest | high / medium |
| DECSC | saves position, LCF, pen (SGR, link, DECSCA), G0–G3, GL, single shift, origin mode; not DECAWM. One slot per screen, starting at the power-up state | wraptest; VT510 DECSC/DECRC; xterm (datapoint) | high |
| Screens | one record per screen: grid, wrap marks, DECSC slot. 47 switches, 1047 also erases on exit, 1048 saves/restores, 1049 saves, switches and erases | ctlseqs | high |
| Resets | RIS restores the power-up state. DECSTR returns each setting to its power-up value (matrix powers up with autowrap on), the pen and charsets to their defaults, and both slots to power-up | VT510 DECSTR Table 5-9 (principle) | medium |
| Resize | keeps the text, without reflow. Shrinking drops rows below the cursor first, then moves top rows into the scrollback; growing takes them back. Saved cursors move with their rows; margins reset; LCF cleared; open cluster closed | xterm (datapoint) | medium-high |
| Blank | one blank for every erase, insert, delete and scroll: a space, no attributes, unprotected, on the pen's background (BCE). The grid repairs a grapheme cut by any operation with that operation's colour | ECMA-48 §8.3.26; VT510 DCH/EL/DL | high |
| Selective erase | DECSED/DECSEL remove the characters of unprotected cells and keep their rendition | VT220 PRM §4.12; VT510 DECSERA | high |
| Protection | a non-visual bit in the grid's attribute plane, written only by `set_grapheme ~protected`, read by `is_protected` and `erase_unprotected`. It moves with the cell, so no copy of the grid's move logic exists | VT220 §4.9 (a character attribute) | medium-high |
| Wrap marks | a per-row fact owned by the VTE's screen module, set only by printing, kept in the scrollback | layer test: the grid never moves rows by itself | medium-high |
| Printing | `Matrix_text.Segmenter` carries UAX #29 state and the width of the unit in progress, and width never decreases within a unit. The VTE places a unit as soon as it has width, and re-places it from its origin when its bytes grow. A wrap erases to the margin first; with autowrap off a unit ends at the margin; insert mode shifts by the growth; a unit wider than the line is clamped. A zero-width unit that does not continue the open cluster joins the cell before the cursor without changing its width (dropped if there is none). One print path; batching lives in the segmenter | UAX #29 (no look-ahead); exhaustive width check (142M steps); xterm (zero-width join) | high; rules: medium |
| No controls in cells | the grid raises on a control character in `set_grapheme`; `draw_text` skips zero-width clusters | — | high |
| Replies | `feed` returns the replies (`""` when none): DA `CSI ? 1 ; 2 c`, DA2 `CSI > 0 ; 0 ; 0 c`, DSR 5 `CSI 0 n`, CPR, DECXCPR `CSI ? r ; c ; 1 R` (a stated deviation from the VT510 page). No identity knobs; the identity promises only what is implemented | ctlseqs; VT510 DA1/DA2/DSR; vim and tmux (no difference between candidates) | medium-high |
| Parameters | counts are clamped to the distance they can act on before any arithmetic. REP repeats the open cluster n times with no cap, reduced exactly by the screen's period beyond one screen plus scrollback | ECMA-48 §8.3.103 | medium |
| Charsets | DEC Special Graphics (incl. 0x5F blank), UK, the national sets with all their finals (incl. 9, f, `), SS2/SS3 | VT220 PRM tables 2-5..2-15; VT100 UG table 3-9 | medium |
| Span width | packed cells hold extents up to the grid's width (no 4-cell cap) | — | high |

## 3. What was weighed and rejected
- **Named parser tokens** (today's design). They forced the parser to change for every new VTE
  function: ten PRs added 138 parser lines. They also cannot express meanings that depend on
  device state.
- **Executing UTF-8 C1.** Mis-encoded text (cp1250 "ž" is 0x9E, PM) would swallow the output
  that follows it.
- **Showing U+FFFD for UTF-8 C1.** The characters are well-formed, and no terminal does this.
- **Protection as a VTE side plane.** Equally correct on screen, but it must copy every cell move
  the grid makes, which is the duplication behind #62/#64/#84.
- **An ASCII fast path in the VTE.** A second print path, and the one that broke split invariance
  in the audit's prototype.
- **A VT220 identity.** It promises 8-bit controls and DECSCL, which matrix lacks; no client was
  found that behaves differently.

## 4. PRs #46–#96 under this design
All 35 commits are kept in history, with their authorship, and their tests are kept. The redesign
commits that follow them change:
- **Reversed:**
  - #49 (UTF-8 C1 executed);
  - #85's dispatch of cut-short strings;
  - #96's split at Prepend (UAX #29 clusters are now kept whole and split-independent);
  - #79's zero-width join in the grid, which stored C1 controls (the capability moves to the VTE).
- **Rewritten on the new shape:** #46–#48, #86, #88 (parser); #52 (replies), #53 (REP),
  #62/#64/#84 (edits over `Grid.shift`), #63 (resets), #65 (the one blank), #82 (dropped marks),
  #94 (protection in the grid; selective erase keeps rendition), #95 (wrap marks).
- **Fixed beyond the PRs:**
  - VPR in origin mode;
  - mode 47 clearing on entry;
  - mode 1048;
  - REP's cap;
  - DECALN resetting origin mode and SGR;
  - DECSTR unprotecting cells;
  - SS2/SS3, DEC graphics 0x5F and the alternate national finals;
  - `CSI <max_int> C` overflowing;
  - CBT at a pending wrap;
  - the emulator examples now send replies back to the PTY.

## 5. Behaviour breaks
- `Ansi.Parser`'s token type is replaced (syntax tokens), and SGR decoding moves to
  `Style.apply_sgr`.
- UTF-8 U+0080–U+009F are no longer controls.
- Cut-short strings no longer act.
- `Vte.create` loses `?da1`/`?da2`, and `feed` returns replies (`take_replies` is removed).
- `resize` keeps text.
- DECSEL/DECSED keep the rendition of erased cells.
- Prepend clusters are drawn whole.
- `Grid.scroll` becomes `Grid.shift ~color`.
- Blanks repaired by the grid take the operation's colour.

## 6. Verification
Every behaviour is pinned by a unit test. A differential fuzzer compares the VTE with xterm 398
under Xvfb: random sequences drawn from the implemented functions are written to both, and their
screens are compared, xterm's dumped through its print-screen control. Each difference is either
fixed, or recorded as a deviation this RFC decides (§2: the wrap erase, DECSTR's autowrap, BCE on
selective erase, C1 handling, the identity).
