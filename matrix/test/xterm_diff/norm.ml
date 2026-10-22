(* Normalisations: the differences between matrix and xterm 398 that are
   expected, each named, explained and narrow.

   A difference is expected when matrix follows a rule that RFC 0004 decides
   (a deviation, with its reference) or when xterm departs from the reference
   that matrix follows (a quirk, cited to xterm's source or changelog). Each
   one is removed in one of four ways, chosen to hide as little as possible:

   - a generator restriction: the generator never produces the input;
   - an xterm rewrite: xterm receives an input that it treats as matrix treats
     the original, and matrix receives the original;
   - a matrix rerun: matrix also runs with another width method;
   - a screen or reply normalisation, applied to the outputs.

   Every one fires only on its documented pattern, and [--strict NAME] (or
   [--strict all]) turns it off. The comparison reports which ones fired, and
   anything left is a finding.

   The xterm rewrites follow matrix's state as they go: a VTE (with the
   wcwidth method, xterm's) is fed the original bytes alongside, so that a
   rewrite can ask where matrix's cursor is, as some patterns depend on it.
   Some screen normalisations concern cells that move after the pattern: a
   second VTE is fed the same bytes and a tag, a private-use character, in
   those cells, and the cells still tagged at the end are the ones
   normalised. *)

type kind = Deviation | Quirk | Configuration

let kind_name = function
  | Deviation -> "deviation"
  | Quirk -> "xterm quirk"
  | Configuration -> "configuration"

type info = { name : string; kind : kind; how : string; why : string }

(* The registry. [why] states the rule and cites it; the code below carries
   the details. *)
let registry =
  [
    {
      name = "graphemes";
      kind = Deviation;
      how = "matrix rerun";
      why =
        "matrix places UAX #29 grapheme clusters under its default width \
         method (RFC 0004 §2 Printing); xterm places each character by its \
         wcwidth and attaches the zero-width ones (charproc.c, doparsing), and \
         398 ignores VS15/VS16 for width (changelog, patch #404). When the two \
         methods give matrix different screens, xterm is compared with \
         matrix's `Wcwidth run.";
    };
    {
      name = "zero-width-join";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "a zero-width unit that continues nothing joins the cell before the \
         cursor, or the cursor's with the Last Column Flag, or is dropped at \
         the first column, and REP after it repeats nothing (RFC 0004 §2 \
         Printing; vte.mli); xterm joins the last cell written, or after a \
         cursor motion the cell at the cursor, and in an empty cell makes it a \
         character of its own (charproc.c, doparsing: char_was_written). xterm \
         gets the unit between CUB and CUF to matrix's cell, or not at all; on \
         a blank cell it is removed from both screens.";
    };
    {
      name = "format-chars";
      kind = Quirk;
      how = "screen";
      why =
        "a format character continues the cluster before it (UAX #29 GB9) and \
         is kept in its cell; xterm discards the zero-width ones its \
         character-class table puts in CNTRL, U+200B..U+200F, U+202A..U+202E, \
         U+2060..U+206F and U+FEFF (charproc.c, doparsing: is_formatter, then \
         CASE_IGNORE). They are removed from matrix's screen.";
    };
    {
      name = "wrap-erase";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "a wide unit that does not fit before the right margin erases the \
         cells to the margin before it wraps (RFC 0004 §2 Printing); xterm \
         leaves them. xterm gets EL 0 before such a character.";
    };
    {
      name = "autowrap-off-wide";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "with autowrap off, a wide unit that does not fit ends at the margin, \
         replacing what is there (RFC 0004 §2 Printing); xterm does not write \
         it. xterm gets CUB 1 before such a character.";
    };
    {
      name = "rep-cluster";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "REP repeats the whole unit before it, combining marks included (RFC \
         0004 §2 Parameters; ECMA-48 §8.3.103 'the preceding graphic \
         character'); xterm repeats the last character only. xterm gets the \
         unit written out.";
    };
    {
      name = "lcf-edits";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "SU, SD, SL, SR, DECIC and DECDC change cells, so they clear the Last \
         Column Flag (RFC 0004 §2 Cursor; wraptest); xterm keeps it. xterm \
         gets CHA to the cursor's column after them when the flag was set.";
    };
    {
      name = "decaln-pen";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "DECALN resets the whole rendition, colours included (RFC 0004 §4, DEC \
         STD 070 via xterm); xterm's resetRendition keeps the pen's colours \
         (charproc.c, CASE_DECALN). xterm gets SGR 39;49 before it.";
    };
    {
      name = "underline-style";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "SGR 4:n selects an underline style, 4:0 none and 4:2 double \
         (Style.apply_sgr); xterm 398 ignores a parameter with sub-parameters \
         other than 38 and 48 (charproc.c, CASE_SGR). xterm gets 24, 21 or 4.";
    };
    {
      name = "c1-utf8";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "U+0080..U+009F decoded from UTF-8 are not controls and are dropped \
         (RFC 0004 §1 C1 in UTF-8); xterm performs them. xterm does not get \
         them, outside escape and control sequences.";
    };
    {
      name = "identity";
      kind = Deviation;
      how = "replies";
      why =
        "DA answers CSI ? 1 ; 2 c and DA2 CSI > 0 ; 0 ; 0 c, with no identity \
         knobs (RFC 0004 §2 Replies); xterm's DA2 carries its patch number, \
         and above decTerminalID 100 its DA has its own level and features.";
    };
    {
      name = "decxcpr-page";
      kind = Deviation;
      how = "replies";
      why =
        "DECXCPR is CSI ? r ; c ; 1 R (RFC 0004 §2 Replies, a stated deviation \
         from the VT510 page); xterm as a VT100 omits the page.";
    };
    {
      name = "sub-glyph";
      kind = Deviation;
      how = "screen";
      why =
        "SUB shows the reversed question mark U+2E2E (VT510 Programmer \
         Information ch. 4; vte.mli); xterm shows U+2592 up to decTerminalID \
         200 and U+2426 above (charproc.c, CASE_SUB).";
    };
    {
      name = "can-checkerboard";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "CAN shows no error character (VT220 Programmer Reference Manual ch. \
         4; vte.mli); xterm at decTerminalID 100..199 shows U+2592 \
         (charproc.c, CASE_CAN, following the VT100 User Guide). xterm gets ST \
         instead of CAN, which ends what CAN ends.";
    };
    {
      name = "lone-continuation";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "a continuation byte that begins no character is ill-formed, one \
         U+FFFD (Unicode §3.9; RFC 0004 §1 Input, and C1 in UTF-8 for \
         0x80..0x9F); xterm takes it as the code point of its value when the \
         next byte is 0x21..0x7F, 'to allow for C1 control string' (ptydata.c, \
         decodeUtf8, whatever utf8Weblike says). xterm gets U+FFFD.";
    };
    {
      name = "sequence-latin1";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "a non-ASCII character ends an escape or control sequence, which has \
         no effect (RFC 0004 §1); xterm reads U+0080..U+00FF there as their \
         7-bit counterparts, since its parse tables have 256 entries \
         (charproc.c, doparsing). xterm gets U+0100, which it drops as matrix \
         does.";
    };
    {
      name = "s7c1t-vt100";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "ESC SP F and ESC SP G are escape sequences like any other; xterm as a \
         VT100 leaves its parser in their state and eats the next character \
         (charproc.c, CASE_S7C1T/CASE_S8C1T reset the state only at level 2). \
         xterm does not get them below decTerminalID 200.";
    };
    {
      name = "decstr-rendition";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "DECSTR returns the rendition to its default (VT510 DECSTR, Table 5-9; \
         RFC 0004 §2 Resets); xterm keeps faint, crossed-out and double \
         underline (charproc.c, ReallyReset). xterm gets SGR 22;24;29 before \
         it.";
    };
    {
      name = "cbt-pending-wrap";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "CBT sets the cursor position, so it clears the Last Column Flag (RFC \
         0004 §2 Cursor); xterm moves the cursor and leaves its wrap pending \
         (tabs.c, TabToPrevStop calls set_cur_col and not ResetWrap). xterm \
         gets CHA to the cursor's column after it when the flag was set.";
    };
    {
      name = "rep-max";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "REP's count has no maximum (RFC 0004 §2 Parameters); xterm clamps \
         every parameter to 65535 (ptyx.h, MAX_I_PARAM). Above that, xterm \
         gets the smallest count with the same effect: a screen of rows \
         filled, plus the rest modulo the units a row holds.";
    };
    {
      name = "sd-zero";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "SD 0 scrolls one line, as 0 is the default for a count (VT510 §4.3.3; \
         RFC 0004 §1 Parameters); xterm reads CSI 0 T as the reset of \
         highlight mouse tracking (charproc.c, CASE_TRACK_MOUSE). xterm gets \
         CSI T.";
    };
    {
      name = "param-budget";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "a control sequence of more than 256 parameter fields has no effect \
         (RFC 0004 §1 Parameters, a memory budget); xterm acts on its first \
         ones. xterm does not get it.";
    };
    {
      name = "rep-after-control";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "REP repeats the unit printed just before it, if nothing came between \
         them (vte.mli); ECMA-48 §8.3.103 leaves REP after a control function \
         undefined, and xterm repeats its last character after some (another \
         REP, a string, an aborted sequence). xterm does not get REP when \
         matrix repeats nothing.";
    };
    {
      name = "sequence-zero-width";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "a non-ASCII character ends an escape or control sequence, and is \
         content in a string (RFC 0004 §1); xterm handles a zero-width \
         character before its parser, in any state, joining it to a cell \
         (charproc.c, doparsing: the combining branch precedes the state \
         tables). xterm gets U+0100 in a sequence and nothing in a string.";
    };
    {
      name = "charset-internal";
      kind = Quirk;
      how = "generator";
      why =
        "the UK set's pound sign and DEC Special Graphics' blank (0x23, 0x5F) \
         are characters of one cell like any other (VT100 User Guide table \
         3-9); xterm stores them as internal codes, its print-screen writes \
         the pound as 0x1E, and a line holding one loses the character that \
         wraps (observed in 398). After a designation of either set, no # or _ \
         respectively is generated.";
    };
    {
      name = "selective-erase";
      kind = Deviation;
      how = "screen";
      why =
        "DECSED and DECSEL turn the characters of unprotected cells into \
         spaces and keep their rendition (VT220 Programmer Reference Manual \
         §4.12; RFC 0004 §2 Selective erase, §6 'BCE on selective erase'); \
         xterm blanks them as ED and EL do, on the pen's background. Those \
         cells are compared by their text.";
    };
    {
      name = "subparams-default";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "a control sequence other than SGR with sub-parameters is malformed \
         and has no effect (ECMA-48 §5.4.2; RFC 0004 §1 Recovery); xterm 398 \
         'rejects' it with ResetState, which clears the parameters, and still \
         performs the function with its defaults: CSI 2:3 H homes the cursor \
         (charproc.c, doparsing). xterm does not get it.";
    };
    {
      name = "delete-wide-stale";
      kind = Quirk;
      how = "screen";
      why =
        "DCH, SL and DECDC move the cells after the deleted ones left (ECMA-48 \
         §8.3.26); xterm then clears the vacated columns from stale data, and \
         when the first of them held the second half of a wide character, the \
         clear extends left over the column that has just received the row's \
         last character (screen.c, ScrnDeleteChar: ClearCells; util.c, \
         DamagedCells). That cell is not compared, wherever it moves.";
    };
    {
      name = "cut-wide";
      kind = Quirk;
      how = "screen";
      why =
        "a character written over half of a wide one blanks the other half: a \
         space with no attributes on the background of the operation, its \
         combining characters gone (RFC 0004 §2 Blank); xterm puts a space in \
         that half and keeps its attributes and combining characters (observed \
         in 398). Such blanks, followed as they move, are compared by their \
         text.";
    };
    {
      name = "moved-wide-marker";
      kind = Quirk;
      how = "screen";
      why =
        "a wide character keeps its two cells when an edit moves it; xterm's \
         SL, SR, DECIC and DECDC move its first cell without the marker of its \
         second, which then prints as a space though the character still \
         covers it (observed in 398; fuzz triage C10). A space after a wide \
         character in xterm's print counts as its second cell.";
    };
    {
      name = "unknown-charsets";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "a designation of a set this terminal has no table for, such as DEC \
         Supplemental (<) or DEC Technical (>), designates nothing: the slot \
         leaves characters unchanged (vte.mli; RFC 0004 §2 Charsets lists the \
         sets); xterm has those tables. xterm gets the ASCII designation.";
    };
    {
      name = "national-utf8";
      kind = Deviation;
      how = "generator";
      why =
        "national mode (DECNRCM) changes which sets are available and nothing \
         else: UTF-8 is decoded first (RFC 0004 §1 Input; vte.mli); xterm \
         turns UTF-8 off while it is set (charproc.c, modified_DECNRCM: 'UTF8 \
         mode temporarily OFF'). After DECSET 42 no non-ASCII byte is \
         generated.";
    };
    {
      name = "ris-national";
      kind = Quirk;
      how = "generator";
      why =
        "RIS returns to the power-up state (RFC 0004 §2 Resets); xterm's RIS \
         clears national mode without turning UTF-8 back on, so UTF-8 stays \
         off (charproc.c, ReallyReset sets the flags to their initial values \
         and skips modified_DECNRCM), and the state leaks into later cases. \
         RIS is not generated after DECSET 42.";
    };
    {
      name = "national-designation";
      kind = Deviation;
      how = "generator";
      why =
        "national sets are available in national mode only: designated outside \
         it they leave characters unchanged until DECNRCM is set, and they do \
         again once it is reset (vte.mli, after VT220 Programmer Reference \
         Manual §2.4.3); xterm designates ASCII outside national mode, and \
         keeps translating with a national set after DECRST 42 (charproc.c, \
         CASE_GSETS). National sets are designated only after DECSET 42, and \
         DECRST 42 does not follow them.";
    };
    {
      name = "wider-than-line";
      kind = Deviation;
      how = "generator";
      why =
        "a unit wider than the line is narrowed to it (RFC 0004 §2 Printing; \
         vte.mli); xterm with one column shows nothing for a wide character. \
         No wide character is generated below two columns.";
    };
    {
      name = "decrc-sgr-background";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "DECRC restores the rendition DECSC saved, colours included (VT510 \
         DECSC; RFC 0004 §2 DECSC); xterm restores its SGR foreground and not \
         its SGR background, so a later SGR 7 or 27 brings back the background \
         set before DECRC (cursor.c, CursorRestoreFlags; charproc.c, \
         setExtendedBG). xterm gets the restored background as an SGR after \
         DECRC, SCORC and the resets of 1048 and 1049.";
    };
    {
      name = "autowrap-off-flag";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "with autowrap off the Last Column Flag is never set (RFC 0004 §2 \
         Cursor; wraptest), so setting autowrap again lets the next character \
         replace the last column; xterm sets its wrap flag at the last column \
         whatever the mode, and DECSET 7 or DECSTR then makes the next \
         character wrap (observed in 398). xterm gets CHA to the cursor's \
         column before them, there.";
    };
    {
      name = "orphan-half";
      kind = Quirk;
      how = "screen";
      why =
        "a character written over the first half of a wide character blanks \
         the second (RFC 0004 §2 Blank); xterm keeps that second half, and \
         writing there later blanks the cell before it, which by then holds \
         another character (observed in 398; fuzz triage C11). That cell is \
         not compared, wherever it moves.";
    };
    {
      name = "decsc-slots";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "DECSTR and RIS return both DECSC slots to the power-up state (RFC \
         0004 §2 Resets; vte.mli); xterm's DECSTR saves the current state, \
         wrap flag included, in the slot of the screen in use only, and its \
         RIS resets only the normal screen's (charproc.c, ReallyReset: \
         CursorSave). After a DECRC from such a slot, xterm gets the power-up \
         state: origin mode off, home, SGR 0, DECSCA 0, ASCII sets and SI.";
    };
    {
      name = "su-whole-region";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "SU scrolls the scroll region only (ECMA-48 §8.3.147; DECSTBM); when \
         it scrolls by the region's whole height or more, xterm also blanks \
         the rows outside the region (observed in 398, with jumpScroll off). \
         xterm gets SU h - 1 then SU 1, h the region's height.";
    };
    {
      name = "reverse-wrap-lcf";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "with reverse-wrap, CUB n from the Last Column Flag moves n - 1 \
         columns, the flag taking the first step (RFC 0004 §2 Cursor; \
         vte.mli); xterm moves n - 2, as its loop loses a step after the one \
         the flag took (cursor.c, CursorBack). xterm gets CUB n + 1.";
    };
    {
      name = "combining-limit";
      kind = Configuration;
      how = "screen";
      why =
        "a cell holds any number of combining characters; xterm keeps 5 at \
         most, whatever combiningChars asks (main.c clamps it). Cells of \
         matrix with more are cut to 5 for the comparison.";
    };
    {
      name = "reverse-wrap-origin";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "with reverse-wrap, BS or CUB that would go back past the first column \
         of the first row has no row to go to and stops there (RFC 0004 §2 \
         Cursor; vte.mli); xterm reads the line above the screen, and above a \
         scroll region ends in it (cursor.c, CursorBack). xterm gets CHA 1 \
         instead.";
    };
    {
      name = "insert-cut-wide";
      kind = Quirk;
      how = "screen";
      why =
        "DECIC at the second half of a wide character cuts it, and the cut \
         character becomes blanks (RFC 0004 §2 Blank); xterm keeps its first \
         half (observed in 398). That cell is not compared, wherever it moves.";
    };
    {
      name = "decstr-national";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "DECSTR returns national mode to the multinational set (VT510 DECSTR, \
         Table 5-9: DECNRCM); xterm keeps it (charproc.c, ReallyReset). xterm \
         gets DECRST 42 after it.";
    };
    {
      name = "decstr-protection";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "DECSTR returns the pen to unprotected; the cells written protected \
         stay so (RFC 0004 §4, 'DECSTR unprotecting cells'; VT510 DECSTR); \
         xterm also turns its protection mode off, after which DECSED and \
         DECSEL erase protected cells (charproc.c, ReallyReset: protected_mode \
         = OFF_PROTECT). xterm gets DECSCA 0 after it, which turns the mode on \
         again.";
    };
    {
      name = "alt-1049-reset";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "resetting 1049 is resetting 1047, which erases the alternate screen \
         as it leaves it, then DECRC (vte.mli: the modes compose four steps); \
         xterm's 1049 reset switches back without erasing (ctlseqs: 'Use \
         Normal Screen Buffer and restore cursor'). xterm gets DECRST 1047 \
         first, on the alternate screen.";
    };
    {
      name = "lcf-protected-erase";
      kind = Deviation;
      how = "xterm rewrite";
      why =
        "DECSED and DECSEL are functions that change cells, so they clear the \
         Last Column Flag, whatever their range holds (RFC 0004 §2 Cursor); \
         xterm keeps it when every cell of the range is protected (util.c, \
         ClearInLine2 returns before ResetWrap). xterm gets CHA to the \
         cursor's column after them when the flag was set.";
    };
    {
      name = "decrc-origin-row";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "in origin mode the cursor stays in the scroll region, so DECRC brings \
         a saved row outside it into it (VT510 DECOM, DECRC; vte.mli); xterm \
         restores the row as saved, outside the region, where its CPR then \
         reports a negative row (cursor.c, CursorRestoreFlags). xterm gets CUP \
         to matrix's position after DECRC then.";
    };
    {
      name = "reverse-wrap-marks";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "with reverse-wrap, BS and CUB go back past the first column over the \
         rows autowrap continued, and an edit that moves a row's cells \
         sideways ends that row's mark (vte.mli, wrap marks); xterm keeps the \
         mark through SR, ICH and DECIC, and goes back over such a row \
         (screen.c, ScrnInsertChar leaves LineTstWrapped). When they go back \
         past the first column, xterm gets CUP to matrix's cursor instead.";
    };
    {
      name = "protect-mode-off";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "a protected character stays protected until erased by a non-selective \
         erase (VT220 Programmer Reference Manual §4.9; RFC 0004 §2 \
         Protection); a DECSED of the whole display that finds no protected \
         cell turns xterm's protection mode off, so the characters DECSCA \
         protects afterwards are erased by DECSED and DECSEL (util.c, \
         do_erase_display). xterm gets DECSCA with the pen's protection after \
         such a DECSED, which turns the mode on again.";
    };
    {
      name = "ris-alternate";
      kind = Quirk;
      how = "xterm rewrite";
      why =
        "RIS returns both screens to the power-up state (RFC 0004 §2 Resets); \
         xterm's RIS leaves the alternate screen's cells and saved cursor as \
         they were (charproc.c, ReallyReset saves the cursor of the normal \
         screen only). After RIS xterm gets the alternate screen cleared and \
         its cursor saved at home, as between cases.";
    };
    {
      name = "margin-cut-wide";
      kind = Quirk;
      how = "screen";
      why =
        "an edit that cuts a wide character leaves blanks (RFC 0004 §2 Blank); \
         xterm's ICH, SR and DECIC leave the first half of a wide character \
         they push across the right margin in the last column, where it has no \
         room (screen.c, ScrnInsertChar). That cell is not compared, wherever \
         it moves.";
    };
    {
      name = "dcs-requests";
      kind = Configuration;
      how = "replies";
      why =
        "a DCS this terminal does not implement has no effect and no answer \
         (RFC 0004 §2 Replies: the identity promises only what is \
         implemented); xterm answers its requests (DECRQSS, XTGETTCAP) with a \
         report or, when it cannot parse one, with CAN (ctlseqs). In a case \
         with a DCS, xterm's DCS reports and CAN replies are removed.";
    };
    {
      name = "media-copy";
      kind = Configuration;
      how = "generator";
      why =
        "media copy (MC, CSI Pm i) has no effect here, a terminal without a \
         printer; the harness reads xterm's screen through MC, so a case's own \
         MC would take the place of the harness's print, or turn printer \
         controller mode on (ctlseqs). It is not formed.";
    };
    {
      name = "vt-level";
      kind = Configuration;
      how = "generator";
      why =
        "xterm acts as the level its decTerminalID gives (charproc.c, \
         vtXX_level): below 200 it has no single shifts, LS2/LS3 or national \
         mode (DECNRCM), below 400 no DECIC/DECDC. They are not generated \
         there.";
    };
    {
      name = "xterm-escapes";
      kind = Configuration;
      how = "generator";
      why =
        "escape sequences xterm performs and matrix, a VT100 by its identity \
         (RFC 0004 §2 Replies), does not (ctlseqs): ISO protection (SPA, EPA: \
         ESC V, ESC W), HP memory lock (ESC l, ESC m), line attributes \
         (DECDHL, DECSWL, DECDWL: ESC # 3 to 6), ANSI conformance levels (ESC \
         SP L, M, N), the locking shifts into GR from level 2 (ESC ~, ESC }, \
         ESC |), DECBI and DECFI at level 4 (ESC 6, ESC 9). An ESC is not \
         followed by them.";
    };
    {
      name = "s8c1t";
      kind = Configuration;
      how = "generator";
      why =
        "matrix is a VT100 and replies with 7-bit controls only (RFC 0004 §3, \
         'A VT220 identity'); xterm above decTerminalID 100 switches to 8-bit \
         replies on S8C1T. ESC SP G is not generated there.";
    };
    {
      name = "g2-raw-bytes";
      kind = Quirk;
      how = "generator";
      why =
        "UTF-8 is decoded before character sets apply (RFC 0004 §1 Input); \
         once G2 holds a set other than ASCII, xterm takes every byte \
         0x80..0xFF as a GR character, without decoding UTF-8 (ptydata.c, \
         decodeUtf8, vt100_graphics; changelog, patch #407). After such a \
         designation, no non-ASCII byte is generated.";
    };
    {
      name = "ss-non-gl";
      kind = Quirk;
      how = "generator";
      why =
        "a single shift invokes G2 or G3 for the next character only (VT220 \
         Programmer Reference Manual ch. 2; vte.mli); xterm prints the \
         previous character again when anything but a GL character follows SS2 \
         or SS3, the case's end included, or when that previous character is \
         beyond U+00FF (observed in 398). Only printable ASCII follows a \
         single shift, and none follows such a character.";
    };
  ]

(* In the order the report lists them: deviations, quirks, configuration. *)
let all =
  let rank = function Deviation -> 0 | Quirk -> 1 | Configuration -> 2 in
  List.stable_sort (fun a b -> compare (rank a.kind) (rank b.kind)) registry

let names = List.map (fun i -> i.name) all

(* {1 Context} *)

type ctx = {
  decid : int;
  rows : int;
  cols : int;
  width : int -> int;  (** xterm's width of a scalar: 0, 1 or 2. *)
  enabled : string -> bool;
  mutable fired : string list;
}

let fire ctx name =
  assert (List.mem name names);
  if not (List.mem name ctx.fired) then ctx.fired <- name :: ctx.fired

let on ctx name =
  assert (List.mem name names);
  ctx.enabled name

(* {1 UTF-8}

   [decode s i] is the scalar at [i], or [-1] for an ill-formed subsequence,
   with its length: the maximal subpart of Unicode §3.9, Table 3-7. *)

let decode s i =
  let n = String.length s in
  let b k = if i + k < n then Char.code s.[i + k] else -1 in
  let ok k lo hi =
    let c = b k in
    c >= lo && c <= hi
  in
  let c0 = b 0 in
  let tail k = b k land 0x3F in
  if c0 < 0x80 then (c0, 1)
  else if c0 >= 0xC2 && c0 <= 0xDF then
    if ok 1 0x80 0xBF then (((c0 land 0x1F) lsl 6) lor tail 1, 2) else (-1, 1)
  else if c0 >= 0xE0 && c0 <= 0xEF then
    let lo, hi =
      if c0 = 0xE0 then (0xA0, 0xBF)
      else if c0 = 0xED then (0x80, 0x9F)
      else (0x80, 0xBF)
    in
    if not (ok 1 lo hi) then (-1, 1)
    else if not (ok 2 0x80 0xBF) then (-1, 2)
    else (((c0 land 0x0F) lsl 12) lor (tail 1 lsl 6) lor tail 2, 3)
  else if c0 >= 0xF0 && c0 <= 0xF4 then
    let lo, hi =
      if c0 = 0xF0 then (0x90, 0xBF)
      else if c0 = 0xF4 then (0x80, 0x8F)
      else (0x80, 0xBF)
    in
    if not (ok 1 lo hi) then (-1, 1)
    else if not (ok 2 0x80 0xBF) then (-1, 2)
    else if not (ok 3 0x80 0xBF) then (-1, 3)
    else
      ( ((c0 land 0x07) lsl 18) lor (tail 1 lsl 12) lor (tail 2 lsl 6) lor tail 3,
        4 )
  else (-1, 1)

(* Whether [sub] occurs in [s], ending after [ending_after]. *)
let occurs ?(ending_after = -1) s sub =
  let n = String.length s and m = String.length sub in
  let rec matches i j = j = m || (s.[i + j] = sub.[j] && matches i (j + 1)) in
  let rec go i = i + m <= n && (matches i 0 || go (i + 1)) in
  go (max 0 (ending_after - m + 1))

let utf8 cp =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int cp);
  Buffer.contents b

let replacement = utf8 0xFFFD

(* lone-continuation: the bytes xterm sees, with each lone continuation byte
   that xterm would take raw (followed by 0x21..0x7F) made U+FFFD. Matrix
   decodes both strings the same way. *)
let lone_continuations ctx s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then (
      let cp, len = decode s i in
      let c = Char.code s.[i] in
      if
        cp < 0 && len = 1 && c >= 0x80 && c <= 0xBF
        && i + 1 < n
        && Char.code s.[i + 1] > 0x20
        && Char.code s.[i + 1] < 0x80
        && on ctx "lone-continuation"
      then (
        fire ctx "lone-continuation";
        Buffer.add_string b replacement)
      else Buffer.add_string b (String.sub s i len);
      go (i + len))
  in
  go 0;
  Buffer.contents b

(* {1 Lexing}

   The input in lexical units, as matrix's parser reads it (Ansi.Parser):
   text, a C0 control, an escape or control sequence, a string, or bytes that
   form no token (a sequence ended by ESC or a non-ASCII character, a lone
   ST, a dropped C1). Units that form no token do not end the unit in
   progress, so REP still sees the text before them. *)

type scalar = { cp : int; bytes : string }

type lexeme =
  | Text of scalar list
  | Ctl of char
  | Esc of { inter : string; final : char; raw : string }
  | Csi of {
      marker : string;
      params : string;
      inter : string;
      final : char;
      raw : string;
      clean : bool;  (** No C0 control inside. *)
    }
  | Str of string
  | Silent of string  (** No token. *)
  | Overlong of string  (** A control sequence of more than 256 fields. *)

let lex s =
  let n = String.length s in
  let out = ref [] in
  let push l = out := l :: !out in
  (* A C0 control inside an escape or control sequence is performed where it
     occurs and the sequence goes on, so it is the same as the control
     before the sequence; DEL there is dropped. The units of a sequence are
     made so: its controls first, then the sequence without them. *)
  let open_string = function
    | (Str raw | Silent raw) :: _
      when String.length raw > 1 && String.contains "P]_^X" raw.[1] ->
        not
          (String.ends_with ~suffix:"\027\\" raw
          || (raw.[1] = ']' && String.ends_with ~suffix:"\x07" raw))
    | _ -> false
  in
  let hoist raw =
    (* Not after a string this sequence's ESC ends: there the controls would
       move into the string. *)
    if open_string !out then raw
    else
      let b = Buffer.create (String.length raw) in
      Buffer.add_char b raw.[0];
      String.iteri
        (fun i c ->
          if i > 0 then
            if c < '\x20' then push (Ctl c)
            else if c <> '\x7F' then Buffer.add_char b c)
        raw;
      Buffer.contents b
  in
  let text = ref [] in
  let flush_text () =
    if !text <> [] then (
      push (Text (List.rev !text));
      text := [])
  in
  let sub i j = String.sub s i (j - i) in
  (* The end of a string starting at [i] (after its opening): the index after
     its terminator, and whether ESC, CAN or SUB that is not part of it ended
     it (they are left to be read next). *)
  let rec string_end ~osc i =
    if i >= n then n
    else
      match s.[i] with
      | '\027' when i + 1 < n && s.[i + 1] = '\\' -> i + 2
      | '\027' | '\x18' | '\x1a' -> i
      | '\x07' when osc -> i + 1
      | _ ->
          let _, len = decode s i in
          string_end ~osc (i + len)
  in
  (* A sequence from [start] (its ESC); [i] is after ESC. *)
  let rec escape start i inter =
    if i >= n then push (Silent (hoist (sub start n)))
    else
      let c = s.[i] in
      match c with
      | '\027' | '\x18' | '\x1a' ->
          push (Silent (hoist (sub start i)));
          ground i
      | '[' when inter = "" -> csi start (i + 1) (Buffer.create 8) ""
      | (']' | 'P' | '_' | '^' | 'X') when inter = "" ->
          let e = string_end ~osc:(c = ']') (i + 1) in
          (* A DCS whose header is malformed (a non-ASCII character, a
             marker after a parameter, a parameter after an intermediate)
             forms no token. *)
          let content_end =
            if e - 2 > i && s.[e - 2] = '\027' && s.[e - 1] = '\\' then e - 2
            else e
          in
          (* A header with no final byte forms no token either. *)
          let rec header j ~params ~inter =
            if j >= content_end then true
            else
              match s.[j] with
              | '\x40' .. '\x7E' -> false
              | '\x30' .. '\x3F' as b ->
                  inter
                  || (params && b >= '<' && b <= '?')
                  || header (j + 1) ~params:true ~inter
              | '\x20' .. '\x2F' -> header (j + 1) ~params ~inter:true
              | '\x00' .. '\x1F' | '\x7F' -> header (j + 1) ~params ~inter
              | _ -> true
          in
          if c = 'P' && header (i + 1) ~params:false ~inter:false then
            push (Silent (sub start e))
          else push (Str (sub start e));
          ground e
      | '\x20' .. '\x2F' -> escape start (i + 1) (inter ^ String.make 1 c)
      | '\x30' .. '\x7E' ->
          let raw = hoist (sub start (i + 1)) in
          if inter = "" && c = '\\' then push (Silent raw)
          else push (Esc { inter; final = c; raw });
          ground (i + 1)
      | '\x00' .. '\x1F' | '\x7F' -> escape start (i + 1) inter
      | _ ->
          let _, len = decode s i in
          push (Silent (hoist (sub start (i + len))));
          ground (i + len)
  and csi ?(bad = false) start i params inter =
    if i >= n then push (Silent (hoist (sub start n)))
    else
      let c = s.[i] in
      match c with
      | '\027' | '\x18' | '\x1a' ->
          push (Silent (hoist (sub start i)));
          ground i
      | '\x30' .. '\x3F' when inter = "" ->
          Buffer.add_char params c;
          csi ~bad start (i + 1) params inter
      | '\x20' .. '\x2F' ->
          csi ~bad start (i + 1) params (inter ^ String.make 1 c)
      | '\x40' .. '\x7E' ->
          let raw = hoist (sub start (i + 1)) in
          let p = Buffer.contents params in
          let marker, params =
            if p <> "" && p.[0] >= '<' && p.[0] <= '?' then
              (String.make 1 p.[0], String.sub p 1 (String.length p - 1))
            else ("", p)
          in
          let fields =
            1
            + String.fold_left
                (fun k c -> if c = ';' || c = ':' then k + 1 else k)
                0 params
          in
          (* Malformed: a marker after a parameter, a parameter after an
             intermediate, a fifth intermediate, a 257th field. *)
          if
            bad
            || String.exists (fun c -> c >= '<' && c <= '?') params
            || String.length inter > 4
          then push (Silent raw)
          else if fields > 256 then push (Overlong raw)
          else
            push
              (Csi
                 {
                   marker;
                   params;
                   inter;
                   final = c;
                   raw;
                   clean =
                     not
                       (String.exists
                          (fun c -> c < '\x20' || c = '\x7F')
                          (String.sub raw 2 (String.length raw - 2)));
                 });
          ground (i + 1)
      | '\x30' .. '\x3F' -> csi ~bad:true start (i + 1) params inter
      | '\x00' .. '\x1F' | '\x7F' -> csi ~bad start (i + 1) params inter
      | _ ->
          let _, len = decode s i in
          push (Silent (hoist (sub start (i + len))));
          ground (i + len)
  and ground i =
    if i < n then (
      match s.[i] with
      | '\027' ->
          flush_text ();
          escape i (i + 1) ""
      | ('\x00' .. '\x1F' | '\x7F') as c ->
          flush_text ();
          push (Ctl c);
          ground (i + 1)
      | _ ->
          let cp, len = decode s i in
          text := { cp; bytes = sub i (i + len) } :: !text;
          ground (i + len))
    else flush_text ()
  in
  ground 0;
  List.rev !out

let is_c1 cp = cp >= 0x80 && cp <= 0x9F

(* The format characters xterm discards: the zero-width ones its
   character-class table puts in CNTRL. Others, such as U+0600 or U+110BD, it
   attaches to a cell as combining characters. *)
let is_format cp =
  (cp >= 0x200B && cp <= 0x200F)
  || (cp >= 0x202A && cp <= 0x202E)
  || (cp >= 0x2060 && cp <= 0x206F)
  || cp = 0xFEFF

let scalars s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let d = String.get_utf_8_uchar s i in
      go
        (i + Uchar.utf_decode_length d)
        (Uchar.to_int (Uchar.utf_decode_uchar d) :: acc)
  in
  go 0 []

let first_scalar s = match scalars s with cp :: _ -> cp | [] -> 0

(* {1 The xterm rewrite} *)

type rewrite = {
  xterm : string;  (** What xterm gets. *)
  orphans : int list;
      (** The zero-width characters of units that continue nothing and joined a
          blank. *)
  erased : (int * int) list;
      (** The cells, [(row, col)], that a selective erase blanked. *)
  cut : (int * int) list;  (** The blanks left by a wide character cut. *)
  lost : (int * int) list;
      (** The cells whose character xterm loses (delete-wide-stale). *)
}

(* A control sequence other than SGR with sub-parameters, which the VTE
   gives no effect: it is transparent, as one the parser rejects. *)
let with_subparams = function
  | Csi { marker = ""; inter = ""; final = 'm'; _ } -> false
  | Csi { params; _ } -> String.contains params ':'
  | _ -> false

(* The parameters of a CSI as matrix reads them for a count (Params.pn): the
   first field, absent or 0 meaning 1, saturating. *)
let count params =
  match String.split_on_char ';' params with
  | [] -> 1
  | first :: _ -> (
      let f = List.hd (String.split_on_char ':' first) in
      if f = "" then 1
      else
        match int_of_string_opt f with
        | Some 0 -> 1
        | Some v -> v
        | None -> max_int)

(* xterm's width of a scalar of the input, U+FFFD for an ill-formed one. *)
let width ctx sc = if sc.cp < 0 then ctx.width 0xFFFD else ctx.width sc.cp

(* The tags that follow cells (see [tags] below). *)
let erased_tag = "\u{E000}"
let cut_tag = "\u{E001}"
let lost_tag = "\u{E002}"

let rewrite ctx bytes =
  let bytes = lone_continuations ctx bytes in
  let m =
    Vte_adapter.create ~width_method:`Wcwidth ~rows:ctx.rows ~cols:ctx.cols ()
  in
  (* [tags] follows the cells a selective erase blanked, as they move with
     the screen: it is fed the same bytes, and such cells hold a tag. *)
  let tags =
    Vte_adapter.create ~width_method:`Wcwidth ~rows:ctx.rows ~cols:ctx.cols ()
  in
  let feed s =
    ignore (Vte_adapter.feed m s : string);
    ignore (Vte_adapter.feed tags s : string)
  in
  let out = Buffer.create (String.length bytes * 2) in
  let emit = Buffer.add_string out in
  let orphans = ref [] in
  let cha () = Printf.sprintf "\027[%dG" ((Vte_adapter.cursor m).col + 1) in
  let last = ref (Ctl '\x00') in
  (* A lexeme xterm does not get: if its ESC ended a string, xterm gets a
     sequence that does nothing instead, which ends the string as well. *)
  let drop () =
    let unfinished raw =
      let n = String.length raw in
      let b = raw.[n - 1] in
      if n >= 2 && String.contains "P]_^X" raw.[1] then
        not (String.ends_with ~suffix:"\027\\" raw)
      else if b >= '\x80' then false
      else if n >= 2 && raw.[1] = '[' then n = 2 || b < '\x40' || b > '\x7E'
      else n = 1 || b < '\x30' || b > '\x7E'
    in
    match !last with
    | Str raw
      when not
             (String.ends_with ~suffix:"\027\\" raw
             || (raw.[1] = ']' && String.ends_with ~suffix:"\x07" raw)) ->
        emit "\027[?0l"
    | Silent raw when unfinished raw -> emit "\027[?0l"
    | _ -> ()
  in
  (* The bytes [m] got, to replay them. *)
  let seen = Buffer.create (String.length bytes) in
  let feed s =
    Buffer.add_string seen s;
    feed s
  in
  (* decrc-sgr-background: after DECRC xterm gets the pen's background again,
     read from where a character printed after the bytes so far lands. *)
  (* The pen: the cell a character printed after the bytes so far gets. *)
  let pen () =
    let probe =
      Vte_adapter.create ~width_method:`Wcwidth ~rows:ctx.rows ~cols:ctx.cols ()
    in
    ignore (Vte_adapter.feed probe (Buffer.contents seen) : string);
    ignore (Vte_adapter.feed probe "\027[?7l" : string);
    let c = Vte_adapter.cursor probe in
    ignore (Vte_adapter.feed probe "x" : string);
    ( (Vte_adapter.screen probe).lines.(c.row).(c.col),
      Vte_adapter.protected probe ~row:c.row ~col:c.col )
  in
  let restore_background () =
    if on ctx "decrc-sgr-background" then begin
      fire ctx "decrc-sgr-background";
      let bg = (fst (pen ())).bg in
      emit
        (match bg with
        | Screen.Default -> "\027[49m"
        | Index i when i < 8 -> Printf.sprintf "\027[%dm" (40 + i)
        | Index i when i < 16 -> Printf.sprintf "\027[%dm" (100 + i - 8)
        | Index i -> Printf.sprintf "\027[48;5;%dm" i
        | Rgb (r, g, b) -> Printf.sprintf "\027[48;2;%d;%d;%dm" r g b)
    end
  in
  (* The scroll region, as DECSTBM sets it, and RIS, DECSTR and DECALN reset
     it. *)
  let margins = ref (0, ctx.rows - 1) in
  (* orphan-half: an erase or deletion that starts on a blank a cut left,
     where xterm keeps the second half of the wide character, blanks the cell
     before it there. *)
  let touch_cut ~row ~col =
    if
      col > 0
      && (Vte_adapter.cell tags ~row ~col).text = cut_tag
      && on ctx "orphan-half"
    then (
      fire ctx "orphan-half";
      Some (row, col - 1))
    else None
  in
  let lose = function
    | Some cell -> Vte_adapter.tag tags ~protected:true ~text:lost_tag [ cell ]
    | None -> ()
  in
  (* reverse-wrap-origin: BS or CUB from the first column of the first row
     with reverse-wrap, which moves nowhere. *)
  let reverse_wrap_origin n =
    let c = Vte_adapter.cursor m in
    c.row = 0 && c.reverse_wrap && c.auto_wrap
    && (if c.last_column then n - 1 else n) > c.col
    && on ctx "reverse-wrap-origin"
  in
  (* reverse-wrap-marks: BS or CUB that go back past the first column of a
     row below the first, over the wrap marks. *)
  let reverse_wrap_crossing n =
    let c = Vte_adapter.cursor m in
    c.row > 0 && c.reverse_wrap && c.auto_wrap
    && (if c.last_column then n - 1 else n) > c.col
    && on ctx "reverse-wrap-marks"
  in
  (* CUP to matrix's cursor, whatever the origin mode. *)
  let cup () =
    let c = Vte_adapter.cursor m and top, _ = !margins in
    let row = if (Vte_adapter.modes m).origin then c.row - top else c.row in
    Printf.sprintf "\027[%d;%dH" (row + 1) (c.col + 1)
  in
  (* decsc-slots: the screens, primary then alternate, whose DECSC slot
     matrix has returned to the power-up state and xterm has not. *)
  let stale = [| false; false |] in
  (* decrc-origin-row: the row each screen's DECSC slot holds. *)
  let saved_rows = [| 0; 0 |] in
  let screen_index () = if (Vte_adapter.cursor m).alternate then 1 else 0 in
  let saved () =
    stale.(screen_index ()) <- false;
    saved_rows.(screen_index ()) <- (Vte_adapter.cursor m).row
  in
  let restored () =
    let i = screen_index () in
    (* In origin mode, matrix's DECRC brings a row outside the region into
       it; xterm restores the row as it was. *)
    (let c = Vte_adapter.cursor m and top, bottom = !margins in
     let saved = saved_rows.(i) in
     if
       (Vte_adapter.modes m).origin
       && (saved < top || saved > bottom)
       && (not c.last_column) && on ctx "decrc-origin-row"
     then (
       fire ctx "decrc-origin-row";
       emit (Printf.sprintf "\027[%d;%dH" (c.row - top + 1) (c.col + 1))));
    if stale.(i) && on ctx "decsc-slots" then (
      fire ctx "decsc-slots";
      emit "\027[?6l\027[H\027[0m\027[0\"q\027(B\027)B\027*B\027+B\x0f")
  in
  (* autowrap-off-flag: with autowrap off, xterm still sets its wrap flag
     at the last column, and acts on it once autowrap is set again; CHA to
     the cursor's column clears it. *)
  let autowrap_on () =
    let c = Vte_adapter.cursor m in
    if (not c.auto_wrap) && c.col = c.cols - 1 && on ctx "autowrap-off-flag"
    then (
      fire ctx "autowrap-off-flag";
      emit (cha ()))
  in
  (* One scalar of text, with what xterm must get before it. *)
  let pending_lost = ref None in
  let wide_seen = ref false in
  (* [m] gets [bytes], which print a character of width [w]; the cells it
     cuts and the cells xterm loses are tagged. *)
  let place ~w bytes =
    if w = 2 then wide_seen := true;
    (* cut-wide: the half of a wide character this one cuts. *)
    let c = Vte_adapter.cursor m in
    (* Where the character lands: after the wrap the Last Column Flag or its
       width asks for, which scrolls at the bottom margin (no cut then). *)
    let target =
      if w = 0 then None
      else
        let wraps =
          c.auto_wrap && (c.last_column || (c.col + w > c.cols && c.cols >= w))
        in
        if wraps then
          let _, bottom = !margins in
          if c.row = bottom then None
          else Some ((if c.row = c.rows - 1 then c.row else c.row + 1), 0)
        else
          Some (c.row, if c.col + w > c.cols then max 0 (c.cols - w) else c.col)
    in
    let placed = target <> None in
    let row, col = Option.value target ~default:(c.row, c.col) in
    let cuts =
      if not placed then []
      else
        let cont x =
          x >= 0 && x < c.cols && (Vte_adapter.cell m ~row ~col:x).cont
        in
        (if cont col then [ (row, col - 1) ] else [])
        @ if cont (col + w) then [ (row, col + w) ] else []
    in
    (* orphan-half: writing over a blank a cut left, where xterm keeps the
       second half of the wide character, xterm blanks the cell before it. *)
    let lost =
      if not placed then []
      else
        List.filter_map
          (fun d ->
            if
              d > 0 && d < c.cols
              && (Vte_adapter.cell tags ~row ~col:d).text = cut_tag
              && (d - 1 < col || d - 1 >= col + w)
            then Some (row, d - 1)
            else None)
          (List.init w (fun i -> col + i))
    in
    let insert_push =
      if placed && c.insert && w > 0 && on ctx "margin-cut-wide" then
        let x = c.cols - 1 - w in
        x >= col && (Vte_adapter.cell m ~row ~col:(x + 1)).cont
      else false
    in
    feed bytes;
    lose !pending_lost;
    pending_lost := None;
    if insert_push then (
      fire ctx "margin-cut-wide";
      Vte_adapter.tag tags ~protected:true ~text:lost_tag [ (row, c.cols - 1) ]);
    if lost <> [] && on ctx "orphan-half" then (
      fire ctx "orphan-half";
      Vte_adapter.tag tags ~protected:true ~text:lost_tag lost);
    let cuts =
      List.filter
        (fun (row, col) -> (Vte_adapter.cell m ~row ~col).text = " ")
        cuts
    in
    if cuts <> [] then Vte_adapter.tag tags ~text:cut_tag cuts
  in
  let print sc =
    if is_c1 sc.cp && on ctx "c1-utf8" then fire ctx "c1-utf8"
    else begin
      if width ctx sc = 2 then begin
        let c = Vte_adapter.cursor m in
        if c.cols >= 2 && c.col = c.cols - 1 && not c.last_column then
          if c.auto_wrap then (
            if on ctx "wrap-erase" then (
              fire ctx "wrap-erase";
              pending_lost := touch_cut ~row:c.row ~col:c.col;
              emit "\027[K"))
          else if on ctx "autowrap-off-wide" then (
            fire ctx "autowrap-off-wide";
            emit "\027[D")
      end;
      emit sc.bytes
    end;
    (* An ill-formed subsequence is one U+FFFD, which [m] gets whole: fed
       alone, its bytes would leave a character pending. *)
    let bytes = if sc.cp < 0 then replacement else sc.bytes in
    place ~w:(if is_c1 sc.cp then 0 else width ctx sc) bytes
  in
  (* The last unit of a text, as wcwidth cuts it: from its last character of
     positive width. *)
  let last_unit scalars =
    let rec go acc = function
      | [] -> None
      | sc :: rest ->
          if is_c1 sc.cp then go acc rest
          else if width ctx sc > 0 then Some (sc :: acc)
          else go (sc :: acc) rest
    in
    go [] (List.rev scalars)
  in
  let crossed = ref false and joined = ref false in
  let rep ~raw ~params ~before =
    match Option.bind before last_unit with
    | None when before <> None && on ctx "zero-width-join" ->
        (* After a unit of no width, which joined a cell, REP repeats
           nothing; xterm repeats the last character it printed. *)
        fire ctx "zero-width-join";
        drop ();
        feed raw
    | None when on ctx "rep-after-control" ->
        (* After a control function REP repeats nothing (ECMA-48 leaves it
           undefined); xterm repeats its last character after some. *)
        fire ctx "rep-after-control";
        drop ();
        feed raw
    | None ->
        emit raw;
        feed raw
    | Some unit ->
        let n = count params in
        let w = max 1 (min ctx.cols (width ctx (List.hd unit))) in
        let reduced = n > 65535 && on ctx "rep-max" in
        let n =
          if reduced then (
            fire ctx "rep-max";
            let per_row = max 1 (ctx.cols / w) in
            let base = (ctx.rows + 2) * per_row in
            base + ((n - base) mod per_row))
          else n
        in
        let multi =
          List.length (List.filter (fun sc -> not (is_c1 sc.cp)) unit) > 1
        in
        let crossing = !crossed && on ctx "rep-after-control" in
        (* Written out too when a wide character came before, so that the
           cuts each copy makes are followed. *)
        if (multi && on ctx "rep-cluster") || w = 2 || crossing || !wide_seen
        then begin
          if multi then fire ctx "rep-cluster";
          (* After bytes that form no token, REP repeats the unit; xterm
             repeats it after some and not after others. *)
          if crossing then fire ctx "rep-after-control";
          drop ();
          (* REP's ESC ends a sequence left open before it, in [m] too. *)
          feed "\027[?0l";
          (* Written out: each copy a unit of its own, as REP makes it. *)
          for _ = 1 to n do
            List.iter print unit
          done;
          (* REP is a control function, which ends the unit in progress:
             [m], which got the copies instead, gets one too. *)
          feed "\027[?0l"
        end
        else (
          emit (if reduced then Printf.sprintf "\027[%db" n else raw);
          feed raw)
  in
  let previous = ref None in
  List.iter
    (fun l ->
      let before = !previous in
      (previous :=
         match l with
         | Text scalars -> (
             (* A text after text continues it. *)
             match before with
             | Some t -> Some (t @ scalars)
             | None -> Some scalars)
         | Silent raw
           when String.length raw > 1
                && (not (String.contains "P]_^X" raw.[1]))
                && String.exists
                     (fun c -> c < '\x20' && c <> '\027')
                     (String.sub raw 1 (String.length raw - 1)) ->
             (* A C0 control left inside a sequence is performed. *)
             None
         | Silent _ | Overlong _ -> before
         | l when with_subparams l -> before
         | _ -> None);
      let lexeme = l in
      (* After each unit: [crossed], the unit in progress went on through
         bytes that form no token; [joined], it is a unit of no width that
         joined a cell. *)
      let after () =
        last := lexeme;
        match lexeme with
        | Text _ -> crossed := false
        | Silent _ | Overlong _ -> if before <> None then crossed := true
        | l when with_subparams l -> if before <> None then crossed := true
        | _ ->
            crossed := false;
            joined := false
      in
      Fun.protect ~finally:after @@ fun () ->
      match l with
      | Text scalars ->
          (* Zero-width characters at the start of a text that continues
             nothing form a unit that joins the cell before the cursor, or the
             cursor's with the Last Column Flag, or nothing at the first
             column. xterm gets them moved to that cell. A blank cell gets
             them in matrix and not in xterm, whose cell may hold no
             character; they are removed from blanks. *)
          let rec split run = function
            | sc :: rest when is_c1 sc.cp || width ctx sc = 0 ->
                split (sc :: run) rest
            | rest -> (List.rev run, rest)
          in
          let run, rest =
            if before = None || !joined then split [] scalars else ([], scalars)
          in
          joined := run <> [] && rest = [];
          let marks = List.filter (fun sc -> not (is_c1 sc.cp)) run in
          if marks <> [] && on ctx "zero-width-join" then begin
            fire ctx "zero-width-join";
            let c = Vte_adapter.cursor m in
            let text = String.concat "" (List.map (fun sc -> sc.bytes) marks) in
            if c.last_column then
              if
                (* The cursor's cell, which xterm last wrote, unless a screen
                 switch came between. *)
                first_scalar (Vte_adapter.cell m ~row:c.row ~col:c.col).text
                = 0x20
              then orphans := List.map (fun sc -> sc.cp) marks @ !orphans
              else emit text
            else if c.col > 0 then begin
              let screen = Vte_adapter.screen m in
              let line = screen.lines.(c.row) in
              let x = ref (c.col - 1) in
              while !x > 0 && line.(!x).cont do
                decr x
              done;
              if first_scalar line.(!x).text = 0x20 then
                orphans := List.map (fun sc -> sc.cp) marks @ !orphans
              else
                let k = c.col - !x in
                emit (Printf.sprintf "\027[%dD%s\027[%dC" k text k)
            end;
            List.iter
              (fun sc ->
                if is_c1 sc.cp then
                  if on ctx "c1-utf8" then fire ctx "c1-utf8" else emit sc.bytes)
              run;
            feed (String.concat "" (List.map (fun sc -> sc.bytes) run))
          end
          else List.iter print run;
          List.iter print rest
      | Ctl '\x18' when ctx.decid >= 100 && ctx.decid < 200 ->
          if on ctx "can-checkerboard" then (
            fire ctx "can-checkerboard";
            emit "\027\\")
          else emit "\x18";
          feed "\x18"
      | Ctl '\x08' when reverse_wrap_origin 1 ->
          fire ctx "reverse-wrap-origin";
          emit "\027[1G";
          feed "\x08"
      | Ctl '\x08' when reverse_wrap_crossing 1 ->
          fire ctx "reverse-wrap-marks";
          feed "\x08";
          emit (cup ())
      | Ctl '\x1a' ->
          (* SUB prints its error character, which may cut a wide one. *)
          emit "\x1a";
          place ~w:1 "\x1a"
      | Ctl c ->
          let s = String.make 1 c in
          emit s;
          feed s
      | Esc { inter = " "; final = 'F' | 'G'; raw } when ctx.decid < 200 ->
          if on ctx "s7c1t-vt100" then (
            fire ctx "s7c1t-vt100";
            drop ())
          else emit raw;
          feed raw
      | Esc { inter = "#"; final = '8'; raw } ->
          margins := (0, ctx.rows - 1);
          if on ctx "decaln-pen" then (
            fire ctx "decaln-pen";
            emit "\027[39;49m");
          emit raw;
          feed raw
      | Esc { inter; final; raw }
        when String.length inter >= 1
             && String.contains "()*+" inter.[0]
             && (String.length inter > 1
                || not (String.contains "0A4C5RfQ9KYE6`ZH7=B" final))
             && on ctx "unknown-charsets" ->
          (* A set without a table here designates nothing. *)
          fire ctx "unknown-charsets";
          emit ("\027" ^ String.make 1 inter.[0] ^ "B");
          feed raw
      | Esc { inter = ""; final = '8'; raw } ->
          emit raw;
          feed raw;
          restored ();
          restore_background ()
      | Esc { inter = ""; final = '7'; raw } ->
          emit raw;
          feed raw;
          saved ()
      | Esc { inter = ""; final = 'c'; raw } ->
          emit raw;
          feed raw;
          (* xterm's RIS resets neither the alternate screen nor its slot:
             it gets them cleared, as the harness does between cases. *)
          if on ctx "ris-alternate" then (
            fire ctx "ris-alternate";
            emit "\027[?47h\027[2J\0277\027[?47l")
          else stale.(1) <- true;
          saved_rows.(0) <- 0;
          saved_rows.(1) <- 0;
          margins := (0, ctx.rows - 1)
      | Esc { raw; _ } ->
          emit raw;
          feed raw
      | l when with_subparams l ->
          let raw = match l with Csi { raw; _ } -> raw | _ -> assert false in
          if on ctx "subparams-default" then (
            fire ctx "subparams-default";
            drop ())
          else emit raw;
          feed raw
      | Csi
          {
            marker = "?";
            inter = "";
            final = ('J' | 'K') as final;
            params;
            raw;
            clean = true;
          } ->
          (* Selective erase: the cells it blanks keep their rendition here,
             and take xterm's blank there. *)
          if on ctx "selective-erase" then begin
            let c = Vte_adapter.cursor m in
            let p =
              match int_of_string_opt params with
              | Some p -> p
              | None -> if params = "" then 0 else -1
            in
            let row r a b =
              List.init (max 0 (b - a + 1)) (fun i -> (r, a + i))
            in
            let rows a b =
              List.concat
                (List.init
                   (max 0 (b - a + 1))
                   (fun i -> row (a + i) 0 (c.cols - 1)))
            in
            let cells =
              match (final, p) with
              | 'J', 0 ->
                  row c.row c.col (c.cols - 1) @ rows (c.row + 1) (c.rows - 1)
              | 'J', 1 -> rows 0 (c.row - 1) @ row c.row 0 c.col
              | 'J', 2 -> rows 0 (c.rows - 1)
              | 'K', 0 -> row c.row c.col (c.cols - 1)
              | 'K', 1 -> row c.row 0 c.col
              | 'K', 2 -> row c.row 0 (c.cols - 1)
              | _ -> []
            in
            let cells =
              List.filter
                (fun (row, col) -> not (Vte_adapter.protected m ~row ~col))
                cells
            in
            let before = Vte_adapter.screen m in
            let touched =
              if p = 0 then touch_cut ~row:c.row ~col:c.col else None
            in
            emit raw;
            feed raw;
            lose touched;
            if
              c.last_column
              && (not (Vte_adapter.cursor m).last_column)
              && on ctx "lcf-protected-erase"
            then (
              fire ctx "lcf-protected-erase";
              emit (cha ()));
            (* protect-mode-off: after a DECSED of the whole display. *)
            if
              final = 'J'
              && (p = 2
                 || (p = 0 && c.row = 0 && c.col = 0)
                 || (p = 1 && c.row = c.rows - 1 && c.col = c.cols - 1))
              && on ctx "protect-mode-off"
            then (
              fire ctx "protect-mode-off";
              emit (if snd (pen ()) then "\027[1\"q" else "\027[0\"q"));
            let after = Vte_adapter.screen m in
            (* The range, and the halves of wide characters its edges cut. *)
            let blanked = ref [] in
            Array.iteri
              (fun r (l : Screen.cell array) ->
                Array.iteri
                  (fun col (cell : Screen.cell) ->
                    if cell.text = " " && before.lines.(r).(col).text <> " "
                    then blanked := (r, col) :: !blanked)
                  l)
              after.lines;
            let cells =
              List.sort_uniq compare
                (!blanked
                @ List.filter
                    (fun (row, col) -> after.lines.(row).(col).text = " ")
                    cells)
            in
            (* A cell already lost stays lost. *)
            let cells =
              List.filter
                (fun (row, col) ->
                  (Vte_adapter.cell tags ~row ~col).text <> lost_tag)
                cells
            in
            if cells <> [] then (
              fire ctx "selective-erase";
              Vte_adapter.tag tags ~text:erased_tag cells)
          end
          else (
            emit raw;
            feed raw)
      | Csi { marker = ""; inter = ""; final = 'b'; params; raw; clean = true }
        ->
          rep ~raw ~params ~before
      | Csi { marker = ""; inter = ""; final = 'u'; params = ""; raw; _ } ->
          emit raw;
          feed raw;
          restored ();
          restore_background ()
      | Csi { marker = ""; inter = ""; final = 's'; params = ""; raw; _ } ->
          emit raw;
          feed raw;
          saved ()
      | Csi
          {
            marker = "?";
            inter = "";
            final = 'l';
            params = ("1048" | "1049") as params;
            raw;
            _;
          } ->
          if
            params = "1049" && (Vte_adapter.cursor m).alternate
            && on ctx "alt-1049-reset"
          then (
            fire ctx "alt-1049-reset";
            emit "\027[?1047l");
          emit raw;
          feed raw;
          restored ();
          restore_background ()
      | Csi { marker = "?"; inter = ""; final = 'h'; params = "1048"; raw; _ }
        ->
          emit raw;
          feed raw;
          saved ()
      | Csi { marker = "?"; inter = ""; final = 'h'; params = "1049"; raw; _ }
        ->
          (* DECSC on the screen in use, then the switch. *)
          saved ();
          emit raw;
          feed raw
      | Csi { marker = "?"; inter = ""; final = 'h'; params = "7"; raw; _ } ->
          autowrap_on ();
          emit raw;
          feed raw
      | Csi { marker = ""; inter = ""; final = 'D'; raw; clean = true; params }
        when reverse_wrap_origin (count params) ->
          fire ctx "reverse-wrap-origin";
          emit "\027[1G";
          feed raw
      | Csi { marker = ""; inter = ""; final = 'D'; raw; clean = true; params }
        when reverse_wrap_crossing (count params) ->
          fire ctx "reverse-wrap-marks";
          drop ();
          feed raw;
          emit (cup ())
      | Csi { marker = ""; inter = ""; final = 'D'; params; raw; clean = true }
        when let c = Vte_adapter.cursor m in
             c.last_column && c.reverse_wrap && c.auto_wrap
             && count params >= 2
             && on ctx "reverse-wrap-lcf" ->
          fire ctx "reverse-wrap-lcf";
          emit (Printf.sprintf "\027[%dD" (min (count params) 65535 + 1));
          feed raw
      | Csi { marker = ""; inter = ""; final = 'r'; params; raw; clean = true }
        ->
          (let p i =
             match List.nth_opt (String.split_on_char ';' params) i with
             | None | Some "" -> None
             | Some f -> (
                 let f = List.hd (String.split_on_char ':' f) in
                 match int_of_string_opt f with
                 | Some 0 -> None
                 | Some v -> Some v
                 | None -> if f = "" then None else Some max_int)
           in
           let top = min ctx.rows (Option.value (p 0) ~default:1)
           and bottom = min ctx.rows (Option.value (p 1) ~default:ctx.rows) in
           if top < bottom && not (String.contains params ':') then
             margins := (top - 1, bottom - 1));
          emit raw;
          feed raw
      | Csi { marker = ""; inter = "!"; final = 'p'; raw; _ } ->
          margins := (0, ctx.rows - 1);
          stale.(0) <- true;
          stale.(1) <- true;
          saved_rows.(0) <- 0;
          saved_rows.(1) <- 0;
          autowrap_on ();
          if on ctx "decstr-rendition" then (
            fire ctx "decstr-rendition";
            emit "\027[22;24;29m");
          let national = (Vte_adapter.modes m).national in
          emit raw;
          feed raw;
          if national && on ctx "decstr-national" then (
            fire ctx "decstr-national";
            emit "\027[?42l");
          if on ctx "decstr-protection" then (
            fire ctx "decstr-protection";
            emit "\027[0\"q")
      | Csi { marker = ""; inter = ""; final = 'm'; params; raw; clean = true }
        when String.contains params ':' && on ctx "underline-style" ->
          let param p =
            match String.split_on_char ':' p with
            | "4" :: style :: _ -> (
                match int_of_string_opt style with
                | Some 0 -> Some "24"
                | Some 2 -> Some "21"
                | Some _ -> Some "4"
                | None -> None)
            | _ -> None
          in
          let ps = String.split_on_char ';' params in
          if List.exists (fun p -> param p <> None) ps then (
            fire ctx "underline-style";
            emit "\027[";
            emit
              (String.concat ";"
                 (List.map (fun p -> Option.value (param p) ~default:p) ps));
            emit "m")
          else emit raw;
          feed raw
      | Csi { marker = ""; inter; final; raw; clean = true; params }
        when inter = ""
             && (final = 'S' || final = 'T' || final = 'Z' || final = 'P'
               || final = '@')
             || (inter = " " && (final = '@' || final = 'A'))
             || (inter = "'" && (final = '}' || final = '~')) ->
          let c = Vte_adapter.cursor m in
          let pending = c.last_column in
          (* delete-wide-stale: DCH, SL and DECDC move the cells after the
             deleted ones left; where the first column they vacate held the
             second half of a wide character, xterm also blanks the column
             before it, which has just received the row's last character. *)
          let x = if inter = " " then 0 else c.col in
          let n = min (count params) (c.cols - x) in
          let deletes =
            final = 'P'
            || (inter = " " && final = '@')
            || (inter = "'" && final = '~')
          in
          let lost =
            if deletes && n < c.cols - x && on ctx "delete-wide-stale" then
              let screen = Vte_adapter.screen m in
              List.filter
                (fun r -> screen.lines.(r).(c.cols - n).cont)
                (if final = 'P' then [ c.row ] else List.init c.rows Fun.id)
            else []
          in
          (* margin-cut-wide: ICH, SR and DECIC move cells right; a wide
             character they push across the right margin leaves a blank in
             the last column, and xterm its first half. *)
          let k =
            if inter = "" && final = '@' then min (count params) (c.cols - c.col)
            else if inter = " " && final = 'A' then min (count params) c.cols
            else if inter = "'" && final = '}' then
              min (count params) (c.cols - c.col)
            else 0
          in
          let pushed =
            if k > 0 && on ctx "margin-cut-wide" then
              let from = if inter = " " then 0 else c.col in
              let rows =
                if inter = "" then [ c.row ]
                else
                  let top, bottom = !margins in
                  List.init (bottom - top + 1) (fun i -> top + i)
              in
              List.filter
                (fun r ->
                  let x = c.cols - 1 - k in
                  x >= from
                  && x + 1 < c.cols
                  && (Vte_adapter.cell m ~row:r ~col:(x + 1)).cont)
                rows
            else []
          in
          (* insert-cut-wide: DECIC inside a wide character. *)
          let split_rows =
            if
              inter = "'" && final = '}' && c.col > 0
              && on ctx "insert-cut-wide"
            then
              let top, bottom = !margins in
              List.filter
                (fun r -> (Vte_adapter.cell m ~row:r ~col:c.col).cont)
                (List.init (bottom - top + 1) (fun i -> top + i))
            else []
          in
          let sd_zero =
            inter = "" && final = 'T' && params <> ""
            && String.for_all (fun c -> c = '0') params
            && on ctx "sd-zero"
          in
          let top, bottom = !margins in
          let h = bottom - top + 1 in
          let su_whole =
            inter = "" && final = 'S'
            && count params >= h
            && h < ctx.rows && h >= 2 && on ctx "su-whole-region"
          in
          if sd_zero then (
            fire ctx "sd-zero";
            emit "\027[T")
          else if su_whole then (
            fire ctx "su-whole-region";
            emit (Printf.sprintf "\027[%dS\027[S" (h - 1)))
          else emit raw;
          let touched =
            if
              (inter = "" && (final = 'P' || final = '@'))
              || (inter = "'" && (final = '}' || final = '~'))
            then touch_cut ~row:c.row ~col:c.col
            else None
          in
          feed raw;
          lose touched;
          if split_rows <> [] then (
            fire ctx "insert-cut-wide";
            Vte_adapter.tag tags ~protected:true ~text:lost_tag
              (List.map (fun r -> (r, c.col - 1)) split_rows));
          if pushed <> [] then (
            fire ctx "margin-cut-wide";
            Vte_adapter.tag tags ~protected:true ~text:lost_tag
              (List.map (fun r -> (r, c.cols - 1)) pushed));
          (* A lost cell holding half of a wide character: both halves. *)
          let lost =
            List.concat_map
              (fun (r : int) ->
                let col = c.cols - n - 1 in
                if col > 0 && (Vte_adapter.cell m ~row:r ~col).cont then
                  [ r; -1 - r ]
                else [ r ])
              lost
          in
          let lost_cells =
            List.map
              (fun r ->
                if r >= 0 then (r, c.cols - n - 1) else (-1 - r, c.cols - n - 2))
              lost
          in
          let lost = List.filter (fun r -> r >= 0) lost in
          if lost <> [] then (
            fire ctx "delete-wide-stale";
            Vte_adapter.tag tags ~protected:true ~text:lost_tag lost_cells);
          let name = if final = 'Z' then "cbt-pending-wrap" else "lcf-edits" in
          if
            final <> 'P'
            && (not (inter = "" && final = '@'))
            && pending
            && (not (Vte_adapter.cursor m).last_column)
            && on ctx name
          then (
            fire ctx name;
            emit (cha ()))
      | Csi
          {
            marker = "";
            inter = "";
            final = ('J' | 'K' | 'X') as final;
            params;
            raw;
            _;
          } ->
          let c = Vte_adapter.cursor m in
          let from_cursor = final = 'X' || params = "" || params = "0" in
          let touched =
            if from_cursor then touch_cut ~row:c.row ~col:c.col else None
          in
          emit raw;
          feed raw;
          lose touched
      | Csi { raw; _ } ->
          emit raw;
          feed raw
      | Str raw ->
          let b = Buffer.create (String.length raw) in
          let rec go i =
            if i < String.length raw then (
              let cp, len = decode raw i in
              if is_c1 cp && on ctx "c1-utf8" then fire ctx "c1-utf8"
              else if
                cp >= 0x300
                && ctx.width cp = 0
                && (not (is_format cp))
                && on ctx "sequence-zero-width"
              then fire ctx "sequence-zero-width"
              else Buffer.add_string b (String.sub raw i len);
              go (i + len))
          in
          go 0;
          emit (Buffer.contents b);
          feed raw
      | Overlong raw ->
          if on ctx "param-budget" then (
            fire ctx "param-budget";
            drop ())
          else emit raw;
          feed raw
      | Silent raw ->
          (* A sequence ended by a non-ASCII character: xterm reads one in
             U+0080..U+00FF as a 7-bit byte, so it gets U+0100 instead. *)
          let len = String.length raw in
          let rec start i =
            if i <= 0 then None
            else if Char.code raw.[i] land 0xC0 <> 0x80 then Some i
            else start (i - 1)
          in
          let last =
            if len > 1 && raw.[0] = '\027' then start (len - 1) else None
          in
          (match last with
          | Some i when i > 0 -> (
              let cp, l = decode raw i in
              match () with
              | ()
                when i + l = len
                     && cp >= 0x80 && cp <= 0xFF && on ctx "sequence-latin1" ->
                  fire ctx "sequence-latin1";
                  emit (String.sub raw 0 i);
                  emit (utf8 0x100)
              | ()
                when i + l = len
                     && cp >= 0x300
                     && ctx.width cp = 0
                     && (not (is_format cp))
                     && on ctx "sequence-zero-width" ->
                  fire ctx "sequence-zero-width";
                  emit (String.sub raw 0 i);
                  emit (utf8 0x100)
              | () -> emit raw)
          | _ -> emit raw);
          feed raw)
    (lex bytes);
  {
    xterm = lone_continuations ctx (Buffer.contents out);
    orphans = List.sort_uniq compare !orphans;
    erased = Vte_adapter.tagged tags ~text:erased_tag;
    cut = Vte_adapter.tagged tags ~text:cut_tag;
    lost = Vte_adapter.tagged tags ~text:lost_tag;
  }

(* {1 Replies} *)

(* [csi_reply s i] is the control sequence at [i] in [s], [CSI P... F], as
   its parameter bytes and final, and the index after it. *)
let csi_reply s i =
  let n = String.length s in
  if i + 1 < n && s.[i] = '\027' && s.[i + 1] = '[' then
    let rec go j =
      if j >= n then None
      else if s.[j] >= '\x40' && s.[j] <= '\x7E' then
        Some (String.sub s (i + 2) (j - i - 2), s.[j], j + 1)
      else go (j + 1)
    in
    go (i + 2)
  else None

let replies ctx ~input s =
  let dcs = occurs input "\027P" in
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      match csi_reply s i with
      | Some (p, 'c', j)
        when String.length p > 0
             && p.[0] = '?'
             && p <> "?1;2" && on ctx "identity" ->
          fire ctx "identity";
          Buffer.add_string b "\027[?1;2c";
          go j
      | Some (p, 'c', j)
        when String.length p > 0
             && p.[0] = '>'
             && p <> ">0;0;0" && on ctx "identity" ->
          fire ctx "identity";
          Buffer.add_string b "\027[>0;0;0c";
          go j
      | Some (p, 'R', j)
        when String.length p > 0
             && p.[0] = '?'
             && List.length (String.split_on_char ';' p) = 2
             && on ctx "decxcpr-page" ->
          fire ctx "decxcpr-page";
          Buffer.add_string b ("\027[" ^ p ^ ";1R");
          go j
      | Some (_, _, j) ->
          Buffer.add_string b (String.sub s i (j - i));
          go j
      | None when dcs && s.[i] = '\x18' && on ctx "dcs-requests" ->
          fire ctx "dcs-requests";
          go (i + 1)
      | None
        when dcs
             && i + 1 < n
             && s.[i] = '\027'
             && s.[i + 1] = 'P'
             && on ctx "dcs-requests" -> (
          fire ctx "dcs-requests";
          match occurs (String.sub s i (n - i)) "\027\\" with
          | true ->
              let rec st k =
                if k + 1 >= n then n
                else if s.[k] = '\027' && s.[k + 1] = '\\' then k + 2
                else st (k + 1)
              in
              go (st (i + 2))
          | false -> ())
      | None ->
          Buffer.add_char b s.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents b

(* {1 Screens} *)

let map_cells f (s : Screen.t) =
  { s with lines = Array.map (Array.map f) s.lines }

(* A cell without the scalars [drop] selects; a cell left empty is a space. *)
let strip drop (c : Screen.cell) =
  if c.cont then c
  else
    let kept = List.filter (fun cp -> not (drop cp)) (scalars c.text) in
    if List.length kept = List.length (scalars c.text) then c
    else
      let text = String.concat "" (List.map utf8 kept) in
      { c with text = (if text = "" then " " else text) }

let screens ctx ~orphans ~erased ~cut ~lost ~(xterm : Screen.t)
    ~(matrix : Screen.t) =
  let changed = ref false in
  let tracking name f s =
    let s' = f s in
    if s' <> s then (
      changed := true;
      fire ctx name);
    s'
  in
  let xterm, matrix =
    if orphans <> [] && on ctx "zero-width-join" then
      let drop cp = List.mem cp orphans in
      let blank (c : Screen.cell) =
        if String.length c.text > 0 && c.text.[0] = ' ' then strip drop c else c
      in
      ( tracking "zero-width-join" (map_cells blank) xterm,
        tracking "zero-width-join" (map_cells blank) matrix )
    else (xterm, matrix)
  in
  let unknown (s : Screen.t) =
    {
      s with
      lines =
        Array.mapi
          (fun r l ->
            Array.mapi
              (fun c (cell : Screen.cell) ->
                if
                  List.mem (r, c) erased
                  && String.length cell.text > 0
                  && cell.text.[0] = ' '
                  && List.for_all
                       (fun cp -> ctx.width cp = 0)
                       (List.tl (scalars cell.text))
                then { cell with text = " "; known = false }
                else cell)
              l)
          s.lines;
    }
  in
  let xterm, matrix =
    if erased <> [] then
      ( tracking "selective-erase" unknown xterm,
        tracking "selective-erase" unknown matrix )
    else (xterm, matrix)
  in
  let matrix =
    if on ctx "format-chars" then
      tracking "format-chars"
        (map_cells (strip (fun cp -> is_format cp && ctx.width cp = 0)))
        matrix
    else matrix
  in
  let xterm =
    if on ctx "sub-glyph" then
      let glyph = if ctx.decid > 200 then 0x2426 else 0x2592 in
      let sub = utf8 0x2E2E in
      tracking "sub-glyph"
        (fun (s : Screen.t) ->
          {
            s with
            lines =
              Array.mapi
                (fun r l ->
                  Array.mapi
                    (fun c (cell : Screen.cell) ->
                      let m = matrix.lines.(r) in
                      let rest t = String.sub t 3 (String.length t - 3) in
                      if
                        first_scalar cell.text = glyph
                        && c < Array.length m
                        && String.starts_with ~prefix:sub m.(c).text
                        && rest cell.text = rest m.(c).text
                      then { cell with text = m.(c).text }
                      else cell)
                    l)
                s.lines;
          })
        xterm
    else xterm
  in
  let matrix =
    if on ctx "combining-limit" then
      tracking "combining-limit"
        (map_cells (fun (c : Screen.cell) ->
             let cps = scalars c.text in
             if List.length cps > 6 then
               {
                 c with
                 text =
                   String.concat ""
                     (List.map utf8 (List.filteri (fun i _ -> i < 6) cps));
               }
             else c))
        matrix
    else matrix
  in
  let xterm =
    if on ctx "moved-wide-marker" then
      tracking "moved-wide-marker"
        (fun (s : Screen.t) ->
          {
            s with
            lines =
              Array.mapi
                (fun r (l : Screen.cell array) ->
                  let l = Array.copy l in
                  for c = 0 to Array.length l - 2 do
                    if
                      (not l.(c).cont)
                      && ctx.width (first_scalar l.(c).text) = 2
                      && (not l.(c + 1).cont) && l.(c + 1).text = " "
                      && (not (List.mem (r, c) lost))
                      && not (List.mem (r, c) cut)
                    then
                      l.(c + 1) <- { (l.(c + 1)) with text = ""; cont = true }
                  done;
                  l)
                s.lines;
          })
        xterm
    else xterm
  in
  let xterm, matrix =
    if cut <> [] && on ctx "cut-wide" then
      let blank (s : Screen.t) =
        {
          s with
          lines =
            Array.mapi
              (fun r l ->
                Array.mapi
                  (fun c (cell : Screen.cell) ->
                    let next_cont =
                      c + 1 < Array.length l && l.(c + 1).Screen.cont
                    in
                    if
                      List.mem (r, c) cut
                      && String.length cell.text > 0
                      && (cell.text.[0] = ' '
                          && List.for_all
                               (fun cp -> ctx.width cp = 0)
                               (List.tl (scalars cell.text))
                         (* The whole character, where xterm had moved it
                            without the marker of its second half
                            (moved-wide-marker). *)
                         || ctx.width (first_scalar cell.text) = 2
                            && not next_cont)
                    then { cell with text = " "; known = false }
                    else cell)
                  l)
              s.lines;
        }
      in
      (tracking "cut-wide" blank xterm, tracking "cut-wide" blank matrix)
    else (xterm, matrix)
  in

  (* A cell xterm lost shows matrix's. *)
  let xterm =
    if lost <> [] then
      (fun (s : Screen.t) ->
        {
          s with
          lines =
            Array.mapi
              (fun r l ->
                let m = matrix.lines.(r) in
                let l =
                  if Array.length l < Array.length m then
                    Array.append l
                      (Array.make
                         (Array.length m - Array.length l)
                         Screen.blank)
                  else l
                in
                Array.mapi
                  (fun c (cell : Screen.cell) ->
                    if List.mem (r, c) lost && c < Array.length m then m.(c)
                    else cell)
                  l)
              s.lines;
        })
        xterm
    else xterm
  in
  (xterm, matrix)

(* {1 Generator restrictions} *)

let national_finals = "4C5RfQ9KYE6`ZH7="

(* [allow ctx ~prev ~last t] restricts the generator. Tokens may form a
   sequence together (ESC, then text that starts with c), so the checks read
   the bytes so far, [before], and the bytes with [t], [after]. *)
let allow ctx ~(prev : Gen.token list) ~last (t : Gen.token) =
  let reject name =
    if on ctx name then (
      fire ctx name;
      false)
    else true
  in
  (* C0 controls are performed inside a sequence, which goes on, and DEL is
     dropped there: neither keeps ESC and its final apart. *)
  let bare s =
    let b = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        if
          (c >= '\x20' && c <> '\x7f') || c = '\027' || c = '\x18' || c = '\x1a'
        then Buffer.add_char b c)
      s;
    Buffer.contents b
  in
  let before =
    bare (String.concat "" (List.rev_map (fun (p : Gen.token) -> p.bytes) prev))
  in
  let after = before ^ bare t.bytes in
  let n = String.length before in
  (* [forms sub]: [t] completes an occurrence of [sub]. *)
  let forms sub = occurs ~ending_after:n after sub in
  let forms_any subs = List.exists forms subs in
  let designations sets finals =
    List.concat_map
      (fun g -> List.map (fun f -> Printf.sprintf "\027%c%c" g f) finals)
      sets
  in
  let chars s = List.init (String.length s) (String.get s) in
  let non_ascii = String.exists (fun c -> Char.code c >= 0x80) t.bytes in
  let national = occurs before "\027[?42h" in
  let vt_level () =
    ctx.decid < 200
    && forms_any
         ([ "\027N"; "\027O"; "\027n"; "\027o"; "\027[?42h"; "\027[?42l" ]
         @ designations [ '('; ')'; '*'; '+' ] (chars national_finals))
    || ctx.decid < 400
       && (String.starts_with ~prefix:"DECIC" t.label
          || String.starts_with ~prefix:"DECDC" t.label)
  in
  let s8c1t () = ctx.decid >= 200 && forms "\027 G" in
  (* Once G2 holds a set other than ASCII. *)
  let g2 () =
    non_ascii
    &&
    let rec go i =
      i + 2 < String.length before
      && ((before.[i] = '\027' && before.[i + 1] = '*' && before.[i + 2] <> 'B')
         || go (i + 1))
    in
    go 0
  in

  (* UK # and DEC Special Graphics _ after a designation of their set. *)
  let internal () =
    String.contains t.bytes '#'
    && List.exists (occurs before) (designations [ '('; ')'; '*'; '+' ] [ 'A' ])
    || String.contains t.bytes '_'
       && List.exists (occurs before)
            (designations [ '('; ')'; '*'; '+' ] [ '0' ])
  in
  (* A single shift: [t] completes ESC N or ESC O, or follows one. *)
  let shifted = forms "\027N" || forms "\027O" in
  let after_shift =
    String.ends_with ~suffix:"\027N" before
    || String.ends_with ~suffix:"\027O" before
  in
  let shift_last =
    String.ends_with ~suffix:"\027N" after
    || String.ends_with ~suffix:"\027O" after
  in
  (* A character beyond U+00FF printed before. *)
  let wide_before =
    List.exists
      (fun (p : Gen.token) ->
        String.starts_with ~prefix:"text" p.label
        && List.exists (fun cp -> cp > 0xFF) (scalars p.bytes))
      prev
  in
  let ss () =
    (shifted && ((last && shift_last) || wide_before))
    || after_shift
       && not
            (String.length t.bytes > 0
            && String.for_all (fun c -> c >= '\x20' && c <= '\x7E') t.bytes
            && String.starts_with ~prefix:"text" t.label)
  in
  (* Escape sequences xterm performs and matrix, a VT100, does not. *)
  let escapes () =
    forms_any
      ([
         "\027V";
         "\027W";
         "\027l";
         "\027m";
         "\027#3";
         "\027#4";
         "\027#5";
         "\027#6";
         "\027 L";
         "\027 M";
         "\027 N";
       ]
      @ (if ctx.decid >= 200 then [ "\027~"; "\027}"; "\027|" ] else [])
      @ if ctx.decid >= 400 then [ "\027" ^ "6"; "\027" ^ "9" ] else [])
  in
  (* Media copy, CSI Pm i: it prints, or turns printer controller mode on,
     which takes the harness's own print. *)
  let media_copy () =
    let m = String.length after in
    let rec csi i =
      i < m
      &&
      match after.[i] with
      | '0' .. '9' | ';' | '?' -> csi (i + 1)
      | 'i' -> i >= n
      | _ -> false
    in
    let rec go i =
      i + 1 < m
      && ((after.[i] = '\027' && after.[i + 1] = '[' && csi (i + 2))
         || go (i + 1))
    in
    go (max 0 (n - 300))
  in
  let national_designation () =
    ctx.decid >= 200
    && ((not national)
        && forms_any
             (designations [ '('; ')'; '*'; '+' ] (chars national_finals))
       || forms "\027[?42l"
          && List.exists (occurs before)
               (designations [ '('; ')'; '*'; '+' ] (chars national_finals)))
  in
  let too_wide () =
    ctx.cols < 2 && List.exists (fun cp -> ctx.width cp = 2) (scalars t.bytes)
  in
  let nrcm () = non_ascii && national in
  let ris () = national && forms "\027c" in
  not
    ((vt_level () && not (reject "vt-level"))
    || (s8c1t () && not (reject "s8c1t"))
    || (g2 () && not (reject "g2-raw-bytes"))
    || (ss () && not (reject "ss-non-gl"))
    || (internal () && not (reject "charset-internal"))
    || (nrcm () && not (reject "national-utf8"))
    || (ris () && not (reject "ris-national"))
    || (escapes () && not (reject "xterm-escapes"))
    || (media_copy () && not (reject "media-copy"))
    || (national_designation () && not (reject "national-designation"))
    || (too_wide () && not (reject "wider-than-line")))
