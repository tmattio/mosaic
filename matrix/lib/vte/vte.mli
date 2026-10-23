(** Terminal emulator.

    A program running in a terminal writes bytes: the characters it shows, and
    the coded forms of control functions that move the cursor, edit the screen
    or ask the terminal something ({!Ansi.Parser} reads them). [t] applies those
    bytes to a model of a DEC VT-series terminal as xterm extends it, and keeps
    what a renderer shows: the cells of the screen, the cursor, and the lines
    scrolled off the top. It does no I/O: the answers the program asked for are
    the result of {!feed}.

    The screen is a function of the bytes, never of how they were split into
    {!feed} calls.

    {1:model The model}

    {2:cursor The cursor}

    The {e cursor state} decides where and how the next character is written:
    - the position;
    - the {e Last Column Flag};
    - the {e pen}: the rendition SGR selects, the hyperlink OSC 8 opens, and
      whether DECSCA protects the characters written next;
    - the character sets designated as G0 to G3, the one invoked into GL, and a
      pending single shift;
    - origin mode (DECOM).

    DECSC saves exactly this and DECRC restores it. Autowrap (DECAWM) is a
    setting, not part of it: DEC's VT220, VT420 and VT510, measured, restore the
    flag and not the mode. With autowrap off the flag is never set (below), so
    DECRC then restores the position without it.

    A character printed into the last column with autowrap on leaves the cursor
    on it with the Last Column Flag set; the next character first moves to the
    start of the next line. Every function that sets the cursor position or
    changes cells clears the flag, and so does resetting DECAWM. HT and CHT keep
    it, since they never leave the last column; SGR, settings, DECSC and reports
    keep it too. With autowrap off the flag is never set, and a character
    printed at the last column replaces the one there. Cursor reports never show
    the flag: the cursor is in the last column.

    How the cursor looks is not part of this state. DECTCEM shows or hides it,
    and DECSCUSR, [CSI Ps SP q], sets its style: 0, 1 or none is a blinking
    block, 2 a steady block, 3 and 4 a blinking and a steady underline (VT510
    Programmer Information, DECSCUSR), 5 and 6 a blinking and a steady bar, and
    7 the style at power-up (xterm's ctlseqs); other values have no effect. Both
    screens share the style, and DECSC does not save it.

    {2:screens Screens}

    The terminal has two screens, primary and alternate. Each has its own cells,
    its own wrap marks and its own DECSC slot, which holds the power-up cursor
    state until DECSC fills it; everything else is shared. xterm's modes 47,
    1047, 1048 and 1049 compose four steps (switch screens, erase the alternate
    screen, DECSC, DECRC):
    - 47 switches;
    - 1047 switches, and erases the alternate screen when leaving it;
    - 1048 is DECSC when set and DECRC when reset;
    - 1049 is DECSC, a switch and an erase when set, and the reset of 1047 then
      DECRC when reset.

    A row's {e wrap mark} says that its text goes on at the start of the next
    row because autowrap moved there. Only printing sets it; an edit that
    reaches the row's last column or moves its cells sideways clears it, and a
    scroll moves it with the row. Rows scrolled off the top of the primary
    screen, while the scroll region starts at the first row, enter the
    scrollback with their mark.

    {2:blanks Blanks}

    Every cell an erase, insertion, deletion or scroll empties becomes a
    {e blank}: a space with no attributes, unprotected, on the pen's background
    colour. ECMA-48 puts such cells "into the erased state" (§8.3.26, DCH); the
    colour is xterm's, which terminfo advertises as [bce]. A wide character such
    an operation cuts becomes blanks of the same colour. Selective erase
    (DECSED, DECSEL) instead turns the characters of unprotected cells into
    spaces and keeps their rendition: it "does not affect video line attributes
    or video character attributes (SGR)" (VT220 Programmer Reference Manual
    §4.12).

    {2:printing Printing}

    Characters are placed by display unit, as {!Matrix_text.Segmenter} cuts them
    under the grid's width method: a grapheme cluster, or a wcwidth unit. The
    terminal places a unit as soon as it has a width, before it knows that the
    unit is complete, and places it again from the same position when later
    characters extend it; a unit never narrows. For the screen not to depend on
    where the output was split, placement follows four rules:
    + a unit that does not fit before the right margin, with autowrap on, first
      erases the cells from its position to the margin, then moves to the next
      line. xterm leaves those cells alone; but a unit placed narrow and then
      widened past the margin must vacate the cells it wrote, and the screen
      must not show whether that happened;
    + with autowrap off, a unit that does not fit ends at the margin, replacing
      what is there;
    + in insert mode (IRM) a unit moves the rest of the line right by its width,
      and by its growth when it widens;
    + a unit wider than the line is narrowed to it.

    A control function ends the unit in progress. A unit of no width that
    continues nothing, such as U+200B, or a combining mark after a control
    function, joins the character in the cell before the cursor, or in the
    cursor's cell when the Last Column Flag is set, without changing its width
    or rendition; with no such cell it is dropped. The cell before the cursor
    holds the character a mark after an SGR change belongs to, and is found from
    the cursor alone, whatever came before. xterm instead joins the cell it last
    wrote, or after a cursor motion the cell at the cursor: after [ab], CUB and
    a combining acute accent it accents [b] where this terminal accents [a]. No
    cell holds a control character: U+0080 to U+009F are not controls in UTF-8
    and never reach a cell ({!Ansi.Parser}).

    CAN ends the sequence or string in progress and shows nothing (VT220
    Programmer Reference Manual ch. 4: "No error character is displayed"). SUB
    ends it too, and, as anywhere, shows the error character, a reversed
    question mark U+2E2E (VT510 Programmer Information ch. 4).

    REP repeats the unit printed just before it, if nothing came between them
    (ECMA-48 §8.3.103), as many times as asked, each repetition a unit of its
    own. Its work is bounded by the screen and the scrollback, whatever the
    count: with autowrap, once every row it can reach holds the same units, the
    screen repeats with the period of a row, so a unit [w] cells wide is placed
    at most about [cols / w * (rows + scrollback)] times; without autowrap at
    most [cols + 1] times. That bound is still large: a REP of a few bytes on an
    80-column screen with 10,000 lines of scrollback places about 800,000 units,
    a tenth of a second.

    Character sets translate characters before units are formed: DEC Special
    Graphics, the UK set and the national replacement sets (VT220 Programmer
    Reference Manual tables 2-5 to 2-15, with the final characters xterm's
    ctlseqs lists), invoked by SI, SO, LS2 and LS3, or for one character by SS2
    and SS3. The national sets "are available when you select national mode"
    (§2.4.3), DECNRCM: out of it a slot designated with one leaves characters
    unchanged, and translates once national mode is selected. A designation of a
    set this terminal has no table for, such as DEC Supplemental, leaves the
    characters unchanged.

    {2:line_feeds Line feeds}

    LF, VT and FF move the cursor down a line, scrolling at the bottom margin,
    and keep its column: "These three codes are always processed identically"
    (VT220 Programmer Reference Manual §4.6.6). With line feed/new line mode
    (LNM, [CSI 20 h]) set, they also move it to the first column, as NEL does;
    IND and NEL do not change. LNM also says that the Return key sends CR LF
    instead of CR, which {!type-modes} reports for whoever encodes the keys.

    A program's output usually reaches its terminal through a tty, whose output
    processing (the ONLCR flag of termios) writes each LF as CR LF: the terminal
    never sees a bare LF, and lines start at the first column. This terminal
    reads the bytes it is fed, as a tty delivers them. To read a program's
    output as a cooked tty shows it, set LNM: feed [CSI 20 h] first. The two
    differ only on VT and FF, which LNM also returns and ONLCR leaves alone.

    {2:parameters Parameters}

    For counts and positions 0 means the default, as an absent parameter does
    (VT510 §4.3.3). A count acts only as far as it can: [CSI 99999 C] stops at
    the right margin.

    A parameter may be a string of sub-parameters separated by colons, whose
    meaning the function defines (ECMA-48 §5.4.2). Only SGR defines any: in any
    other control sequence one makes the sequence malformed, and it has no
    effect, so [CSI 2:3 H] does not move the cursor.

    {2:replies Replies}

    A program asks the terminal questions, and the terminal answers on the
    program's input. The answers are {!Matrix_input.Reply.t} values, the ones a
    program's input decoder reads, which {!Matrix_input.Reply.encode} writes as
    the terminal's bytes. The terminal answers:
    - DA ([CSI c]), and DECID ([ESC Z]), its obsolete form (ctlseqs), with
      [Primary_attributes [1; 2]], [CSI ? 1 ; 2 c], a VT100 with Advanced Video,
      and DA2 ([CSI > c]) with [Secondary_attributes [0; 0; 0]],
      [CSI > 0 ; 0 ; 0 c]. The identity is a promise, and the same for every
      terminal: a VT220 (level 2) would promise 8-bit controls and DECSCL, which
      this terminal lacks. A caller that must pass for another terminal, such as
      a recorder whose sessions are compared with that terminal's, rewrites the
      reply before relaying it, where the promise is visibly its own;
    - DSR 5 with [Operating_status `Ok], [CSI 0 n], and CPR (DSR 6) with a
      [Cursor] with no page, [CSI r ; c R], the row counted from the top margin
      in origin mode;
    - DECXCPR ([CSI ? 6 n]) with a [Cursor] on page 1, [CSI ? r ; c ; 1 R]. The
      VT510 manual shows no [?]; xterm sends it, as DEC's other answers to
      [CSI ? ... n] carry it, and it keeps the answer apart from a modified F3
      key ([CSI 1 ; 2 R]);
    - DECRQM, which asks for the state of one mode, [CSI Ps $ p] for an ANSI
      mode and [CSI ? Ps $ p] for a DEC private one, with DECRPM, a [Mode]:
      [CSI Ps ; Pm $ y] and [CSI ? Ps ; Pm $ y], where [Pm] is 1 for set, 2 for
      reset, and 0 for a mode this terminal does not recognize (VT320 Programmer
      Information, "Mode Settings"). The modes recognized are those of
      {!type-modes}; modes 47, 1047 and 1049 report the alternate screen;
    - a colour spec [?] in OSC 4, 10, 11 or 12, which asks for a colour of the
      palette ({!section-palette}), with a [Color] from the palette as it is,
      the OSC that would set it, such as [OSC 11 ; rgb:0000/0000/0000], ended as
      the query was, by BEL or ST: xterm "uses the same terminator used in a
      query" (ctlseqs, "Operating System Commands").

    {2:palette The palette}

    The {e palette} is the colours the emulated terminal's user configured: the
    256 indexed colours SGR selects, the default foreground and background, and
    the cursor's colour. {!create} is given it, as it is given the size, and
    programs may change it, as xterm lets them (ctlseqs, "Operating System
    Commands"):
    - OSC 4 [; c ; spec], any number of pairs, sets indexed colour [c], from 0
      to 255;
    - OSC 10, 11 and 12 [; spec] set the default foreground, the default
      background and the cursor's colour. "Each successive parameter changes the
      next color in the list": [OSC 10 ; a ; b] sets the foreground to [a] and
      the background to [b];
    - a spec [?] asks for the colour instead ({!section-replies});
    - OSC 104 [; c] resets the indexed colours listed, all of them when none is,
      and OSC 110, 111 and 112 reset the foreground, the background and the
      cursor's colour, each to the palette {!create} was given.

    A spec is read by {!Matrix_input.Reply.rgb_of_spec}, the forms [rgb:r/g/b]
    and [#rrggbb]; a pair or spec that names no colour, such as an X11 colour
    name or an index above 255, is skipped. A change marks every row and the
    cursor changed ({!section-dirty}).

    Cells keep colours as the pen gave them: the defaults (SGR 39 and 49, or no
    colour) as {!Ansi.Color.default_fg} and {!Ansi.Color.default_bg}, indexed
    colours by their index, and RGB colours as they are. The palette alone says
    what the defaults and the indices show, so a change of the palette changes
    no cell: a renderer resolves the default and indexed intents of
    {!Ansi.Color} through {!val-palette} when it draws.

    {2:resets Resets}

    RIS returns to the power-up state, the cursor a blinking block. DECSTR
    returns each terminal setting to its power-up value: insert, origin,
    autowrap, reverse-wrap and national modes, cursor keys, cursor visibility
    and the margins; the pen and the character sets to their defaults; both
    DECSC slots to the power-up cursor state. The VT510's table gives "No
    autowrap", its own power-up value; this terminal powers up with autowrap on,
    so DECSTR sets it on. The screen, the cursor position, the active screen,
    LNM, the cursor style, bracketed paste, the tab stops, the title and the
    palette stay: the table lists neither LNM, DECSCUSR nor colours. xterm, a
    datapoint, resets the cursor style and the indexed colours there too. RIS
    restores the palette {!create} was given: it "replaces all set-up features
    with their saved settings" (VT510, RIS). *)

type t
(** The type for terminal emulators. Mutable, not thread-safe. *)

type palette = {
  colors : (int * int * int) array;
      (** The 256 indexed colours, from index 0: the eight of SGR 30 to 37, the
          eight bright ones of SGR 90 to 97, then those only SGR 38 ; 5 selects.
      *)
  foreground : int * int * int;  (** The default foreground (OSC 10). *)
  background : int * int * int;  (** The default background (OSC 11). *)
  cursor : int * int * int;  (** The cursor's colour (OSC 12). *)
}
(** The type for palettes ({!section-palette}): each colour is red, green and
    blue, from 0 to 255. *)

(** {1:create Creating} *)

val create :
  ?scrollback:int ->
  ?width_method:Matrix_text.width_method ->
  ?palette:palette ->
  rows:int ->
  cols:int ->
  unit ->
  t
(** [create ~rows ~cols ()] is a terminal in its power-up state, with:
    - [scrollback], the number of lines the scrollback keeps, the oldest dropped
      first. Defaults to [10000]; [0] keeps none.
    - [width_method], how wide characters are. Defaults to [`Unicode].
    - [palette], the palette ({!section-palette}). Defaults to the colours of
      {!Ansi.Color.indexed}, with indexed colour 7 as the default foreground and
      the cursor's colour and 0 as the default background, white on black. [t]
      keeps a copy.

    [rows] and [cols] below [1] are taken as [1].

    Raises [Invalid_argument] if [cols > Matrix_grid.max_width], or if [palette]
    does not have 256 colours or has a channel outside 0..255. *)

(** {1:feeding Feeding output} *)

val feed : t -> bytes -> int -> int -> Matrix_input.Reply.t list
(** [feed t b off len] applies the [len] bytes of [b] from [off] and is the
    replies they asked for ({!section-replies}), in order: write
    [Matrix_input.Reply.encode replies] to the program's input, after rewriting
    or dropping any the caller answers otherwise. It is [[]], with no
    allocation, when there are none. A sequence split across calls is answered
    by the call that completes it.

    Raises [Invalid_argument] if [off] and [len] do not designate a range of
    [b]. *)

val feed_string : t -> string -> Matrix_input.Reply.t list
(** [feed_string t s] is
    [feed t (Bytes.unsafe_of_string s) 0 (String.length s)]. *)

val reset : t -> unit
(** [reset t] is RIS: the power-up state, with the scrollback emptied and the
    title cleared. It also drops a sequence left unfinished by {!feed}. *)

val resize : t -> rows:int -> cols:int -> unit
(** [resize t ~rows ~cols] changes the screen size and keeps the text, without
    reflowing it. Each screen keeps the rows around its cursor (the active
    screen's cursor, or the cursor saved in the other's DECSC slot): shrinking
    drops rows below that cursor first, then moves rows off the top, into the
    scrollback for the primary screen; growing takes the newest scrollback lines
    back onto the primary screen, then adds blank rows at the bottom. Rows keep
    their wrap marks; a row back from the scrollback has its text, rendition and
    mark, and no protection. Cursors and DECSC slots move with their rows and
    lose the Last Column Flag, the margins become the whole screen, and a unit
    in progress is ended. [rows] and [cols] below [1] are taken as [1].

    Raises [Invalid_argument], and changes nothing, if
    [cols > Matrix_grid.max_width]. *)

(** {1:state State} *)

val rows : t -> int
(** [rows t] is the number of rows. *)

val cols : t -> int
(** [cols t] is the number of columns. *)

val grid : t -> Matrix_grid.t
(** [grid t] are the cells of the active screen. The grid belongs to [t]: read
    it, and do not change it. *)

val cursor_pos : t -> int * int
(** [cursor_pos t] is the cursor's [(row, col)], from [(0, 0)], with
    [row < rows t] and [col < cols t]. *)

val last_column : t -> bool
(** [last_column t] is the Last Column Flag ({!section-cursor}): the cursor is
    on the character just printed in the last column, and the next one wraps
    first. It implies autowrap and [snd (cursor_pos t) = cols t - 1]. Cursor
    reports never show it. *)

val cursor_style : t -> Ansi.cursor_style * bool
(** [cursor_style t] is the cursor's shape and whether it blinks, as DECSCUSR
    last set them ({!section-cursor}): [(`Block, true)] at power-up. *)

type modes = {
  insert : bool;  (** IRM (SM 4). *)
  new_line : bool;
      (** LNM (SM 20): LF, VT and FF also return to the first column
          ({!section-line_feeds}), and the Return key sends CR LF. *)
  origin : bool;  (** DECOM (DECSET 6). *)
  auto_wrap : bool;  (** DECAWM (DECSET 7). *)
  reverse_wrap : bool;
      (** xterm's reverse-wraparound (DECSET 45): with autowrap, BS and CUB go
          back across the wraps that printing made. *)
  cursor_keys : bool;  (** DECCKM (DECSET 1): cursor keys send [ESC O]. *)
  cursor_visible : bool;  (** DECTCEM (DECSET 25). *)
  bracketed_paste : bool;  (** xterm's bracketed paste (DECSET 2004). *)
  alternate_screen : bool;  (** The alternate screen is active. *)
  national : bool;
      (** DECNRCM (DECSET 42): the national replacement character sets are
          available. *)
}
(** The type for the terminal's modes. At power-up autowrap is on, the cursor
    visible and the others off. *)

val modes : t -> modes
(** [modes t] are [t]'s modes. *)

val title : t -> string
(** [title t] is the title set by OSC 0 or OSC 2. *)

val palette : t -> palette
(** [palette t] is the palette as programs left it ({!section-palette}). It
    belongs to [t]: read it, and do not change its [colors]. *)

(** {1:rows Rows and the scrollback}

    Rows are numbered from the top of the screen, [0] to [rows t - 1], and on
    upward into the scrollback: row [-1] is the line that last scrolled off the
    top, and row [-(scrollback_size t)] the oldest one kept. A scrollback line
    is the row as it left the screen: each grapheme with its width, column,
    rendition and hyperlink, and the row's wrap mark; past its last cell that
    shows something, it is blank. Any other row is blank, with no wrap mark.

    The screen's rows are also the cells of {!grid}. *)

val scrollback_capacity : t -> int
(** [scrollback_capacity t] is the number of lines the scrollback keeps. *)

val scrollback_size : t -> int
(** [scrollback_size t] is the number of lines in the scrollback, [0] while the
    alternate screen is active. *)

val wrapped : t -> int -> bool
(** [wrapped t y] is the wrap mark of row [y] ({!section-screens}): its text
    goes on at the start of row [y + 1]. *)

val text : t -> int -> string
(** [text t y] is the text of row [y]: the grapheme of each cell, a space for an
    empty one, without the spaces that end it. The scrollback's lines, oldest
    first, are [List.init n (fun i -> text t (i - n))] with
    [n = scrollback_size t]. *)

val blit_row : t -> int -> Matrix_grid.t -> y:int -> unit
(** [blit_row t r dst ~y] writes row [r] of [t] on row [y] of [dst]: its cells,
    with their rendition and hyperlinks, as far as [dst] is wide, then blanks on
    {!Ansi.Color.default_bg}. A view [offset] lines up into the scrollback is
    [for y = 0 to h - 1 do blit_row t (y - offset) dst ~y done], for a [dst] [h]
    rows high.

    Raises [Invalid_argument] if [y] is not a row of [dst]. *)

(** {1:dirty Changes}

    A renderer redraws what changed since it last looked. Writing cells, moving
    the cursor or changing its visibility or style, changing the rendition, and
    changing the palette mark [t] changed; the title and the other modes do not.
*)

val is_dirty : t -> bool
(** [is_dirty t] is [true] iff [t] changed since {!clear_dirty}. *)

val dirty_rows : t -> int list
(** [dirty_rows t] are the rows whose cells changed since {!clear_dirty}, in
    increasing order. *)

val is_cursor_dirty : t -> bool
(** [is_cursor_dirty t] is [true] iff the cursor moved, was shown or hidden, or
    changed style since {!clear_dirty}. *)

val clear_dirty : t -> unit
(** [clear_dirty t] forgets the changes. *)

(** {1:debug Debugging} *)

val to_string : t -> string
(** [to_string t] is the text of the screen, a line a row, separated by
    newlines, trailing spaces kept. *)
