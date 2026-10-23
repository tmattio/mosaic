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

    {2:parameters Parameters}

    For counts and positions 0 means the default, as an absent parameter does
    (VT510 §4.3.3). A count acts only as far as it can: [CSI 99999 C] stops at
    the right margin.

    A parameter may be a string of sub-parameters separated by colons, whose
    meaning the function defines (ECMA-48 §5.4.2). Only SGR defines any: in any
    other control sequence one makes the sequence malformed, and it has no
    effect, so [CSI 2:3 H] does not move the cursor.

    {2:replies Replies}

    The terminal answers:
    - DA ([CSI c]), and DECID ([ESC Z]), its obsolete form (ctlseqs), with
      [CSI ? 1 ; 2 c], a VT100 with Advanced Video, and DA2 ([CSI > c]) with
      [CSI > 0 ; 0 ; 0 c]. The identity is a promise: a VT220 (level 2) would
      promise 8-bit controls and DECSCL, which this terminal lacks;
    - DSR 5 with [CSI 0 n], and CPR (DSR 6) with [CSI r ; c R], the row counted
      from the top margin in origin mode;
    - DECXCPR ([CSI ? 6 n]) with [CSI ? r ; c ; 1 R]. The VT510 manual shows no
      [?]; xterm sends it, as DEC's other answers to [CSI ? ... n] carry it, and
      it keeps the answer apart from a modified F3 key ([CSI 1 ; 2 R]);
    - DECRQM for a DEC private mode ([CSI ? Ps $ p]) with [CSI ? Ps ; Pm $ y],
      [Pm] 1 for set, 2 for reset and 0 for a mode not in {!type-modes}; 47,
      1047 and 1049 report the alternate screen. ctlseqs gives DECRQM to a VT300
      and up; the private form is answered anyway, since programs ask it, and
      the ANSI form ([CSI Ps $ p]) is not;
    - OSC 10, 11 and 12 with [?] (the default foreground, background and cursor
      colour) with [OSC Ps ; rgb:RRRR/GGGG/BBBB], and OSC 4 with [c ; ?]
      (palette entry [c]) with [OSC 4 ; c ; rgb:RRRR/GGGG/BBBB], ended as the
      query was. As in xterm, a further [?] asks for the next colour and OSC 4
      takes several pairs. A colour they set is ignored.

    {2:resets Resets}

    RIS returns to the power-up state. DECSTR returns each terminal setting to
    its power-up value: insert, origin, autowrap, reverse-wrap and national
    modes, cursor keys, cursor visibility and the margins; the pen and the
    character sets to their defaults; both DECSC slots to the power-up cursor
    state. The VT510's table gives "No autowrap", its own power-up value; this
    terminal powers up with autowrap on, so DECSTR sets it on. The screen, the
    cursor position, the active screen, bracketed paste, the tab stops and the
    title stay. *)

type t
(** The type for terminal emulators. Mutable, not thread-safe. *)

(** {1:create Creating} *)

val create :
  ?scrollback:int ->
  ?width_method:Matrix_text.width_method ->
  ?default_fg:Ansi.Color.t ->
  ?default_bg:Ansi.Color.t ->
  ?palette:([ `Fg | `Bg | `Cursor | `Index of int ] -> Ansi.Color.t) ->
  rows:int ->
  cols:int ->
  unit ->
  t
(** [create ~rows ~cols ()] is a terminal in its power-up state, with:
    - [scrollback], the number of lines the scrollback keeps, the oldest dropped
      first. Defaults to [10000]; [0] keeps none.
    - [width_method], how wide characters are. Defaults to [`Unicode].
    - [default_fg] and [default_bg], the colours cells take when the pen sets
      none or sets the default (SGR 39, 49). Default to {!Ansi.Color.white} and
      {!Ansi.Color.black}; {!Ansi.Color.default_fg} and {!Ansi.Color.default_bg}
      keep the terminal default in the cells.
    - [palette], the colours the terminal reports ({!section-replies}): [`Fg],
      [`Bg] and [`Cursor] for OSC 10, 11 and 12, [`Index i] for entry [i] of OSC
      4. Defaults to [default_fg] for [`Fg] and [`Cursor], [default_bg] for
      [`Bg] and [Ansi.Color.indexed i] for [`Index i]. A colour whose intent is
      [Default] is not reported: only the renderer knows it.

    [rows] and [cols] below [1] are taken as [1].

    Raises [Invalid_argument] if [cols > Matrix_grid.max_width]. *)

(** {1:feeding Feeding output} *)

val feed : t -> bytes -> int -> int -> string
(** [feed t b off len] applies the [len] bytes of [b] from [off] and is the
    replies they asked for ({!section-replies}), in order: write them to the
    program's input. It is [""], with no allocation, when there are none. A
    sequence split across calls is answered by the call that completes it.

    Raises [Invalid_argument] if [off] and [len] do not designate a range of
    [b]. *)

val feed_string : t -> string -> string
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

val wrapped : t -> int -> bool
(** [wrapped t y] is the wrap mark of row [y] of the active screen, or for
    [y < 0] of the [-y]th newest scrollback line. [false] outside them. *)

type modes = {
  insert : bool;  (** IRM (SM 4). *)
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

(** {1:scrollback Scrollback} *)

val scrollback_capacity : t -> int
(** [scrollback_capacity t] is the number of lines the scrollback keeps. *)

val scrollback_size : t -> int
(** [scrollback_size t] is the number of lines in the scrollback, [0] while the
    alternate screen is active. *)

val scrollback_lines : t -> string list
(** [scrollback_lines t] are the scrollback's lines, oldest first, as text
    without trailing spaces. Empty while the alternate screen is active. *)

val render_with_scrollback : t -> offset:int -> Matrix_grid.t -> unit
(** [render_with_scrollback t ~offset dst] draws into [dst] the view [offset]
    lines above the live screen: the scrollback lines at the top, oldest first,
    then the screen. [offset] is clamped to \[[0];{!scrollback_size}\]; [0]
    shows the live screen. Scrollback lines keep their rendition and hyperlinks.
    [dst] should be as wide as [t]. On the alternate screen it copies the
    screen. *)

(** {1:dirty Changes}

    A renderer redraws what changed since it last looked. Writing cells, moving
    the cursor or changing its visibility, and changing the rendition mark [t]
    changed; the title and the other modes do not. *)

val is_dirty : t -> bool
(** [is_dirty t] is [true] iff [t] changed since {!clear_dirty}. *)

val dirty_rows : t -> int list
(** [dirty_rows t] are the rows whose cells changed since {!clear_dirty}, in
    increasing order. *)

val is_cursor_dirty : t -> bool
(** [is_cursor_dirty t] is [true] iff the cursor moved or was shown or hidden
    since {!clear_dirty}. *)

val clear_dirty : t -> unit
(** [clear_dirty t] forgets the changes. *)

(** {1:debug Debugging} *)

val to_string : t -> string
(** [to_string t] is the text of the screen, a line a row, separated by
    newlines, trailing spaces kept. *)
