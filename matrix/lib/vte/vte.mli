(** Virtual Terminal Emulator.

    A high-performance terminal emulator providing ANSI/VT100 escape sequence
    processing with primary and alternate screen buffers. Designed for embedding
    in terminal applications, the VTE maintains complete terminal state
    including cursor position, style, scrollback history, and mode flags.

    {1 Overview}

    The VTE processes raw input bytes containing ANSI escape sequences and plain
    text, updating an internal grid that represents the visible terminal screen.
    Applications render this grid to display terminal output.

    Key features:
    - {b ANSI/VT100 compatibility}: CSI, SGR, OSC sequences
    - {b Dual buffers}: Primary screen with scrollback, alternate screen without
    - {b Grapheme support}: Full Unicode with wide character handling
    - {b Incremental processing}: Stream-based input handling
    - {b Dirty tracking}: Efficient render-on-change detection

    {1 Quick Start}

    {[
      (* Create a VTE *)
      let vte = Vte.create ~rows:24 ~cols:80 () in

      (* Feed terminal output *)
      Vte.feed_string vte "\027[1;31mHello\027[0m World\n";

      (* Access the grid for rendering *)
      let grid = Vte.grid vte in

      (* Check cursor position *)
      let row, col = Vte.cursor_pos vte in
      Printf.printf "Cursor at (%d, %d)\n" row col
    ]}

    {1 Terminal Processing}

    The VTE accepts raw byte streams and processes them incrementally:

    {[
      let vte = Vte.create ~rows:24 ~cols:80 () in

      (* Process input in chunks *)
      let process_output channel =
        let buf = Bytes.create 4096 in
        let rec loop () =
          match input channel buf 0 4096 with
          | 0 -> ()
          | n ->
              Vte.feed vte buf 0 n;
              if Vte.is_dirty vte then (
                render_grid (Vte.grid vte);
                Vte.clear_dirty vte);
              loop ()
        in
        loop ()
    ]}

    {1 Screen Management}

    The VTE maintains two screen buffers:

    - {b Primary screen}: Persistent grid with optional scrollback history
    - {b Alternate screen}: Temporary grid for full-screen apps (no scrollback)

    Applications like vim and less use the alternate screen to preserve the
    terminal content when they exit. The VTE automatically switches between
    buffers in response to DECSET/DECRST sequences.

    {[
      (* Query current screen *)
      if Vte.is_alternate_screen vte then
        print_endline "Running in alternate screen"
      else print_endline "On main screen"
    ]}

    {1 Scrollback}

    The primary screen supports a configurable scrollback buffer implemented as
    an efficient ring buffer. Scrollback is disabled for the alternate screen.

    {[
      (* Create with 10000 lines of scrollback *)
      let vte = Vte.create ~scrollback:10000 ~rows:24 ~cols:80 () in

      (* Query scrollback *)
      let capacity = Vte.scrollback_capacity vte in
      let current_size = Vte.scrollback_size vte in

      (* Access scrollback content *)
      let lines = Vte.scrollback_lines vte in
      List.iter print_endline lines
    ]}

    {1 Dirty Tracking}

    The VTE maintains granular dirty tracking to optimize rendering. A global
    dirty flag tracks whether any changes occurred, while row-level tracking
    enables partial screen updates for improved performance. Only mutations of
    the grid, cursor, or active style flip this flag; metadata changes such as
    {!title} updates or mode toggles must be tracked separately.

    {[
      let render_loop vte =
        let rec loop () =
          (* Process input... *)
          if Vte.is_dirty vte then (
            (* Option 1: Full redraw *)
            render_grid (Vte.grid vte);

            (* Option 2: Partial update (more efficient) *)
            let rows = Vte.dirty_rows vte in
            List.iter (fun row -> render_row (Vte.grid vte) row) rows;

            (* Update cursor if needed *)
            if Vte.is_cursor_dirty vte then render_cursor vte;

            Vte.clear_dirty vte);
          Unix.sleepf 0.016;
          (* ~60 FPS *)
          loop ()
        in
        loop ()
    ]}

    {1 Terminal Modes}

    The VTE tracks various terminal modes that affect behavior:

    - {b Auto-wrap mode}: Wrap text at right margin (DECAWM)
    - {b Insert mode}: Insert vs replace characters (IRM)
    - {b Cursor key mode}: Application vs normal cursor keys (DECCKM)
    - {b Bracketed paste}: Distinguish pasted vs typed text
    - {b Origin mode}: Cursor positioning relative to scroll region (DECOM).
      Tracking exists for future DECOM support, but handling is not wired yet so
      {!origin_mode} currently always reports [false].

    {[
      (* Check mode status *)
      if Vte.auto_wrap_mode vte then print_endline "Auto-wrap enabled"
    ]}

    {1 Performance Characteristics}

    - {b Feed}: O(n) where n is input bytes. Amortized constant per character.
    - {b Grid access}: O(1) reference to current grid
    - {b Scrollback access}: O(k) where k is number of scrollback lines
    - {b Resize}: O(rows × cols) for grid resize. Scrollback storage is already
      compressed and is unaffected by window size changes.
    - {b Memory}: O(scrollback_capacity × average_line_length) for compressed
      storage, typically 100-500 bytes per line

    {1 Invariants}

    The VTE maintains these invariants:

    - Cursor row always satisfies 0 <= row < rows. Column always satisfies 0 <=
      col <= cols, and col = cols encodes the pending wrap column required by
      DECAWM.
    - Scroll region always within \[0, rows)
    - Active grid is either primary or alternate (never both)
    - Dirty flag only covers grid, cursor, or style mutations
    - Scrollback only accumulates on primary screen *)

type t
(** Mutable VTE instance.

    Encapsulates terminal state including visible grid, cursor position, style,
    mode flags, scrollback buffer, and parser state.

    Not thread-safe: concurrent modifications from multiple threads result in
    undefined behavior. *)

(** {1 Creation and Configuration} *)

val create :
  ?scrollback:int ->
  ?glyph_pool:Glyph.Pool.t ->
  ?width_method:Glyph.width_method ->
  ?respect_alpha:bool ->
  rows:int ->
  cols:int ->
  unit ->
  t
(** [create ?scrollback ?glyph_pool ?width_method ?respect_alpha ~rows ~cols ()]
    creates a VTE instance.

    @param scrollback
      Maximum scrollback lines (ring buffer). Defaults to 10000. Use 0 to
      disable. Only applies to primary screen; alternate screen never has
      scrollback.
    @param glyph_pool
      Shared grapheme pool for efficient multi-width character storage. If
      omitted, creates a new pool shared between primary/alternate grids and
      scrollback.
    @param width_method
      Grapheme width computation method. Defaults to [`Unicode] so full Unicode
      display widths are honoured. See {!Glyph.width_method} for available
      methods.
    @param respect_alpha
      Enable alpha blending for semi-transparent colors. Defaults to [false].
    @param rows Terminal height in lines. Clamped to \[1, ∞).
    @param cols Terminal width in columns. Clamped to \[1, ∞).
    @return
      A fresh VTE instance in initial state. Both screens start empty with
      default style (terminal default foreground and background). Cursor is at
      (0, 0) and visible. All terminal modes are reset to defaults: auto-wrap
      on, insert mode off, origin mode off, cursor key mode off, bracketed paste
      off. Scroll region spans the full screen. *)

(** {1 Terminal Dimensions} *)

val rows : t -> int
(** [rows t] returns the terminal height. *)

val cols : t -> int
(** [cols t] returns the terminal width. *)

val resize : t -> rows:int -> cols:int -> unit
(** [resize t ~rows ~cols] resizes the terminal.

    Primary and alternate grids are resized independently. Both grids are
    cleared after resize to match terminal behavior where applications repaint
    the screen. Cursor and scroll region are clamped to new bounds. Scrollback
    content is preserved (compressed storage adapts to new width during
    decompression).

    Parameters are clamped to minimum of 1.

    Marks the VTE dirty. *)

(** {1 Input Processing} *)

val feed : t -> bytes -> int -> int -> unit
(** [feed t bytes ofs len] processes [len] bytes starting at [ofs] in [bytes] as
    terminal input.

    Accepts partial and mixed text/escape sequences. Invalid or unrecognized
    sequences are ignored without raising exceptions. Updates the active grid,
    cursor position and visibility, terminal title, current style, and mode
    flags based on parsed sequences.

    Preconditions: [0 <= ofs] and [ofs + len <= Bytes.length bytes].

    Invalid arguments or out-of-bounds access result in undefined behavior.

    Time complexity: O(len), amortized constant time per byte. *)

val feed_string : t -> string -> unit
(** [feed_string t s] processes [s] as terminal input.

    Convenience wrapper around {!feed}. *)

(** {1 Terminal State} *)

val grid : t -> Grid.t
(** [grid t] returns the current visible grid (primary or alternate).

    The grid is mutable and shared with the VTE. External modifications break
    dirty tracking and may cause rendering inconsistencies.

    Complexity: O(1) reference to active grid. *)

val title : t -> string
(** [title t] returns the terminal title set via OSC 0 or OSC 2 sequences. *)

val is_dirty : t -> bool
(** [is_dirty t] is [true] if grid/cursor/style state changed since the last
    call to {!clear_dirty}.

    Applications can poll this to skip redundant rendering. The flag flips when
    draw operations, cursor moves, or SGR changes modify the visible cells. OSC
    title updates and mode toggles do not mark the VTE dirty; re-render those
    manually if you display them. *)

val dirty_rows : t -> int list
(** [dirty_rows t] returns a list of row indices that have been modified.

    Row indices are 0-based and sorted in ascending order. Empty list if no rows
    are dirty. Enables efficient partial screen updates by only rendering
    modified rows.

    Complexity: O(k) where k is number of dirty rows (tree traversal only; no
    re-sorting required).

    Example:
    {[
      if Vte.is_dirty vte then (
        let rows = Vte.dirty_rows vte in
        List.iter (fun row -> render_row (Vte.grid vte) row) rows;
        Vte.clear_dirty vte)
    ]} *)

val is_cursor_dirty : t -> bool
(** [is_cursor_dirty t] is [true] if cursor state changed.

    Tracks cursor position and visibility changes. *)

val clear_dirty : t -> unit
(** [clear_dirty t] resets dirty tracking state.

    Clears the dirty flag, dirty row set, and cursor dirty flag. Does not alter
    content. Typically called after rendering. *)

val is_alternate_screen : t -> bool
(** [is_alternate_screen t] is [true] when the alternate screen is active.

    Switched via DECSET/DECRST 1047, 1049, or legacy 47 sequences.

    Defaults to [false] (primary screen active). *)

(** {1 Cursor Information} *)

val cursor_pos : t -> int * int
(** [cursor_pos t] returns the current cursor position [(row, col)].

    Satisfies [0 <= row < rows t] and [0 <= col <= cols t]. Note that [col] can
    equal [cols] for pending wrap state when auto-wrap mode is enabled. *)

val cursor_visible : t -> bool
(** [cursor_visible t] is [true] if the cursor is visible.

    Controlled via DECTCEM (CSI ?25h/l).

    Defaults to [true]. *)

val set_cursor_pos : t -> row:int -> col:int -> unit
(** [set_cursor_pos t ~row ~col] moves the cursor to [(row, col)].

    Out-of-bounds values are clamped to the valid range: [row] is clamped to
    \[0, rows-1\], [col] is clamped to \[0, cols\]. Note that [col] can equal
    [cols] to support auto-wrap positioning; the cursor wraps to the next line
    when the next character is written.

    Marks cursor dirty if position changes. *)

val set_cursor_visible : t -> bool -> unit
(** [set_cursor_visible t visible] shows or hides the cursor.

    Marks cursor dirty if visibility changes. *)

(** {1 Scrollback} *)

val scrollback_capacity : t -> int
(** [scrollback_capacity t] returns the maximum scrollback lines.

    Returns 0 if scrollback is disabled. *)

val scrollback_size : t -> int
(** [scrollback_size t] returns the current number of lines in scrollback.

    Returns 0 if scrollback disabled, empty, or on alternate screen. Satisfies
    [0 <= scrollback_size t <= scrollback_capacity t]. *)

val scrollback_lines : t -> string list
(** [scrollback_lines t] returns scrollback content as plain text lines.

    Lines are ordered oldest to newest. Returns plain text without style
    information (colors and attributes removed). Trailing spaces are trimmed
    during compression to keep storage bounded, so these strings may be shorter
    than the terminal width. Returns empty list if scrollback is disabled or
    empty, or if on alternate screen.

    Complexity: O(scrollback_size); strings are reused from the compressed ring
    buffer. *)

val render_with_scrollback : t -> offset:int -> Grid.t -> unit
(** [render_with_scrollback t ~offset dst] renders terminal content with
    scrollback into [dst].

    Produces a snapshot that combines scrollback history with the visible
    screen. Scrollback rows occupy the top of [dst]: the first row is the oldest
    line in the requested history window (furthest from the live screen), and
    the last scrollback row is the newest line shown (closest to the live
    screen). The remaining rows are filled with the current screen starting at
    the top.

    The [offset] parameter specifies how many lines above the live screen the
    snapshot extends. [offset = 0] shows the live terminal. [offset = 10] shows
    the 10 most recent scrollback lines above the screen, or fewer if [dst] is
    shorter. Only [min offset (Grid.height dst)] history rows can be displayed
    at a time.

    The destination grid should have the same width as the terminal for proper
    glyph placement; smaller widths will clip decompressed graphemes.

    If [offset] exceeds available scrollback, only available history is shown.
    If on alternate screen (no scrollback), behaves identically to
    [Grid.blit ~src:(grid t) ~dst].

    @param offset
      Number of lines scrolled back from bottom. Clamped to \[0,
      scrollback_size\]. Offset 0 shows the live terminal without history.
    @param dst
      Destination grid to render into. Should ideally match terminal width for
      correct decompression of scrollback lines.

    Example:
    {[
      (* Create a larger viewport showing scrollback *)
      let view = Grid.create ~width:80 ~height:34 () in
      Vte.render_with_scrollback vte ~offset:10 view
      (* view shows 10 lines of scrollback + 24 lines of current screen *)
    ]}

    Time complexity: O(Grid.height dst × cols). Each scrollback line
    decompression is O(cols). *)

(** {1 Terminal Modes} *)

val cursor_key_mode : t -> bool
(** [cursor_key_mode t] is [true] when cursor keys use application mode.

    Application mode sends different escape sequences for arrow keys (e.g.,
    [ESC O A] vs [ESC \[ A]). Controlled via DECCKM (CSI ?1h/l).

    Defaults to [false] (normal mode). *)

val insert_mode : t -> bool
(** [insert_mode t] is [true] when insert mode is active.

    In insert mode, new characters push existing characters right rather than
    replacing them. Controlled via IRM (CSI 4h/l).

    Defaults to [false] (replace mode). *)

val auto_wrap_mode : t -> bool
(** [auto_wrap_mode t] is [true] when automatic wrap mode is active.

    When enabled, writing past the right margin wraps to the next line.
    Otherwise, cursor stays at right edge. Controlled via DECAWM (CSI ?7h/l).

    Default: [true]. *)

val bracketed_paste_mode : t -> bool
(** [bracketed_paste_mode t] is [true] when bracketed paste mode is active.

    When enabled, pasted text is wrapped with [ESC \[ 200 ~] and [ESC \[ 201 ~]
    markers, allowing applications to distinguish pasted from typed input.
    Controlled via CSI ?2004h/l.

    Defaults to [false]. *)

val origin_mode : t -> bool
(** [origin_mode t] is [true] when origin mode is active.

    In origin mode, cursor positioning commands (CUP, HVP) use coordinates
    relative to the scroll region instead of the entire screen. Controlled via
    DECOM (CSI ?6h/l). Handling for DECOM is not wired yet, so this accessor
    always reports [false] in the current release.

    Defaults to [false] (absolute coordinates). *)

(** {1 Grid Manipulation} *)

val scroll_up : t -> int -> unit
(** [scroll_up t n] scrolls up by [n] lines within the current scroll region.

    Lines scrolled off the top enter scrollback only when the primary screen is
    active, scrollback is enabled, and the scroll region starts at row 0. Newly
    exposed rows at the bottom are cleared to spaces with the default
    transparent background (alpha 0), independent of the current SGR style. No
    effect if [n <= 0].

    The scroll region is set via DECSTBM (CSI [top];[bottom]r) escape sequences
    and defaults to the full screen. Scrolling only affects rows within this
    region.

    Marks all rows in the scroll region as dirty if [n > 0].

    Complexity: O(n × cols) for content shift, O(n × cols) for scrollback
    compression if applicable. *)

val scroll_down : t -> int -> unit
(** [scroll_down t n] scrolls down by [n] lines within the current scroll
    region.

    Blank rows (spaces with the default transparent background) are inserted at
    the top of the scroll region. Lines at the bottom are discarded (never saved
    to scrollback). No effect if [n <= 0].

    This is the reverse of {!scroll_up}, used for reverse index (RI, CSI T)
    terminal operations when the cursor moves above the scroll region.

    Marks all rows in the scroll region as dirty if [n > 0]. *)

(** {1 Terminal Control} *)

val reset : t -> unit
(** [reset t] resets the VTE to initial state.

    Resets terminal to initial state: clears both grids and scrollback, resets
    cursor to (0, 0) and makes it visible, resets style to default, resets all
    modes (auto-wrap on, others off), switches to primary screen if needed, and
    resets scroll region to full screen. Terminal dimensions (rows, cols) remain
    unchanged.

    Equivalent to RIS (Reset to Initial State) escape sequence [ESC c].

    Marks all rows as dirty. *)

(** {1 Debugging} *)

val to_string : t -> string
(** [to_string t] returns the visible screen content as a multi-line string.

    The textual representation reflects exactly what a terminal screen shows:
    - One line per terminal row (no rows omitted), separated by newlines.
    - Trailing spaces are preserved; blank cells appear as spaces.
    - Graphemes are rendered once even when they span multiple columns.
    - Styling (colors/attributes) is not included.

    The string does not end with a trailing newline. *)
