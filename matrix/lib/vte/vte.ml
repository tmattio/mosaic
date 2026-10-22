module Grid = Matrix_grid
module Text = Matrix_text
module Params = Ansi.Parser.Params

(* {1:state State} *)

(* [last_column] is DEC's Last Column Flag: the cursor is on the character just
   printed in the last column, and the next one wraps first. It implies
   [col = cols - 1]. Only [set_cursor] changes the cursor, for [goto] and
   printing; a saved cursor is a copy, never changed. *)
type cursor = {
  mutable row : int;
  mutable col : int;
  mutable last_column : bool;
}

(* [style] is the SGR rendition, never with a link: OSC 8 sets [link]. *)
type pen = { style : Ansi.Style.t; link : string option; protected : bool }

(* A 94-character set: ASCII, or the code points that replace 0x20..0x7F. *)
type charset = Ascii | Mapped of int array

(* [g] is never mutated: a designation makes a new array. *)
type charsets = { g : charset array; gl : int }

(* What DECSC saves. *)
type saved = { cursor : cursor; pen : pen; charsets : charsets; origin : bool }

(* A screen's cells, the wrap mark of each row (a byte), and its DECSC slot.
   The screen functions below are the only ones that change [grid] and
   [wrapped]. *)
type screen = {
  grid : Grid.t;
  mutable wrapped : Bytes.t;
  mutable saved : saved;
}

type margins = { top : int; bottom : int }

type modes = {
  insert : bool;
  origin : bool;
  auto_wrap : bool;
  reverse_wrap : bool;
  cursor_keys : bool;
  cursor_visible : bool;
  bracketed_paste : bool;
  alternate_screen : bool;
}

(* A scrollback line: its text, the style of each run of it from
   [start_byte], and its wrap mark. *)
type style_run = { start_byte : int; style : Ansi.Style.t }
type line = { text : string; styles : style_run array; wrapped : bool }

(* A ring of the newest lines: [head] is where the next one goes. *)
type history = { lines : line array; mutable head : int; mutable count : int }

(* The display unit in progress, the text printed since the last unit boundary
   or control function: its bytes after translation, its width as the
   segmenter measures it, and whether it [grew] since it was last placed.
   Placed, it covers [cells] cells from [(row, col)]; joined, having no width,
   its bytes follow those of the grapheme at [(row, col)], whose text was
   [prev]. *)
type placement = Nowhere | Cells | Joined

type cluster = {
  seg : Text.Segmenter.t;
  mutable text : Bytes.t;
  mutable len : int;
  mutable width : int;
  mutable grown : bool;
  mutable at : placement;
  mutable row : int;
  mutable col : int;
  mutable cells : int;
  mutable prev : string;
}

(* An OSC is kept only when the VTE performs it: [Osc_number n] while its
   number is read (-1 before the first digit), [Osc_text n] once it is known
   to be kept, and [Osc_skip] otherwise. *)
type osc = Osc_skip | Osc_number of int | Osc_text of int

(* The rows changed, a byte a row. *)
type dirty = {
  mutable any : bool;
  mutable cursor : bool;
  mutable rows : Bytes.t;
}

type t = {
  mutable rows : int;
  mutable cols : int;
  primary : screen;
  alternate : screen;
  history : history;
  cursor : cursor;
  mutable pen : pen;
  mutable charsets : charsets;
  mutable modes : modes;
  mutable margins : margins;
  mutable tabs : bool array;
  mutable title : string;
  cluster : cluster;
  default_fg : Ansi.Color.t;
  default_bg : Ansi.Color.t;
  parser : Ansi.Parser.t;
  mutable osc : osc;
  osc_text : Buffer.t;
  (* The replies owed, until [take_replies], and the identity given. *)
  replies : Buffer.t;
  da1 : string;
  da2 : string;
  dirty : dirty;
}

let top_left = { row = 0; col = 0; last_column = false }
let plain_pen = { style = Ansi.Style.default; link = None; protected = false }
let ascii_sets = { g = [| Ascii; Ascii; Ascii; Ascii |]; gl = 0 }

let power_up =
  { cursor = top_left; pen = plain_pen; charsets = ascii_sets; origin = false }

let power_up_modes =
  {
    insert = false;
    origin = false;
    auto_wrap = true;
    reverse_wrap = false;
    cursor_keys = false;
    cursor_visible = true;
    bracketed_paste = false;
    alternate_screen = false;
  }

(* Tab stops every eight columns, as at power-up. *)
let default_tabs cols = Array.init cols (fun i -> i > 0 && i mod 8 = 0)
let empty_line = { text = ""; styles = [||]; wrapped = false }

let create ?(scrollback = 10000) ?(width_method = `Unicode)
    ?(default_fg = Ansi.Color.white) ?(default_bg = Ansi.Color.black)
    ?(da1 = "\027[?1;2c") ?(da2 = "\027[>0;0;0c") ~rows ~cols () =
  let rows = max 1 rows and cols = max 1 cols in
  let screen () =
    let grid = Grid.create ~width:cols ~height:rows ~width_method () in
    Grid.clear ~color:default_bg grid;
    { grid; wrapped = Bytes.make rows '\000'; saved = power_up }
  in
  {
    rows;
    cols;
    primary = screen ();
    alternate = screen ();
    history =
      { lines = Array.make (max 0 scrollback) empty_line; head = 0; count = 0 };
    cursor = { row = 0; col = 0; last_column = false };
    pen = plain_pen;
    charsets = ascii_sets;
    modes = power_up_modes;
    margins = { top = 0; bottom = rows - 1 };
    tabs = default_tabs cols;
    title = "";
    cluster =
      {
        seg = Text.Segmenter.create width_method;
        text = Bytes.create 16;
        len = 0;
        width = 0;
        grown = false;
        at = Nowhere;
        row = 0;
        col = 0;
        cells = 0;
        prev = "";
      };
    default_fg;
    default_bg;
    parser = Ansi.Parser.create ();
    osc = Osc_skip;
    osc_text = Buffer.create 64;
    replies = Buffer.create 16;
    da1;
    da2;
    dirty = { any = false; cursor = false; rows = Bytes.make rows '\000' };
  }

let screen t = if t.modes.alternate_screen then t.alternate else t.primary
let grid t = (screen t).grid

(* A colour of the pen, or the default for none and for SGR 39/49. *)
let resolve ~default = function
  | None -> default
  | Some c -> (
      match Ansi.Color.intent c with Default -> default | Indexed _ | Rgb -> c)

let fg t = resolve ~default:t.default_fg t.pen.style.fg

(* The background of what is printed, and of every blank. *)
let bg t = resolve ~default:t.default_bg t.pen.style.bg
let protected_yes = Some true
let protection t = if t.pen.protected then protected_yes else None

(* {1:dirty Changes} *)

let touch_row t row =
  t.dirty.any <- true;
  Bytes.unsafe_set t.dirty.rows row '\001'

let touch_rows t top bottom =
  if top <= bottom then (
    t.dirty.any <- true;
    Bytes.fill t.dirty.rows top (bottom - top + 1) '\001')

let set_cursor t ~row ~col ~last_column =
  let c = t.cursor in
  if row <> c.row || col <> c.col then (
    t.dirty.any <- true;
    t.dirty.cursor <- true);
  c.row <- row;
  c.col <- col;
  c.last_column <- last_column

(* Every function that sets the cursor position does it here, so it clears
   the Last Column Flag. *)
let goto t ~row ~col =
  let row = max 0 (min row (t.rows - 1))
  and col = max 0 (min col (t.cols - 1)) in
  set_cursor t ~row ~col ~last_column:false

(* {1:history Scrollback} *)

(* Styles are stored only when they change. Trailing spaces are dropped
   unless they show a background, an attribute or a link. *)
let compress_row t (s : screen) row =
  let g = s.grid and cols = t.cols in
  let buf = Buffer.create (cols * 2) in
  let styles = ref [] in
  let current_fg = ref Ansi.Color.default_fg in
  let current_bg = ref Ansi.Color.default_bg in
  let current_attrs = ref 0 in
  let current_link = ref "" in
  let content_end = ref 0 in
  for col = 0 to cols - 1 do
    let idx = (row * cols) + col in
    if not (Grid.is_continuation g idx) then begin
      let text = if Grid.is_empty g idx then " " else Grid.get_text g idx in
      let fg = Grid.get_fg g idx and bg = Grid.get_bg g idx in
      let attrs = Grid.get_attrs g idx and link = Grid.get_link g idx in
      if
        !styles = []
        || (not (Ansi.Color.equal fg !current_fg))
        || (not (Ansi.Color.equal bg !current_bg))
        || attrs <> !current_attrs || link <> !current_link
      then begin
        styles :=
          { start_byte = Buffer.length buf; style = Grid.get_style g idx }
          :: !styles;
        current_fg := fg;
        current_bg := bg;
        current_attrs := attrs;
        current_link := link
      end;
      Buffer.add_string buf text;
      if
        text <> " "
        || Ansi.Color.Packed.alpha (Ansi.Color.Packed.encode bg) > 0
           && not (Ansi.Color.equal bg t.default_bg)
        || attrs <> 0 || link <> ""
      then content_end := Buffer.length buf
    end
  done;
  let text = Buffer.sub buf 0 !content_end in
  let rec trim_runs = function
    | run :: rest when run.start_byte >= !content_end -> trim_runs rest
    | runs -> runs
  in
  {
    text;
    styles = Array.of_list (List.rev (trim_runs !styles));
    wrapped = Bytes.get s.wrapped row <> '\000';
  }

let decompress_line line grid row =
  let col = ref 0 in
  let width_method = Grid.width_method grid in
  for i = 0 to Array.length line.styles - 1 do
    let run = line.styles.(i) in
    let stop =
      if i + 1 = Array.length line.styles then String.length line.text
      else line.styles.(i + 1).start_byte
    in
    if !col < Grid.width grid then begin
      let text = String.sub line.text run.start_byte (stop - run.start_byte) in
      Grid.draw_text grid ~x:!col ~y:row ~style:run.style ~text;
      col := !col + Text.measure ~width_method ~tab_width:2 text
    end
  done

let push_line t s row =
  let h = t.history in
  let capacity = Array.length h.lines in
  if capacity > 0 then (
    h.lines.(h.head) <- compress_row t s row;
    h.head <- (h.head + 1) mod capacity;
    h.count <- min capacity (h.count + 1))

(* The [i]th newest line, [i < count]. *)
let newest h i =
  let capacity = Array.length h.lines in
  h.lines.((h.head - 1 - i + capacity) mod capacity)

let clear_history t =
  let h = t.history in
  Array.fill h.lines 0 (Array.length h.lines) empty_line;
  h.head <- 0;
  h.count <- 0

(* {1:screen Changing the screen}

   Every change of cells goes through these functions, and each clears the
   Last Column Flag: with [goto], that is the rule "every function that sets
   the cursor position or changes cells clears it". Printing sets the flag
   after its changes. Every blank is on the pen's background. *)

let changed t ~top ~bottom =
  touch_rows t top bottom;
  t.cursor.last_column <- false

let unwrap t ~top ~bottom =
  Bytes.fill (screen t).wrapped top (bottom - top + 1) '\000'

(* A wrap mark asserts that the row's last cell is followed by the next row,
   so an erase that reaches the last column clears it. *)
let erase_with f t ~x ~y ~width ~height =
  if width > 0 && height > 0 then (
    f (grid t) { Grid.x; y; width; height };
    if x + width = t.cols then unwrap t ~top:y ~bottom:(y + height - 1);
    changed t ~top:y ~bottom:(y + height - 1))

let erase t =
  erase_with
    (fun g { Grid.x; y; width; height } ->
      Grid.clear_rect ~color:(bg t) g ~x ~y ~width ~height)
    t

let erase_unprotected = erase_with Grid.erase_unprotected

(* Move rows [top] to [bottom] up by [n] rows, or down for [n < 0], with
   their marks. *)
let shift_rows t ~top ~bottom n =
  let height = bottom - top + 1 in
  let k = min (abs n) height in
  Grid.shift (grid t)
    { Grid.x = 0; y = top; width = t.cols; height }
    ~dx:0 ~dy:(-n) ~color:(bg t);
  let marks = (screen t).wrapped in
  if n > 0 then (
    Bytes.blit marks (top + k) marks top (height - k);
    Bytes.fill marks (bottom - k + 1) k '\000')
  else (
    Bytes.blit marks top marks (top + k) (height - k);
    Bytes.fill marks top k '\000');
  changed t ~top ~bottom

(* Move the cells of rows [top] to [bottom] from column [x] by [dx] columns.
   The rows' last cells change, so their marks go. *)
let shift_cols t ~x ~top ~bottom dx =
  if dx <> 0 then (
    Grid.shift (grid t)
      { Grid.x; y = top; width = t.cols - x; height = bottom - top + 1 }
      ~dx ~dy:0 ~color:(bg t);
    unwrap t ~top ~bottom;
    changed t ~top ~bottom)

let mark_wrapped t row = Bytes.set (screen t).wrapped row '\001'

(* {1:scroll Scrolling} *)

(* Rows leaving the top of a scroll region that starts at the first row of the
   primary screen enter the scrollback. *)
let scroll_up t n =
  let { top; bottom } = t.margins in
  let n = min n (bottom - top + 1) in
  if top = 0 && not t.modes.alternate_screen then
    for row = 0 to n - 1 do
      push_line t t.primary row
    done;
  shift_rows t ~top ~bottom n

let scroll_down t n =
  let { top; bottom } = t.margins in
  shift_rows t ~top ~bottom (-min n (bottom - top + 1))

(* IND: down a row, scrolling at the bottom margin. Below the region the
   cursor stops at the last row. *)
let index t =
  if t.cursor.row = t.margins.bottom then scroll_up t 1
  else goto t ~row:(t.cursor.row + 1) ~col:t.cursor.col

let reverse_index t =
  if t.cursor.row = t.margins.top then scroll_down t 1
  else goto t ~row:(t.cursor.row - 1) ~col:t.cursor.col

(* Autowrap: the text goes on at the start of the next row, which the row
   records. At the last row below the region there is no next row: the line
   starts over on the same row. *)
let wrap t =
  let row = t.cursor.row in
  if row = t.margins.bottom || row < t.rows - 1 then mark_wrapped t row;
  index t;
  goto t ~row:t.cursor.row ~col:0

(* {1:charsets Character sets} *)

(* The set whose [chars] are replaced by [by], in order. *)
let replacing chars by =
  let table = Array.init 96 (fun i -> i + 0x20) in
  let i = ref 0 in
  String.iter
    (fun c ->
      let d = String.get_utf_8_uchar by !i in
      table.(Char.code c - 0x20) <- Uchar.to_int (Uchar.utf_decode_uchar d);
      i := !i + Uchar.utf_decode_length d)
    chars;
  Mapped table

(* DEC Special Graphics (VT100 User Guide table 3-9). *)
let dec_graphics =
  replacing "`abcdefghijklmnopqrstuvwxyz{|}~"
    "\u{25c6}\u{2592}\u{2409}\u{240c}\u{240d}\u{240a}\u{00b0}\u{00b1}\u{2424}\u{240b}\u{2518}\u{2510}\u{250c}\u{2514}\u{253c}\u{23ba}\u{23bb}\u{2500}\u{23bc}\u{23bd}\u{251c}\u{2524}\u{2534}\u{252c}\u{2502}\u{2264}\u{2265}\u{03c0}\u{2260}\u{00a3}\u{00b7}"

let uk = replacing "#" "£"
let dutch = replacing "#@[\\]{|}~" "£¾ĳ½|¨ƒ¼´"
let finnish = replacing "[\\]^`{|}~" "ÄÖÅÜéäöåü"
let french = replacing "#@[\\]{|}~" "£à°ç§éùè¨"
let french_canadian = replacing "@[\\]^`{|}~" "àâçêîôéùèû"
let german = replacing "@[\\]{|}~" "§ÄÖÜäöüß"
let italian = replacing "#@[\\]`{|}~" "£§°çéùàòèì"
let norwegian_danish = replacing "@[\\]^`{|}~" "ÄÆØÅÜäæøåü"
let spanish = replacing "#@[\\]{|}" "£§¡Ñ¿°ñç"
let swedish = replacing "@[\\]^`{|}~" "ÉÄÖÅÜéäöåü"
let swiss = replacing "#@[\\]^_`{|}~" "ùàéçêîèôäöüû"

(* The 94-character sets by their one-byte designators (ctlseqs, SCS). *)
let charset_94 = function
  | '0' -> dec_graphics
  | 'A' -> uk
  | '4' -> dutch
  | 'C' | '5' -> finnish
  | 'R' -> french
  | 'Q' -> french_canadian
  | 'K' -> german
  | 'Y' -> italian
  | 'E' | '6' -> norwegian_danish
  | 'Z' -> spanish
  | 'H' | '7' -> swedish
  | '=' -> swiss
  | _ -> Ascii

let translate t u =
  match t.charsets.g.(t.charsets.gl) with
  | Ascii -> u
  | Mapped table ->
      let c = Uchar.to_int u in
      if c >= 0x20 && c < 0x80 then Uchar.unsafe_of_int table.(c - 0x20) else u

let untranslated t =
  match t.charsets.g.(t.charsets.gl) with Ascii -> true | Mapped _ -> false

(* {1:print Printing} *)

let write_cluster t ~row ~col ~width =
  let cl = t.cluster in
  Grid.set_grapheme (grid t) ~x:col ~y:row ~width ~fg:(fg t) ~bg:(bg t)
    ~attrs:t.pen.style.attrs ?link:t.pen.link ?protected:(protection t)
    (Bytes.unsafe_to_string cl.text)
    ~pos:0 ~len:cl.len;
  touch_row t row

(* Place the cluster from its position, by the four rules of vte.mli. With
   autowrap off it ends at the margin; otherwise, if it does not fit, the rest
   of the line is erased and it moves to the next. In insert mode the line
   moves right by the cells it gains. *)
let place t =
  let cl = t.cluster and m = t.modes in
  let width = min cl.width t.cols in
  if cl.col + width > t.cols && not m.auto_wrap then (
    write_cluster t ~row:cl.row ~col:(t.cols - width) ~width;
    cl.cells <- width;
    set_cursor t ~row:cl.row ~col:(t.cols - 1) ~last_column:false)
  else begin
    if cl.col + width > t.cols then begin
      erase t ~x:cl.col ~y:cl.row ~width:(t.cols - cl.col) ~height:1;
      wrap t;
      cl.row <- t.cursor.row;
      cl.col <- 0;
      cl.cells <- 0
    end;
    if m.insert then
      shift_cols t ~x:(cl.col + cl.cells) ~top:cl.row ~bottom:cl.row
        (width - cl.cells);
    write_cluster t ~row:cl.row ~col:cl.col ~width;
    cl.cells <- width;
    let last = cl.col + width - 1 in
    if last = t.cols - 1 then
      set_cursor t ~row:cl.row ~col:last ~last_column:m.auto_wrap
    else set_cursor t ~row:cl.row ~col:(last + 1) ~last_column:false
  end

(* The grapheme at the cluster's [(row, col)], with [text] and its own width
   and rendition. *)
let rewrite_joined t text =
  let cl = t.cluster and g = grid t in
  let idx = (cl.row * t.cols) + cl.col in
  let link = Grid.get_link g idx in
  Grid.set_grapheme g ~x:cl.col ~y:cl.row
    ~width:(max 1 (Grid.cell_width g idx))
    ~fg:(Grid.get_fg g idx) ~bg:(Grid.get_bg g idx)
    ~attrs:(Ansi.Attr.unpack (Grid.get_attrs g idx))
    ?link:(if link = "" then None else Some link)
    ~protected:(Grid.is_protected g idx) text ~pos:0 ~len:(String.length text);
  touch_row t cl.row

(* A unit of no width that continues nothing joins the grapheme in the cell
   before the cursor, or under it at the last column; with no such cell it
   is dropped (vte.mli says how xterm differs). It is joined as soon as it is seen, so a reader of
   the cells finds it, and taken back if it gains a width. *)
let join t =
  let cl = t.cluster and c = t.cursor in
  let x = if c.last_column then c.col else c.col - 1 in
  if cl.at = Nowhere && x >= 0 then begin
    let g = grid t and base = c.row * t.cols in
    let x = ref x in
    while !x > 0 && Grid.is_continuation g (base + !x) do
      decr x
    done;
    let prev = Grid.get_text g (base + !x) in
    cl.at <- Joined;
    cl.row <- c.row;
    cl.col <- !x;
    cl.prev <- (if prev = "" then " " else prev)
  end;
  if cl.at = Joined then
    rewrite_joined t (cl.prev ^ Bytes.sub_string cl.text 0 cl.len)

(* Place the cluster if it grew since it was last placed. Its first
   placement starts where the cursor is, after the wrap the Last Column Flag
   asks for. *)
let settle t =
  let cl = t.cluster in
  if cl.grown then begin
    cl.grown <- false;
    if cl.width = 0 then join t
    else begin
      if cl.at = Joined then (
        rewrite_joined t cl.prev;
        cl.at <- Nowhere);
      if cl.at = Nowhere then begin
        if t.cursor.last_column && t.modes.auto_wrap then wrap t;
        cl.at <- Cells;
        cl.row <- t.cursor.row;
        cl.col <- t.cursor.col;
        cl.cells <- 0
      end;
      place t
    end
  end

(* The unit in progress is complete. *)
let finish t =
  settle t;
  let cl = t.cluster in
  cl.len <- 0;
  cl.width <- 0;
  cl.at <- Nowhere

(* A control function ends the unit, and the next character starts one. *)
let close t =
  finish t;
  Text.Segmenter.reset t.cluster.seg

let append cl u =
  let n = Uchar.utf_8_byte_length u in
  if cl.len + n > Bytes.length cl.text then begin
    let text = Bytes.create (2 * (cl.len + n)) in
    Bytes.blit cl.text 0 text 0 cl.len;
    cl.text <- text
  end;
  ignore (Bytes.set_utf_8_uchar cl.text cl.len u : int);
  cl.len <- cl.len + n

(* The cluster is placed at boundaries and at the end of a text, not at each
   character, so a run of marks is written once. *)
let print_scalar t u =
  let cl = t.cluster in
  if Text.Segmenter.add cl.seg u then finish t;
  append cl u;
  cl.width <- Text.Segmenter.width cl.seg;
  cl.grown <- true

(* The [k] bytes of [s] from [pos] are units of one ASCII character each
   ({!Text.Segmenter.ascii_run}): place them a row segment at a time, as each
   would be placed alone. The last one stays the unit in progress. *)
let print_run t s pos k =
  finish t;
  let g = grid t and fg = fg t and bg = bg t in
  let attrs = t.pen.style.attrs and link = t.pen.link in
  let protected = protection t in
  let put ~x ~y i =
    Grid.set_grapheme g ~x ~y ~width:1 ~fg ~bg ~attrs ?link ?protected s ~pos:i
      ~len:1
  in
  let i = ref pos and stop = pos + k in
  let last_row = ref 0 and last_col = ref 0 in
  while !i < stop do
    if t.cursor.last_column && t.modes.auto_wrap then wrap t;
    let row = t.cursor.row and col = t.cursor.col in
    let n = min (stop - !i) (t.cols - col) in
    if t.modes.insert then shift_cols t ~x:col ~top:row ~bottom:row n;
    for j = 0 to n - 1 do
      put ~x:(col + j) ~y:row (!i + j)
    done;
    i := !i + n;
    let last = col + n - 1 in
    if last < t.cols - 1 then
      set_cursor t ~row ~col:(last + 1) ~last_column:false
    else if t.modes.auto_wrap then set_cursor t ~row ~col:last ~last_column:true
    else begin
      (* Without autowrap each further character replaces the last column. *)
      if !i < stop then put ~x:last ~y:row (stop - 1);
      i := stop;
      set_cursor t ~row ~col:last ~last_column:false
    end;
    touch_row t row;
    last_row := row;
    last_col := last
  done;
  let cl = t.cluster in
  Bytes.set cl.text 0 s.[stop - 1];
  cl.len <- 1;
  cl.width <- 1;
  cl.at <- Cells;
  cl.row <- !last_row;
  cl.col <- !last_col;
  cl.cells <- 1

let print t s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let k =
      if untranslated t then
        Text.Segmenter.ascii_run t.cluster.seg s ~pos:!i ~len:(len - !i)
      else 0
    in
    if k > 0 then (
      print_run t s !i k;
      i := !i + k)
    else begin
      let d = String.get_utf_8_uchar s !i in
      print_scalar t (translate t (Uchar.utf_decode_uchar d));
      i := !i + Uchar.utf_decode_length d
    end
  done;
  settle t

(* REP prints the unit before it [n] more times. Without autowrap the cursor
   reaches the margin within [cols] prints and each print after it is the
   same. With autowrap, once every row the prints can reach (the screen, the
   scrollback and the row being written) holds the same [per_row] units, the
   state depends on [n] only modulo [per_row], so the work is bounded by the
   screen and the scrollback, whatever [n]. *)
let repetitions t n width =
  if not t.modes.auto_wrap then min n (t.cols + 1)
  else
    let per_row = t.cols / width in
    let saturated = per_row * (t.rows + Array.length t.history.lines + 2) in
    if n <= saturated then n else saturated + ((n - saturated) mod per_row)

let rep t n =
  settle t;
  let cl = t.cluster in
  if cl.at = Cells then
    for _ = 1 to repetitions t n (min cl.width t.cols) do
      cl.at <- Nowhere;
      cl.grown <- true;
      settle t
    done

(* {1:motion Cursor motion} *)

(* Counts are clamped to the distance they act on before any arithmetic, so a
   saturated parameter never overflows. *)
let toward ~from ~limit n =
  if limit >= from then from + min n (limit - from)
  else from - min n (from - limit)

(* Up and down stop at the margin when they start inside the region, and at
   the screen edge otherwise. *)
let cuu t n =
  let { top; _ } = t.margins and row = t.cursor.row in
  let limit = if row >= top then top else 0 in
  goto t ~row:(toward ~from:row ~limit n) ~col:t.cursor.col

let cud t n =
  let { bottom; _ } = t.margins and row = t.cursor.row in
  let limit = if row <= bottom then bottom else t.rows - 1 in
  goto t ~row:(toward ~from:row ~limit n) ~col:t.cursor.col

let cuf t n =
  goto t ~row:t.cursor.row
    ~col:(toward ~from:t.cursor.col ~limit:(t.cols - 1) n)

let wrapped_row t row = row >= 0 && Bytes.get (screen t).wrapped row <> '\000'

(* BS and CUB: [n] columns left. With reverse-wrap (and autowrap) the Last
   Column Flag takes the first step, and a move past the first column goes on
   from the end of the row above while that row wrapped onto this one. *)
let cub t n =
  let c = t.cursor in
  if not (t.modes.reverse_wrap && t.modes.auto_wrap) then
    goto t ~row:c.row ~col:(toward ~from:c.col ~limit:0 n)
  else begin
    let row = ref c.row and col = ref c.col in
    let n = ref (if c.last_column then n - 1 else n) in
    while !n > 0 do
      if !n <= !col then (
        col := !col - !n;
        n := 0)
      else if wrapped_row t (!row - 1) then (
        n := !n - !col - 1;
        decr row;
        col := t.cols - 1)
      else (
        col := 0;
        n := 0)
    done;
    goto t ~row:!row ~col:!col
  end

let cnl t n =
  cud t n;
  goto t ~row:t.cursor.row ~col:0

let cpl t n =
  cuu t n;
  goto t ~row:t.cursor.row ~col:0

let cha t n = goto t ~row:t.cursor.row ~col:(min n t.cols - 1)

(* In origin mode (DECOM) rows count from the top margin and stay in the
   region. *)
let origin_row t n =
  if t.modes.origin then
    let { top; bottom } = t.margins in
    top + min (n - 1) (bottom - top)
  else min n t.rows - 1

let cup t row col = goto t ~row:(origin_row t row) ~col:(min col t.cols - 1)
let vpa t n = goto t ~row:(origin_row t n) ~col:t.cursor.col

(* VPR: down [n] rows, past the bottom margin, but not out of the region in
   origin mode. *)
let vpr t n =
  let limit = if t.modes.origin then t.margins.bottom else t.rows - 1 in
  let row = t.cursor.row in
  goto t
    ~row:(if row >= limit then row else toward ~from:row ~limit n)
    ~col:t.cursor.col

let home t = goto t ~row:(if t.modes.origin then t.margins.top else 0) ~col:0

(* {1:tabs Tab stops} *)

let next_tab t col =
  let rec go x =
    if x >= t.cols - 1 then t.cols - 1 else if t.tabs.(x) then x else go (x + 1)
  in
  go (col + 1)

let prev_tab t col =
  let rec go x = if x <= 0 then 0 else if t.tabs.(x) then x else go (x - 1) in
  go (col - 1)

let tab_steps t n next =
  let col = ref t.cursor.col in
  for _ = 1 to min n t.cols do
    col := next t !col
  done;
  goto t ~row:t.cursor.row ~col:!col

(* HT and CHT stop at the last column, so with the Last Column Flag set they
   do nothing and keep it. *)
let cht t n = if not t.cursor.last_column then tab_steps t n next_tab
let cbt t n = tab_steps t n prev_tab
let hts t = t.tabs.(t.cursor.col) <- true

let tbc t = function
  | 0 -> t.tabs.(t.cursor.col) <- false
  | 3 -> Array.fill t.tabs 0 t.cols false
  | _ -> ()

(* {1:edit Editing} *)

(* ED and DECSED, EL and DECSEL, by the erase they use. *)
let erase_display t erase = function
  | 0 ->
      let ({ row; col; _ } : cursor) = t.cursor in
      erase t ~x:col ~y:row ~width:(t.cols - col) ~height:1;
      erase t ~x:0 ~y:(row + 1) ~width:t.cols ~height:(t.rows - row - 1)
  | 1 ->
      let ({ row; col; _ } : cursor) = t.cursor in
      erase t ~x:0 ~y:0 ~width:t.cols ~height:row;
      erase t ~x:0 ~y:row ~width:(col + 1) ~height:1
  | 2 -> erase t ~x:0 ~y:0 ~width:t.cols ~height:t.rows
  | 3 -> clear_history t
  | _ -> ()

let erase_line t erase n =
  let ({ row; col; _ } : cursor) = t.cursor in
  match n with
  | 0 -> erase t ~x:col ~y:row ~width:(t.cols - col) ~height:1
  | 1 -> erase t ~x:0 ~y:row ~width:(col + 1) ~height:1
  | 2 -> erase t ~x:0 ~y:row ~width:t.cols ~height:1
  | _ -> ()

let ed t n = erase_display t erase n
let decsed t n = erase_display t erase_unprotected n
let el t n = erase_line t erase n
let decsel t n = erase_line t erase_unprotected n

let ech t n =
  let ({ row; col; _ } : cursor) = t.cursor in
  erase t ~x:col ~y:row ~width:(min n (t.cols - col)) ~height:1

let in_region t =
  t.cursor.row >= t.margins.top && t.cursor.row <= t.margins.bottom

(* IL and DL act from a cursor inside the region and return it to the first
   column. *)
let il t n =
  if in_region t then (
    let row = t.cursor.row in
    shift_rows t ~top:row ~bottom:t.margins.bottom
      (-min n (t.margins.bottom - row + 1));
    goto t ~row ~col:0)

let dl t n =
  if in_region t then (
    let row = t.cursor.row in
    shift_rows t ~top:row ~bottom:t.margins.bottom
      (min n (t.margins.bottom - row + 1));
    goto t ~row ~col:0)

let ich t n =
  let ({ row; col; _ } : cursor) = t.cursor in
  shift_cols t ~x:col ~top:row ~bottom:row (min n (t.cols - col))

let dch t n =
  let ({ row; col; _ } : cursor) = t.cursor in
  shift_cols t ~x:col ~top:row ~bottom:row (-min n (t.cols - col))

(* SL, SR, DECIC and DECDC shift the columns of the region's rows, from a
   cursor inside it. *)
let shift_region_cols t ~x dx =
  if in_region t then
    shift_cols t ~x ~top:t.margins.top ~bottom:t.margins.bottom dx

let sl t n = shift_region_cols t ~x:0 (-min n t.cols)
let sr t n = shift_region_cols t ~x:0 (min n t.cols)

let decic t n =
  shift_region_cols t ~x:t.cursor.col (min n (t.cols - t.cursor.col))

let decdc t n =
  shift_region_cols t ~x:t.cursor.col (-min n (t.cols - t.cursor.col))

let su t n = scroll_up t n
let sd t n = scroll_down t n

let decstbm t top bottom =
  let top = min top t.rows and bottom = min bottom t.rows in
  if top < bottom then (
    t.margins <- { top = top - 1; bottom = bottom - 1 };
    home t)

(* DECALN fills the screen with E in the default rendition, resets origin
   mode, the rendition, the margins and the Last Column Flag, and homes the
   cursor. xterm, a datapoint (charproc.c, CASE_DECALN, citing DEC STD 070),
   does the same, except that it keeps the pen's colours. *)
let decaln t =
  t.modes <- { t.modes with origin = false };
  t.pen <- { t.pen with style = Ansi.Style.default };
  t.margins <- { top = 0; bottom = t.rows - 1 };
  let g = grid t in
  for y = 0 to t.rows - 1 do
    for x = 0 to t.cols - 1 do
      Grid.set_grapheme g ~x ~y ~width:1 ~fg:t.default_fg ~bg:t.default_bg
        ~attrs:Ansi.Attr.empty "E" ~pos:0 ~len:1
    done
  done;
  unwrap t ~top:0 ~bottom:(t.rows - 1);
  changed t ~top:0 ~bottom:(t.rows - 1);
  goto t ~row:0 ~col:0

(* {1:modes Modes and resets} *)

let decsc t =
  (screen t).saved <-
    {
      cursor = { t.cursor with row = t.cursor.row };
      pen = t.pen;
      charsets = t.charsets;
      origin = t.modes.origin;
    }

(* DECRC restores the Last Column Flag with the position. *)
let decrc t =
  let s = (screen t).saved in
  t.pen <- s.pen;
  t.charsets <- s.charsets;
  t.modes <- { t.modes with origin = s.origin };
  let row =
    if s.origin then max t.margins.top (min t.margins.bottom s.cursor.row)
    else s.cursor.row
  in
  set_cursor t ~row ~col:s.cursor.col ~last_column:s.cursor.last_column

let set_cursor_visible t visible =
  if t.modes.cursor_visible <> visible then (
    t.modes <- { t.modes with cursor_visible = visible };
    t.dirty.any <- true;
    t.dirty.cursor <- true)

let switch_screen t ~alternate =
  if t.modes.alternate_screen <> alternate then (
    t.modes <- { t.modes with alternate_screen = alternate };
    touch_rows t 0 (t.rows - 1))

let erase_screen t = erase t ~x:0 ~y:0 ~width:t.cols ~height:t.rows

let leave_alternate t =
  if t.modes.alternate_screen then erase_screen t;
  switch_screen t ~alternate:false

let set_ansi_mode t mode on =
  match mode with 4 -> t.modes <- { t.modes with insert = on } | _ -> ()

let set_dec_mode t mode on =
  let m = t.modes in
  match mode with
  | 1 -> t.modes <- { m with cursor_keys = on }
  | 6 ->
      t.modes <- { m with origin = on };
      home t
  | 7 ->
      t.modes <- { m with auto_wrap = on };
      if not on then t.cursor.last_column <- false
  | 25 -> set_cursor_visible t on
  | 45 -> t.modes <- { m with reverse_wrap = on }
  | 47 -> switch_screen t ~alternate:on
  | 1047 -> if on then switch_screen t ~alternate:true else leave_alternate t
  | 1048 -> if on then decsc t else decrc t
  | 1049 ->
      if on then (
        decsc t;
        switch_screen t ~alternate:true;
        erase_screen t)
      else (
        leave_alternate t;
        decrc t)
  | 2004 -> t.modes <- { m with bracketed_paste = on }
  | _ -> ()

(* SM, RM, DECSET and DECRST: every mode listed, absent ones skipped. *)
let set_modes t p set on =
  for i = 0 to Params.count p - 1 do
    Option.iter (fun mode -> set t mode on) (Params.value p i)
  done

(* RIS: the power-up state. *)
let ris t =
  List.iter
    (fun s ->
      Grid.clear ~color:t.default_bg s.grid;
      Bytes.fill s.wrapped 0 t.rows '\000';
      s.saved <- power_up)
    [ t.primary; t.alternate ];
  clear_history t;
  set_cursor_visible t true;
  t.modes <- power_up_modes;
  set_cursor t ~row:0 ~col:0 ~last_column:false;
  t.pen <- plain_pen;
  t.charsets <- ascii_sets;
  t.margins <- { top = 0; bottom = t.rows - 1 };
  t.tabs <- default_tabs t.cols;
  t.title <- "";
  touch_rows t 0 (t.rows - 1)

(* DECSTR: the settings, the pen and the character sets to their power-up
   values (VT510 DECSTR, Table 5-9), autowrap on as at power-up. *)
let decstr t =
  set_cursor_visible t true;
  t.modes <-
    {
      t.modes with
      insert = false;
      origin = false;
      auto_wrap = true;
      reverse_wrap = false;
      cursor_keys = false;
    };
  t.margins <- { top = 0; bottom = t.rows - 1 };
  t.pen <- plain_pen;
  t.charsets <- ascii_sets;
  t.primary.saved <- power_up;
  t.alternate.saved <- power_up

(* {1:charset_functions Character set functions} *)

(* SCS: [ESC ( F] to [ESC + F] designate the 94-character set [F] as G0 to
   G3. *)
let scs t g final =
  let sets = Array.copy t.charsets.g in
  sets.(g) <- charset_94 final;
  t.charsets <- { t.charsets with g = sets }

(* SI, SO, LS2 and LS3 invoke G0, G1, G2 and G3 into GL. *)
let ls t g = t.charsets <- { t.charsets with gl = g }

(* {1:replies Replies} *)

let reply t s = Buffer.add_string t.replies s
let da t = reply t t.da1
let da2 t = reply t t.da2

(* The 1-based position cursor reports give: in origin mode the row counts
   from the top margin. *)
let report_row t =
  t.cursor.row - (if t.modes.origin then t.margins.top else 0) + 1

let dsr t = function
  | 5 -> reply t "\027[0n"
  | 6 -> Printf.bprintf t.replies "\027[%d;%dR" (report_row t) (t.cursor.col + 1)
  | _ -> ()

let decxcpr t =
  Printf.bprintf t.replies "\027[?%d;%d;1R" (report_row t) (t.cursor.col + 1)

(* {1:dispatch Dispatch}

   A function is named by the private marker, the intermediates and the final
   byte (ECMA-48 §5.4; ECMA-35 §13.1). Counts and positions read 0 as their
   default (Params.pn); selectors do not (Params.ps). *)

let sgr t p =
  let style = Ansi.Style.apply_sgr p t.pen.style in
  if not (Ansi.Style.equal style t.pen.style) then (
    t.pen <- { t.pen with style };
    t.dirty.any <- true)

let decsca t = function
  | 0 | 2 -> t.pen <- { t.pen with protected = false }
  | 1 -> t.pen <- { t.pen with protected = true }
  | _ -> ()

let control t = function
  | '\x08' -> cub t 1
  | '\x09' -> cht t 1
  | '\x0a' | '\x0b' | '\x0c' -> index t
  | '\x0d' -> goto t ~row:t.cursor.row ~col:0
  | '\x0e' -> ls t 1
  | '\x0f' -> ls t 0
  | '\x84' -> index t
  | '\x85' ->
      index t;
      goto t ~row:t.cursor.row ~col:0
  | '\x88' -> hts t
  | '\x8d' -> reverse_index t
  | _ -> ()

let esc t intermediates final =
  match (intermediates, final) with
  | "", 'c' -> ris t
  | "", '7' -> decsc t
  | "", '8' -> decrc t
  | "", 'n' -> ls t 2
  | "", 'o' -> ls t 3
  | "#", '8' -> decaln t
  | "(", f -> scs t 0 f
  | ")", f -> scs t 1 f
  | "*", f -> scs t 2 f
  | "+", f -> scs t 3 f
  | _ -> ()

let csi t (s : Ansi.Parser.sequence) =
  let p = s.params in
  let pn i = Params.pn p i ~default:1 and ps i = Params.ps p i ~default:0 in
  match (s.marker, s.intermediates, s.final) with
  | None, "", 'A' -> cuu t (pn 0)
  | None, "", 'B' -> cud t (pn 0)
  | None, "", ('C' | 'a') -> cuf t (pn 0) (* HPR is CUF on this device *)
  | None, "", 'D' -> cub t (pn 0)
  | None, "", 'E' -> cnl t (pn 0)
  | None, "", 'F' -> cpl t (pn 0)
  | None, "", ('G' | '`') -> cha t (pn 0) (* HPA is CHA on this device *)
  | None, "", 'd' -> vpa t (pn 0)
  | None, "", 'e' -> vpr t (pn 0)
  | None, "", ('H' | 'f') -> cup t (pn 0) (pn 1) (* HVP is CUP *)
  | None, "", 'I' -> cht t (pn 0)
  | None, "", 'Z' -> cbt t (pn 0)
  | None, "", 'g' -> tbc t (ps 0)
  | None, "", 'J' -> ed t (ps 0)
  | None, "", 'K' -> el t (ps 0)
  | None, "", 'X' -> ech t (pn 0)
  | None, "", 'L' -> il t (pn 0)
  | None, "", 'M' -> dl t (pn 0)
  | None, "", 'P' -> dch t (pn 0)
  | None, "", '@' -> ich t (pn 0)
  | None, "", 'r' -> decstbm t (pn 0) (Params.pn p 1 ~default:t.rows)
  | None, "", 'S' -> su t (pn 0)
  | None, "", 'T' -> sd t (pn 0)
  | None, "", 'm' -> sgr t p
  | None, "", 'c' when ps 0 = 0 -> da t
  | None, "", 'n' -> dsr t (ps 0)
  | None, "", 's' -> decsc t (* SCOSC *)
  | None, "", 'u' -> decrc t (* SCORC *)
  | None, "", 'h' -> set_modes t p set_ansi_mode true
  | None, "", 'l' -> set_modes t p set_ansi_mode false
  | Some '?', "", 'h' -> set_modes t p set_dec_mode true
  | Some '?', "", 'l' -> set_modes t p set_dec_mode false
  | Some '?', "", 'J' -> decsed t (ps 0)
  | Some '?', "", 'K' -> decsel t (ps 0)
  | Some '?', "", 'n' when ps 0 = 6 -> decxcpr t
  | Some '>', "", 'c' when ps 0 = 0 -> da2 t
  | None, "!", 'p' -> decstr t
  | None, "\"", 'q' -> decsca t (ps 0)
  | None, " ", '@' -> sl t (pn 0)
  | None, " ", 'A' -> sr t (pn 0)
  | None, "'", '}' -> decic t (pn 0)
  | None, "'", '~' -> decdc t (pn 0)
  | _ -> ()

(* {1:strings Strings}

   Only an OSC the VTE performs is kept, and only up to [osc_budget] bytes; a
   longer one is dropped whole, as xterm drops an over-long string. It is
   performed when it ends with ST or BEL, never when it is cut short. OSC 0 and
   2 set the title; OSC 8 opens a hyperlink, [8 ; params ; URI], or closes it
   when the URI is empty. *)

let osc_budget = 65536

let osc_drop t =
  t.osc <- Osc_skip;
  Buffer.reset t.osc_text

let osc_keep t s off =
  let len = String.length s - off in
  if Buffer.length t.osc_text + len > osc_budget then osc_drop t
  else Buffer.add_substring t.osc_text s off len

(* Ps, the number before the first [;], decides whether the rest is kept. *)
let rec osc_number t s i n =
  if i >= String.length s then t.osc <- Osc_number n
  else
    match s.[i] with
    | '0' .. '9' as c ->
        osc_number t s (i + 1) (min 1000 ((max n 0 * 10) + Char.code c - 48))
    | ';' when n = 0 || n = 2 || n = 8 ->
        t.osc <- Osc_text n;
        osc_keep t s (i + 1)
    | _ -> osc_drop t

let osc_hyperlink t text =
  match String.index_opt text ';' with
  | None -> ()
  | Some i ->
      let uri = String.sub text (i + 1) (String.length text - i - 1) in
      t.pen <- { t.pen with link = (if uri = "" then None else Some uri) }

let open_string t (o : Ansi.Parser.opening) =
  match o with Osc -> t.osc <- Osc_number (-1) | _ -> t.osc <- Osc_skip

let string_data t s =
  match t.osc with
  | Osc_skip -> ()
  | Osc_number n -> osc_number t s 0 n
  | Osc_text _ -> osc_keep t s 0

let close_string t ending =
  (match (t.osc, ending) with
  | Osc_text (0 | 2), (`St | `Bel) -> t.title <- Buffer.contents t.osc_text
  | Osc_text 8, (`St | `Bel) -> osc_hyperlink t (Buffer.contents t.osc_text)
  | _ -> ());
  osc_drop t

(* Every token but text ends the unit in progress. REP reads it first. *)
let handle_token t = function
  | Ansi.Parser.Text s -> print t s
  | Csi { marker = None; intermediates = ""; final = 'b'; params } ->
      rep t (Params.pn params 0 ~default:1);
      close t
  | Csi s ->
      close t;
      csi t s
  | Control c ->
      close t;
      control t c
  | Esc { intermediates; final } ->
      close t;
      esc t intermediates final
  | Open o ->
      close t;
      open_string t o
  | Data s -> string_data t s
  | Close ending -> close_string t ending

let feed t bytes off len =
  Ansi.Parser.feed t.parser bytes ~off ~len (handle_token t)

let feed_string t s = feed t (Bytes.unsafe_of_string s) 0 (String.length s)

let take_replies t =
  let replies = Buffer.contents t.replies in
  Buffer.clear t.replies;
  replies

let reset t =
  Ansi.Parser.reset t.parser;
  osc_drop t;
  close t;
  ris t

(* {1:resize Resizing} *)

(* The screens are cleared: a program repaints after a size change. *)
let resize t ~rows ~cols =
  let rows = max 1 rows and cols = max 1 cols in
  if rows <> t.rows || cols <> t.cols then begin
    close t;
    let clamp (c : cursor) =
      {
        row = min c.row (rows - 1);
        col = min c.col (cols - 1);
        last_column = false;
      }
    in
    List.iter
      (fun s ->
        Grid.resize s.grid ~width:cols ~height:rows;
        Grid.clear ~color:t.default_bg s.grid;
        s.wrapped <- Bytes.make rows '\000';
        s.saved <- { s.saved with cursor = clamp s.saved.cursor })
      [ t.primary; t.alternate ];
    let tabs = default_tabs cols in
    Array.blit t.tabs 0 tabs 0 (min cols (Array.length t.tabs));
    t.rows <- rows;
    t.cols <- cols;
    t.tabs <- tabs;
    t.margins <- { top = 0; bottom = rows - 1 };
    t.dirty.rows <- Bytes.make rows '\001';
    t.dirty.any <- true;
    t.dirty.cursor <- true;
    let c = clamp t.cursor in
    set_cursor t ~row:c.row ~col:c.col ~last_column:false
  end

(* {1:access Access} *)

let rows t = t.rows
let cols t = t.cols
let cursor_pos t = (t.cursor.row, t.cursor.col)

let wrapped t y =
  if y >= 0 then y < t.rows && wrapped_row t y
  else
    (not t.modes.alternate_screen)
    && -y <= t.history.count && (newest t.history (-y - 1)).wrapped

let modes t = t.modes
let title t = t.title
let scrollback_capacity t = Array.length t.history.lines
let scrollback_size t = if t.modes.alternate_screen then 0 else t.history.count

let scrollback_lines t =
  if t.modes.alternate_screen then []
  else
    List.init t.history.count (fun i ->
        let line = newest t.history (t.history.count - 1 - i) in
        let stop = ref (String.length line.text) in
        while !stop > 0 && line.text.[!stop - 1] = ' ' do
          decr stop
        done;
        if !stop = String.length line.text then line.text
        else String.sub line.text 0 !stop)

let render_with_scrollback t ~offset dst =
  if t.modes.alternate_screen || Array.length t.history.lines = 0 then
    Grid.blit ~src:(grid t) ~dst
  else begin
    let offset = max 0 (min offset t.history.count) in
    let height = Grid.height dst in
    Grid.clear ~color:t.default_bg dst;
    let from_history = min offset height in
    (* Row [i] shows the line [offset - i] above the screen, so the newest
       line sits next to the screen. *)
    for i = 0 to from_history - 1 do
      decompress_line (newest t.history (offset - 1 - i)) dst i
    done;
    if height > from_history then
      Grid.blit_region ~src:(grid t) ~dst ~src_x:0 ~src_y:0 ~width:t.cols
        ~height:(min (height - from_history) t.rows)
        ~dst_x:0 ~dst_y:from_history
  end

let is_dirty t = t.dirty.any

let dirty_rows t =
  let rows = ref [] in
  for row = t.rows - 1 downto 0 do
    if Bytes.get t.dirty.rows row <> '\000' then rows := row :: !rows
  done;
  !rows

let is_cursor_dirty t = t.dirty.cursor

let clear_dirty t =
  t.dirty.any <- false;
  t.dirty.cursor <- false;
  Bytes.fill t.dirty.rows 0 t.rows '\000'

let to_string t =
  let g = grid t in
  let line row =
    let buf = Buffer.create t.cols in
    for col = 0 to t.cols - 1 do
      let idx = (row * t.cols) + col in
      if not (Grid.is_continuation g idx) then
        let s = Grid.get_text g idx in
        Buffer.add_string buf (if s = "" then " " else s)
    done;
    Buffer.contents buf
  in
  String.concat "\n" (List.init t.rows line)
